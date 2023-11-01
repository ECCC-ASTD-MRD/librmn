#include "rmn/fst_file.h"

#include <stdlib.h>

#include <App.h>
#include "fst_record_internal.h"
#include "qstdir.h"
#include "rmn/fnom.h"
#include "xdf98.h"

static inline size_t strlen_up_to(const char* string, const size_t max_length) {
    return Min(strlen(string), Max(max_length, 0));
}

//! Verify that the file pointer is valid and the file is open
//! \return 1 if the pointer is valid and the file is open, 0 otherwise
int32_t fst_file_is_open(const fst_file* file) {
    return (file != NULL && file->type != FST_NONE && file->file_index >= 0 && file->iun != 0);
}

//! To be called from Fortran
//! \return The iun of the input file struct. -1 if NULL pointer
int32_t fst23_get_iun(fst_file* file) {
    if (file != NULL) return file->iun;
    return -1;
}

//! Open a standard file (FST). Will create it if it does not already exist
//! \return A handle to the opened file. NULL if there was an error
fst_file* fst23_open(
    const char* file_name,  //!< Path of the file to open
    const char* options     //!< A list of options, as a string, with each pair of options separated by a comma or a '+'
) {
    fst_file* the_file = (fst_file *)malloc(sizeof(fst_file));
    if (the_file == NULL) return NULL;

    *the_file = default_fst_file;

    const int MAX_LENGTH = 1024;
    char local_options[MAX_LENGTH];
    if (options == NULL) {
        strncpy(local_options, "", MAX_LENGTH);
    } else {
        strncpy(local_options, options, MAX_LENGTH);
    }

    if (c_fnom(&(the_file->iun), file_name, local_options, 0) != 0) return NULL;
    if (c_fstouv(the_file->iun, local_options) < 0) return NULL;

    // Find type of newly-opened file (RSF or XDF)
    int index_fnom;
    const int rsf_status = is_rsf(the_file->iun, &index_fnom);
    the_file->file_index = index_fnom;
    if (rsf_status == 1) {
        the_file->type = FST_RSF;
    }
    else {
        the_file->type = FST_XDF;
    }

    return the_file;
}

//! Close the given standard file
//! \return 0 if no error, a negative number otherwise
int32_t fst23_close(fst_file* file) {
    int status;
    status = c_fstfrm(file->iun);   // Close the actual file
    if (status < 0) return status;

    status = c_fclos(file->iun);    // Reset file entry in global table
    if (status < 0) return status;

    *file = default_fst_file;

    return 0;
}

static int32_t fst23_write_rsf(fst_file* file, const fst_record* record) {
    // return c_fstecr_rsf(
    //     record->data, NULL, record->npak, file->iun, file->file_index, record->date, record->deet, record->npas,
    //     record->ni, record->nj, record->nk, record->ip1, record->ip2, record->ip3,
    //     record->typvar, record->nomvar, record->etiket, record->grtyp,
    //     record->ig1, record->ig2, record->ig3, record->ig4, record->datyp, 0);

    RSF_handle file_handle = FGFDT[file->file_index].rsf_fh;

    if (file_handle.p == NULL) {
        Lib_Log(APP_LIBFST, APP_ERROR, "%s: file (unit=%d) is not open\n", __func__, file->iun);
        return ERR_NO_FILE;
    }

    if (! FGFDT[file->file_index].attr.std) {
        Lib_Log(APP_LIBFST, APP_ERROR, "%s: file (unit=%d) is not a RPN standard file\n", __func__, file->iun);
        return ERR_NO_FILE;
    }

    if ((RSF_Get_mode(file_handle) & RSF_RO) == RSF_RO) {
        Lib_Log(APP_LIBFST, APP_ERROR, "%s: file (unit=%d) not open with write permission\n", __func__, file->iun);
        return ERR_NO_WRITE;
    }


    const int num_elements = record->ni * record->nj * record->nk;
    const int in_datyp_ori = record->datyp;

    // will be cancelled later if not supported or no missing values detected
    // missing value feature used flag
    int is_missing = in_datyp_ori & FSTD_MISSING_FLAG;
    // suppress missing value flag (64)
    int in_datyp = in_datyp_ori & ~FSTD_MISSING_FLAG;
    if ( (in_datyp & 0xF) == 8) {
        if (in_datyp_ori != 8) {
           Lib_Log(APP_LIBFST, APP_WARNING, "%s: compression and/or missing values not supported, "
                   "data type %d reset to %d (complex)\n", __func__, in_datyp_ori, 8);
        }
        /* missing values not supported for complex type */
        is_missing = 0;
        /* extra compression not supported for complex type */
        in_datyp = 8;
    }

    /* 512+256+32+1 no interference with turbo pack (128) and missing value (64) flags */
    int datyp = in_datyp == 801 ? 1 : in_datyp;

    PackFunctionPointer packfunc;
    if ((xdf_double) || (in_datyp == 801)) {
        packfunc = (PackFunctionPointer) &compact_double;
    } else {
        packfunc = (PackFunctionPointer) &compact_float;
    }

    int nbits;
    if (record->npak == 0) {
        nbits = FTN_Bitmot;
    } else {
        nbits = (record->npak < 0) ? -record->npak : Max(1, FTN_Bitmot / Max(1, record->npak));
    }
    int minus_nbits = -nbits;

    if ( (in_datyp_ori == 133) && (nbits > 32) ) {
        Lib_Log(APP_LIBFST, APP_WARNING, "%s: extra compression not supported for IEEE when nbits > 32, "
                "data type 133 reset to 5 (IEEE)\n", __func__);
        /* extra compression not supported */
        in_datyp = 5;
        datyp = 5;
    }

    if ((in_datyp == 1) && ((nbits == 31) || (nbits == 32)) && !image_mode_copy) {
        /* R32 to E32 automatic conversion */
        datyp = 5;
        nbits = 32;
        minus_nbits = -32;
    }

    /* flag 64 bit IEEE (type 5 or 8) */
    int IEEE_64 = 0;
    /* 64 bits real IEEE */
    if ( ((in_datyp & 0xF) == 5) && (nbits == 64) ) IEEE_64 = 1;
    /* 64 bits complex IEEE */
    if ( ((in_datyp & 0xF) == 8) && (nbits == 64) ) IEEE_64 = 1;

    /* validate range of arguments */
    if (fst_record_validate_params(record) != 0) {
        Lib_Log(APP_LIBFST, APP_ERROR, "%s: Invalid value for certain parameters\n", __func__);
        return ERR_OUT_RANGE;
    }

    /* Increment date by timestep size */
    unsigned int datev = record->date;
    int32_t f_datev = (int32_t) datev;
    if (( (long long) record->deet * record->npas) > 0) {
        long long deltat = (long long) record->deet * record->npas;
        double nhours = (double) deltat;
        nhours = nhours / 3600.;
        f77name(incdatr)(&f_datev, &f_datev, &nhours);
        datev = (unsigned int) f_datev;
    }

    if ((record->npak == 0) || (record->npak == 1)) {
        /* no compaction */
        datyp = 0;
    }

    /* allocate and initialize a buffer interface for RSF_Put */
    /* an extra 512 bytes are allocated for cluster alignment purpose (seq) */ // Are they???

    if (! image_mode_copy) {
        for (int i = 0; i < nb_remap; i++) {
            if (datyp == remap_table[0][i]) {
                datyp = remap_table[1][i];
            }
        }
    }

    static int dejafait_rsf_1 = 0;
    static int dejafait_rsf_2 = 0;

    /* no extra compression if nbits > 16 */
    if ((nbits > 16) && (datyp != 133)) datyp &= 0x7F;
    /*  if ((datyp < 128) && (extra_compression > 0) && (nbits <= 16)) datyp += extra_compression; */
    if ((datyp == 6) && (nbits > 24)) {
        if (! dejafait_rsf_1) {
            Lib_Log(APP_LIBFST, APP_WARNING, "%s: nbits > 16, writing E32 instead of F%2d\n", __func__, nbits);
            dejafait_rsf_1 = 1;
        }
        datyp = 5;
        nbits = 32;
        minus_nbits = -32;
    }
    if ((datyp == 6) && (nbits > 16)) {
        if (! dejafait_rsf_2) {
            Lib_Log(APP_LIBFST, APP_WARNING, "%s: nbits > 16, writing R%2d instead of F%2d\n", __func__, nbits, nbits);
            dejafait_rsf_2 = 1;
        }
        datyp = 1;
    }

    /* Determine size of data to be stored */
    int header_size;
    int stream_size;
    // int nw;
    size_t num_word64;
    switch (datyp) {
        case 6: {
            int p1out;
            int p2out;
            c_float_packer_params(&header_size, &stream_size, &p1out, &p2out, num_elements);
            num_word64 = ((header_size+stream_size) * 8 + 63) / 64;
            header_size /= sizeof(int32_t);
            stream_size /= sizeof(int32_t);
            break;
        }

        case 8:
            num_word64 = 2 * ((num_elements *nbits + 63) / 64);
            break;

        case 129:
            /* 120 bits (floatpack header)+8, 32 bits (extra header) */
            num_word64 = (num_elements * Max(nbits, 16) + 128 + 32 + 63) / 64;
            break;

        case 130:
            /* 32 bits (extra header) */
            num_word64 = (num_elements * Max(nbits, 16) + 32 + 63) / 64;
            break;

        case 134: {
            int p1out;
            int p2out;
            c_float_packer_params(&header_size, &stream_size, &p1out, &p2out, num_elements);
            num_word64 = ((header_size+stream_size) * 8 + 32 + 63) / 64;
            stream_size /= sizeof(int32_t);
            header_size /= sizeof(int32_t);
            break;
        }

        default:
            num_word64 = (num_elements * nbits + 120 + 63) / 64;
            break;
    }

    /* Allocate new record */
    const size_t num_word32 = W64TOWD(num_word64);
    const size_t num_data_bytes = num_word32 * 4;
    const size_t dir_metadata_size = (sizeof(stdf_dir_keys) + 3) / 4; // In 32-bit units
    RSF_record* new_record = RSF_New_record(file_handle, dir_metadata_size, dir_metadata_size, num_data_bytes, NULL, 0);
    if (new_record == NULL) {
        Lib_Log(APP_LIBFST, APP_FATAL, "%s: Unable to create new new_record with %ld bytes\n", __func__, num_data_bytes);
        return(ERR_MEM_FULL);
    }
    uint32_t* record_data = new_record->data;

    const int num_bits_per_word = 32;
    RSF_Record_set_num_elements(new_record, num_word32, sizeof(uint32_t));

    char typvar[TYPVAR_LEN] = {' ', ' ', '\0'};
    strncpy(typvar, record->typvar, strlen_up_to(record->typvar, TYPVAR_LEN - 1));
    char nomvar[NOMVAR_LEN] = {' ', ' ', ' ', ' ', '\0'};
    strncpy(nomvar, record->nomvar, strlen_up_to(record->nomvar, NOMVAR_LEN - 1));
    char etiket[ETIKET_LEN] = {' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ' , ' ' , '\0'};
    strncpy(etiket, record->etiket, strlen_up_to(record->etiket, ETIKET_LEN - 1));
    char grtyp[GTYP_LEN] = {' ', '\0'};
    strncpy(grtyp, record->grtyp, strlen_up_to(record->grtyp, GTYP_LEN - 1));

    /* set stdf_entry to address of buffer->data for building keys */
    stdf_dir_keys * stdf_entry = (stdf_dir_keys *) new_record->meta;
    
    stdf_entry->deleted = 0; // Unused by RSF
    stdf_entry->select = 0;  // Unused by RSF
    stdf_entry->lng = 0;     // Unused by RSF
    stdf_entry->addr = 0;    // Unused by RSF

    stdf_entry->deet = record->deet;
    stdf_entry->nbits = nbits;
    stdf_entry->ni = record->ni;
    stdf_entry->gtyp = grtyp[0];
    stdf_entry->nj = record->nj;
    /* propagate missing values flag */
    stdf_entry->datyp = datyp | is_missing;
    /* this value may be changed later in the code to eliminate improper flags */
    stdf_entry->nk = record->nk;
    stdf_entry->ubc = 0;
    stdf_entry->npas = record->npas;
    stdf_entry->pad7 = 0;
    stdf_entry->ig4 = record->ig4;
    stdf_entry->ig2a = record->ig2 >> 16;
    stdf_entry->ig1 = record->ig1;
    stdf_entry->ig2b = record->ig2 >> 8;
    stdf_entry->ig3 = record->ig3;
    stdf_entry->ig2c = record->ig2 & 0xff;
    stdf_entry->etik15 =
        (ascii6(etiket[0]) << 24) |
        (ascii6(etiket[1]) << 18) |
        (ascii6(etiket[2]) << 12) |
        (ascii6(etiket[3]) <<  6) |
        (ascii6(etiket[4]));
    stdf_entry->pad1 = 0;
    stdf_entry->etik6a =
        (ascii6(etiket[5]) << 24) |
        (ascii6(etiket[6]) << 18) |
        (ascii6(etiket[7]) << 12) |
        (ascii6(etiket[8])<<  6) |
        (ascii6(etiket[9]));
    stdf_entry->pad2 = 0;
    stdf_entry->etikbc =
        (ascii6(etiket[10]) <<  6) |
        (ascii6(etiket[11]));
    stdf_entry->typvar =
        (ascii6(typvar[0]) <<  6) |
        (ascii6(typvar[1]));
    stdf_entry->pad3 = 0;
    stdf_entry->nomvar =
        (ascii6(nomvar[0]) << 18) |
        (ascii6(nomvar[1]) << 12) |
        (ascii6(nomvar[2]) <<  6) |
        (ascii6(nomvar[3]));
    stdf_entry->pad4 = 0;
    stdf_entry->ip1 = record->ip1;
    stdf_entry->levtyp = 0;
    stdf_entry->ip2 = record->ip2;
    stdf_entry->pad5 = 0;
    stdf_entry->ip3 = record->ip3;
    stdf_entry->pad6 = 0;
    stdf_entry->date_stamp = 8 * (datev/10) + (datev % 10);

    uint32_t * field = record->data;
    if (image_mode_copy) {
        /* no pack/unpack, used by editfst */
        if (datyp > 128) {
            /* first element is length */
            const int num_field_words32 = field[0];
            memcpy(new_record->data, field, (num_field_words32 + 1) * sizeof(uint32_t));
        } else {
            int num_field_bits;
            if (datyp == 6) {
                int p1out;
                int p2out;
                c_float_packer_params(&header_size, &stream_size, &p1out, &p2out, num_elements);
                num_field_bits = (header_size + stream_size) * 8;
            } else {
                num_field_bits = num_elements * nbits;
            }
            if (datyp == 1) num_field_bits += 120;
            if (datyp == 3) num_field_bits = record->ni * record->nj * 8;
            const int num_field_words32 = (num_field_bits + num_bits_per_word - 1) / num_bits_per_word;

            memcpy(new_record->data, field, num_field_words32 * sizeof(uint32_t));
        }
    } else {
        // not image mode copy
        // time to fudge field if missing value feature is used

        // number of bytes per data item
        int sizefactor = 4;
        if (xdf_byte)  sizefactor = 1;
        if (xdf_short) sizefactor = 2;
        if (xdf_double | IEEE_64) sizefactor = 8;
        /* put appropriate values into field after allocating it */
        if (is_missing) {
            // allocate self deallocating scratch field
            field = (uint32_t *)alloca(num_elements * sizefactor);
            if ( 0 == EncodeMissingValue(field, record->data, num_elements, in_datyp, nbits, xdf_byte, xdf_short, xdf_double) ) {
                field = record->data;
                Lib_Log(APP_LIBFST, APP_INFO, "%s: NO missing value, data type %d reset to %d\n", __func__,
                        stdf_entry->datyp, datyp);
                /* cancel missing data flag in data type */
                stdf_entry->datyp = datyp;
                is_missing = 0;
            }
        }

        switch (datyp) {

            case 0: case 128: {
                /* transparent mode */
                if (datyp == 128) {
                    Lib_Log(APP_LIBFST, APP_WARNING, "%s: extra compression not available, data type %d reset to %d\n",
                            __func__, stdf_entry->datyp, 0);
                    datyp = 0;
                    stdf_entry->datyp = 0;
                }
                const int32_t num_word32 = ((num_elements * nbits) + num_bits_per_word - 1) / num_bits_per_word;
                memcpy(new_record->data, field, num_word32 * sizeof(uint32_t));
                break;
            }

            case 1: case 129: {
                /* floating point */
                double tempfloat = 99999.0;
                if ((datyp > 128) && (nbits <= 16)) {
                    /* use an additional compression scheme */
                    /* nbits>64 flags a different packing */
                    // Use data pointer as uint32_t for compatibility with XDF format
                    packfunc(field, (void *)&((uint32_t *)new_record->data)[1], (void *)&((uint32_t *)new_record->data)[5],
                        num_elements, nbits + 64 * Max(16, nbits), 0, xdf_stride, 1, 0, &tempfloat);
                    const int compressed_lng = armn_compress((unsigned char *)((uint32_t *)new_record->data + 5),
                                                             record->ni, record->nj, record->nk, nbits, 1);
                    if (compressed_lng < 0) {
                        stdf_entry->datyp = 1;
                        packfunc(field, (void*)new_record->data, (void*)&((uint32_t*)new_record->data)[3],
                            num_elements, nbits, 24, xdf_stride, 1, 0, &tempfloat);
                    } else {
                        int nbytes = 16 + compressed_lng;
                        const uint32_t num_word64 = (nbytes * 8 + 63) / 64;
                        const uint32_t num_word32 = W64TOWD(num_word64);
                        ((uint32_t*)new_record->data)[0] = num_word32;
                        RSF_Record_set_num_elements(new_record, num_word32, sizeof(uint32_t));
                    }
                } else {
                    packfunc(field, (void*)new_record->data, (void*)&((uint32_t*)new_record->data)[3],
                        num_elements, nbits, 24, xdf_stride, 1, 0, &tempfloat);
                }
                break;
            }

            case 2: case 130:
                /* integer, short integer or byte stream */
                {
                    int offset = (datyp > 128) ? 1 :0;
                    if (datyp > 128) {
                        if (xdf_short) {
                            stdf_entry->nbits = Min(16, nbits);
                            nbits = stdf_entry->nbits;
                            memcpy(record_data + offset, (void *)field, num_elements * 2);
                        } else if (xdf_byte) {
                            stdf_entry->nbits = Min(8, nbits);
                            nbits = stdf_entry->nbits;
                            memcpy_8_16((int16_t *)(record_data + offset), (void *)field, num_elements);
                        } else {
                            memcpy_32_16((short *)(record_data + offset), (void *)field, nbits, num_elements);
                        }
                        c_armn_compress_setswap(0);
                        const int compressed_lng = armn_compress((unsigned char *)&((uint32_t *)new_record->data)[offset],
                                                                 record->ni, record->nj, record->nk, nbits, 1);
                        c_armn_compress_setswap(1);
                        if (compressed_lng < 0) {
                            stdf_entry->datyp = 2;
                            compact_integer((void *)field, (void *) NULL, &((uint32_t *)new_record->data)[offset],
                                num_elements, nbits, 0, xdf_stride, 1);
                        } else {
                            const int nbytes = 4 + compressed_lng;
                            const uint32_t num_word64 = (nbytes * 8 + 63) / 64;
                            const uint32_t num_word32 = W64TOWD(num_word64);
                            ((uint32_t *)new_record->data)[0] = num_word32;
                            RSF_Record_set_num_elements(new_record, num_word32, sizeof(uint32_t));
                        }
                    } else {
                        if (xdf_short) {
                            stdf_entry->nbits = Min(16, nbits);
                            nbits = stdf_entry->nbits;
                            compact_short((void *)field, (void *) NULL, &((uint32_t *)new_record->data)[offset],
                                num_elements, nbits, 0, xdf_stride, 5);
                        } else if (xdf_byte) {
                            compact_char((void *)field, (void *) NULL, new_record->data,
                                num_elements, Min(8, nbits), 0, xdf_stride, 9);
                            stdf_entry->nbits = Min(8, nbits);
                            nbits = stdf_entry->nbits;
                        } else {
                            compact_integer((void *)field, (void *) NULL, &((uint32_t *)new_record->data)[offset],
                                num_elements, nbits, 0, xdf_stride, 1);
                        }
                    }
                }
                break;


            case 3: case 131:
                /* character */
                {
                    int nc = (record->ni * record->nj + 3) / 4;
                    if (datyp == 131) {
                        Lib_Log(
                            APP_LIBFST, APP_WARNING, "%s: extra compression not available, data type %d reset to %d\n",
                            __func__, stdf_entry->datyp, 3);
                        datyp = 3;
                        stdf_entry->datyp = 3;
                    }
                    compact_integer(field, (void *) NULL, new_record->data, nc, 32, 0, xdf_stride, 1);
                    stdf_entry->nbits = 8;
                }
                break;

            case 4: case 132:
                /* signed integer */
                if (datyp == 132) {
                    Lib_Log(APP_LIBFST, APP_WARNING, "%s: extra compression not supported, data type %d reset to %d\n",
                            __func__, stdf_entry->datyp, is_missing | 4);
                    datyp = 4;
                }
                /* turbo compression not supported for this type, revert to normal mode */
                stdf_entry->datyp = is_missing | 4;
#ifdef use_old_signed_pack_unpack_code
                uint32_t * field3 = field;
                if (xdf_short || xdf_byte) {
                    field3 = (uint32_t *)alloca(num_elements * sizeof(int));
                    short * s_field = (short *)field;
                    signed char * b_field = (signed char *)field;
                    if (xdf_short) for (int i = 0; i < num_elements;i++) { field3[i] = s_field[i]; };
                    if (xdf_byte)  for (int i = 0; i < num_elements;i++) { field3[i] = b_field[i]; };
                }
                compact_integer(field3, (void *) NULL, new_record->data, num_elements, nbits, 0, xdf_stride, 3);
#else
                if (xdf_short) {
                    compact_short(field, (void *) NULL, new_record->data, num_elements, nbits, 0, xdf_stride, 7);
                } else if (xdf_byte) {
                    compact_char(field, (void *) NULL, new_record->data, num_elements, nbits, 0, xdf_stride, 11);
                } else {
                    compact_integer(field, (void *) NULL, new_record->data, num_elements, nbits, 0, xdf_stride, 3);
                }
#endif
                break;

            case 5: case 8: case 133:  case 136:
                /* IEEE and IEEE complex representation */
                {
                    int32_t f_ni = record->ni;
                    int32_t f_njnk = record->nj * record->nk;
                    int32_t f_zero = 0;
                    int32_t f_one = 1;
                    int32_t f_minus_nbits = (int32_t) minus_nbits;
                    if (datyp == 136) {
                        Lib_Log(
                            APP_LIBFST, APP_WARNING, "%s: extra compression not available, data type %d reset to %d\n",
                            __func__, stdf_entry->datyp, 8);
                        datyp = 8;
                        stdf_entry->datyp = 8;
                    }
                    if (datyp == 133) {
                        /* use an additionnal compression scheme */
                        const int compressed_lng = c_armn_compress32(
                            (unsigned char *)&((uint32_t *)new_record->data)[1], (void *)field, record->ni, record->nj,
                            record->nk, nbits);
                        if (compressed_lng < 0) {
                            stdf_entry->datyp = 5;
                            f77name(ieeepak)((int32_t *)field, new_record->data, &f_ni, &f_njnk, &f_minus_nbits, &f_zero, &f_one);
                        } else {
                            const int nbytes = 16 + compressed_lng;
                            const uint32_t num_word64 = (nbytes * 8 + 63) / 64;
                            const uint32_t num_word32 = W64TOWD(num_word64);
                            ((uint32_t *)new_record->data)[0] = num_word32;
                            RSF_Record_set_num_elements(new_record, num_word32, sizeof(uint32_t));
                        }
                    } else {
                        if (datyp == 8) f_ni = f_ni * 2;
                        f77name(ieeepak)((int32_t *)field, new_record->data, &f_ni, &f_njnk, &f_minus_nbits, &f_zero, &f_one);
                    }
                }
                break;

            case 6: case 134:
                /* floating point, new packers */

                if ((datyp > 128) && (nbits <= 16)) {
                    /* use an additional compression scheme */
                    c_float_packer((void *)field, nbits, &((int32_t *)new_record->data)[1],
                                   &((int32_t *)new_record->data)[1+header_size], num_elements);
                    const int compressed_lng = armn_compress(
                        (unsigned char *)&((uint32_t *)new_record->data)[1+header_size], record->ni, record->nj,
                        record->nk, nbits, 1);
                    if (compressed_lng < 0) {
                        stdf_entry->datyp = 6;
                        c_float_packer((void *)field, nbits, new_record->data, &((int32_t *)new_record->data)[header_size],
                                        num_elements);
                    } else {
                        const int nbytes = 16 + (header_size*4) + compressed_lng;
                        const uint32_t num_word64 = (nbytes * 8 + 63) / 64;
                        const uint32_t num_word32 = W64TOWD(num_word64);
                        ((uint32_t *)new_record->data)[0] = num_word32;
                        RSF_Record_set_num_elements(new_record, num_word32, sizeof(uint32_t));
                    }
                } else {
                    c_float_packer((void *)field, nbits, new_record->data,
                                   &((int32_t *)new_record->data)[header_size], num_elements);
                }
                break;


            case 7: case 135:
                /* character string */
                if (datyp == 135) {
                    Lib_Log(APP_LIBFST, APP_WARNING, "%s: extra compression not available, data type %d reset to %d\n",
                            __func__, stdf_entry->datyp, 7);
                    datyp = 7;
                    stdf_entry->datyp = 7;
                }
                compact_char(field, (void *) NULL, new_record->data, num_elements, 8, 0, xdf_stride, 9);
                break;

            default:
                Lib_Log(APP_LIBFST, APP_ERROR, "%s: (unit=%d) invalid datyp=%d\n", __func__, file->iun, datyp);
                return ERR_BAD_DATYP;
        } /* end switch */
    } /* end if image mode copy */

    /* write new_record to file and add entry to directory */
    const int64_t record_handle = RSF_Put_record(file_handle, new_record, num_data_bytes);
    if (Lib_LogLevel(APP_LIBFST,NULL) > APP_INFO) {
        char string[12];
        sprintf(string, "Write(%d)", file->iun);
        print_std_parms(stdf_entry, string, prnt_options, -1);
    }

    xdf_double = 0;
    xdf_short = 0;
    xdf_byte = 0;
    return record_handle > 0 ? 0 : -1;
}

//! Write the given record into the given standard file
//! \return 0 if everything was a success, a negative error code otherwise
int32_t fst23_write(fst_file* file, const fst_record* record) {
    if (!fst_file_is_open(file)) return ERR_NO_FILE;
    if (!fst_record_is_valid(record)) return ERR_BAD_INIT;

    char typvar[TYPVAR_LEN];
    char nomvar[NOMVAR_LEN];
    char etiket[ETIKET_LEN];
    char grtyp[GTYP_LEN];

    strncpy(typvar, record->typvar, TYPVAR_LEN);
    strncpy(nomvar, record->nomvar, NOMVAR_LEN);
    strncpy(etiket, record->etiket, ETIKET_LEN);
    strncpy(grtyp, record->grtyp, GTYP_LEN);

    switch (file->type)
    {
    case FST_RSF:
        return fst23_write_rsf(file, record);
    case FST_XDF:
        return c_fstecr_xdf(
            record->data, NULL, record->npak, file->iun, record->date, record->deet, record->npas,
            record->ni, record->nj, record->nk, record->ip1, record->ip2, record->ip3,
            typvar, nomvar, etiket, grtyp, record->ig1, record->ig2, record->ig3, record->ig4, record->datyp, 0);
    default:
        Lib_Log(APP_LIBFST, APP_ERROR, "%s: Unknown/invalid file type %d\n", __func__, file->type);
        return -1;
    } // End switch
}

//! Look for a record in the given file matching certain criteria
//! The criteria are given as a fst_record struct, with the value of -1 acting as a wildcard.
//! For the character-type variables, the wildcard is a space
fst_record fst23_find_record(fst_file* file, const fst_record* criteria) {
    fst_record result = default_fst_record;

    if (!fst_file_is_open(file)) return result;
    if (!fst_record_is_valid(criteria)) return result;

    // Pack criteria into format used by both backends
    stdf_dir_keys* search_criteria;
    stdf_dir_keys* search_mask;
    make_search_criteria(criteria, &search_criteria, &search_mask);

    int64_t key = -1;
    const stdf_dir_keys* record_meta = NULL;

    // Look for the record in the file, depending on backend
    switch (file->type) {
    case FST_RSF:
        {
            RSF_handle file_handle = FGFDT[file->file_index].rsf_fh;
            fstd_open_files[file->file_index].search_start_key = 0;
            fstd_open_files[file->file_index].search_criteria = *search_criteria;
            fstd_open_files[file->file_index].search_mask = *search_mask;

            key = find_next_record(file_handle, &fstd_open_files[file->file_index]);

            if (key >= 0) {
                const RSF_record_info record_info = RSF_Get_record_info(file_handle, key);
                record_meta = (const stdf_dir_keys*)record_info.meta;
            }
        }
        break;
    case FST_XDF:
        {
            uint32_t* pkeys = (uint32_t *) search_criteria;
            uint32_t* pmask = (uint32_t *) search_mask;

            pkeys += W64TOWD(1);
            pmask += W64TOWD(1);

            key = c_xdfloc2(file->iun, 0, pkeys, 16, pmask);

            if (key >= 0) {
                int addr, lng, idtyp;
                c_xdfprm(key, &addr, &lng, &idtyp, pkeys, 16);
                record_meta = search_criteria;
            }
        }
        break;
    default:
        Lib_Log(APP_LIBFST, APP_ERROR, "%s: Unknown/invalid file type %d\n", __func__, file->type);
        break;
    } // end switch

    if (key >= 0) {
        // Got the record. Now unpack it into a more convenient format
        Lib_Log(APP_LIBFST, APP_DEBUG, "%s: (unit=%d) Found record at key 0x%x\n", __func__, file->iun, key);
        result = record_from_dir_keys(record_meta);
        result.handle = key;
    }

    free(search_criteria);
    free(search_mask);

    return result;
}

fst_record fst23_read_record(fst_file* file, const int64_t key) {
    fst_record result = default_fst_record;

    if (!fst_file_is_open(file)) return result;


    switch(file->type) {
        case FST_RSF: {
                const int32_t key32 = RSF_Key32(key);
                RSF_handle file_handle = RSF_Key32_to_handle(key32);
                RSF_record* rec = RSF_Get_record(file_handle, key);
                if (rec == NULL) {
                    Lib_Log(APP_LIBFST, APP_ERROR, "%s: Could not get record corresponding to key 0x%x (0x%x)\n",
                            __func__, key32, key);
                    return result;
                }

                stdf_dir_keys* stdf_entry = (stdf_dir_keys*)rec->meta;
                //TODO use actual new Meta
                if (rec->rec_meta > rec->dir_meta) {
                    tmp_meta_struct* meta = (tmp_meta_struct*)(stdf_entry + 1); // Right after directory metadata
                    Lib_Log(APP_LIBFST, APP_INFO, "%s: Retrieved metadata with value %ld for record 0x%x\n",
                            __func__, meta->some_metadata, key);
                }

                //TODO This is reading it a second time!!!! Should unpack right here rather than using fstluk!!!!
                result.data = malloc(stdf_entry->ni * stdf_entry->nj * stdf_entry->nk * 16 + 500); //TODO allocate the right amount

                c_fstluk_rsf(result.data, file_handle, key32, &result.ni, &result.nj, &result.nk);

                result.handle = key;
                break;
            }
        default:
            Lib_Log(APP_LIBFST, APP_ERROR, "%s: Unrecognized file type\n", __func__);
            break;
    }

    return result;
}
