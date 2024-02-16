#include "fst_file_internal.h"

#include <stdlib.h>
#include <float.h>
#include <math.h>

#include <App.h>
#include <str.h>
#include "fst_record_internal.h"
#include "fstd98_internal.h"
#include "qstdir.h"
#include "rmn/fnom.h"
#include "xdf98.h"
#include "Meta.h"

//! Verify that the file pointer is valid and the file is open
//! \return 1 if the pointer is valid and the file is open, 0 otherwise
int32_t fst24_is_open(const fst_file* file) {
    return (file != NULL && file->type != FST_NONE && file->file_index >= 0 && file->iun != 0);
}

//! To be called from Fortran
//! \return The iun of the input file struct. -1 if NULL pointer
int32_t fst24_get_iun(fst_file* file) {
    if (file != NULL) return file->iun;
    return -1;
}

//! Open a standard file (FST). Will create it if it does not already exist
//! \return A handle to the opened file. NULL if there was an error
fst_file* fst24_open(
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
    if (rsf_status == 1) {
        the_file->file_index = index_fnom;
        the_file->type = FST_RSF;
    }
    else {
        the_file->file_index = file_index_xdf(the_file->iun);
        the_file->type = FST_XDF;
    }

    return the_file;
}

//TODO what happens if closing a linked file?
//! Close the given standard file
//! \return 0 if no error, a negative number otherwise
int32_t fst24_close(fst_file* file) {
    if (!fst24_is_open(file)) return ERR_NO_FILE;

    int status;
    status = c_fstfrm(file->iun);   // Close the actual file
    if (status < 0) return status;

    status = c_fclos(file->iun);    // Reset file entry in global table
    if (status < 0) return status;

    *file = default_fst_file;

    return 0;
}

int64_t fst24_get_num_records(const fst_file* file) {
    if (!fst24_is_open(file)) return 0;

    int64_t total_num_records = 0;

    if (file->type == FST_RSF) {
        RSF_handle file_handle = FGFDT[file->file_index].rsf_fh;
        total_num_records = (int64_t)RSF_Get_num_records(file_handle);
    }
    else if (file->type == FST_XDF) {
        const int status = c_fstnbr_xdf(file->iun);
        if (status < 0) return 0; // Stop recursion here if error
        total_num_records = status;
    }
    else {
        Lib_Log(APP_LIBFST, APP_ERROR, "%s: Unrecognized file type\n", __func__);
        return 0;
    }

    if (file->next != NULL) total_num_records += fst24_get_num_records(file->next);

    return total_num_records;
}

int64_t fst24_get_num_records_single(const fst_file* file) {
    if (!fst24_is_open(file)) return ERR_NO_FILE;

    if (file->type == FST_RSF) {
        RSF_handle file_handle = FGFDT[file->file_index].rsf_fh;
        return (int64_t)RSF_Get_num_records(file_handle);
    }
    else if (file->type == FST_XDF) {
        const int status = c_fstnbr_xdf(file->iun);
        if (status < 0) return 0;
        return status;
    }

    return 0;
}

int32_t fst24_print_summary(const fst_file* file, const fst_record_fields* fields) {
    if (!fst24_is_open(file)) return ERR_NO_FILE;

    const int64_t num_records = fst24_get_num_records(file);
    size_t total_file_size = 0;
    
    fst_record rec;
    for (unsigned int i = 0; i < num_records; i++) {
        fst24_get_record_by_index(file, i, &rec);
        // total_file_size += info.rl;
        // char string[20];
        // sprintf(string, "%5d-", i);
        // print_std_parms(metadata, string, options, ((i % 70) == 0));
        fst24_record_print_short(&rec, fields, ((i % 70) == 0));
    }

    fprintf(stdout, "\n%d records in RPN standard file(s). Total size %ld bytes.\n", num_records, total_file_size);

    return 0;
}

//! Retreive record's data minimum and maximum value
void fst24_bounds(const fst_record *record,
    double *Min,  //!< Mimimum value (NaNif not retreivable)
    double *Max   //!< Maximum value (NaNif not retreivable)
) {

    uint64_t n,sz=(record->ni*record->nj*record->nk);
    double dmin=DBL_MAX,dmax=DBL_MIN,dval;
    uint64_t  umin=ULONG_MAX,umax=0,uval;
    char cmin=CHAR_MAX,cmax=CHAR_MIN,cval;
    int64_t lmin=LONG_MAX,lmax=0,lval;

    *Min=*Max=NAN;

    // Loop on data type to avoid type casting as much as possible
    switch (record->datyp) {

        case 0: case 128:
            /* binary */
            *Min=0;*Max=1;
            break;

        case 1: case 5: case 6: 
            /* floating point */
            for(n=0;n<sz;n++) {
                dval=record->dasiz==32?((float*)record->data)[n]:((double*)record->data)[n];
                if (dval<dmin) dmin=dval; else if (dval>dmax) dmax=dval;
            }
            *Min=dmin;*Max=dmax;
            break;

        case 2:
            /* integer, short integer or byte stream */
            for(n=0;n<sz;n++) {
                uval=record->dasiz==32?((uint32_t*)record->data)[n]:record->dasiz==64?((uint64_t*)record->data)[n]:((uint8_t*)record->data)[n];
                if (uval<umin) umin=uval; else if (uval>umax) umax=uval;
            }
            *Min=umin;*Max=umax;
            break;


        case 3: //TODO: WTF is character
            /* character */
            for(n=0;n<sz;n++) {
                cval=((char*)record->data)[n];
                if (cval<cmin) cmin=cval; else if (cval>cmax) cmax=cval;
            }
            *Min=cmin;*Max=cmax;
            break;

        case 4: case 132:
            /* signed integer */
            for(n=0;n<sz;n++) {
                lval=record->dasiz==32?((int32_t*)record->data)[n]:record->dasiz==64?((int64_t*)record->data)[n]:((int8_t*)record->data)[n];
                if (lval<lmin) lmin=lval; else if (lval>lmax) lmax=lval;
            }
            *Min=lmin;*Max=lmax;
            break;

        case 7: case 135:
            break;
    }
}

static int32_t fst24_write_rsf(fst_file* file, const fst_record* record) {

    RSF_handle file_handle = FGFDT[file->file_index].rsf_fh;

    if (file_handle.p == NULL) {
        Lib_Log(APP_LIBFST, APP_ERROR, "%s: file (unit=%d) is not open\n", __func__, file->iun);
        return ERR_NO_FILE;
    }

    if (!FGFDT[file->file_index].attr.std) {
        Lib_Log(APP_LIBFST, APP_ERROR, "%s: file (unit=%d) is not a RPN standard file\n", __func__, file->iun);
        return ERR_NO_FILE;
    }

    if ((RSF_Get_mode(file_handle) & RSF_RO) == RSF_RO) {
        Lib_Log(APP_LIBFST, APP_ERROR, "%s: file (unit=%d) not open with write permission\n", __func__, file->iun);
        return ERR_NO_WRITE;
    }

    const int num_elements = record->ni * record->nj * record->nk;
    // int in_dasiz = record->dasiz;

    // will be cancelled later if not supported or no missing values detected
    // missing value feature used flag
    int is_missing = record->datyp & FSTD_MISSING_FLAG;
    // suppress missing value flag (64)
    int in_datyp = record->datyp & ~FSTD_MISSING_FLAG;
    if ( (in_datyp & 0xF) == 8) {
        if (record->datyp != 8) {
           Lib_Log(APP_LIBFST, APP_WARNING, "%s: compression and/or missing values not supported, "
                   "data type %d reset to %d (complex)\n", __func__, record->datyp, 8);
        }
        /* missing values not supported for complex type */
        is_missing = 0;
        /* extra compression not supported for complex type */
        in_datyp = 8;
    }

    /* 512+256+32+1 no interference with turbo pack (128) and missing value (64) flags */
    int datyp = in_datyp == 801 ? 1 : in_datyp;

    PackFunctionPointer packfunc;
    double dmin=0.0,dmax=0.0;
    if (record->dasiz == 64 || in_datyp == 801) {
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

    if ( (record->datyp == 133) && (nbits > 32) ) {
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


    /* flag 64 bit IEEE */
    const int force_64 = (nbits == 64 && (is_type_real(in_datyp) || is_type_complex(in_datyp)));
    const int8_t elem_size = force_64 ? 64 : record->dasiz;

    /* validate range of arguments */
    if (fst24_record_validate_params(record) != 0) {
        Lib_Log(APP_LIBFST, APP_ERROR, "%s: Invalid value for certain parameters\n", __func__);
        return ERR_OUT_RANGE;
    }

    /* Increment date by timestep size */
    unsigned int datev = record->dateo;
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

    // New json metadata
    char *metastr = NULL;
    int   metalen = 0;
    size_t rec_metadata_size = dir_metadata_size;
    if (record->metadata) {
       fst24_bounds(record,&dmin,&dmax);
       Meta_DefData(record->metadata,record->ni,record->nj,record->nk,FST_TYPE_NAMES[datyp],"lorenzo",nbits,record->dasiz,dmin,dmax);
       if ((metastr = Meta_Stringify(record->metadata))) {
          metalen = strlen(metastr);
          rec_metadata_size += ((metastr?metalen:0)+3)/4;
       }
    }

    RSF_record* new_record = RSF_New_record(file_handle, rec_metadata_size, dir_metadata_size, num_data_bytes, NULL, 0);
    if (new_record == NULL) {
        Lib_Log(APP_LIBFST, APP_FATAL, "%s: Unable to create new new_record with %ld bytes\n", __func__, num_data_bytes);
        return(ERR_MEM_FULL);
    }
    uint32_t* record_data = new_record->data;

    const int num_bits_per_word = 32;
    RSF_Record_set_num_elements(new_record, num_word32, sizeof(uint32_t));

    char typvar[FST_TYPVAR_LEN];
    copy_record_string(typvar, record->typvar, FST_TYPVAR_LEN);
    char nomvar[FST_NOMVAR_LEN];
    copy_record_string(nomvar, record->nomvar, FST_NOMVAR_LEN);
    char etiket[FST_ETIKET_LEN];
    copy_record_string(etiket, record->etiket, FST_ETIKET_LEN);
    char grtyp[FST_GTYP_LEN];
    copy_record_string(grtyp, record->grtyp, FST_GTYP_LEN);

    /* set stdf_entry to address of buffer->data for building keys */
    stdf_dir_keys* stdf_entry = (stdf_dir_keys *) new_record->meta;

    // Insert json metadata
    if (metastr) {
        memcpy((char *)(stdf_entry + 1),metastr,metalen); // Copy metadata into RSF record struct, just after directory metadata
    }
    
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
        (ascii6(etiket[8]) <<  6) |
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
    stdf_entry->ip1 = record->ip1;
    stdf_entry->levtyp = 0;
    stdf_entry->ip2 = record->ip2;
    stdf_entry->pad5 = 0;
    stdf_entry->ip3 = record->ip3;
    stdf_entry->pad6 = 0;
    stdf_entry->date_stamp = 8 * (datev/10) + (datev % 10);
    stdf_entry->dasiz = elem_size;

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

        /* put appropriate values into field after allocating it */
        if (is_missing) {
            // allocate self deallocating scratch field
            field = (uint32_t *)alloca(num_elements * stdf_entry->dasiz/8);
            if ( 0 == EncodeMissingValue(field, record->data, num_elements, in_datyp, stdf_entry->dasiz, nbits) ) {
                field = record->data;
                Lib_Log(APP_LIBFST, APP_INFO, "%s: NO missing value, data type %d reset to %d\n", __func__, stdf_entry->datyp, datyp);
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
                        num_elements, nbits + 64 * Max(16, nbits), 0, xdf_stride, 1, 0, &tempfloat ,&dmin ,&dmax);
                    const int compressed_lng = armn_compress((unsigned char *)((uint32_t *)new_record->data + 5),
                                                             record->ni, record->nj, record->nk, nbits, 1);
                    if (compressed_lng < 0) {
                        stdf_entry->datyp = 1;
                        packfunc(field, (void*)new_record->data, (void*)&((uint32_t*)new_record->data)[3],
                            num_elements, nbits, 24, xdf_stride, 1, 0, &tempfloat ,&dmin ,&dmax);
                    } else {
                        int nbytes = 16 + compressed_lng;
                        const uint32_t num_word64 = (nbytes * 8 + 63) / 64;
                        const uint32_t num_word32 = W64TOWD(num_word64);
                        ((uint32_t*)new_record->data)[0] = num_word32;
                        RSF_Record_set_num_elements(new_record, num_word32, sizeof(uint32_t));
                    }
                } else {
                    packfunc(field, (void*)new_record->data, (void*)&((uint32_t*)new_record->data)[3],
                        num_elements, nbits, 24, xdf_stride, 1, 0, &tempfloat ,&dmin ,&dmax);
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
                if (xdf_short) {
                    compact_short(field, (void *) NULL, new_record->data, num_elements, nbits, 0, xdf_stride, 7);
                } else if (xdf_byte) {
                    compact_char(field, (void *) NULL, new_record->data, num_elements, nbits, 0, xdf_stride, 11);
                } else {
                    compact_integer(field, (void *) NULL, new_record->data, num_elements, nbits, 0, xdf_stride, 3);
                }
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
        char string[18];
        sprintf(string, "fst24 Write(%d)", file->iun);
        print_std_parms(stdf_entry, string, prnt_options, -1);
    }

    RSF_Free_record(new_record);

    return record_handle > 0 ? 0 : -1;
}

//! Write the given record into the given standard file
//! \return 0 if everything was a success, a negative error code otherwise
int32_t fst24_write(fst_file* file, const fst_record* record, int rewrit) {
    if (!fst24_is_open(file)) return ERR_NO_FILE;
    if (!fst24_record_is_valid(record)) return ERR_BAD_INIT;

    char typvar[FST_TYPVAR_LEN];
    char nomvar[FST_NOMVAR_LEN];
    char etiket[FST_ETIKET_LEN];
    char grtyp[FST_GTYP_LEN];

    switch (file->type) {
        case FST_RSF:
            return fst24_write_rsf(file, record);
        case FST_XDF:
            if (record->metadata != NULL) {
                Lib_Log(APP_LIBFST, APP_WARNING, "%s: Trying to write a record that contains extra metadata in an XDF file."
                        " This is not supported, we will ignore that metadata.\n", __func__);
            }
            strncpy(typvar, record->typvar, FST_TYPVAR_LEN);
            strncpy(nomvar, record->nomvar, FST_NOMVAR_LEN);
            strncpy(etiket, record->etiket, FST_ETIKET_LEN);
            strncpy(grtyp, record->grtyp, FST_GTYP_LEN);
            return c_fstecr_xdf(
                record->data, NULL, record->npak, file->iun, record->dateo, record->deet, record->npas,
                record->ni, record->nj, record->nk, record->ip1, record->ip2, record->ip3,
                typvar, nomvar, etiket, grtyp, record->ig1, record->ig2, record->ig3, record->ig4, record->datyp, 0);
        default:
            Lib_Log(APP_LIBFST, APP_ERROR, "%s: Unknown/invalid file type %d\n", __func__, file->type);
            return -1;
    } // End switch
}

//! Get basic information about the record with the given key (search or "directory" metadata)
//! \return TRUE if we were able to get the info, FALSE otherwise
int32_t fst24_get_record_from_key(
    const fst_file* file, //!< File to which the record belongs. Must be open
    const int64_t key,    //!< Key of the record we are looking for. Must be valid
    fst_record* record    //!< [out] Record information (no data or advanced metadata)
) {
    if (!fst24_is_open(file)) {
       Lib_Log(APP_LIBFST, APP_ERROR, "%s: File not open\n", __func__);
       return FALSE;
    }

    if (key < 0) {
        Lib_Log(APP_LIBFST, APP_ERROR, "%s: Invalid key\n", __func__);
        return FALSE;
    }

    *record = default_fst_record;
    record->handle = key;

    if (file->type == FST_RSF) {
        RSF_handle file_handle = FGFDT[file->file_index].rsf_fh;
        const RSF_record_info record_info = RSF_Get_record_info(file_handle, key);
        fill_with_dir_keys(record, (stdf_dir_keys*)record_info.meta);
        return TRUE;
    }
    else if (file->type == FST_XDF) {
        int addr, lng, idtyp;
        stdf_dir_keys record_meta_xdf;
        uint32_t* pkeys = (uint32_t *) &record_meta_xdf;
        pkeys += W64TOWD(1);
        c_xdfprm(key & 0xffffffff, &addr, &lng, &idtyp, pkeys, 16);

        fill_with_dir_keys(record, &record_meta_xdf);
        return TRUE;
    }

    Lib_Log(APP_LIBFST, APP_ERROR, "%s: Unknown/invalid file type %d\n", __func__, file->type);
    return FALSE;
}

//! Indicate a set of criteria that will be used whenever we will use "find next record" 
//! for the given file, within the FST 23 implementation.
//! If for some reason the user also makes calls to the old interface (FST 98) for the
//! same file (they should NOT), these criteria will be used if the file is RSF, but not with the
//! XDF backend.
//! \return TRUE if the inputs are valid (open file, OK criteria struct), FALSE otherwise
int32_t fst24_set_search_criteria(fst_file* file, const fst_record* criteria) {
    if (!fst24_is_open(file)) {
       Lib_Log(APP_LIBFST, APP_ERROR, "%s: File not open\n", __func__);
       return FALSE;
    }
    if (!fst24_record_is_valid(criteria)) {
       Lib_Log(APP_LIBFST, APP_ERROR, "%s: Invalid criteria\n", __func__);        
       return FALSE;
    }

    make_search_criteria(criteria,
                         &fstd_open_files[file->file_index].search_criteria,
                         &fstd_open_files[file->file_index].search_mask);

    const int64_t start_key = criteria->handle > 0 ? criteria->handle : 0;
    fstd_open_files[file->file_index].search_start_key = start_key;
    fstd_open_files[file->file_index].search_meta = criteria->metadata;
    fstd_open_files[file->file_index].search_done = 0;

    return TRUE;
}

int32_t fst24_get_record_by_index(const fst_file* file, const int64_t index, fst_record* record) {
    if (!fst24_is_open(file)) return ERR_NO_FILE;

    *record = default_fst_record;

    const int64_t num_records = fst24_get_num_records_single(file);
    if (index >= num_records) {
        if (file->next != NULL) return fst24_get_record_by_index(file->next, index - num_records, record);
        return ERR_OUT_RANGE;
    }

    if (file->type == FST_RSF) {
        RSF_handle file_handle = FGFDT[file->file_index].rsf_fh;
        const RSF_record_info record_info = RSF_Get_record_info_by_index(file_handle, index);
        fill_with_dir_keys(record, (stdf_dir_keys*)record_info.meta);
        record->handle = RSF_Make_key(RSF_File_slot(file_handle), index);
        return TRUE;
    }
    else if (file->type == FST_XDF) {
        const int page_id = index / ENTRIES_PER_PAGE;
        const int record_id = index - (page_id * ENTRIES_PER_PAGE);
        const int32_t key = MAKE_RND_HANDLE(page_id, record_id, file->file_index);

        int addr, lng, idtyp;
        stdf_dir_keys record_meta_xdf;
        uint32_t* pkeys = (uint32_t *) &record_meta_xdf;
        pkeys += W64TOWD(1);
        c_xdfprm(key & 0xffffffff, &addr, &lng, &idtyp, pkeys, 16);

        fill_with_dir_keys(record, &record_meta_xdf);
        record->handle = key;
        return TRUE;
    }

    return FALSE;
}

//! Find the next record in the given file that matches the previously set
//! criteria (either with a call to fst24_set_search_criteria or a search with explicit
//! criteria)
//! \return TRUE if a record was found, FALSE otherwise (not found, file not open, etc.)
int32_t fst24_find_next(
    fst_file* file,     //!< File we are searching. Must be open
    fst_record* result  //!< [out] Record information if found (no data or advanced metadata, unless included in search)
) {
    if (!fst24_is_open(file)) {
       Lib_Log(APP_LIBFST, APP_ERROR, "%s: File not open\n", __func__);
       return FALSE;
    }

    // Skip search, or search next file in linked list, if we were already done searching this file
    if (fstd_open_files[file->file_index].search_done == 1) {
        if (file->next != NULL) {
            return fst24_find_next(file->next, result);
        }
        return FALSE;
    } 

    int64_t key = -1;
    void *meta = NULL;

    // Depending on backend
    if (file->type == FST_RSF) {
        RSF_handle file_handle = FGFDT[file->file_index].rsf_fh;

        while(key == -1) {
            // Look for the record in the file
            if ((key = find_next_record(file_handle, &fstd_open_files[file->file_index])) < 0) {
                break;
            }
            
            // If metadata search is specified, look for a match or carry on looking
            if (fstd_open_files[file->file_index].search_meta) {
    //TODO:                meta=fst24_read_meta();
                if (Meta_Match(fstd_open_files[file->file_index].search_meta, meta, FALSE)) {
                    result->metadata = meta;
                    break;
                } else {
                    key = -1;
                }
            }  
        }    
    }
    else if (file->type == FST_XDF) {
        uint32_t* pkeys = (uint32_t *) &fstd_open_files[file->file_index].search_criteria;
        uint32_t* pmask = (uint32_t *) &fstd_open_files[file->file_index].search_mask;

        pkeys += W64TOWD(1);
        pmask += W64TOWD(1);

        const int32_t start_key = fstd_open_files[file->file_index].search_start_key & 0xffffffff;

        key = c_xdfloc2(file->iun, start_key, pkeys, 16, pmask);

        if (key >= 0) {
            fstd_open_files[file->file_index].search_start_key = key;
        }
        else {
            // Mark search as finished in this file if no record is found
            fstd_open_files[file->file_index].search_done = 1;
        }
    }
    else {
        Lib_Log(APP_LIBFST, APP_ERROR, "%s: Unknown/invalid file type %d\n", __func__, file->type);
        return FALSE;
    }

    if (key >= 0) {
        Lib_Log(APP_LIBFST, APP_DEBUG, "%s: (unit=%d) Found record at key 0x%x in file %p\n",
                __func__, file->iun, key, file);
        return fst24_get_record_from_key(file, key, result);
    }

    if (file->next != NULL) {
        // We're done searching this file, but there's another one in the linked list, so 
        // we need to setup the search in that one
        fstd_open_files[file->next->file_index].search_criteria = fstd_open_files[file->file_index].search_criteria;
        fstd_open_files[file->next->file_index].search_mask = fstd_open_files[file->file_index].search_mask;
        fstd_open_files[file->next->file_index].background_search_mask = fstd_open_files[file->file_index].background_search_mask;
        fstd_open_files[file->next->file_index].search_start_key = 0;
        fstd_open_files[file->next->file_index].search_done = 0;

        //TODO also copy search metadata!

        return fst24_find_next(file->next, result);
    }

    Lib_Log(APP_LIBFST, APP_DEBUG, "%s: No (more) record found matching the criteria\n", __func__);

    return FALSE;
}

//! Find the next record in a file that matches the given criteria
int32_t fst24_find(
    fst_file* file,             //!< [in] File in which we are looking. Must be open
    const fst_record* criteria, //!< [in] Search criteria
    fst_record* result          //!< [out] First record in the file that matches the criteria
) {
    fst24_set_search_criteria(file, criteria);
    return fst24_find_next(file, result);
}

//! Find all record that match the criteria specified with fst24_set_search_criteria
//! \return Number of records found, 0 if none or if error.
int32_t fst24_find_all(
    fst_file* file,               //!< File to search
    fst_record* results,          //!< [in,out] List of records found. Must be already allocated
    const int32_t max_num_results //!< [in] Size of the given list of records. We will stop looking if we find that many
) {
    if (!fst24_is_open(file)) {
       Lib_Log(APP_LIBFST, APP_ERROR, "%s: File not open\n", __func__);
       return FALSE;
    }

    // Reset start of search, because we want all
    fstd_open_files[file->file_index].search_start_key = 0;
    fstd_open_files[file->file_index].search_done = 0;

    for (int i = 0; i < max_num_results; i++) {
        if (!fst24_find_next(file, &(results[i]))) return i;
    }
    return max_num_results;
}


int32_t fst24_unpack_data(
    void* dest,
    void* source, //!< Should be const, but we might swap stuff in-place. It's supposed to be temporary anyway...
    const fst_record* record, //!< [in] Record information
    const int32_t skip_unpack,//!< Whether to just copy the data rather than unpacking it
    const int32_t stride      //!< Kept for compatibility with the XDF backend. Should be 1 if file is not XDF
) {
    uint32_t* dest_u32 = dest;
    uint32_t* source_u32 = source;

    // Get missing data flag
    const int has_missing = has_type_missing(record->datyp);
    // Suppress missing data flag
    const int32_t simple_datyp = record->datyp & ~FSTD_MISSING_FLAG;

    PackFunctionPointer packfunc;
    if (record->dasiz == 64) {
        packfunc = &compact_double;
    } else {
        packfunc = &compact_float;
    }

    // const size_t record_size_32 = record->rsz / 4;
    // size_t record_size = record_size_32;
    // if ((simple_datyp == FST_TYPE_FREAL) || (simple_datyp == FST_TYPE_REAL)) {
    //     record_size = (record->dasiz == 64) ? 2*record_size : record_size;
    // }

    const int multiplier = (simple_datyp == FST_TYPE_COMPLEX) ? 2 : 1;
    const int nelm = fst24_record_num_elem(record) * multiplier;

    double dmin = 0.0;
    double dmax = 0.0;

    const int bitmot = 32;
    int ier = 0;
    if (skip_unpack) {
        if (is_type_turbopack(simple_datyp)) {
            int lngw = ((int *)source)[0];
            // fprintf(stderr, "Debug+ lecture mode image lngw=%d\n", lngw);
            memcpy(dest, source, (lngw + 1) * sizeof(uint32_t));
        } else {
            int lngw = nelm * record->dasiz;
            if (simple_datyp == FST_TYPE_FREAL) lngw += 120;
            if (simple_datyp == FST_TYPE_FCHAR) lngw = record->ni * record->nj * 8;
            if (simple_datyp == FST_TYPE_IEEE_16) {
                int header_size, stream_size, p1out, p2out;
                c_float_packer_params(&header_size, &stream_size, &p1out, &p2out, nelm);
                lngw = (header_size + stream_size) * 8;
            }
            lngw = (lngw + bitmot - 1) / bitmot;
            memcpy(dest, source, lngw * sizeof(uint32_t));
        }
    } else {
        switch (simple_datyp) {
            case FST_TYPE_BINARY:
            {
                // Raw binary
                const int lngw = ((nelm * record->dasiz) + bitmot - 1) / bitmot;
                memcpy(dest, source, lngw * sizeof(uint32_t));
                break;
            }

            case FST_TYPE_FREAL:
            case FST_TYPE_FREAL | FST_TYPE_TURBOPACK:
            {
                // Floating Point
                double tempfloat = 99999.0;
                if (is_type_turbopack(record->datyp)) {
                    armn_compress((unsigned char *)(source_u32+ 5), record->ni, record->nj, record->nk,
                                  record->dasiz, 2);
                    packfunc(dest_u32, source_u32 + 1, source_u32 + 5, nelm, record->dasiz + 64 * Max(16, record->dasiz),
                             0, stride, FLOAT_UNPACK, 0, &tempfloat, &dmin, &dmax);
                } else {
                    packfunc(dest_u32, source_u32, source_u32 + 3, nelm, record->dasiz, 24, stride, FLOAT_UNPACK,
                             0, &tempfloat, &dmin, &dmax);
                }
                break;
            }

            case FST_TYPE_UNSIGNED:
            case FST_TYPE_UNSIGNED | FST_TYPE_TURBOPACK:
            {
                // Integer, short integer or byte stream
                const int offset = is_type_turbopack(record->datyp) ? 1 : 0;
                if (record->dasiz == 16) {
                    if (is_type_turbopack(record->datyp)) {
                        c_armn_compress_setswap(0);
                        const int nbytes = armn_compress((unsigned char *)(source_u32 + offset), record->ni,
                                                    record->nj, record->nk, record->dasiz, 2);
                        c_armn_compress_setswap(1);
                        memcpy(dest, source_u32 + offset, nbytes);
                    } else {
                        ier = compact_short(dest, (void *) NULL, (void *)(source_u32 + offset), nelm, record->dasiz,
                                            0, stride, 6);
                    }
                }  else if (record->dasiz == 8) {
                    if (is_type_turbopack(record->datyp)) {
                        c_armn_compress_setswap(0);
                        armn_compress((unsigned char *)(source_u32 + offset), record->ni, record->nj,
                                        record->nk, record->dasiz, 2);
                        c_armn_compress_setswap(1);
                        memcpy_16_8((int8_t *)dest, (int16_t *)(source_u32 + offset), nelm);
                    } else {
                        ier = compact_char(dest, (void *)NULL, (void *)source, nelm, 8, 0, stride, 10);
                    }
                } else {
                    if (is_type_turbopack(record->datyp)) {
                        c_armn_compress_setswap(0);
                        armn_compress((unsigned char *)(source_u32 + offset), record->ni, record->nj,
                                        record->nk, record->dasiz, 2);
                        c_armn_compress_setswap(1);
                        memcpy_16_32((int32_t *)dest, (int16_t *)(source_u32 + offset), record->dasiz, nelm);
                    } else {
                        ier = compact_integer(dest, (void *)NULL, source_u32 + offset, nelm, record->dasiz,
                                                0, stride, 2);
                    }
                }
                break;
            }

            case FST_TYPE_FCHAR:
            {
                // Character
                const int num_ints = (nelm + 3) / 4;
                ier = compact_integer(dest, (void *)NULL, source, num_ints, 32, 0, stride, 2);
                break;
            }

            case FST_TYPE_SIGNED: {
                // Signed integer
                if (record->dasiz == 16) {
                    ier = compact_short(dest, (void *)NULL, source, nelm, record->dasiz, 0, stride, 8);
                } else if (record->dasiz == 8) {
                    ier = compact_char(dest, (void *)NULL, source, nelm, record->dasiz, 0, stride, 12);
                } else {
                    ier = compact_integer(dest, (void *)NULL, source, nelm, record->dasiz, 0, stride, 4);
                }
                break;
            }

            case FST_TYPE_REAL:
            case FST_TYPE_COMPLEX: {
                // IEEE representation
                if ((downgrade_32) && (record->dasiz == 64)) {
                    // Downgrade 64 bit to 32 bit
#if defined(Little_Endian)
                    swap_words(source_u32, nelm);
#endif
                    float * ptr_real = (float *) dest;
                    double * ptr_double = (double *) source;
                    for (int i = 0; i < nelm; i++) {
                        *ptr_real++ = *ptr_double++;
                    }
                } else {
                    const int32_t f_one = 1;
                    const int32_t f_zero = 0;
                    const int32_t f_mode = 2;
                    const int32_t npak = -record->dasiz;
                    f77name(ieeepak)((int32_t *)dest, source, &nelm, &f_one, &npak, &f_zero, &f_mode);
                }

                break;
            }

            case FST_TYPE_IEEE_16:
            case FST_TYPE_IEEE_16 | FST_TYPE_TURBOPACK:
            {
                int nbits;
                int header_size, stream_size, p1out, p2out;
                c_float_packer_params(&header_size, &stream_size, &p1out, &p2out, nelm);
                if (is_type_turbopack(record->datyp)) {
                    armn_compress((unsigned char *)(source_u32 + 1 + header_size), record->ni, record->nj,
                                  record->nk, record->dasiz, 2);
                    c_float_unpacker((float *)dest, (int32_t *)(source_u32 + 1),
                                     (int32_t *)(source_u32 + 1 + header_size), nelm, &nbits);
                } else {
                    c_float_unpacker((float *)dest, (int32_t *)source, (int32_t *)(source_u32 + header_size),
                                     nelm, &nbits);
                }
                break;
            }

            case FST_TYPE_REAL | FST_TYPE_TURBOPACK:
            {
                // Floating point, new packers
                c_armn_uncompress32((float *)dest, (unsigned char *)(source_u32 + 1), record->ni, record->nj,
                                    record->nk, record->dasiz);
                break;
            }

            case FST_TYPE_STRING:
                // Character string
                ier = compact_char(dest, (void *)NULL, source, nelm, 8, 0, stride, 10);
                break;

            default:
                Lib_Log(APP_LIBFST, APP_ERROR, "%s: invalid datyp=%d\n", __func__, simple_datyp);
                return(ERR_BAD_DATYP);
        } /* end switch */
    }

    if (Lib_LogLevel(APP_LIBFST, NULL) >= APP_TRIVIAL) {
        Lib_Log(APP_LIBFST, APP_TRIVIAL, "%s: Read record with key 0x%x\n", __func__, record->handle);
        fst24_record_print(record);
    }

    if (has_missing) {
        // Replace "missing" data points with the appropriate values given the type of data (int/float)
        // if nbits = 64 and IEEE , set double
        // TODO review this logic
        int sz = record->dasiz;
        if (is_type_real(record->datyp) && record->dasiz == 64 ) sz = 64;
        DecodeMissingValue(dest, fst24_record_num_elem(record), record->datyp & 0x3F, sz);
    }

    return 0;
}

int32_t fst24_read_rsf(
    fst_file* file,
    //!> [in,out] Record for which we want to read data.
    //!> Must have a valid handle!
    //!> Must have already allocated its data buffer
    fst_record* record_fst,
    const int32_t skip_unpack //!< Whether to skip the unpacking process (e.g. if we just want to copy the record)
) {
    RSF_handle file_handle = FGFDT[file->file_index].rsf_fh;
    if (!RSF_Is_record_in_file(file_handle, record_fst->handle)) return ERR_BAD_HNDL;
    RSF_record* record_rsf = RSF_Get_record(file_handle, record_fst->handle);

    if (record_rsf == NULL) {
        Lib_Log(APP_LIBFST, APP_ERROR, "%s: Could not get record corresponding to key 0x%x\n",
                __func__, record_fst->handle);
        return ERR_BAD_HNDL;
    }

    fill_with_dir_keys(record_fst, (stdf_dir_keys*)record_rsf->meta);

    // Extract metadata form record if present
    record_fst->metadata=NULL;
    if (record_rsf->rec_meta > record_rsf->dir_meta) {
        record_fst->metadata=Meta_Parse((char*)((stdf_dir_keys*)record_rsf->meta+1));
        Lib_Log(APP_LIBFST,APP_DEBUG,"%s: Retrieved metadata with value '%s' for record key 0x%x\n",__func__,(char*)((stdf_dir_keys*)record_rsf->meta+1),record_fst->handle);
    }
    // fst24_record_print(record_fst)

    const int32_t ier = fst24_unpack_data(record_fst->data, record_rsf->data, record_fst, skip_unpack, 1);

    free(record_rsf);

    return ier;
}

int32_t fst24_read_xdf(
    fst_file* file,
    //!> [in,out] Record for which we want to read data.
    //!> Must have a valid handle!
    //!> Must have already allocated its data buffer
    fst_record* record
) {
    const int32_t key32 = record->handle & 0xffffffff;

    if (!c_xdf_handle_in_file(key32)) return ERR_BAD_HNDL;

    stdf_dir_keys stdf_entry;
    uint32_t * pkeys = (uint32_t *) &stdf_entry;
    pkeys += W64TOWD(1);

    {
        int addr, lng, idtyp;
        int ier = c_xdfprm(key32, &addr, &lng, &idtyp, pkeys, 16);
        if (ier < 0) return ier;
    }

    const int32_t h = c_fstluk_xdf(record->data, key32, &record->ni, &record->nj, &record->nk);
    if (h != key32) return ERR_NOT_FOUND;
    fill_with_dir_keys(record, &stdf_entry);
    return h;
}

//! Read data from file, for a given record
int32_t fst24_read(
    fst_file* file,
    fst_record* record //!< [in,out] Record for which we want to read data. Must have a valid handle!
) {
    if (!fst24_is_open(file)) {
       Lib_Log(APP_LIBFST, APP_ERROR, "%s: File not open\n",__func__);
       return ERR_NO_FILE;
    }

    if (!fst24_record_is_valid(record) || record->handle < 0) {
       Lib_Log(APP_LIBFST, APP_ERROR, "%s: Invalid record\n", __func__);        
       return -1;
    }

    if (record->data == NULL) {
        record->data = malloc(fst24_record_data_size(record) + 500);
        if (record->data == NULL) return ERR_MEM_FULL;
    }

    int32_t ret = -1;
    if (file->type == FST_RSF) {
            ret = fst24_read_rsf(file, record, 0);
    }
    else if (file->type == FST_XDF) {
        ret = fst24_read_xdf(file, record);
    }
    else {
        Lib_Log(APP_LIBFST, APP_ERROR, "%s: Unrecognized file type\n", __func__);
        ret = -1;
    }

    // Record in not in this file, so look in the next one (if present)
    if (ret == ERR_BAD_HNDL && file->next != NULL) {
        return fst24_read(file->next, record);
    }

    if (ret < 0) {
        Lib_Log(APP_LIBFST, APP_ERROR, "%s: Could not read record, ier = %d\n", __func__, ret);
        free(record->data);
        record->data = NULL;
        return -1;
    }

    return TRUE;
}

int32_t fst24_read_next(fst_file* file, fst_record* record) {
    if (!fst24_find_next(file, record)) {
        return FALSE;
    }

    return fst24_read(file, record);
}

int32_t fst24_link(fst_file** file, const int32_t num_files) {
    if (num_files <= 1) {
        Lib_Log(APP_LIBFST, APP_INFO, "%s: only passed %d files, nothing to link\n", __func__, num_files);
        return FALSE;
    }

    // Perform checks on all files before doing anything
    for (int i = 0; i < num_files; i++) {
        if (!fst24_is_open(file[i])) {
            Lib_Log(APP_LIBFST, APP_ERROR, "%s: File %d not open. We won't link anything.\n", __func__, i);
            return FALSE;
        }

        if (file[i]->next != NULL) {
            Lib_Log(APP_LIBFST, APP_ERROR,
                    "%s: File %d is already linked to another one. We won't link anything (else).\n", __func__, i);
            return FALSE;
        }
    }

    for (int i = 0; i < num_files - 1; i++) {
        file[i]->next = file[i + 1];
    }

    return TRUE;
}


int32_t fst24_unlink(fst_file* file) {
    if (!fst24_is_open(file)) {
       Lib_Log(APP_LIBFST, APP_ERROR, "%s: File not open\n", __func__);
       return FALSE;
    }

    fst_file* current = file;
    while (current->next != NULL) {
        fst_file* tmp = current->next;
        current->next = NULL;
        current = tmp;
    }

    return TRUE;
}
