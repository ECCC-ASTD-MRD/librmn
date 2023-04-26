//! \file fstd98_rsf.c
//! Implementation of the FSTD98 interface to RSF files

#include <App.h>
#include "fstd98_internal.h"

static inline size_t strlen_up_to(const char* string, const size_t max_length) {
    for (size_t i = 0; i < max_length; i++) {
        if (string[i] == '\0') return i;
    }
    return max_length;
}

//! Checks whether the given unit corresponds to an RSF file
//! \return 1 if the unit is an RSF, 0 if not, something else if there was an error
int32_t is_rsf(
    //! [in] Unit number associated to the file
    const int32_t iun,
    //! [out] (Optional) The index given to this file by fnom
    int32_t* out_index_fnom
) {
    const int index_fnom = fnom_index(iun);
    if (index_fnom == -1) {
        Lib_Log(APP_LIBFST, APP_ERROR, "%s: file (unit=%d) is not connected with fnom\n", __func__, iun);
        return(ERR_NO_FNOM);
    }

    if (out_index_fnom != NULL) *out_index_fnom = index_fnom;

    return FGFDT[index_fnom].attr.rsf == 1 ? 1 : 0;
}

//! Find the next record in a given file, according to the given parameters
//! \return Key of the record found (negative if error or nothing found)
static int64_t find_next_record(RSF_handle file_handle, fstd_usage_info* search_params) {

    stdf_dir_keys actual_mask;
    uint32_t* actual_mask_u32     = (uint32_t *)&actual_mask;
    uint32_t* mask_u32            = (uint32_t *)&search_params->search_mask;
    uint32_t* background_mask_u32 = (uint32_t *)&search_params->background_search_mask;
    for (int i = 0; i < search_params->num_criteria; i++) {
        actual_mask_u32[i] = mask_u32[i] & background_mask_u32[i];
    }
    const int64_t rsf_key = RSF_Lookup(file_handle,
                                       search_params->search_start_key,
                          (uint32_t *)&search_params->search_criteria,
                                       actual_mask_u32,
                                       search_params->num_criteria);
    if (rsf_key > 0) search_params->search_start_key = rsf_key;
    return rsf_key;
}

//! \copydoc c_fstecr
//! RSF version
int c_fstecr_rsf(
    //! [in] Field to write to the file
    void *field_in,
    //! [in] Work field (kept for backward compatibility)
    void *work,
    //! [in] Number of bits kept for the elements of the field
    int npak,
    //! [in] Unit number associated to the file in which to write the field
    int iun,
    //! [in] Index of the file given by fnom
    int index_fnom,
    //! [in] Date timestamp
    int date,
    //! [in] Length of the time steps in seconds
    int deet,
    //! [in] Time step number
    int npas,
    //! [in] First dimension of the data field
    int ni,
    //! [in] Second dimension of the data field
    int nj,
    //! [in] Thierd dimension of the data field
    int nk,
    //! [in] Vertical level
    int ip1,
    //! [in] Forecast hour
    int ip2,
    //! [in] User defined identifier
    int ip3,
    //! [in] Type of field (forecast, analysis, climatology)
    char *in_typvar,
    //! [in] Variable name
    char *in_nomvar,
    //! [in] Label
    char *in_etiket,
    //! [in] Type of geographical projection
    char *in_grtyp,
    //! [in] First grid descriptor
    int ig1,
    //! [in] Second grid descriptor
    int ig2,
    //! [in] Third grid descriptor
    int ig3,
    //! [in] Fourth grid descriptor
    int ig4,
    //! [in] Data type of elements
    int in_datyp_ori,
    //! [in] Rewrite existing record, append otherwise
    int rewrit
) {
    (void)work; // Unused

    // will be cancelled later if not supported or no missing values detected
    // missing value feature used flag
    int is_missing = in_datyp_ori & 64;
    // suppress missing value flag (64)
    int in_datyp = in_datyp_ori & 0xFFBF;
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

    RSF_handle file_handle = FGFDT[index_fnom].rsf_fh;

    if (file_handle.p == NULL) {
        Lib_Log(APP_LIBFST, APP_ERROR, "%s: file (unit=%d) is not open\n", __func__, iun);
        return ERR_NO_FILE;
    }

    // file_table_entry * fte = file_table[index];

    if (! FGFDT[index_fnom].attr.std) {
        Lib_Log(APP_LIBFST, APP_ERROR, "%s: file (unit=%d) is not a RPN standard file\n", __func__, iun);
        return ERR_NO_FILE;
    }

    if ((RSF_Get_mode(file_handle) & RSF_RO) == RSF_RO) {
        Lib_Log(APP_LIBFST, APP_ERROR, "%s: file (unit=%d) not open with write permission\n", __func__, iun);
        return ERR_NO_WRITE;
    }

    int nbits;
    if (npak == 0) {
        nbits = FTN_Bitmot;
    } else {
        nbits = (npak < 0) ? -npak : Max(1, FTN_Bitmot / Max(1, npak));
    }
    nk = Max(1, nk);
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
    VALID(ni, 1, NI_MAX, "ni")
    VALID(nj, 1, NJ_MAX, "nj")
    VALID(nk, 1, NK_MAX, "nk")
    VALID(deet, 0, DEET_MAX, "deet")
    VALID(npas, 0, NPAS_MAX, "npas")
    VALID(nbits, 1, NBITS_MAX, "nbits")
    VALID(ig1, 0, IG1_MAX, "ig1")
    VALID(ig2, 0, IG2_MAX, "ig2")
    VALID(ig3, 0, IG3_MAX, "ig3")
    VALID(ig4, 0, IG4_MAX, "ig4")
    VALID(ip1, 0, IP1_MAX, "ip1")
    VALID(ip2, 0, IP2_MAX, "ip2")
    VALID(ip3, 0, IP3_MAX, "ip3")
    VALID(ni * nj * nk * nbits / FTN_Bitmot, 0, MAX_RECORD_LENGTH, "record length > 128MB");

    /* Increment date by timestep size */
    unsigned int datev = date;
    int32_t f_datev = (int32_t) datev;
    if (( (long long) deet * npas) > 0) {
        long long deltat = (long long) deet * npas;
        double nhours = (double) deltat;
        nhours = nhours / 3600.;
        f77name(incdatr)(&f_datev, &f_datev, &nhours);
        datev = (unsigned int) f_datev;
    }

    if ((npak == 0) || (npak == 1)) {
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
    int num_word64;
    switch (datyp) {
        case 6: {
            int p1out;
            int p2out;
            c_float_packer_params(&header_size, &stream_size, &p1out, &p2out, ni * nj * nk);
            num_word64 = ((header_size+stream_size) * 8 + 63) / 64;
            header_size /= sizeof(int32_t);
            stream_size /= sizeof(int32_t);
            break;
        }

        case 8:
            num_word64 = 2 * ((ni * nj * nk *nbits + 63) / 64);
            break;

        case 129:
            /* 120 bits (floatpack header)+8, 32 bits (extra header) */
            num_word64 = (ni * nj * nk * Max(nbits, 16) + 128 + 32 + 63) / 64;
            break;

        case 130:
            /* 32 bits (extra header) */
            num_word64 = (ni * nj * nk * Max(nbits, 16) + 32 + 63) / 64;
            break;

        case 134: {
            int p1out;
            int p2out;
            c_float_packer_params(&header_size, &stream_size, &p1out, &p2out, ni * nj * nk);
            num_word64 = ((header_size+stream_size) * 8 + 32 + 63) / 64;
            stream_size /= sizeof(int32_t);
            header_size /= sizeof(int32_t);
            break;
        }

        default:
            num_word64 = (ni * nj * nk * nbits + 120 + 63) / 64;
            break;
    }

    /* Allocate new record */
    const int num_word32 = W64TOWD(num_word64);
    const size_t num_data_bytes = num_word32 * 4;
    const int32_t dir_metadata_size = (sizeof(stdf_dir_keys) + 3) / 4; // In 32-bit units
    RSF_record* record = RSF_New_record(file_handle, dir_metadata_size, dir_metadata_size, num_data_bytes, NULL, 0);
    if (record == NULL) {
        Lib_Log(APP_LIBFST, APP_FATAL, "%s: Unable to create new record with %ld bytes\n", __func__, num_data_bytes);
        return(ERR_MEM_FULL);
    }
    uint32_t* record_data = record->data;

    const int num_bits_per_word = 32;
    RSF_Record_set_num_elements(record, num_word32, sizeof(uint32_t));

    char typvar[3] = {' ', ' ', '\0'};
    strncpy(typvar, in_typvar, strlen_up_to(in_typvar, 2));
    char nomvar[5] = {' ', ' ', ' ', ' ', '\0'};
    strncpy(nomvar, in_nomvar, strlen_up_to(in_nomvar, 4));
    char etiket[13] = {' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ' , ' ' , '\0'};
    strncpy(etiket, in_etiket, strlen_up_to(in_etiket, 12));
    char grtyp[2] = {' ', '\0'};
    strncpy(grtyp, in_grtyp, strlen_up_to(in_grtyp, 1));

    /* set stdf_entry to address of buffer->data for building keys */
    stdf_dir_keys * stdf_entry = (stdf_dir_keys *) record->meta;
    
    stdf_entry->deleted = 0; // Unused by RSF
    stdf_entry->select = 0;  // Unused by RSF
    stdf_entry->lng = 0;     // Unused by RSF
    stdf_entry->addr = 0;    // Unused by RSF

    stdf_entry->deet = deet;
    stdf_entry->nbits = nbits;
    stdf_entry->ni = ni;
    stdf_entry->gtyp = grtyp[0];
    stdf_entry->nj = nj;
    /* propagate missing values flag */
    stdf_entry->datyp = datyp | is_missing;
    /* this value may be changed later in the code to eliminate improper flags */
    stdf_entry->nk = nk;
    stdf_entry->ubc = 0;
    stdf_entry->npas = npas;
    stdf_entry->pad7 = 0;
    stdf_entry->ig4 = ig4;
    stdf_entry->ig2a = ig2 >> 16;
    stdf_entry->ig1 = ig1;
    stdf_entry->ig2b = ig2 >> 8;
    stdf_entry->ig3 = ig3;
    stdf_entry->ig2c = ig2 & 0xff;
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
    stdf_entry->ip1 = ip1;
    stdf_entry->levtyp = 0;
    stdf_entry->ip2 = ip2;
    stdf_entry->pad5 = 0;
    stdf_entry->ip3 = ip3;
    stdf_entry->pad6 = 0;
    stdf_entry->date_stamp = 8 * (datev/10) + (datev % 10);

    if (rewrit) {
        Lib_Log(APP_LIBFST, APP_WARNING,
                "%s: The \"rewrite\" option is not available for RSF files. It will be ignored. "
                "(Set rewrit param to 0 if you don't want to see this warning)\n", __func__);
    }

    uint32_t * field = field_in;
    if (image_mode_copy) {
        /* no pack/unpack, used by editfst */
        if (datyp > 128) {
            /* first element is length */
            const int num_field_words32 = field[0];
            memcpy(record->data, field, (num_field_words32 + 1) * sizeof(uint32_t));
        } else {
            int num_field_bits;
            if (datyp == 6) {
                int p1out;
                int p2out;
                c_float_packer_params(&header_size, &stream_size, &p1out, &p2out, ni * nj * nk);
                num_field_bits = (header_size + stream_size) * 8;
            } else {
                num_field_bits = ni * nj * nk * nbits;
            }
            if (datyp == 1) num_field_bits += 120;
            if (datyp == 3) num_field_bits = ni * nj * 8;
            const int num_field_words32 = (num_field_bits + num_bits_per_word - 1) / num_bits_per_word;

            memcpy(record->data, field, num_field_words32 * sizeof(uint32_t));
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
            field = (uint32_t *)alloca(ni * nj * nk * sizefactor);
            if ( 0 == EncodeMissingValue(field, field_in, ni * nj * nk, in_datyp, nbits, xdf_byte, xdf_short, xdf_double) ) {
                field = field_in;
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
                const int32_t num_word32 = ((ni * nj * nk * nbits) + num_bits_per_word - 1) / num_bits_per_word;
                memcpy(record->data, field, num_word32 * sizeof(uint32_t));
                break;
            }

            case 1: case 129: {
                /* floating point */
                double tempfloat = 99999.0;
                if ((datyp > 128) && (nbits <= 16)) {
                    /* use an additional compression scheme */
                    /* nbits>64 flags a different packing */
                    // Use data pointer as uint32_t for compatibility with XDF format
                    packfunc(field, (void *)&((uint32_t *)record->data)[1], (void *)&((uint32_t *)record->data)[5],
                        ni * nj * nk, nbits + 64 * Max(16, nbits), 0, xdf_stride, 1, 0, &tempfloat);
                    const int compressed_lng = armn_compress((unsigned char *)((uint32_t *)record->data + 5), ni, nj, nk, nbits, 1);
                    if (compressed_lng < 0) {
                        stdf_entry->datyp = 1;
                        packfunc(field, (void*)record->data, (void*)&((uint32_t*)record->data)[3],
                            ni * nj * nk, nbits, 24, xdf_stride, 1, 0, &tempfloat);
                    } else {
                        int nbytes = 16 + compressed_lng;
                        const uint32_t num_word64 = (nbytes * 8 + 63) / 64;
                        const uint32_t num_word32 = W64TOWD(num_word64);
                        ((uint32_t*)record->data)[0] = num_word32;
                        RSF_Record_set_num_elements(record, num_word32, sizeof(uint32_t));
                    }
                } else {
                    packfunc(field, (void*)record->data, (void*)&((uint32_t*)record->data)[3],
                        ni * nj * nk, nbits, 24, xdf_stride, 1, 0, &tempfloat);
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
                            memcpy(record_data + offset, (void *)field, ni * nj * nk * 2);
                        } else if (xdf_byte) {
                            stdf_entry->nbits = Min(8, nbits);
                            nbits = stdf_entry->nbits;
                            memcpy_8_16((int16_t *)(record_data + offset), (void *)field, ni * nj * nk);
                        } else {
                            memcpy_32_16((short *)(record_data + offset), (void *)field, nbits, ni * nj * nk);
                        }
                        c_armn_compress_setswap(0);
                        const int compressed_lng = armn_compress((unsigned char *)&((uint32_t *)record->data)[offset],
                                                                 ni, nj, nk, nbits, 1);
                        c_armn_compress_setswap(1);
                        if (compressed_lng < 0) {
                            stdf_entry->datyp = 2;
                            compact_integer((void *)field, (void *) NULL, &((uint32_t *)record->data)[offset],
                                ni * nj * nk, nbits, 0, xdf_stride, 1);
                        } else {
                            const int nbytes = 4 + compressed_lng;
                            const uint32_t num_word64 = (nbytes * 8 + 63) / 64;
                            const uint32_t num_word32 = W64TOWD(num_word64);
                            ((uint32_t *)record->data)[0] = num_word32;
                            RSF_Record_set_num_elements(record, num_word32, sizeof(uint32_t));
                        }
                    } else {
                        if (xdf_short) {
                            stdf_entry->nbits = Min(16, nbits);
                            nbits = stdf_entry->nbits;
                            compact_short((void *)field, (void *) NULL, &((uint32_t *)record->data)[offset],
                                ni * nj * nk, nbits, 0, xdf_stride, 5);
                        } else if (xdf_byte) {
                            compact_char((void *)field, (void *) NULL, record->data,
                                ni * nj * nk, Min(8, nbits), 0, xdf_stride, 9);
                            stdf_entry->nbits = Min(8, nbits);
                            nbits = stdf_entry->nbits;
                        } else {
                            compact_integer((void *)field, (void *) NULL, &((uint32_t *)record->data)[offset],
                                ni * nj * nk, nbits, 0, xdf_stride, 1);
                        }
                    }
                }
                break;


            case 3: case 131:
                /* character */
                {
                    int nc = (ni * nj + 3) / 4;
                    if (datyp == 131) {
                        Lib_Log(
                            APP_LIBFST, APP_WARNING, "%s: extra compression not available, data type %d reset to %d\n",
                            __func__, stdf_entry->datyp, 3);
                        datyp = 3;
                        stdf_entry->datyp = 3;
                    }
                    compact_integer(field, (void *) NULL, record->data, nc, 32, 0, xdf_stride, 1);
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
                    field3 = (uint32_t *)alloca(ni * nj * nk * sizeof(int));
                    short * s_field = (short *)field;
                    signed char * b_field = (signed char *)field;
                    if (xdf_short) for (int i = 0; i < ni * nj * nk;i++) { field3[i] = s_field[i]; };
                    if (xdf_byte)  for (int i = 0; i < ni * nj * nk;i++) { field3[i] = b_field[i]; };
                }
                compact_integer(field3, (void *) NULL, record->data, ni * nj * nk, nbits, 0, xdf_stride, 3);
#else
                if (xdf_short) {
                    compact_short(field, (void *) NULL, record->data, ni * nj * nk, nbits, 0, xdf_stride, 7);
                } else if (xdf_byte) {
                    compact_char(field, (void *) NULL, record->data, ni * nj * nk, nbits, 0, xdf_stride, 11);
                } else {
                    compact_integer(field, (void *) NULL, record->data, ni * nj * nk, nbits, 0, xdf_stride, 3);
                }
#endif
                break;

            case 5: case 8: case 133:  case 136:
                /* IEEE and IEEE complex representation */
                {
                    int32_t f_ni = (int32_t) ni;
                    int32_t f_njnk = nj * nk;
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
                            (unsigned char *)&((uint32_t *)record->data)[1], (void *)field, ni, nj, nk, nbits);
                        if (compressed_lng < 0) {
                            stdf_entry->datyp = 5;
                            f77name(ieeepak)((int32_t *)field, record->data, &f_ni, &f_njnk, &f_minus_nbits, &f_zero, &f_one);
                        } else {
                            const int nbytes = 16 + compressed_lng;
                            const uint32_t num_word64 = (nbytes * 8 + 63) / 64;
                            const uint32_t num_word32 = W64TOWD(num_word64);
                            ((uint32_t *)record->data)[0] = num_word32;
                            RSF_Record_set_num_elements(record, num_word32, sizeof(uint32_t));
                        }
                    } else {
                        if (datyp == 8) f_ni = f_ni * 2;
                        f77name(ieeepak)((int32_t *)field, record->data, &f_ni, &f_njnk, &f_minus_nbits, &f_zero, &f_one);
                    }
                }
                break;

            case 6: case 134:
                /* floating point, new packers */

                if ((datyp > 128) && (nbits <= 16)) {
                    /* use an additional compression scheme */
                    c_float_packer((void *)field, nbits, &((int32_t *)record->data)[1],
                                   &((int32_t *)record->data)[1+header_size], ni * nj * nk);
                    const int compressed_lng = armn_compress(
                        (unsigned char *)&((uint32_t *)record->data)[1+header_size], ni, nj, nk, nbits, 1);
                    if (compressed_lng < 0) {
                        stdf_entry->datyp = 6;
                        c_float_packer((void *)field, nbits, record->data, &((int32_t *)record->data)[header_size], ni * nj * nk);
                    } else {
                        const int nbytes = 16 + (header_size*4) + compressed_lng;
                        const uint32_t num_word64 = (nbytes * 8 + 63) / 64;
                        const uint32_t num_word32 = W64TOWD(num_word64);
                        ((uint32_t *)record->data)[0] = num_word32;
                        RSF_Record_set_num_elements(record, num_word32, sizeof(uint32_t));
                    }
                } else {
                    c_float_packer((void *)field, nbits, record->data,
                                   &((int32_t *)record->data)[header_size], ni * nj * nk);
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
                compact_char(field, (void *) NULL, record->data, ni * nj * nk, 8, 0, xdf_stride, 9);
                break;

            default:
                Lib_Log(APP_LIBFST, APP_ERROR, "%s: (unit=%d) invalid datyp=%d\n", __func__, iun, datyp);
                return ERR_BAD_DATYP;
        } /* end switch */
    } /* end if image mode copy */

    /* write record to file and add entry to directory */
    const int64_t record_handle = RSF_Put_record(file_handle, record, num_data_bytes);
    if (Lib_LogLevel(APP_LIBFST,NULL) > APP_INFO) {
        char string[12];
        sprintf(string, "Write(%d)", iun);
        print_std_parms(stdf_entry, string, prnt_options, -1);
    }

    xdf_double = 0;
    xdf_short = 0;
    xdf_byte = 0;
    return record_handle > 0 ? 0 : -1;
}

//! \copydoc c_fstnbr
//! RSF version
int c_fstnbr_rsf(
    //! [in] Index given by fnom associated with the file
    const int index_fnom
) {
    if (FGFDT[index_fnom].rsf_fh.p != NULL)
        return RSF_Get_num_records_at_open(FGFDT[index_fnom].rsf_fh);

    Lib_Log(APP_LIBFST, APP_ERROR, "%s: file at index %d is not open\n", __func__, index_fnom);
    return ERR_NO_FILE;
}

//! Wrapper on the RSF call to open a file. Also sets appropriate flag on the fnom-given slot
//! \return 0 if the opening was successful, -1 otherwise
int c_fstouv_rsf(
    //!> [in] Index given to this file by fnom
    const int index_fnom,
    //!> [in] Opening mode (read/write/append)
    const int mode,
    //!> [in] I don't know what "appl" is
    char appl[5]
) {
    FGFDT[index_fnom].attr.rsf = 1;
    const int32_t meta_dim = (sizeof(stdf_dir_keys) + 3)/ sizeof(int32_t); // In 32-bit units
    FGFDT[index_fnom].rsf_fh = RSF_Open_file(FGFDT[index_fnom].file_name, mode, meta_dim, appl, NULL);
    
    if (RSF_Valid_handle(FGFDT[index_fnom].rsf_fh)) {
        Lib_Log(APP_LIBFST, APP_DEBUG, "%s: Opened file %s, mode %d, fnom index %d, meta dim 0x%x\n", __func__,
                FGFDT[index_fnom].file_name, mode, index_fnom, meta_dim);
        return 0;
    }

    Lib_Log(APP_LIBFST, APP_WARNING, "%s: Failed to open file %s, mode %d, fnom index %d, meta dim 0x%x\n",
            __func__, FGFDT[index_fnom].file_name, mode, index_fnom, meta_dim);

    return -1;
}

//! \copydoc c_fstinfx
//! RSF version
int c_fstinfx_rsf(
    //! [in] Handle from which the search begins.  Start from the beginning when handle = -2
    const int handle,
    //! [in] Unit number associated to the file
    const int iun,
    //! [in] Index of the file given by fnom
    const int index_fnom,
    //! [out] Dimension 1 of the data field
    int * const ni,
    //! [out] Dimension 2 of the data field
    int * const nj,
    //! [out] Dimension 3 of the data field
    int * const nk,
    //! [in] Validity date
    const int datev,
    //! [in] Label
    const char * const in_etiket,
    //! [in] Vertical level
    const int ip1,
    //! [in] Forecast hour
    const int ip2,
    //! [in] User defined identifier
    const int ip3,
    //! [in] Type of field
    const char * const in_typvar,
    //! [in] Variable name
    const char * const in_nomvar
) {
    unsigned int u_datev = datev;

    char etiket[13] = {' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', '\0'};
    char typvar[3] = {' ', ' ', '\0'};
    char nomvar[5] = {' ', ' ', ' ', ' ', '\0'};

    strncpy(etiket, in_etiket, strlen_up_to(in_etiket, 12));
    strncpy(typvar, in_typvar, strlen_up_to(in_typvar, 2));
    strncpy(nomvar, in_nomvar, strlen_up_to(in_nomvar, 4));
    Lib_Log(APP_LIBFST, APP_DEBUG, "%s: iun %d recherche: datev=%d etiket=[%s] ip1=%d ip2=%d ip3=%d typvar=[%s] "
            "nomvar=[%s]\n", __func__, iun, datev, etiket, ip1, ip2, ip3, typvar, nomvar);

    RSF_handle file_handle = FGFDT[index_fnom].rsf_fh;

    if (file_handle.p == NULL) {
        Lib_Log(APP_LIBFST, APP_ERROR, "%s: file (unit=%d) is not open\n", __func__, iun);
        return ERR_NO_FILE;
    }

    // Initialize search parameters
    fstd_open_files[index_fnom].search_start_key = 0;
    stdf_dir_keys *search_criteria  = &fstd_open_files[index_fnom].search_criteria;
    stdf_dir_keys *search_mask = &fstd_open_files[index_fnom].search_mask;

    // Reset search mask for initialization
    {
        uint32_t *pmask = (uint32_t *) search_mask;
        for (uint i = 0; i < (sizeof(stdf_dir_keys) / sizeof(uint32_t)); i++) {
            pmask[i] = -1;
        }
    }

    search_mask->pad1 = 0;
    search_mask->pad2 = 0;
    search_mask->pad3 = 0;
    search_mask->pad4 = 0;
    search_mask->pad5 = 0;
    search_mask->pad6 = 0;
    search_mask->pad7 = 0;
    search_mask->deleted = 0;
    search_mask->select = 0;
    search_mask->lng = 0;
    search_mask->addr = 0;
    search_mask->deet = 0;
    search_mask->nbits = 0;
    search_mask->ni = 0;
    search_mask->gtyp = 0;
    search_mask->nj = 0;
    search_mask->datyp = 0;
    search_mask->nk = 0;
    search_mask->ubc = 0;
    search_mask->npas = 0;
    search_mask->ig4 = 0;
    search_mask->ig2a = 0;
    search_mask->ig1 = 0;
    search_mask->ig2b = 0;
    search_mask->ig3 = 0;
    search_mask->ig2c = 0;
    search_mask->levtyp = 0;

    search_criteria->date_stamp = 8 * (u_datev/10) + (u_datev % 10);
    search_mask->date_stamp &= ~(0x7);
    if (datev == -1) search_mask->date_stamp = 0;

    search_criteria->ip1 = ip1;
    if ((ip1 == -1) || (ip1s_flag)) search_mask->ip1 = 0;

    search_criteria->ip2 = ip2;
    if ((ip2 == -1) || (ip2s_flag)) search_mask->ip2 = 0;

    search_criteria->ip3 = ip3;
    if ((ip3 == -1) || (ip3s_flag)) search_mask->ip3 = 0;

    search_criteria->nomvar = (ascii6(nomvar[0]) << 18) |
                              (ascii6(nomvar[1]) << 12) |
                              (ascii6(nomvar[2]) <<  6) |
                              (ascii6(nomvar[3]));
    if (search_criteria->nomvar == 0) search_mask->nomvar = 0;

    search_criteria->typvar = (ascii6(typvar[0]) << 6) |
                              (ascii6(typvar[1]));
    if (search_criteria->typvar == 0) search_mask->typvar = 0;

    search_criteria->etik15 = (ascii6(etiket[0]) << 24) |
                              (ascii6(etiket[1]) << 18) |
                              (ascii6(etiket[2]) << 12) |
                              (ascii6(etiket[3]) <<  6) |
                              (ascii6(etiket[4]));

    search_criteria->etik6a = (ascii6(etiket[5]) << 24) |
                              (ascii6(etiket[6]) << 18) |
                              (ascii6(etiket[7]) << 12) |
                              (ascii6(etiket[8]) <<  6) |
                              (ascii6(etiket[9]));

    search_criteria->etikbc = (ascii6(etiket[10]) <<  6) |
                              (ascii6(etiket[11]));

    if ((search_criteria->etik15 == 0) && (search_criteria->etik6a == 0)) {
        search_mask->etik15 = 0;
        search_mask->etik6a = 0;
        search_mask->etikbc = 0;
    }

    // Perform the search itself
    int64_t rsf_key = -1;
    if (handle == -2) {
        /* means handle not specified */
        rsf_key = find_next_record(file_handle, &fstd_open_files[index_fnom]);
    } else {
        // Verify that the given handle (record key) belongs to the given file
        if (handle > 0) {
            const uint32_t file_slot = RSF_Key64_to_file_slot(handle);
            if ((int32_t)file_slot != RSF_File_slot(file_handle)) {
                Lib_Log(APP_LIBFST, APP_ERROR, "%s: invalid handle=%d, or iun=%d\n", __func__, handle, iun);
                return(ERR_BAD_HNDL);
            }
        }

        rsf_key = find_next_record(file_handle, &fstd_open_files[index_fnom]);
    }
    int32_t lhandle = RSF_Key32(rsf_key);

    if (rsf_key < 0) {
        Lib_Log(APP_LIBFST, APP_TRIVIAL, "%s: (unit=%d) record not found, errcode=%ld\n", __func__, iun, rsf_key);
        if (ip1s_flag || ip2s_flag || ip3s_flag) init_ip_vals();
        return (int32_t)rsf_key;
    }

    Lib_Log(APP_LIBFST, APP_DEBUG, "%s: (unit=%d) Found record at key 0x%x\n", __func__, iun, lhandle);

    RSF_record_info record_info = RSF_Get_record_info(file_handle, rsf_key);

    // Continue looking until we have a match. Why is this not part of the RSF lookup function????
    if (ip1s_flag || ip2s_flag || ip3s_flag) {
        int nomatch = 1;
        while ((lhandle >=  0) && (nomatch)) {
            nomatch = 0;
            if ((ip1s_flag) && (ip1 >= 0)) {
                if (ip_is_equal(ip1, search_criteria->ip1, 1) == 0) {
                    nomatch = 1;
                } else if ((ip2s_flag) && (ip2 >= 0)) {
                    if (ip_is_equal(ip2, search_criteria->ip2, 2) == 0) {
                        nomatch = 1;
                    } else if ((ip3s_flag) && (ip3 >= 0)) {
                        if (ip_is_equal(ip3, search_criteria->ip3, 3) == 0) {
                            nomatch = 1;
                        }
                    }
                }
            }
            if (nomatch) {
                rsf_key = find_next_record(file_handle, &fstd_open_files[index_fnom]);
                lhandle = RSF_Key32(rsf_key);
                if (rsf_key >= 0) {
                    record_info = RSF_Get_record_info(file_handle, rsf_key);
                }
            }
        }
    }
    
    // Not sure what this does, or if it is useful for RSF...
    if (ip1s_flag || ip2s_flag || ip3s_flag) {
        /* arranger les masques de recherches pour un fstsui */
        if (ip1s_flag) search_mask->ip1 = 0xFFFFFFF;
        if (ip2s_flag) search_mask->ip2 = 0xFFFFFFF;
        if (ip3s_flag) search_mask->ip3 = 0xFFFFFFF;
        init_ip_vals();
    }

    const stdf_dir_keys* record_meta = (const stdf_dir_keys*)record_info.meta;

    *ni = record_meta->ni;
    *nj = record_meta->nj;
    *nk = record_meta->nk;

    return lhandle;
}

//! \copydoc c_fstfrm
//! RSF version
int c_fstfrm_rsf(
    //! [in] Unit number associated to the file
    int iun,
    //! [in] Index of the file given by fnom
    const int index_fnom
) {
    RSF_handle file_handle = FGFDT[index_fnom].rsf_fh;

    if (file_handle.p == NULL) {
        Lib_Log(APP_LIBFST, APP_ERROR, "%s: file (unit=%d) is not open\n", __func__, iun);
        return ERR_NO_FILE;
    }

    const int32_t status = RSF_Close_file(file_handle);

    return status == 1 ? 0 : -1;
}

//! \copydoc c_fstlirx
//! RSF version
int c_fstlirx_rsf(
    //! [out] Field to be read
    void *field,
    //! [in] Record handle from which the search begins
    int handle,
    //! [in] Unit number associated to the file
    int iun,
    //! [in] Index of the file given by fnom
    const int index_fnom,
    //! [out] First of the data field
    int *ni,
    //! [out] Second of the data field
    int *nj,
    //! [out] Third of the data field
    int *nk,
    //! [in] Validity date
    int datev,
    //! [in] Label
    char *etiket,
    //! [in] Vertical level
    int ip1,
    //! [in] Forecast hour
    int ip2,
    //! [in] User defined identifier
    int ip3,
    //! [in] Type of field
    char *typvar,
    //! [in] Variable name
    char *nomvar
) {
    RSF_handle file_handle = FGFDT[index_fnom].rsf_fh;

    if (file_handle.p == NULL) {
        Lib_Log(APP_LIBFST, APP_ERROR, "%s: file (unit=%d) is not open\n", __func__, iun);
        return ERR_NO_FILE;
    }

    const int32_t key32 = c_fstinfx_rsf(handle, iun, index_fnom, ni, nj, nk, datev, etiket, ip1, ip2, ip3, typvar, nomvar);
    if (key32 < 0) {
        Lib_Log(APP_LIBFST,APP_WARNING,"%s: (unit=%d) record not found, errcode=%d\n", __func__, iun, key32);
        return key32;
    }

    const int ier = c_fstluk_rsf(field, file_handle, key32, ni, nj, nk);
    if (ier < 0) {
        return ier;
    } else {
        return key32;
    }
}

//! \copydoc c_fstluk
//! RSF version
int c_fstluk_rsf(
    //! [out] Pointer to where the data read will be placed.  Must be allocated!
    void * const vfield,
    //! Handle to the file we are reading
    RSF_handle file_handle,
    //! [in] Handle (key) of the record to be read
    const int key32,
    //! [out] Dimension 1 of the data field
    int * const ni,
    //! [out] Dimension 2 of the data field
    int * const nj,
    //! [out] Dimension 3 of the data field
    int * const nk
) {
    uint32_t *field=vfield;
    
    RSF_record* record = RSF_Get_record(file_handle, RSF_Key64(key32));

    if (record == NULL) {
        Lib_Log(APP_LIBFST, APP_ERROR, "%s: Could not get record corresponding to key 0x%x (0x%x)\n",
                __func__, key32, RSF_Key64(key32));
        return ERR_BAD_HNDL;
    }

    stdf_dir_keys* stdf_entry = (stdf_dir_keys*)record->meta;
    uint32_t* record_data = record->data;

    *ni = stdf_entry->ni;
    *nj = stdf_entry->nj;
    *nk = stdf_entry->nk;
    // Get missing data flag
    int has_missing = stdf_entry->datyp & 64;
    // Suppress missing data flag
    stdf_entry->datyp = stdf_entry->datyp & 0xBF;
    xdf_datatyp = stdf_entry->datyp;

    PackFunctionPointer packfunc;
    if (xdf_double) {
        packfunc = &compact_double;
    } else {
        packfunc = &compact_float;
    }

    const size_t record_size_32 = record->rsz / 4;
    size_t record_size = record_size_32;
    if ((xdf_datatyp == 1) || (xdf_datatyp == 5)) {
        record_size = (xdf_double) ? 2*record_size : record_size;
    }

    const int multiplier = (stdf_entry->datyp == 8) ? 2 : 1;
    int nelm = stdf_entry->ni * stdf_entry->nj * stdf_entry->nk * multiplier;

    int npak = -(stdf_entry->nbits);
    const int bitmot = 32;
    int ier = 0;
    if (image_mode_copy) {
        // No pack/unpack, used by editfst
        if (stdf_entry->datyp > 128) {
            int lngw = ((int *)record->data)[0];
            // fprintf(stderr, "Debug+ lecture mode image lngw=%d\n", lngw);
            for (int i = 0; i < lngw + 1; i++) {
                field[i] =  ((uint32_t*)record->data)[i];
            }
        } else {
            int lngw = nelm * stdf_entry->nbits;
            if (stdf_entry->datyp == 1) lngw += 120;
            if (stdf_entry->datyp == 3) lngw = *ni * *nj * 8;
            if (stdf_entry->datyp == 6) {
                int header_size, stream_size, p1out, p2out;
                c_float_packer_params(&header_size, &stream_size, &p1out, &p2out, nelm);
                lngw = (header_size + stream_size) * 8;
            }
            lngw = (lngw + bitmot - 1) / bitmot;
            for (int i = 0; i < lngw; i++) {
                field[i] =  ((uint32_t*)record->data)[i];
            }
        }
    } else {
        switch (stdf_entry->datyp) {
            case 0: {
                // Raw binary
                int lngw = ((nelm * stdf_entry->nbits) + bitmot - 1) / bitmot;
                for (int i = 0; i < lngw; i++) {
                    field[i] = record_data[i];
                }
                break;
            }

            case 1:
            case 129: {
                // Floating Point
                double tempfloat = 99999.0;
                if (stdf_entry->datyp > 128) {
                    armn_compress((unsigned char *)(record_data + 5), *ni, *nj, *nk, stdf_entry->nbits, 2);
                    packfunc(field, record_data + 1, record_data + 5, nelm, stdf_entry->nbits + 64 * Max(16, stdf_entry->nbits),
                             0, xdf_stride, FLOAT_UNPACK, 0, &tempfloat);
                } else {
                    packfunc(field, record_data, record_data + 3, nelm, stdf_entry->nbits, 24, xdf_stride, FLOAT_UNPACK, 0, &tempfloat);
                }
                break;
            }

            case 2:
            case 130:
                {
                    // Integer, short integer or byte stream
                    int offset = stdf_entry->datyp > 128 ? 1 : 0;
                    if (xdf_short) {
                        if (stdf_entry->datyp > 128) {
                            c_armn_compress_setswap(0);
                            int nbytes = armn_compress((unsigned char *)(record_data + offset), *ni, *nj, *nk, stdf_entry->nbits, 2);
                            c_armn_compress_setswap(1);
                            memcpy(field, record_data + offset, nbytes);
                        } else {
                            ier = compact_short(field, (void *) NULL, record_data + offset, nelm, stdf_entry->nbits, 0, xdf_stride, 6);
                        }
                    }  else if (xdf_byte) {
                        if (stdf_entry->datyp > 128) {
                            c_armn_compress_setswap(0);
                            armn_compress((unsigned char *)(record_data + offset), *ni, *nj, *nk, stdf_entry->nbits, 2);
                            c_armn_compress_setswap(1);
                            memcpy_16_8((int8_t *)field, (int16_t *)(record_data + offset), nelm);
                        } else {
                            ier = compact_char(field, (void *) NULL, record_data, nelm, 8, 0, xdf_stride, 10);
                        }
                    } else {
                        if (stdf_entry->datyp > 128) {
                            c_armn_compress_setswap(0);
                            armn_compress((unsigned char *)(record_data + offset), *ni, *nj, *nk, stdf_entry->nbits, 2);
                            c_armn_compress_setswap(1);
                            memcpy_16_32((int32_t *)field, (int16_t *)(record_data + offset), stdf_entry->nbits, nelm);
                        } else {
                            ier = compact_integer(field, (void *) NULL, record_data + offset, nelm, stdf_entry->nbits, 0, xdf_stride, 2);
                        }
                    }
                    break;
                }

            case 3: {
                // Character
                int nc = (nelm + 3) / 4;
                ier = compact_integer(field, (void *) NULL, record->data, nc, 32, 0, xdf_stride, 2);
                break;
            }


            case 4: {
                // Signed integer
#if defined(use_old_signed_pack_unpack_code)
                int *field_out;
                short *s_field_out;
                signed char *b_field_out;
                if (xdf_short || xdf_byte) {
                    field_out = alloca(nelm * sizeof(int));
                    s_field_out = (short *)field;
                    b_field_out = (signed char *)field;
                } else {
                    field_out = (int32_t *)field;
                }
                ier = compact_integer(field_out, (void *) NULL, record->data, nelm, stdf_entry->nbits, 0, xdf_stride, 4);
                if (xdf_short) {
                    for (int i = 0; i < nelm; i++) {
                        s_field_out[i] = field_out[i];
                    }
                }
                if (xdf_byte) {
                    for (int i = 0; i < nelm; i++) {
                        b_field_out[i] = field_out[i];
                    }
                }
#else
                if (xdf_short) {
                    ier = compact_short(field, (void *) NULL, record->data, nelm, stdf_entry->nbits, 0, xdf_stride, 8);
                } else if (xdf_byte) {
                    ier = compact_char(field, (void *) NULL, record->data, nelm, stdf_entry->nbits, 0, xdf_stride, 12);
                } else {
                    ier = compact_integer(field, (void *) NULL, record->data, nelm, stdf_entry->nbits, 0, xdf_stride, 4);
                }
#endif
                break;
            }

            case 5:
            case 8: {
                // IEEE representation
                register int32_t temp32, *src, *dest;
                if ((downgrade_32) && (stdf_entry->nbits == 64)) {
                    // Downgrade 64 bit to 32 bit
                    float * ptr_real = (float *) field;
                    double * ptr_double = (double *) record->data;
#if defined(Little_Endian)
                    src = (int32_t *) record->data;
                    dest = (int32_t *) record->data;
                    for (int i = 0; i < nelm; i++) {
                        temp32 = *src++;
                        *dest++ = *src++;
                        *dest++ = temp32;
                    }
#endif
                    for (int i = 0; i < nelm; i++) {
                        *ptr_real++ = *ptr_double++;
                    }
                } else {
                    int32_t f_one = 1;
                    int32_t f_zero = 0;
                    int32_t f_mode = 2;
                    f77name(ieeepak)((int32_t *)field, record->data, &nelm, &f_one, &npak, &f_zero, &f_mode);
                }

                break;
            }

            case 6:
            case 134: {
                // Floating point, new packers
                int nbits;
                int header_size, stream_size, p1out, p2out;
                c_float_packer_params(&header_size, &stream_size, &p1out, &p2out, nelm);
                if (stdf_entry->datyp > 128) {
                    armn_compress((unsigned char *)(record_data + 1 + header_size), *ni, *nj, *nk, stdf_entry->nbits, 2);
                    c_float_unpacker((float *)field, (int32_t *)(record_data + 1), (int32_t *)(record_data + 1 + header_size), nelm, &nbits);
                } else {
                    c_float_unpacker((float *)field, (int32_t *)record_data, (int32_t *)(record_data + header_size), nelm, &nbits);
                }
                break;
            }

            case 133: {
                // Floating point, new packers
                c_armn_uncompress32((float *)field, (unsigned char *)(record_data + 1), *ni, *nj, *nk, stdf_entry->nbits);
                break;
            }

            case 7:
                // Character string
                ier = compact_char(field, (void *) NULL, record->data, nelm, 8, 0, xdf_stride, 10);
                break;

            default:
                Lib_Log(APP_LIBFST, APP_ERROR, "%s: invalid datyp=%d\n", __func__, stdf_entry->datyp);
                return(ERR_BAD_DATYP);
        } /* end switch */
    }

    if (Lib_LogLevel(APP_LIBFST,NULL)>=APP_TRIVIAL) {
        char string[11];
        Lib_Log(APP_LIBFST, APP_TRIVIAL, "%s: Read record with key 0x%x\n", __func__, key32);
        stdf_entry->datyp = stdf_entry->datyp | has_missing;
        print_std_parms(stdf_entry, string, prnt_options, -1);
    }
    if (has_missing) {
        // Replace "missing" data points with the appropriate values given the type of data (int/float)
        // if nbits = 64 and IEEE , set xdf_double
        if ((stdf_entry->datyp & 0xF) == 5 && stdf_entry->nbits == 64 ) xdf_double = 1;
        DecodeMissingValue(field , (*ni) * (*nj) * (*nk) , xdf_datatyp & 0x3F, xdf_byte, xdf_short, xdf_double);
    }

    xdf_double = 0;
    xdf_short = 0;
    xdf_byte = 0;

    free(record);

    if (ier < 0) return ier;

    return key32;
}

//! Delete a record
//! RSF version
int c_fsteff_rsf(
    //! Handle to the file that contains the record to delete
    RSF_handle file_handle,
    //! Handle of the record to delete
    int handle
) {
    // Suppress compilation warnings
    (void)file_handle;
    (void)handle;

    Lib_Log(APP_LIBFST, APP_ERROR, "%s: Cannot delete a record from a RSF-type file\n", __func__);
    return ERR_BAD_FTYPE;
}

//! \copydoc c_fstinl
//! RSF version
int c_fstinl_rsf(
    //! [in] Unit number associated to the file in which to search
    int iun,
    //! [in] Index of the file given by fnom
    const int index_fnom,
    //! [out] First dimension of the field
    int *ni,
    //! [out] Second dimension of the field
    int *nj,
    //! [out] Third dimension of the field
    int *nk,
    //! [in] Validity date
    int datev,
    //! [in] Label
    char *etiket,
    //! [in] Vertical level
    int ip1,
    //! [in] Forecast hour
    int ip2,
    //! [in] User defined identifier
    int ip3,
    //! [in] Type of field
    char *typvar,
    //! [in] Variable name
    char *nomvar,
    //! [out] List of handles of the matching records
    int *liste,
    //! [out] Number of elements for the list (number of matching records)
    int *infon,
    //! [in] List size (maximum number of matches)
    int nmax
) {
    Lib_Log(APP_LIBFST, APP_DEBUG,
            "%s: iun %d recherche: datev=%d etiket=[%s] ip1=%d ip2=%d ip3=%d typvar=[%s] nomvar=[%s]\n",
            __func__, iun, datev, etiket, ip1, ip2, ip3, typvar, nomvar);

    int handle = c_fstinfx_rsf(-2, iun, index_fnom, ni, nj, nk, datev, etiket, ip1, ip2, ip3, typvar, nomvar);

    int nijkmax = (*ni) * (*nj) * (*nk);
    int nimax = *ni;
    int njmax = *nj;
    int nkmax = *nk;

    int nfound = 0;
    while ((handle >= 0) && (nfound < nmax)) {
        liste[nfound] = handle;
        nfound++;
        if (nfound >= nmax) break;
        handle = c_fstsui_rsf(iun, index_fnom, ni, nj, nk);
        if ( ((*ni) * (*nj) * (*nk)) > nijkmax ) {
            nimax = *ni;
            njmax = *nj;
            nkmax = *nk;
            nijkmax = (*ni) * (*nj) * (*nk);
        }
    }

    *ni = nimax;
    *nj = njmax;
    *nk = nkmax;
    *infon = nfound;

    Lib_Log(APP_LIBFST, APP_DEBUG, "%s: nombre trouve=%d nmax=%d\n", __func__, nfound, nmax);

    while ( (handle = c_fstsui_rsf(iun, index_fnom, ni, nj, nk)) >= 0 ) {
        nfound++;
    }

    if (nfound > nmax) {
        Lib_Log(APP_LIBFST, APP_ERROR, "%s: number of records found (%d) > nmax specified (%d)\n",
                 __func__, nfound, nmax);
        return -nfound;
    } else {
        return 0;
    }
}

//! \copydoc c_fstlis
//! RSF version
int c_fstlis_rsf(
    //! [out] Field to be read
    void *field,
    //! [in] Unit number associated to the file
    int iun,
    //! [in] Index of the file given by fnom
    const int index_fnom,
    //! [out] First of the data field
    int *ni,
    //! [out] Second of the data field
    int *nj,
    //! [out] Third of the data field
    int *nk
) {
    RSF_handle file_handle = FGFDT[index_fnom].rsf_fh;

    if (file_handle.p == NULL) {
        Lib_Log(APP_LIBFST, APP_ERROR, "%s: file (unit=%d) is not open\n", __func__, iun);
        return ERR_NO_FILE;
    }

    /* Get the next record that matches the last search criterias */
    const int64_t rsf_key = find_next_record(file_handle, &fstd_open_files[index_fnom]);

    if (rsf_key < 0) {
        Lib_Log(APP_LIBFST, APP_DEBUG, "%s: record not found, errcode=%ld\n", __func__, rsf_key);
        return (int32_t)rsf_key;
    }

    return c_fstluk_rsf(field, file_handle, RSF_Key32(rsf_key), ni, nj, nk);
}

//! Helper function for c_fstmsq
static inline char isignore(const char chr) {
    return (chr == '*') ? 0 : 0x3f;
}


//! Helper function for c_fstmsq
static inline char inv_isignore(const char chr) {
    return (chr == 0x3f) ? ' ' : '*';
}

//! \copydoc c_fstmsq
//! RSF version
int c_fstmsq_rsf(
    //! [in] Unit number associated to the file
    const int iun,
    //! [in] Index of the file given by fnom
    const int index_fnom,
    //! [in,out] Mask for vertical level
    int *mip1,
    //! [in,out] Mask for the forecast hour
    int *mip2,
    //! [in,out] Mask for the user defined identifier
    int *mip3,
    //! [in,out] Mask for the label
    char *metiket,
    //! [in] Operation: Set when 0, Get otherwise
    const int getmode
) {
    RSF_handle file_handle = FGFDT[index_fnom].rsf_fh;

    if (file_handle.p == NULL) {
        Lib_Log(APP_LIBFST, APP_ERROR, "%s: file (unit=%d) is not open\n", __func__, iun);
        return ERR_NO_FILE;
    }

    stdf_dir_keys* search_mask = &fstd_open_files[index_fnom].background_search_mask;

    if (getmode == 0) {
        search_mask->ip1 = ~(*mip1) & 0xfffffff;
        search_mask->ip2 = ~(*mip2) & 0xfffffff;
        search_mask->ip3 = ~(*mip3) & 0xfffffff;
        search_mask->etik15 =
            (isignore(metiket[0]) << 24) |
            (isignore(metiket[1]) << 18) |
            (isignore(metiket[2]) << 12) |
            (isignore(metiket[3]) <<  6) |
            (isignore(metiket[4]));
        search_mask->etik6a =
            (isignore(metiket[5]) << 24) |
            (isignore(metiket[6]) << 18) |
            (isignore(metiket[7]) << 12) |
            (isignore(metiket[8]) <<  6) |
            (isignore(metiket[9]));
        search_mask->etikbc =
            (isignore(metiket[10]) <<  6) |
            (isignore(metiket[11]));
    } else {
        *mip1 = ~(search_mask->ip1) & 0xfffffff;
        *mip2 = ~(search_mask->ip2) & 0xfffffff;
        *mip3 = ~(search_mask->ip3) & 0xfffffff;
        for (int i = 0; i <= 4; i++) {
            metiket[i] = inv_isignore( ((search_mask->etik15 >> ((4-i)*6)) & 0x3f) );
        }

        for (int i = 5; i <= 9; i++) {
            metiket[i] = inv_isignore( ((search_mask->etik6a >> ((9-i)*6)) & 0x3f) );
        }

        metiket[10] = inv_isignore( ((search_mask->etikbc >> 6) & 0x3f) );
        metiket[11] = inv_isignore((search_mask->etikbc  & 0x3f));
        metiket[12] = '\0';
    }

    return 0;
}

//! Get the number of valid records (excluding deleted records, including ones added since opening) in a file
//! RSF version
int c_fstnbrv_rsf(
    //! [in] Index of the file given by fnom
    const int index_fnom
) {
    if (FGFDT[index_fnom].rsf_fh.p != NULL)
        return RSF_Get_num_records(FGFDT[index_fnom].rsf_fh);

    Lib_Log(APP_LIBFST, APP_ERROR, "%s: file at index %d is not open\n", __func__, index_fnom);
    return ERR_NO_FILE;
}

//! Get all the descriptors of a record
//! RSF version
int c_fstprm_rsf(
    //! Handle to the file we are reading
    RSF_handle file_handle,
    //! [in] Handle of the record from which to retreive the descriptors
    int handle,
    //! [out] Origin date time stamp
    int *dateo,
    //! [out] Duration of time steps in seconds
    int *deet,
    //! [out] Time step number
    int *npas,
    //! [out] First dimension of the data field
    int *ni,
    //! [out] Second dimension of the data field
    int *nj,
    //! [out] Third dimension of the data field
    int *nk,
    //! [out] Number of bits kept for each elements of the field
    int *nbits,
    //! [out] Data type of the elements
    int *datyp,
    //! [out] Vertical level
    int *ip1,
    //! [out] Forecast hour
    int *ip2,
    //! [out] User defined identifier
    int *ip3,
    //! [out] Type of field (forecast, analysis, climatology)
    char *typvar,
    //! [out] Variable name
    char *nomvar,
    //! [out] Label
    char *etiket,
    //! [out] Type of geographical projection
    char *grtyp,
    //! [out] First grid descriptor
    int *ig1,
    //! [out] Second grid descriptor
    int *ig2,
    //! [out] Third grid descriptor
    int *ig3,
    //! [out] Fourth grid descriptor
    int *ig4,
    //! [out] Starting word address
    int *swa,
    //! [out] Record length
    int *lng,
    //! [out] Delete flag
    int *dltf,
    //! [out] Unused bit count
    int *ubc,
    //! [out] Extra parameter
    int *extra1,
    //! [out] Extra parameter
    int *extra2,
    //! [out] Extra parameter
    int *extra3
) {
    if (file_handle.p == NULL) {
        Lib_Log(APP_LIBFST, APP_ERROR, "%s: file is not open\n", __func__);
        return ERR_NO_FILE;
    }

    // Get record information from handle
    int64_t rsf_key = RSF_Key64(handle);
    RSF_record_info record_info = RSF_Get_record_info(file_handle, rsf_key);
    if (record_info.rl <= 0) {
        Lib_Log(APP_LIBFST, APP_ERROR, "%s: Could not retrieve record with key %ld\n", __func__, rsf_key);
        return -1;
    }

    stdf_dir_keys *stdf_entry = (stdf_dir_keys *)record_info.meta;
    stdf_special_parms cracked;
    crack_std_parms(stdf_entry, &cracked);

    *ni = stdf_entry->ni;
    *nj = stdf_entry->nj;
    *nk = stdf_entry->nk;
    *dateo = cracked.date_stamp;
    *deet = stdf_entry->deet;
    *npas = stdf_entry->npas;
    *nbits = stdf_entry->nbits;
    *datyp = stdf_entry->datyp;
    *ip1 = stdf_entry->ip1;
    *ip2 = stdf_entry->ip2;
    *ip3 = stdf_entry->ip3;
    *ig1 = stdf_entry->ig1;
    *ig2 = cracked.ig2;
    *ig3 = stdf_entry->ig3;
    *ig4 = stdf_entry->ig4;
    *dltf = stdf_entry->deleted;
    *ubc = stdf_entry->ubc;

    // Address and size in 64-bit units
    const uint64_t max_val = 0x8fffffff;
    const uint64_t addr_word64        = ((record_info.wa - 1) >> 3) + 1;
    const uint64_t record_size_word64 = ((record_info.rl - 1) >> 3) + 1;
    *swa = addr_word64 <= max_val ? (int32_t)addr_word64 : 0;
    *lng = record_size_word64 & max_val;

    if (addr_word64 > max_val || record_size_word64 > max_val) {
        Lib_Log(APP_LIBFST, APP_WARNING,
                "%s: record address or size (in 64-bit units) is larger than what can be handled by this interface. "
                "Address = %lu, size = %lu, max = %d\n",
                __func__, addr_word64, record_size_word64, (int32_t)max_val);
    }

    /* new, use to be undefined */
    *extra1 = cracked.date_valid;
    *extra2 = 0;
    *extra3 = 0;

    strncpy(typvar, cracked.typvar, strlen_up_to(typvar, 2));
    strncpy(nomvar, cracked.nomvar, strlen_up_to(nomvar, 4));
    strncpy(etiket, cracked.etiket, strlen_up_to(etiket, 12));
    strncpy(grtyp, cracked.gtyp, strlen_up_to(grtyp, 1));

    return 0;
}

//! \copydoc c_fstsui
//! RSF version
int c_fstsui_rsf(
    //! [in] Unit number associated to the file
    int iun,
    //! [in] Index of the file given by fnom
    const int index_fnom,
    //! [out] Dimension 1 of the data field
    int *ni,
    //! [out] Dimension 2 of the data field
    int *nj,
    //! [out] Dimension 3 of the data field
    int *nk
) {
    // uint32_t *primk = NULL;

    RSF_handle file_handle = FGFDT[index_fnom].rsf_fh;

    if (file_handle.p == NULL) {
        Lib_Log(APP_LIBFST, APP_ERROR, "%s: file (unit=%d) is not open\n", __func__, iun);
        return ERR_NO_FILE;
    }

    /* position to the next record that matches the last search criterias */
    const int64_t rsf_key = find_next_record(file_handle, &fstd_open_files[index_fnom]);
    if (rsf_key < 0) {
        Lib_Log(APP_LIBFST, APP_DEBUG, "%s: record not found, errcode=%ld\n", __func__, rsf_key);
        return (int32_t)rsf_key;
    }

    RSF_record_info record_info = RSF_Get_record_info(file_handle, rsf_key);
    stdf_dir_keys* stdf_entry = (stdf_dir_keys*)record_info.meta;
    *ni = stdf_entry->ni;
    *nj = stdf_entry->nj;
    *nk = stdf_entry->nk;

    const int32_t xdf_handle = RSF_Key32(rsf_key);
    return xdf_handle;
}

//! \copydoc c_fstvoi
//! RSF version
int c_fstvoi_rsf(
    //! [in] Unit number associated to the file
    const int iun,
    //! [in] Index of the file given by fnom
    const int index_fnom,
    //! [in] List of fields to print
    const char * const options
) {
    Lib_Log(APP_LIBFST, APP_ERROR, "%s: function not implemented yet\n", __func__);
    return ERR_WRONG_FTYPE;
}
