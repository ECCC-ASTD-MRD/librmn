//! \file fst24_backend_rsf.c
//! RSF-specific implementation of the fst24 file API.
//!
//! This file contains the code that is specific to the RSF backend. The common
//! fst24 API (in fst24_file.c) dispatches to these functions when the file is
//! of type RSF. The low-level RSF format engine itself lives in rsf.c.

#include <stdlib.h>
#include <string.h>

#include <App.h>

#include "fst_internal.h"
#include "fst24_file_internal.h"
#include "fst24_record_internal.h"
#include "fst98_internal.h"
#include "rsf_internal.h"
#include "fst24_backend_rsf.h"
#include "rmn/Meta.h"

extern const char * const FST_TYPE_NAMES[];

//! Fill fst_record attributes from given RSF metadata
int32_t update_attributes_from_rsf_info(
    fst_record* record,     //!< [in,out] Record struct where to put the information
    const int64_t key,      //!< Key where to find the record
    const RSF_record_info* record_info  //!< Record info from directory
) {
    fill_with_search_meta(record, (const search_metadata*)record_info->meta, FST_RSF);

    record->do_not_touch.stored_data_size = (record_info->data_size + 3) / 4;
    record->do_not_touch.handle = key;
    record->num_meta_bytes = record_info->rec_meta * sizeof(uint32_t);
    record->file_index = RSF_Key64_to_index(key);
    if (record_info->rec_type == RT_DEL) record->do_not_touch.deleted = 1;

    record->file_offset = record_info->wa;
    record->total_stored_bytes = record_info->rl;

    if (record_info->rl > fst24_record_data_size(record) + record->num_meta_bytes + sizeof(RSF_record)) { // With some small buffer
        Lib_Log(APP_LIBFST, APP_ERROR,
            "%s: Record data on disk (%llu) is larger than computed value (%lld)\n",
            __func__, record_info->rl, fst24_record_data_size(record) + record->num_meta_bytes + sizeof(RSF_record));
        if (Lib_LogLevel(APP_LIBFST, NULL) >= APP_DEBUG) {
            fst24_record_print(record);
        }
        return FALSE;
    }

    return TRUE;
}

//! Write a record in an RSF file
int32_t fst24_write_rsf(
    //! RSF handle to the file where we are writing
    RSF_handle rsf_file,
    //! [in,out] Record we want to write. Will be updated as we adjust some parameters
    fst_record * const record,
    //! Compaction parameter. When in doubt, leave at 1
    const int32_t stride
) {
    //! Sometimes the requested writing parameters are not compatible and are changed. If that is
    //! the case, the given fst_record struct will be updated.
    //! \return TRUE (1) if writing was successful, 0 or a negative number otherwise

    if (rsf_file.p == NULL) {
        Lib_Log(APP_LIBFST, APP_ERROR, "%s: file is not open\n", __func__);
        return ERR_NO_FILE;
    }

    if ((RSF_Get_mode(rsf_file) & RSF_RO) == RSF_RO) {
        Lib_Log(APP_LIBFST, APP_ERROR, "%s: file not open with write permission\n", __func__);
        return ERR_NO_WRITE;
    }

    // Pointer to the data to be written. The data may be processed before encoding/compression, so this pointer
    // could change. This avoids modifying the original data.
    void* field = record->data;
    float* field_f = NULL; // float version of the data
    uint32_t* field_missing = NULL; // data with missing values transformed

    const int num_elements = record->ni * record->nj * record->nk;
    const int num_bits_per_word = 32;

    // will be cancelled later if not supported or no missing values detected
    // missing value feature used flag
    int has_missing = record->data_type & FSTD_MISSING_FLAG;
    // suppress missing value flag (64)
    int in_data_type = record->data_type & ~FSTD_MISSING_FLAG;
    if (is_type_complex(in_data_type)) {
        if (record->data_type != FST_TYPE_COMPLEX) {
           Lib_Log(APP_LIBFST, APP_WARNING, "%s: compression and/or missing values not supported, "
                   "data type %d reset to %d (complex)\n", __func__, record->data_type, 8);
        }
        // missing values not supported for complex type
        has_missing = 0;
        // extra compression not supported for complex type
        in_data_type = FST_TYPE_COMPLEX;
    }

    // 512+256+32+1 no interference with turbo pack (128) and missing value (64) flags
    int data_type = in_data_type == FST_TYPE_MAGIC ? 1 : in_data_type;

    // flag 64 bit IEEE
    const int force_64 = (record->pack_bits == 64 && (is_type_real(in_data_type) || is_type_complex(in_data_type)));
    int8_t elem_size = force_64 ? 64 : record->data_bits;

    if (is_type_real(in_data_type) && elem_size == 64) {
        if (record->pack_bits <= 32) {
            // We convert now from double to float
            elem_size = 32;
            field_f = (float*)malloc(fst24_record_num_elem(record) * sizeof(float));
            double* data_d = record->data;
            for (int i = 0; i < fst24_record_num_elem(record); i++) {
                field_f[i] = (float)data_d[i];
            }
            field = field_f;
        }
        else {
            if (record->pack_bits != 64) {
                static int warned_once_1 = 0;
                if (!warned_once_1) {
                    warned_once_1 = 1;
                    Lib_Log(APP_LIBFST, APP_WARNING, "%s: Requested %d packed bits for 64-bit reals, but we can only do"
                            " 64 or less than 32. Will store 64 bits.\n", __func__, record->pack_bits);
                }
                record->pack_bits = 64;
            }
            // For regular double precision, there is no turbopack, and we only take FST_TYPE_REAL_IEEE
            in_data_type = FST_TYPE_REAL_IEEE;
            data_type = FST_TYPE_REAL_IEEE;
        }

    }

    PackFunctionPointer packfunc;
    double dmin = 0.0;
    double dmax = 0.0;
    if (elem_size == 64 || in_data_type == FST_TYPE_MAGIC) {
        packfunc = (PackFunctionPointer) &compact_p_double;
    } else {
        packfunc = (PackFunctionPointer) &compact_p_float;
    }

    if ( (record->data_type == (FST_TYPE_REAL_IEEE | FST_TYPE_TURBOPACK)) && (record->pack_bits > 32) ) {
        static int warned_once_2 = 0;
        if (!warned_once_2) {
            warned_once_2 = 1;
            Lib_Log(APP_LIBFST, APP_WARNING, "%s: extra compression not supported for IEEE when nbits > 32, "
                    "data type 133 reset to 5 (IEEE)\n", __func__);
        }
        // extra compression not supported
        in_data_type = FST_TYPE_REAL_IEEE;
        data_type = FST_TYPE_REAL_IEEE;
    }

    if (is_type_real(data_type) && record->pack_bits <= 32 && record->data_bits == 64) {
        // Will convert the double to float before doing anything else
        record->data_bits = 32;
    }

    if (is_type_turbopack(data_type) && record->nk > 1) {
        Lib_Log(APP_LIBFST, APP_WARNING, "%s: Turbo compression not supported for 3D data.\n", __func__);
        data_type &= ~FST_TYPE_TURBOPACK;
    }

    if ((is_type_integer(data_type) && record->data_bits == 64) && (is_type_turbopack(data_type) || record->pack_bits != 64)) {
        Lib_Log(APP_LIBFST, APP_WARNING, "%s: Compression not supported for 64-bit integer types\n", __func__);
        data_type &= ~FST_TYPE_TURBOPACK;
        record->pack_bits = 64;
    }

    if ((base_fst_type(in_data_type) == FST_TYPE_REAL_OLD_QUANT) && ((record->pack_bits == 31) || (record->pack_bits == 32)) && !image_mode_copy) {
        // R32 to E32 automatic conversion
        data_type = FST_TYPE_REAL_IEEE;
        if (is_type_turbopack(in_data_type)) data_type |= FST_TYPE_TURBOPACK;
        record->pack_bits = 32;
    }

    if ((data_type == (FST_TYPE_REAL_OLD_QUANT | FST_TYPE_TURBOPACK)) && !image_mode_copy) {
        static int warn_old_quant_turbo = 1;
        if (warn_old_quant_turbo == 1) {
            Lib_Log(APP_LIBFST, APP_WARNING,
                "%s: Extra compression not available for type %d (FST_TYPE_REAL_OLD_QUANT). "
                "Switching to type %d (FST_TYPE_REAL)\n",
                __func__, FST_TYPE_REAL_OLD_QUANT, FST_TYPE_REAL);
            warn_old_quant_turbo = 0;
        }
        data_type = FST_TYPE_REAL | FST_TYPE_TURBOPACK;
    }

    // validate range of arguments
    if (fst24_record_validate_params(record) != 0) {
        Lib_Log(APP_LIBFST, APP_ERROR, "%s: Invalid value for certain parameters\n", __func__);
        return ERR_OUT_RANGE;
    }

    // Increment date by timestep size
    record->datev = get_valid_date32(record->dateo, record->deet, record->npas);

    //TODO Remove any reference to remap_table?
    if (! image_mode_copy) {
        for (int i = 0; i < nb_remap; i++) {
            if (data_type == remap_table[0][i]) {
                data_type = remap_table[1][i];
            }
        }
    }

    // no extra compression if nbits > 16
    if ((record->pack_bits > 16) && (data_type != (FST_TYPE_REAL_IEEE | FST_TYPE_TURBOPACK))) data_type = base_fst_type(data_type);
    if ((data_type == FST_TYPE_REAL) && (record->pack_bits > 32) && (record->data_bits == 64)) {
        data_type = FST_TYPE_REAL_IEEE;
        record->pack_bits = 64;
    }
    else if ((data_type == FST_TYPE_REAL) && (record->pack_bits > 24)) {
        Lib_Log(APP_LIBFST, APP_TRIVIAL, "%s: nbits > 24, writing E32 instead of F%2d\n", __func__, record->pack_bits);
        data_type = FST_TYPE_REAL_IEEE;
        record->pack_bits = 32;
    }
    if ((data_type == FST_TYPE_REAL) && (record->pack_bits > 16)) {
        Lib_Log(APP_LIBFST, APP_TRIVIAL, "%s: nbits > 16, writing R%2d instead of F%2d\n", __func__, record->pack_bits, record->pack_bits);
        data_type = FST_TYPE_REAL_OLD_QUANT;
    }

    if (base_fst_type(data_type) == FST_TYPE_REAL_IEEE && (record->pack_bits < 16)) {
        Lib_Log(APP_LIBFST, APP_ERROR, "%s: nbits = %d, but anything less than 16 is not available for IEEE 32-bit float\n",
                __func__, record->pack_bits);
        return -1;
    }

    // Determine size of data to be stored
    int header_size;
    int stream_size;
    size_t num_word32;
    // Size if the data were packed as plain (non-turbopack)
    const size_t plain_num_word32 = W64TOWD((num_elements * record->pack_bits + 120 + 63) / 64);
    if (image_mode_copy) {
        if (is_type_turbopack(data_type)) {
            // first element is length
            const int num_field_words32 = ((uint32_t*)record->data)[0] + 1;
            num_word32 = num_field_words32;
        }
        else {
            int num_field_bits;
            if (data_type == FST_TYPE_REAL) {
                int p1out;
                int p2out;
                c_float_packer_params(&header_size, &stream_size, &p1out, &p2out, num_elements);
                num_field_bits = (header_size + stream_size) * 8;
            } else {
                num_field_bits = num_elements * record->pack_bits;
            }
            if (data_type == FST_TYPE_REAL_OLD_QUANT) num_field_bits += 120;
            if (data_type == FST_TYPE_CHAR) num_field_bits = record->ni * record->nj * 8;
            const int num_field_words32 = (num_field_bits + num_bits_per_word - 1) / num_bits_per_word;
            num_word32 = num_field_words32;
        }
    }
    else {
        switch (data_type) {
            case FST_TYPE_REAL: {
                int p1out;
                int p2out;
                c_float_packer_params(&header_size, &stream_size, &p1out, &p2out, num_elements);
                num_word32 = W64TOWD(((header_size+stream_size) * 8 + 63) / 64);
                header_size /= sizeof(int32_t);
                stream_size /= sizeof(int32_t);
                break;
            }

            case FST_TYPE_COMPLEX:
                num_word32 = W64TOWD(2 * ((num_elements * record->pack_bits + 63) / 64));
                break;

            case FST_TYPE_REAL_OLD_QUANT | FST_TYPE_TURBOPACK:
                // 120 bits (floatpack header)+8, 32 bits (extra header)
                num_word32 = W64TOWD((num_elements * Max(record->pack_bits, 16) + 128 + 32 + 63) / 64);
                break;

            case FST_TYPE_UNSIGNED | FST_TYPE_TURBOPACK:
                // 32 bits (extra header)
                num_word32 = W64TOWD((num_elements * Max(record->pack_bits, 16) + 32 + 63) / 64);
                break;

            case FST_TYPE_REAL | FST_TYPE_TURBOPACK: {
                int p1out;
                int p2out;
                c_float_packer_params(&header_size, &stream_size, &p1out, &p2out, num_elements);
                num_word32 = W64TOWD(((header_size+stream_size) * 8 + 32 + 63) / 64);
                stream_size /= sizeof(int32_t);
                header_size /= sizeof(int32_t);
                break;
            }

            default:
                num_word32 = plain_num_word32;
                break;
        }
    }

    // Allocate new record
    const size_t num_data_bytes = num_word32 * 4;
    const size_t dir_metadata_size = (sizeof(search_metadata) + 3) / 4; // In 32-bit units

    // New json metadata
    char *metastr = NULL;
    int  metalen = 0;
    size_t rec_metadata_size = dir_metadata_size;
    uint16_t ext_metadata_size = 0;
    if (record->metadata) {
       if (!image_mode_copy) {
          fst24_bounds(record,&dmin,&dmax);
          if (!Meta_DefData(record->metadata, record->ni, record->nj, record->nk, FST_TYPE_NAMES[data_type],
                            "lorenzo", record->pack_bits, record->data_bits, dmin, dmax)) {
             Lib_Log(APP_LIBFST, APP_ERROR, "%s: Invalid metadata profile\n", __func__);
             return(ERR_METADATA);
          }
       }
       if ((metastr = Meta_Stringify(record->metadata,JSON_C_TO_STRING_PLAIN)) != NULL) {
          metalen = strlen(metastr) + 1; // Include null character
          ext_metadata_size = (metalen + 3) / 4; // Round up to 4 bytes
          rec_metadata_size += ext_metadata_size;
       }
    }

    record->do_not_touch.num_search_keys = dir_metadata_size;
    record->do_not_touch.extended_meta_size = ext_metadata_size;
    record->do_not_touch.stored_data_size = num_word32;
    record->do_not_touch.unpacked_data_size = fst24_record_data_size(record) / sizeof(uint32_t); // 32-bit units

    record->num_meta_bytes = rec_metadata_size * sizeof(uint32_t);

    size_t total_payload_bytes = num_data_bytes + record->data_blocks.map_size * sizeof(uint32_t);
    RSF_record* new_record = RSF_New_record(
        rsf_file, rec_metadata_size, rec_metadata_size, RT_DATA, total_payload_bytes, NULL, 0);
    if (new_record == NULL) {
        Lib_Log(APP_LIBFST, APP_FATAL, "%s: Unable to create new new_record with %ld bytes\n",
                __func__, total_payload_bytes);
        return(ERR_MEM_FULL);
    }
    search_metadata* meta = (search_metadata *) new_record->meta;
    stdf_dir_keys* stdf_entry = &meta->fst98_meta;

    // Insert json metadata
    if (metastr) {
        // Copy metadata into RSF record struct, just after directory metadata
        memcpy((char *)(meta + 1), metastr, metalen);
    }

    // Insert data map, just after json metadata and before actual data (it's part of the "payload")
    if (record->data_blocks.map != NULL) {
        new_record->data_map_size = record->data_blocks.map_size;
        new_record->data_map = new_record->data;

        // Move data pointer forward (it's after the data map), adjust max data size accordingly
        new_record->data = (char*)new_record->data_map + sizeof(uint32_t) * record->data_blocks.map_size;
        new_record->data_size = num_data_bytes;

        memcpy(new_record->data_map, record->data_blocks.map, record->data_blocks.map_size * sizeof(uint32_t));
    }

    record->data_type = data_type | has_missing;
    make_search_metadata(record, meta);
    new_record->data_size = elem_size;
    uint32_t* record_data = new_record->data;
    RSF_Record_set_num_elements(new_record, num_word32, sizeof(uint32_t));

    uint32_t * field_u32 = field;
    if (field_f != NULL) {
        field_u32 = (uint32_t*)field_f;
        packfunc = &compact_p_float; // Use corresponding packing function
    }
    if (image_mode_copy) {
        memcpy(new_record->data, field_u32, num_data_bytes);
    } else {
        // not image mode copy
        // time to fudge field if missing value feature is used

        // put appropriate values into field after allocating it
        if (has_missing) {
            const int data_bits = field_f == NULL ? stdf_entry->dasiz : 64;
            field_missing = (uint32_t *)malloc(num_elements * data_bits / 8);
            if (EncodeMissingValue(field_missing, record->data, num_elements, in_data_type, data_bits,
                                   record->pack_bits) > 0)
            {
                field_u32 = field_missing;
                if (field_f != NULL) packfunc = &compact_p_double;
            }
            else {
                field_u32 = field_f == NULL ? record->data : field_f;
                Lib_Log(APP_LIBFST, APP_INFO, "%s: NO missing value, data type %d reset to %d\n", __func__, stdf_entry->datyp, data_type);
                // cancel missing data flag in data type
                stdf_entry->datyp = data_type;
                has_missing = 0;
            }
        }

        switch (data_type) {

            case FST_TYPE_BINARY:
            case FST_TYPE_BINARY | FST_TYPE_TURBOPACK: {
                // transparent mode
                if (is_type_turbopack(data_type)) {
                    Lib_Log(APP_LIBFST, APP_WARNING, "%s: extra compression not available, data type %d reset to FST_TYPE_BINARY (%d)\n",
                            __func__, stdf_entry->datyp, FST_TYPE_BINARY);
                    data_type = FST_TYPE_BINARY;
                    stdf_entry->datyp = data_type;
                }
                const int32_t num_word32 = ((num_elements * record->pack_bits) + num_bits_per_word - 1) / num_bits_per_word;
                memcpy(new_record->data, field_u32, num_word32 * sizeof(uint32_t));
                break;
            }

            case FST_TYPE_REAL_OLD_QUANT:
            case FST_TYPE_REAL_OLD_QUANT | FST_TYPE_TURBOPACK: {
                // floating point
                double tempfloat = 99999.0;
                if (is_type_turbopack(data_type) && (record->pack_bits <= 16)) {
                    // use an additional compression scheme
                    // nbits>64 flags a different packing
                    // Use data pointer as uint32_t for compatibility with XDF format
                    packfunc(field_u32, (void *)&((uint32_t *)new_record->data)[1], (void *)&((uint32_t *)new_record->data)[5],
                        num_elements, record->pack_bits + 64 * Max(16, record->pack_bits), 0, stride, 0, &tempfloat, &dmin, &dmax);
                    const int compressed_lng = armn_compress((unsigned char *)((uint32_t *)new_record->data + 5),
                                                             record->ni, record->nj, record->nk, record->pack_bits, 1, 1);
                    if (compressed_lng < 0) {
                        stdf_entry->datyp = FST_TYPE_REAL_OLD_QUANT;
                        packfunc(field_u32, (void*)new_record->data, (void*)&((uint32_t*)new_record->data)[3],
                            num_elements, record->pack_bits, 24, stride, 0, &tempfloat, &dmin, &dmax);
                    } else {
                        int nbytes = 16 + compressed_lng;
                        const uint32_t num_word64 = (nbytes * 8 + 63) / 64;
                        const uint32_t num_word32 = W64TOWD(num_word64);
                        ((uint32_t*)new_record->data)[0] = num_word32;
                        RSF_Record_set_num_elements(new_record, num_word32 + 1, sizeof(uint32_t));
                    }
                } else {
                    packfunc(field_u32, (void*)new_record->data, (void*)&((uint32_t*)new_record->data)[3],
                        num_elements, record->pack_bits, 24, stride, 0, &tempfloat, &dmin, &dmax);
                }
                break;
            }

            case FST_TYPE_UNSIGNED:
            case FST_TYPE_UNSIGNED | FST_TYPE_TURBOPACK:
                // integer, short integer or byte stream
                {
                    int offset = is_type_turbopack(data_type) ? 1 :0;
                    if (is_type_turbopack(data_type)) {
                        if (record->data_bits == 16) { // short
                            stdf_entry->nbits = Min(16, record->pack_bits);
                            memcpy(record_data + offset, (void *)field_u32, num_elements * 2);
                        } else if (record->data_bits == 8) { // byte
                            stdf_entry->nbits = Min(8, record->pack_bits);
                            memcpy_8_16((int16_t *)(record_data + offset), (void *)field_u32, num_elements);
                        } else {
                            memcpy_32_16((short *)(record_data + offset), (void *)field_u32, record->pack_bits, num_elements);
                        }
                        const int compressed_lng = armn_compress((unsigned char *)&((uint32_t *)new_record->data)[offset],
                                                                 record->ni, record->nj, record->nk, record->pack_bits, 1, 0);
                        if (compressed_lng < 0) {
                            stdf_entry->datyp = FST_TYPE_UNSIGNED;
                            if (record->data_bits == 16) {
                                compact_p_short((void *)field_u32, (void *) NULL, new_record->data,
                                    num_elements, record->pack_bits, 0, stride);
                            } else if (record->data_bits == 8) {
                                compact_p_char((void *)field_u32, (void *) NULL, new_record->data,
                                    num_elements, Min(8, record->pack_bits), 0, stride);
                            } else {
                                compact_p_integer((void *)field_u32, (void *) NULL, new_record->data,
                                    num_elements, record->pack_bits, 0, stride, 0);
                            }
                            // The buffer was allocated for the (larger) turbopack size, but the
                            // plain packing is smaller. Use the precomputed plain size so that
                            // RSF_Put_record writes only the bytes the read path expects.
                            total_payload_bytes = plain_num_word32 * sizeof(uint32_t) + record->data_blocks.map_size * sizeof(uint32_t);
                            record->do_not_touch.stored_data_size = plain_num_word32;
                            RSF_Record_set_num_elements(new_record, plain_num_word32, sizeof(uint32_t));
                        } else {
                            const int nbytes = 4 + compressed_lng;
                            const uint32_t num_word64 = (nbytes * 8 + 63) / 64;
                            const uint32_t num_word32 = W64TOWD(num_word64);
                            ((uint32_t *)new_record->data)[0] = num_word32;
                            RSF_Record_set_num_elements(new_record, num_word32, sizeof(uint32_t));
                        }
                    } else {
                        if (record->data_bits == 16) { // short
                            stdf_entry->nbits = Min(16, record->pack_bits);
                            compact_p_short((void *)field_u32, (void *) NULL, &((uint32_t *)new_record->data)[offset],
                                num_elements, record->pack_bits, 0, stride);
                        } else if (record->data_bits == 8) { // byte
                            compact_p_char((void *)field_u32, (void *) NULL, new_record->data,
                                num_elements, Min(8, record->pack_bits), 0, stride);
                            stdf_entry->nbits = Min(8, record->pack_bits);
                        } else if (record->data_bits == 64) {
                            memcpy(new_record->data, field_u32, num_elements * sizeof(uint64_t));
                        } else {
                            compact_p_integer((void *)field_u32, (void *) NULL, &((uint32_t *)new_record->data)[offset],
                                num_elements, record->pack_bits, 0, stride, 0);
                        }
                    }
                }
                break;


            case FST_TYPE_CHAR:
            case FST_TYPE_CHAR | FST_TYPE_TURBOPACK:
                // character
                {
                    int nc = (record->ni * record->nj + 3) / 4;
                    if (is_type_turbopack(data_type)) {
                        Lib_Log(
                            APP_LIBFST, APP_WARNING, "%s: extra compression not available, data type %d reset to FST_TYPE_CHAR (%d)\n",
                            __func__, stdf_entry->datyp, FST_TYPE_CHAR);
                        data_type = FST_TYPE_CHAR;
                        stdf_entry->datyp = data_type;
                    }
                    compact_p_integer(field_u32, (void *) NULL, new_record->data, nc, 32, 0, stride, 0);
                    stdf_entry->nbits = 8;
                }
                break;

            case FST_TYPE_SIGNED:
            case FST_TYPE_SIGNED | FST_TYPE_TURBOPACK: {
                // signed integer
                if (is_type_turbopack(data_type)) {
                    Lib_Log(APP_LIBFST, APP_WARNING, "%s: extra compression not supported, data type %d reset to FST_TYPE_SIGNED (%d)\n",
                            __func__, stdf_entry->datyp, has_missing | FST_TYPE_SIGNED);
                    data_type = FST_TYPE_SIGNED;
                }
                // turbo compression not supported for this type, revert to normal mode
                stdf_entry->datyp = has_missing | FST_TYPE_SIGNED;

                int32_t * field3 = (int32_t*)field_u32;
                const int64_t num_elem = fst24_record_num_elem(record);

                if (record->data_bits == 64) {
                    memcpy(new_record->data, field_u32, num_elem * sizeof(int64_t));
                } else {
                    if (record->data_bits == 16 || record->data_bits == 8) {
                        if (num_elem > (1 << 30)) {
                            Lib_Log(APP_LIBFST, APP_ERROR,
                                "%s: Number of elements in record (%ld) is too large for what we can handle (%d) for now\n",
                                __func__, num_elem, (1<<30));
                        }
                        field3 = (int *)malloc(num_elem * sizeof(int));
                        if (field3 == NULL) {
                            Lib_Log(APP_LIBFST, APP_ERROR, "%s: Unable to allocate tmp array for int conversion\n", __func__);
                            return ERR_MEM_FULL;
                        }
                        short * s_field = (short *)field_u32;
                        signed char * b_field = (signed char *)field_u32;
                        if (record->data_bits == 16) for (int i = 0; i < num_elem;i++) { field3[i] = s_field[i]; };
                        if (record->data_bits == 8)  for (int i = 0; i < num_elem;i++) { field3[i] = b_field[i]; };
                    }
                    compact_p_integer(field3, (void *) NULL, new_record->data, num_elem, record->pack_bits, 0, stride, 1);
                }
                if (field3 != (int32_t*)field_u32) free(field3);

                break;
            }

            case FST_TYPE_REAL_IEEE:
            case FST_TYPE_REAL_IEEE | FST_TYPE_TURBOPACK:
            case FST_TYPE_COMPLEX:
            case FST_TYPE_COMPLEX | FST_TYPE_TURBOPACK:
                // IEEE and IEEE complex representation
                {
                    int32_t f_ni = record->ni;
                    int32_t f_njnk = record->nj * record->nk;
                    int32_t f_zero = 0;
                    int32_t f_one = 1;
                    int32_t f_minus_nbits = -record->pack_bits;
                    if (data_type == (FST_TYPE_COMPLEX | FST_TYPE_TURBOPACK)) {
                        Lib_Log(
                            APP_LIBFST, APP_WARNING, "%s: extra compression not available, data type %d reset to FST_TYPE_COMPLEX (%d)\n",
                            __func__, stdf_entry->datyp, FST_TYPE_COMPLEX);
                        data_type = FST_TYPE_COMPLEX;
                        stdf_entry->datyp = data_type;
                    }
                    if (data_type == (FST_TYPE_REAL_IEEE | FST_TYPE_TURBOPACK)) {
                        // use an additionnal compression scheme
                        const int compressed_lng = c_armn_compress32(
                            (unsigned char *)&((uint32_t *)new_record->data)[1], (void *)field_u32, record->ni, record->nj,
                            record->nk, record->pack_bits);

                        if (compressed_lng < 0) {
                            stdf_entry->datyp = FST_TYPE_REAL_IEEE;
                            f77name(ieeepak)((int32_t *)field_u32, new_record->data, &f_ni, &f_njnk, &f_minus_nbits, &f_zero, &f_one);
                        } else {
                            const int nbytes = 16 + compressed_lng;
                            const uint32_t num_word64 = (nbytes * 8 + 63) / 64;
                            const uint32_t num_word32 = W64TOWD(num_word64);
                            ((uint32_t *)new_record->data)[0] = num_word32;
                            RSF_Record_set_num_elements(new_record, num_word32, sizeof(uint32_t));
                        }
                    } else {
                        if (data_type == FST_TYPE_COMPLEX) f_ni = f_ni * 2;
                        f77name(ieeepak)((int32_t *)field_u32, new_record->data, &f_ni, &f_njnk, &f_minus_nbits, &f_zero, &f_one);
                    }
                }
                break;

            case FST_TYPE_REAL:
            case FST_TYPE_REAL | FST_TYPE_TURBOPACK:
                // floating point, new packers

                if (is_type_turbopack(data_type) && (record->pack_bits <= 16)) {
                    // use an additional compression scheme
                    c_float_packer((void *)field_u32, record->pack_bits, &((int32_t *)new_record->data)[1],
                                   &((int32_t *)new_record->data)[1+header_size], num_elements);
                    const int compressed_lng = armn_compress(
                        (unsigned char *)&((uint32_t *)new_record->data)[1+header_size], record->ni, record->nj,
                        record->nk, record->pack_bits, 1, 1);
                    if (compressed_lng < 0) {
                        stdf_entry->datyp = FST_TYPE_REAL;
                        c_float_packer((void *)field_u32, record->pack_bits, new_record->data, &((int32_t *)new_record->data)[header_size],
                                        num_elements);
                    } else {
                        const int nbytes = 16 + (header_size*4) + compressed_lng;
                        const uint32_t num_word64 = (nbytes * 8 + 63) / 64;
                        const uint32_t num_word32 = W64TOWD(num_word64);
                        ((uint32_t *)new_record->data)[0] = num_word32;
                        RSF_Record_set_num_elements(new_record, num_word32, sizeof(uint32_t));
                    }
                } else {
                    c_float_packer((void *)field_u32, record->pack_bits, new_record->data,
                                   &((int32_t *)new_record->data)[header_size], num_elements);
                }
                break;

            case FST_TYPE_STRING:
            case FST_TYPE_STRING | FST_TYPE_TURBOPACK:
                // character string
                if (is_type_turbopack(data_type)) {
                    Lib_Log(APP_LIBFST, APP_WARNING,
                            "%s: extra compression not available, data type %d reset to FST_TYPE_STRING (%d)\n",
                            __func__, stdf_entry->datyp, FST_TYPE_STRING);
                    data_type = FST_TYPE_STRING;
                    stdf_entry->datyp = data_type;
                }
                compact_p_char(field_u32, (void *) NULL, new_record->data, num_elements, 8, 0, stride);
                break;

            default:
                Lib_Log(APP_LIBFST, APP_ERROR, "%s: invalid data_type=%d\n", __func__, data_type);
                return ERR_BAD_DATYP;
        } // end switch
    } // end if/else image mode copy

    record->data_type = stdf_entry->datyp;
    record->pack_bits = stdf_entry->nbits;
    record->data_bits = stdf_entry->dasiz;

    // write new_record to file and add entry to directory
    const int64_t record_handle = RSF_Put_record(rsf_file, new_record, total_payload_bytes);
    record->do_not_touch.handle = record_handle;
    record->file_index = RSF_Key64_to_index(record_handle);

    if (Lib_LogLevel(APP_LIBFST,NULL) >= APP_INFO) {
        fst_record_fields f = default_fields;
        // f.grid_info = 1;
        f.deet = 1;
        f.npas = 1;
        fst24_record_print_short(record, &f, 0, "(INFO) FST|Write:");
    }

    RSF_Free_record(new_record);

    if (field_f != NULL) free(field_f);
    if (field_missing != NULL) free(field_missing);

    if (record_handle <= 0) {
        Lib_Log(APP_LIBFST, APP_ERROR, "%s: Error writing RSF record to file\n", __func__, record_handle);
        return -1;
    }

    return TRUE;
}

//! Rewrite the metadata of a record in an RSF file
//! Currently only does the "search metadata", without the extended one
//! \return TRUE (1) if successful, FALSE (0) if there was an error
int32_t fst24_rewrite_meta_rsf(
    RSF_handle file_handle,         //!< [in] File where the record is located
    const int64_t record_handle,    //!< [in] Record handle
    const fst_record* const record  //!< [in] The new metadata to store
) {
    // Sanity check
    if (record->do_not_touch.fst_version != FST24_VERSION_COUNT) {
        Lib_Log(APP_LIBFST, APP_ERROR,
            "%s: Existing record written with FST version %d, but this library is compiled for version %d."
            " We cannot rewrite this record's metadata.\n",
            __func__, record->do_not_touch.fst_version, FST24_VERSION_COUNT);
        return FALSE;
    }

    // Compute extended metadata size requirements
    int ext_meta_bytes = 0;
    uint16_t ext_meta_words = 0; // 32-bit words
    char* meta_str = NULL;
    if (record->metadata != NULL) {
        if ((meta_str = Meta_Stringify(record->metadata,JSON_C_TO_STRING_PLAIN)) != NULL) {
            ext_meta_bytes = strlen(meta_str) + 1; // Include null character
            ext_meta_words = (ext_meta_bytes + 3) / 4; // Round up to 4 bytes
        }
    }

    // Put info together in a single array
    uint32_t meta[sizeof(search_metadata) / sizeof(uint32_t) + ext_meta_words];
    make_search_metadata(record, (search_metadata*)meta);
    if (meta_str != NULL) memcpy(((search_metadata*)meta) + 1, meta_str, ext_meta_bytes);

    // Do the rewrite
    if (RSF_Rewrite_record_meta(file_handle, record_handle, meta,
                                sizeof(search_metadata) + ext_meta_bytes) != 1) {
        Lib_Log(APP_LIBFST, APP_ERROR, "%s: Error trying to rewrite in RSF file\n", __func__);
        return FALSE;
    }

    return TRUE;
}

//! \return TRUE (1) if we were able to get the information, a negative number otherwise
int32_t get_record_from_key_rsf(
    const RSF_handle rsf_file,  //!< [in] File to which the record belongs. Must be open
    const int64_t key,          //!< [in] Key of the record we are looking for. Must be valid
    fst_record* const record    //!< [in,out] Record information (no data or advanced metadata)
) {
    const RSF_record_info record_info = RSF_Get_record_info(rsf_file, key);

    if (record_info.rl <= 0) {
        Lib_Log(APP_LIBFST, APP_ERROR, "%s: Could not retrieve record with key %ld\n", __func__, key);
        return ERR_BAD_HNDL;
    }

    return update_attributes_from_rsf_info(record, key, &record_info);
}

//! Find the next record in a given RSF file, according to the given parameters
//! \return Key of the record found (negative if error or nothing found)
int64_t find_next_rsf(
    const RSF_handle file_handle, //!> Handle to an open RSF file
    fst_query* const query        //!> 
) {

    search_metadata actual_mask;
    uint32_t* actual_mask_u32     = (uint32_t *)&actual_mask;
    uint32_t* mask_u32            = (uint32_t *)&query->mask;
    uint32_t* background_mask_u32 = (uint32_t *)&query->background_mask;
    for (int i = 0; i < query->num_criteria; i++) {
        actual_mask_u32[i] = mask_u32[i] & background_mask_u32[i];
    }
    const int64_t key = RSF_Lookup(file_handle,
                                   query->search_index,
                      (uint32_t *)&query->criteria,
                                   actual_mask_u32,
                                   query->num_criteria);
    if (key > 0) {
        // Found it. Next search will start here
        query->search_index = key;
    }
    else {
        // Did not find it. Mark this search as finished
        query->search_done = 1;
    }
    return key;
}

//! Decode the given raw data pointer as if it were the content of an RSF record.
//! \return A properly initialized fst_record object. If we were successful in decoding the data, the record `data`
//!         pointer will be valid; if we were not successful, the `data` pointer will be NULL.
fst_record fst24_decode_data_rsf(
    //!> [in] Input data to be extracted (it will not be modified)
    void* data,
    //!> [in,out] [Optional] If non-NULL, must point to a sufficiently large space to hold the entire extracted data
    void* dest_data
) {

    // First interpret the RSF record
    RSF_record rsf_rec = RSF_as_record(data);
    if (rsf_rec.data == NULL) return default_fst_record; // Error trying to interpret the data as an RSF record

    // Extract metadata
    fst_record rec = default_fst_record;
    const search_metadata* meta = (const search_metadata*)rsf_rec.meta;
    fill_with_search_meta(&rec, meta, FST_RSF);

    // Allocate space if needed
    void* dest = dest_data;
    if (dest == NULL) {
        rec.do_not_touch.alloc = fst24_record_data_size(&rec);
        dest = malloc(rec.do_not_touch.alloc);
        if (dest == NULL) {
            Lib_Log(APP_LIBFST, APP_FATAL, "%s: Unable to allocate memory for unpacking record data\n", __func__);
            return rec;
        }
    }

    // Extract metadata from record if present
    if (rec.do_not_touch.extended_meta_size > 0) {
        // Located after the search keys
        rec.metadata = Meta_Parse((char*)((uint32_t*)rsf_rec.meta + rec.do_not_touch.num_search_keys));
    }

    // Unpack the data
    const int32_t status = fst24_unpack_data(dest, rsf_rec.data, &rec, 0, 1, rec.data_bits);

    // Indicate success
    if (status == 0) rec.data = dest;

    return rec;
}

//! Read a record from an RSF file
//! \return 0 for success, negative for error
int32_t fst24_read_record_rsf(
    //!> [in,out] Record for which we want to read data.
    //!> Must have a valid handle!
    //!> Must have already allocated its data buffer
    fst_record* record_fst,
    const int32_t skip_unpack,  //!< Whether to skip the unpacking process (e.g. if we just want to copy the record)
    const int32_t metadata_only //!< Whether we want to only read metadata, rather than including everything
) {
    RSF_handle file_handle = record_fst->file->rsf_handle;
    if (!RSF_Is_record_in_file(file_handle, record_fst->do_not_touch.handle)) return ERR_BAD_HNDL;

    if (record_fst->do_not_touch.deleted == 1) {
        Lib_Log(APP_LIBFST, APP_WARNING, "%s: Cannot read data from a deleted record\n", __func__);
        return ERR_BAD_HNDL;
    }

    const size_t needed_data_size = Max(fst24_record_data_size(record_fst),
                                        record_fst->do_not_touch.unpacked_data_size * sizeof(uint32_t));
    const size_t work_size_bytes = needed_data_size +                       // The data itself
                                   record_fst->num_meta_bytes +             // The metadata
                                   sizeof(RSF_record) +                     // Space for the RSF struct itself
                                   128 * sizeof(uint32_t);                  // Enough space for the largest compression scheme + rounding up for alignment

    void* work_space = malloc(work_size_bytes);
    if (work_space == NULL) {
        Lib_Log(APP_LIBFST, APP_FATAL, "%s: Unable to allocate workspace for reading record (%zu bytes)\n",
                __func__, work_size_bytes);
        return ERR_MEM_FULL;
    }

    memset(work_space, 0, work_size_bytes);

    RSF_record_info record_info;
    RSF_record* record_rsf = RSF_Get_record(
        file_handle, record_fst->do_not_touch.handle, metadata_only, (void*)work_space, &record_info);

    if ((uint64_t*)record_rsf != work_space) {
        Lib_Log(APP_LIBFST, APP_ERROR, "%s: Could not get record corresponding to key 0x%x\n",
                __func__, record_fst->do_not_touch.handle);
        free(work_space);
        return ERR_BAD_HNDL;
    }

    const int requested_num_bits = record_fst->data_bits;
    if (update_attributes_from_rsf_info(record_fst, record_fst->do_not_touch.handle, &record_info) != TRUE) {
        Lib_Log(APP_LIBFST, APP_ERROR, "%s: Failed to update attributes from RSF info\n", __func__);
        free(work_space);
        return ERR_BAD_HNDL;
    }

    if (record_fst->data_blocks.map_size > 0) {
        // data_map pointer should have been freed by the update attributes function
        record_fst->data_blocks.map = (uint32_t*)malloc(record_fst->data_blocks.map_size * sizeof(uint32_t));
        memcpy(record_fst->data_blocks.map, record_rsf->data_map, record_fst->data_blocks.map_size * sizeof(uint32_t));
    }

    int32_t status = 0;
    if (record_fst->data_bits < record_fst->pack_bits) {
        Lib_Log(APP_LIBFST, APP_ERROR, "%s: Cannot handle data size (%d bits) smaller than packed size (%d bits)\n",
            __func__, record_fst->data_bits, record_fst->pack_bits);
        status = -1;
        goto end_read;
    }

    // Determine into what size we are reading (only 8, 16, 32 and 64 allowed)
    const int32_t original_num_bits = record_fst->data_bits;
    if (is_type_integer(record_fst->data_type) || is_type_real(record_fst->data_type)) {
        if (requested_num_bits < record_fst->pack_bits) {
            Lib_Log(APP_LIBFST, APP_WARNING,
                "%s: Reading %d-bit data elements (compressed to %d) into an array of %d-bit elements\n",
                __func__, record_fst->data_bits, requested_num_bits, record_fst->pack_bits);
        }

        if (requested_num_bits > 32)
            record_fst->data_bits = 64;
        else if (requested_num_bits > 16)
            record_fst->data_bits = 32;
        else if (requested_num_bits > 8)
            record_fst->data_bits = 16;
        else
            record_fst->data_bits = 8;
    }

    // Extract metadata from record if present
    if (record_fst->do_not_touch.extended_meta_size > 0) {
        // Located after the search keys
        record_fst->metadata = Meta_Parse((char*)((uint32_t*)record_rsf->meta + record_fst->do_not_touch.num_search_keys));
    }

    // Extract data
    if (metadata_only != 1)
        status = fst24_unpack_data(record_fst->data, record_rsf->data, record_fst, skip_unpack, 1, original_num_bits);

    if (Lib_LogLevel(APP_LIBFST, NULL) >= APP_INFO) {
        fst_record_fields f = default_fields;
        // f.grid_info = 1;
        f.deet = 1;
        f.npas = 1;
        fst24_record_print_short(record_fst, &f, 0, "(fst) Read : ");
    }

end_read:
    free(work_space);
    return status;
}
