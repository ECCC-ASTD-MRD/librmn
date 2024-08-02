#include <stdlib.h>
#include <float.h>
#include <math.h>

#include <pthread.h>

#include <App.h>
#include <str.h>

#include "fst_internal.h"
#include "fst24_file_internal.h"
#include "fst24_record_internal.h"
#include "fst98_internal.h"
#include "rmn/fnom.h"
#include "rmn/Meta.h"
#include "xdf98.h"

static pthread_mutex_t fst24_xdf_mutex = PTHREAD_MUTEX_INITIALIZER;

static const char * fst_file_type_name[] = {
    [FST_NONE] = "FST_NONE",
    [FST_XDF]  = "FST_XDF",
    [FST_RSF]  = "FST_RSF"
};

static fst_file default_fst_file = (fst_file) {
    .iun                =  0,
    .file_index         = -1,
    .file_index_backend = -1,
    .rsf_handle.p       = NULL,
    .type               = FST_NONE,
    .next               = NULL,
    .path               = NULL,
    .tag                = NULL
};

static int64_t fst24_get_num_records_single(const fst_file* file);

//! Verify that the file pointer is valid and the file is open. This is meant to verify that
//! the file struct has been initialized by a call to fst24_open; it should *not* be called
//! on a file that has been closed, since it will result in undefined behavior.
//! \return 1 if the pointer is valid and the file is open, 0 otherwise
int32_t fst24_is_open(const fst_file* const file) {
    return file != NULL &&
           (file->type == FST_RSF || file->type == FST_XDF) &&
           file->file_index >= 0 &&
           file->file_index_backend >= 0 &&
           file->iun != 0 &&
           file->path != NULL;
}

//! \return The name of the file, if open. NULL otherwise
const char* fst24_file_name(const fst_file* const file) {
    if (fst24_is_open(file)) return file->path;
    return NULL;
}

//! Get unit number for API calls that require it. This exists mostly for compatibility with
//! other libraries and tools that only work with unit numbers rather than a fst_file struct.
//! \return Unit number. 0 if file is not open or struct is not valid.
int32_t fst24_get_unit(const fst_file* const file) {
    if (fst24_is_open(file)) return file->iun;
    return 0;
}

//! \return File tag
const char* fst24_get_tag(const fst_file* const file) {
    return file->tag;
}

//! \return The new file tag
const char* fst24_set_tag(fst_file* file,const char* const tag) {
    if (file->tag) free(file->tag);
    return file->tag=strdup(tag);
}

//! Test if the given path is a readable FST file
//! \return TRUE (1) if the file makes sense, FALSE (0) if an error is detected
int32_t fst24_is_valid(
    const char* const filePath
) {
    const int32_t type = c_wkoffit(filePath, strlen(filePath));
    if (type == WKF_STDRSF) {
        return RSF_Basic_check(filePath);
    }
    else {
        if (c_fstcheck_xdf(filePath) == 0) return TRUE;
    }

    return FALSE;
}

//! Open a standard file (FST)
//!
//! File will be created if it does not already exist and is opened in R/W mode.
//! The same file can be opened several times simultaneously as long as some rules are followed:
//! for XDF files, they have to be in R/O (read-only) mode; for RSF files, R/O mode is always OK,
//! and R/W (read-write) mode is allowed if the PARALLEL option is used. Additionally, for RSF
//! only, if a file is already open in R/O mode, it *must* be closed before any other thread or process can
//! open it in write mode.
//!
//! Refer to the README for information on how to write in parallel in an RSF file.
//!
//! Thread safety: Always safe to open files concurrently (from a threading perspective).
//!
//! \return A handle to the opened file. NULL if there was an error
fst_file* fst24_open(
    const char* const filePath,  //!< Path of the file to open
    const char* const options     //!< A list of options, as a string, with each pair of options separated by a comma or a '+'
) {
    fst_file* the_file = (fst_file *)malloc(sizeof(fst_file));
    if (the_file == NULL) return NULL; //!< \todo Shouldn't this throw some kind of error!?

    *the_file = default_fst_file;

    const int MAX_LENGTH = 1024;
    char local_options[MAX_LENGTH];
    snprintf(local_options, MAX_LENGTH, "RND+%s%s",
             (!options || !(strcasestr(options, "R/W") || strcasestr(options, "R/O"))) ? "R/O+" : "",
             options ? options : "");
    Lib_Log(APP_LIBFST, APP_DEBUG, "%s: filePath = %s, options = %s\n", __func__, filePath, local_options);

    // Lib_Log(APP_LIBFST, APP_VERBATIM, "%s: Calling fnom\n", __func__);
    if (c_fnom(&(the_file->iun), filePath, local_options, 0) != 0) {
        free(the_file);
        return NULL;
    }
    // Lib_Log(APP_LIBFST, APP_VERBATIM, "%s: Calling fstouv\n", __func__);
    if (c_fstouv(the_file->iun, local_options) < 0) {
        free(the_file);
        return NULL;
    }

    // Find type of newly-opened file (RSF or XDF)
    int index_fnom;
    const int rsf_status = is_rsf(the_file->iun, &index_fnom);
    the_file->file_index = index_fnom;
    the_file->path = FGFDT[index_fnom].file_name;
    if (rsf_status == 1) {
        the_file->type = FST_RSF;
        the_file->rsf_handle = FGFDT[the_file->file_index].rsf_fh;
        the_file->file_index_backend = RSF_Get_file_slot(the_file->rsf_handle);
    }
    else {
        the_file->type = FST_XDF;
        // Lib_Log(APP_LIBFST, APP_VERBATIM, "%s: Calling file_index_xdf\n", __func__);
        the_file->file_index_backend = file_index_xdf(the_file->iun);
    }

    return the_file;
}

//! Close the given standard file and free the memory associated with the struct
//!
//! Thread safety: Closing several different files concurrently is always safe. Closing the same file
//! several times is an error. Closing a file while another fst24 API call is running on that same
//! file is also an error. The user is responsible to make sure that other calls on a certain
//! file are finished before closing that file.
//!
//! \todo What happens if closing a linked file?
//! \return TRUE (1) if no error, FALSE (0) or a negative number otherwise
int32_t fst24_close(fst_file* const file) {
    if (!fst24_is_open(file)) return ERR_NO_FILE;

    int status;
    status = c_fstfrm(file->iun);   // Close the actual file
    if (status < 0) return status;

    status = c_fclos(file->iun);    // Reset file entry in global table
    if (status < 0) return status;

    *file = default_fst_file;

    free(file);

    return TRUE;
}

//! Commit data and metadata to disk if the file has changed in memory
//!
//! Thread safety: Always safe to call concurrently on different open files.
//! *For RSF only*, it is safe to call this function from one thread while another is writing to the same file;
//! it is also safe to call it concurrently on the same file (although that would be useless).
//!
//! \return A negative number if there was an error, 0 or positive otherwise
int32_t fst24_flush(
    const fst_file* const file //!< Handle to the open file we want to checkpoint
) {
    if (!fst24_is_open(file)) return ERR_NO_FILE;

    if (file->type == FST_RSF) {
        RSF_handle file_handle = FGFDT[file->file_index].rsf_fh;
        return RSF_Checkpoint(file_handle);
    }
    else if (file->type == FST_XDF) {
        return c_fstckp_xdf(file->iun);
    }

    Lib_Log(APP_LIBFST, APP_ERROR, "%s: Unrecognized file type %d (%s)\n", __func__, file->type, file->path);
    return -1;
}

//! Get the number of records in a file including linked files
//!
//! Thread safety: Always safe to call it concurrently with other API calls on any open file.
//!
//! \return Number of records in the file and in any linked files
int64_t fst24_get_num_records(
    const fst_file* const file    //!< [in] Handle to an open file
) {
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
        Lib_Log(APP_LIBFST, APP_ERROR, "%s: Unrecognized file type (%s)\n", __func__, file->path);
        return 0;
    }

    if (file->next != NULL) total_num_records += fst24_get_num_records(file->next);

    return total_num_records;
}

//! Get the number of records in the file
//! \return Number of records in the file
static int64_t fst24_get_num_records_single(const fst_file* file) {
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

//! Print a summary of the records found in the given file (including any linked files)
//!
//! Thread safety: Safe to call concurrently (but the output could be interleaved).
//! *For RSF only*, safe to call from one thread while another is writing to the same file.
//!
//! \return a negative number if there was an error, TRUE (1) if all was OK
int32_t fst24_print_summary(
    fst_file* const file, //!< [in] Handle to an open file
    const fst_record_fields* const fields //!< [optional] What fields we want to see printed
) {
    if (!fst24_is_open(file)) return ERR_NO_FILE;

    fst_query* query = fst24_new_query(file, NULL, NULL); // Look for every (non-deleted) record

    fst_record rec = default_fst_record;
    int64_t num_records = 0;
    size_t total_data_size = 0;
    char prefix[16];

    int num_digits = 0;
    for (int64_t tmp_num_records = fst24_get_num_records(file); tmp_num_records > 0; tmp_num_records /= 10) num_digits++;

    while (fst24_find_next(query, &rec) == TRUE) {
        snprintf(prefix, num_digits + 2, "%*ld-", num_digits, num_records);
        fst24_record_print_short(&rec, fields, ((num_records % 70) == 0), prefix);
        num_records++;
        total_data_size += fst24_record_data_size(&rec);
    }

    Lib_Log(APP_LIBFST, APP_VERBATIM,
            "\n%d records in RPN standard file(s). Total data size %ld bytes (%.1f MB).\n",
            num_records, total_data_size, total_data_size / (1024.0f * 1024.0f));

    return TRUE;
}

//! Retreive record's data minimum and maximum value
void fst24_bounds(
    const fst_record *record, //!< [in] Record with its data already available in memory
    double *Min,  //!< [out] Mimimum value (NaN if not retreivable)
    double *Max   //!< [out] Maximum value (NaN if not retreivable)
) {
    uint64_t sz = (record->ni * record->nj * record->nk);

    *Min = *Max = NAN;

    // Loop on data type to avoid type casting as much as possible
    switch (record->data_type) {
        case 0: case 128:
            // binary
            *Min = 0;
            *Max = 1;
            break;

        case 1: case 5: case 6: {
            // floating point
            double dmin = DBL_MAX;
            double dmax = DBL_MIN;
            double dval;
            for(uint64_t n = 0; n < sz; n++) {
                dval = record->data_bits == 32 ? ((float*)record->data)[n] : ((double*)record->data)[n];
                if (dval < dmin) {
                    dmin = dval;
                }
                else if (dval > dmax) {
                    dmax = dval;
                }
            }
            *Min = dmin;
            *Max = dmax;
            break;
        }

        case 2: {
            // integer, short integer or byte stream
            uint64_t umin = ULONG_MAX;
            uint64_t umax = 0;
            uint64_t uval;
            for(uint64_t n = 0; n < sz; n++) {
                uval = record->data_bits == 32 ? ((uint32_t*)record->data)[n] :
                       record->data_bits == 64 ? ((uint64_t*)record->data)[n] :
                                                 ((uint8_t*)record->data)[n];
                if (uval < umin) {
                    umin = uval;
                }
                else if (uval > umax) {
                    umax = uval;
                }
            }
            *Min = umin;
            *Max = umax;
            break;
        }

        case 3: {
            //! \todo WTF is character
            // character
            char cmin = CHAR_MAX;
            char cmax = CHAR_MIN;
            char cval;
            for(uint64_t n = 0; n < sz; n++) {
                cval = ((char*)record->data)[n];
                if (cval < cmin) {
                    cmin = cval;
                }
                else if (cval > cmax) {
                    cmax = cval;
                }
            }
            *Min = cmin;
            *Max = cmax;
            break;
        }

        case 4: case 132: {
            // signed integer
            int64_t lmin = LONG_MAX;
            int64_t lmax = 0;
            int64_t lval;
            for(uint64_t n = 0; n < sz; n++) {
                lval = record->data_bits == 32 ? ((int32_t*)record->data)[n] :
                       record->data_bits == 64 ? ((int64_t*)record->data)[n] :
                                                 ((int8_t*)record->data)[n];
                if (lval < lmin) {
                    lmin = lval;
                }
                else if (lval > lmax) {
                    lmax = lval;
                }
            }
            *Min = lmin;
            *Max = lmax;
            break;
        }

        case 7: case 135:
            break;
    }
}

//! Write a record to an RSF file.
//! Sometimes the requested writing parameters are not compatible and are changed. If that is
//! the case, the given fst_record struct will be updated.
//! \return TRUE (1) if writing was successful, 0 or a negative number otherwise
int32_t fst24_write_rsf(
    RSF_handle rsf_file,
    fst_record* record,
    const int32_t stride    //!< Compaction parameter. When in doubt, leave at 1
) {
    if (rsf_file.p == NULL) {
        Lib_Log(APP_LIBFST, APP_ERROR, "%s: file is not open\n", __func__);
        return ERR_NO_FILE;
    }

    if ((RSF_Get_mode(rsf_file) & RSF_RO) == RSF_RO) {
        Lib_Log(APP_LIBFST, APP_ERROR, "%s: file not open with write permission\n", __func__);
        return ERR_NO_WRITE;
    }

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

    float* data_f = record->data;
    if (is_type_real(in_data_type) && elem_size == 64) {
        if (record->pack_bits <= 32) {
            // We convert now from double to float
            elem_size = 32;
            data_f = alloca(fst24_record_num_elem(record) * sizeof(float));
            double* data_d = record->data;
            for (int i = 0; i < fst24_record_num_elem(record); i++) {
                data_f[i] = (float)data_d[i];
            }
        }
        else if (record->pack_bits != 64) {
            Lib_Log(APP_LIBFST, APP_WARNING, "%s: Requested %d packed bits for 64-bit reals, but we can only do"
                    " 64 or less than 32. Will store 64 bits.\n", __func__, record->pack_bits);
            record->pack_bits = 64;
        }
    }

    PackFunctionPointer packfunc;
    double dmin = 0.0;
    double dmax = 0.0;
    if (elem_size == 64 || in_data_type == FST_TYPE_MAGIC) {
        packfunc = (PackFunctionPointer) &compact_double;
    } else {
        packfunc = (PackFunctionPointer) &compact_float;
    }

    if ( (record->data_type == (FST_TYPE_REAL_IEEE | FST_TYPE_TURBOPACK)) && (record->pack_bits > 32) ) {
        Lib_Log(APP_LIBFST, APP_WARNING, "%s: extra compression not supported for IEEE when nbits > 32, "
                "data type 133 reset to 5 (IEEE)\n", __func__);
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

    // validate range of arguments
    if (fst24_record_validate_params(record) != 0) {
        Lib_Log(APP_LIBFST, APP_ERROR, "%s: Invalid value for certain parameters\n", __func__);
        return ERR_OUT_RANGE;
    }

    // Increment date by timestep size
    const uint32_t valid_date = get_valid_date32(record->dateo, record->deet, record->npas);

    // allocate and initialize a buffer interface for RSF_Put
    // an extra 512 bytes are allocated for cluster alignment purpose (seq). Are they???
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
    if ((data_type == FST_TYPE_REAL) && (record->pack_bits > 24)) {
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
                num_word32 = W64TOWD((num_elements * record->pack_bits + 120 + 63) / 64);
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
          metalen = strlen(metastr) + 1;
          ext_metadata_size = (metalen + 3) / 4;
          rec_metadata_size += ext_metadata_size;
       }
    }

    record->do_not_touch.num_search_keys = dir_metadata_size;
    record->do_not_touch.extended_meta_size = ext_metadata_size;
    record->do_not_touch.stored_data_size = num_word32;

    record->num_meta_bytes = rec_metadata_size * sizeof(uint32_t);
    RSF_record* new_record = RSF_New_record(rsf_file, rec_metadata_size, dir_metadata_size, RT_DATA, num_data_bytes, NULL, 0);
    if (new_record == NULL) {
        Lib_Log(APP_LIBFST, APP_FATAL, "%s: Unable to create new new_record with %ld bytes\n", __func__, num_data_bytes);
        return(ERR_MEM_FULL);
    }
    uint32_t* record_data = new_record->data;

    RSF_Record_set_num_elements(new_record, num_word32, sizeof(uint32_t));

    char typvar[FST_TYPVAR_LEN];
    copy_record_string(typvar, record->typvar, FST_TYPVAR_LEN);
    char nomvar[FST_NOMVAR_LEN];
    copy_record_string(nomvar, record->nomvar, FST_NOMVAR_LEN);
    char etiket[FST_ETIKET_LEN];
    copy_record_string(etiket, record->etiket, FST_ETIKET_LEN);
    char grtyp[FST_GTYP_LEN];
    copy_record_string(grtyp, record->grtyp, FST_GTYP_LEN);

    // set stdf_entry to address of buffer->data for building keys
    search_metadata* meta = (search_metadata *) new_record->meta;
    stdf_dir_keys* stdf_entry = &meta->fst98_meta;

    // Insert json metadata
    if (metastr) {
        // Copy metadata into RSF record struct, just after directory metadata
        memcpy((char *)(meta + 1), metastr, metalen);
    }

    // RSF reserved metadata
    for (int i = 0; i < RSF_META_RESERVED; i++) {
        meta->rsf_reserved[i] = 0;
    }
    
    // FST reserved metadata
    meta->fst24_reserved[0] = fst24_reserved_0(ext_metadata_size);
    meta->fst24_reserved[1] = 0; // Datamap size

    // fst98 metadata 
    {
        (void)stdf_entry->deleted; // Reserved by RSF. Don't write anything here!
        (void)stdf_entry->select;  // Reserved by RSF. Don't write anything here!
        (void)stdf_entry->lng;     // Reserved by RSF. Don't write anything here!
        (void)stdf_entry->addr;    // Reserved by RSF. Don't write anything here!

        stdf_entry->deet = record->deet;
        stdf_entry->nbits = record->pack_bits;
        stdf_entry->ni = record->ni;
        stdf_entry->gtyp = grtyp[0];
        stdf_entry->nj = record->nj;
        // propagate missing values flag
        stdf_entry->datyp = data_type | has_missing;
        // this value may be changed later in the code to eliminate improper flags
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
        stdf_entry->date_stamp = stamp_from_date(valid_date);
        stdf_entry->dasiz = elem_size;
    }

    uint32_t * field = record->data;
    if (image_mode_copy) {
        memcpy(new_record->data, field, num_data_bytes);
    } else {
        // not image mode copy
        // time to fudge field if missing value feature is used

        // put appropriate values into field after allocating it
        if (has_missing) {
            // allocate self deallocating scratch field
            field = (uint32_t *)alloca(num_elements * stdf_entry->dasiz/8);
            if ( 0 == EncodeMissingValue(field, record->data, num_elements, in_data_type, stdf_entry->dasiz, record->pack_bits) ) {
                field = record->data;
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
                memcpy(new_record->data, field, num_word32 * sizeof(uint32_t));
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
                    packfunc(field, (void *)&((uint32_t *)new_record->data)[1], (void *)&((uint32_t *)new_record->data)[5],
                        num_elements, record->pack_bits + 64 * Max(16, record->pack_bits), 0, stride, 1, 0, &tempfloat ,&dmin ,&dmax);
                    const int compressed_lng = armn_compress((unsigned char *)((uint32_t *)new_record->data + 5),
                                                             record->ni, record->nj, record->nk, record->pack_bits, 1, 1);
                    if (compressed_lng < 0) {
                        stdf_entry->datyp = 1;
                        packfunc(field, (void*)new_record->data, (void*)&((uint32_t*)new_record->data)[3],
                            num_elements, record->pack_bits, 24, stride, 1, 0, &tempfloat ,&dmin ,&dmax);
                    } else {
                        int nbytes = 16 + compressed_lng;
                        const uint32_t num_word64 = (nbytes * 8 + 63) / 64;
                        const uint32_t num_word32 = W64TOWD(num_word64);
                        ((uint32_t*)new_record->data)[0] = num_word32;
                        RSF_Record_set_num_elements(new_record, num_word32 + 1, sizeof(uint32_t));
                    }
                } else {
                    packfunc(field, (void*)new_record->data, (void*)&((uint32_t*)new_record->data)[3],
                        num_elements, record->pack_bits, 24, stride, 1, 0, &tempfloat ,&dmin ,&dmax);
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
                            memcpy(record_data + offset, (void *)field, num_elements * 2);
                        } else if (record->data_bits == 8) { // byte
                            stdf_entry->nbits = Min(8, record->pack_bits);
                            memcpy_8_16((int16_t *)(record_data + offset), (void *)field, num_elements);
                        } else {
                            memcpy_32_16((short *)(record_data + offset), (void *)field, record->pack_bits, num_elements);
                        }
                        const int compressed_lng = armn_compress((unsigned char *)&((uint32_t *)new_record->data)[offset],
                                                                 record->ni, record->nj, record->nk, record->pack_bits, 1, 0);
                        if (compressed_lng < 0) {
                            stdf_entry->datyp = FST_TYPE_UNSIGNED;
                            compact_integer((void *)field, (void *) NULL, &((uint32_t *)new_record->data)[offset],
                                num_elements, record->pack_bits, 0, stride, 1);
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
                            compact_short((void *)field, (void *) NULL, &((uint32_t *)new_record->data)[offset],
                                num_elements, record->pack_bits, 0, stride, 5);
                        } else if (record->data_bits == 8) { // byte
                            compact_char((void *)field, (void *) NULL, new_record->data,
                                num_elements, Min(8, record->pack_bits), 0, stride, 9);
                            stdf_entry->nbits = Min(8, record->pack_bits);
                        } else if (record->data_bits == 64) {
                            memcpy(new_record->data, field, num_elements * sizeof(uint64_t));
                        } else {
                            compact_integer((void *)field, (void *) NULL, &((uint32_t *)new_record->data)[offset],
                                num_elements, record->pack_bits, 0, stride, 1);
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
                    compact_integer(field, (void *) NULL, new_record->data, nc, 32, 0, stride, 1);
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

                int32_t * field3 = (int32_t*)field;
                const int64_t num_elem = fst24_record_num_elem(record);

                if (record->data_bits == 64) {
                    memcpy(new_record->data, field, num_elem * sizeof(int64_t));
                }
                else {
                    if (record->data_bits == 16 || record->data_bits == 8) {
                        if (num_elem > (1 << 30)) {
                            Lib_Log(APP_LIBFST, APP_ERROR,
                                "%s: Number of elements in record (%ld) is too large for what we can handle (%d) for now\n",
                                __func__, num_elem, (1<<30));
                        }
                        field3 = (int *)alloca(num_elem * sizeof(int));
                        short * s_field = (short *)field;
                        signed char * b_field = (signed char *)field;
                        if (record->data_bits == 16) for (int i = 0; i < num_elem;i++) { field3[i] = s_field[i]; };
                        if (record->data_bits == 8)  for (int i = 0; i < num_elem;i++) { field3[i] = b_field[i]; };
                    }
                    compact_integer(field3, (void *) NULL, new_record->data, num_elem, record->pack_bits, 0, stride, 3);
                }

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
                            (unsigned char *)&((uint32_t *)new_record->data)[1], (void *)field, record->ni, record->nj,
                            record->nk, record->pack_bits);

                        if (compressed_lng < 0) {
                            stdf_entry->datyp = FST_TYPE_REAL_IEEE;
                            f77name(ieeepak)((int32_t *)field, new_record->data, &f_ni, &f_njnk, &f_minus_nbits, &f_zero, &f_one);
                        } else {
                            const int nbytes = 16 + compressed_lng;
                            const uint32_t num_word64 = (nbytes * 8 + 63) / 64;
                            const uint32_t num_word32 = W64TOWD(num_word64);
                            ((uint32_t *)new_record->data)[0] = num_word32;
                            RSF_Record_set_num_elements(new_record, num_word32, sizeof(uint32_t));
                        }
                    } else {
                        if (data_type == FST_TYPE_COMPLEX) f_ni = f_ni * 2;
                        f77name(ieeepak)((int32_t *)data_f, new_record->data, &f_ni, &f_njnk, &f_minus_nbits, &f_zero, &f_one);
                    }
                }
                break;

            case FST_TYPE_REAL:
            case FST_TYPE_REAL | FST_TYPE_TURBOPACK:
                // floating point, new packers

                if (is_type_turbopack(data_type) && (record->pack_bits <= 16)) {
                    // use an additional compression scheme
                    c_float_packer((void *)field, record->pack_bits, &((int32_t *)new_record->data)[1],
                                   &((int32_t *)new_record->data)[1+header_size], num_elements);
                    const int compressed_lng = armn_compress(
                        (unsigned char *)&((uint32_t *)new_record->data)[1+header_size], record->ni, record->nj,
                        record->nk, record->pack_bits, 1, 1);
                    if (compressed_lng < 0) {
                        stdf_entry->datyp = FST_TYPE_REAL;
                        c_float_packer((void *)field, record->pack_bits, new_record->data, &((int32_t *)new_record->data)[header_size],
                                        num_elements);
                    } else {
                        const int nbytes = 16 + (header_size*4) + compressed_lng;
                        const uint32_t num_word64 = (nbytes * 8 + 63) / 64;
                        const uint32_t num_word32 = W64TOWD(num_word64);
                        ((uint32_t *)new_record->data)[0] = num_word32;
                        RSF_Record_set_num_elements(new_record, num_word32, sizeof(uint32_t));
                    }
                } else {
                    c_float_packer((void *)field, record->pack_bits, new_record->data,
                                   &((int32_t *)new_record->data)[header_size], num_elements);
                }
                break;


            case FST_TYPE_STRING:
            case FST_TYPE_STRING | FST_TYPE_TURBOPACK:
                // character string
                if (is_type_turbopack(data_type)) {
                    Lib_Log(APP_LIBFST, APP_WARNING, "%s: extra compression not available, data type %d reset to FST_TYPE_STRING (%d)\n",
                            __func__, stdf_entry->datyp, FST_TYPE_STRING);
                    data_type = FST_TYPE_STRING;
                    stdf_entry->datyp = data_type;
                }
                compact_char(field, (void *) NULL, new_record->data, num_elements, 8, 0, stride, 9);
                break;

            default:
                Lib_Log(APP_LIBFST, APP_ERROR, "%s: invalid data_type=%d\n", __func__, data_type);
                return ERR_BAD_DATYP;
        } // end switch
    } // end if image mode copy

    record->data_type = stdf_entry->datyp;
    record->pack_bits = stdf_entry->nbits;
    record->data_bits = stdf_entry->dasiz;

    // write new_record to file and add entry to directory
    const int64_t record_handle = RSF_Put_record(rsf_file, new_record, num_data_bytes);
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

    return record_handle > 0 ? TRUE : -1;
}

//! Write the given record into the given standard file
//!
//! Thread safety: Several threads may write concurrently in the same open file (the same fst_file struct), as
//! well as in different files. The fst_record to write must be different though.
//!
//! \return TRUE (1) if everything was a success, a negative error code otherwise
int32_t fst24_write(
    fst_file* file,     //!< [in,out] The file where we want to write
    fst_record* record, //!< [in,out] The record we want to write
    const int rewrite   //!< Whether we want to overwrite FST_YES, skip FST_SKIP or write again FST_NO an existing record
) {
    fst_record crit = default_fst_record;

    if (!fst24_is_open(file)) return ERR_NO_FILE;
    if (!fst24_record_is_valid(record)) return ERR_BAD_INIT;

    record->file = file;

    Lib_Log(APP_LIBFST, APP_DEBUG, "%s: file %s, type %s, rewrite %d\n",
            __func__, file->path, fst_file_type_name[file->type], rewrite);

    if (rewrite) { 
        fst24_record_copy_metadata(&crit,record,FST_META_GRID|FST_META_INFO|FST_META_TIME|FST_META_SIZE);
        crit.datev = get_valid_date32(crit.dateo,crit.deet,crit.npas);
        crit.dateo=-1;
    }
    if (rewrite==FST_SKIP) {
        fst_query* q = fst24_new_query(file, &crit, NULL);
        if (fst24_find_next(q,NULL)) {
            Lib_Log(APP_LIBFST, APP_INFO, "%s: Skipping already existing record\n", __func__);
            return(TRUE);
        }
        fst24_query_free(q);
    } 

    if  (file->type == FST_RSF) {
        if (rewrite==FST_YES) {
            int nb;
            if ((nb=fst24_search_and_delete(file, &crit, NULL))) {
                Lib_Log(APP_LIBFST, APP_INFO, "%s: Deleted %i matching records\n", __func__,nb);
            }
        }
        return fst24_write_rsf(file->rsf_handle, record, 1);
    }
    else {
        if (record->metadata != NULL) {
            Lib_Log(APP_LIBFST, APP_WARNING, "%s: Trying to write a record that contains extra metadata in an XDF file."
                    " This is not supported, we will ignore that metadata. (file %s)\n", __func__, file->path);
        }

        if (record->data == NULL) {
            Lib_Log(APP_LIBFST, APP_ERROR, "%s: No data associated with this record!\n", __func__);
            return -1;
        }

        char typvar[FST_TYPVAR_LEN];
        char nomvar[FST_NOMVAR_LEN];
        char etiket[FST_ETIKET_LEN];
        char grtyp[FST_GTYP_LEN];

        strncpy(typvar, record->typvar, FST_TYPVAR_LEN);
        strncpy(nomvar, record->nomvar, FST_NOMVAR_LEN);
        strncpy(etiket, record->etiket, FST_ETIKET_LEN);
        strncpy(grtyp, record->grtyp, FST_GTYP_LEN);

        // --- START critical region ---
        pthread_mutex_lock(&fst24_xdf_mutex);

        if (record->data_bits == 8) {
            c_fst_data_length(1);
        }
        else if (record->data_bits == 16) {
            c_fst_data_length(2);
        }
        else if (record->data_bits == 64) {
            c_fst_data_length(8);
        }

        const int ier = c_fstecr_xdf(
            record->data, NULL, -record->pack_bits, file->iun, record->dateo, record->deet, record->npas,
            record->ni, record->nj, record->nk, record->ip1, record->ip2, record->ip3,
            typvar, nomvar, etiket, grtyp, record->ig1, record->ig2, record->ig3, record->ig4, record->data_type, rewrite==FST_YES);

        pthread_mutex_unlock(&fst24_xdf_mutex);
        // --- END critical region ---

        record->do_not_touch.num_search_keys = sizeof(stdf_dir_keys) / sizeof(int32_t) - 2;
        record->do_not_touch.extended_meta_size = 0;
        record->do_not_touch.stored_data_size = 0; // We don't have a good way of knowing that number, so it stays at 0 for now. Maybe xdfprm?
        record->file_index = -1;

        if (ier < 0) return ier;
        return TRUE;
    }

    Lib_Log(APP_LIBFST, APP_ERROR, "%s: Unknown/invalid file type %d (%s)\n", __func__, file->type, file->path);
    return -1;
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

    fill_with_search_meta(record, (search_metadata*)record_info.meta, FST_RSF);
    record->num_meta_bytes = record_info.rec_meta * sizeof(uint32_t);
    record->do_not_touch.handle = key;
    record->do_not_touch.stored_data_size = (record_info.data_size + 3) / 4;
    if (record_info.rec_type == RT_DEL) record->do_not_touch.deleted = 1;
    record->file_index = RSF_Key64_to_index(key);

    return TRUE;
}

static inline int32_t fst24_make_index_from_xdf_handle(const int handle) {
    return RECORD_FROM_HANDLE((handle & 0xffffffff)) + (PAGENO_FROM_HANDLE((handle & 0xffffffff)) * ENTRIES_PER_PAGE);
}

static inline int32_t fst24_make_xdf_handle_from_index(const int index, const int file_id) {
    return MAKE_RND_HANDLE(index / ENTRIES_PER_PAGE, index % ENTRIES_PER_PAGE, file_id);
}

//! Get basic information about the record with the given key (search or "directory" metadata)
//!
//! Thread safety: This function may be called concurrently by several threads for the same file.
//! The output must be to a different fst_record object.
//!
//! \return TRUE (1) if we were able to get the info, FALSE (0) or a negative number otherwise
int32_t fst24_get_record_from_key(
    const fst_file* const file, //!< [in] File to which the record belongs. Must be open
    const int64_t key,          //!< [in] Key of the record we are looking for. Must be valid
    fst_record* const record    //!< [in,out] Record information (no data or advanced metadata)
) {
    if (!fst24_is_open(file)) {
       Lib_Log(APP_LIBFST, APP_ERROR, "%s: File not open (%s)\n", __func__, file ? file->path : "(nil)");
       return FALSE;
    }

    if (key < 0) {
        Lib_Log(APP_LIBFST, APP_ERROR, "%s: Invalid key\n", __func__);
        return FALSE;
    }

    fst_record_set_to_default(record);
    record->do_not_touch.handle = key;

    if (file->type == FST_RSF) {
        RSF_handle file_handle = FGFDT[file->file_index].rsf_fh;
        if (get_record_from_key_rsf(file_handle, key, record) <= 0) {
            Lib_Log(APP_LIBFST, APP_ERROR, "%s: Unable to get record with key %lx\n", __func__, key);
            return FALSE;
        }
    }
    else if (file->type == FST_XDF) {
        int addr, lng, idtyp;
        search_metadata record_meta;
        stdf_dir_keys* record_meta_xdf = &record_meta.fst98_meta;
        uint32_t* pkeys = (uint32_t *) record_meta_xdf;
        pkeys += W64TOWD(1);
        if (c_xdfprm(key & 0xffffffff, &addr, &lng, &idtyp, pkeys, 16) < 0) {
            Lib_Log(APP_LIBFST, APP_ERROR, "%s: Unable to get record with key %x\n", __func__, key & 0xffffffff);
            return FALSE;
        }
        fill_with_search_meta(record, &record_meta, file->type);
        record->do_not_touch.num_search_keys = 16;
        record->do_not_touch.stored_data_size = W64TOWD(lng);
        record->file_index = fst24_make_index_from_xdf_handle(key & 0xffffffff);
    }
    else {
        Lib_Log(APP_LIBFST, APP_ERROR, "%s: Unknown/invalid file type %d (%s)\n", __func__, file->type, file->path);
        return FALSE;
    }

    record->file = file;
    return TRUE;
}

//! Retrieve record information at a given index.
//!
//! Thread safety: This function may be called concurrently by several threads for the same file.
//! The output must be to a different fst_record object.
//!
//! \return TRUE (1) if everything was successful, FALSE (0) or negative if there was an error.
int32_t fst24_get_record_by_index(
    const fst_file* const file, //!< [in] File handle
    const int32_t index,        //!< [in] Record key within its file
    fst_record* const record    //!< [in,out] Record information
) {
    if (!fst24_is_open(file)) return ERR_NO_FILE;

    fst_record_set_to_default(record);

    // const int64_t num_records = fst24_get_num_records_single(file);
    // if (index >= num_records) {
    //     Lib_Log(APP_LIBFST, APP_ERROR, "%s: requested file key %d is beyond the number of records (%d)\n",
    //             __func__, index, num_records);
    //     return ERR_OUT_RANGE;
    // }
    record->file = file;

    if (file->type == FST_RSF) {
        RSF_handle file_handle = FGFDT[file->file_index].rsf_fh;
        const RSF_record_info record_info = RSF_Get_record_info_by_index(file_handle, index);

        if (record_info.rec_type == RT_NULL) return FALSE; // Error retrieving the record

        fill_with_search_meta(record, (search_metadata*)record_info.meta, file->type);
        record->do_not_touch.handle = RSF_Make_key(file->file_index_backend, index);
        record->num_meta_bytes = record_info.rec_meta * sizeof(uint32_t);
        record->file_index = index;
        return TRUE;
    }
    else if (file->type == FST_XDF) {
        const int32_t key = fst24_make_xdf_handle_from_index(index, file->file_index_backend);

        int addr, lng, idtyp;
        search_metadata record_meta;
        stdf_dir_keys* record_meta_xdf = &record_meta.fst98_meta;
        uint32_t* pkeys = (uint32_t *) record_meta_xdf;
        pkeys += W64TOWD(1);
        if (c_xdfprm(key, &addr, &lng, &idtyp, pkeys, 16) < 0) return FALSE;
        if ((idtyp | 0x80) == 255 || (idtyp | 0x80) == 254) return FALSE; // Record is deleted

        fill_with_search_meta(record, &record_meta, file->type);
        record->do_not_touch.handle = key;
        record->file_index = index;
        return TRUE;
    }

    return FALSE;
}

//! Create a search query that will apply the given criteria during a search in a file.
//!
//! This function is thread safe.
//!
//! \return A pointer to a search query if the inputs are valid (open file, OK criteria struct), NULL otherwise
fst_query* fst24_new_query(
    const fst_file* const file, //!< File that will be searched with the query
    const fst_record* criteria, //!< [Optional] Criteria to be used for the search. If NULL, will look for any record
    const fst_query_options* options //!< [Optional] Options to modify how the search will be performed
) {
    if (!fst24_is_open(file)) {
       Lib_Log(APP_LIBFST, APP_ERROR, "%s: File not open (%s)\n", __func__, file ? file->path : "(nil)");
       return FALSE;
    }

    if (criteria == NULL) {
       criteria = &default_fst_record;
    }
    else {
        if (!fst24_record_is_valid(criteria)) {
            Lib_Log(APP_LIBFST, APP_ERROR, "%s: Invalid criteria\n", __func__);        
            return NULL;
        }
    }

    fst_query* query = (fst_query*)malloc(sizeof(fst_query));

    if (query == NULL) {
        Lib_Log(APP_LIBFST, APP_FATAL, "%s: Unable to allocate space for a new query\n", __func__);
        return NULL;
    }

    *query = new_fst_query(options);
    make_search_criteria(criteria, query);
    query->num_criteria = sizeof(query->criteria) / sizeof(uint32_t);
    query->search_index = criteria->do_not_touch.handle > 0 ? criteria->do_not_touch.handle : 0;
    query->file         = file;

    if (criteria->metadata != NULL) {
        if (file->type == FST_RSF) {
            query->search_meta  = criteria->metadata;
        }
        else {
            Lib_Log(APP_LIBFST, APP_WARNING, "%s: Extended metadata criterion is non-NULL, but we can only search"
                    " extended metadata in RSF files (%s)\n", __func__, file->path);
        }
    }

    if (Lib_LogLevel(APP_LIBFST, NULL) >= APP_DEBUG) {
        Lib_Log(APP_LIBFST, APP_DEBUG, "%s: Setting search criteria\n", __func__);
        print_non_wildcards(criteria);
        print_non_default_options(&query->options);
    }

    return query;
}

//! Reset start index of search without changing the criteria.
//!
//! Thread safety: Can be called concurrently only if the queries are different objects.
//!
//! \return TRUE (1) if file is valid and open, FALSE (0) otherwise
int32_t fst24_rewind_search(fst_query* const query) {
    if (!fst24_query_is_valid(query)) {
        Lib_Log(APP_LIBFST, APP_ERROR, "%s: Query is not valid\n", __func__);
        return FALSE;
    }

    query->search_index = 0;
    query->search_done  = 0;

    if (query->next != NULL) {
        return fst24_rewind_search(query->next);
    }

    return TRUE;
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

//! Find the next record in a given XDF file, according to the given parameters
//! \return Key of the record found (negative if error or nothing found)
int64_t find_next_xdf(const int32_t iun, fst_query* const query) {
    uint32_t* pkeys = (uint32_t *) &query->criteria.fst98_meta;
    uint32_t* pmask = (uint32_t *) &query->mask.fst98_meta;

    pkeys += W64TOWD(1);
    pmask += W64TOWD(1);

    const int32_t start_key = query->search_index & 0xffffffff;

    const int64_t key = (int64_t) c_xdfloc2(iun, start_key, pkeys, 16, pmask);
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

//! Make sure that the (next) query linked to this given query will
//! search in the (next) file linked to this given query's file
//! It's slightly complicated because we want to be able to search in the
//! correct file(s) even if they were unlinked and re-linked differently
static void ensure_next_query(fst_query* query) {
    if (query->file->next == NULL) return;
    if (query->next != NULL && query->next->file == query->file->next) return;

    if (query->next != NULL) fst24_query_free(query->next);

    query->next = (fst_query*)malloc(sizeof(fst_query));
    if (query->next == NULL) {
        Lib_Log(APP_LIBFST, APP_FATAL, "%s: Unable to allocate space (%d bytes) for a new fst_query\n",
                __func__, sizeof(fst_query));
    }
    *(query->next) = fst_query_copy(query);
    query->next->file = query->file->next;
}

//! For some searches, we are not looking for an exact match at certain attributes, so we need to
//! check those manually, outside the backend search functions
int32_t is_actual_match(fst_record* const record, const fst_query* const query) {

    // Check on excdes desire/exclure clauses
    if (query->file->type == FST_RSF &&
        !C_fst_rsf_match_req(record->datev, record->ni, record->nj, record->nk, record->ip1, record->ip2, record->ip3,
        record->typvar, record->nomvar, record->etiket, record->grtyp, record->ig1, record->ig2, record->ig3, record->ig4)) {
        return FALSE;
    }

    // If search on all IP encodings is requested
    if (query->options.ip1_all > 0) {
        if (record->ip1 != query->ip1s[0] && record->ip1 != query->ip1s[1]) return FALSE;
    }

    if (query->options.ip2_all > 0) {
        if (record->ip2 != query->ip2s[0] && record->ip2 != query->ip2s[1]) return FALSE;
    }

    if (query->options.ip3_all > 0) {
        if (record->ip3 != query->ip3s[0] && record->ip3 != query->ip3s[1]) return FALSE;
    }

    // If metadata search is specified, look for a match or carry on looking
    if (query->search_meta != NULL) {
        if (!fst24_read_metadata(record)) {
            Lib_Log(APP_LIBFST, APP_ERROR, "%s: Unable to read metadata while doing search (%s)\n", __func__, query->file->path);
            return FALSE;
        }

        if (!Meta_Match(query->search_meta, record->metadata, FALSE)) return FALSE;
    }

    return TRUE;
}

//! Find the next record in the given file that matches the given query criteria. Search through linked files, if any.
//!
//! Thread safety: This function may be called concurrently by several threads on *different queries* that belong to
//! the same file. However, it cannot be called concurrently on the same fst_query object.
//!
//! \return TRUE (1) if a record was found, FALSE (0) or a negative number otherwise (not found, file not open, etc.)
int32_t fst24_find_next(
    fst_query* const query, //!< [in] Query used for the search. Must be for an open file.
    //!> [in,out] Will contain record information if found and, optionally, metadata (if included in the search query).
    //!> If NULL, we will only check for the existence of a match to the query, without extracting any information from
    //!> that match. If not NULL, must be a valid, initialized record.
    fst_record* record
) {
    if (!fst24_query_is_valid(query)) {
        Lib_Log(APP_LIBFST, APP_ERROR, "%s: Query at %p is not valid\n", __func__, query);
        return FALSE;
    }

    if ((record != NULL) && !fst24_record_is_valid(record)) {
        Lib_Log(APP_LIBFST, APP_ERROR, "%s: Must give a valid record into which information will be put\n", __func__);
        return FALSE;
    }

    // Skip search, or search next file in linked list, if we were already done searching this file
    if (query->search_done == 1) {
        if (query->file->next != NULL) {
            ensure_next_query(query);
            return fst24_find_next(query->next, record);
        }
        return FALSE;
    }

    Lib_Log(APP_LIBFST, APP_DEBUG, "%s: Searching in file %s at %p (next %p)\n", __func__, query->file->path, query->file, query->file->next);

    fst_record tmp_record = default_fst_record;
    while (TRUE) { // Search forever (until found or end-of-file)
        const int64_t key = 
            query->file->type == FST_RSF ? find_next_rsf(query->file->rsf_handle, query) :
            query->file->type == FST_XDF ? find_next_xdf(query->file->iun, query) :
                                           -1;

        if (key < 0) break; // Not in this file

        if (fst24_get_record_from_key(query->file, key, &tmp_record) != TRUE) {
            Lib_Log(APP_LIBFST, APP_ERROR, "%s: Unable to retrieve record info after having found it in %s.\n",
                    __func__, query->file->path);
            return -1;
        }

        if (!is_actual_match(&tmp_record, query)) continue; // Keep looking

        Lib_Log(APP_LIBFST, APP_DEBUG, "%s: (unit=%d) Found record at key 0x%x in file %s\n",
                __func__, query->file->iun, key, query->file->path);

        if (record != NULL) fst_record_copy_info(record, &tmp_record);
        query->search_index = key;

        return TRUE;
    }

    // We haven't found anything in this file
    query->search_done = 1;

    if (query->file->next != NULL) {
        // We're done searching this file, but there's another one in the linked list, so 
        // we need to setup the search in that one
        ensure_next_query(query);
        return fst24_find_next(query->next, record);
    }

    Lib_Log(APP_LIBFST, APP_DEBUG, "%s: No (more) record found matching the criteria\n", __func__);

    return FALSE;
}

//! Find all record that match the given query, up to a certain maximum.
//! Search through linked files, if any.
//!
//! Thread safety: This function may be called concurrently by several threads on *different queries* that belong to
//! the same file. However, it cannot be called concurrently on the same fst_query object.
//!
//! \return Number of records found, 0 if none or if error.
int32_t fst24_find_all(
    //!> [in,out] Query used for the search. Will be rewinded before doing the search, but when the function returns,
    //!> it will be pointing to the end of its search.
    fst_query* query,
    fst_record* results,          //!< [in,out] List of records found. The list must be already allocated
    const int32_t max_num_results //!< [in] Size of the given list of records. We will stop looking if we find that many
) {
    int32_t max;

    if (fst24_rewind_search(query) != TRUE) return 0;

    max = results ? max_num_results : INT_MAX;

    for (int i = 0; i < max; i++) {
        if (results) {
           results[i] = default_fst_record;
           if (!fst24_find_next(query, &(results[i]))) return i;
        } else {
           if (!fst24_find_next(query, NULL)) return i;
        }
    }
    return max_num_results;
}

//! Get the number of records matching to query
//!
//! Thread safety: This function may be called concurrently by several threads on *different queries* that belong to
//! the same file. However, it cannot be called concurrently on the same fst_query object.
//!
//! \return Number of records found by the query
int32_t fst24_find_count(
    //!> [in,out] Query used for the search. It will be rewinded before doing the search, but when the function
    //!> returns, it will point to the end of its search.
    fst_query * const query
) {
    fst_record record = default_fst_record;

    fst24_rewind_search(query);

    int32_t count = 0;
    while (fst24_find_next(query, &record)) {
        count++;
    }
    return count;
}


//! Unpack the given data array, according to the given record information
int32_t fst24_unpack_data(
    void* dest,
    void* source, //!< Should be const, but we might swap stuff in-place. It's supposed to be temporary anyway...
    const fst_record* record, //!< [in] Record information
    const int32_t skip_unpack,//!< Only copy data if non-zero
    const int32_t stride      //!< Kept for compatibility with fst98 interface
) {
    uint32_t* dest_u32 = dest;
    uint32_t* source_u32 = source;

    // Get missing data flag
    const int has_missing = has_type_missing(record->data_type);
    // Suppress missing data flag
    const int32_t simple_data_type = record->data_type & ~FSTD_MISSING_FLAG;

    // Unpack function son output element size
    PackFunctionPointer packfunc;
    if (record->data_bits == 64) {
        packfunc = &compact_double;
    } else {
        packfunc = &compact_float;
    }

    // const size_t record_size_32 = record->rsz / 4;
    // size_t record_size = record_size_32;
    // if ((simple_data_type == FST_TYPE_OLD_QUANT) || (simple_data_type == FST_TYPE_REAL_IEEE)) {
    //     record_size = (record->data_bits == 64) ? 2*record_size : record_size;
    // }

    const int multiplier = (simple_data_type == FST_TYPE_COMPLEX) ? 2 : 1;
    const int nelm = fst24_record_num_elem(record) * multiplier;

    Lib_Log(APP_LIBFST, APP_EXTRA, "%s: Unpacking %d %d-bit %s elements from %p into %p\n",
            __func__, nelm, record->data_bits, FST_TYPE_NAMES[base_fst_type(record->data_type)], source, dest);

    double dmin = 0.0;
    double dmax = 0.0;

    const int bitmot = 32;
    int ier = 0;
    if (skip_unpack) {
        if (is_type_turbopack(simple_data_type)) {
            int lngw = ((int *)source)[0];
            // fprintf(stderr, "Debug+ lecture mode image lngw=%d\n", lngw);
            memcpy(dest, source, (lngw + 1) * sizeof(uint32_t));
        } else {
            int lngw = nelm * record->pack_bits;
            if (simple_data_type == FST_TYPE_REAL_OLD_QUANT) lngw += 120;
            if (simple_data_type == FST_TYPE_CHAR) lngw = record->ni * record->nj * 8;
            if (simple_data_type == FST_TYPE_REAL) {
                int header_size, stream_size, p1out, p2out;
                c_float_packer_params(&header_size, &stream_size, &p1out, &p2out, nelm);
                lngw = (header_size + stream_size) * 8;
            }
            lngw = (lngw + bitmot - 1) / bitmot;
            memcpy(dest, source, lngw * sizeof(uint32_t));
        }
    } else {
        switch (simple_data_type) {
            case FST_TYPE_BINARY:
            {
                // Raw binary
                const int lngw = ((nelm * record->pack_bits) + bitmot - 1) / bitmot;
                memcpy(dest, source, lngw * sizeof(uint32_t));
                break;
            }

            case FST_TYPE_REAL_OLD_QUANT:
            case FST_TYPE_REAL_OLD_QUANT | FST_TYPE_TURBOPACK:
            {
                // Floating Point
                double tempfloat = 99999.0;
                if (is_type_turbopack(record->data_type)) {
                    armn_compress((unsigned char *)(source_u32+ 5), record->ni, record->nj, record->nk, record->pack_bits, 2, 1);
                    packfunc(dest_u32, source_u32 + 1, source_u32 + 5, nelm, record->pack_bits + 64 * Max(16, record->pack_bits),
                             0, stride, FLOAT_UNPACK, 0, &tempfloat, &dmin, &dmax);
                } else {
                    packfunc(dest_u32, source_u32, source_u32 + 3, nelm, record->pack_bits, 24, stride, FLOAT_UNPACK,
                             0, &tempfloat, &dmin, &dmax);
                }
                break;
            }

            case FST_TYPE_UNSIGNED:
            case FST_TYPE_UNSIGNED | FST_TYPE_TURBOPACK:
            {
                // Integer, short integer or byte stream
                const int offset = is_type_turbopack(record->data_type) ? 1 : 0;
                if (record->data_bits == 16) {
                    if (is_type_turbopack(record->data_type)) {
                        const int nbytes = armn_compress((unsigned char *)(source_u32 + offset), record->ni, record->nj,
                            record->nk, record->pack_bits, 2, 0);
                        memcpy(dest, source_u32 + offset, nbytes);
                    } else {
                        ier = compact_short(dest, (void *) NULL, (void *)(source_u32 + offset), nelm, record->pack_bits, 0, stride, 6);
                    }
                }  else if (record->data_bits == 8) {
                    if (is_type_turbopack(record->data_type)) {
                        armn_compress((unsigned char *)(source_u32 + offset), record->ni, record->nj, record->nk, record->pack_bits, 2, 0);
                        memcpy_16_8((int8_t *)dest, (int16_t *)(source_u32 + offset), nelm);
                    } else {
                        ier = compact_char(dest, (void *)NULL, (void *)source, nelm, 8, 0, stride, 10);
                    }
                } else if (record->data_bits == 64) {
                    memcpy(dest, source, nelm * sizeof(uint64_t));
                } else {
                    if (is_type_turbopack(record->data_type)) {
                        armn_compress((unsigned char *)(source_u32 + offset), record->ni, record->nj, record->nk, record->pack_bits, 2, 0);
                        memcpy_16_32((int32_t *)dest, (int16_t *)(source_u32 + offset), record->pack_bits, nelm);
                    } else {
                        ier = compact_integer(dest, (void *)NULL, source_u32 + offset, nelm, record->pack_bits, 0, stride, 2);
                    }
                }
                break;
            }

            case FST_TYPE_CHAR:
            {
                // Character
                const int num_ints = (nelm + 3) / 4;
                ier = compact_integer(dest, (void *)NULL, source, num_ints, 32, 0, stride, 2);
                break;
            }

            case FST_TYPE_SIGNED: {
                if (record->data_bits == 64) {
                    memcpy(dest, source, nelm * sizeof(int64_t));
                }
                else {
                    const int use32 = !(record->data_bits == 64 || record->data_bits == 16 || record->data_bits == 8);
                    int32_t*     field_out   = use32 ? dest : alloca(nelm * sizeof(int32_t));
                    int16_t*     field_out_16 = (int16_t*)dest;
                    int8_t*      field_out_8  = (int8_t*)dest;
                    ier = compact_integer(field_out, (void *) NULL, source, nelm, record->pack_bits, 0, stride, 4);
                    if (record->data_bits == 16) {
                        for (int i = 0; i < nelm; i++) {
                            field_out_16[i] = field_out[i];
                        }
                    }
                    else if (record->data_bits == 8) {
                        for (int i = 0; i < nelm; i++) {
                            field_out_8[i] = field_out[i];
                        }
                    }
                }
                break;
            }

            case FST_TYPE_REAL_IEEE:
            case FST_TYPE_COMPLEX: {

                // IEEE representation
                if ((downgrade_32) && (record->data_bits == 64)) {
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
                    const int32_t npak = -record->pack_bits;
                    f77name(ieeepak)((int32_t *)dest, source, &nelm, &f_one, &npak, &f_zero, &f_mode);
                }

                break;
            }

            case FST_TYPE_REAL:
            case FST_TYPE_REAL | FST_TYPE_TURBOPACK:
            {
                int bits;
                int header_size, stream_size, p1out, p2out;
                c_float_packer_params(&header_size, &stream_size, &p1out, &p2out, nelm);
                header_size /= 4;
                if (is_type_turbopack(record->data_type)) {
                    armn_compress((unsigned char *)(source_u32 + 1 + header_size), record->ni, record->nj, record->nk, record->pack_bits, 2, 1);
                    c_float_unpacker((float *)dest, (int32_t *)(source_u32 + 1), (int32_t *)(source_u32 + 1 + header_size), nelm, &bits);
                } else {
                    c_float_unpacker((float *)dest, (int32_t *)source, (int32_t *)(source_u32 + header_size), nelm, &bits);
                }
                break;
            }

            case FST_TYPE_REAL_IEEE | FST_TYPE_TURBOPACK:
            {
                // Floating point, new packers
                c_armn_uncompress32((float *)dest, (unsigned char *)(source_u32 + 1), record->ni, record->nj, record->nk, record->pack_bits);
                break;
            }

            case FST_TYPE_STRING:
                // Character string
                ier = compact_char(dest, (void *)NULL, source, nelm, 8, 0, stride, 10);
                break;

            default:
                Lib_Log(APP_LIBFST, APP_ERROR, "%s: invalid data_type=%d\n", __func__, simple_data_type);
                return(ERR_BAD_DATYP);
        } // end switch
    }

    Lib_Log(APP_LIBFST, APP_DEBUG, "%s: Read record with key 0x%x\n", __func__, record->do_not_touch.handle);
    if (Lib_LogLevel(APP_LIBFST, NULL) >= APP_EXTRA) fst24_record_print_short(record, NULL, 1, NULL);

    if (has_missing) {
        // Replace "missing" data points with the appropriate values given the type of data (int/float)
        // if nbits = 64 and IEEE , set double
        // TODO review this logic
        int sz = record->data_bits;
        if (is_type_real(record->data_type) && record->data_bits == 64 ) sz = 64;
        DecodeMissingValue(dest, fst24_record_num_elem(record), record->data_type & 0x3F, sz);
    }

    return 0;
}

//! Read a record from an RSF file
//! \return 0 for success, negative for error
int32_t fst24_read_record_rsf(
    const RSF_handle rsf_file,  //!< Handle to the file where the record is stored
    //!> [in,out] Record for which we want to read data.
    //!> Must have a valid handle!
    //!> Must have already allocated its data buffer
    fst_record* record_fst,
    const int32_t skip_unpack,  //!< Whether to skip the unpacking process (e.g. if we just want to copy the record)
    const int32_t metadata_only //!< Whether we want to only read metadata, rather than including everything
) {
    RSF_handle file_handle = FGFDT[record_fst->file->file_index].rsf_fh;
    if (!RSF_Is_record_in_file(file_handle, record_fst->do_not_touch.handle)) return ERR_BAD_HNDL;

    if (record_fst->do_not_touch.deleted == 1) {
        Lib_Log(APP_LIBFST, APP_WARNING, "%s: Cannot read data from a deleted record\n", __func__);
        return ERR_BAD_HNDL;
    }

    const size_t work_size_bytes = fst24_record_data_size(record_fst) +     // The data itself
                                   record_fst->num_meta_bytes +             // The metadata
                                   sizeof(RSF_record) +                     // Space for the RSF struct itself
                                   128 * sizeof(uint32_t);                  // Enough space for the largest compression scheme + rounding up for alignment

    uint64_t work_space[work_size_bytes / sizeof(uint64_t)];
    memset(work_space, 0, sizeof(work_space));

    RSF_record* record_rsf = RSF_Get_record(file_handle, record_fst->do_not_touch.handle, metadata_only, (void*)work_space);

    if ((uint64_t*)record_rsf != work_space) {
        Lib_Log(APP_LIBFST, APP_ERROR, "%s: Could not get record corresponding to key 0x%x\n",
                __func__, record_fst->do_not_touch.handle);
        return ERR_BAD_HNDL;
    }

    fill_with_search_meta(record_fst, (search_metadata*)record_rsf->meta, FST_RSF);
    record_fst->do_not_touch.stored_data_size = (record_rsf->data_size + 3) / 4;

    // Extract metadata from record if present
    record_fst->metadata = NULL;
    if (record_fst->do_not_touch.extended_meta_size > 0) {
        // Located after the search keys
        record_fst->metadata = Meta_Parse((char*)((uint32_t*)record_rsf->meta + record_fst->do_not_touch.num_search_keys));
    }

    // Extract data
    int32_t ier = 0;
    if (metadata_only != 1)
        ier = fst24_unpack_data(record_fst->data, record_rsf->data, record_fst, skip_unpack, 1);

    if (Lib_LogLevel(APP_LIBFST, NULL) >= APP_INFO) {
        fst_record_fields f = default_fields;
        // f.grid_info = 1;
        f.deet = 1;
        f.npas = 1;
        fst24_record_print_short(record_fst, &f, 0, "(fst) Read : ");
    }

    return ier;
}

//! Read a record from an XDF file
//! \return 0 for success, negative for error
int32_t fst24_read_record_xdf(
    //!> [in,out] Record for which we want to read data.
    //!> Must have a valid handle!
    //!> Must have already allocated its data buffer
    fst_record* record
) {
    const int32_t key32 = record->do_not_touch.handle & 0xffffffff;

    if (!c_xdf_handle_in_file(key32)) return ERR_BAD_HNDL;

    search_metadata meta;
    stdf_dir_keys* stdf_entry = &meta.fst98_meta;
    uint32_t * pkeys = (uint32_t *) stdf_entry;
    pkeys += W64TOWD(1);

    int lng = 0;
    {
        int addr, idtyp;
        int ier = c_xdfprm(key32, &addr, &lng, &idtyp, pkeys, 16);
        if (ier < 0) return ier;
    }

    // --- START critical region ---
    pthread_mutex_lock(&fst24_xdf_mutex);

    if (record->data_bits == 8) {
        c_fst_data_length(1);
    }
    else if (record->data_bits == 16) {
        c_fst_data_length(2);
    }
    else if (record->data_bits == 64) {
        c_fst_data_length(8);
    }
    const int32_t handle = c_fstluk_xdf(record->data, key32, &record->ni, &record->nj, &record->nk);

    pthread_mutex_unlock(&fst24_xdf_mutex);
    // --- END critical region ---

    if (handle != key32) return ERR_NOT_FOUND;
    fill_with_search_meta(record, &meta, FST_XDF);
    record->do_not_touch.num_search_keys = 16;
    record->do_not_touch.stored_data_size = W64TOWD(lng);
    return handle;
}

//! Read only metadata for the given record
//!
//! Thread safety: This function may be called concurrently by several threads on *different records* that belong to
//! the same file. However, it cannot be called concurrently on the same fst_record object.
//!
//! \return A pointer to the metadata, NULL if error (or no metadata)
void* fst24_read_metadata(
    fst_record* record //!< [in,out] Record for which we want to read metadata. Must have a valid handle!
) {
    if (!fst24_record_is_valid(record) || record->do_not_touch.handle < 0) {
       Lib_Log(APP_LIBFST, APP_ERROR, "%s: Invalid record\n", __func__);
       return NULL;
    }

    if (!fst24_is_open(record->file)) {
       Lib_Log(APP_LIBFST, APP_ERROR, "%s: File not open (%s)\n",__func__, record->file ? record->file->path : "(nil)");
       return NULL;
    }

    if (record->file->type == FST_RSF) {
        if (fst24_read_record_rsf(record->file->rsf_handle, record, 0, 1) != 0) {
            Lib_Log(APP_LIBFST, APP_ERROR, "%s: Error trying to read meta from RSF file %s\n", __func__, record->file->path);
            return NULL;
        }
        return record->metadata;
    }
    else if (record->file->type == FST_XDF) {
        Lib_Log(APP_LIBFST, APP_WARNING, "%s: Cannot read metatada for XDF files (%s)\n", __func__, record->file->path);
        return NULL;
    }

    Lib_Log(APP_LIBFST, APP_ERROR, "%s: Unrecognized file type %d (%s)\n", __func__, record->file->type, record->file->path);
    return NULL;
}

//! Read the data and metadata of a given record from its corresponding file.
//!
//! Thread safety: This function may be called concurrently by several threads on *different records* that belong to
//! the same file. However, it cannot be called concurrently on the same fst_record object.
//!
//! \return TRUE (1) if reading was successful FALSE (0) or a negative number otherwise
int32_t fst24_read_record(
    //!> [in,out] Record for which we want to read data. Must have a valid handle!
    //!> If the `data` attribute of this record is NULL, space will be automatically allocated
    //!> and the data will be considered "managed" by the API.
    //!> If the `data` attribute is non-NULL and the memory is managed by the API, the size of
    //!> the allocation may be adjusted to fit this new record size (if needed).
    //!> If the `data` attribute is non-NULL and the memory is *not* managed by the API, the space
    //!> it points to must be large enough to contain all the data.
    fst_record* const record
) {
    if (record != NULL && !fst24_is_open(record->file)) {
       Lib_Log(APP_LIBFST, APP_ERROR, "%s: File not open (%s)\n",__func__, record->file ? record->file->path : "(nil)");
       return ERR_NO_FILE;
    }

    if (!fst24_record_is_valid(record) || record->do_not_touch.handle < 0) {
       Lib_Log(APP_LIBFST, APP_ERROR, "%s: Invalid record\n", __func__);
       return -1;
    }

    if (record->do_not_touch.flags & FST_REC_ASSIGNED) {
       Lib_Log(APP_LIBFST, APP_ERROR, "%s: Cannot reallocate data due to pointer ownership\n", __func__);
       return -1;
    }

    // Allocate buffer if not already done or big enough
    const int64_t size = fst24_record_data_size(record);
    if (size == 0) {
       Lib_Log(APP_LIBFST, APP_INFO, "%s: NULL size buffer \n", __func__);
       return -1;
    }

    if ((record->data == NULL) || (record->do_not_touch.alloc > 0 && size * 2 > record->do_not_touch.alloc)) {
        record->data = realloc(record->data, size * 2);
        if (!record->data) {
            return ERR_MEM_FULL;
        }
        record->do_not_touch.alloc = size * 2;
    }

    int32_t ret = -1;
    if (record->file->type == FST_RSF) {
        ret = fst24_read_record_rsf(record->file->rsf_handle, record, image_mode_copy, 0);
    }
    else if (record->file->type == FST_XDF) {
        ret = fst24_read_record_xdf(record);
    }
    else {
        Lib_Log(APP_LIBFST, APP_ERROR, "%s: Unrecognized file type (%s)\n", __func__, record->file->path);
        ret = -1;
    }

    if (ret < 0) {
        Lib_Log(APP_LIBFST, APP_ERROR, "%s: Could not read record, ier = %d\n", __func__, ret);
        if (record->do_not_touch.alloc>0) {
           free(record->data);
           record->data = NULL;
           record->do_not_touch.alloc = 0;
        }
        return ret;
    }

    return TRUE;
}

//! Read the next record (data and all) that corresponds to the given query criteria.
//! Search through linked files, if any.
//!
//! Thread safety: This function may be called concurrently by several threads on *different queries* that belong to
//! the same file (the records must be different). However, it cannot be called concurrently on the same
//! fst_query object.
//!
//! \return TRUE (1) if able to read a record, FALSE (0) or a negative number otherwise (not found or error)
int32_t fst24_read_next(
    fst_query* const query,   //!< Query used for the search
    fst_record* const record  //!< [out] Record content and info, if found
) {
    if (!fst24_find_next(query, record)) {
        return FALSE;
    }

    return fst24_read_record(record);
}

//! Search a file with given criteria and read the first record that matches these criteria.
//! Search through linked files, if any.
//!
//! Thread safety: This function may be called concurrently by several threads on the same file
//! (the records must be different).
//!
//! \return TRUE (1) if able to find and read a record, FALSE (0) or a negative number otherwise (not found or error)
int32_t fst24_read(
    const fst_file* const file,         //!< File we want to search
    const fst_record* criteria,         //!< [Optional] Criteria to be used for the search
    const fst_query_options* options,   //!< [Optional] Options to modify how the search will be performed
    fst_record* const record            //!< [out] Record content and info, if found
) {
    fst_query* q = fst24_new_query(file, criteria, options);
    int32_t status = fst24_read_next(q, record);
    fst24_query_free(q);
    return status;
}

//! Link the given list of files together, so that they are treated as one for the purpose
//! of searching and reading. Once linked, the user can use the first file in the list
//! as a replacement for all the given files.
//!
//! Thread safety: This function may be called concurrently on two sets of fst_file objects *if and only if* the
//! two sets do not overlap. It is OK if two different fst_file objects refer to the same file on disk (i.e. the file
//! has been opened more than once).
//!
//! *Note*: Some librmn functions and some tools may still make use of the `iun` from the fst98 interface. In order to
//! be backward-compatible with these functions and tools, we also perform a link of the files with that interface.
//! That old fstlnk itself is *not* thread-safe and may not work if there are several sets of linked files. This
//! means that if you concurrently create several lists of linked files, they might not work as intended if these
//! lists are accessed through the first file's iun.
//!
//! \return TRUE (1) if files were linked, FALSE (0) or a negative number otherwise
int32_t fst24_link(
    fst_file** files,           //!< List of handles to open files
    const int32_t num_files     //!< How many files are in the list
) {
    if (num_files <= 1) {
        Lib_Log(APP_LIBFST, APP_INFO, "%s: only passed %d files, nothing to link\n", __func__, num_files);
        return TRUE;
    }

    int iun_list[num_files];

    // Perform checks on all files before doing anything
    for (int i = 0; i < num_files; i++) {
        if (!fst24_is_open(files[i])) {
            Lib_Log(APP_LIBFST, APP_ERROR, "%s: File %d (%s) not open. We won't link anything.\n", __func__, i, files[i] ? files[i]->path : "(nil)");
            return FALSE;
        }

        if (files[i]->next != NULL) {
            Lib_Log(APP_LIBFST, APP_ERROR,
                    "%s: File %d (%s) is already linked to another one. We won't link anything (else).\n", __func__, i, files[i]->path);
            return FALSE;
        }

        iun_list[i] = files[i]->iun;
    }

    for (int i = 0; i < num_files - 1; i++) {
        files[i]->next = files[i + 1];
    }
    
    // Link with old interface too, for compatibility with old libraries
    if (c_fstlnk(iun_list, num_files) < 0) {
        Lib_Log(APP_LIBFST, APP_WARNING, "%s: Old interface linking failed. Old-style functions that require"
                " iun as input will not consider this list of files as linked.\n",
                __func__);
    }

    return TRUE;
}

//! Unlink the given file(s). The files are assumed to have been linked by
//! a previous call to fst24_link, so only the first one should be given as input.
//!
//! Thread safety: Assuming the rules for calling fst24_link have been followed, it is always safe
//! to call fst24_unlink concurrently on two separate lists of files.
//!
//! \return TRUE (1) if unlinking was successful, FALSE (0) or a negative number otherwise
int32_t fst24_unlink(fst_file* const file) {
    if (!fst24_is_open(file)) {
       Lib_Log(APP_LIBFST, APP_ERROR, "%s: File not open (%s)\n", __func__, file ? file->path : "(nil)");
       return FALSE;
    }

    while (file->next != NULL) {
        fst_file* current = file;
        fst_file* tmp = current->next;
        current->next = NULL;
        current = tmp;
    }

    // Unlink with old interface too. This is not completely equivalent, since the old interface cannot
    // have more than one linked list of files.
    c_fstunl();

    return TRUE;
}

//! Move to the end of the given sequential file
//!
//! Thread safety: This function may always be called concurrently.
//!
//! \return The result of \ref c_fsteof if the file was open, FALSE (0) otherwise
int32_t fst24_eof(const fst_file* const file) {
    if (!fst24_is_open(file)) {
       Lib_Log(APP_LIBFST, APP_ERROR, "%s: File not open (%s)\n", __func__, file ? file->path : "(nil)");
       return FALSE;
    }

    return c_fsteof(fst24_get_unit(file));
}

//! \return Whether the given query pointer is a valid query. A query's file must be
//! open for the query to be valid.
//! Calling this function on a query whose file has been closed results in undefined behavior.
int32_t fst24_query_is_valid(const fst_query* const q) {
    return (q != NULL && fst24_is_open(q->file) && q->num_criteria > 0);
}

//! Free memory used by the given query. Must be called on every query created by fst24_new_query.
//! This also releases queries that were created automatically to search through linked files.
void fst24_query_free(fst_query* const query) {
    if (query != NULL) {
        fst24_query_free(query->next);
        query->next = NULL;

        if (query->file != NULL) {
            query->file = NULL; // Make the query invalid, in case someone tries to use the pointer after this
            free(query);
        }
    }
}

//! To be called from fortran. Determine whether the given FST query options pointer matches the default
//! fst_query_options struct.
//! \return 0 if they match, -1 if not
int32_t fst24_validate_default_query_options(
    const fst_query_options* fortran_options, //!< Pointer to a default-initialized fst_query_options[_c] struct
    const size_t fortran_size         //!< Size of the fst_query_options_c struct in Fortran
) {
    if (fortran_options == NULL) {
        Lib_Log(APP_LIBFST, APP_ERROR, "%s: Called with NULL pointer\n", __func__);
        return -1;
    }

    if (sizeof(fst_query_options) != fortran_size) {
        Lib_Log(APP_LIBFST, APP_ERROR, "%s: Size C != size Fortran (%d != %d)\n",
                __func__, sizeof(fst_query_options), fortran_size);
        return -1;
    }

    if (memcmp(&default_query_options, fortran_options, sizeof(fst_query_options)) != 0) {
        Lib_Log(APP_LIBFST, APP_ERROR, "%s: Not the same!\n", __func__);
        for (int i = 0; i < sizeof(fst_query_options) / 4; i += 4) {
            const uint32_t* c = (const uint32_t*)&default_query_options;
            const uint32_t* f = (const uint32_t*)fortran_options;
            fprintf(stderr, "c 0x %.8x %.8x %.8x %.8x\n", c[i], c[i+1], c[i+2], c[i+3]);
            fprintf(stderr, "f 0x %.8x %.8x %.8x %.8x\n", f[i], f[i+1], f[i+2], f[i+3]);
        }
        return -1;
    }

    return 0;
}

//! Delete a record from its file on disk. This does not reduce file size, it only makes the record
//! unreadable and removes it from the directory.
//!
//! Thread safety: Multiple records can be deleted concurrently from the same file. A single record cannot be deleted
//! more than once.
//!
//! \return TRUE if we were able to delete the record, FALSE otherwise
int32_t fst24_delete(
    fst_record* const record //!< The record we want to delete
) {
    if (!fst24_record_is_valid(record) || record->do_not_touch.handle <= 0) {
        Lib_Log(APP_LIBFST, APP_ERROR, "%s: Record is not valid\n", __func__);
        return FALSE;
    }

    if (!fst24_is_open(record->file)) {
        Lib_Log(APP_LIBFST, APP_ERROR, "%s: File is not open (%s)\n", __func__, record->file ? record->file->path : "(nil)");
        return FALSE;
    }

    Lib_Log(APP_LIBFST, APP_DEBUG, "%s: Deleting record %d from file %s, type %s\n",
            __func__, record->do_not_touch.handle, record->file->path, fst_file_type_name[record->file->type]);

    if (record->file->type == FST_RSF) {
        if (RSF_Delete_record(record->file->rsf_handle, record->do_not_touch.handle) != 1) return FALSE;
    }
    else if (record->file->type == FST_XDF) {
        if (c_fsteff_xdf(record->do_not_touch.handle) != 0) return FALSE;
    }
    else {
        Lib_Log(APP_LIBFST, APP_ERROR, "%s: Unrecognized file type (%s)\n", __func__, record->file->path);
        return FALSE;
    }

    record->do_not_touch.deleted = 1;

    return TRUE;
}

//! Search a file and delete all its records that match the given criteria.
int32_t fst24_search_and_delete(
    fst_file* const file,            //!< The file we want to clean
    const fst_record* criteria,      //!< Delete records that match these criteria
    const fst_query_options* options //!< Additional options for selecting the records to delete
) {
    fst_query* q = fst24_new_query(file, criteria, options);
    fst_record r = default_fst_record;

    int nb=0;
    while (fst24_find_next(q, &r) > 0) {
        nb+= fst24_delete(&r);
    }

    fst24_query_free(q);

    return nb;
}

//! Print human-readable version of the given options, if they differ from their default value.
void print_non_default_options(const fst_query_options* const options) {
    char buffer[1024];
    char* ptr = buffer;

    if (options->ip1_all != default_query_options.ip1_all) ptr += snprintf(ptr, 30, "ip1all=%d ", options->ip1_all);
    if (options->ip2_all != default_query_options.ip2_all) ptr += snprintf(ptr, 30, "ip2all=%d ", options->ip2_all);
    if (options->ip3_all != default_query_options.ip3_all) ptr += snprintf(ptr, 30, "ip3all=%d ", options->ip3_all);
    if (ptr == buffer) sprintf(ptr, "[none]");

    Lib_Log(APP_LIBFST, APP_ALWAYS, "options: %s\n", buffer);
}
