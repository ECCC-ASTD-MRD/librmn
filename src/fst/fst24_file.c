#include <errno.h>
#include <float.h>
#include <math.h>
#include <stdlib.h>

#include <pthread.h>

#include <App.h>
#include <str.h>

#include "fst_internal.h"
#include "fst24_file_internal.h"
#include "fst24_record_internal.h"
#include "fst98_internal.h"
#include "fst24_backend.h"
#include "rmn/fnom.h"
#include "rmn/Meta.h"
#include "xdf98.h"
#include "rmn/swap_buffer.h"

const fst_query_options default_query_options = {
    .ip1_all = 0,
    .ip2_all = 0,
    .ip3_all = 0,
    .stamp_norun = 0,
    .skip_filter = 0,
    .skip_grid_descriptors = 0,
};

extern const char * const FST_TYPE_NAMES[];

//! Names of the file types, matching the backend names (ops->name)
static const char * fst_file_type_name[] = {
    [FST_NONE] = "NONE",
    [FST_XDF]  = "XDF",
    [FST_RSF]  = "RSF"
};

#define default_fst_file ((fst_file) {      \
    .iun                =  0,               \
    .file_index         = -1,               \
    .file_index_backend = -1,               \
    .rsf_handle.p       = NULL,             \
    .type               = FST_NONE,         \
    .ops                = NULL,             \
    .next               = NULL,             \
    .path               = NULL,             \
    .tag                = NULL,             \
    .read_timer         = NULL_TIMER,       \
    .write_timer        = NULL_TIMER,       \
    .find_timer         = NULL_TIMER,       \
    .num_bytes_read     = 0,                \
    .num_bytes_written  = 0,                \
    .num_records_found  = 0,                \
})

//! Verify that the file pointer is valid and the file is open. This is meant to verify that
//! the file struct has been initialized by a call to fst24_open; it should *not* be called
//! on a file that has been closed, since it will result in undefined behavior.
//! \return 1 if the pointer is valid and the file is open, 0 otherwise
int32_t fst24_is_open(const fst_file* const file) {
    return file != NULL &&
           file->ops != NULL &&
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

//! \return Whether the given file is of type RSF, or 0 if the input does not point to an open file
int32_t fst24_is_rsf(const fst_file* const file) {
    if (fst24_is_open(file)) return file->type == FST_RSF;
    return 0;
}

//! \return The name of the backend used by the given file (e.g. "RSF", "XDF"), or NULL if the input
//! does not point to an open file. Ignores any potential linked files.
const char* fst24_backend_name(const fst_file* const file) {
    if (fst24_is_open(file) && file->ops != NULL) return file->ops->name;
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

    char local_options[1024];
    snprintf(local_options, sizeof(local_options), "RND+%s%s",
             (!options || !(strcasestr(options, "R/W") || strcasestr(options, "R/O"))) ? "R/O+" : "",
             options ? options : "");
    Lib_Log(APP_LIBFST, APP_DEBUG, "%s: filePath = %s, options = %s\n", __func__, filePath, local_options);

    if (c_fnom(&(the_file->iun), filePath, local_options, 0) != 0) {
        free(the_file);
        return NULL;
    }
    if (c_fstouv_fst24(the_file->iun, local_options) < 0) {
        c_fclos(the_file->iun);
        free(the_file);
        return NULL;
    }

    App_TimerStart(&the_file->open_timer);

    // Find type of newly-opened file (RSF or XDF)
    int index_fnom;
    const int rsf_status = is_rsf(the_file->iun, &index_fnom);
    the_file->file_index = index_fnom;
    the_file->path = FGFDT[index_fnom].file_name;
    if (rsf_status == 1) {
        the_file->type = FST_RSF;
        the_file->ops = &fst24_rsf_ops;
        the_file->rsf_handle = FGFDT[the_file->file_index].rsf_fh;
        the_file->file_index_backend = RSF_Get_file_slot(the_file->rsf_handle);
    }
    else {
        the_file->type = FST_XDF;
        the_file->ops = &fst24_xdf_ops;
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
//! \return TRUE (1) if no error, FALSE (0) or a negative number otherwise
//! \todo What happens if closing a linked file?
int32_t fst24_close(fst_file* const file) {
    if (!fst24_is_open(file)) {
        Lib_Log(APP_LIBFST, APP_DEBUG, "%s: Not an open file\n", __func__);
        return ERR_NO_FILE;
    }

    {
        const float read_time = App_TimerTotalTime_ms(&file->read_timer) / 1000.0f;
        const float write_time = App_TimerTotalTime_ms(&file->write_timer) / 1000.0f;
        const float find_time = App_TimerTotalTime_ms(&file->find_timer);
        const float open_time = App_TimerTimeSinceStart_ms(&file->open_timer);
        const float num_read_mb = file->num_bytes_read / (1024.0f * 1024.0f);
        const float num_written_mb = file->num_bytes_written / (1024.0f * 1024.0f);
        Lib_Log(APP_LIBFST, APP_STAT,
            "%s: Closing file %s\n"
            "\tRead  %.2f MB in %.3f seconds\n"
            "\tWrote %.2f MB in %.3f seconds\n"
            "\tFound %d records in %.2f ms\n"
            "\tFile was open for %.3f seconds\n",
            __func__, file->path, num_read_mb, read_time, num_written_mb, write_time,
            file->num_records_found, find_time, open_time / 1000.0f);
    }

    int status;
    status = c_fstfrm(file->iun);   // Close the actual file
    if (status < 0) return status;

    status = c_fclos(file->iun);    // Reset file entry in global table
    if (status < 0) return status;

    *file = default_fst_file;

    free(file);

    return TRUE;
}

//! Open a list of files and link them together
//! \return The first file in the linked list
fst_file* fst24_open_link(
   const char** const filePaths,  //!< List of file path to open
   const int32_t      fileNb      //!< Number of files in list
) {
    fst_file** files = (fst_file**)calloc(fileNb, sizeof(fst_file*));

    int n = 0;
    int nerr = 0;
    for(n = 0; n < fileNb; n++) {
        files[n - nerr] = fst24_open(filePaths[n], "RND+R/O");
        if (files[n - nerr] == NULL) {
            Lib_Log(APP_LIBFST,APP_ERROR, "%s: Unable to open file (%s)\n", __func__, filePaths[n]);
            nerr++;
        }
    }

    n = fst24_link(files, fileNb - nerr);
    fst_file* first = files[0];
    free(files);
    return(first);
}

//! Close a list of files that were opened with fst24_open_link
//! \return TRUE (1) if we were able to close all of them, FALSE (0) otherwise
int32_t fst24_close_unlink(
   fst_file* const file   //!< first file of link
) {
    fst_file* current,*tmp;

    if (!fst24_is_open(file)) {
        Lib_Log(APP_LIBFST, APP_ERROR, "%s: File not open\n", __func__);
        return FALSE;
    }

    current = file; 

    int32_t all_good = TRUE;
    while (current != NULL) {
        tmp = current;
        current = current->next;
        tmp->next = NULL; 
        all_good &= fst24_close(tmp);
    }

    return all_good;
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

    if (file->ops == NULL || file->ops->flush == NULL) {
        Lib_Log(APP_LIBFST, APP_ERROR, "%s: flush not available for file type %s (%s)\n",
            __func__, fst_file_type_name[file->type], file->path);
        return -1;
    }
    return file->ops->flush(file);
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

    if (file->ops == NULL || file->ops->get_num_records == NULL) {
        Lib_Log(APP_LIBFST, APP_ERROR, "%s: get_num_records not available for file type %s (%s)\n",
             __func__, fst_file_type_name[file->type], file->path);
        return 0;
    }
    total_num_records = file->ops->get_num_records(file);
    if (total_num_records < 0) return 0; // Stop recursion here if error

    if (file->next != NULL) total_num_records += fst24_get_num_records(file->next);

    return total_num_records;
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

    if (query == NULL) {
        Lib_Log(APP_LIBFST, APP_ERROR, "%s: Unable to create a query to look through the file! (%s)\n", __func__,
                file->path);
        return -1;
    }

    fst_record rec = default_fst_record;
    int64_t num_records = 0;
    size_t total_data_size = 0;
    char prefix[16];

    uint8_t num_digits = 0;
    for (int64_t tmp_num_records = fst24_get_num_records(file); tmp_num_records > 0; tmp_num_records /= 10) num_digits++;

    while (fst24_find_next(query, &rec) == TRUE) {
        snprintf(prefix, num_digits + 2, "%*ld-", num_digits, num_records);
        fst24_record_print_short(&rec, fields, ((num_records % 70) == 0), prefix);
        num_records++;
        total_data_size += fst24_record_data_size(&rec);
    }

    fst24_record_free(&rec);
    fst24_query_free(query);

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
        case FST_TYPE_BINARY:
        case FST_TYPE_BINARY | FST_TYPE_TURBOPACK:
            // binary
            *Min = 0;
            *Max = 1;
            break;

        case FST_TYPE_REAL_OLD_QUANT:
        case FST_TYPE_REAL_IEEE:
        case FST_TYPE_REAL: {
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

        case FST_TYPE_UNSIGNED: {
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

        case FST_TYPE_CHAR: {
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

        case FST_TYPE_SIGNED:
        case FST_TYPE_SIGNED | FST_TYPE_TURBOPACK: {
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

        case FST_TYPE_STRING:
        case FST_TYPE_STRING | FST_TYPE_TURBOPACK:
            break;
    }
}

search_metadata* make_search_metadata(
    const fst_record* record,
    search_metadata* const dest
) {
    search_metadata* meta = dest;
    stdf_dir_keys* stdf_entry = &meta->fst98_meta;
    if (meta == NULL) { meta = malloc(sizeof(search_metadata)); }

    char typvar[FST_TYPVAR_LEN];
    copy_record_string(typvar, record->typvar, FST_TYPVAR_LEN);
    char nomvar[FST_NOMVAR_LEN];
    copy_record_string(nomvar, record->nomvar, FST_NOMVAR_LEN);
    char etiket[FST_ETIKET_LEN];
    copy_record_string(etiket, record->etiket, FST_ETIKET_LEN);
    char grtyp[FST_GTYP_LEN];
    copy_record_string(grtyp, record->grtyp, FST_GTYP_LEN);


    // RSF reserved metadata
    memset(&meta->rsf_reserved, 0, sizeof(RSF_Reserved));
    
    // FST reserved metadata
    meta->fst24_reserved = make_fst24_reserved(record->do_not_touch.extended_meta_size);

    // fst98 metadata 
    {
        (void)stdf_entry->deleted; // Reserved by RSF/FST. Don't write anything here!
        (void)stdf_entry->select;  // Reserved by RSF/FST. Don't write anything here!
        (void)stdf_entry->lng;     // Reserved by RSF/FST. Don't write anything here!
        (void)stdf_entry->addr;    // Reserved by RSF/FST. Don't write anything here!

        stdf_entry->deet = record->deet;
        stdf_entry->nbits = record->pack_bits;
        stdf_entry->ni_a = record->ni & 0xffffff;
        stdf_entry->ni_b = record->ni >> 24;
        stdf_entry->gtyp = grtyp[0];
        stdf_entry->nj_a = record->nj & 0xffffff;
        stdf_entry->nj_b = (record->nj & 0x0f000000) >> 24;
        stdf_entry->nj_c = record->nj >> 28;
        // propagate missing values flag
        stdf_entry->datyp = record->data_type;
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
        stdf_entry->nomvar =
            (ascii6(nomvar[0]) << 18) |
            (ascii6(nomvar[1]) << 12) |
            (ascii6(nomvar[2]) <<  6) |
            (ascii6(nomvar[3]));
        stdf_entry->ip1 = record->ip1;
        stdf_entry->levtyp = 0;
        stdf_entry->ip2 = record->ip2;
        stdf_entry->ip3 = record->ip3;
        stdf_entry->date_stamp = stamp_from_date(record->datev);
        stdf_entry->dasiz = record->data_bits;
    }

    return meta;
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
    //!> - FST_YES:  overwrite existing record data
    //!> - FST_SKIP: if record already exists, don't write anything
    //!> - FST_NO:   append record to file
    //!> - FST_META: Overwrite metadata of existing record. The record must come from the given file.
    const int rewrite
) {
    fst_record crit = default_fst_record;

    if (!fst24_is_open(file)) return ERR_NO_FILE;
    if (!fst24_record_is_valid(record)) return ERR_BAD_INIT;

    Lib_Log(APP_LIBFST, APP_DEBUG, "%s: file %s, type %s, rewrite %d\n",
            __func__, file->path, fst_file_type_name[file->type], rewrite);

    if (rewrite == FST_META) {
        if (record->do_not_touch.handle < 0 || file != record->file) {
            Lib_Log(APP_LIBFST, APP_ERROR,
                    "%s: Trying to rewrite metadata, but record does not seem to have been read from this file\n",
                    __func__);
            return -1;
        }

        int return_value = -1;
        App_TimerStart(&file->write_timer);
        if (file->ops == NULL || file->ops->rewrite_meta == NULL) {
            Lib_Log(APP_LIBFST, APP_ERROR, "%s: rewrite_meta not available for file type %s (%s)\n",
                __func__, fst_file_type_name[file->type], file->path);
        }
        else {
            return_value = file->ops->rewrite_meta(file, record);
        }
        App_TimerStop(&file->write_timer);
        return return_value;
    }

    App_TimerStart(&file->write_timer);

    record->file = file;

    // Check and set origin date, if appropriate
    if (record->deet != default_fst_record.deet && record->npas != default_fst_record.npas) {
        if (record->datev != default_fst_record.datev) {
            const int32_t dateo = get_origin_date32(record->datev, record->deet, record->npas);
            if (record->dateo == default_fst_record.dateo) {
                Lib_Log(APP_LIBFST, APP_DEBUG,
                    "%s: No origin date specified. Setting it to %d (from datev = %d, deet = %d, npas = %d)\n",
                    __func__, dateo, record->datev, record->deet, record->npas);
                record->dateo = dateo;
            }
            else if (record->dateo != dateo) {
                Lib_Log(APP_LIBFST, APP_WARNING,
                    "%s: Origin and validity dates are not consistent; validity date will be overwritten."
                    " dateo = %d, datev = %d, deet = %d, npas = %d\n",
                    __func__, record->dateo, record->datev, record->deet, record->npas);
            }
        }
    }

    if (record->pack_bits > record->data_bits) {
        Lib_Log(APP_LIBFST, APP_WARNING,
            "%s: Trying to pack a record into more bits (%d) than its original data (%d). "
            "Setting pack_bits to %d (uncompressed)\n",
            __func__, record->pack_bits, record->data_bits, record->data_bits);
        record->pack_bits = record->data_bits;
    }

    // Use the skip_filter option, to *not* miss the record because of the global filter
    fst_query_options rewrite_options = default_query_options;
    rewrite_options.skip_filter = 1;

    if (rewrite == FST_YES || rewrite == FST_SKIP) { 
        fst24_record_copy_metadata(&crit,record,FST_META_GRID|FST_META_INFO|FST_META_TIME|FST_META_SIZE);
        crit.datev = get_valid_date32(crit.dateo,crit.deet,crit.npas);
        crit.dateo=-1;
    }

    // If the record already exists in the file, we skip writing altogether (FST_SKIP), or we delete
    // it before writing (FST_YES)
    if (rewrite == FST_SKIP || rewrite == FST_YES) {
        fst_query* q = fst24_new_query(file, &crit, &rewrite_options);
        fst_record to_delete = default_fst_record;
        const int32_t found = fst24_find_next(q,&to_delete);
        fst24_query_free(q);
        if (found == TRUE) {
            if (rewrite == FST_SKIP) {
                Lib_Log(APP_LIBFST, APP_INFO, "%s: Skipping (record already exists)\n", __func__);
                App_TimerStop(&file->write_timer);
                return TRUE;
            }

            if (fst24_delete(&to_delete) != TRUE) {
                Lib_Log(APP_LIBFST, APP_ERROR, "%s: Unable to delete existing record\n", __func__);
                App_TimerStop(&file->write_timer);
                return -1;
            }

            Lib_Log(APP_LIBFST, APP_INFO, "%s: Rewriting existing record\n", __func__);
        }
    } 

    // No skip, so we write
    int32_t return_value = -1;
    if (file->ops == NULL || file->ops->write == NULL) {
        Lib_Log(APP_LIBFST, APP_ERROR, "%s: write not available for file type %s (%s)\n",
            __func__, fst_file_type_name[file->type], file->path);
    }
    else {
        return_value = file->ops->write(file, record);
    }

    App_TimerStop(&file->write_timer);
    if (return_value == TRUE) file->num_bytes_written += fst24_record_data_size(record);
    return return_value;
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

    if (file->ops == NULL || file->ops->get_record_from_key == NULL) {
        Lib_Log(APP_LIBFST, APP_ERROR, "%s: get_record_from_key not available for file type %s (%s)\n",
            __func__, fst_file_type_name[file->type], file->path);
        return FALSE;
    }
    if (file->ops->get_record_from_key(file, key, record) != TRUE) {
        Lib_Log(APP_LIBFST, APP_ERROR, "%s: Unable to get record with key %lx\n", __func__, key);
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

    record->file = file;

    if (file->ops == NULL || file->ops->get_record_by_index == NULL) {
        Lib_Log(APP_LIBFST, APP_ERROR, "%s: get_record_by_index not available for file type %s (%s)\n",
            __func__, fst_file_type_name[file->type], file->path);
        return FALSE;
    }
    return file->ops->get_record_by_index(file, index, record);
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

    const fst_record default_criteria = default_fst_record;
    if (criteria == NULL) {
       criteria = &default_criteria;
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
        fst24_record_print_non_default(criteria);
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

    // Check on excdes desire/exclure clauses (applies to all non-XDF backends)
    if (query->file->type != FST_XDF &&
        !query->options.skip_filter &&
        !C_fst_rsf_match_req(record->datev, record->ni, record->nj, record->nk, record->ip1, record->ip2, record->ip3,
        record->typvar, record->nomvar, record->etiket, record->grtyp, record->ig1, record->ig2, record->ig3, record->ig4)) {
        return FALSE;
    }

    if (query->options.skip_grid_descriptors > 0) {
        const char** descriptor_names = fst24_record_get_descriptors();
        for (int i = 0; descriptor_names[i] != NULL; i++) {
            if (is_same_record_string(record->nomvar, descriptor_names[i], FST_NOMVAR_LEN - 1)) return FALSE;
        }
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
    //!> [in,out] Will contain record information if found and, optionally, metadata (if included in search).
    //!> If NULL, we will only check for the existence of a match to the query, without extracting any data from that
    //!> match. If not NULL, must be a valid, initialized record.
    fst_record * const record
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

    App_TimerStart((TApp_Timer*)&query->file->find_timer); // Casting because it's a pointer to const fst_file
    fst_record tmp_record = default_fst_record;
    int found = FALSE;
    while (!found) {
        if (query->file->ops == NULL || query->file->ops->find_next == NULL) {
            Lib_Log(APP_LIBFST, APP_ERROR, "%s: find_next not available for file type %s (%s)\n", __func__,
                    fst_file_type_name[query->file->type], query->file->path);
            return -1;
        }
        const int64_t key = query->file->ops->find_next(query->file, query);

        if (key < 0) break; // Not in this file

        if (fst24_get_record_from_key(query->file, key, &tmp_record) != TRUE) {
            Lib_Log(APP_LIBFST, APP_ERROR, "%s: Unable to retrieve record info after having found it in %s.\n",
                    __func__, query->file->path);
            found = -1;
            break;
        }

        if (!is_actual_match(&tmp_record, query)) continue; // Keep looking

        found = TRUE;
        (*(int32_t*)&query->file->num_records_found)++; // Need to cast because it's a pointer to a const fst_file object

        Lib_Log(APP_LIBFST, APP_DEBUG, "%s: (unit=%d) Found record at key 0x%x in file %s\n",
                __func__, query->file->iun, key, query->file->path);
        if (Lib_LogLevel(APP_LIBFST, NULL) >= APP_EXTRA) fst24_record_print(&tmp_record);

        if (record != NULL) fst_record_copy_info(record, &tmp_record);
        query->search_index = key;
    }

    fst24_record_free(&tmp_record);
    App_TimerStop((TApp_Timer*)&query->file->find_timer); // Casting because it's a pointer to const fst_file

    if (found != FALSE) {
        return found;
    }

    // We haven't found anything in this file
    query->search_done = 1;

    if (query->file->next != NULL) {
        // We're done searching this file, but there's another one in the linked list, so 
        // we need to setup the search in that one
        ensure_next_query(query);
        return fst24_find_next(query->next, record);
    }

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
    //!> [out] (Optional) List of records found. The list must be already allocated, but the records are considered uninitialized.
    //!> This means they will be overwritten and if they contained any memory allocation, it will be lost.
    //!> If NULL, it will just be ignored.
    fst_record* results,
    const int32_t max_num_results //!< [in] Size of the given list of records. We will stop looking if we find that many
) {
    if (fst24_rewind_search(query) != TRUE) return 0;

    const int32_t max = results ? max_num_results : INT_MAX;

    for (int i = 0; i < max; i++) {
        if (results != NULL) {
            results[i] = default_fst_record;
            if (fst24_find_next(query, &(results[i])) != TRUE) return i;
        } else {
            if (fst24_find_next(query, NULL) != TRUE) return i;
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
    // fst_record record = default_fst_record;

    fst24_rewind_search(query);

    int32_t count = 0;
    while (fst24_find_next(query, NULL) == TRUE) {
        count++;
    }

    // fst24_record_free(&record);
    return count;
}

//! Find the first record the corresponds to the given criteria.
//!
//! Thread safety: This function may be called by multiple threads on the same file.
//! 
//! \return TRUE (1) if a record was found, FALSE (0) or a negative number otherwise (not found, file not open, etc.)
int32_t fst24_find_one(
    const fst_file* const file,
    const fst_record* criteria,
    const fst_query_options* options,
    fst_record* record
) {
    fst_query* q = fst24_new_query(file, criteria, options);
    if (q == NULL) {
        Lib_Log(APP_LIBFST, APP_ERROR, "%s: Unable to create query\n", __func__);
        return 0;
    }

    const int32_t status = fst24_find_next(q, record);
    fst24_query_free(q);
    return status;
}



//! Unpack the given data array, according to the given record information.
//! \return 0 on success, negative if error.
int32_t fst24_unpack_data(
    void* dest,
    void* source, //!< Should be const, but we might swap stuff in-place. It's supposed to be temporary anyway...
    const fst_record* record, //!< [in] Record information, must have a correct `data_bits` value (8, 16, 32, 64)
    const int32_t skip_unpack,//!< Only copy data (no uncompression) if non-zero
    const int32_t stride,     //!< Kept for compatibility with fst98 interface
    const int32_t original_num_bits //!< Size of data elements of the array from which we are reading
) {
    uint32_t* dest_u32 = dest;
    uint32_t* source_u32 = source;

    // Get missing data flag
    const int has_missing = has_type_missing(record->data_type);
    // Suppress missing data flag
    const int32_t simple_data_type = record->data_type & ~FSTD_MISSING_FLAG;

    // Unpack function son output element size
    UnpackFunctionPointer unpackfunc = original_num_bits == 64 ? &compact_u_double : &compact_u_float;

    // const size_t record_size_32 = record->rsz / 4;
    // size_t record_size = record_size_32;
    // if ((simple_data_type == FST_TYPE_OLD_QUANT) || (simple_data_type == FST_TYPE_REAL_IEEE)) {
    //     record_size = (original_num_bits == 64) ? 2*record_size : record_size;
    // }

    const int multiplier = (simple_data_type == FST_TYPE_COMPLEX) ? 2 : 1;
    const int nelm = fst24_record_num_elem(record) * multiplier;

    Lib_Log(APP_LIBFST, APP_EXTRA, "%s: Unpacking %d %d-bit %s elements from %p into %p\n",
            __func__, nelm, original_num_bits, FST_TYPE_NAMES[base_fst_type(record->data_type)], source, dest);

    double dmin = 0.0;
    double dmax = 0.0;

    const int bitmot = 32;
    int compact_ier = 0;
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
                    unpackfunc(dest_u32, source_u32 + 1, source_u32 + 5, nelm, record->pack_bits + 64 * Max(16, record->pack_bits),
                             0, stride, 0, &tempfloat, &dmin, &dmax);
                } else {
                    unpackfunc(dest_u32, source_u32, source_u32 + 3, nelm, record->pack_bits, 24, stride, 0, &tempfloat, &dmin, &dmax);
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
                        compact_ier = compact_u_short(dest, (void *) NULL, (void *)(source_u32 + offset), nelm, record->pack_bits, 0, stride);
                    }
                }  else if (record->data_bits == 8) {
                    if (is_type_turbopack(record->data_type)) {
                        armn_compress((unsigned char *)(source_u32 + offset), record->ni, record->nj, record->nk, record->pack_bits, 2, 0);
                        memcpy_16_8((int8_t *)dest, (int16_t *)(source_u32 + offset), nelm);
                    } else {
                        compact_ier = compact_u_char(dest, (void *)NULL, (void *)source, nelm, record->pack_bits, 0, stride);
                    }
                } else if (record->data_bits == 64 && record->pack_bits == 64) {
                    memcpy(dest, source, nelm * sizeof(uint64_t));
                } else {
                    if (is_type_turbopack(record->data_type)) {
                        armn_compress((unsigned char *)(source_u32 + offset), record->ni, record->nj, record->nk, record->pack_bits, 2, 0);
                        memcpy_16_32((int32_t *)dest, (int16_t *)(source_u32 + offset), record->pack_bits, nelm);
                    } else {
                        compact_ier = compact_u_integer(dest, (void *)NULL, source_u32 + offset, nelm, record->pack_bits, 0, stride, 0);
                    }

                    if (record->data_bits == 64) {
                        int32_t x[nelm];
                        memcpy(x, dest, nelm * sizeof(int32_t));
                        resize_int(dest, 64, x, 32, nelm);
                    }
                }

                break;
            }

            case FST_TYPE_CHAR: {
                // Character
                const int num_ints = (nelm + 3) / 4;
                compact_ier = compact_u_integer(dest, (void *)NULL, source, num_ints, 32, 0, stride, 0);
                break;
            }

            case FST_TYPE_SIGNED: {
                if (original_num_bits == 64) {
                    memcpy(dest, source, nelm * sizeof(int64_t));
                } else {
                    const int use32 = (record->data_bits == 32);
                    int32_t* field_out = dest;
                    if (!use32) {
                        field_out = (int32_t*)malloc(nelm * sizeof(int32_t));
                        if (field_out == NULL) {
                            Lib_Log(APP_LIBFST, APP_ERROR, "%s: Allocation failed\n", __func__);
                            return ERR_MEM_FULL;
                        }
                    }
                    compact_ier = compact_u_integer(field_out, (void *) NULL, source, nelm, record->pack_bits, 0, stride, 1);
                    if (record->data_bits == 16) {
                        int16_t* field_out_16 = (int16_t*)dest;
                        for (int i = 0; i < nelm; i++) {
                            field_out_16[i] = field_out[i];
                        }
                    } else if (record->data_bits == 8) {
                        int8_t* field_out_8 = (int8_t*)dest;
                        for (int i = 0; i < nelm; i++) {
                            field_out_8[i] = field_out[i];
                        }
                    } else if (record->data_bits == 64) {
                        int64_t* field_out_64 = (int64_t*)dest;
                        for (int i = 0; i < nelm; i++) {
                            field_out_64[i] = field_out[i];
                        }
                    }
                    if (field_out != dest) free(field_out);
                }
                break;
            }

            case FST_TYPE_REAL_IEEE:
            case FST_TYPE_COMPLEX: {

                // IEEE representation
                if ((downgrade_32) && (original_num_bits == 64)) {
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
                compact_ier = compact_u_char(dest, (void *)NULL, source, nelm, 8, 0, stride);
                break;

            default:
                Lib_Log(APP_LIBFST, APP_ERROR, "%s: invalid data_type=%d\n", __func__, simple_data_type);
                return(ERR_BAD_DATYP);
        } // end switch
    }

    // Upgrade from float to double, if needed
    if (original_num_bits == 32 && record->data_bits == 64 && is_type_real(record->data_type) && !skip_unpack) {
        const int64_t num_elem = fst24_record_num_elem(record);
        float f[num_elem];
        memcpy(f, dest, num_elem * sizeof(float));
        upgrade_size(dest, record->data_bits, f, original_num_bits, num_elem, 0);
    }

    Lib_Log(APP_LIBFST, APP_DEBUG, "%s: Unpacked record with key 0x%llx\n", __func__, record->do_not_touch.handle);
    if (Lib_LogLevel(APP_LIBFST, NULL) >= APP_EXTRA) fst24_record_print_short(record, NULL, 1, NULL);

    if (compact_ier < 0) {
        Lib_Log(APP_LIBFST, APP_ERROR, "%s: Problem while un-compacting the data\n", __func__);
        return compact_ier;
    }

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



//! Read only data map + metadata for the given record
//!
//! Thread safety: This function may be called concurrently by several threads on *different records* that belong to
//! the same file. However, it cannot be called concurrently on the same fst_record object.
//!
//! \return A pointer to the data map, NULL if error (or there is no data map)
void* fst24_read_data_map(
    fst_record* record //!< [in,out] Record for which we want to read the data map. Must have a valid handle!
) {
    if (!fst24_record_is_valid(record) || record->do_not_touch.handle < 0) {
       Lib_Log(APP_LIBFST, APP_ERROR, "%s: Invalid record\n", __func__);
       return NULL;
    }

    if (record->data_blocks.map != NULL) return record->data_blocks.map;

    if (!fst24_is_open(record->file)) {
       Lib_Log(APP_LIBFST, APP_ERROR, "%s: File not open\n",__func__);
       return NULL;
    }

    if (record->file->ops == NULL || record->file->ops->read_data_map == NULL) {
        Lib_Log(APP_LIBFST, APP_ERROR, "%s: read_data_map not available for file type %s (%s)\n",
            __func__, fst_file_type_name[record->file->type], record->file->path);
        return NULL;
    }
    return record->file->ops->read_data_map(record);
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

    if (record->metadata != NULL) return record->metadata;

    if (!fst24_is_open(record->file)) {
       Lib_Log(APP_LIBFST, APP_ERROR, "%s: File not open\n",__func__);
       return NULL;
    }

    if (record->file->ops == NULL || record->file->ops->read_metadata == NULL) {
        Lib_Log(APP_LIBFST, APP_ERROR, "%s: read_metadata not available for file type %s (%s)\n",
            __func__, fst_file_type_name[record->file->type], record->file->path);
        return NULL;
    }
    return record->file->ops->read_metadata(record);
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
    //!> (in) The `data_bits` attribute may have a value larger than that stored in the file (either 16, 32 or 64). If
    //!> that is the case, the type pointed to by `data` will be considered as having that larger size. This is only
    //!> allowed for signed integers, unsigned integers and real types.
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

    App_TimerStart((TApp_Timer*)&record->file->read_timer); // Cast because it's a pointer to a const fst_file object

    int32_t ret = -1;
    if (record->file->ops == NULL || record->file->ops->read_record == NULL) {
        Lib_Log(APP_LIBFST, APP_ERROR, "%s: read_record not available for file type %s (%s)\n",
            __func__, fst_file_type_name[record->file->type], record->file->path);
    }
    else {
        ret = record->file->ops->read_record(record);
    }

    App_TimerStop((TApp_Timer*)&record->file->read_timer); // Cast because it's a pointer to a const fst_file object

    if (ret < 0) {
        Lib_Log(APP_LIBFST, APP_ERROR, "%s: Could not read record, ier = %d\n", __func__, ret);
        if (record->do_not_touch.alloc>0) {
           free(record->data);
           record->data = NULL;
           record->do_not_touch.alloc = 0;
        }
        return ret;
    }

    // Update num bytes read. Cheating a bit because it's a pointer to a const fst_file object
    *(int64_t*)&record->file->num_bytes_read += size;

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
    if (fst24_find_next(query, record) != TRUE) {
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
            // Make the query invalid, in case someone tries to use the pointer after this. This would be undefined
            // behavior in any case...
            query->file = NULL;
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
        for (unsigned int i = 0; i < sizeof(fst_query_options) / 4; i += 4) {
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

    if (record->file->ops == NULL || record->file->ops->delete_record == NULL) {
        Lib_Log(APP_LIBFST, APP_ERROR, "%s: delete_record not available for file type %s (%s)\n",
            __func__, fst_file_type_name[record->file->type], record->file->path);
        return FALSE;
    }
    if (record->file->ops->delete_record(record) != TRUE) return FALSE;

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

//! Force close a file that might have been left open in write mode by a crash.
//! *Be careful when using this function, it will reset the read-write flag of the file on disk, even if
//! another process currently has the file open for writing.*
//!
//! Thread safety: It is "safe" to call this function several times on the same file at the same time because it
//! does not take an `fst_file` pointer as input, *but you shouldn't do it*.
//!
//! \return TRUE (1) on success, FALSE (0) or a negative number on failure
int32_t fst24_force_close(
    const char* filename //!< Name of the file whose flag needs resetting. Must be a valid RPN Standard file
) {

    fst_file* file = fst24_open(filename, "R/O"); // Must be able to open it read-only

    if (file == NULL) {
        Lib_Log(APP_LIBFST, APP_ERROR, "%s: File must be openable in read-only mode (%d)\n", __func__, filename);
        return -1;
    }

    int32_t status = -1;
    if (file->ops == NULL || file->ops->force_close == NULL) {
        Lib_Log(APP_LIBFST, APP_ERROR, "%s: force_close not available for file type %s (%s)\n",
            __func__, fst_file_type_name[file->type], file->path);
    }
    else {
        status = file->ops->force_close(filename);
    }

    if (status == TRUE) {
        fst24_close(file);
    }

    return status;
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

//! Read a segment of a file, without reading anything else from that file.
//! For best results, this function should only be called with the offset and size given by an `fst_record` from that
//! same, previously-opened file.
//! *There could have been some changes to the file between the time the offset/size were determined and the time it
//! is read by this function. For example, in an XDF file, the record could have been overwritten. In an RSF file, it
//! could have been marked as deleted. Neither change will prevent reading the record by this function.*
int32_t fst24_read_raw_record(
    const char* const filename, //!< [in] Name of the file where the record is stored
    const size_t offset,        //!< [in] Offset of the record in the file
    const size_t num_bytes,     //!< [in] Number of bytes to read (this must correspond to the size of the record)
    void* const dest            //!< [in,out] Pointer to an already-allocated space where to put the data
) {
    // Open the file
    const int fd = open(filename, O_RDONLY);
    if (fd < 0) {
        Lib_Log(APP_LIBFST, APP_ERROR, "%s: Unable to open file %s: %s\n", __func__, filename, strerror(errno));
        return -1;
    }

    // Read + close the file
    lseek(fd, offset, SEEK_SET);
    const ssize_t num_read = read(fd, dest, num_bytes);
    close(fd);

    // Check if succeeded
    if (num_read == num_bytes) return TRUE;

    // Did not read full record
    if (num_read >= 0) {
        Lib_Log(APP_LIBFST, APP_ERROR, "%s: Did not read full record from %s -> only %lld bytes\n",
                __func__, filename, num_read);
        }
    else {
        Lib_Log(APP_LIBFST, APP_ERROR, "%s: Error while reading record from %s: %s\n",
                __func__, filename, strerror(errno));
    }
    return -1;
}
