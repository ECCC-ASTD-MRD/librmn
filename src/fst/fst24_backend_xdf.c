//! \file fst24_backend_xdf.c
//! XDF-specific implementation of the fst24 file API.
//!
//! This file contains the code that is specific to the XDF backend. The common
//! fst24 API (in fst24_file.c) dispatches to these functions when the file is
//! of type XDF. The low-level XDF primitives live in xdf98.c.

#include <pthread.h>
#include <stdlib.h>
#include <string.h>

#include <App.h>

#include "fst_internal.h"
#include "fst24_file_internal.h"
#include "fst24_record_internal.h"
#include "fst98_internal.h"
#include "xdf98.h"
#include "fst24_backend_xdf.h"
#include "rmn/swap_buffer.h"

//! Mutex protecting the (not thread-safe) XDF primitives that are shared across
//! all open XDF files. Exposed (non-static) so that the common fst24 API in
//! fst24_file.c can also take it (e.g. when rewriting an XDF record's metadata).
pthread_mutex_t fst24_xdf_mutex = PTHREAD_MUTEX_INITIALIZER;

int32_t fst24_make_index_from_xdf_handle(const int handle) {
    return RECORD_FROM_HANDLE((handle & 0xffffffff)) + (PAGENO_FROM_HANDLE((handle & 0xffffffff)) * ENTRIES_PER_PAGE);
}

int32_t fst24_make_xdf_handle_from_index(const int index, const int file_id) {
    return MAKE_RND_HANDLE(index / ENTRIES_PER_PAGE, index % ENTRIES_PER_PAGE, file_id);
}

//! Fill fst_record attributes from metadata found in XDF file directory
int32_t update_attributes_from_xdf_handle(
    fst_record* record,     //!< [in,out] Record struct where to put the information
    const int xdf_handle    //!< key to find the record
) {
    // Retrieve record info
    int addr, lng, idtyp;
    search_metadata record_meta;
    stdf_dir_keys* record_meta_xdf = &record_meta.fst98_meta;
    uint32_t* pkeys = (uint32_t *) record_meta_xdf;
    pkeys += W64TOWD(1);
    const int num_keys = 16;
    if (c_xdfprm(xdf_handle, &addr, &lng, &idtyp, pkeys, num_keys) < 0) {
        Lib_Log(APP_LIBFST, APP_ERROR, "%s: Unable to get record with key %x\n", __func__, xdf_handle);
        return FALSE;
    }

    // Check whether record is deleted
    if ((idtyp | 0x80) == 255 || (idtyp | 0x80) == 254) {
        record->do_not_touch.deleted = 1;
        return FALSE;
    }

    // Put info in fst_record struct
    fill_with_search_meta(record, &record_meta, FST_XDF);
    record->do_not_touch.num_search_keys = num_keys;
    record->do_not_touch.stored_data_size = W64TOWD(lng) - num_keys;
    record->do_not_touch.handle = xdf_handle;
    record->file_index = fst24_make_index_from_xdf_handle(xdf_handle);
    record->num_meta_bytes = 0;

    record->file_offset = W64TOWD(addr - 1) * sizeof(uint32_t);
    record->total_stored_bytes = W64TOWD(lng) * sizeof(uint32_t);

    return TRUE;
}

int32_t fst24_write_xdf(
    fst_record* record,
    const int rewrite
) {
    if (record->metadata != NULL) {
        Lib_Log(APP_LIBFST, APP_WARNING, "%s: Trying to write a record that contains extended metadata in an XDF file."
                " This is not supported, we will ignore that metadata. (file %s)\n", __func__, record->file->path);
    }

    if (record->data_blocks.map != NULL) {
        Lib_Log(APP_LIBFST, APP_WARNING, "%s: Trying to write a record that contains a data map in an XDF file."
                " This is not supported, we will ignore the data map. (file %s)\n", __func__, record->file->path);
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
        record->data, NULL, -record->pack_bits, record->file->iun, record->dateo, record->deet, record->npas,
        record->ni, record->nj, record->nk, record->ip1, record->ip2, record->ip3,
        typvar, nomvar, etiket, grtyp, record->ig1, record->ig2, record->ig3, record->ig4, record->data_type, rewrite);

    pthread_mutex_unlock(&fst24_xdf_mutex);
    // --- END critical region ---

    record->do_not_touch.num_search_keys = sizeof(stdf_dir_keys) / sizeof(int32_t) - 2;
    record->do_not_touch.extended_meta_size = 0;
    record->do_not_touch.stored_data_size = 0; // We don't have a good way of knowing that number, so it stays at 0 for now. Maybe xdfprm?
    record->do_not_touch.unpacked_data_size = 0; // We also don't know that one reliably
    record->file_index = -1;

    if (ier < 0) return ier;
    return TRUE;
}

//! Find the next record in a given XDF file, according to the given parameters
//! \return Key of the record found (negative if error or nothing found)
int64_t find_next_xdf(const int32_t iun, fst_query* const query) {
    uint32_t* pkeys = (uint32_t *) &query->criteria.fst98_meta;
    uint32_t* pmask = (uint32_t *) &query->mask.fst98_meta;

    pkeys += W64TOWD(1);
    pmask += W64TOWD(1);

    const int32_t start_key = query->search_index & 0xffffffff;

    // --- START critical section (maybe) ---
    match_fn old_filter = NULL;
    if (query->options.skip_filter) {
        pthread_mutex_lock(&fst24_xdf_mutex);
        old_filter = xdf_set_file_filter(iun, NULL);
    }

    const int64_t key = (int64_t) c_xdfloc2(iun, start_key, pkeys, 16, pmask);

    if (query->options.skip_filter) {
        xdf_set_file_filter(iun, old_filter);
        pthread_mutex_unlock(&fst24_xdf_mutex);
    }
    // --- END critical section (if necessary) ---

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

//! Decode the given raw data pointer as if it were the content of an XDF record.
//! \return A properly initialized fst_record object. If we were successful in decoding the data, the record `data`
//!         pointer will be valid; if we were not successful, the `data` pointer will be NULL.
fst_record fst24_decode_data_xdf(
    //!> [in] Input data to be extracted
    const void* const data,
    //!> [in,out] [Optional] If non-NULL, must point to a sufficiently large space to hold the entire extracted data
    void* const dest_data
) {
    fst_record rec = default_fst_record;

    union {
        file_record rec;
        stdf_dir_keys keys;
        uint32_t words[sizeof(stdf_dir_keys) / sizeof(uint32_t)];
    } xdf_info;

    xdf_info.keys = *(stdf_dir_keys*)data;
    #ifdef Little_Endian
        swap_buffer_endianness(xdf_info.words, sizeof(stdf_dir_keys) / sizeof(uint32_t));
    #endif // Little endian

    Lib_Log(APP_LIBFST, APP_DEBUG, "%s: rec lng = %d, addr %8x, idtyp %d\n",
        __func__, xdf_info.rec.lng, xdf_info.rec.addr, xdf_info.rec.idtyp);

    // Extract metadata
    search_metadata meta;
    meta.fst98_meta = xdf_info.keys;
    fill_with_search_meta(&rec, &meta, FST_RSF);
    // fst24_record_print(&rec);

    const size_t num_raw_bytes = xdf_info.rec.lng * sizeof(uint64_t); 
    const size_t needed_data_size = Max(num_raw_bytes, rec.do_not_touch.unpacked_data_size * sizeof(uint32_t));
    const size_t workspace_size = needed_data_size + 
                                  sizeof(stdf_dir_keys) + 
                                  128 * sizeof(uint32_t); // Enough space for the largest compression scheme + rounding up for alignment

    uint32_t* workspace = (uint32_t*)malloc(workspace_size);
    if (workspace == NULL) {
        Lib_Log(APP_LIBFST, APP_FATAL, "%s: Could not allocate %zu bytes for workspace\n", __func__, workspace_size);
        return rec;
    }

    memcpy(workspace, data, num_raw_bytes);
    #ifdef Little_Endian
        swap_buffer_endianness(workspace, num_raw_bytes / sizeof(uint32_t));
    #endif // Little endian

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

    // Unpack the data
    const int32_t status = fst24_unpack_data(dest, workspace + sizeof(stdf_dir_keys) / sizeof(uint32_t) + 2, &rec, 0, 1, rec.data_bits);

    // Indicate success
    if (status == 0) rec.data = dest;

    free(workspace);
    return rec;
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

    int32_t requested_num_bits = record->data_bits;
    if (is_type_integer(record->data_type) || is_type_real(record->data_type)) {
        if (requested_num_bits > 32)
            requested_num_bits = 64;
        else if (requested_num_bits > 16)
            requested_num_bits = 32;
        else if (requested_num_bits > 8)
            requested_num_bits = 16;
        else
            requested_num_bits = 8;
    }

    // --- START critical region ---
    pthread_mutex_lock(&fst24_xdf_mutex);

    if (requested_num_bits == 8) {
        c_fst_data_length(1);
    }
    else if (requested_num_bits == 16) {
        c_fst_data_length(2);
    }
    else if (requested_num_bits == 64) {
        c_fst_data_length(8);
    }
    const int32_t handle = c_fstluk_xdf(record->data, key32, &record->ni, &record->nj, &record->nk);

    pthread_mutex_unlock(&fst24_xdf_mutex);
    // --- END critical region ---

    if (handle != key32) { return ERR_NOT_FOUND; }
    if (update_attributes_from_xdf_handle(record, handle) != TRUE) { return FALSE; }

    if (requested_num_bits > record->data_bits) record->data_bits = requested_num_bits;

    return handle;
}
