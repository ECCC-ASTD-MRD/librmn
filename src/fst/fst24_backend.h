#ifndef RMN_FST24_BACKEND_H__
#define RMN_FST24_BACKEND_H__

#include "fst24_file_internal.h"

//! Function-pointer table describing the backend-specific operations of the
//! fst24 file API. Each backend (RSF, XDF, ...) provides an instance of this
//! struct. The common fst24 API (fst24_file.c) dispatches to these functions
//! through the `ops` pointer stored in each open fst_file.
//!
//! Adding a new backend means providing a new fst_backend_ops table and setting
//! fst_file::ops to it in fst24_open; no changes to fst24_file.c are required.
typedef struct fst_backend_ops_ {
    const char* name; //!< Human-readable backend name (for logging)

    //!> Commit data/metadata to disk
    int32_t  (*flush)(const fst_file* file);
    //!> Number of records in the file (excluding linked files)
    int64_t  (*get_num_records)(const fst_file* file);
    //!> Write a record (append/overwrite is handled by the caller)
    int32_t  (*write)(fst_file* file, fst_record* record);
    //!> Rewrite the metadata of an existing record
    int32_t  (*rewrite_meta)(fst_file* file, fst_record* record);
    //!> Get record info (no data) from its key
    int32_t  (*get_record_from_key)(const fst_file* file, const int64_t key, fst_record* record);
    //!> Get record info (no data) at a given index
    int32_t  (*get_record_by_index)(const fst_file* file, const int32_t index, fst_record* record);
    //!> Find the next record matching the query; returns the key (negative if none)
    int64_t  (*find_next)(const fst_file* file, fst_query* query);
    //!> Read a record's data (and metadata)
    int32_t  (*read_record)(fst_record* record);
    //!> Read only the data map (NULL if unsupported)
    void*    (*read_data_map)(fst_record* record);
    //!> Read only the extended metadata (NULL if unsupported)
    void*    (*read_metadata)(fst_record* record);
    //!> Delete a record
    int32_t  (*delete_record)(fst_record* record);
    //!> Force-close a file left open in write mode by a crash
    int32_t  (*force_close)(const char* filename);
} fst_backend_ops;

//!> The RSF backend operations (defined in fst24_backend_rsf.c)
extern const fst_backend_ops fst24_rsf_ops;
//!> The XDF backend operations (defined in fst24_backend_xdf.c)
extern const fst_backend_ops fst24_xdf_ops;

#endif // RMN_FST24_BACKEND_H__
