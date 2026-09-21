#ifndef RMN_FST24_BACKEND_XDF_H__
#define RMN_FST24_BACKEND_XDF_H__

#include <pthread.h>

#include "fst24_file_internal.h"

//! Mutex protecting the (not thread-safe) XDF primitives that are shared across
//! all open XDF files. Defined in fst24_backend_xdf.c.
extern pthread_mutex_t fst24_xdf_mutex;

//! Convert an XDF record handle to an fst24 record index
int32_t fst24_make_index_from_xdf_handle(const int handle);

//! Convert an fst24 record index to an XDF record handle
int32_t fst24_make_xdf_handle_from_index(const int index, const int file_id);

//! Fill fst_record attributes from metadata found in XDF file directory
int32_t update_attributes_from_xdf_handle(fst_record* record, const int xdf_handle);

//! Write a record in an XDF file
int32_t fst24_write_xdf(fst_record* record, const int rewrite);

//! Find the next record in a given XDF file, according to the given parameters
int64_t find_next_xdf(const int32_t iun, fst_query* const query);

//! Decode the given raw data pointer as if it were the content of an XDF record
fst_record fst24_decode_data_xdf(const void* data, void* dest);

//! Read a record from an XDF file
int32_t fst24_read_record_xdf(fst_record* record);

#endif // RMN_FST24_BACKEND_XDF_H__
