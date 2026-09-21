#ifndef RMN_FST24_BACKEND_RSF_H__
#define RMN_FST24_BACKEND_RSF_H__

#include "fst24_file_internal.h"

//! Fill fst_record attributes from given RSF metadata
int32_t update_attributes_from_rsf_info(
    fst_record* record,
    const int64_t key,
    const RSF_record_info* record_info
);

//! Write a record in an RSF file
int32_t fst24_write_rsf(RSF_handle rsf_file, fst_record * const record, const int32_t stride);

//! Rewrite the metadata of a record in an RSF file
int32_t fst24_rewrite_meta_rsf(RSF_handle file_handle, const int64_t record_handle, const fst_record* const record);

//! Get basic information about the record with the given key from an RSF file
int32_t get_record_from_key_rsf(const RSF_handle rsf_file, const int64_t key, fst_record* const record);

//! Find the next record in a given RSF file, according to the given parameters
int64_t find_next_rsf(const RSF_handle file_handle, fst_query* const query);

//! Decode the given raw data pointer as if it were the content of an RSF record
fst_record fst24_decode_data_rsf(void* data, void* dest);

//! Read a record from an RSF file
int32_t fst24_read_record_rsf(fst_record* record, const int32_t skip_unpack, const int32_t metadata_only);

#endif // RMN_FST24_BACKEND_RSF_H__
