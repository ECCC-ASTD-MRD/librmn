#ifndef RMN_FST24_FILE_INTERNAL_H__
#define RMN_FST24_FILE_INTERNAL_H__

#include "rmn/fst24_record.h"

int32_t fst24_write_rsf(RSF_handle rsf_file, fst_record* record, const int32_t stride);
int32_t get_record_from_key_rsf(const RSF_handle rsf_file, const int64_t key, fst_record* const record);
int32_t fst24_unpack_data(void* dest, void* source, const fst_record* record, const int32_t skip_unpack,
                          const int32_t stride);
int64_t find_next_rsf(const RSF_handle file_handle, fst_query* const query); // RSF only


#endif // RMN_FST24_FILE_INTERNAL_H__
