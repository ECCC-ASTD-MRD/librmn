#ifndef RMN_FST_FILE_INTERNAL_H__
#define RMN_FST_FILE_INTERNAL_H__

#include "rmn/fst_file.h"

int32_t fst24_get_iun(fst_file* file);
int32_t fst24_find(fst_file* file, const fst_record* criteria, fst_record* result);
int64_t fst24_get_num_records_single(const fst_file* file);
int32_t fst24_get_record_from_key(const fst_file* file, const int64_t key, fst_record* record);
int32_t fst24_get_record_by_index(const fst_file* file, const int64_t index, fst_record* record);

#endif // RMN_FST_FILE_INTERNAL_H__
