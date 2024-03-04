#ifndef RMN_FST_RECORD_INTERNAL_H__
#define RMN_FST_RECORD_INTERNAL_H__

#include "rmn/fst24_record.h"

#include "fst98_internal.h"
#include "qstdir.h"

void make_search_criteria(const fst_record* record, stdf_dir_keys* criteria, stdf_dir_keys* mask);
void fill_with_dir_keys(fst_record* record, const stdf_dir_keys* keys);
fst_record record_from_dir_keys(const stdf_dir_keys* keys);
void print_non_wildcards(const fst_record* record);

#endif // RMN_FST_RECORD_INTERNAL_H__
