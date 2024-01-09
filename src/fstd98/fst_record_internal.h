#ifndef RMN_FST_RECORD_INTERNAL_H__
#define RMN_FST_RECORD_INTERNAL_H__

#include "rmn/fst_record.h"

#include "fstd98_internal.h"
#include "qstdir.h"

void make_search_criteria(const fst_record* record, stdf_dir_keys* criteria, stdf_dir_keys* mask);
fst_record record_from_dir_keys(const stdf_dir_keys* keys);

void fst23_record_diff(const fst_record* a, const fst_record* b);

#endif // RMN_FST_RECORD_INTERNAL_H__
