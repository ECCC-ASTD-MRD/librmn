#ifndef RMN_FST_RECORD_INTERNAL_H__
#define RMN_FST_RECORD_INTERNAL_H__

#include <rmn/fst24_record.h>

#include "fst98_internal.h"
#include "qstdir.h"

void make_search_criteria(const fst_record* record, stdf_dir_keys* criteria, stdf_dir_keys* mask);
void fill_with_dir_keys(fst_record* record, const stdf_dir_keys* keys);
fst_record record_from_dir_keys(const stdf_dir_keys* keys);
void print_non_wildcards(const fst_record* const record);
void print_dir_keys(const stdf_dir_keys* const keys);

//! Set most members of the given fst_record struct to their default value, while preserving
//! the data pointer and the allocation flag and status;
static inline void fst_record_set_to_default(fst_record* const record) {
    const int64_t flags = record->flags;
    const int64_t alloc = record->alloc;
    void* const data  = record->data;
    *record = default_fst_record;
    record->flags = flags;
    record->alloc = alloc;
    record->data  = data;
}

#endif // RMN_FST_RECORD_INTERNAL_H__
