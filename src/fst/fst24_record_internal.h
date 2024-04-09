#ifndef RMN_FST_RECORD_INTERNAL_H__
#define RMN_FST_RECORD_INTERNAL_H__

#include <rmn/fst24_record.h>

#include "fst98_internal.h"
#include "fst24_file_internal.h"
#include "qstdir.h"

void make_search_criteria(const fst_record* record, fst_query* const query);
void fill_with_search_meta(fst_record* record, const search_metadata* meta, const fst_file_type type);
fst_record record_from_search_meta(const search_metadata* meta, const fst_file_type type);
void print_non_wildcards(const fst_record* const record);
void print_search_meta(const search_metadata* const meta, const fst_file_type type);

//! Copy record information (including metadata *pointer*) into destination, while preserving
//! the data pointer and the allocation flag and status;
//! Does not free any memory!
static inline void fst_record_copy_info(fst_record* const dest, const fst_record* const src) {
    const int64_t flags = dest->do_not_touch.flags;
    const int64_t alloc = dest->do_not_touch.alloc;
    void* const data  = dest->data;
    *dest = *src;
    dest->do_not_touch.flags = flags;
    dest->do_not_touch.alloc = alloc;
    dest->data  = data;
}

//! Set most members of the given fst_record struct to their default value, while preserving
//! the data pointer and the allocation flag and status;
static inline void fst_record_set_to_default(fst_record* const record) {
    fst_record_copy_info(record, &default_fst_record);
}

#endif // RMN_FST_RECORD_INTERNAL_H__
