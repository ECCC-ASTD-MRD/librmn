#ifndef RMN_FST_INTERNAL_H_
#define RMN_FST_INTERNAL_H_

//! \file fst_internal.h
//! Functionality used by both FST interfaces (98 and 24)

#include "qstdir.h"

typedef struct fst24_file_ fst_file;  // Forward declare the fst_file type

//! Object used to describe a search query into a standard file.
//! Used for FST24 (both RSF and XDF) and for FST98 RSF
typedef struct fst_query_ {
    const fst_file* file;   //!< The fst_file in which this query will search (fst24 only)
    void *search_meta;      //!< A metadata object, if we want to search inside metadata
    stdf_dir_keys criteria; //!< The criteria themselves
    stdf_dir_keys mask;     //!< Bitmask of things to ignore 
    stdf_dir_keys background_mask; //!< No idea
    int64_t search_index;   //!< Index to keep our position within the file when searching
    int32_t num_criteria;   //!< How many criteria (32-bit elements) will be evaluated (size of stdf_dir_keys for now)
    int32_t search_done;    //!< Whether we are done searching the whole file (with a certain criteria)
    struct fst_query_* next;//!< A link to the query that will search into the next linked file (fst24 only)
} fst_query;


static inline fst_query new_fst_query() {
    fst_query q;
    q.file         = NULL;
    q.search_meta  = NULL;
    memset(&q.criteria, 0xff, sizeof(stdf_dir_keys));
    memset(&q.mask, 0xff, sizeof(stdf_dir_keys));
    memset(&q.background_mask, 0xff, sizeof(stdf_dir_keys));
    q.search_index = 0;
    q.num_criteria = 0;
    q.search_done  = 0;
    q.next         = NULL;

    return q;
}

//! Copy a query's criteria, but with a reset start index, without file, without "next"
static inline fst_query fst_query_copy(const fst_query* const query) {
    fst_query result = new_fst_query();
    result.search_meta  = query->search_meta;
    result.criteria     = query->criteria;
    result.mask         = query->mask;
    result.background_mask = query->background_mask;
    result.num_criteria = query->num_criteria;

    return result;
}

static inline int32_t is_query_valid(const fst_query* const q) {
    return (q != NULL && q->file != NULL && q->num_criteria > 0);
}

int64_t find_next_rsf(const RSF_handle file_handle, fst_query* const query); // RSF only

#endif // RMN_FST_INTERNAL_H_
