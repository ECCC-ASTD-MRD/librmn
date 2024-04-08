#ifndef RMN_FST_INTERNAL_H_
#define RMN_FST_INTERNAL_H_

//! \file fst_internal.h
//! Functionality used by both FST interfaces (98 and 24)

#include "qstdir.h"
#include "rmn/fst24_file.h"
#include "rmn/fst24_record.h"

typedef struct fst24_file_ fst_file;  // Forward declare the fst_file type

//! Number of 32-bit elements in the search metadata that are reserved for
//! the fst24 implementation
#define FST24_META_RESERVED 1

//! Object that encodes the criteria for a search into an FST file
//! It is meant to be compatible with both RSF and XDF files, and to remain backward-compatible
//! when the criteria change (they can only be added)
typedef struct {
    union {
        struct {
            uint32_t rsf_reserved[RSF_META_RESERVED];       //!< Reserved for RSF backend usage
            uint32_t fst24_reserved[FST24_META_RESERVED];   //!< Reserved RSF fst24 interface
        };

        // Elements that are inherited from the fst98 interface
        struct {
            //!> Additional space reserved for RSF backend or fst24 interface
            uint32_t fst98_reserved[RSF_META_RESERVED + FST24_META_RESERVED - 2];
            //!> Elements inherited from the fst98 interface. The first two 32-bit elements
            //!> were not used as search criteria, so we will be using them as reserved space in
            //!> RSF files
            stdf_dir_keys fst98_meta;
        };
    };
    // From this point, any additional search criteria (keys) will only be available in RSF files
    // Every time criteria are added to the search_metadata struct, FST24_ME
} search_metadata;

static const uint32_t FST24_META_RESERVED_0 = (FST24_VERSION_COUNT << 8) | (sizeof(search_metadata) / sizeof(uint32_t));

static inline void decode_fst24_reserved_0(
    const uint32_t reserved_0,
    uint8_t* fst24_version_count,
    uint8_t* num_criteria
) {
    *num_criteria = (reserved_0 & 0xff);                // First byte
    *fst24_version_count = (reserved_0 & 0xff00) >> 8;  // Second byte
}

//! Object used to describe a search query into a standard file.
//! Used for FST24 (both RSF and XDF) and for FST98 RSF
typedef struct fst_query_ {
    const fst_file* file;     //!< The fst_file in which this query will search (fst24 only)
    void *search_meta;        //!< A metadata object, if we want to search inside metadata
    search_metadata criteria; //!< The criteria themselves
    search_metadata mask;     //!< Bitmask of things to ignore 
    search_metadata background_mask; //!< No idea, seems to be for fst98 interface
    int64_t search_index;     //!< Index to keep our position within the file when searching
    int32_t num_criteria;     //!< How many criteria (32-bit elements) will be evaluated (size of stdf_dir_keys for now)
    int32_t search_done;      //!< Whether we are done searching the whole file (with a certain criteria)
    fst_query_options options;//!< Options to modulate how the search is performed
    int32_t ip1s[2];
    int32_t ip2s[2];
    int32_t ip3s[2];
    int32_t multistep_match;
    struct fst_query_* next;  //!< A link to the query that will search into the next linked file (fst24 only)
} fst_query;

//! Initialize a query to neutral values everywhere
static inline fst_query new_fst_query(const fst_query_options* options) {
    if (options == NULL) options = &default_query_options;

    fst_query q;
    q.file         = NULL;
    q.search_meta  = NULL;
    memset(&q.criteria, 0xff, sizeof(search_metadata));
    memset(&q.mask, 0xff, sizeof(search_metadata));
    memset(&q.background_mask, 0xff, sizeof(search_metadata));
    q.search_index = 0;
    q.num_criteria = 0;
    q.search_done  = 0;
    q.options      = *options;
    q.ip1s[0] = 0; q.ip1s[1] = 0;
    q.ip2s[0] = 0; q.ip2s[1] = 0;
    q.ip3s[0] = 0; q.ip3s[1] = 0;
    q.multistep_match = 0;

    q.next = NULL;

    return q;
}

//! Copy a query's criteria, but with a reset start index, without file, without "next"
static inline fst_query fst_query_copy(const fst_query* const query) {
    fst_query result = new_fst_query(&query->options);
    result.search_meta  = query->search_meta;
    result.criteria     = query->criteria;
    result.mask         = query->mask;
    result.background_mask = query->background_mask;
    result.num_criteria = query->num_criteria;

    return result;
}

int32_t fst24_query_is_valid(const fst_query* const q);

#endif // RMN_FST_INTERNAL_H_
