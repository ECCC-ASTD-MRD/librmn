#ifndef RMN_FST_INTERNAL_H_
#define RMN_FST_INTERNAL_H_

//! \file fst_internal.h
//! Functionality used by both FST interfaces (98 and 24)

#include "qstdir.h"
#include "rmn/fst24_file.h"
#include "rmn/fst24_record.h"
#include "rsf_internal.h"

//! Number of 32-bit elements in the search metadata that are reserved for
//! the fst24 implementation. See \ref search_metadata
//! In VERSION 0: 2 reserved spots
//! In VERSION 1: 1 reserved spot  (the other one was transferred to the RSF layer)
#define FST_META_RESERVED 1

//! A set of reserved 32-bit words at the start of FST record metadata
typedef struct {
    union {
        struct {
            uint16_t extended_meta_size; //!< Size of extended metadata in 32-bit elements
            uint8_t  num_keys; //!< Total number of keys in the metadata (including all reserved slots)
            uint8_t  version; //!< FST version number
        };
        uint32_t meta[1];
    };
} fst24_reserved_t;

//! Object that encodes the criteria for a search into an FST file
//! It is meant to be compatible with both RSF and XDF files, and to remain backward-compatible
//! when the criteria change (they can only be added)
//! Reserved 0 (FST): Contains version, number of search keys, size of extended metadata (see \ref fst24_reserved_t)
//! The metadata structure for an FST record is as follows:
//! - Search (directory) metadata, including fst24 reserved keys and RSF reserved keys
//! - Extended (JSON) metadata
typedef struct {
    union {
        struct {
            RSF_Reserved rsf_reserved;
            fst24_reserved_t fst24_reserved;
        };

        // Elements that are inherited from the fst98 interface
        struct {
            //!> Additional space reserved for RSF backend or fst24 interface
            uint32_t fst98_reserved[RSF_META_RESERVED + FST_META_RESERVED - 2];
            //!> Elements inherited from the fst98 interface. The first two 32-bit elements
            //!> were not used as search criteria, so we will be using them as reserved space in
            //!> RSF files
            stdf_dir_keys fst98_meta;
        };
    };
    // From this point, any additional search criteria (keys) will only be available in RSF files
    // Every time criteria are added to the search_metadata struct, FST24_ME

} search_metadata;

//! Search metadata struct as it was at RSF VERSION 0 and FST VERSION 0 (they were both incremented to 1 at the same time)
//! We keep this one so that we can still interpret records written with that old version combination
typedef struct {
    #define tmp_RSF_RESERVED_0 1
    #define tmp_FST_RESERVED_0 2
    union {
        struct {
            uint32_t rsf_reserved_uint[tmp_RSF_RESERVED_0];
            uint32_t fst24_reserved[tmp_FST_RESERVED_0];
        };
        struct {
            uint32_t fst98_reserved[tmp_RSF_RESERVED_0 + tmp_FST_RESERVED_0 - 2];
            stdf_dir_keys fst98_meta;
        };
    };
    #undef tmp_RSF_RESERVED_0
    #undef tmp_FST_RESERVED_0
} search_metadata_version_0_0;

//! The first reserved 32-bit word contains the version, number of search keys (32-bit elements)
//! and size of extended metadata in 32-bit elements
//! (layout on little-endian architecture)
//! |    31 - 24     |    23 - 16     |            15 - 0              |
//! | -- version --- | -- num keys -- | --- num ext meta elements ---- |
static inline fst24_reserved_t make_fst24_reserved(const uint16_t extended_meta_size) {
    fst24_reserved_t r;
    r.version = FST24_VERSION_COUNT;
    r.num_keys = sizeof(search_metadata) / sizeof(uint32_t);
    r.extended_meta_size = extended_meta_size;
    return r;
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

    q.next = NULL;

    return q;
}

//! Copy a query's criteria, but with a reset start index, without file, without "next"
static inline fst_query fst_query_copy(const fst_query* const query) {
    fst_query result = *query;
    result.file = NULL;
    result.search_index = 0;
    result.search_done = 0;
    result.next = NULL;

    return result;
}

int32_t fst24_query_is_valid(const fst_query* const q);

#endif // RMN_FST_INTERNAL_H_
