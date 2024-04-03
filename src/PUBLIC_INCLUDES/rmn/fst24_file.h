#ifndef RMN_FST_FILE_H__
#define RMN_FST_FILE_H__

#include <stdint.h>

#include "rmn/fst24_record.h"
#include "rmn/fst98.h"
#include "rmn/rsf.h"

//! Base type to reference a FST file
typedef struct fst24_file_ {
    int32_t       iun;                  //!< File unit, used by fnom
    int32_t       file_index;           //!< File index in list of open FST files (the list is different for RSF and XDF)
    int32_t       file_index_backend;   //!< File index in one of the lists of either RSF or XDF open files
    fst_file_type type;                 //!< Type of file (RSF, XDF, etc.)
    RSF_handle    rsf_handle;           //!< If type is RSF, handle to the file
    struct fst24_file_ *next;           //!< Next file in linked list of files (if any)
} fst_file;

static fst_file default_fst_file = (fst_file) {
    .iun                =  0,
    .file_index         = -1,
    .file_index_backend = -1,
    .rsf_handle.p       = NULL,
    .type               = FST_NONE,
    .next               = NULL
};

typedef struct fst_query_ fst_query; // Forward declare

typedef struct {
    //!> Several encodings can represent the same floating point value stored in an IP. When setting ip1_all
    //!> (and ip2_all, and ip3_all), we indicate that we want to match with any encoding that result in the same
    //!> encoded value in the given criterion. If not set, we will only match with the specific encoding given.
    int32_t ip1_all;
    int32_t ip2_all; //!< When trying to match a certain IP2, match all encodings that result in the same encoded value
    int32_t ip3_all; //!< When trying to match a certain IP3, match all encodings that result in the same encoded value
} fst_query_options;

static fst_query_options default_query_options = (fst_query_options) {
    .ip1_all = 0,
    .ip2_all = 0,
    .ip3_all = 0,
};

//! @defgroup public_fst Public FST C API
//! @{
int32_t   fst24_is_valid(const char* const filePath);
int32_t   fst24_is_open(const fst_file* const file);
fst_file* fst24_open(const char* const filePath, const char* const options);
int32_t   fst24_close(fst_file* const file);
int64_t   fst24_get_num_records(const fst_file* const file);
int32_t   fst24_eof(const fst_file* const file);

int32_t    fst24_read_record(fst_record* const record);
void*      fst24_read_metadata(fst_record* const record);
int32_t    fst24_read_next(fst_query* const query, fst_record* const record);
int32_t    fst24_write(fst_file* const file, fst_record* const record, int rewrit);
int32_t    fst24_read(const fst_file* const file, const fst_record* criteria, const fst_query_options* options,
                      fst_record* const record);
fst_query* fst24_new_query(const fst_file* const file, const fst_record* criteria, const fst_query_options* options);
int32_t    fst24_rewind_search(fst_query* query);
// int32_t    fst24_find(fst_file* file,const fst_record* criteria, fst_record* result);
int32_t    fst24_find_next(fst_query* const query, fst_record* const result);
int32_t    fst24_find_all(fst_query* const query, fst_record* const results, const int32_t max_num_results);
int32_t    fst24_find_count(fst_query * const query);
int32_t    fst24_query_is_valid(const fst_query* const q);
void       fst24_query_free(fst_query* const query);

int32_t   fst24_link(fst_file** files, const int32_t num_files);
int32_t   fst24_unlink(fst_file* const file);
int32_t   fst24_print_summary(fst_file* const file, const fst_record_fields* const fields);
int32_t   fst24_flush(const fst_file* const file);
//! @}

#endif // RMN_FST_FILE_H__
