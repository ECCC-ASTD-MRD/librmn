#ifndef RMN_FST_FILE_H__
#define RMN_FST_FILE_H__

#include <stdint.h>

#include "rmn/fst24_record.h"
#include "rmn/fst98.h"

typedef enum {
    FST_NONE = 0,
    FST_XDF  = 1,
    FST_RSF  = 2
} fst_file_type;

static const char* fst_file_type_name[] = {
    [FST_NONE] = "FST_NONE",
    [FST_XDF]  = "FST_XDF",
    [FST_RSF]  = "FST_RSF"
};

//! Base type to refrerence a FST file
typedef struct fst24_file_ {
    int32_t       iun;                  //!< File unit, used by fnom
    int32_t       file_index;           //!< File index in list of open FST files (the list is different for RSF and XDF)
    int32_t       file_index_backend;   //!< File index in one of the lists of either RSF or XDF open files
    fst_file_type type;                 //!< Type of file (RSF, XDF, etc.)
    struct fst24_file_ *next;           //!< Next file in linked list of files (if any)
} fst_file;

static fst_file default_fst_file = (fst_file) {
    .iun                =  0,
    .file_index         = -1,
    .file_index_backend = -1,
    .type               = FST_NONE,
    .next               = NULL
};

//! @defgroup public_fst Public FST C API
//! @{
int32_t   fst24_is_valid(const char* const filePath);
int32_t   fst24_is_open(const fst_file* file);
fst_file* fst24_open(const char* const filePath, const char* const options);
int32_t   fst24_close(fst_file* const file);
int64_t   fst24_get_num_records(const fst_file* const file);
int32_t   fst24_eof(const fst_file* const file);

int32_t   fst24_read(fst_record* const record);
void*     fst24_read_metadata(fst_record* record);
int32_t   fst24_read_next(fst_file* file, fst_record* record);
int32_t   fst24_write(fst_file* file, fst_record* record, int rewrit);
int32_t   fst24_set_search_criteria(fst_file* file, const fst_record* criteria);
int32_t   fst24_rewind_search(fst_file* file);
int32_t   fst24_find(fst_file* file, const fst_record* criteria, fst_record* result);
int32_t   fst24_find_next(fst_file* const file, fst_record* result);
int32_t   fst24_find_all(fst_file* file, fst_record* results, const int32_t max_num_results);

int32_t   fst24_link(fst_file** files, const int32_t num_files);
int32_t   fst24_unlink(fst_file* const file);
int32_t   fst24_print_summary(fst_file* const file, const fst_record_fields* const fields);
int32_t   fst24_checkpoint(const fst_file* const file);
//! @}

#endif // RMN_FST_FILE_H__
