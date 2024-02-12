#ifndef RMN_FST_FILE_H__
#define RMN_FST_FILE_H__

#include <stdint.h>

#include "rmn/fst_record.h"
#include "rmn/fstd98.h"

typedef enum {
    FST_NONE = 0,
    FST_XDF  = 1,
    FST_RSF  = 2
} fst_file_type;

typedef struct fst23_file {
    int32_t       iun;          //!< File unit, used by fnom
    int32_t       file_index;   //!< File index in list of open files (the list is different for RSF vs XDF)
    fst_file_type type;         //!< Type of file (RSF, XDF, etc.)
    struct fst23_file* next;    //!< Next file in linked list of files (if any)
} fst_file;

static fst_file default_fst_file = (fst_file) {
    .iun        =  0,
    .file_index = -1,
    .type       = FST_NONE,
    .next       = NULL
};

int32_t    fst23_file_is_open(const fst_file* file);
fst_file*  fst23_open(const char* file_name, const char* options);
int32_t    fst23_close(fst_file* file);
int64_t fst23_get_num_records(const fst_file* file);

int32_t    fst23_write(fst_file* file, const fst_record* record, int rewrit);

int32_t fst23_set_search_criteria(fst_file* file, const fst_record* criteria);
int32_t fst23_read(fst_file* file, fst_record* record /* in,out */);  // read data for a record that was already found
int32_t fst23_read_new(fst_file* file, fst_record* record);
int32_t fst23_find_next(fst_file* file, fst_record* result);
int32_t fst23_find_all(fst_file* file, fst_record* results, const int32_t max_num_results);
int32_t fst23_read_next(fst_file* file, fst_record* record /* out only */); // read data + metadata for the next record to be found

int32_t fst23_link_files(fst_file** file, const int32_t num_files);
int32_t fst23_unlink_files(fst_file* file);
int32_t fst23_print_summary(const fst_file* file, const fst_record_fields* fields);

#endif // RMN_FST_FILE_H__
