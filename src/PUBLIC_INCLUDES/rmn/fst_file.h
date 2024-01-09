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

typedef struct {
    int32_t       iun;
    int32_t       file_index;
    fst_file_type type;
} fst_file;

static fst_file default_fst_file = (fst_file) {
    .iun        =  0,
    .file_index = -1,
    .type       = FST_NONE
};

int32_t    fst23_file_is_open(const fst_file* file);
fst_file*  fst23_open(const char* file_name, const char* options);
int32_t    fst23_close(fst_file* file);
int32_t    fst23_write(fst_file* file, const fst_record* record,int rewrit);
int32_t    fst23_get_iun(fst_file* file);

fst_record fst23_read(fst_file* file, const int64_t handle);
int32_t fst23_read_new(fst_file* file, fst_record* record);
int32_t fst23_find(fst_file* file, const fst_record* criteria, fst_record* result);
int32_t fst23_set_search_criteria(fst_file* file, const fst_record* criteria);
int32_t fst23_find_next(fst_file* file, fst_record* result);

int32_t fst23_get_record_from_key(fst_file* file, const int64_t key, fst_record* record);


// Proposed interface
int32_t fst23_read_data(fst_file* file, fst_record* record /* in,out */);  // read data for a record that was already found
int32_t fst23_read_record(fst_file* file, const int64_t handle, fst_record* record /* out only */); // read data + metadata for a record using only its key/handle
int32_t fst23_read_next(fst_file* file, fst_record* record /* out only */); // read data + metadata for the next record to be found

#endif // RMN_FST_FILE_H__
