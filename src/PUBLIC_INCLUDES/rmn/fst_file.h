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

int32_t fst_file_is_open(const fst_file* file);
fst_file* fst23_open(const char* file_name, const char* options);
int32_t fst23_close(fst_file* file);
int32_t fst23_write(fst_file* file, const fst_record* record);
fst_record fst23_find_record(fst_file* file, const fst_record* criteria);
int32_t fst23_get_iun(fst_file* file);

fst_record fst23_read_record(fst_file* file, const int64_t handle);

#endif // RMN_FST_FILE_H__
