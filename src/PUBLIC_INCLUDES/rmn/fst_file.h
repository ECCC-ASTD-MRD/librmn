#ifndef RMN_FST_FILE_H__
#define RMN_FST_FILE_H__

#include <stdint.h>

#include "rmn/fst_record.h"
#include "rmn/fstd98.h"

typedef struct {
    int32_t iun;
    int32_t file_index;
} fst_file;

static fst_file default_fst_file = (fst_file) {
    .iun        =  0,
    .file_index = -1
};

fst_file* fst23_open(const char* file_name, const char* options);
void fst23_close(fst_file* file);
int32_t fst23_write(fst_file* file, const fst_record* record);

#endif // RMN_FST_FILE_H__
