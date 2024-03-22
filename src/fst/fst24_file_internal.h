#ifndef RMN_FST24_FILE_INTERNAL_H__
#define RMN_FST24_FILE_INTERNAL_H__

#include "rmn/fst24_record.h"

int32_t fst24_write_rsf(RSF_handle rsf_file, fst_record* record, const int32_t stride);

#endif // RMN_FST24_FILE_INTERNAL_H__
