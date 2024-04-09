#ifndef RMN_FST24_FILE_INTERNAL_H__
#define RMN_FST24_FILE_INTERNAL_H__

#include "rmn/fst24_record.h"

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

int32_t fst24_write_rsf(RSF_handle rsf_file, fst_record* record, const int32_t stride);
int32_t get_record_from_key_rsf(const RSF_handle rsf_file, const int64_t key, fst_record* const record);
int32_t fst24_unpack_data(void* dest, void* source, const fst_record* record, const int32_t skip_unpack,
                          const int32_t stride);
int64_t find_next_rsf(const RSF_handle file_handle, fst_query* const query); // RSF only
int C_fst_rsf_match_req(int datev, int ni, int nj, int nk, int ip1, int ip2, int ip3,
                        const char* typvar, const char* nomvar, const char* etiket, const char* grtyp,
                        int ig1, int ig2, int ig3, int ig4);


#endif // RMN_FST24_FILE_INTERNAL_H__
