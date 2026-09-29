#ifndef RMN_FST24_FILE_INTERNAL_H__
#define RMN_FST24_FILE_INTERNAL_H__

#include <App.h>

#include "rmn/fst24_file.h"
#include "rmn/fst24_record.h"

typedef enum {
    FST_NONE = 0,
    FST_XDF  = 1,
    FST_RSF  = 2
} fst_file_type;

typedef struct fst_backend_ops_ fst_backend_ops; //!< Backend operations (defined in fst24_backend.h)

//! Base type to reference a FST file
typedef struct fst24_file_ {
    int32_t       iun;                  //!< File unit, used by fnom
    int32_t       file_index;           //!< File index in list of open FST files (the list is different for RSF and XDF)
    int32_t       file_index_backend;   //!< File index in one of the lists of either RSF or XDF open files
    fst_file_type type;                 //!< Type of file (RSF, XDF, etc.)
    const fst_backend_ops* ops;         //!< Backend operations (set in fst24_open)
    RSF_handle    rsf_handle;           //!< If type is RSF, handle to the file
    struct fst24_file_ *next;           //!< Next file in linked list of files (if any)
    const char* path;                   //!< Given when opening this file
    char*       tag;                    //!< Optional object tag (used in interpreted wrappers)
    TApp_Timer  read_timer;             //!< Keep track of time spent reading data
    TApp_Timer  write_timer;            //!< Keep track of time spent writing data
    TApp_Timer  find_timer;             //!< Keep track of time spent looking for data
    int64_t     num_bytes_read;         //!< Keep track of how many data bytes were read from this file
    int64_t     num_bytes_written;      //!< Keep track of how many data bytes were written to this file
    int32_t     num_records_found;      //!< Keep trakc of how many records were found in this file through the various find functions
} fst_file;

int32_t fst24_get_record_from_key(const fst_file* const file, const int64_t key, fst_record* const record);
int32_t fst24_unpack_data(void* dest, void* source, const fst_record* record, const int32_t skip_unpack,
                          const int32_t stride, const int32_t original_num_bits);
int C_fst_rsf_match_req(int datev, int ni, int nj, int nk, int ip1, int ip2, int ip3,
                        const char* typvar, const char* nomvar, const char* etiket, const char* grtyp,
                        int ig1, int ig2, int ig3, int ig4);
void print_non_default_options(const fst_query_options* const options);

//! Build the packed search metadata for the given record (shared by the backends)
search_metadata* make_search_metadata(const fst_record* record, search_metadata* const dest);

//! Retrieve a record's data minimum and maximum value (shared by the backends)
void fst24_bounds(const fst_record* record, double* Min, double* Max);

#endif // RMN_FST24_FILE_INTERNAL_H__
