#include <inttypes.h>
#include <string.h> //size_t
#include <rmn.h>
#include <stdbool.h>
#include <limits.h>

#define FST_LABEL_LEN 13
#define FST_RUN_LEN 3
#define FST_IMPLEMENTATION_LEN 4
#define FST_ENSEMBLE_MEMBER_LEN 12
#define FST_ETIKET_FORMAT_LEN 10
#define FST_DATA_TYPE_STR_LEN 2
#define FST_GRID_LEN 32
#define FST_GRID_DESC_LEN 45
#define FST_PATH_LEN PATH_MAX
#define FST_INTERVAL_IP_LEN 12

/*
 * Each field represents a column in a table where each row represents a record
 * This organization allows the creation of numpy arrays of homogenous type
 * that can become the columns of a Pandas data frame.
 */
#pragma pack(push, 1)
typedef struct
{
    size_t nb_records;

    char *nomvar;
    char *typvar;
    char *etiket;
    uint32_t *ni;
    uint32_t *nj;
    uint32_t *nk;

    uint32_t *dateo;
    uint32_t *ip1;
    uint32_t *ip2;
    uint32_t *ip3;
    uint32_t *deet;
    uint32_t *npas;

    uint32_t *data_type;
    uint32_t *pack_bits;
    uint32_t *data_bits;
    char *grtyp;

    uint32_t *ig1;
    uint32_t *ig2;
    uint32_t *ig3;
    uint32_t *ig4;

    // char *grid_desc;
    char *path;
    uint32_t *file_index;

} RecordData;

#pragma pack(pop)
typedef struct
{
    size_t size;
    size_t capacity;
    fst_record *records;
} RecordVector;

RecordData *NewRecordData(size_t nb_records);
void free_record_data(RecordData *data);

void RecordVector_free(RecordVector *rv);
#define RecordVector_FREE(rv)    \
    do                           \
    {                            \
        RecordVector_free((rv)); \
        (rv) = NULL;             \
    } while (0)

int RecordVector_push(RecordVector *rv, fst_record *rec);
int RecordVector_grow(RecordVector *rv);
RecordVector *RecordVector_new(size_t initial_capacity);

RecordData *rmn_get_index_columns_raw(const char **filenames, int nb_files);

void print_record(RecordData *data);
void *fst_read_data_at_index(const char *path, const int32_t index);

