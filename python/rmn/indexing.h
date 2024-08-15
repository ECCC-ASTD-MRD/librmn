#include <inttypes.h>
#include <string.h> //size_t
#include <rmn.h>
/*
 * Each field represents a column in a table where each row represents a record
 * This organization allows the creation of numpy arrays of homogenous type
 * that can become the columns of a Pandas data frame.
 */
typedef struct
{
    size_t nb_records;
    uint32_t *ni;
    uint32_t *nj;
    uint32_t *nk;
    uint32_t *dateo;
    uint32_t *deet;
    uint32_t *npas;
    uint32_t *pack_bits;
    uint32_t *data_bits;
    uint32_t *data_type;
    uint32_t *ip1;
    uint32_t *ip2;
    uint32_t *ip3;
    char *typvar;
    char *nomvar;
    char *etiket;
    char *grtyp;
    uint32_t *ig1;
    uint32_t *ig2;
    uint32_t *ig3;
    uint32_t *ig4;
    uint32_t *file_index;
    char *path;
} RecordData;

typedef struct {
    size_t size;
    size_t capacity;
    fst_record *records;
} RecordVector;

RecordData *NewRecordData(size_t nb_records);
void free_record_data(RecordData *data);

void RecordVector_free(RecordVector *rv);
#define RecordVector_FREE(rv) do{RecordVector_free((rv)); (rv) = NULL;}while(0)
int RecordVector_push(RecordVector *rv, fst_record *rec);
int RecordVector_grow(RecordVector *rv);
RecordVector *RecordVector_new(size_t initial_capacity);
RecordData *rmn_get_index_columns_raw(const char **filenames, int nb_files);
