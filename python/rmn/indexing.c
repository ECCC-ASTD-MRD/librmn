#include "indexing.h"
#include <rmn.h>

RecordData *index_rmn_file(const char *filename, const char *mode){

    struct fst24_file_ *f = fst24_open(filename, NULL);

    fst_record rec = default_fst_record;

    fst_query *query = fst24_new_query(f, &rec, NULL);

    return NULL;
}

RecordData *NewRecordData(size_t nb_records)
{
    RecordData *data = malloc(sizeof(*data));
    // Reallocate memory for each array
    data->ni = calloc(nb_records, sizeof(*(data->ni)));
    data->nj = calloc(nb_records, sizeof(*(data->nj)));
    data->nk = calloc(nb_records, sizeof(*(data->nk)));
    data->dateo = calloc(nb_records, sizeof(*(data->dateo)));
    data->deet = calloc(nb_records, sizeof(*(data->deet)));
    data->npas = calloc(nb_records, sizeof(*(data->npas)));
    data->pack_bits = calloc(nb_records, sizeof(*(data->pack_bits)));
    data->data_type = calloc(nb_records, sizeof(*(data->data_type
    )));
    data->ip1 = calloc(nb_records, sizeof(*(data->ip1)));
    data->ip2 = calloc(nb_records, sizeof(*(data->ip2)));
    data->ip3 = calloc(nb_records, sizeof(*(data->ip3)));

    data->typvar = calloc(nb_records, sizeof(*(data->typvar)));
    data->nomvar = calloc(nb_records, sizeof(*(data->nomvar)));
    data->etiket = calloc(nb_records, sizeof(*(data->etiket)));
    data->grtyp = calloc(nb_records, sizeof(*(data->grtyp)));
    data->path = calloc(nb_records, sizeof(*(data->path)));

    for (int i = 0; i < nb_records; ++i)
    {
        (data->typvar)[i] = (char *)calloc(3, sizeof(char));
        (data->nomvar)[i] = (char *)calloc(5, sizeof(char));
        (data->etiket)[i] = (char *)calloc(13, sizeof(char));
        (data->grtyp)[i] = (char *)calloc(2, sizeof(char));
        (data->path)[i] = (char *)calloc(PATH_MAX, sizeof(char));
        strcpy((data->typvar[i]), "  \0");
        strcpy((data->nomvar[i]), "    \0");
        strcpy((data->etiket[i]), "            \0");
        strcpy((data->grtyp[i]), " \0");
        strcpy((data->path[i]), "\0");
    }

    data->ig1 = calloc(nb_records, sizeof(*(data->ig1)));
    data->ig2 = calloc(nb_records, sizeof(*(data->ig2)));
    data->ig3 = calloc(nb_records, sizeof(*(data->ig3)));
    data->ig4 = calloc(nb_records, sizeof(*(data->ig4)));

    data->swa = calloc(nb_records, sizeof(*(data->swa)));
    data->lng = calloc(nb_records, sizeof(*(data->lng)));
    data->dltf = calloc(nb_records, sizeof(*(data->dltf)));
    data->ubc = calloc(nb_records, sizeof(*(data->ubc)));
    data->extra1 = calloc(nb_records, sizeof(*(data->extra1)));
    data->extra2 = calloc(nb_records, sizeof(*(data->extra2)));
    data->extra3 = calloc(nb_records, sizeof(*(data->extra3)));

    // Check if realloc was successful
    if (data->ni == NULL || data->nj == NULL || data->nk == NULL ||
        data->dateo == NULL || data->deet == NULL || data->npas == NULL ||
        data->pack_bits == NULL || data->data_type == NULL || data->ip1 == NULL ||
        data->ip2 == NULL || data->ip3 == NULL || data->typvar == NULL ||
        data->nomvar == NULL || data->etiket == NULL || data->grtyp == NULL ||
        data->ig1 == NULL || data->ig2 == NULL || data->ig3 == NULL ||
        data->ig4 == NULL || data->swa == NULL || data->lng == NULL ||
        data->dltf == NULL || data->ubc == NULL || data->extra1 == NULL ||
        data->extra2 == NULL || data->extra3 == NULL || data->path == NULL){
        free_record_data(data);
        return NULL;
    }

    return data;
}

void free_record_data(RecordData *data)
{
    free(data->ni);
    free(data->nj);
    free(data->nk);
    free(data->dateo);
    free(data->deet);
    free(data->npas);
    free(data->pack_bits);
    free(data->data_type);
    free(data->ip1);
    free(data->ip2);
    free(data->ip3);

    for (int i = 0; i < data->nb_records; i++)
    {
        free((data->typvar)[i]);
        free((data->nomvar)[i]);
        free((data->etiket)[i]);
        free((data->grtyp)[i]);
        free((data->path)[i]);
    }

    free(data->typvar);
    free(data->nomvar);
    free(data->etiket);
    free(data->grtyp);
    free(data->path);
    free(data->ig1);
    free(data->ig2);
    free(data->ig3);
    free(data->ig4);
    free(data->swa);
    free(data->lng);
    free(data->dltf);
    free(data->ubc);
    free(data->extra1);
    free(data->extra2);
    free(data->extra3);
}



RecordVector *RecordVector_new(size_t initial_capacity)
{
    RecordVector *rv = malloc(sizeof(*rv));
    rv->capacity = initial_capacity;
    rv->size = 0;
    rv->records = calloc(rv->capacity, sizeof(*rv->records));

    return rv;
}

int RecordVector_grow(RecordVector *rv){
    fprintf(stderr, "%s(): growing to %lu\n", __func__, 2*rv->capacity);
    fst_record *new = reallocarray(rv->records, 2*rv->capacity, sizeof(*rv->records));
    if(new == NULL){
        perror("reallocarray");
        exit(1);
    }

    rv->records = new;
    rv->capacity = 2*rv->capacity;
    return 0;
}

int RecordVector_push(RecordVector *rv, fst_record *rec)
{
    if(rv->size == rv->capacity){
        if(RecordVector_grow(rv)){
            fprintf(stderr, "Could not grow vector of records\n");
            return 1;
        }
    }
    rv->records[rv->size++] = *rec;
    return 0;
}

void RecordVector_free(RecordVector *rv)
{
    free(rv->records);
}

void free_record_data(RecordData *data);
