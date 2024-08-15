#include "indexing.h"
#include <pthread.h>
#include "App_Timer.h"
#include <rmn.h>
#include <sys/types.h> // gettid
static pthread_mutex_t grow_mutex = PTHREAD_MUTEX_INITIALIZER;

RecordData *index_rmn_file(const char *filename, const char *mode){

    struct fst24_file_ *f = fst24_open(filename, NULL);

    fst_record rec = default_fst_record;

    fst_query *query = fst24_new_query(f, &rec, NULL);

    return NULL;
}

RecordData *NewRecordData(size_t nb_records)
{
    RecordData *data = malloc(sizeof(*data));
    data->ni = malloc(nb_records*sizeof(*(data->ni)));
    data->nj = malloc(nb_records*sizeof(*(data->nj)));
    data->nk = malloc(nb_records*sizeof(*(data->nk)));
    data->dateo = malloc(nb_records*sizeof(*(data->dateo)));
    data->deet = malloc(nb_records*sizeof(*(data->deet)));
    data->npas = malloc(nb_records*sizeof(*(data->npas)));
    data->pack_bits = malloc(nb_records*sizeof(*(data->pack_bits)));
    data->data_type = malloc(nb_records*sizeof(*(data->data_type
    )));
    data->ip1 = malloc(nb_records*sizeof(*(data->ip1)));
    data->ip2 = malloc(nb_records*sizeof(*(data->ip2)));
    data->ip3 = malloc(nb_records*sizeof(*(data->ip3)));

    data->typvar = malloc(nb_records*FST_TYPVAR_LEN * sizeof(char));
    data->nomvar = malloc(nb_records*FST_NOMVAR_LEN * sizeof(char));
    data->etiket = malloc(nb_records*FST_ETIKET_LEN * sizeof(char));
    data->grtyp = malloc(nb_records*FST_GTYP_LEN * sizeof(char));
    data->path = malloc(nb_records*(PATH_MAX+1) * sizeof(char));

    data->ig1 = malloc(nb_records*sizeof(*(data->ig1)));
    data->ig2 = malloc(nb_records*sizeof(*(data->ig2)));
    data->ig3 = malloc(nb_records*sizeof(*(data->ig3)));
    data->ig4 = malloc(nb_records*sizeof(*(data->ig4)));

    data->file_index = malloc(nb_records*sizeof(*(data->file_index)));

    // Check if realloc was successful
    if (data->ni == NULL || data->nj == NULL || data->nk == NULL ||
        data->dateo == NULL || data->deet == NULL || data->npas == NULL ||
        data->pack_bits == NULL || data->data_type == NULL || data->ip1 == NULL ||
        data->ip2 == NULL || data->ip3 == NULL || data->typvar == NULL ||
        data->nomvar == NULL || data->etiket == NULL || data->grtyp == NULL ||
        data->ig1 == NULL || data->ig2 == NULL || data->ig3 == NULL ||
        data->ig4 == NULL || data->path == NULL){
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

    free(data->typvar);
    free(data->nomvar);
    free(data->etiket);
    free(data->grtyp);
    free(data->path);

    free(data->ig1);
    free(data->ig2);
    free(data->ig3);
    free(data->ig4);

    free(data->file_index);
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
    pthread_mutex_lock(&grow_mutex);
    fst_record *new = reallocarray(rv->records, 2*rv->capacity, sizeof(*rv->records));
    pthread_mutex_unlock(&grow_mutex);
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
    free(rv);
}

void strncpytrm(char *dest, const char *src, size_t size){
    const char *p = src;
    char *q = dest;
    for(int i = 0; i < size; i++,q++,p++){
        char c = *p;
        switch(c){
            case '\0':
            case ' ':
                *q = '\0';
                return;
            default:
                *q = c;
                break;
        }
    }
    // Since RMN says that thinks like `FST_TYPVAR_LEN` *includes* space for the
    // terminating NUL byte, we can be pretty sure that this is not needed
    *--q = '\0'; // Unlikely
}

/*
 * Simple threading solution:  The global work is in file_queue which has
 * an array of filenames and an array of RecordVector's.  Each thread gets
 * a worker args which contains the address of the files_to_index struct and
 * a range of indices to work on.  For i in its given range, it fills rvs[i]
 * with the records of filenames[i].
 */
struct file_to_index {
    const char **filenames;
    RecordVector **rvs;
};
static pthread_mutex_t iun_lock = PTHREAD_MUTEX_INITIALIZER;

struct worker_args {
    struct file_to_index *q;
    int status;
    char message[512];
    int begin_index;
    int end_index;
};

void file_queue_worker(struct worker_args *args){
    for(int i = args->begin_index; i < args->end_index; i++){

        // fprintf(stderr, "HELLO\n");
        const char * filename = args->q->filenames[i];
        RecordVector *rv = args->q->rvs[i];

        pthread_mutex_lock(&iun_lock);
        fst_file *f = fst24_open(filename, NULL);
        pthread_mutex_unlock(&iun_lock);
        if(f == NULL){
            args->status = 1;
            snprintf(args->message, 512, "'%s': %s", filename, App_ErrorGet());
            pthread_exit((void*)1);
        }
        fst_query *q = fst24_new_query(f, &default_fst_record, NULL);
        fst_record result = default_fst_record;
        while(fst24_find_next(q, &result)){
            RecordVector_push(rv, &result);
        }

        fst24_query_free(q);
        fst24_close(f);
    }
    pthread_exit(NULL);
}

RecordData *rmn_get_index_columns_raw(const char **filenames, int nb_files)
{
    TApp_Timer t; App_TimerInit(&t);
    RecordData *raw_columns = NULL;
    int total_nb_records = 0;

    RecordVector *record_vectors[nb_files];
    for(int i = 0; i < nb_files; i++){
        record_vectors[i] = RecordVector_new(400);
    }

    struct file_to_index q = {
        .filenames = filenames,
        .rvs = record_vectors,
    };

    fprintf(stderr, "===> Collecting all records ...\n");
    App_TimerStart(&t);

#define min(a,b) ( ((a)<(b))?(a):(b) )
#define MULTITHREAD_INDEXING
#ifdef MULTITHREAD_INDEXING
    int nb_threads = 10;
    pthread_t thread_pool[nb_threads];
    struct worker_args thread_args[nb_threads];
    fprintf(stderr, "Creating %d worker threads\n", nb_threads);
    for(int i = 0; i < nb_threads; i++){
        thread_args[i] = (struct worker_args){
            .q = &q,
            .status = 0,
            .message = "",
            /*
             * Note: with cases like nb_files=322, nb_threads=20,
             * we can't pre-calculate (nb_files/nb_threads) outside
             * the loop because
             * (i+1)*(nb_files/nb_threads) < ((i+1)*nb_files)/nb_threads
             */
            .begin_index = i * nb_files / nb_threads,
            .end_index = (i+1) * nb_files / nb_threads,
        };
        fprintf(stderr, "Starting thread %d with index range [%d,%d)\n", i, thread_args[i].begin_index, thread_args[i].end_index);
        pthread_create(&thread_pool[i], NULL, (void *(*)(void*))file_queue_worker, (void*)&thread_args[i]);
    }

    fprintf(stderr, "Waiting for the threads\n");
    int any_thread_error = 0;
    for(int i = 0; i < nb_threads; i++){
        pthread_join(thread_pool[i], NULL);
    }
    for(int i = 0; i < nb_threads; i++){
        if(thread_args[i].status != 0){
            fprintf(stderr, "Worker %d ERROR: %s", i, thread_args[i].message);
            any_thread_error = 1;
        }
    }
    if(any_thread_error){
        goto error;
    }
#else
    for(int i = 0; i < nb_files; i++){
        fst_file *f = fst24_open(filenames[i], NULL);
        if(f == NULL){
            fprintf(stderr, "%s(): ERROR: '%s': %s", __func__, filenames[i], App_ErrorGet());
            goto error;
        }
        RecordVector *rv = record_vectors[i];
        fst_query *q = fst24_new_query(f, &default_fst_record, NULL);
        fst_record result = default_fst_record;
        while(fst24_find_next(q, &result)){
            RecordVector_push(rv, &result);
        }

        fst24_query_free(q);
        fst24_close(f);
    }

#endif
    App_TimerStop(&t);
    fprintf(stderr, ".... Collecting all records took %f ms\n", App_TimerTime_ms(&t));

    fprintf(stderr, "===> Reorganizing record data into columns for DataFrame\n");
    App_TimerInit(&t);
    App_TimerStart(&t);
    fprintf(stderr, "Summing number of records from each field: ");
    for(int f = 0; f < nb_files; f++){
        total_nb_records += record_vectors[f]->size;
    }
    fprintf(stderr, "%d\n", total_nb_records);


    fprintf(stderr, "Reorganizing data into column arrays\n");
    raw_columns = NewRecordData(total_nb_records);
    if(raw_columns == NULL){
        return NULL;

    }
    raw_columns->nb_records = total_nb_records; // TODO SHould be set in RecordDataNew obviously

    int i = 0;
    for(int f = 0; f < nb_files; f++){
        RecordVector *rv = record_vectors[f];
        const char *filename = filenames[f];
        for(size_t j = 0; j < rv->size; j++,i++){
            fst_record *r = &rv->records[j];

            raw_columns->ni[i] = r->ni;
            raw_columns->nj[i] = r->nj;
            raw_columns->nk[i] = r->nk;
            raw_columns->dateo[i] = r->dateo;
            raw_columns->deet[i] = r->deet;
            raw_columns->npas[i] = r->npas;
            raw_columns->pack_bits[i] = r->pack_bits;
            raw_columns->data_type[i] = r->data_type;

            raw_columns->ip1[i] = r->ip1;
            raw_columns->ip2[i] = r->ip2;
            raw_columns->ip3[i] = r->ip3;

            // Remove 'trm' if we want to keep the trailing spaces
            strncpy(raw_columns->typvar + i*FST_TYPVAR_LEN, r->typvar, FST_TYPVAR_LEN);
            strncpy(raw_columns->nomvar + i*FST_NOMVAR_LEN, r->nomvar, FST_NOMVAR_LEN);
            strncpy(raw_columns->etiket + i*FST_ETIKET_LEN, r->etiket, FST_ETIKET_LEN);
            strncpy(raw_columns->grtyp  + i*FST_GTYP_LEN, r->grtyp, FST_GTYP_LEN);
            // Discuss with JP how we can maintain the filepath association with
            // the records.
            strncpy(raw_columns->path + i*(PATH_MAX+1), filename, PATH_MAX);

            raw_columns->ig1[i] = r->ig1;
            raw_columns->ig2[i] = r->ig2;
            raw_columns->ig3[i] = r->ig3;
            raw_columns->ig4[i] = r->ig4;

            raw_columns->file_index[i] = r->file_index;
        }
    }
    App_TimerStop(&t);
    fprintf(stderr, ".... Reorganizing data into columns for DataFrame took %f ms\n", App_TimerTime_ms(&t));

error:
    fprintf(stderr, "Freeing record vectors\n");
    for(int f = 0; f< nb_files ; f++){
        RecordVector_FREE(record_vectors[f]);
    }
    return raw_columns;
}
