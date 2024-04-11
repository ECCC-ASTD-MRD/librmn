
#include <omp.h>

#include <App.h>
#include <rmn/fst24_file.h>

static const char* test_filename = "multithread.fst";

float gen_data(const int index, const int iteration, const int thread_id, const int num_threads) {
    return  (float)(index * thread_id + iteration) / num_threads;
}

int main(void) {

    const int NUM_IT = 100;
    const int NUM_ELEM = 1000;
    const int NUM_THREADS = 8;

    remove(test_filename);

    fst_file* test_file = fst24_open(test_filename, "R/W+RSF");
    if (test_file == NULL) {
        App_Log(APP_ERROR, "Unable to open test file for writing\n");
        return -1;
    }

    int num_errors = 0;

    #pragma omp parallel num_threads(NUM_THREADS) default(shared) reduction(+:num_errors)
    {
        App_Log(APP_INFO, "This is thread %d/%d\n",  omp_get_thread_num(), omp_get_num_threads());

        fst_record rec = default_fst_record;

        float rec_data[NUM_ELEM];

        rec.data = rec_data;
        rec.ni = NUM_ELEM;
        rec.nj = 1;
        rec.nk = 1;
        rec.datyp = FST_TYPE_REAL;
        rec.dasiz = 32;
        rec.npak = -32;

        rec.dateo = 0;
        rec.deet = 0;
        rec.npas = 0;

        rec.ip1 = omp_get_thread_num();
        rec.ip2 = 1;
        rec.ip3 = 1;

        rec.ig1 = 0;
        rec.ig2 = 0;
        rec.ig3 = 0;
        rec.ig4 = 0;

        for (int i = 0; i < NUM_IT; i++) {

            rec.ip2++;
            for (int j = 0; j < NUM_ELEM; j++) {
                rec_data[j] = gen_data(j, rec.ip2, rec.ip1, NUM_THREADS);
            }

            if (fst24_write(test_file, &rec, 0) < 0) {
                App_Log(APP_ERROR, "Unable to write record\n");
                num_errors++;
                break;
            }
        }
    }

    if (num_errors > 0) {
        App_Log(APP_ERROR, "There were %d errors (writing)\n", num_errors);
        return -1;
    }

    fst24_close(test_file);
    free(test_file);

    test_file = fst24_open(test_filename, NULL);

    #pragma omp parallel num_threads(NUM_THREADS) default(shared) reduction(+:num_errors)
    {
        const int thread_id   = omp_get_thread_num();

        fst_record rec = default_fst_record;
        fst_record criteria = default_fst_record;

        criteria.ip1 = thread_id;

        fst_query* query = fst24_new_query(test_file, &criteria, NULL);
        for (int i = 0; i < NUM_IT; i++) {
        // for (int i = 0; i < 1; i++) {
            if (fst24_read_next(query, &rec) <= 0) {
                App_Log(APP_ERROR, "Unable to read record\n");
                num_errors++;
                break;
            }

            for (int j = 0; j < NUM_ELEM; j++) {
                if (((float*)rec.data)[j] != gen_data(j, rec.ip2, rec.ip1, NUM_THREADS)) {
                    App_Log(APP_ERROR, "We have some wrong data!\n");
                    num_errors++;
                    break;
                }
            }
        }

        fst24_record_free(&rec);
        fst24_query_free(query);
    }

    if (num_errors > 0) {
        App_Log(APP_ERROR, "There were %d errors (reading)\n", num_errors);
        return -1;
    }

    fst24_close(test_file);
    free(test_file);

    App_Log(APP_INFO, "Test successful.\n");

    remove(test_filename); // This is a big file, so better remove it

    return 0;
}
