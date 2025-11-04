#include <math.h>

#include <omp.h>

#include <App.h>
#include <rmn.h>

const char* test_filename = "parallel.fst";

const size_t NUM_ELEM = 1000;

void gen_data(float* buffer, const int size_x, const int rank, const int record_count) {
    for (int i = 0; i < size_x; i++) {
        const float powi = pow((1.0 * i / size_x), 2);
        for (int j = 0; j < size_x; j++) {
            const float powj = pow((1.0 * j / size_x), 2);
            const float numerator = 1 + cos(12 * sqrt( powi + powj ) + record_count);
            const float denominator = 0.5 * ( powi + powj ) + 2;
            buffer[i * size_x + j] = numerator / denominator + rank * 10.0;
            // buffer[i * size_x + j] = 1.0;
        }
    }
}

void print_data(const float* array, const int size_x, const char* name) {
    char buffer[1024];
    char* ptr = buffer;
    const int col_size = 8;
    const char format[] = {'%', '0' + col_size - 1, '.', '3', 'f', ' ', '\0'};
    const int num_cols = 12;
    App_Log(APP_ALWAYS, "%s:\n", name);
    for (int i = 0; i < size_x && i < 10; i++) {
        ptr += sprintf(ptr, "%3d: ", i);
        for (int j = 0; j < num_cols; j++) {
            ptr += sprintf(ptr, format, array[i * size_x + j]);
        }
        if (num_cols < size_x) ptr += sprintf(ptr, " ...");
        App_Log(APP_VERBATIM, "%s\n", buffer);
        ptr = buffer;
    }
}

//! \return 0 if the two arrays are identical, -1 if not
int compare_data(const float* a, const float* b, const int64_t num_elements) {
    for (int64_t i = 0; i < num_elements; i++) {
        if (a[i] != b[i]) return -1;
    }
    return 0;
}

int test_parallel_write() {
    remove(test_filename);

    const char* options = "RSF+R/W+PARALLEL";
    int num_errors = 0;
    int num_records = 0;

#pragma omp parallel num_threads(4) shared(num_errors, num_records, options)
    {
        #pragma omp single
        {
            App_Log(APP_ALWAYS, "%s: Using %d threads\n", __func__, omp_get_num_threads());
        }

        const int thread_id = omp_get_thread_num();

        float* data = (float*)malloc(NUM_ELEM * NUM_ELEM * sizeof(float));
        if (data == NULL) {
            App_Log(APP_ERROR, "Could not malloc\n");
            #pragma omp atomic
            num_errors++;
        }

        fst_file* test_file = fst24_open(test_filename, options);
        if (test_file == NULL) {
            App_Log(APP_ERROR, "Unable to open new test file with name %s and options %s\n", test_filename, options);
            #pragma omp atomic
            num_errors++;
        }

        fst_record rec = default_fst_record;
        rec.data = data;
        rec.data_type = FST_TYPE_REAL_IEEE;
        rec.data_bits = 32;
        rec.pack_bits = 32;
        rec.ni = NUM_ELEM;
        rec.nj = NUM_ELEM;
        rec.nk = 1;

        rec.dateo = 0;
        rec.datev = 0;

        rec.deet = 300;
        rec.npas = 0;
        rec.ip1 = 1;
        rec.ip2 = thread_id;
        rec.ip3 = 0;

        rec.ig1 = 0;
        rec.ig2 = 0;
        rec.ig3 = 0;
        rec.ig4 = 0;

        strcpy(rec.typvar, "P");
        strcpy(rec.nomvar, "WAVE");
        strcpy(rec.etiket, "float");
        strcpy(rec.grtyp, "X");

#pragma omp barrier
        for (int i = 0; i < 5; i++) {
            gen_data(data, NUM_ELEM, thread_id, i);
            rec.ip3 = i;
            if (fst24_write(test_file, &rec, FST_NO) <= 0) {
                App_Log(APP_ERROR, "Unable to write record in parallel\n");
                #pragma omp atomic
                num_errors++;
            }
            #pragma omp atomic
            num_records++;
        }

        fst24_close(test_file);
        test_file = NULL;

        if (data != NULL) free(data);
    } // end omp parallel

    if (num_errors > 0) {
        App_Log(APP_ERROR, "%s: There have been %d errors\n", __func__, num_errors);
        return -1;
    }

    fst_file* test_file = fst24_open(test_filename, NULL);
    if (test_file == NULL) {
        App_Log(APP_ERROR, "Unable to open file after it was closed\n");
        return -1;
    }

    fst24_print_summary(test_file, NULL);

    const int64_t num_rec = fst24_get_num_records(test_file);
    if (num_rec != num_records) {
        App_Log(APP_ERROR, "%s: Wrote %d records, but there are only %d in the file!\n",
                __func__, num_records, num_rec);
        return -1;
    }
    int64_t num_read = 0;
    // fst24_set_search_criteria(test_file, &default_fst_record);
    float* expected_data = (float*)malloc(NUM_ELEM * NUM_ELEM * sizeof(float));
    if (expected_data == NULL) {
        App_Log(APP_ERROR, "Could not malloc expected data\n");
        return -1;
    }

    fst_record read_rec = default_fst_record;
    fst_query* query = fst24_new_query(test_file, NULL, NULL);
    while (fst24_read_next(query, &read_rec)) {
        num_read++;

        gen_data(expected_data, NUM_ELEM, read_rec.ip2, read_rec.ip3);
        if (compare_data((float*)read_rec.data, expected_data, NUM_ELEM * NUM_ELEM) < 0) {
            App_Log(APP_ERROR, "Data is not the same!\n");
            fst24_record_print(&read_rec);
            print_data(expected_data, NUM_ELEM, "expected");
            print_data((float*)read_rec.data, NUM_ELEM, "actual");
            return -1;
        }

    }

    free(expected_data);

    if (num_read != num_rec) {
        App_Log(APP_ERROR, "Num records read is not the same as num in file (%ld vs %ld)\n", num_read, num_rec);
        return -1;
    }

    fst24_record_free(&read_rec);
    fst24_query_free(query);
    fst24_close(test_file);
    test_file = NULL;

    return 0;
}

int main(int argc, char* argv[]) {

    if (test_parallel_write() != 0) return -1;

    App_Log(APP_ALWAYS, "Test successful\n");

    return 0;
}

