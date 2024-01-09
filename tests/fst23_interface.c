
#include <math.h>
#include <stdio.h>
#include <string.h>

#include <rmn/fst_file.h>
#include <App.h>

const char* test_file_name = "fst23_interface.fst";
const char* test_meta = "testing metadata";

const int DATA_SIZE = 1024;
float* test_data = NULL;
fst_record test_record;

void make_test_data() {
    if (test_data == NULL) {
        test_data = malloc(DATA_SIZE * DATA_SIZE * sizeof(float));
        if (test_data == NULL) { fprintf(stderr, "malloc failed\n"); exit(-1); }
        for (int i = 0; i < DATA_SIZE; i++) {
            for (int j = 0; j < DATA_SIZE; j++) {
                const float powi = pow((1.0 * i / DATA_SIZE), 2);
                const float powj = pow((1.0 * j / DATA_SIZE), 2);
                const float numerator = 1 + cos(12 * sqrt( powi + powj ));
                const float denominator = 0.5 * ( powi + powj ) + 2;
                test_data[i * DATA_SIZE + j] = numerator / denominator;
            }
        }
    }
}

void delete_test_data() {
    if (test_data != NULL) {
        free(test_data);
        test_data = NULL;
    }
}

void make_test_record() {
    make_test_data();
    test_record = default_fst_record;
    test_record.data = test_data;
    test_record.npak = -32;
    test_record.ni   = DATA_SIZE;
    test_record.nj   = DATA_SIZE;
    test_record.nk   = 1;
    test_record.dateo= 20220610;
    test_record.deet = 300;
    test_record.npas = 0;
    test_record.ip1  = 1;
    test_record.ip2  = 1;
    test_record.ip3  = 1;
    strcpy(test_record.typvar, "P");
    strcpy(test_record.nomvar, "WAVE");
    strcpy(test_record.etiket, "float");
    strcpy(test_record.grtyp, "X");
    test_record.ig1   = 0;
    test_record.ig2   = 0;
    test_record.ig3   = 0;
    test_record.ig4   = 0;
    test_record.datyp = FST_TYPE_FLOAT;

    test_record.metadata = test_meta;
}

int test_fst23_interface(const int is_rsf) {

    // Create some arbitrary data field
    const int DATA_SIZE = 1024;
    float data[DATA_SIZE][DATA_SIZE];
    for (int i = 0; i < DATA_SIZE; i++) {
        for (int j = 0; j < DATA_SIZE; j++) {
            const float powi = pow((1.0 * i / DATA_SIZE), 2);
            const float powj = pow((1.0 * j / DATA_SIZE), 2);
            const float numerator = 1 + cos(12 * sqrt( powi + powj ));
            const float denominator = 0.5 * ( powi + powj ) + 2;
            data[i][j] = numerator / denominator;
            // printf("domain[%d][%d] = %f\n", i, j, domain[i][j]);
        }
    }

    // Create file
    remove(test_file_name);
    const char* options1 = is_rsf ? "RND+R/W+RSF" : "RND+R/W";
    fst_file* test_file = fst23_open(test_file_name, options1);
    if (test_file == NULL) {
        App_Log(APP_ERROR, "Unable to open new test file with name %s and options %s\n", test_file_name, options1);
        return -1;
    }

    // Write a record
    {
        fst_record record = test_record;

        if (fst23_write(test_file, &record,FALSE) < 0) {
            App_Log(APP_ERROR, "Unable to write record (1) to new file %s\n", test_file_name);
            return -1;
        }

        record.ip3++;
        if (fst23_write(test_file, &record,FALSE) < 0) {
            App_Log(APP_ERROR, "Unable to write record (2) to new file %s\n", test_file_name);
            return -1;
        }

        record.ip3++;
        if (fst23_write(test_file, &record,FALSE) < 0) {
            App_Log(APP_ERROR, "Unable to write record (3) to new file %s\n", test_file_name);
            return -1;
        }
    }

    // Close the new file
    if (fst23_close(test_file) < 0) {
        App_Log(APP_ERROR, "Unable to close new file %s\n", test_file_name);
        return -1;
    }

    // Open existing file
    const char* options2 = "RND+R/O";
    test_file = fst23_open(test_file_name, options2);
    if (test_file == NULL) {
        App_Log(APP_ERROR, "Unable to open newly-created test file with name %s and options %s\n", test_file_name, options2);
        return -1;
    }

    fst_record record;
    fst_record expected = test_record;
    int num_found = 0;
    fst23_set_search_criteria(test_file, &default_fst_record);
    int64_t keys[3];
    while (fst23_find_next(test_file, &record)) {
        // fst23_record_print(&record);
        keys[num_found] = record.handle;
        num_found++;

        expected.handle = record.handle; // Determined at read tim
        expected.dasiz = record.dasiz;   // Determine at write time
        expected.ip3 = num_found;
        if (!fst23_record_is_same(&record, &expected)) {
            App_Log(APP_ERROR, "Record read from file is not identical to the one written! (num_found = %d)\n", num_found);
            fst23_record_print(&record);
            fst23_record_diff(&record, &expected);
            return -1;
        }
    }

    if (num_found != 3) {
        App_Log(APP_ERROR, "Found %d of the 3 records we wrote!\n", num_found);
        return -1;
    }

    record = fst23_read(test_file, keys[0]);
    if (record.handle != keys[0]) {
        App_Log(APP_ERROR, "Could not read the content of the record (that we previously found!)\n");
        return -1;
    }

    for (int i = 0; i < DATA_SIZE; i++) {
        for (int j = 0; j < DATA_SIZE; j++) {
            if (((float*)record.data)[i*DATA_SIZE + j] != data[i][j]) {
                App_Log(APP_ERROR, "AAAhhhh did not read the same that was put in!\n");
                return -1;
            }
        }
    }

    if (fst23_close(test_file) < 0) {
        App_Log(APP_ERROR, "Unable to close file %s\n", test_file_name);
        return -1;
    }

    free(test_file);

    return 0;
}

int main(void) {

    make_test_record();

    App_Log(APP_INFO, "Testing RSF\n");
    if (test_fst23_interface(1) != 0) return -1; // RSF files

    App_Log(APP_INFO, "Testing XDF\n");
    if (test_fst23_interface(0) != 0) return -1; // XDF files

    delete_test_data();

    App_Log(APP_INFO, "Tests successful\n");
    return 0;
}
