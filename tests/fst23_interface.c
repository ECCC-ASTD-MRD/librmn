
#include <math.h>
#include <stdio.h>
#include <string.h>

#include <rmn/fst_file.h>
#include <App.h>

const char* test_file_name = "fst23_interface.fst";

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
        fst_record record = default_fst_record;
        record.data = data;
        record.npak = -32;
        record.ni   = DATA_SIZE;
        record.nj   = DATA_SIZE;
        record.nk   = 1;
        record.dateo= 20220610;
        record.deet = 300;
        record.npas = 0;
        record.ip1  = 1;
        record.ip2  = 1;
        record.ip3  = 1;
        strcpy(record.typvar, "P");
        strcpy(record.nomvar, "WAVE");
        strcpy(record.etiket, "float");
        strcpy(record.grtyp, "X");
        record.ig1   = 0;
        record.ig2   = 0;
        record.ig3   = 0;
        record.ig4   = 0;
        record.datyp = FST_TYPE_FLOAT;

        char *meta= "testing metadata";
        record.metadata = meta;

        if (fst23_write(test_file, &record,FALSE) < 0) {
            App_Log(APP_ERROR, "Unable to write record (1) to new file %s\n", test_file_name);
            return -1;
        }

        record.ip3 = 2;
        if (fst23_write(test_file, &record,FALSE) < 0) {
            App_Log(APP_ERROR, "Unable to write record (2) to new file %s\n", test_file_name);
            return -1;
        }

        record.ip3 = 3;
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
    int num_found = 0;
    fst23_set_search_criteria(test_file, &default_fst_record);
    while (fst23_find_next(test_file, &record)) {
        num_found++;
        fst23_record_print(&record);
    }

    if (num_found != 3) {
        App_Log(APP_ERROR, "Found %d of the 3 records we wrote!\n", num_found);
        return -1;
    }

    const int64_t old_handle = record.handle;
    record = fst23_read(test_file, record.handle);
    if (record.handle != old_handle) {
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

    App_Log(APP_INFO, "Testing RSF\n");
    if (test_fst23_interface(1) != 0) return -1; // RSF files

    App_Log(APP_INFO, "Testing XDF\n");
    if (test_fst23_interface(0) != 0) return -1; // XDF files

    App_Log(APP_INFO, "Tests successful\n");
    return 0;
}
