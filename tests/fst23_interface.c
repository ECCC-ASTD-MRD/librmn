
#include <math.h>
#include <stdio.h>
#include <string.h>

#include <rmn/fst_file.h>
#include <App.h>

const char* test_file_names[3] = {
    "fst23_interface.fst",
    "fst23_interface2.fst",
    "fst23_interface3.fst"
};

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
    test_record.ip2  = 10;
    test_record.ip3  = 100;
    strcpy(test_record.typvar, "P");
    strcpy(test_record.nomvar, "WAVE");
    strcpy(test_record.etiket, "float");
    strcpy(test_record.grtyp, "X");
    test_record.ig1   = 0;
    test_record.ig2   = 0;
    test_record.ig3   = 0;
    test_record.ig4   = 0;
    test_record.datyp = FST_TYPE_REAL;
    test_record.dasiz = 32;
}

int check_content(const float* content, const float* expected, const int num_elem) {
    for (int i = 0; i < num_elem; i++) {
        for (int j = 0; j < num_elem; j++) {
            if (content[i*num_elem + j] != expected[i * num_elem + j]) {
                App_Log(APP_ERROR, "AAAhhhh did not read the same that was put in!\n");
                return -1;
            }
        }
    }

    return 0;
}

int32_t create_file(const char* name, const int is_rsf, const int ip2, const int ip3) {
    remove(name);
    const char* options = is_rsf ? "RND+R/W+RSF" : "RND+R/W";
    fst_file* new_file = fst23_open(name, options);
    if (new_file == NULL) {
        App_Log(APP_ERROR, "Unable to open new test file with name %s and options %s\n", name, options);
        return -1;
    }

    /////////////////////////////////////
    // Write a record
    {
        fst_record record = test_record;
        record.ip2 = ip2;
        record.ip3 = ip3;

        if (fst23_write(new_file, &record, FALSE) < 0) {
            App_Log(APP_ERROR, "Unable to write record (1) to new file %s\n", name);
            return -1;
        }

        record.ip1++;
        if (fst23_write(new_file, &record, FALSE) < 0) {
            App_Log(APP_ERROR, "Unable to write record (2) to new file %s\n", name);
            return -1;
        }

        record.ip1++;
        if (fst23_write(new_file, &record, FALSE) < 0) {
            App_Log(APP_ERROR, "Unable to write record (3) to new file %s\n", name);
            return -1;
        }
    }

    ///////////////////////////
    // Close the new file
    if (fst23_close(new_file) < 0) {
        App_Log(APP_ERROR, "Unable to close new file %s\n", name);
        return -1;
    }

    free(new_file);

    return 0;
}

int test_fst23_interface(const int is_rsf) {
    ///////////////////////////////////
    // File creation
    if (create_file(test_file_names[0], is_rsf, test_record.ip2, test_record.ip3) < 0) return -1;

    /////////////////////////////
    // Open existing file
    const char* options2 = "RND+R/O";
    fst_file* test_file = fst23_open(test_file_names[0], options2);
    if (test_file == NULL) {
        App_Log(APP_ERROR, "Unable to open newly-created test file with name %s and options %s\n", test_file_names[0], options2);
        return -1;
    }

    {
        const int64_t num_rec = fst23_get_num_records(test_file);
        if (num_rec != 3) {
            App_Log(APP_ERROR, "Wrong number of records in test file! %d\n", num_rec);
            return -1;
        }
    }

    fst_record_fields fields = default_fields;
    // fields.datev = 1;
    fields.datestamps = 0;
    // fields.ip2 = 1;
    // fields.ip3 = 1;
    // fields.level = 1;
    // fields.deet = 1;
    // fields.npas = 1;
    // fields.decoded_ip = 1;
    fields.datyp = 1;
    fst23_print_summary(test_file, &fields);

    fst_record record;
    fst_record expected = test_record;

    ///////////////////////////////////////////////
    // Find next + read
    int num_found = 0;
    fst23_set_search_criteria(test_file, &default_fst_record);
    int64_t keys[3];
    while (fst23_find_next(test_file, &record)) {
        // fst23_record_print(&record);
        keys[num_found] = record.handle;
        num_found++;

        expected.handle = record.handle; // Determined at read time
        expected.dasiz = record.dasiz;   // Determined at write time
        expected.ip1 = num_found;
        if (!fst23_record_is_same(&record, &expected)) {
            App_Log(APP_ERROR, "Record read from file is not identical to the one written! (num_found = %d)\n", num_found);
            fst23_record_print(&record);
            fst23_record_diff(&record, &expected);
            return -1;
        }

        // App_Log(APP_WARNING, "Found a record\n");
        // fst23_record_print(&record);
        if (fst23_read(test_file, &record) <= 0) {
            App_Log(APP_ERROR, "Could not read data from record!\n");
            fst23_record_print(&record);
            return -1;
        }

        if (check_content(record.data, test_data, DATA_SIZE) < 0) return -1;

        if (!fst23_file_is_open(test_file)) {
            App_Log(APP_ERROR, "AAAhhhh file is no longer open!!\n");
            return -1;
        }
    }

    if (num_found != 3) {
        App_Log(APP_ERROR, "Found only %d of the 3 records we wrote!\n", num_found);
        return -1;
    }

    ////////////////////////////////////////////////////////////
    // Read next
    fst23_set_search_criteria(test_file, &default_fst_record); // Rewind search
    num_found = 0;
    App_Log(APP_INFO, "Reading again, with read_next\n");
    while (fst23_read_next(test_file, &record) > 0) {
        num_found++;

        expected.handle = record.handle; // Determined at read time
        expected.dasiz = record.dasiz;   // Determined at write time
        expected.ip1 = num_found;
        if (!fst23_record_is_same(&record, &expected)) {
            App_Log(APP_ERROR, "Record read from file is not identical to the one written! (num_found = %d)\n", num_found);
            fst23_record_print(&record);
            fst23_record_diff(&record, &expected);
            return -1;
        }

        if (check_content(record.data, test_data, DATA_SIZE) < 0) return -1;
    }
    
    if (num_found != 3) {
        App_Log(APP_ERROR, "Should have read 3 records (not %d)\n", num_found);
        return -1;
    }

    ///////////////////////////////////////////////
    // Find all
    App_Log(APP_INFO, "Testing find_all\n");
    fst_record all_records[5];
    num_found = fst23_find_all(test_file, all_records, 1);

    if (num_found != 1) {
        App_Log(APP_ERROR, "Find all with a max of 1 actually found %d record(s)\n", num_found);
        return -1;
    }

    expected.handle = all_records[0].handle; // Determined at read time
    expected.dasiz = all_records[0].dasiz;   // Determined at write time
    expected.ip1 = 1;

    if (!fst23_record_is_same(&all_records[0], &expected)) {
        App_Log(APP_ERROR, "Record read from file is not identical to the one written! (num_found = %d)\n", num_found);
        fst23_record_print(&all_records[0]);
        fst23_record_diff(&all_records[0], &expected);
        return -1;
    }

    num_found = fst23_find_all(test_file, all_records, 5);

    if (num_found != 3) {
        App_Log(APP_ERROR, "Find all with a max of 5 actually found %d record(s)\n", num_found);
        return -1;
    }

    for (int i = 0; i < 3; i++) {
        expected.handle = all_records[i].handle; // Determined at read time
        expected.dasiz = all_records[i].dasiz;   // Determined at write time
        expected.ip1 = i + 1;

        if (!fst23_record_is_same(&all_records[i], &expected)) {
            App_Log(APP_ERROR, "Record read from file is not identical to the one written! (num_found = %d)\n", num_found);
            fst23_record_print(&all_records[i]);
            fst23_record_diff(&all_records[i], &expected);
            return -1;
        }
    }

    /////////////////////////////////////////
    // Everything again, with linked files
    fst_file* file_list[3];
    file_list[0] = test_file;

    const int status1 = create_file(test_file_names[1], !is_rsf, test_record.ip2 + 1, test_record.ip3 + 1);
    const int status2 = create_file(test_file_names[2], is_rsf, test_record.ip2 + 2, test_record.ip3 + 1);

    if (status1 < 0 || status2 < 0) {
        App_Log(APP_ERROR, "Unable to create other files for link tests\n");
        return -1;
    }

    file_list[1] = fst23_open(test_file_names[1], options2);
    file_list[2] = fst23_open(test_file_names[2], options2);

    if (file_list[1] == NULL || file_list[2] == NULL) {
        App_Log(APP_ERROR, "Unable to create other files for link tests\n");
        return -1;
    }

    if (fst23_link_files(file_list, 1)) {
        App_Log(APP_ERROR, "Should not succeed linking only 1 file\n");
        return -1;
    }

    if (!fst23_link_files(file_list, 3)) {
        App_Log(APP_ERROR, "Error trying to link 3 files\n");
        return -1;
    }

    if (fst23_link_files(file_list, 2)) {
        App_Log(APP_ERROR, "Should not succeed linking already-linked file\n");
        return -1;
    }

    {
        const int64_t num_rec = fst23_get_num_records(test_file);
        if (num_rec != 9) {
            App_Log(APP_ERROR, "Wrong number of records in test file! %d\n", num_rec);
            return -1;
        }
    }

    {
        fst_record criteria = default_fst_record;
        fst_record result;
        fst_record results[10];

        // Should find the 3 records in the second file only
        criteria.ip2 = test_record.ip2 + 1;
        fst23_set_search_criteria(test_file, &criteria);
        num_found = 0;
        App_Log(APP_INFO, "Looking for 3 records (should be in second file)\n");
        while (fst23_find_next(test_file, &result)) {
            num_found++;

            expected.handle = result.handle; // Determined at read time
            expected.dasiz = result.dasiz;   // Determined at write time
            expected.ip1 = num_found;
            expected.ip2 = test_record.ip2 + 1;
            expected.ip3 = test_record.ip3 + 1;
            if (!fst23_record_is_same(&result, &expected)) {
                App_Log(APP_ERROR, "Record read from file is not identical to the one written! (num_found = %d)\n", num_found);
                fst23_record_print(&result);
                fst23_record_diff(&result, &expected);
                return -1;
            }
        }

        if (num_found != 3) {
            App_Log(APP_ERROR, "Should have found 3 records in linked list instead of %d\n", num_found);
            return -1;
        }

        if (fst23_find_all(test_file, results, 10) != 3) {
            App_Log(APP_ERROR, "Find all should have found 3\n");
            return -1;
        }

        // Should find the 6 records in second + third file
        criteria = default_fst_record;
        criteria.ip3 = test_record.ip3 + 1;
        fst23_set_search_criteria(test_file, &criteria);
        num_found = 0;
        App_Log(APP_INFO, "Looking for 6 records (should be in second + third files)\n");
        while (fst23_find_next(test_file, &result)) {
            num_found++;
            if (fst23_read(test_file, &result) <= 0) {
                App_Log(APP_ERROR, "Unable to read record from linked files\n");
                fst23_record_print(&result);
                return -1;
            }
        }

        if (num_found != 6) {
            App_Log(APP_ERROR, "Should have found 6 records in linked list instead of %d\n", num_found);
            return -1;
        }

        if (fst23_find_all(test_file, results, 10) != 6) {
            App_Log(APP_ERROR, "Find all should have found 3\n");
            return -1;
        }

        fst23_set_search_criteria(test_file, &default_fst_record); // Rewind search
        num_found = 0;
        while (fst23_read_next(test_file, &record) > 0) {
            num_found++;
        }
        if (num_found != 9) {
            App_Log(APP_ERROR, "Read next should have read 9\n");
            return -1;
        }
    }

    if (!fst23_unlink_files(test_file)) {
        App_Log(APP_ERROR, "Error unlinking 3 files\n");
        return -1;
    }

    if (fst23_close(test_file) < 0 || fst23_close(file_list[1]) < 0 || fst23_close(file_list[2]) < 0) {
        App_Log(APP_ERROR, "Unable to close file %s\n", test_file_names[0]);
        return -1;
    }

    App_Log(APP_INFO, "A few calls that should fail\n");
    if (fst23_link_files(file_list, 3)) {
        App_Log(APP_ERROR, "Should not be able to link closed files\n");
        return -1;
    }

    if (fst23_unlink_files(test_file)) {
        App_Log(APP_ERROR, "Should not be able to unlink closed file\n");
        return -1;
    }

    if (fst23_find_next(test_file, &record)) {
        App_Log(APP_ERROR, "Should not be able to search a closed file\n");
        return -1;
    }

    if (fst23_read_next(test_file, &record) > 0) {
        App_Log(APP_ERROR, "Should not be able to search a closed file\n");
        return -1;
    }

    if (fst23_set_search_criteria(test_file, &record)) {
        App_Log(APP_ERROR, "Should not be able to set search criteria on a closed file\n");
        return -1;
    }

    if (fst23_close(test_file) == 0) {
        App_Log(APP_ERROR, "Should not be able to close closed file\n");
        return -1;
    }

    if (fst23_get_num_records(test_file) > 0) {
        App_Log(APP_ERROR, "Should not be able to get num records of closed file\n");
        return -1;
    }

    free(test_file);

    return 0;
}

int main(void) {

    make_test_record();

    App_Log(APP_INFO, "Testing RSF\n");
    if (test_fst23_interface(1) != 0) return -1; // RSF files

    // App_Log(APP_INFO, "Testing XDF\n");
    // if (test_fst23_interface(0) != 0) return -1; // XDF files

    delete_test_data();

    App_Log(APP_INFO, "Tests successful\n");
    return 0;
}
