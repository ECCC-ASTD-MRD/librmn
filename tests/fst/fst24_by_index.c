
#include <App.h>
#include <rmn.h>


const int NUM_RECORDS = 10000;

int do_test(const char* backend) {
    App_Log(APP_ALWAYS, "Testing %s\n", backend);

    char test_filename[64];
    snprintf(test_filename, sizeof(test_filename), "by_index.%s", backend);
    remove(test_filename);

    char options[256];
    sprintf(options, "%s+R/W", backend);

    fst_file* test_file = fst24_open(test_filename, options);

    if (test_file == NULL) {
        App_Log(APP_ERROR, "Unable to create test file\n");
        return -1;
    }

    float test_data[1];
    fst_record input_rec = default_fst_record;
    input_rec.data = test_data;
    input_rec.pack_bits = 32;
    input_rec.ni   = 1;
    input_rec.nj   = 1;
    input_rec.nk   = 1;
    input_rec.dateo= 458021600;
    input_rec.deet = 300;
    input_rec.npas = 0;
    input_rec.ip1  = 0;
    input_rec.ip2  = 10;
    input_rec.ip3  = 100;
    strcpy(input_rec.typvar, "P");
    strcpy(input_rec.nomvar, "WAVE");
    strcpy(input_rec.etiket, "float");
    strcpy(input_rec.grtyp, "X");
    input_rec.ig1   = 0;
    input_rec.ig2   = 0;
    input_rec.ig3   = 0;
    input_rec.ig4   = 0;
    input_rec.data_type = FST_TYPE_REAL_IEEE;
    input_rec.data_bits = 32;
    input_rec.metadata = NULL;

    for (int i = 0; i < NUM_RECORDS; i++) {
        if (fst24_write(test_file, &input_rec, FST_NO) < 0) {
            App_Log(APP_ERROR, "Unable to write record %d\n", i);
            return -1;
        }
        input_rec.ip1++;
    }

    fst_record rec = default_fst_record;
    for (int i = 0; i < NUM_RECORDS; i++) {
        if (fst24_get_record_by_index(test_file, i, &rec) <= 0) {
            App_Log(APP_ERROR, "Unable to read record %d\n", i);
            return -1;
        }

        if (rec.ip1 != i) {
            App_Log(APP_ERROR, "Got the wrong record (%d)!\n", i);
            fst24_record_print(&rec);
            return -1;
        }
    }

    if (!fst24_close(test_file)) {
        App_Log(APP_ERROR, "Unable to close test file \n");
        return -1;
    }

    return 0;
}

int main(void) {

    if (do_test("RSF") < 0) return -1;
    if (do_test("XDF") < 0) return -1;

    App_Log(APP_ALWAYS, "Test successful\n");

    return 0;
}
