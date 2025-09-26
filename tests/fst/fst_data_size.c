
#include <App.h>
#include <rmn.h>

const char* filename_rsf = "data_size.rsf";
const char* filename_xdf = "data_size.xdf";

const int NUM_ELEM = 1000; // Must be large enough to cause a crash when reading

static int create_file(const int is_rsf) {
    const char* filename = is_rsf ? filename_rsf : filename_xdf;
    const char* options = is_rsf ? "RSF+R/W" : "XDF+R/W";

    remove(filename);

    fst_file* test_file = fst24_open(filename, options);
    if (test_file == NULL) {
        App_Log(APP_ERROR, "%s: Unable to open file %s (create)\n", __func__, filename);
        return -1;
    }

    void* dummy_data = malloc(NUM_ELEM * sizeof(double));

    fst_record rec = default_fst_record;
    rec.data_type = FST_TYPE_REAL;
    rec.data_bits = 64;
    rec.pack_bits = 64;
    rec.data = (void*)dummy_data;

    rec.ni = 1;
    rec.nj = 1;
    rec.nk = 1;

    rec.ip1 = 0;
    rec.ip2 = 0;
    rec.ip3 = 0;
    rec.ig1 = 0;
    rec.ig2 = 0;
    rec.ig3 = 0;
    rec.ig4 = 0;

    rec.deet = 0;
    rec.npas = 0;

    // Write doubles (64, packed to 64) with fst24
    if (fst24_write(test_file, &rec, FST_NO) != TRUE) {
        App_Log(APP_ERROR, "%s: Unable to write fst24 record\n", __func__);
        return -1;
    }

    // Write doubles (64, packed to 64) with fst98
    if (c_fstecr(dummy_data, NULL, -64, fst24_get_unit(test_file), 0, 0, 0, 1, 1, 1, 0, 0, 0,
                 "XX", "XXXX", "ETIKET", "X", 0, 0, 0, 0, FST_TYPE_REAL, 0) != 0) {
        App_Log(APP_ERROR, "%s: Unable to write fst98 record\n", __func__);
        return -1;
    }

    // Try to write floats, 32 packed to 64, with fst24 (not possible with fst98)
    rec.data_bits = 32;
    rec.ni = NUM_ELEM; // MUst be large enough to cause a crash when reading
    strcpy(rec.etiket, "BAD_SIZE");
    if (fst24_write(test_file, &rec, FST_NO) != TRUE) {
        App_Log(APP_ERROR, "%s: Unable to write fst24 record\n", __func__);
        return -1;
    }

    if (rec.pack_bits != rec.data_bits) {
        App_Log(APP_ERROR, "%s: pack_bits (= %d) should have been switched to %d (same size as input data)\n",
            __func__, rec.pack_bits, rec.data_bits);
        return -1;
    }

    if (fst24_close(test_file) != TRUE) {
        App_Log(APP_ERROR, "%s: Unable to close file properly\n", __func__);
        return -1;
    }

    test_file = fst24_open(filename, "R/W");
    if (test_file == NULL) {
        App_Log(APP_ERROR, "%s: Unable to open file %s (modify)\n", __func__, filename);
        return -1;
    }
    
    fst_record crit = default_fst_record;
    strcpy(crit.etiket, "BAD_SIZE");
    fst_query* q = fst24_new_query(test_file, &crit, NULL);
    if (q == NULL) {
        App_Log(APP_ERROR, "%s: Unable to create query\n", __func__);
        return -1;
    }
    fst24_find_next(q, &rec);
    rec.pack_bits = 64;
    if (fst24_write(test_file, &rec, FST_META) != TRUE) {
        App_Log(APP_ERROR, "%s: Unable to modify record pack_bits\n", __func__);
        return -1;
    }

    fst24_query_free(q);
    fst24_record_free(&rec);
    free(dummy_data);

    if (fst24_close(test_file) != TRUE) {
        App_Log(APP_ERROR, "%s: Unable to close file properly\n", __func__);
        return -1;
    }

    return 0;
}

static int run_test(const int is_rsf) {
    App_Log(APP_ALWAYS, "Testing %s\n", is_rsf ? "RSF" : "XDF");
    if (create_file(is_rsf) != 0) {
        return -1;
    }

    const char* filename = is_rsf ? filename_rsf : filename_xdf;

    fst_file* test_file = fst24_open(filename, NULL);
    if (test_file == NULL) {
        App_Log(APP_ERROR, "%s: Unable to open file %s (modify)\n", __func__, filename);
        return -1;
    }
    
    fst_query* q = fst24_new_query(test_file, NULL, NULL);
    if (q == NULL) {
        App_Log(APP_ERROR, "%s: Unable to create empty query\n", __func__);
        return -1;
    }

    fst_record all_recs[10];
    const int num_found = fst24_find_all(q, all_recs, 10);
    App_Log(APP_ALWAYS, "Found %d\n", num_found);

    for (int i = 0; i < num_found; i++) {
        // fst24_record_print(&all_recs[i]);
        fst24_read_record(&all_recs[i]);
        fst24_record_free(&all_recs[i]);
    }

    fst24_query_free(q);

    if (fst24_close(test_file) != TRUE) {
        App_Log(APP_ERROR, "%s: Unable to close file\n", __func__);
        return -1;
    }

    return 0;
}

int main(void) {
    if (run_test(1) != 0) return -1;
    if (run_test(0) != 0) return -1;
    return 0;
}
