#include <App.h>
#include <rmn.h>

const char* filename_rsf = "large_record.rsf";
const char* filename_xdf = "large_record.xdf";

const int32_t BIG_NI = 1 << 30;
const int32_t BIG_NJ = BIG_NI;
const int32_t MEDIUM_NJ = 1 << 26;

int create_file(const int is_rsf) {
    const char* filename = is_rsf ? filename_rsf : filename_xdf;
    const char* options = is_rsf ? "RSF+R/W" : "XDF+R/W";
    remove(filename);

    fst_file* test_file = fst24_open(filename, options);

    if (test_file == NULL) {
        App_Log(APP_ERROR, "%s: Unable to open file '%s' (create)\n", __func__, filename);
        return -1;
    }

    void* dummy_data = malloc(BIG_NI / 8);
    memset(dummy_data, 0, BIG_NI / 8);

    fst_record rec = default_fst_record;
    rec.data = dummy_data;
    rec.data_type = FST_TYPE_BINARY;
    rec.data_bits = 1;
    rec.pack_bits = 1;

    rec.ni = BIG_NI;
    rec.nj = 1;
    rec.nk = 1;

    rec.deet = 0;
    rec.npas = 0;
    rec.dateo = 0;

    rec.ip1 = 0;
    rec.ip2 = 0;
    rec.ip3 = 0;
    rec.ig1 = 0;
    rec.ig2 = 0;
    rec.ig3 = 0;
    rec.ig4 = 0;

    if (fst24_write(test_file, &rec, FST_NO) != TRUE) {
        App_Log(APP_ERROR, "%s: Unable to write record a\n", __func__);
        fst24_record_print(&rec);
        return -1;
    }

    rec.ni = 1;
    rec.nj = MEDIUM_NJ;

    if (fst24_write(test_file, &rec, FST_NO) != TRUE) {
        App_Log(APP_ERROR, "%s: Unable to write record b\n", __func__);
        fst24_record_print(&rec);
        return -1;
    }

    rec.nj = BIG_NJ;

    if (fst24_write(test_file, &rec, FST_NO) != TRUE) {
        App_Log(APP_ERROR, "%s: Unable to write record c\n", __func__);
        fst24_record_print(&rec);
        return -1;
    }

    if (fst24_close(test_file) != TRUE) {
        App_Log(APP_ERROR, "%s: Unable to close file '%s'\n", __func__, filename);
        return -1;
    }

    fst24_record_free(&rec);
    free(dummy_data);

    return 0;
}

int run_test(const int is_rsf) {
    App_Log(APP_ALWAYS, "%s: Testing %s\n", __func__, is_rsf ? "RSF" : "XDF");
    if (create_file(is_rsf) != 0) return -1;

    const char* filename = is_rsf ? filename_rsf : filename_xdf;

    fst_file* test_file = fst24_open(filename, NULL);

    if (test_file == NULL) {
        App_Log(APP_ERROR, "%s: Unable to open file '%s'\n", __func__, filename);
        return -1;
    }

    fst_record rec = default_fst_record;
    fst_query* q = fst24_new_query(test_file, NULL, NULL);

    if (q == NULL) {
        App_Log(APP_ERROR, "%s: Unable to create query\n", __func__);
        return -1;
    }

    if (fst24_read_next(q, &rec) != TRUE) {
        App_Log(APP_ERROR, "%s: Unable to read record a\n", __func__);
        return -1;
    }

    if (rec.ni != BIG_NI) {
        App_Log(APP_ERROR, "%s: Did not retrieve correct ni (%d, expected %d)\n", __func__, rec.ni, BIG_NI);
        return -1;
    }

    if (fst24_read_next(q, &rec) != TRUE) {
        App_Log(APP_ERROR, "%s: Unable to read record b\n", __func__);
        return -1;
    }

    if (rec.nj != MEDIUM_NJ) {
        App_Log(APP_ERROR, "%s: Did not retrieve correct nj (%d, expected %d)\n", __func__, rec.nj, MEDIUM_NJ);
        return -1;
    }

    if (fst24_read_next(q, &rec) != TRUE) {
        App_Log(APP_ERROR, "%s: Unable to read record b\n", __func__);
        return -1;
    }

    if (rec.nj != BIG_NJ) {
        App_Log(APP_ERROR, "%s: Did not retrieve correct nj (%d, expected %d)\n", __func__, rec.nj, BIG_NJ);
        return -1;
    }

    if (fst24_close(test_file) != TRUE) {
        App_Log(APP_ERROR, "%s: Unable to close file '%s'\n", __func__, filename);
        return -1;
    }

    fst24_record_free(&rec);
    fst24_query_free(q);

    return 0;
}

int main(void) {
    if (run_test(1) != 0) return -1;
    return 0;
}
