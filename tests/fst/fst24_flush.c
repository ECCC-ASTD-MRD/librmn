#include <App.h>
#include <rmn.h>

const char* filename_rsf = "flush.rsf";
const char* filename_xdf = "flush.xdf";

float dummy_data[] = { 0.f };

static fst_record make_record() {
    fst_record rec = default_fst_record;

    rec.data = dummy_data;
    rec.data_type = FST_TYPE_REAL;
    rec.data_bits = 32;
    rec.pack_bits = 32;
    rec.ni = 1;
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

    return rec;
}

int run_test(const int is_rsf) {
    App_Log(APP_ALWAYS, "%s: Testing %s\n", __func__, is_rsf ? "RSF" : "XDF");

    const char* filename = is_rsf ? filename_rsf : filename_xdf;
    const char* options = is_rsf ? "RSF+R/W" : "XDF+R/W";

    remove(filename);

    fst_file* test_file = fst24_open(filename, options);
    if (test_file == NULL) {
        App_Log(APP_ERROR, "%s: Could not open file %s (create)\n", __func__, filename);
        return -1;
    }

    fst_record rec = make_record();
    if (fst24_write(test_file, &rec, FST_NO) != TRUE) {
        App_Log(APP_ERROR, "%s: Could not write record\n", __func__);
        return -1;
    }

    fst24_flush(test_file);

    // Second opening
    fst_file* test_file_2 = fst24_open(filename, NULL);
    if (test_file_2 == NULL) {
        App_Log(APP_ERROR, "%s: Could not open %s a second time\n", __func__, filename);
        return -1;
    }

    const int64_t num_records = fst24_get_num_records(test_file_2);
    if (num_records != 1) {
        App_Log(APP_ERROR, "%s: We should see exactly 1 record in second opening of the file (got %lld)\n",
                __func__, num_records);
        return -1;
    }

    fst24_close(test_file);
    fst24_close(test_file_2);

    return 0;
}

int main(int argc, char** argv) {
    if (argc > 1) {
        const int arg = atoi(argv[1]);
        if (run_test(arg) != 0) return -1;
    }
    else {
        if (run_test(1) != 0) return -1;
        if (run_test(0) != 0) return -1;
    }
    return 0;
}
