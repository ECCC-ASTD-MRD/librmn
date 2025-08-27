#include <App.h>
#include <rmn.h>

const char* filename_rsf = "no_dateo.rsf";
const char* filename_xdf = "no_dateo.xdf";

const float data[] = {1.0};

int TEST_DATE = 20250506;
int TEST_TIME = 0;
int TEST_STAMP = -1;

const int TEST_STEPSIZE = 24;
const int TEST_STEPNUM = 10;

static fst_record make_record() {

    fst_record rec = default_fst_record;
    rec.data_type = FST_TYPE_REAL;
    rec.data_bits = 32;
    rec.pack_bits = 32;
    rec.ni = 1;
    rec.nj = 1;
    rec.nk = 1;
    rec.data = (void*)data;

    rec.ip1 = 0;
    rec.ip2 = 0;
    rec.ip3 = 0;
    rec.ig1 = 0;
    rec.ig2 = 0;
    rec.ig3 = 0;
    rec.ig4 = 0;

    rec.deet = TEST_STEPSIZE;
    rec.npas = TEST_STEPNUM;

    const int mode = 3;
    newdate_c(&rec.datev, &TEST_DATE, &TEST_TIME, &mode); // Gotta have a sensible date

    return rec;
}

static int create_file(const int is_rsf) {

    const int mode = 3;
    newdate_c(&TEST_STAMP, &TEST_DATE, &TEST_TIME, &mode);

    App_Log(APP_ALWAYS, "%s: TEST_STAMP = %d\n", __func__, TEST_STAMP);

    const char* filename = is_rsf ? filename_rsf : filename_xdf;
    const char* options = is_rsf ? "RSF+R/W" : "XDF+R/W";

    remove(filename);

    fst_file* test_file = fst24_open(filename, options);
    if (test_file == NULL) {
        App_Log(APP_ERROR, "%s: could not open file %s\n", __func__, filename);
        return -1;
    }

    fst_record rec1 = make_record();
    rec1.ip1 = 1;
    if (fst24_write(test_file, &rec1, FST_NO) != TRUE) {
        App_Log(APP_ERROR, "%s: Unable to write record 1\n", __func__);
        return -1;
    }

    fst_record rec2 = make_record();
    rec2.dateo = rec2.datev;
    rec2.ip1 = 2;
    App_Log(APP_ALWAYS, "%s: Expecting warning\n", __func__);
    if (fst24_write(test_file, &rec2, FST_NO) != TRUE) {
        App_Log(APP_ERROR, "%s: Unable to write record 2\n", __func__);
        return -1;
    }

    if (fst24_close(test_file) != TRUE) {
        App_Log(APP_ERROR, "%s: Could not close test file %s\n", __func__, filename);
        return -1;
    }

    return 0;
}

static int run_test(const int is_rsf) {
    App_Log(APP_ALWAYS, "Testing %s\n", is_rsf ? "RSF" : "XDF");
    if (create_file(is_rsf) != 0) return -1;


    // Verify content of file
    const char* filename = is_rsf ? filename_rsf : filename_xdf;
    fst_file* test_file = fst24_open(filename, NULL);
    if (test_file == NULL) {
        App_Log(APP_ERROR, "%s: Unable to open test file %s\n", __func__, filename);
        return -1;
    }

    // First record (unspecified dateo)
    {
        fst_record crit = default_fst_record;
        crit.ip1 = 1;
        fst_query* q = fst24_new_query(test_file, &crit, NULL);
        fst_record rec = default_fst_record;
        if (fst24_find_next(q, &rec) != TRUE) {
            App_Log(APP_ERROR, "%s: Could not find first record\n", __func__);
            return -1;
        }

        if (rec.datev != TEST_STAMP) {
            App_Log(APP_ERROR, "%s: Record 1 has the wrong datev (%d, expected %d)\n", __func__, rec.datev, TEST_STAMP);
            return -1;
        }

        const int expected_dateo = get_origin_date32(TEST_STAMP, TEST_STEPSIZE, TEST_STEPNUM);
        if (rec.dateo != expected_dateo) {
            App_Log(APP_ERROR, "%s: Record 1 has the wrong dateo (%d, expected %d)\n", __func__, rec.dateo, expected_dateo);
            return -1;
        }
        fst24_query_free(q);
    }

    // Second record (inconsistent origin/validity)
    {
        fst_record crit = default_fst_record;
        crit.ip1 = 2;
        fst_query* q = fst24_new_query(test_file, &crit, NULL);
        fst_record rec = default_fst_record;
        if (fst24_find_next(q, &rec) != TRUE) {
            App_Log(APP_ERROR, "%s: Could not find second record\n", __func__);
            return -1;
        }

        const int expected_datev = get_valid_date32(TEST_STAMP, TEST_STEPSIZE, TEST_STEPNUM);
        if (rec.datev != expected_datev) {
            App_Log(APP_ERROR, "%s: Record 2 has the wrong datev (%d, expected %d)\n", __func__, rec.datev, expected_datev);
            return -1;
        }

        if (rec.dateo != TEST_STAMP) {
            App_Log(APP_ERROR, "%s: Record 2 has the wrong dateo (%d, expected %d)\n", __func__, rec.dateo, TEST_STAMP);
            return -1;
        }
        fst24_query_free(q);

    }

    if (fst24_close(test_file) != TRUE) {
        App_Log(APP_ERROR, "%s: Unable to close test file %s\n", __func__, filename);
        return -1;
    }

    return 0;
}

int main(void) {
    if (run_test(0) != 0) return -1;
    if (run_test(1) != 0) return -1;

    return 0;
}
