#include <rmn.h>
#include <App.h>
#include <rmn/excdes_new.h>

const char* test_filename = "dummy.fst";
const int TEST_IP = 1000;

int test98(const int is_rsf) {
    remove(test_filename);

    int iun = 0;
    char* options = is_rsf ? "STD+RND+RSF+R/W" : "STD+RND+XDF+R/W";
    if (c_fnom(&iun, test_filename, options, 0) != 0) {
        App_Log(APP_ERROR, "%s: Unable to fnom\n", __func__);
        return -1;
    }

    if (c_fstouv(iun, options) < 0) {
        App_Log(APP_ERROR, "%s: Unable to fstouv\n", __func__);
        return -1;
    }

    float dummy_data[] = {0.0f};
    int status = c_fstecr(
        dummy_data, NULL, -32, iun, 0, 0, 0, 1, 1, 1, TEST_IP + 1, 1, 1, "  ", "    ", "            ", "  ", 1, 1, 1, 1, 1, FST_NO);
    if (status != 0) {
        App_Log(APP_ERROR, "%s: Error writing single record (1). Status = %d (0x%x)\n", __func__, status, status);
        return -1;
    }

    status = c_fstecr(dummy_data, NULL, -32, iun, 0, 0, 0, 1, 1, 1, TEST_IP + 1, 1, 1, "  ", "    ", "            ", "  ",
                 1, 1, 1, 1, 1, FST_SKIP);
    if (status <= 0) {
        App_Log(APP_ERROR, "%s: Error writing single record (2). Status = %d (0x%x)\n", __func__, status, status);
        return -1;
    }

    c_fstfrm(iun);
    c_fclos(iun);

    c_fnom(&iun, test_filename, options, 0);
    const int num_records = c_fstouv(iun, options);

    if (num_records != 1) {
        App_Log(APP_ERROR, "%s: There should be %d records, not %d\n", __func__, 1, num_records);
        return -1;
    }

    c_fstfrm(iun);
    c_fclos(iun);

    return 0;
}



int test24(const int is_rsf) {
    remove(test_filename);

    const char* options = is_rsf ? "RSF+R/W" : "XDF+R/W";
    fst_file* f = fst24_open(test_filename, options);
    if (f == NULL) {
        App_Log(APP_ERROR, "%s: Unable to open %s for writing\n", __func__, test_filename);
        return -1;
    }

    fst_record new_record = default_fst_record;
    float dummy_data[] = {0.0f};

    new_record.datev = 0;
    new_record.dateo = 0;
    new_record.data = dummy_data;
    new_record.data_type = FST_TYPE_REAL;
    new_record.data_bits = 32;
    new_record.pack_bits = 32;
    new_record.ni = 1;
    new_record.nj = 1;
    new_record.nk = 1;
    new_record.ip1 = TEST_IP + 1;
    new_record.ip2 = 1;
    new_record.ip3 = 1;
    new_record.ig1 = 1;
    new_record.ig2 = 1;
    new_record.ig3 = 1;
    new_record.ig4 = 1;
    new_record.deet = 0;
    new_record.npas = 0;

    if (fst24_write(f, &new_record, FST_NO) <= 0) {
        App_Log(APP_ERROR, "%s: Error writing single record\n", __func__);
        return -1;
    }

    fst24_flush(f);

    RequetesInit();
    const int ips[] = {TEST_IP};
    Xc_Select_ip1(1, 1, ips, 1);

    // Write record again. This should NOT add it.
    if (fst24_write(f, &new_record, FST_SKIP) <= 0) {
        App_Log(APP_ERROR, "%s: Second write failed!\n", __func__);
        return -1;
    }

    fst24_flush(f); // This does NOT seem to do what I think it does...

    if (is_rsf && fst24_get_num_records(f) != 1) {
        App_Log(APP_ERROR, "%s: Not the correct number of records! %d, but should be 1\n", __func__, fst24_get_num_records(f));
        return -1;
    }

    fst24_close(f);

    if (!is_rsf) {
        f = fst24_open(test_filename, "R/O");
        if (is_rsf && fst24_get_num_records(f) != 1) {
            App_Log(APP_ERROR, "%s: Not the correct number of records! %d, but should be 1\n", __func__, fst24_get_num_records(f));
            return -1;
        }
        fst24_close(f);
    }

    return 0;
}

int main(void) {
    App_Log(APP_INFO, "Testing RSF\n");
    if (test24(1) < 0) return -1;
    if (test98(1) < 0) return -1;

    App_Log(APP_INFO, "Testing XDF\n");
    if (test24(0) < 0) return -1;
    if (test98(0) < 0) return -1;


    App_Log(APP_INFO, "Tests successful\n");
    return 0;
}
