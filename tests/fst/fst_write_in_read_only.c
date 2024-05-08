#include <rmn/fst24_file.h>
#include <App.h>
#include <rmn.h>

const char* test_filename = "read_only.fst";

int test_write_in_ro(const int is_rsf) {

    // Create file
    remove(test_filename);
    fst_file* f = fst24_open(test_filename, is_rsf ? "RSF+R/W" : "XDF+R/W");
    if (!f || fst24_close(f) <= 0) return -1;

    f = fst24_open(test_filename, "R/O");
    if (!f) return -1;

    fst_record rec = default_fst_record;
    if (fst24_write(f, &rec, 0) == TRUE) {
        App_Log(APP_ERROR, "%s: Write (fst24) should have failed\n", __func__);
        return -1;
    }
    fst24_close(f);

    int32_t iun = 0;
    if (c_fnom(&iun, test_filename, "STD+RND+OLD+R/O", 0) != 0 ||
        c_fstouv(iun, "R/O") < 0) {
        App_Log(APP_ERROR, "%s: Unable to open with fst98 interface\n", __func__);
        return -1;
    }

    void* data;
    void* work;
    int32_t status = c_fstecr(data, work, -32, iun, 0, 0, 0, 1, 1, 1, 1, 1, 1, "", "", "", "", 1, 1, 1, 1, 5, 0);
    if (status >= 0) {
        App_Log(APP_ERROR, "%s: Write (fst98) should have failed\n", __func__);
        return -1;
    }

    c_fstfrm(iun);

    return 0;
}

int main(void) {

    if (test_write_in_ro(1) < 0) return -1;
    if (test_write_in_ro(0) < 0) return -1;

    App_Log(APP_INFO, "Test successful\n");

    return 0;
}
