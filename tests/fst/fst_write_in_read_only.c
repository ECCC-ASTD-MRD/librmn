#include <rmn/fst24_file.h>
#include <App.h>
#include <rmn.h>

int test_write_in_ro(const char* backend) {
    App_Log(APP_ALWAYS, "Testing %s\n", backend);

    char test_filename[64];
    snprintf(test_filename, sizeof(test_filename), "read_only.%s", backend);
    // Create file
    remove(test_filename);
    char options[64];
    snprintf(options, sizeof(options), "%s+R/W", backend);
    fst_file* f = fst24_open(test_filename, options);
    if (f == NULL || fst24_close(f) <= 0) return -1;

    f = fst24_open(test_filename, "R/O");
    if (f == NULL) return -1;

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

    void* data = NULL;
    void* work = NULL;
    int32_t status = c_fstecr(data, work, -32, iun, 0, 0, 0, 1, 1, 1, 1, 1, 1, "", "", "", "", 1, 1, 1, 1, 5, 0);
    if (status >= 0) {
        App_Log(APP_ERROR, "%s: Write (fst98) should have failed\n", __func__);
        return -1;
    }

    c_fstfrm(iun);

    return 0;
}

int main(void) {

    if (test_write_in_ro("RSF") < 0) return -1;
    if (test_write_in_ro("XDF") < 0) return -1;

    App_Log(APP_INFO, "Test successful\n");

    return 0;
}
