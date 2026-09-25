
#include <App.h>
#include <rmn.h>

const char* filename_rsf = "skip_descriptors.rsf";
const char* filename_xdf = "skip_descriptors.xdf";

int create_file(const char* backend) {
    const char* filename = strcmp(backend, "RSF") == 0 ? filename_rsf : filename_xdf;
    char options[64];
    snprintf(options, sizeof(options), "R/W+%s", backend);

    remove(filename);

    fst_file* f = fst24_open(filename, options);
    if (f == NULL) {
        App_Log(APP_ERROR, "Unable to create file! %s\n", filename);
        return -1;
    }

    float dummy = 0;

    fst_record rec = default_fst_record;
    rec.data = &dummy;
    rec.ni = 1;
    rec.nj = 1;
    rec.nk = 1;

    rec.data_bits = 32;
    rec.pack_bits = 32;
    rec.data_type = FST_TYPE_REAL;

    rec.deet = 0;
    rec.npas = 0;
    rec.ig1 = 0;
    rec.ig2 = 0;
    rec.ig3 = 0;
    rec.ig4 = 0;
    rec.ip1 = 0;
    rec.ip2 = 0;
    rec.ip3 = 0;

    const char* names[17] = {
        ">>", "^^", "^>", "!!", "##", "#>>#", "#^^#", "####", "HY", "PROJ", "MTRX", // Should be skipped
        "hy", "PRoj", "mtRX",                                                       // Should also be skipped
        "><<<", "!!!!", "#abc",                                                     // Should NOT be skipped
    };
    for (int i = 0; i < 17; i++) {
        snprintf(rec.nomvar, FST_NOMVAR_LEN, "%s", names[i]);
        if (!fst24_write(f, &rec, FST_NO)) {
            App_Log(APP_ERROR, "Unable to write!\n");
            return -1;
        }
    }

    if (!fst24_close(f)) {
        App_Log(APP_ERROR, "Unable to close file\n");
        return -1;
    }

    fst24_record_free(&rec);

    return 0;
}

int test_skip_descriptors(const char* backend) {
    App_Log(APP_ALWAYS, "Testing %s\n", backend);

    if (create_file(backend) != 0) return -1;

    const char* filename = strcmp(backend, "RSF") == 0 ? filename_rsf : filename_xdf;

    fst_file* f = fst24_open(filename, NULL);

    if (f == NULL) {
        App_Log(APP_ERROR, "Unable to open test file (%s)\n", filename);
        return -1;
    }

    fst_record crit = default_fst_record;
    fst_query_options o = default_query_options;
    o.skip_grid_descriptors = 1;
    fst_query* q_all = fst24_new_query(f, &crit, NULL);
    fst_query* q_skip = fst24_new_query(f, &crit, &o);

    const int num_all = fst24_find_count(q_all);
    const int num_skip = fst24_find_count(q_skip);

    if (num_all != 17 || num_skip != 3) {
        App_Log(APP_ERROR, "Found %d records (%d), but should have been %d (%d)\n", num_skip, num_all, 3, 17);
        return -1;
    }

    fst24_query_free(q_all);
    fst24_query_free(q_skip);

    if (!fst24_close(f)) {
        App_Log(APP_ERROR, "Unable to close test file (%s)\n", filename);
        return -1;
    }

    return 0;
}

int main(void) {
    if (test_skip_descriptors("RSF") != 0) return -1;
    if (test_skip_descriptors("XDF") != 0) return -1;
    App_Log(APP_ALWAYS, "Test successful\n");

    return 0;
}
