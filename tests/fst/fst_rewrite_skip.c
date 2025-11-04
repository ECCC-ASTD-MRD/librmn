#include <App.h>
#include <rmn.h>

const char* filename_xdf = "rewrite_skip.xdf";
const char* filename_rsf = "rewrite_skip.rsf";

const uint32_t data_old[] = { 1, 2, 3, 4, 5, 6, 7, 8 };
const uint32_t data_new[] = { 11, 12, 13, 14, 15, 16, 17, 18 };

// We need to test 6 cases
//  1- Rewrite fail both
//  2- Rewrite success 98/fail 24  (datev check)
//  3- Rewrite success both
//  4- Skip fail both
//  5- Skip success 98/fail 24 (datev check)
//  6- Skip success both
static fst_record test_records[] = {
    default_fst_record,
    default_fst_record,
    default_fst_record,
    default_fst_record,
    default_fst_record,
    default_fst_record,
};

static fst_record new_records[] = {
    default_fst_record,
    default_fst_record,
    default_fst_record,
    default_fst_record,
    default_fst_record,
    default_fst_record,
};

const int num_test_records = sizeof(test_records) / sizeof(fst_record);

static inline fst_record make_basic_record() {
    fst_record rec = default_fst_record;

    rec.ni = 8;
    rec.nj = 1;
    rec.nk = 1;
    rec.data_type = FST_TYPE_UNSIGNED;
    rec.data_bits = 32;
    rec.pack_bits = 32;
    rec.data = (void*)data_old;

    rec.dateo = 0;
    rec.npas = 0;
    rec.deet = 1;

    const int mode = 3;
    int origin_date = 20250506;
    int origin_time = 0;
    newdate_c(&rec.dateo, &origin_date, &origin_time, &mode); // Gotta have a sensible date

    rec.ip1 = 0;
    rec.ip2 = 0;
    rec.ip3 = 0;
    rec.ig1 = 0;
    rec.ig2 = 0;
    rec.ig3 = 0;
    rec.ig4 = 0;

    return rec;
}

static int create_files(const int is_rsf) {
    const char* filename = is_rsf ? filename_rsf : filename_xdf;
    const char* options = is_rsf ? "R/W+RSF" : "R/W+XDF";
    
    remove(filename);
    fst_file* test_file = fst24_open(filename, options);
    if (test_file == NULL) {
        App_Log(APP_ERROR, "%s: Unable to open file(s) (create)\n", __func__);
        return -1;
    }

    for (int i = 0; i < num_test_records; i++) {
        test_records[i] = make_basic_record();
        test_records[i].ip1 = i + 1;
    }

    int write_success = 1;
    for (int i = 0; i < num_test_records; i++) {
        write_success &= (fst24_write(test_file, &test_records[i], FST_NO) == TRUE);
    }

    if (!write_success) {
        App_Log(APP_ERROR, "%s: Could not write all records to test file\n", __func__);
        return -1;
    }

    if (fst24_close(test_file) != TRUE) {
        App_Log(APP_ERROR, "%s: Unable to close test files\n", __func__);
        return -1;
    }

    for (int i = 0; i < num_test_records; i++) {
        fst24_record_copy_metadata(new_records + i, test_records + i, FST_META_ALL);
        new_records[i].data = (void*)data_new;
    }

    // Case 1
    new_records[0].ip2 = 2; // Will fail both rewrite (FST_YES) tests (fst98 + fst24)

    // Case 2
    // Will fail rewrite (FST_YES) for fst24, but succeed with fst98
    double increment = 6.0;
    incdatr_c(&new_records[1].dateo, &test_records[1].dateo, &increment);

    // Case 3
    // No difference, will succeed with FST_YES

    // Case 4
    new_records[3].ip2 = 2; // Will fail both FST_SKIP tests (fst98 + fst24)

    // Case 5
    // Will fail FST_SKIP for fst24, but succeed with fst98
    incdatr_c(&new_records[4].dateo, &test_records[4].dateo, &increment);

    // Case 6
    // No difference, will succeed with FST_SKIP

    return 0;
}

static int test98(const int is_rsf) {
    const char* filename = is_rsf ? filename_rsf : filename_xdf;

    int iun = 0;
    if (c_fnom(&iun, filename, "RND+R/W", 0) != 0) {
        App_Log(APP_ERROR, "%s: fnom error\n", __func__);
        return -1;
    }
    if (c_fstouv(iun, "RND+R/W") < 0) {
        App_Log(APP_ERROR, "%s: fstouv error\n", __func__);
        return -1;
    }

    for (int i = 0; i < num_test_records; i++) {
        const fst_record* rec = &new_records[i];
        const int rewrite = (i < 3) ? FST_YES : FST_SKIP;
        const int status = c_fstecr(
            rec->data, NULL, -rec->pack_bits, iun, rec->dateo, rec->deet, rec->npas, rec->ni, rec->nj, rec->nk,
            rec->ip1, rec->ip2, rec->ip3, rec->typvar, rec->nomvar, rec->etiket, rec->grtyp, rec->ig1, rec->ig2,
            rec->ig3, rec->ig4, rec->data_type, rewrite);
        if (status < 0) {
            App_Log(APP_ERROR, "%s: Unable to write record %d\n", __func__, rec->ip1);
            return -1;
        }
    }


    if (c_fstfrm(iun) < 0) {
        App_Log(APP_ERROR, "%s: fstfrm error\n", __func__);
        return -1;
    }
    if (c_fclos(iun) != 0) {
        App_Log(APP_ERROR, "%s: fclos error\n", __func__);
        return -1;
    }

    // Check the results
    iun = 0;
    if (c_fnom(&iun, filename, "RND+R/O", 0) != 0) {
        App_Log(APP_ERROR, "%s: fnom error\n", __func__);
        return -1;
    }
    if (c_fstouv(iun, "RND+R/O") < 0) {
        App_Log(APP_ERROR, "%s: fstouv error\n", __func__);
        return -1;
    }

    int duplicates[2];
    int num_found = 0;
    int ni, nj, nk;
    int status = 0;
    uint32_t data[sizeof(data_old) / sizeof(uint32_t)];

    // Case 1 (rewrite "failed")
    status = c_fstinl(iun, &ni, &nj, &nk, -1, "", test_records[0].ip1, -1, -1, "", "", duplicates, &num_found, 2);
    if (status != 0 || num_found != 2) {
        App_Log(APP_ERROR,
                 "%s: (Case 1) Should have found exactly 2 records, with status 0 (num_found = %d, status = %d)\n",
                __func__, num_found, status);
        return -1;
    }

    c_fstluk(data, duplicates[0], &ni, &nj, &nk);
    if (memcmp(data, data_old, sizeof(data_old)) != 0) {
        App_Log(APP_ERROR, "%s: Old record (1) seems to have been rewritten\n", __func__);
        return -1;
    }

    c_fstluk(data, duplicates[1], &ni, &nj, &nk);
    if (memcmp(data, data_new, sizeof(data_old)) != 0) {
        App_Log(APP_ERROR, "%s: New record (1) does not seem to be there\n", __func__);
        return -1;
    }

    // Case 2 (rewrite succeeds)
    status = c_fstinl(iun, &ni, &nj, &nk, -1, "", test_records[1].ip1, -1, -1, "", "", duplicates, &num_found, 1);
    if (status != 0 || num_found != 1) {
        App_Log(APP_ERROR,
                 "%s: (Case 2) Should have found exactly 1 record, with status 0 (num_found = %d, status = %d)\n",
                __func__, num_found, status);
        return -1;
    }
    c_fstluk(data, duplicates[0], &ni, &nj, &nk);
    if (memcmp(data, data_new, sizeof(data_new)) != 0) {
        App_Log(APP_ERROR, "%s: Old record (2) does not seem to have been rewritten\n", __func__);
        return -1;
    }

    // Case 3 (rewrite succeeds)
    status = c_fstinl(iun, &ni, &nj, &nk, -1, "", test_records[2].ip1, -1, -1, "", "", duplicates, &num_found, 1);
    if (status != 0 || num_found != 1) {
        App_Log(APP_ERROR,
                 "%s: (Case 3) Should have found exactly 1 record, with status 0 (num_found = %d, status = %d)\n",
                __func__, num_found, status);
        return -1;
    }
    c_fstluk(data, duplicates[0], &ni, &nj, &nk);
    if (memcmp(data, data_new, sizeof(data_new)) != 0) {
        App_Log(APP_ERROR, "%s: Old record (3) does not seem to have been rewritten\n", __func__);
        return -1;
    }

    // Case 4 (skip "failed")
    status = c_fstinl(iun, &ni, &nj, &nk, -1, "", test_records[3].ip1, -1, -1, "", "", duplicates, &num_found, 2);
    if (status != 0 || num_found != 2) {
        App_Log(APP_ERROR,
                 "%s: (Case 4) Should have found exactly 2 records, with status 0 (num_found = %d, status = %d)\n",
                __func__, num_found, status);
        return -1;
    }

    c_fstluk(data, duplicates[0], &ni, &nj, &nk);
    if (memcmp(data, data_old, sizeof(data_old)) != 0) {
        App_Log(APP_ERROR, "%s: Old record (4) seems to have been rewritten\n", __func__);
        return -1;
    }

    c_fstluk(data, duplicates[1], &ni, &nj, &nk);
    if (memcmp(data, data_new, sizeof(data_old)) != 0) {
        App_Log(APP_ERROR, "%s: New record (4) does not seem to be there\n", __func__);
        return -1;
    }

    // Case 5 (skip succeeds)
    status = c_fstinl(iun, &ni, &nj, &nk, -1, "", test_records[4].ip1, -1, -1, "", "", duplicates, &num_found, 1);
    if (status != 0 || num_found != 1) {
        App_Log(APP_ERROR,
                 "%s: (Case 5) Should have found exactly 1 record, with status 0 (num_found = %d, status = %d)\n",
                __func__, num_found, status);
        return -1;
    }
    c_fstluk(data, duplicates[0], &ni, &nj, &nk);
    if (memcmp(data, data_old, sizeof(data_old)) != 0) {
        App_Log(APP_ERROR, "%s: Old record (5) does not seem to have been skipped\n", __func__);
        return -1;
    }

    // Case 6 (skip succeeds)
    status = c_fstinl(iun, &ni, &nj, &nk, -1, "", test_records[5].ip1, -1, -1, "", "", duplicates, &num_found, 1);
    if (status != 0 || num_found != 1) {
        App_Log(APP_ERROR,
                 "%s: (Case 6) Should have found exactly 1 record, with status 0 (num_found = %d, status = %d)\n",
                __func__, num_found, status);
        return -1;
    }
    c_fstluk(data, duplicates[0], &ni, &nj, &nk);
    if (memcmp(data, data_old, sizeof(data_old)) != 0) {
        App_Log(APP_ERROR, "%s: Old record (6) does not seem to have been skipped\n", __func__);
        return -1;
    }

    if (c_fstfrm(iun) < 0) {
        App_Log(APP_ERROR, "%s: fstfrm error\n", __func__);
        return -1;
    }
    if (c_fclos(iun) != 0) {
        App_Log(APP_ERROR, "%s: fclos error\n", __func__);
        return -1;
    }

    return 0;
}

static int test24(const int is_rsf) {
    const char* filename = is_rsf ? filename_rsf : filename_xdf;
    fst_file* test_file = fst24_open(filename, "R/W");
    if (test_file == NULL) {
        App_Log(APP_ERROR, "%s: Unable to open test file %s\n", __func__, filename);
        return -1;
    }

    for (int i = 0; i < num_test_records; i++) {
        const int rewrite = (i < 3) ? FST_YES : FST_SKIP;
        const int32_t status = fst24_write(test_file, &new_records[i], rewrite);
        if (status != TRUE) {
            App_Log(APP_ERROR, "%s: Unable to write record %d\n", __func__, new_records[i].ip1);
            return -1;
        }
    }

    if (fst24_close(test_file) != TRUE) {
        App_Log(APP_ERROR, "%s: Unable to close test file %s\n", __func__, filename);
        return -1;
    }

    // Check result
    test_file = fst24_open(filename, NULL);
    if (test_file == NULL) {
        App_Log(APP_ERROR, "%s: Unable to open test file %s\n", __func__, filename);
        return -1;
    }

    fst_record duplicates[3];

    // Case 1
    {
        fst_record crit = default_fst_record;
        crit.ip1 = test_records[0].ip1;
        fst_query* q = fst24_new_query(test_file, &crit, NULL);
        const int num_found = fst24_find_all(q, duplicates, 3);
        if (num_found  != 2) {
            App_Log(APP_ERROR, "%s: Should have found exactly 2 records for case 1 (got %d)\n", __func__, num_found);
            return -1;
        }

        fst24_read_record(&duplicates[0]);
        fst24_read_record(&duplicates[1]);
        if (memcmp(duplicates[0].data, data_old, sizeof(data_old)) != 0) {
            App_Log(APP_ERROR, "%s: (Case 1) Old record's data is different!\n", __func__);
            return -1;
        }
        if (memcmp(duplicates[1].data, data_new, sizeof(data_new)) != 0) {
            App_Log(APP_ERROR, "%s: (Case 1) New record's data is wrong!\n", __func__);
            return -1;
        }

        fst24_record_free(&duplicates[0]);
        fst24_record_free(&duplicates[1]);
        fst24_query_free(q);
    }

    // Case 2
    {
        fst_record crit = default_fst_record;
        crit.ip1 = test_records[1].ip1;
        fst_query* q = fst24_new_query(test_file, &crit, NULL);
        const int num_found = fst24_find_all(q, duplicates, 3);
        if (num_found  != 2) {
            App_Log(APP_ERROR, "%s: Should have found exactly 2 records for case 2 (got %d)\n", __func__, num_found);
            return -1;
        }

        fst24_read_record(&duplicates[0]);
        fst24_read_record(&duplicates[1]);
        if (memcmp(duplicates[0].data, data_old, sizeof(data_old)) != 0) {
            App_Log(APP_ERROR, "%s: (Case 2) Old record's data is different!\n", __func__);
            return -1;
        }
        if (memcmp(duplicates[1].data, data_new, sizeof(data_new)) != 0) {
            App_Log(APP_ERROR, "%s: (Case 2) New record's data is wrong!\n", __func__);
            return -1;
        }

        fst24_record_free(&duplicates[0]);
        fst24_record_free(&duplicates[1]);
        fst24_query_free(q);
    }

    // Case 3
    {
        fst_record crit = default_fst_record;
        crit.ip1 = test_records[2].ip1;
        fst_query* q = fst24_new_query(test_file, &crit, NULL);
        const int num_found = fst24_find_all(q, duplicates, 3);
        if (num_found  != 1) {
            App_Log(APP_ERROR, "%s: Should have found exactly 1 record for case 3 (got %d)\n", __func__, num_found);
            return -1;
        }

        fst24_read_record(&duplicates[0]);
        if (memcmp(duplicates[0].data, data_new, sizeof(data_new)) != 0) {
            App_Log(APP_ERROR, "%s: (Case 3) New record's data is different!\n", __func__);
            return -1;
        }

        fst24_record_free(&duplicates[0]);
        fst24_query_free(q);
    }

    // Case 4
    {
        fst_record crit = default_fst_record;
        crit.ip1 = test_records[3].ip1;
        fst_query* q = fst24_new_query(test_file, &crit, NULL);
        const int num_found = fst24_find_all(q, duplicates, 3);
        if (num_found  != 2) {
            App_Log(APP_ERROR, "%s: Should have found exactly 2 records for case 4 (got %d)\n", __func__, num_found);
            return -1;
        }

        fst24_read_record(&duplicates[0]);
        fst24_read_record(&duplicates[1]);
        if (memcmp(duplicates[0].data, data_old, sizeof(data_old)) != 0) {
            App_Log(APP_ERROR, "%s: (Case 4) Old record's data is different!\n", __func__);
            return -1;
        }
        if (memcmp(duplicates[1].data, data_new, sizeof(data_new)) != 0) {
            App_Log(APP_ERROR, "%s: (Case 4) New record's data is wrong!\n", __func__);
            return -1;
        }

        fst24_record_free(&duplicates[0]);
        fst24_record_free(&duplicates[1]);
        fst24_query_free(q);
    }

    // Case 5
    {
        fst_record crit = default_fst_record;
        crit.ip1 = test_records[4].ip1;
        fst_query* q = fst24_new_query(test_file, &crit, NULL);
        const int num_found = fst24_find_all(q, duplicates, 3);
        if (num_found  != 2) {
            App_Log(APP_ERROR, "%s: Should have found exactly 2 records for case 5 (got %d)\n", __func__, num_found);
            return -1;
        }

        fst24_read_record(&duplicates[0]);
        fst24_read_record(&duplicates[1]);
        if (memcmp(duplicates[0].data, data_old, sizeof(data_old)) != 0) {
            App_Log(APP_ERROR, "%s: (Case 5) Old record's data is different!\n", __func__);
            return -1;
        }
        if (memcmp(duplicates[1].data, data_new, sizeof(data_new)) != 0) {
            App_Log(APP_ERROR, "%s: (Case 5) New record's data is wrong!\n", __func__);
            return -1;
        }

        fst24_record_free(&duplicates[0]);
        fst24_record_free(&duplicates[1]);
        fst24_query_free(q);
    }

    // Case 6
    {
        fst_record crit = default_fst_record;
        crit.ip1 = test_records[5].ip1;
        fst_query* q = fst24_new_query(test_file, &crit, NULL);
        const int num_found = fst24_find_all(q, duplicates, 3);
        if (num_found  != 1) {
            App_Log(APP_ERROR, "%s: Should have found exactly 1 record for case 6 (got %d)\n", __func__, num_found);
            return -1;
        }

        fst24_read_record(&duplicates[0]);
        if (memcmp(duplicates[0].data, data_old, sizeof(data_old)) != 0) {
            App_Log(APP_ERROR, "%s: (Case 6) New record's data is different!\n", __func__);
            return -1;
        }

        fst24_record_free(&duplicates[0]);
        fst24_query_free(q);
    }

    if (fst24_close(test_file) != TRUE) {
        App_Log(APP_ERROR, "%s: Unable to close test file %s\n", __func__, filename);
        return -1;
    }

    return 0;
}

static int run_test(const int is_rsf) {
    if (create_files(is_rsf) != 0) return -1;
    if (test98(is_rsf) != 0) return -1;

    if (create_files(is_rsf) != 0) return -1;
    if (test24(is_rsf) != 0) return -1;
    return 0;
}

int main(void) {
    if (run_test(0) != 0) return -1;
    if (run_test(1) != 0) return -1;
    App_Log(APP_ALWAYS, "%s: Test successful\n", __func__);

    return 0;
}
