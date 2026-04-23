#include <App.h>
#include <rmn.h>

const char* filename_rsf = "wildcard.rsf";
const char* filename_xdf = "wildcard.xdf";

const char* base_etiket = "ABCDEFGHIJKL";

static int create_file(const int is_rsf) {
    const char* filename = is_rsf ? filename_rsf : filename_xdf;
    remove(filename);

    fst_file* test_file = fst24_open(filename, "R/W");
    if (test_file == NULL) {
        App_Log(APP_ERROR, "%s: Unable to open (create) file %s\n", __func__, filename);
        return - 1;
    }

    int dummy_data[] = { 0 };
    fst_record rec = default_fst_record;

    rec.data = dummy_data;
    rec.data_type = FST_TYPE_SIGNED;
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

    strcpy(rec.etiket, "wildcard");

    // nomvar ------------------------------------------------------------
    strcpy(rec.nomvar, "A");
    if (fst24_write(test_file, &rec, FST_NO) != TRUE) {
        App_Log(APP_ERROR, "%s: Unable to write record '%s' to file %s\n", __func__, rec.nomvar, filename);
        return - 1;
    }

    strcpy(rec.nomvar, "AB");
    if (fst24_write(test_file, &rec, FST_NO) != TRUE) {
        App_Log(APP_ERROR, "%s: Unable to write record '%s' to file %s\n", __func__, rec.nomvar, filename);
        return - 1;
    }

    strcpy(rec.nomvar, "ABC");
    if (fst24_write(test_file, &rec, FST_NO) != TRUE) {
        App_Log(APP_ERROR, "%s: Unable to write record '%s' to file %s\n", __func__, rec.nomvar, filename);
        return - 1;
    }

    strcpy(rec.nomvar, "BB");
    if (fst24_write(test_file, &rec, FST_NO) != TRUE) {
        App_Log(APP_ERROR, "%s: Unable to write record '%s' to file %s\n", __func__, rec.nomvar, filename);
        return - 1;
    }

    strcpy(rec.nomvar, "CBA");
    if (fst24_write(test_file, &rec, FST_NO) != TRUE) {
        App_Log(APP_ERROR, "%s: Unable to write record '%s' to file %s\n", __func__, rec.nomvar, filename);
        return - 1;
    }
    // end nomvar ------------------------------------------------------------

    // etiket -----------------------------------------------------------------
    strcpy(rec.nomvar, "ZZZZ");
    for (int i = 0; i < FST_ETIKET_LEN; i++) {
        strcpy(rec.etiket, base_etiket);
        if (i < FST_ETIKET_LEN - 1) rec.etiket[i] = 'x';
        if (fst24_write(test_file, &rec, FST_NO) != TRUE) {
            App_Log(APP_ERROR, "%s: Unable to write record etiket '%s' to file %s\n", __func__, rec.etiket, filename);
            return - 1;
        }
    }
    // end etiket -------------------------------------------------------------

    // typvar --------------------------------------------------
    strcpy(rec.etiket, "ZZZZZZZZZZZZ");
    strcpy(rec.typvar, "AB");
    if (fst24_write(test_file, &rec, FST_NO) != TRUE) {
        App_Log(APP_ERROR, "%s: Unable to write record typvar '%s' to file %s\n", __func__, rec.typvar, filename);
        return - 1;
    }
    strcpy(rec.typvar, "XB");
    if (fst24_write(test_file, &rec, FST_NO) != TRUE) {
        App_Log(APP_ERROR, "%s: Unable to write record typvar '%s' to file %s\n", __func__, rec.typvar, filename);
        return - 1;
    }
    strcpy(rec.typvar, "AX");
    if (fst24_write(test_file, &rec, FST_NO) != TRUE) {
        App_Log(APP_ERROR, "%s: Unable to write record typvar '%s' to file %s\n", __func__, rec.typvar, filename);
        return - 1;
    }
    // end typvar ----------------------------------------------

    if (fst24_close(test_file) != TRUE) {
        App_Log(APP_ERROR, "%s: Unable to close file %s\n", __func__, filename);
        return - 1;
    }
    return 0;
}

static int run_wildcard_test(const int is_rsf) {

    App_Log(APP_ALWAYS, "Testing %s\n", is_rsf ? "RSF" : "XDF");

    if (create_file(is_rsf) != 0) return -1;

    const char* filename = is_rsf ? filename_rsf : filename_xdf;
    fst_file* test_file = fst24_open(filename, NULL);
    if (test_file == NULL) {
        App_Log(APP_ERROR, "%s: Unable to open (read) file %s\n", __func__, filename);
        return - 1;
    }

    const int MAX_NUM_RECORDS = 10;
    fst_record records_found[MAX_NUM_RECORDS];

    {
        fst_record criteria1 = default_fst_record;
        strcpy(criteria1.nomvar, "A~");
        fst_query* q1 = fst24_new_query(test_file, &criteria1, NULL);

        if (q1 == NULL) {
            App_Log(APP_ERROR, "%s: Unable to create query with the following criteria:\n", __func__);
            fst24_record_print_non_default(&criteria1);
            return -1;
        }

        const int num_expected = 2;
        const int num_found = fst24_find_all(q1, records_found, MAX_NUM_RECORDS);
        if (num_found != num_expected) {
            App_Log(APP_ERROR, "%s: Found %d records, but should have been %d\nCriteria:\n",
                __func__, num_found, num_expected);
            fst24_record_print_non_default(&criteria1);
        }

        fst24_query_free(q1);
        for (int i = 0; i < num_found; i++) fst24_record_free(&records_found[i]);
    }


    {
        fst_record criteria2 = default_fst_record;
        strcpy(criteria2.nomvar, "~B");
        fst_query* q2 = fst24_new_query(test_file, &criteria2, NULL);

        if (q2 == NULL) {
            App_Log(APP_ERROR, "%s: Unable to create query with the following criteria:\n", __func__);
            fst24_record_print_non_default(&criteria2);
            return -1;
        }

        const int num_expected = 2;
        const int num_found = fst24_find_all(q2, records_found, MAX_NUM_RECORDS);
        if (num_found != num_expected) {
            App_Log(APP_ERROR, "%s: Found %d records, but should have been %d\nCriteria:\n",
                __func__, num_found, num_expected);
            fst24_record_print_non_default(&criteria2);
        }

        fst24_query_free(q2);
        for (int i = 0; i < num_found; i++) fst24_record_free(&records_found[i]);
    }

    // Testing etiket
    for (int i = 0; i < FST_ETIKET_LEN - 1; i++) {
        fst_record criteria = default_fst_record;
        strcpy(criteria.etiket, base_etiket);
        criteria.etiket[i] = '~';
        fst_query* q = fst24_new_query(test_file, &criteria, NULL);

        if (q == NULL) {
            App_Log(APP_ERROR, "%s: Unable to create query with the following criteria:\n", __func__);
            fst24_record_print_non_default(&criteria);
            return -1;
        }

        const int num_expected = 2;
        const int num_found = fst24_find_all(q, records_found, MAX_NUM_RECORDS);
        if (num_found != num_expected) {
            App_Log(APP_ERROR, "%s: Found %d records, but should have been %d\nCriteria:\n",
                __func__, num_found, num_expected);
            fst24_record_print_non_default(&criteria);
        }

        int ok = 1;
        char target_etiket[FST_ETIKET_LEN];
        strcpy(target_etiket, base_etiket);
        target_etiket[i] = 'X';
        if (strcasecmp(records_found[0].etiket, base_etiket) != 0 &&
            strcasecmp(records_found[1].etiket, base_etiket) != 0) ok = 0;
        if (strcasecmp(records_found[0].etiket, target_etiket) != 0 &&
            strcasecmp(records_found[1].etiket, target_etiket) != 0) ok = 0;

        if (!ok) {
            App_Log(APP_ERROR, "The records found do not have the correct etikets: %s and %s, but expected %s and %s\n",
                records_found[0].etiket, records_found[1].etiket, target_etiket, base_etiket);
            return -1;
        }

        fst24_query_free(q);
        for (int i = 0; i < num_found; i++) fst24_record_free(&records_found[i]);
    }

    // Testing typvar
    for (int i = 0; i < FST_TYPVAR_LEN - 1; i++) {
        fst_record criteria = default_fst_record;
        const char* base_typvar = "AB";
        strcpy(criteria.typvar, base_typvar);
        criteria.typvar[i] = '~';
        fst_query* q = fst24_new_query(test_file, &criteria, NULL);

        if (q == NULL) {
            App_Log(APP_ERROR, "%s: Unable to create query with the following criteria:\n", __func__);
            fst24_record_print_non_default(&criteria);
            return -1;
        }

        const int num_expected = 2;
        const int num_found = fst24_find_all(q, records_found, MAX_NUM_RECORDS);
        if (num_found != num_expected) {
            App_Log(APP_ERROR, "%s: Found %d records, but should have been %d\nCriteria:\n",
                __func__, num_found, num_expected);
            fst24_record_print_non_default(&criteria);
        }

        int ok = 1;
        char target_typvar[FST_TYPVAR_LEN];
        strcpy(target_typvar, base_typvar);
        target_typvar[i] = 'X';
        if (strcasecmp(records_found[0].typvar, base_typvar) != 0 &&
            strcasecmp(records_found[1].typvar, base_typvar) != 0) ok = 0;
        if (strcasecmp(records_found[0].typvar, target_typvar) != 0 &&
            strcasecmp(records_found[1].typvar, target_typvar) != 0) ok = 0;

        if (!ok) {
            App_Log(APP_ERROR, "The records found do not have the correct typvars: %s and %s, but expected %s and %s\n",
                records_found[0].typvar, records_found[1].typvar, target_typvar, base_typvar);
            return -1;
        }

        fst24_query_free(q);
        for (int i = 0; i < num_found; i++) fst24_record_free(&records_found[i]);
    }


    if (fst24_close(test_file) != TRUE) {
        App_Log(APP_ERROR, "%s: Unable to close file %s\n", __func__, filename);
        return - 1;
    }

    return 0;
}

int main(void) {
    if (run_wildcard_test(1) != 0) return -1;
    if (run_wildcard_test(0) != 0) return -1;
    return 0;
}
