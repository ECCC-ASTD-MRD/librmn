#include <App.h>
#include <rmn.h>

const char* filename_rsf = "wildcard.rsf";
const char* filename_xdf = "wildcard.xdf";

const char* base_etiket = "ABCDEFGHIJKL";

static int create_file(const int is_rsf) {
    const char* filename = is_rsf ? filename_rsf : filename_xdf;
    const char* options = is_rsf ? "R/W+RSF" : "R/W+XDF";

    remove(filename);
    fst_file* test_file = fst24_open(filename, options);
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

static int test_fst24(const int is_rsf);
static int test_fst98(const int is_rsf);

static int run_wildcard_test(const int is_rsf) {

    App_Log(APP_ALWAYS, "Testing %s\n", is_rsf ? "RSF" : "XDF");

    if (create_file(is_rsf) != 0) return -1;
    if (test_fst24(is_rsf) != 0) return -1;
    if (test_fst98(is_rsf) != 0) return -1;

    return 0;
}

static int test_fst24(const int is_rsf) {

    const char* filename = is_rsf ? filename_rsf : filename_xdf;
    fst_file* test_file = fst24_open(filename, NULL);
    if (test_file == NULL) {
        App_Log(APP_ERROR, "%s: Unable to open (read) file %s\n", __func__, filename);
        return -1;
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
            return -1;
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
            return -1;
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
            return -1;
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
            return -1;
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

static int test_fst98(const int is_rsf) {

    const char* filename = is_rsf ? filename_rsf : filename_xdf;

    int status = 0;
    int iun = 0;

    status = c_fnom(&iun, filename, "RND", 0);
    if (status != 0) {
        App_Log(APP_ERROR, "%s: Unable to open (read) file %s (fnom)\n", __func__, filename);
        return -1;
    }

    status = c_fstouv(iun, "R/O");
    if (status <= 0) {
        App_Log(APP_ERROR, "%s: Unable to open (read) file %s (fstouv)\n", __func__, filename);
        return -1;
    }

    const int MAX_NUM_RECORDS = 10;
    int records_found[MAX_NUM_RECORDS];
    int ni, nj, nk, num_found;

    {
        status = c_fstinl(iun, &ni, &nj, &nk, -1, "", -1, -1, -1, "", "A~", records_found, &num_found, MAX_NUM_RECORDS);

        if (status != 0) {
            App_Log(APP_ERROR, "%s: Unable to get list of records with nomvar = \"A~\"\n", __func__);
            return -1;
        }

        const int num_expected = 2;
        if (num_found != num_expected) {
            App_Log(APP_ERROR, "%s: Found %d records, but should have been %d\nCriteria: nomvar = \"A~\"\n",
                __func__, num_found, num_expected);
            return -1;
        }
    }


    {
        status = c_fstinl(iun, &ni, &nj, &nk, -1, "", -1, -1, -1, "", "~B", records_found, &num_found, MAX_NUM_RECORDS);
        if (status != 0) {
            App_Log(APP_ERROR, "%s: Unable to get list of records with nomvar = \"~B\"\n", __func__);
            return -1;
        }

        const int num_expected = 2;
        if (num_found != num_expected) {
            App_Log(APP_ERROR, "%s: Found %d records, but should have been %d\nCriteria:nomvar = \"~B\"\n",
                __func__, num_found, num_expected);
            return -1;
        }
    }

    // Testing etiket
    for (int i = 0; i < FST_ETIKET_LEN - 1; i++) {
        char search_etiket[FST_ETIKET_LEN];
        strcpy(search_etiket, base_etiket);
        search_etiket[i] = '~';

        status = c_fstinl(
            iun, &ni, &nj, &nk, -1, search_etiket, -1, -1, -1, "", "", records_found, &num_found, MAX_NUM_RECORDS);
        if (status != 0) {
            App_Log(APP_ERROR, "%s: Unable to get list of records with etiket = \"%s\"\n", __func__, search_etiket);
            return -1;
        }

        const int num_expected = 2;
        if (num_found != num_expected) {
            App_Log(APP_ERROR, "%s: Found %d records, but should have been %d\nCriteria: etiket = \"%s\"\n",
                __func__, num_found, num_expected, search_etiket);
            return -1;
        }

        int ok = 1;
        char target_etiket[FST_ETIKET_LEN];
        char etiket0[FST_ETIKET_LEN];
        char etiket1[FST_ETIKET_LEN];
        char dummy_nomvar[FST_NOMVAR_LEN];
        char dummy_typvar[FST_TYPVAR_LEN];
        char dummy_grtyp[FST_GTYP_LEN];
        int dateo, deet, npas, nbits, datyp, ip1, ip2, ip3, ig1, ig2, ig3, ig4;
        int swa, lng, dltf, ubc, extra1, extra2, extra3;

        memset(etiket0, (int)' ', FST_ETIKET_LEN); etiket0[FST_ETIKET_LEN - 1] = '\0';
        memset(etiket1, (int)' ', FST_ETIKET_LEN); etiket1[FST_ETIKET_LEN - 1] = '\0';

        status = c_fstprm(records_found[0], &dateo, &deet, &npas, &ni, &nj, &nk, &nbits, &datyp, &ip1, &ip2, &ip3,
                          dummy_typvar, dummy_nomvar, etiket0, dummy_grtyp,
                          &ig1, &ig2, &ig3, &ig4, &swa, &lng, &dltf, &ubc, &extra1, &extra2, &extra3);
        if (status != 0) {
            App_Log(APP_ERROR,
                    "%s: Unable to retrieve parameters for first found record (handle %d), criteria: etiket = \"%s\"\n",
                    __func__, records_found[0], search_etiket);
            return -1;
        }

        status = c_fstprm(records_found[1], &dateo, &deet, &npas, &ni, &nj, &nk, &nbits, &datyp, &ip1, &ip2, &ip3,
                          dummy_typvar, dummy_nomvar, etiket1, dummy_grtyp,
                          &ig1, &ig2, &ig3, &ig4, &swa, &lng, &dltf, &ubc, &extra1, &extra2, &extra3);
        if (status != 0) {
            App_Log(APP_ERROR,
                    "%s: Unable to retrieve parameters for second found record (handle %d), criteria: etiket = \"%s\"\n",
                    __func__, records_found[1], search_etiket);
            return -1;
        }

        strcpy(target_etiket, base_etiket);
        target_etiket[i] = 'X';
        if (strcasecmp(etiket0, base_etiket)   != 0 && strcasecmp(etiket1, base_etiket)   != 0) ok = 0;
        if (strcasecmp(etiket0, target_etiket) != 0 && strcasecmp(etiket1, target_etiket) != 0) ok = 0;

        if (!ok) {
            App_Log(APP_ERROR, "The records found do not have the correct etikets: %s and %s, but expected %s and %s\n",
                etiket0, etiket1, target_etiket, base_etiket);
            return -1;
        }
    }

    // Testing typvar
    for (int i = 0; i < FST_TYPVAR_LEN - 1; i++) {
        const char* base_typvar = "AB";
        char search_typvar[FST_TYPVAR_LEN];
        strcpy(search_typvar, base_typvar);
        search_typvar[i] = '~';

        status = c_fstinl(
            iun, &ni, &nj, &nk, -1, "", -1, -1, -1, search_typvar, "", records_found, &num_found, MAX_NUM_RECORDS);
        if (status != 0) {
            App_Log(APP_ERROR, "%s: Unable to get list of records with typvar = \"%s\"\n", __func__, search_typvar);
            return -1;
        }

        const int num_expected = 2;
        if (num_found != num_expected) {
            App_Log(APP_ERROR, "%s: Found %d records, but should have been %d\nCriteria: typvar = \"%s\"\n",
                __func__, num_found, num_expected, search_typvar);
            return -1;
        }

        int ok = 1;
        char target_typvar[FST_TYPVAR_LEN];
        char typvar0[FST_TYPVAR_LEN];
        char typvar1[FST_TYPVAR_LEN];
        char dummy_nomvar[FST_NOMVAR_LEN];
        char dummy_etiket[FST_ETIKET_LEN];
        char dummy_grtyp[FST_GTYP_LEN];
        int dateo, deet, npas, nbits, datyp, ip1, ip2, ip3, ig1, ig2, ig3, ig4;
        int swa, lng, dltf, ubc, extra1, extra2, extra3;

        memset(typvar0, (int)' ', FST_TYPVAR_LEN); typvar0[FST_TYPVAR_LEN - 1] = '\0';
        memset(typvar1, (int)' ', FST_TYPVAR_LEN); typvar1[FST_TYPVAR_LEN - 1] = '\0';

        status = c_fstprm(records_found[0], &dateo, &deet, &npas, &ni, &nj, &nk, &nbits, &datyp, &ip1, &ip2, &ip3,
                          typvar0, dummy_nomvar, dummy_etiket, dummy_grtyp,
                          &ig1, &ig2, &ig3, &ig4, &swa, &lng, &dltf, &ubc, &extra1, &extra2, &extra3);
        if (status != 0) {
            App_Log(APP_ERROR,
                    "%s: Unable to retrieve parameters for first found record (handle %d), criteria: typvar = \"%s\"\n",
                    __func__, records_found[0], search_typvar);
            return -1;
        }

        status = c_fstprm(records_found[1], &dateo, &deet, &npas, &ni, &nj, &nk, &nbits, &datyp, &ip1, &ip2, &ip3,
                          typvar1, dummy_nomvar, dummy_etiket, dummy_grtyp,
                          &ig1, &ig2, &ig3, &ig4, &swa, &lng, &dltf, &ubc, &extra1, &extra2, &extra3);
        if (status != 0) {
            App_Log(APP_ERROR,
                    "%s: Unable to retrieve parameters for second found record (handle %d), criteria: typvar = \"%s\"\n",
                    __func__, records_found[1], search_typvar);
            return -1;
        }

        strcpy(target_typvar, base_typvar);
        target_typvar[i] = 'X';

        if (strcasecmp(typvar0, base_typvar)   != 0 && strcasecmp(typvar1, base_typvar)   != 0) ok = 0;
        if (strcasecmp(typvar0, target_typvar) != 0 && strcasecmp(typvar1, target_typvar) != 0) ok = 0;

        if (!ok) {
            App_Log(APP_ERROR, "The records found do not have the correct typvars: %s and %s, but expected %s and %s\n",
                typvar0, typvar1, target_typvar, base_typvar);
            return -1;
        }
    }

    if (!(c_fstfrm(iun) == 0 && c_fclos(iun) == 0)) {
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
