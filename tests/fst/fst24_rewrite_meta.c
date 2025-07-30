
#include <App.h>
#include <rmn.h>

const char* filename_rsf = "fst24_rewrite_meta.rsf";
const char* filename_xdf = "fst24_rewrite_meta.xdf";
const char* solution_filename_rsf = "fst24_rewrite_meta_solution.rsf";
const char* solution_filename_xdf = "fst24_rewrite_meta_solution.xdf";

static uint32_t dummy_data[] = {
    0x12341234,0x12341234,0x12341234,0x12341234,
    0x12341234,0x12341234,0x12341234,0x12341234,
    0x12341234,0x12341234,0x12341234,0x12341234,
    0x12341234,0x12341234,0x12341234,0x12341234,
    0x12341234,0x12341234,0x12341234,0x12341234,
    0x12341234,0x12341234,0x12341234,0x12341234,
    0x12341234,0x12341234,0x12341234,0x12341234,
    0x12341234,0x12341234,0x12341234,0x12341234,
};

int ORIGIN_DATE = 20250506;
int ORIGIN_TIME = 0;

static fst_record base_record = default_fst_record;
static fst_record new_record = default_fst_record;

#define WRITE_CHECK(file, rec) \
    if (fst24_write(file, rec, FST_NO) != TRUE) { \
        App_Log(APP_ERROR, "%s:%3d Unable to write record %d\n", __func__, __LINE__, (rec)->ip1); \
        return -1; \
    }

//! Create input test file and initialize base record.
static int create_file(const int is_rsf) {
    const char* filename = is_rsf ? filename_rsf : filename_xdf;
    const char* solution_filename = is_rsf ? solution_filename_rsf : solution_filename_xdf;
    const char* options = is_rsf ? "RSF+R/W" : "XDF+R/W";

    remove(filename);
    remove(solution_filename);
    fst_file* test_file = fst24_open(filename, options);
    if (test_file == NULL) {
        App_Log(APP_ERROR, "%s: Unable to open (create) file '%s'\n", __func__, filename);
        return -1;
    }

    fst_file* solution_file = fst24_open(solution_filename, options);
    if (solution_file == NULL) {
        App_Log(APP_ERROR, "%s: Unable to open (create) file '%s'\n", __func__, solution_filename);
        return -1;
    }


    base_record.data_type = FST_TYPE_UNSIGNED;
    base_record.data_bits = 32;
    base_record.pack_bits = 32;
    base_record.ni = 4;
    base_record.nj = 8;
    base_record.nk = 1;
    base_record.data = dummy_data;

    // Base values
    base_record.ip1 = 0;
    base_record.ip2 = 0;
    base_record.ip3 = 0;
    base_record.ig1 = 0;
    base_record.ig2 = 0;
    base_record.ig3 = 0;
    base_record.ig4 = 0;
    sprintf(base_record.nomvar, "AAAA");
    sprintf(base_record.typvar, "BB");
    sprintf(base_record.etiket, "CCCCDDDDEEEE");
    sprintf(base_record.grtyp, "9");
    base_record.npas = 10;
    base_record.deet = 24;
    const int mode = 3;
    newdate_c(&base_record.dateo, &ORIGIN_DATE, &ORIGIN_TIME, &mode);
    base_record.datev = get_valid_date32(base_record.dateo, base_record.deet, base_record.npas);

    for (int i = 0; i < 13; i++) {
        base_record.ip1 = i;
        if (fst24_write(test_file, &base_record, FST_NO) != TRUE) {
            App_Log(APP_ERROR, "%s: Unable to write record %d\n", __func__, i);
            return -1;
        }
    }

    // Modified values
    double increment = 6.0;
    incdatr_c(&new_record.datev, &base_record.datev, &increment);
    new_record.deet = base_record.deet + 24;
    new_record.npas = base_record.npas + 10;
    new_record.ip2 = base_record.ip2 + 12;
    new_record.ip3 = base_record.ip3 + 33;
    sprintf(new_record.typvar, "TV");
    sprintf(new_record.nomvar, "NAME");
    sprintf(new_record.etiket, "ETIKETTTTTTT");
    sprintf(new_record.grtyp, "0");
    new_record.ig1 = base_record.ig1 + 44;
    new_record.ig2 = base_record.ig2 + 55;
    new_record.ig3 = base_record.ig3 + 66;
    new_record.ig4 = base_record.ig4 + 77;

    fst_record solution_record = default_fst_record;
    solution_record.data = dummy_data;

    // datev
    fst24_record_copy_metadata(&solution_record, &base_record, FST_META_ALL);
    solution_record.ip1 = 0;
    solution_record.datev = new_record.datev;
    solution_record.dateo = get_origin_date32(solution_record.datev, solution_record.deet, solution_record.npas);
    WRITE_CHECK(solution_file, &solution_record);

    // deet
    fst24_record_copy_metadata(&solution_record, &base_record, FST_META_ALL);
    solution_record.ip1 = 1;
    solution_record.deet = new_record.deet;
    solution_record.dateo = get_origin_date32(solution_record.datev, solution_record.deet, solution_record.npas);
    WRITE_CHECK(solution_file, &solution_record);

    // npas
    fst24_record_copy_metadata(&solution_record, &base_record, FST_META_ALL);
    solution_record.ip1 = 2;
    solution_record.npas = new_record.npas;
    solution_record.dateo = get_origin_date32(solution_record.datev, solution_record.deet, solution_record.npas);
    WRITE_CHECK(solution_file, &solution_record);

    // ip2
    fst24_record_copy_metadata(&solution_record, &base_record, FST_META_ALL);
    solution_record.ip1 = 3;
    solution_record.ip2 = new_record.ip2;
    WRITE_CHECK(solution_file, &solution_record);

    // ip3
    fst24_record_copy_metadata(&solution_record, &base_record, FST_META_ALL);
    solution_record.ip1 = 4;
    solution_record.ip3 = new_record.ip3;
    WRITE_CHECK(solution_file, &solution_record);

    // typvar
    fst24_record_copy_metadata(&solution_record, &base_record, FST_META_ALL);
    solution_record.ip1 = 5;
    strcpy(solution_record.typvar, new_record.typvar);
    WRITE_CHECK(solution_file, &solution_record);

    // nomvar
    fst24_record_copy_metadata(&solution_record, &base_record, FST_META_ALL);
    solution_record.ip1 = 6;
    strcpy(solution_record.nomvar, new_record.nomvar);
    WRITE_CHECK(solution_file, &solution_record);

    // etiket
    fst24_record_copy_metadata(&solution_record, &base_record, FST_META_ALL);
    solution_record.ip1 = 7;
    strcpy(solution_record.etiket, new_record.etiket);
    WRITE_CHECK(solution_file, &solution_record);

    // grtyp
    fst24_record_copy_metadata(&solution_record, &base_record, FST_META_ALL);
    solution_record.ip1 = 8;
    strcpy(solution_record.grtyp, new_record.grtyp);
    WRITE_CHECK(solution_file, &solution_record);

    // ig1
    fst24_record_copy_metadata(&solution_record, &base_record, FST_META_ALL);
    solution_record.ip1 = 9;
    solution_record.ig1 = new_record.ig1;
    WRITE_CHECK(solution_file, &solution_record);

    // ig2
    fst24_record_copy_metadata(&solution_record, &base_record, FST_META_ALL);
    solution_record.ip1 = 10;
    solution_record.ig2 = new_record.ig2;
    WRITE_CHECK(solution_file, &solution_record);

    // ig3
    fst24_record_copy_metadata(&solution_record, &base_record, FST_META_ALL);
    solution_record.ip1 = 11;
    solution_record.ig3 = new_record.ig3;
    WRITE_CHECK(solution_file, &solution_record);

    // ig4
    fst24_record_copy_metadata(&solution_record, &base_record, FST_META_ALL);
    solution_record.ip1 = 12;
    solution_record.ig4 = new_record.ig4;
    WRITE_CHECK(solution_file, &solution_record);

    if (is_rsf) {
        json_object* ext_meta = Meta_New(META_TYPE_RECORD,NULL);
        if (ext_meta == NULL) {
            App_Log(APP_ERROR, "%s: Unable to create JSON meta object\n", __func__);
            return -1;
        }
        Meta_DefVar(ext_meta, "AAAA standard name", NULL, "A looooong name",
            "Variable used for testing meta rewrite", "bytes");
        Meta_From89(ext_meta, &base_record);

        // App_Log(APP_ALWAYS, "%s: meta = %s\n", __func__, Meta_Stringify(ext_meta, JSON_C_TO_STRING_PRETTY));
        base_record.metadata = ext_meta;

        // App_Log(APP_ALWAYS, "%s: meta = %s\n", __func__, Meta_Stringify(new_record.metadata, JSON_C_TO_STRING_PRETTY));

        fst24_record_copy_metadata(&solution_record, &base_record, FST_META_ALL);
        solution_record.ip1 = 13;
        solution_record.metadata = Meta_Copy(ext_meta);
        Meta_DefVar(solution_record.metadata, NULL, NULL, NULL, "New description", NULL);
        WRITE_CHECK(solution_file, &solution_record);

        fst24_record_copy_metadata(&new_record, &solution_record, FST_META_EXT);

        base_record.ip1 = 13;
        WRITE_CHECK(test_file, &base_record);
    }
    else {
        base_record.ip1 = 13;
        WRITE_CHECK(test_file, &base_record);

        fst24_record_copy_metadata(&solution_record, &base_record, FST_META_ALL);
        solution_record.ip1 = 13;
        WRITE_CHECK(solution_file, &solution_record);
    }

    if (fst24_close(test_file) != TRUE) {
        App_Log(APP_ERROR, "%s: Unable to close file '%s'\n", __func__, filename);
        return -1;
    }

    if (fst24_close(solution_file) != TRUE) {
        App_Log(APP_ERROR, "%s: Unable to close file '%s'\n", __func__, solution_filename);
        return -1;
    }

    return 0;
}

static int get_record(fst_file* f, const int ip1, fst_record* rec) {
    fst_record criteria = default_fst_record;
    criteria.ip1 = ip1;
    fst_query* q = fst24_new_query(f, &criteria, NULL);
    if (fst24_find_next(q, rec) != TRUE) {
        App_Log(APP_ERROR, "%s: Unable to find record with ip1 %d\n", __func__, ip1);
        return -1;
    }
    fst24_query_free(q);
    return 0;
}

static int run_test(const int is_rsf) {
    App_Log(APP_INFO, "%s: Running %s test\n", __func__, is_rsf ? "RSF" : "XDF");
    if (create_file(is_rsf) != 0) return -1;

    const char* test_filename = is_rsf ? filename_rsf : filename_xdf;
    const char* solution_filename = is_rsf ? solution_filename_rsf : solution_filename_xdf;

    fst_record rec = default_fst_record;
    fst_record solution_rec = default_fst_record;

    fst_file* test_file = fst24_open(test_filename, "R/W");
    if (test_file == NULL) {
        App_Log(APP_ERROR, "%s: fst24_open error\n", __func__);
        return -1;
    }

    fst_file* solution_file = fst24_open(solution_filename, NULL);
    if (solution_file == NULL) {
        App_Log(APP_ERROR, "%s: fst24_open error\n", __func__);
        return -1;
    }

    // Try on a read-only file (should fail)
    get_record(solution_file, 0, &solution_rec);
    App_Log(APP_ALWAYS, "%s: Expecting error \n", __func__);
    if (fst24_write(solution_file, &solution_rec, FST_REWRITE_META) == TRUE) {
        App_Log(APP_ERROR, "%s: Call to fst24_write with FST_REWRITE_META on a read-only file should fail\n",
                __func__);
        return -1;
    }

    // Try an uninitialized record (should fail)
    App_Log(APP_ALWAYS, "%s: Expecting error \n", __func__);
    if (fst24_write(test_file, &rec, FST_REWRITE_META) == TRUE) {
        App_Log(APP_ERROR, "%s: Call to fst24_write with FST_REWRITE_META with a blank record should fail\n",
                __func__);
        return -1;
    }

    // Try a record in a wrong file (should fail)
    App_Log(APP_ALWAYS, "%s: Expecting error \n", __func__);
    if (fst24_write(test_file, &solution_rec, FST_REWRITE_META) == TRUE) {
        App_Log(APP_ERROR, "%s: Call to fst24_write with FST_REWRITE_META with a record in the wrong file should fail\n",
                __func__);
        return -1;
    }


    // date (valid)
    {
        if (get_record(test_file, 0, &rec) != 0) return -1;
        rec.datev = new_record.datev;
        if (fst24_write(test_file, &rec, FST_REWRITE_META) != TRUE) {
            App_Log(APP_ERROR, "%s: Call to rewrite date failed\n", __func__);
            return -1;
        }
    }

    // deet
    {
        if (get_record(test_file, 1, &rec) != 0) return -1;
        rec.deet = new_record.deet;
        if (fst24_write(test_file, &rec, FST_REWRITE_META) != TRUE) {
            App_Log(APP_ERROR, "%s: Call to rewrite deet failed\n", __func__);
            return -1;
        }
    }

    // npas
    {
        if (get_record(test_file, 2, &rec) != 0) return -1;
        rec.npas = new_record.npas;
        if (fst24_write(test_file, &rec, FST_REWRITE_META) != TRUE) {
            App_Log(APP_ERROR, "%s: Call to rewrite npas failed\n", __func__);
            return -1;
        }
    }

    // ip2
    {
        if (get_record(test_file, 3, &rec) != 0) return -1;
        rec.ip2 = new_record.ip2;
        if (fst24_write(test_file, &rec, FST_REWRITE_META) != TRUE) {
            App_Log(APP_ERROR, "%s: Call to rewrite ip2 failed\n", __func__);
            return -1;
        }
    }

    // ip3
    {
        if (get_record(test_file, 4, &rec) != 0) return -1;
        rec.ip3 = new_record.ip3;
        if (fst24_write(test_file, &rec, FST_REWRITE_META) != TRUE) {
            App_Log(APP_ERROR, "%s: Call to rewrite ip3 failed\n", __func__);
            return -1;
        }
    }

    // typvar
    {
        if (get_record(test_file, 5, &rec) != 0) return -1;
        strcpy(rec.typvar, new_record.typvar);
        if (fst24_write(test_file, &rec, FST_REWRITE_META) != TRUE) {
            App_Log(APP_ERROR, "%s: Call to rewrite typvar failed\n", __func__);
            return -1;
        }
    }

    // nomvar
    {
        if (get_record(test_file, 6, &rec) != 0) return -1;
        strcpy(rec.nomvar, new_record.nomvar);
        if (fst24_write(test_file, &rec, FST_REWRITE_META) != TRUE) {
            App_Log(APP_ERROR, "%s: Call to rewrite nomvar failed\n", __func__);
            return -1;
        }
    }

    // etiket
    {
        if (get_record(test_file, 7, &rec) != 0) return -1;
        strcpy(rec.etiket, new_record.etiket);
        if (fst24_write(test_file, &rec, FST_REWRITE_META) != TRUE) {
            App_Log(APP_ERROR, "%s: Call to rewrite etiket failed\n", __func__);
            return -1;
        }
    }

    // grtyp
    {
        if (get_record(test_file, 8, &rec) != 0) return -1;
        strcpy(rec.grtyp, new_record.grtyp);
        if (fst24_write(test_file, &rec, FST_REWRITE_META) != TRUE) {
            App_Log(APP_ERROR, "%s: Call to rewrite grtyp failed\n", __func__);
            return -1;
        }
    }

    // ig1
    {
        if (get_record(test_file, 9, &rec) != 0) return -1;
        rec.ig1 = new_record.ig1;
        if (fst24_write(test_file, &rec, FST_REWRITE_META) != TRUE) {
            App_Log(APP_ERROR, "%s: Call to rewrite ig1 failed\n", __func__);
            return -1;
        }
    }

    // ig2
    {
        if (get_record(test_file, 10, &rec) != 0) return -1;
        rec.ig2 = new_record.ig2;
        if (fst24_write(test_file, &rec, FST_REWRITE_META) != TRUE) {
            App_Log(APP_ERROR, "%s: Call to rewrite ig2 failed\n", __func__);
            return -1;
        }
    }

    // ig3
    {
        if (get_record(test_file, 11, &rec) != 0) return -1;
        rec.ig3 = new_record.ig3;
        if (fst24_write(test_file, &rec, FST_REWRITE_META) != TRUE) {
            App_Log(APP_ERROR, "%s: Call to rewrite ig3 failed\n", __func__);
            return -1;
        }
    }

    // ig4
    {
        if (get_record(test_file, 12, &rec) != 0) return -1;
        rec.ig4 = new_record.ig4;
        if (fst24_write(test_file, &rec, FST_REWRITE_META) != TRUE) {
            App_Log(APP_ERROR, "%s: Call to rewrite ig4 failed\n", __func__);
            return -1;
        }
    }

    // extended metadata
    if (is_rsf) {
        if (get_record(test_file, 13, &rec) != 0) return -1;
        rec.metadata = new_record.metadata;
        if (fst24_write(test_file, &rec, FST_REWRITE_META) != TRUE) {
            App_Log(APP_ERROR, "%s: Call to rewrite extended meta failed\n", __func__);
            return -1;
        }

        json_object* big_meta = Meta_Copy(base_record.metadata);
        Meta_DefVar(big_meta, NULL, "AAAA_with more characters", NULL, NULL, NULL);
        rec.metadata = big_meta;
        App_Log(APP_ALWAYS, "%s: Expecting error\n", __func__);
        if (fst24_write(test_file, &rec, FST_REWRITE_META) == TRUE) {
            App_Log(APP_ERROR, "%s: Call to rewrite larger extended meta succeeded (should have failed)\n", __func__);
            return -1;
        }
    }

    // Check all edited records
    for (int i = 0; i < 14; i++) {
        if (get_record(test_file, i, &rec) != 0) return -1;
        if (get_record(solution_file, i, &solution_rec) != 0) return -1;

        if (is_rsf) {
            fst24_read_metadata(&rec);
            fst24_read_metadata(&solution_rec);
        }
        if (!fst24_record_has_same_meta(&rec, &solution_rec)) {
            App_Log(APP_ERROR, "%s: Record %d has the wrong info!\n", __func__, i);
            fst24_record_diff(&solution_rec, &rec);
            return -1;
        }
    }

    if (fst24_close(test_file) != TRUE) {
        App_Log(APP_ERROR, "%s: Closing file failed\n", __func__);
        return -1;
    }

    // Now check again, after having closed the file
    test_file = fst24_open(test_filename, NULL);
    if (test_file == NULL) {
        App_Log(APP_ERROR, "%s: fst24_open error\n", __func__);
        return -1;
    }

    // Check all edited records
    for (int i = 0; i < 14; i++) {
        get_record(test_file, i, &rec);
        get_record(solution_file, i, &solution_rec);

        if (is_rsf) {
            fst24_read_metadata(&rec);
            fst24_read_metadata(&solution_rec);
        }
        if (!fst24_record_has_same_meta(&rec, &solution_rec)) {
            App_Log(APP_ERROR, "%s: Record %d is different\n", __func__, i);
            return -1;
        }

        // Check that the data is still the same
        fst24_read_record(&rec);
        if (memcmp(rec.data, dummy_data, sizeof(dummy_data)) != 0) {
            App_Log(APP_ERROR, "%s: Record %d data does not match after closing file\n", __func__, i);
            return -1;
        }
    }

    fst24_record_free(&rec);
    fst24_record_free(&solution_rec);

    if (fst24_close(test_file) != TRUE) {
        App_Log(APP_ERROR, "%s: Could not close test file\n", __func__);
        return -1;
    }

    if (fst24_close(solution_file) != TRUE) {
        App_Log(APP_ERROR, "%s: Could not close solution file\n", __func__);
        return -1;
    }

    return 0;
}

int main(void) {
    if (run_test(0) != 0) return -1;
    if (run_test(1) != 0) return -1;
    App_Log(APP_ALWAYS, "%s: Test successful\n", __func__);
    
    fst24_record_free(&base_record);
    fst24_record_free(&new_record);
    return 0;
}
