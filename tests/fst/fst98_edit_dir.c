
#include <App.h>
#include <rmn.h>

const char* filename_rsf = "fst_edit_dir.rsf";
const char* filename_xdf = "fst_edit_dir.xdf";
const char* solution_filename_rsf = "fst_edit_dir_solution.rsf";
const char* solution_filename_xdf = "fst_edit_dir_solution.xdf";

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

    for (int i = 0; i < 20; i++) {
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

//! Retrieve record based on IP1
static int get_record(const int iun, const int ip1) {
    int ni, nj, nk;
    const int handle = c_fstinf(iun, &ni, &nj, &nk, -1, "", ip1, -1, -1, "", "");
    if (handle < 0) {
        App_Log(APP_ERROR, "%s: Unable to get record with ip1 %d\n", __func__, ip1);
    }
    return handle;
}

static int compare_records(const int handle_a, const int handle_b) {
    int date_a, deet_a, npas_a, ni_a, nj_a, nk_a, nbits_a, datyp_a, ip1_a, ip2_a, ip3_a, ig1_a, ig2_a, ig3_a, ig4_a;
    int date_b, deet_b, npas_b, ni_b, nj_b, nk_b, nbits_b, datyp_b, ip1_b, ip2_b, ip3_b, ig1_b, ig2_b, ig3_b, ig4_b;
    char typvar_a[FST_TYPVAR_LEN], typvar_b[FST_TYPVAR_LEN];
    char nomvar_a[FST_NOMVAR_LEN], nomvar_b[FST_NOMVAR_LEN];
    char etiket_a[FST_ETIKET_LEN], etiket_b[FST_ETIKET_LEN];
    char grtyp_a[FST_GTYP_LEN], grtyp_b[FST_GTYP_LEN];
    int swa_a, lng_a, dltf_a, ubc_a, extra1_a, extra2_a, extra3_a;
    int swa_b, lng_b, dltf_b, ubc_b, extra1_b, extra2_b, extra3_b;
    
    sprintf(typvar_a, "  ");
    sprintf(typvar_b, "  ");
    sprintf(nomvar_a, "    ");
    sprintf(nomvar_b, "    ");
    sprintf(etiket_a, "            ");
    sprintf(etiket_b, "            ");
    sprintf(grtyp_a, " ");
    sprintf(grtyp_b, " ");

    c_fstprm(handle_a, &date_a, &deet_a, &npas_a, &ni_a, &nj_a, &nk_a, &nbits_a, &datyp_a, &ip1_a, &ip2_a, &ip3_a, typvar_a,
        nomvar_a, etiket_a, grtyp_a, &ig1_a, &ig2_a, &ig3_a, &ig4_a,
        &swa_a, &lng_a, &dltf_a, &ubc_a, &extra1_a, &extra2_a, &extra3_a);
    c_fstprm(handle_b, &date_b, &deet_b, &npas_b, &ni_b, &nj_b, &nk_b, &nbits_b, &datyp_b, &ip1_b, &ip2_b, &ip3_b, typvar_b,
        nomvar_b, etiket_b, grtyp_b, &ig1_b, &ig2_b, &ig3_b, &ig4_b,
        &swa_b, &lng_b, &dltf_b, &ubc_b, &extra1_b, &extra2_b, &extra3_b);

    // App_Log(APP_ALWAYS, "%s: handle %d, date %d, deet %d, npas %d, nijk %d %d %d\n",
    //         __func__, handle_a, date_a, deet_a, npas_a, ni_a, nj_a, nk_a);
    // App_Log(APP_ALWAYS, "%s: handle %d, date %d, deet %d, npas %d, nijk %d %d %d\n",
    //         __func__, handle_b, date_b, deet_b, npas_b, ni_b, nj_b, nk_b);

    if (date_a != date_b) { App_Log(APP_ERROR, "%s: %d Wrong date (expected %d, got %d)\n", __func__, ip1_a, date_b, date_a); return -1; }
    if (deet_a != deet_b) { App_Log(APP_ERROR, "%s: %d Wrong deet (expected %d, got %d)\n", __func__, ip1_a, deet_b, deet_a); return -1; }
    if (npas_a != npas_b) { App_Log(APP_ERROR, "%s: %d Wrong npas (expected %d, got %d)\n", __func__, ip1_a, npas_b, npas_a); return -1; }
    if (ni_a != ni_b) { App_Log(APP_ERROR, "%s: %d Wrong ni (expected %d, got %d)\n", __func__, ip1_a, ni_b, ni_a); return -1; }
    if (nj_a != nj_b) { App_Log(APP_ERROR, "%s: %d Wrong nj (expected %d, got %d)\n", __func__, ip1_a, nj_b, nj_a); return -1; }
    if (nk_a != nk_b) { App_Log(APP_ERROR, "%s: %d Wrong nk (expected %d, got %d)\n", __func__, ip1_a, nk_b, nk_a); return -1; }
    if (nbits_a != nbits_b) { App_Log(APP_ERROR, "%s: %d Wrong nbits (expected %d, got %d)\n", __func__, ip1_a, nbits_b, nbits_a); return -1; }
    if (datyp_a != datyp_b) { App_Log(APP_ERROR, "%s: %d Wrong datyp (expected %d, got %d)\n", __func__, ip1_a, datyp_b, datyp_a); return -1; }
    if (ip2_a != ip2_b) { App_Log(APP_ERROR, "%s: %d Wrong ip2 (expected %d, got %d)\n", __func__, ip1_a, ip2_b, ip2_a); return -1; }
    if (ip3_a != ip3_b) { App_Log(APP_ERROR, "%s: %d Wrong ip3 (expected %d, got %d)\n", __func__, ip1_a, ip3_b, ip3_a); return -1; }
    if (ig1_a != ig1_b) { App_Log(APP_ERROR, "%s: %d Wrong ig1 (expected %d, got %d)\n", __func__, ip1_a, ig1_b, ig1_a); return -1; }
    if (ig2_a != ig2_b) { App_Log(APP_ERROR, "%s: %d Wrong ig2 (expected %d, got %d)\n", __func__, ip1_a, ig2_b, ig2_a); return -1; }
    if (ig3_a != ig3_b) { App_Log(APP_ERROR, "%s: %d Wrong ig3 (expected %d, got %d)\n", __func__, ip1_a, ig3_b, ig3_a); return -1; }
    if (ig4_a != ig4_b) { App_Log(APP_ERROR, "%s: %d Wrong ig4 (expected %d, got %d)\n", __func__, ip1_a, ig4_b, ig4_a); return -1; }
    if (strcmp(typvar_a, typvar_b) != 0) { App_Log(APP_ERROR, "%s: %d Wrong typvar (expected %s, got %s)\n", __func__, ip1_a, typvar_b, typvar_a); return -1; }
    if (strcmp(nomvar_a, nomvar_b) != 0) { App_Log(APP_ERROR, "%s: %d Wrong nomvar (expected %s, got %s)\n", __func__, ip1_a, nomvar_b, nomvar_a); return -1; }
    if (strcmp(etiket_a, etiket_b) != 0) { App_Log(APP_ERROR, "%s: %d Wrong etiket (expected %s, got %s)\n", __func__, ip1_a, etiket_b, etiket_a); return -1; }
    if (strcmp(grtyp_a, grtyp_b) != 0) { App_Log(APP_ERROR, "%s: %d Wrong grtyp (expected %s, got %s)\n", __func__, ip1_a, grtyp_b, grtyp_a); return -1; }
    return 0;
}

static int run_test(const int is_rsf) {
    App_Log(APP_INFO, "%s: Running %s test\n", __func__, is_rsf ? "RSF" : "XDF");
    if (create_file(is_rsf) != 0) return -1;

    const char* test_filename = is_rsf ? filename_rsf : filename_xdf;
    const char* solution_filename = is_rsf ? solution_filename_rsf : solution_filename_xdf;

    int iun = 0;
    
    // Try on a read-only file first. It should fail
    if (c_fnom(&iun, test_filename, "RND+R/O", 0) != 0) {
        App_Log(APP_ERROR, "%s: fnom error\n", __func__);
        return -1;
    }
    if (c_fstouv(iun, "RND+R/O") < 0) {
        App_Log(APP_ERROR, "%s: fstouv error\n", __func__);
        return -1;
    }
    {
        const int handle = get_record(iun, 0);
        App_Log(APP_ALWAYS, "%s: Expecting error \n", __func__);
        const int result = c_fst_edit_dir(handle, -1, -1, -1, -1, -1, -1, -1, -1, -1, "", "", "", "", -1, -1, -1, -1, -1);
        if (result >= 0) {
            App_Log(APP_ERROR, "%s: Call to c_fst_edit_dir on a read-only file should fail\n", __func__);
            return -1;
        }
    }
    c_fstfrm(iun);
    c_fclos(iun);

    iun = 0;
    if (c_fnom(&iun, test_filename, "RND+R/W", 0) != 0) {
        App_Log(APP_ERROR, "%s: fnom error\n", __func__);
        return -1;
    }
    if (c_fstouv(iun, "RND+R/W") < 0) {
        App_Log(APP_ERROR, "%s: fstouv error\n", __func__);
        return -1;
    }

    int iun_sol = 0;
    if (c_fnom(&iun_sol, solution_filename, "RND+R/O", 0) != 0) {
        App_Log(APP_ERROR, "%s: fnom error\n", __func__);
        return -1;
    }
    if (c_fstouv(iun_sol, "RND+R/O") < 0) {
        App_Log(APP_ERROR, "%s: fstouv error\n", __func__);
        return -1;
    }

    // date (valid)
    {
        const int handle = get_record(iun, 0);
        if (handle < 0) return -1;
        if (c_fst_edit_dir(handle, new_record.datev, -1, -1, -1, -1, -1, -1, -1, -1, "", "", "", "",
                           -1, -1, -1, -1, -1) < 0) {
            App_Log(APP_ERROR, "%s: Call to edit date failed\n", __func__);
            return -1;
        }
    }

    // deet
    {
        const int handle = get_record(iun, 1);
        if (handle < 0) return -1;
        if (c_fst_edit_dir(handle, -1, new_record.deet, -1, -1, -1, -1, -1, -1, -1, "", "", "", "",
                           -1, -1, -1, -1, -1) < 0) {
            App_Log(APP_ERROR, "%s: Call to edit deet failed\n", __func__);
            return -1;
        }
    }

    // npas
    {
        const int handle = get_record(iun, 2);
        if (handle < 0) return -1;
        if (c_fst_edit_dir(handle, -1, -1, new_record.npas, -1, -1, -1, -1, -1, -1, "", "", "", "",
                           -1, -1, -1, -1, -1) < 0) {
            App_Log(APP_ERROR, "%s: Call to edit npas failed\n", __func__);
            return -1;
        }
    }

    // ip2
    {
        const int handle = get_record(iun, 3);
        if (handle < 0) return -1;
        if (c_fst_edit_dir(handle, -1, -1, -1, -1, -1, -1, -1, new_record.ip2, -1, "", "", "", "",
                           -1, -1, -1, -1, -1) < 0) {
            App_Log(APP_ERROR, "%s: Call to edit ip2 failed\n", __func__);
            return -1;
        }
    }

    // ip3
    {
        const int handle = get_record(iun, 4);
        if (handle < 0) return -1;
        if (c_fst_edit_dir(handle, -1, -1, -1, -1, -1, -1, -1, -1, new_record.ip3, "", "", "", "",
                           -1, -1, -1, -1, -1) < 0) {
            App_Log(APP_ERROR, "%s: Call to edit ip3 failed\n", __func__);
            return -1;
        }
    }

    // typvar
    {
        const int handle = get_record(iun, 5);
        if (handle < 0) return -1;
        if (c_fst_edit_dir(handle, -1, -1, -1, -1, -1, -1, -1, -1, -1, new_record.typvar, "", "", "",
                           -1, -1, -1, -1, -1) < 0) {
            App_Log(APP_ERROR, "%s: Call to edit typvar failed\n", __func__);
            return -1;
        }
    }

    // nomvar
    {
        const int handle = get_record(iun, 6);
        if (handle < 0) return -1;
        if (c_fst_edit_dir(handle, -1, -1, -1, -1, -1, -1, -1, -1, -1, "", new_record.nomvar, "", "",
                           -1, -1, -1, -1, -1) < 0) {
            App_Log(APP_ERROR, "%s: Call to edit nomvar failed\n", __func__);
            return -1;
        }
    }

    // etiket
    {
        const int handle = get_record(iun, 7);
        if (handle < 0) return -1;
        if (c_fst_edit_dir(handle, -1, -1, -1, -1, -1, -1, -1, -1, -1, "", "", new_record.etiket, "",
                           -1, -1, -1, -1, -1) < 0) {
            App_Log(APP_ERROR, "%s: Call to edit etiket failed\n", __func__);
            return -1;
        }
    }

    // grtyp
    {
        const int handle = get_record(iun, 8);
        if (handle < 0) return -1;
        if (c_fst_edit_dir_plus(handle, -1, -1, -1, -1, -1, -1, -1, -1, -1, "", "", "", new_record.grtyp,
                           -1, -1, -1, -1, -1) < 0) {
            App_Log(APP_ERROR, "%s: Call to edit grtyp failed\n", __func__);
            return -1;
        }
    }

    // ig1
    {
        const int handle = get_record(iun, 9);
        if (handle < 0) return -1;
        if (c_fst_edit_dir(handle, -1, -1, -1, -1, -1, -1, -1, -1, -1, "", "", "", "",
                           new_record.ig1, -1, -1, -1, -1) < 0) {
            App_Log(APP_ERROR, "%s: Call to edit ig1 failed\n", __func__);
            return -1;
        }
    }

    // ig2
    {
        const int handle = get_record(iun, 10);
        if (handle < 0) return -1;
        if (c_fst_edit_dir(handle, -1, -1, -1, -1, -1, -1, -1, -1, -1, "", "", "", "",
                           -1, new_record.ig2, -1, -1, -1) < 0) {
            App_Log(APP_ERROR, "%s: Call to edit ig2 failed\n", __func__);
            return -1;
        }
    }

    // ig3
    {
        const int handle = get_record(iun, 11);
        if (handle < 0) return -1;
        if (c_fst_edit_dir(handle, -1, -1, -1, -1, -1, -1, -1, -1, -1, "", "", "", "",
                           -1, -1, new_record.ig3, -1, -1) < 0) {
            App_Log(APP_ERROR, "%s: Call to edit ig3 failed\n", __func__);
            return -1;
        }
    }

    // ig4
    {
        const int handle = get_record(iun, 12);
        if (handle < 0) return -1;
        if (c_fst_edit_dir(handle, -1, -1, -1, -1, -1, -1, -1, -1, -1, "", "", "", "",
                           -1, -1, -1, new_record.ig4, -1) < 0) {
            App_Log(APP_ERROR, "%s: Call to edit ig4 failed\n", __func__);
            return -1;
        }
    }

    
    // Check all edited records
    for (int i = 0; i < 13; i++) {
        const int handle_test = get_record(iun, i);
        const int handle_sol = get_record(iun_sol, i);

        if (compare_records(handle_test, handle_sol) != 0) {
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

    // Now check again, after having closed the file
    iun = 0;
    if (c_fnom(&iun, test_filename, "RND+R/O", 0) != 0) {
        App_Log(APP_ERROR, "%s: fnom error\n", __func__);
        return -1;
    }
    if (c_fstouv(iun, "RND+R/O") < 0) {
        App_Log(APP_ERROR, "%s: fstouv error\n", __func__);
        return -1;
    }

    // Check all edited records
    for (int i = 0; i < 13; i++) {
        const int handle_test = get_record(iun, i);
        const int handle_sol = get_record(iun_sol, i);

        if (compare_records(handle_test, handle_sol) != 0) {
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


    if (c_fstfrm(iun_sol) < 0) {
        App_Log(APP_ERROR, "%s: fstfrm error\n", __func__);
        return -1;
    }
    if (c_fclos(iun_sol) != 0) {
        App_Log(APP_ERROR, "%s: fclos error\n", __func__);
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
