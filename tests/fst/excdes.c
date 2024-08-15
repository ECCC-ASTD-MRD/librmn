#include <App.h>
#include <rmn.h>
#include <rmn/excdes_new.h>

const char* test_filename_rsf = "excdes.rsf";
const char* test_filename_xdf = "excdes.xdf";

int create_file(const int is_rsf) {
    const char* test_filename = is_rsf ? test_filename_rsf : test_filename_xdf;
    const char* options = is_rsf ? "RSF+R/W" : "XDF+R/W";

    remove(test_filename);
    fst_file* test_file = fst24_open(test_filename, options);
    if (test_file == NULL) {
        App_Log(APP_ERROR, "%s: Unable to open (create) file '%s'\n", __func__, test_filename);
        return -1;
    }

    const int32_t NUM_ELEM = 100;
    float dummy_data[NUM_ELEM];

    fst_record rec = default_fst_record;
    rec.ni = NUM_ELEM;
    rec.nj = 1;
    rec.nk = 1;
    rec.data_type = FST_TYPE_REAL;
    rec.data_bits = 32;
    rec.pack_bits = 32;
    rec.data = dummy_data;

    rec.dateo = 1;
    rec.deet = 1;
    rec.npas = 1;
    rec.ig1 = 1;
    rec.ig2 = 1;
    rec.ig3 = 1;
    rec.ig4 = 1;

    rec.ip1 = 100;
    rec.ip3 = 100;
    for (int i = 0; i < 5; i++)
    {
        rec.ip1++;
        rec.ip2 = 100;
        for (int j = 0; j < 5; j++)
        {
            rec.ip2++;
            rec.ip3 = 100;
            for (int k = 0; k < 5; k++)
            {
                rec.ip3++;
                if (fst24_write(test_file, &rec, 0) <= 0) {
                    App_Log(APP_ERROR, "%s: Could not write record in test file %s\n", __func__, test_filename);
                    return -1;
                }
                if (k == 2) rec.ip3 = 200000;
            }
            if (j == 2) rec.ip2 = 200000;
        }
        if (i == 2) rec.ip1 = 200000;
    }

    fst24_close(test_file);

    return 0;
}

int validate_num_found(const int32_t num_found, const int32_t num_expected) {
    
    if (num_found == num_expected) return 0;

    App_Log(APP_ERROR, "%s: Expected %d, but found %d\n", __func__, num_expected, num_found);

    return -1;
}

int test_excdes(const int is_rsf) {
    if (create_file(is_rsf) < 0) return -1;

    const char* test_filename = is_rsf ? test_filename_rsf : test_filename_xdf;

    fst_file* test_file = fst24_open(test_filename, NULL);
    fst_query* q = fst24_new_query(test_file, NULL, NULL);

    int ips[5];

    // Basic range request on 1 IP
    RequetesInit();
    ips[0] = 101;
    ips[1] = READLX_RANGE;
    ips[2] = 102;
    Xc_Select_ip1(0, 1, ips, 3);
    if (validate_num_found(fst24_find_count(q), 50) != 0) return -1;

    // Combination of range request on ip1 OR a specific ip2
    ips[0] = 200001;
    Xc_Select_ip2(1, 1, ips, 1);
    if (validate_num_found(fst24_find_count(q), 50 + 15) != 0) return -1;

    // Combination of range request on ip1 OR (a specific ip2 AND a specific ip3)
    Xc_Select_ip3(1, 1, ips, 1);
    if (validate_num_found(fst24_find_count(q), 50 + 3) != 0) return -1;

    // Any record with NOT a specific ip2
    RequetesInit();
    Xc_Select_ip2(0, 0, ips, 1);
    if (validate_num_found(fst24_find_count(q), 100) != 0) return -1;

    // Any record with NOT (a specific ip2 AND a specific ip3)
    Xc_Select_ip3(0, 0, ips, 1);
    if (validate_num_found(fst24_find_count(q), 120) != 0) return -1;

    // I don't get this one. I would think it's
    // {NOT (specific ip2 AND specific ip3)} OR (range of ip1) OR (other range of ip1)
    // but it seems to filter out the 'excluded' first before checking the ranges
    ips[0] = 100; ips[2] = 101;
    Xc_Select_ip1(1, 1, ips, 3);
    ips[0] = 200001; ips[2] = 200002;
    Xc_Select_ip1(2, 1, ips, 3);
    if (validate_num_found(fst24_find_count(q), 72) != 0) return -1;

    // A range ip1 with a delta
    RequetesInit();
    ips[0] = 100;
    ips[1] = READLX_RANGE;
    ips[2] = 102;
    ips[3] = READLX_DELTA;
    ips[4] = 2;
    Xc_Select_ip1(0, 1, ips, 5);
    if (validate_num_found(fst24_find_count(q), 25) != 0) return -1;

    // Another range, ip2, with delta
    RequetesInit();
    ips[2] = 101;
    Xc_Select_ip2(0, 1, ips, 5);
    if (validate_num_found(fst24_find_count(q), 0) != 0) return -1;

    // A range ip3 (with unrecognized encodings)
    RequetesInit();
    ips[0] = 200001;
    ips[2] = 200002;
    Xc_Select_ip3(0, 1, ips, 3);
    if (validate_num_found(fst24_find_count(q), 50) != 0) return -1;

    // A range ip3, with a delta (with unrecognized encodings)
    RequetesInit();
    ips[0] = 200001;
    ips[2] = 200003;
    ips[4] = 2;
    Xc_Select_ip3(0, 1, ips, 5);
    if (validate_num_found(fst24_find_count(q), 25) != 0) return -1;

    fst24_query_free(q);
    fst24_close(test_file);

    return 0;
}

int test_excdes_98(const int is_rsf) {
    if (create_file(is_rsf) < 0) return -1;

    const char* test_filename = is_rsf ? test_filename_rsf : test_filename_xdf;

    int iun = 0;
    if (c_fnom(&iun, test_filename, "STD+RND+R/O", 0) != 0) {
        App_Log(APP_ERROR, "%s: Error with fnom\n", __func__);
        return -1;
    }
    App_Log(APP_VERBATIM, "%s: iun = %d\n", __func__, iun);
    if (c_fstouv(iun, "STD+RND+R/O") < 0) {
        App_Log(APP_ERROR, "%s: Error with fstouv\n", __func__);
        return -1;
    }

    int ips[5];

    const int MAX_NUM_ITEMS = 10000;
    int list[MAX_NUM_ITEMS];
    int num_items;
    int status;
    int ni, nj, nk;

    // Basic range request on 1 IP
    RequetesInit();
    ips[0] = 101;
    ips[1] = READLX_RANGE;
    ips[2] = 102;
    Xc_Select_ip1(0, 1, ips, 3);

    status = c_fstinl(iun, &ni, &nj, &nk, -1, "            ", -1, -1, -1, "  ", "    ", list, &num_items, MAX_NUM_ITEMS);
    if (validate_num_found(status, 0) != 0) return -1;
    if (validate_num_found(num_items, 50) != 0) return -1;

    void* q = NULL;

    // Combination of range request on ip1 OR a specific ip2
    ips[0] = 200001;
    Xc_Select_ip2(1, 1, ips, 1);
    status = c_fstinl(iun, &ni, &nj, &nk, -1, "            ", -1, -1, -1, "  ", "    ", list, &num_items, MAX_NUM_ITEMS);
    if (validate_num_found(status, 0) != 0) return -1;
    if (validate_num_found(num_items, 50 + 15) != 0) return -1;

    // Combination of range request on ip1 OR (a specific ip2 AND a specific ip3)
    Xc_Select_ip3(1, 1, ips, 1);
    status = c_fstinl(iun, &ni, &nj, &nk, -1, "            ", -1, -1, -1, "  ", "    ", list, &num_items, MAX_NUM_ITEMS);
    if (validate_num_found(status, 0) != 0) return -1;
    if (validate_num_found(num_items, 50 + 3) != 0) return -1;

    // Any record with NOT a specific ip2
    RequetesInit();
    Xc_Select_ip2(0, 0, ips, 1);
    status = c_fstinl(iun, &ni, &nj, &nk, -1, "            ", -1, -1, -1, "  ", "    ", list, &num_items, MAX_NUM_ITEMS);
    if (validate_num_found(status, 0) != 0) return -1;
    if (validate_num_found(num_items, 100) != 0) return -1;

    // Any record with NOT (a specific ip2 AND a specific ip3)
    Xc_Select_ip3(0, 0, ips, 1);
    status = c_fstinl(iun, &ni, &nj, &nk, -1, "            ", -1, -1, -1, "  ", "    ", list, &num_items, MAX_NUM_ITEMS);
    if (validate_num_found(status, 0) != 0) return -1;
    if (validate_num_found(num_items, 120) != 0) return -1;

    // I don't get this one. I would think it's
    // {NOT (specific ip2 AND specific ip3)} OR (range of ip1) OR (other range of ip1)
    // but it seems to filter out the 'excluded' first before checking the ranges
    ips[0] = 100; ips[2] = 101;
    Xc_Select_ip1(1, 1, ips, 3);
    ips[0] = 200001; ips[2] = 200002;
    Xc_Select_ip1(2, 1, ips, 3);
    status = c_fstinl(iun, &ni, &nj, &nk, -1, "            ", -1, -1, -1, "  ", "    ", list, &num_items, MAX_NUM_ITEMS);
    if (validate_num_found(status, 0) != 0) return -1;
    if (validate_num_found(num_items, 72) != 0) return -1;

    // A range ip1 with a delta
    RequetesInit();
    ips[0] = 100;
    ips[1] = READLX_RANGE;
    ips[2] = 102;
    ips[3] = READLX_DELTA;
    ips[4] = 2;
    Xc_Select_ip1(0, 1, ips, 5);
    status = c_fstinl(iun, &ni, &nj, &nk, -1, "            ", -1, -1, -1, "  ", "    ", list, &num_items, MAX_NUM_ITEMS);
    if (validate_num_found(status, 0) != 0) return -1;
    if (validate_num_found(num_items, 25) != 0) return -1;

    // Another range, ip2, with delta
    RequetesInit();
    ips[2] = 101;
    Xc_Select_ip2(0, 1, ips, 5);
    status = c_fstinl(iun, &ni, &nj, &nk, -1, "            ", -1, -1, -1, "  ", "    ", list, &num_items, MAX_NUM_ITEMS);
    if (validate_num_found(status, 0) != 0) return -1;
    if (validate_num_found(num_items, 0) != 0) return -1;

    // A range ip3 (with unrecognized encodings)
    RequetesInit();
    ips[0] = 200001;
    ips[2] = 200002;
    Xc_Select_ip3(0, 1, ips, 3);
    status = c_fstinl(iun, &ni, &nj, &nk, -1, "            ", -1, -1, -1, "  ", "    ", list, &num_items, MAX_NUM_ITEMS);
    if (validate_num_found(status, 0) != 0) return -1;
    if (validate_num_found(num_items, 50) != 0) return -1;

    // A range ip3, with a delta (with unrecognized encodings)
    RequetesInit();
    ips[0] = 200001;
    ips[2] = 200003;
    ips[4] = 2;
    Xc_Select_ip3(0, 1, ips, 5);
    status = c_fstinl(iun, &ni, &nj, &nk, -1, "            ", -1, -1, -1, "  ", "    ", list, &num_items, MAX_NUM_ITEMS);
    if (validate_num_found(status, 0) != 0) return -1;
    if (validate_num_found(num_items, 25) != 0) return -1;

    c_fstfrm(iun);
    c_fclos(iun);

    return 0;
}

int main(void) {

    App_Log(APP_INFO, "Testing RSF\n");
    if (test_excdes(1) < 0) return -1;
    if (test_excdes_98(1) < 0) return -1;

    App_Log(APP_INFO, "Testing XDF\n");
    if (test_excdes(0) < 0) return -1;
    if (test_excdes_98(0) < 0) return -1;

    App_Log(APP_INFO, "Test successful\n");

    return 0;
}
