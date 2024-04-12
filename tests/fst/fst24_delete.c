
#include <App.h>
#include <rmn/fst24_file.h>

const char* test_filename = "delete.fst";

int create_file(const int is_rsf) {
    remove(test_filename);

    fst_file* test_file = fst24_open(test_filename, is_rsf ? "RSF" : "XDF");

    float dummy_data[1] = { 1.234 };
    fst_record rec = default_fst_record;

    rec.data = dummy_data;
    rec.datyp = FST_TYPE_REAL;
    rec.dasiz = 32;
    rec.npak = -32;
    rec.ni = 1;
    rec.nj = 1;
    rec.nk = 1;

    rec.dateo = 0;
    rec.deet = 0;
    rec.npas = 0;

    rec.ip1 = 1;
    rec.ip2 = 1;
    rec.ip3 = 1;

    rec.ig1 = 1;
    rec.ig2 = 1;
    rec.ig3 = 1;
    rec.ig4 = 1;

    fst24_write(test_file, &rec, 0);
    rec.ip1++;
    fst24_write(test_file, &rec, 0);
    rec.ip1++;
    fst24_write(test_file, &rec, 0);

    fst24_close(test_file);
    free(test_file);

    return 0;
}

int test_fst24_delete(const int is_rsf) {

    App_Log(APP_INFO, "Doing %s test\n", is_rsf ? "RSF" : "XDF");
    create_file(is_rsf);

    fst_file* test_file = fst24_open(test_filename, NULL);

    fst_record criteria = default_fst_record;
    criteria.ip1 = 2;
    fst_query* query = fst24_new_query(test_file, &criteria, NULL);

    fst_record rec = default_fst_record;
    fst24_find_next(query, &rec);

    if (fst24_delete(&rec) <= 0) {
        App_Log(APP_ERROR, "Unable to delete the record!\n");
        return -1;
    }

    fst24_close(test_file);
    free(test_file);

    return 0;
}

int main(void) {

    if (test_fst24_delete(1) < 0) {
        App_Log(APP_ERROR, "RSF test failed\n");
        return -1;
    }
    
    if (test_fst24_delete(0) < 0) {
        App_Log(APP_ERROR, "XDF test failed\n");
        return -1;
    }
    
    App_Log(APP_INFO, "Test successful\n");
    
    return 0;
}
