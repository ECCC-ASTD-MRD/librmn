
#include <App.h>
#include <rmn/fst24_file.h>

int main(void) {

    const char* f1_name = "1.rsf";
    const char* f2_name = "2.rsf";

    remove(f1_name);
    remove(f2_name);

    float data1[1] = { 111.111 };
    float data2[1] = { 222.222 };
    fst_record rec = default_fst_record;

    rec.data_type = FST_TYPE_REAL;
    rec.data_bits = 32;
    rec.pack_bits = 32;
    rec.ni = 1;
    rec.nj = 1;
    rec.nk = 1;

    rec.dateo = 1000;
    rec.deet = 0;
    rec.npas = 0;
    rec.ip1 = 1;
    rec.ip2 = 2;
    rec.ip3 = 3;
    rec.ig1 = 0;
    rec.ig2 = 2;
    rec.ig3 = 4;
    rec.ig4 = 8;

    // Create 1st file with 1 record
    {
        fst_file* f = fst24_open(f1_name, "RSF");
        rec.data = data1;
        fst24_write(f, &rec, 0);
        fst24_close(f);

        free(f);
    }

    // Create 2nd file with 1 record
    {
        fst_file* f = fst24_open(f2_name, "RSF");
        rec.data = data2;
        fst24_write(f, &rec, 0);
        fst24_close(f);

        free(f);
    }

    // Concatenate the files
    char cmd[2048];
    sprintf(cmd, "cat %s >> %s", f1_name, f2_name);
    system(cmd);

    // Read the concatenated file and check content
    {
        fst_file* f = fst24_open(f2_name, NULL);
        
        fst24_print_summary(f, NULL);

        fst_record read_rec = default_fst_record;
        fst_query* q = fst24_new_query(f, NULL, NULL);
        fst24_read_next(q, &read_rec);
        if (!fst24_record_has_same_info(&read_rec, &rec) || (((float*)read_rec.data)[0] != data2[0])) {
            App_Log(APP_ERROR, "Not the correct record!\n");
            return -1;
        }

        fst24_record_free(&read_rec);
        read_rec = default_fst_record;
        fst24_read_next(q, &read_rec);
        if (!fst24_record_has_same_info(&read_rec, &rec) || (((float*)read_rec.data)[0] != data1[0])) {
            App_Log(APP_ERROR, "Not the correct record!\n");
            return -1;
        }
    }

    return 0;
}
