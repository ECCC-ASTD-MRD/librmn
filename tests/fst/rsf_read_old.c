#include <App.h>
#include <rmn.h>

//! Make sure we can still read a file written with a very old version of RSF (0)
//! Does not check the content
int main() {
    const char* data_dir = getenv("ECCI_DATA_DIR");
    if (data_dir == NULL) {
        App_Log(APP_ERROR, "Need to set the ECCI_DATA_DIR environment variable\n");
        return 1;
    }

    char filename[1024*4];
    snprintf(filename, sizeof(filename), "%s/%s", data_dir, "rpn-tools/old_file.version0.rsf");

    App_Log(APP_ALWAYS, "Reading file %s\n", filename);

    fst_file* f = fst24_open(filename, NULL);
    if (f == NULL) {
        App_Log(APP_ERROR, "Unable to open file %s\n", filename);
        return -1;
    }

    fst_query* q = fst24_new_query(f, NULL, NULL);
    fst_record rec = default_fst_record;

    const int64_t num_records = fst24_get_num_records(f);
    const int64_t EXPECTED_NUM_RECORDS = 5;
    if (num_records != 5) {
        App_Log(APP_ERROR, "Only %lld records found (%lld)\n", num_records, EXPECTED_NUM_RECORDS);
        return -1;
    }

    int num_read = 0;
    while (fst24_read_next(q, &rec) == TRUE && num_read < num_records) num_read++;

    if (num_read != num_records) {
        App_Log(APP_ERROR, "Unable to read all records. Read %d, expected %d\n", num_read, num_records);
        return -1;
    }

    fst24_record_free(&rec);
    fst24_query_free(q);

    fst24_close(f);

    return 0;
}
