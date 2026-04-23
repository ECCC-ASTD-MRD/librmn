
#include <App.h>
#include <rmn.h>

const char* filename = "data_map.fst";

static float dummy_data[] = { 0.0f, 0.1f, 0.2f, 0.3f, 0.4f, 0.5f, 0.6f, 0.7f };

static uint32_t data_map[] = { 1, 2, 3, 4, 5, 6, 7, 8 };

static int create_file(void) {

    remove(filename);
    fst_file* test_file = fst24_open(filename, "RSF+R/W");

    if (test_file == NULL) {
        App_Log(APP_ERROR, "Unable to open (create) file '%s'\n", filename);
        return -1;
    }

    fst_record rec = default_fst_record;

    rec.data = dummy_data;
    rec.data_type = FST_TYPE_REAL;
    rec.ni = sizeof(dummy_data) / sizeof(float);
    rec.nj = 1;
    rec.nk = 1;
    rec.data_bits = 32;
    rec.pack_bits = 32;

    rec.dateo = 0;
    rec.datev = 0;
    rec.deet = 0;
    rec.npas = 0;

    rec.ip1 = 0;
    rec.ip2 = 0;
    rec.ip3 = 0;
    rec.ig1 = 0;
    rec.ig2 = 0;
    rec.ig3 = 0;
    rec.ig4 = 0;

    strcpy(rec.nomvar, "map");

    // First write a record without data map
    strcpy(rec.etiket, "without");
    if (fst24_write(test_file, &rec, FST_NO) != TRUE) {
        App_Log(APP_ERROR, "Unable to write record (without data map) into new file\n");
        return -1;
    }

    strcpy(rec.etiket, "with");
    rec.data_blocks.map_size = sizeof(data_map) / sizeof(uint32_t);
    rec.data_blocks.map = data_map;

    if (fst24_write(test_file, &rec, FST_NO) != TRUE) {
        App_Log(APP_ERROR, "Unable to write record (with data map) into new file\n");
        return -1;
    }

    if (fst24_close(test_file) != TRUE) {
        App_Log(APP_ERROR, "Unable to close file '%s' after creation\n", filename);
        return -1;
    }

    return 0;
}

static int read_file(void) {

    fst_file* test_file = fst24_open(filename, NULL);
    if (test_file == NULL) {
        App_Log(APP_ERROR, "Unable to open file '%s'", filename);
        return -1;
    }

    fst_record crit_with = default_fst_record;
    fst_record crit_without = default_fst_record;

    strcpy(crit_with.etiket, "with");
    strcpy(crit_without.etiket, "without");

    fst_query* q_with = fst24_new_query(test_file, &crit_with, NULL);
    fst_query* q_without = fst24_new_query(test_file, &crit_without, NULL);

    fst_record rec_with = default_fst_record;
    fst_record rec_without = default_fst_record;

    if (q_with == NULL || q_without == NULL) {
        App_Log(APP_ERROR, "Unable to create queries (with = %p, without = %p)\n", q_with, q_without);
        return -1;
    }

    // Check that we can find + read the record with data map
    if (fst24_find_next(q_with, &rec_with) == TRUE) {
        if (rec_with.data_blocks.map_size == 0) {
            App_Log(APP_ERROR, "Record with data map has map_size = 0\n");
            return -1;
        }
        if (rec_with.data_blocks.map != NULL) {
            App_Log(APP_ERROR, "Record with data map has non-NULL map pointer (%p), but it hasn't been read yet\n",
                    rec_with.data_blocks.map);
            return -1;
        }

        // Read just the data map
        fst24_read_data_map(&rec_with);

        if (rec_with.data_blocks.map == NULL) {
            App_Log(APP_ERROR, "Record with data map has NULL map pointer after reading\n");
            return -1;
        }

        // Verify data map content
        if (rec_with.data_blocks.map_size != sizeof(data_map) / sizeof(uint32_t)) {
            App_Log(APP_ERROR, "Record with data map has different data map size than expected\n");
            return -1;
        }

        for (int i = 0; i < rec_with.data_blocks.map_size; i++) {
            if (((uint32_t*)rec_with.data_blocks.map)[i] != data_map[i]) {
                App_Log(APP_ERROR, "Record with data map has different data map content than expected\n");
                return -1;
            }
        }

        App_Log(APP_ALWAYS, "Record with data map has the expected data map content\n");

        // Now read the data too
        fst24_read_record(&rec_with);

        if (rec_with.data_blocks.map == NULL || rec_with.data == NULL) {
            App_Log(APP_ERROR, "Record with data map has NULL map pointer after reading\n");
            return -1;
        }
    }
    else {
        App_Log(APP_ERROR, "Unable to find record with data map\n");
        return -1;
    }

    // Check that we can find + read the record without data map
    if (fst24_find_next(q_without, &rec_without) == TRUE) {
        fst24_read_record(&rec_without);
    }
    else {
        App_Log(APP_ERROR, "Unable to find record without data map\n");
        return -1;
    }

    // fst24_record_print(&rec_with);
    // fst24_record_print(&rec_without);

    // Verify data content of both records
    if (rec_with.ni != sizeof(dummy_data) / sizeof(float) || rec_with.ni != rec_without.ni) {
        App_Log(APP_ERROR, "Record with and without data map have different dimensions\n");
        return -1;
    }

    for (int i = 0; i < rec_with.ni; i++) {
        if (((float*)rec_with.data)[i] != dummy_data[i]) {
            App_Log(APP_ERROR, "Record with data map has different data content than expected\n");
            return -1;
        }
        if (((float*)rec_without.data)[i] != dummy_data[i]) {
            App_Log(APP_ERROR, "Record without data map has different data content than expected\n");
            return -1;
        }
    }

    App_Log(APP_ALWAYS, "Both records have the expected data content\n");

    if (fst24_close(test_file) != TRUE) {
        App_Log(APP_ERROR, "Unable to close file '%s' after creation\n", filename);
        return -1;
    }

    fst24_query_free(q_with);
    fst24_query_free(q_without);
    fst24_record_free(&rec_with);
    fst24_record_free(&rec_without);

    return 0;
}

int main(void) {

    if (create_file() != 0) return -1;
    if (read_file() != 0) return -1;

    return 0;
}
