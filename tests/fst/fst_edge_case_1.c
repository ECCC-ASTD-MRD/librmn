
#include <App.h>
#include <rmn.h>

const char* filename_rsf = "edge_case_1.rsf";
const char* filename_xdf = "edge_case_1.xdf";

static inline float as_float(const void* a) {
    return *((float*)a);
}

static inline uint32_t as_uint(const void* a) {
    return *((uint32_t*)a);
}

static inline float float_diff(const uint32_t a, const uint32_t b) {
    return fabsf(as_float(&a) - as_float(&b));
}

int run_test(const int is_rsf) {

    App_Log(APP_INFO, "Run test for %s\n", is_rsf ? "RSF" : "XDF");

    const char* filename = is_rsf ? filename_rsf : filename_xdf;
    remove(filename);

    const char* options = is_rsf ? "RSF+R/W" : "XDF+R/W";
    fst_file* test_file = fst24_open(filename, options);
    
    if (test_file == NULL) {
        App_Log(APP_ERROR, "Unable to open test file %s\n", filename);
        return -1;
    }

    uint32_t dummy_data[] = {
        0x00017777,
        0x000aaaaa,
        0x00177777,
        0x002aaaaa,
        0x00577777,
        0x00800000,
        0x0080ff00,
        0x01000000,
        0x01400000,
        0x01800000
    };

    {
        fst_record rec = default_fst_record;
        rec.data = dummy_data;
        rec.data_type = FST_TYPE_REAL;
        rec.data_bits = 32;
        rec.pack_bits = 16;
        rec.ni = 10;
        rec.nj = 1;
        rec.nk = 1;

        rec.npas = 1;
        rec.deet = 1;
        rec.ip1 = 1;
        rec.ip2 = 1;
        rec.ip3 = 1;
        rec.ig1 = 1;
        rec.ig2 = 1;
        rec.ig3 = 1;
        rec.ig4 = 1;

        if (fst24_write(test_file, &rec, FST_NO) != TRUE) {
            App_Log(APP_ERROR, "Unable to write record to test file\n");
            return -1;
        }
    }

    fst_record rec = default_fst_record;
    fst_query* query = fst24_new_query(test_file, NULL, NULL);
    if (fst24_read_next(query, &rec) != TRUE) {
        App_Log(APP_ERROR, "Unable to read record from file\n");
        return -1;
    }

    const uint32_t* data = rec.data;
    for (int i = 0; i < fst24_record_num_elem(&rec); i++) {
        if (float_diff(data[i], dummy_data[i]) > 5e-43) {
            char buffer[1024*2];
            char* ptr = buffer;
            for (int j = 0; j < fst24_record_num_elem(&rec); j++) {
                ptr += sprintf(ptr, "%8x | %8x  ---  %.3e | %.3e (diff %.3e)\n",
                               dummy_data[j], data[j], as_float(dummy_data + j), as_float(data + j),
                               float_diff(data[j], dummy_data[j]));
            }
            App_Log(APP_ERROR, "data %d is different!\n%s", i, buffer);
            return -1;
        }
    }

    fst24_query_free(query);

    if (!fst24_close(test_file)) {
        App_Log(APP_ERROR, "Unable to close test file %s\n", filename);
        return -1;
    }



    return 0;
}

int main(void) {
    if (run_test(1) != 0) return -1;
    if (run_test(0) != 0) return -1;
    return 0;
}
