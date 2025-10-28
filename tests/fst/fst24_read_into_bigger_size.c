
#include <App.h>
#include <rmn.h>

#include <rmn/copy_swap.h>

const char* rsf_filename = "diff_size.rsf";
const char* xdf_filename = "diff_size.xdf";

const int DATA_SIZE = 4;

double* data_d = NULL;
float* data_f = NULL;
int64_t* data_i64 = NULL;
int32_t* data_i32 = NULL;
int16_t* data_i16 = NULL;
int8_t* data_i8 = NULL;
uint64_t* data_u64 = NULL;
uint32_t* data_u32 = NULL;
uint16_t* data_u16 = NULL;
uint8_t* data_u8 = NULL;
uint32_t* data_mask = NULL;

static void clear_data(void);

static int init_data(const int size) {
    clear_data();

    data_d = (double*)malloc(size * size * sizeof(double));
    data_f = (float*)malloc(size * size * sizeof(float));
    data_i64 = (int64_t*)malloc(size * size * sizeof(int64_t));
    data_i32 = (int32_t*)malloc(size * size * sizeof(int32_t));
    data_i16 = (int16_t*)malloc(size * size * sizeof(int16_t));
    data_i8 = (int8_t*)malloc(size * size * sizeof(int8_t));
    data_u64 = (uint64_t*)malloc(size * size * sizeof(uint64_t));
    data_u32 = (uint32_t*)malloc(size * size * sizeof(uint32_t));
    data_u16 = (uint16_t*)malloc(size * size * sizeof(uint16_t));
    data_u8 = (uint8_t*)malloc(size * size * sizeof(uint8_t));
    data_mask = (uint32_t*)malloc(size * size * sizeof(uint32_t));

    for (int i = 0; i < size; i++) {
        for (int j = 0; j < size; j++) {
            const size_t index = i * size + j;
            data_d[index] = (i + (double)j / size) * 1.000000001;
            data_f[index] = (float)data_d[index];
            data_i64[index] = -index;
            data_i32[index] = -index & 0xffffffff;
            data_i16[index] = -index & 0xffff;
            data_i8[index] = -index & 0xff;
            data_u64[index] = index;
            data_u32[index] = index & 0xffffffff;
            data_u16[index] = index & 0xffff;
            data_u8[index] = index & 0xff;
            data_mask[index] = index % 2;
        }
    }

    return 0;
}

static void clear_data(void) {
    if (data_d) free(data_d);
    if (data_f) free(data_f);
    if (data_i64) free(data_i64);
    if (data_i32) free(data_i32);
    if (data_i16) free(data_i16);
    if (data_i8) free(data_i8);
    if (data_u64) free(data_u64);
    if (data_u32) free(data_u32);
    if (data_u16) free(data_u16);
    if (data_u8) free(data_u8);
    if (data_mask) free(data_mask);

    data_d = NULL;
    data_f = NULL;
    data_i64 = NULL;
    data_i32 = NULL;
    data_i16 = NULL;
    data_i8 = NULL;
    data_u64 = NULL;
    data_u32 = NULL;
    data_u16 = NULL;
    data_u8 = NULL;
    data_mask = NULL;
}

static inline uint64_t get_elem(const void* array, const int index, const int elem_size) {
    switch(elem_size) {
        case 8: return ((uint8_t*)array)[index];
        case 16: return ((uint16_t*)array)[index];
        case 32: return ((uint32_t*)array)[index];
        case 64: return ((uint64_t*)array)[index];
        default: return 0xabcdabcd43214321;
    }
}

static void compare_int_arrays(const void* a, const int size_a, const void* b, const int size_b, const int num_elem) {
    for (int i = 0; i < num_elem; i++) {
        const uint64_t elem_a = get_elem(a, i, size_a);
        const uint64_t elem_b = get_elem(b, i, size_b);
        if (elem_a != elem_b) {
            App_Log(APP_ERROR, "Data is not identical!!! [%4d] %8x - expected %8x\n", i, elem_a, elem_b);
            {
                char buffer[1024 * 4];
                char* ptr = buffer;
                for (i = 0; i < num_elem && i < 16; i+= 4) {
                    ptr += sprintf(ptr, "[%3d - %3d]: ", i, i+3);
                    for (int j = i; j < i + 4; j++) {
                        ptr += sprintf(ptr, "%16lx ", get_elem(a, j, size_a));
                    }
                    ptr += sprintf(ptr, "\n             ");
                    for (int j = i; j < i + 4; j++) {
                        ptr += sprintf(ptr, "%16lx ", get_elem(b, j, size_b));
                    }
                    ptr += sprintf(ptr, "\n");
                }
                App_Log(APP_ALWAYS, "%s:\n%s\n", __func__, buffer);
            }
            exit(-1);
        }
    }
}

static int create_file(const int is_rsf) {
    const char* filename = is_rsf ? rsf_filename : xdf_filename;
    remove(filename);

    const char* options = is_rsf ? "RSF+R/W" : "XDF+R/W";

    fst_file* f = fst24_open(filename, options);
    if (f == NULL) {
        App_Log(APP_ERROR, "Unable to create file %s with options %s (open)\n", filename, options);
        return -1;
    }

    fst_record rec = default_fst_record;
    rec.ni = DATA_SIZE;
    rec.nj = DATA_SIZE;
    rec.nk = 1;
    rec.deet = 1;
    rec.npas = 1;
    rec.ig1 = 0;
    rec.ig2 = 0;
    rec.ig3 = 0;
    rec.ig4 = 0;
    rec.ip1 = 0;
    rec.ip2 = 0;
    rec.ip3 = 0;

    App_Log(APP_INFO, "Writing records\n");

    rec.data_type = FST_TYPE_REAL;
    rec.data_bits = 64;
    rec.pack_bits = 64;
    rec.data = data_d;
    strcpy(rec.etiket, "DOUBLE");
    if (!fst24_write(f, &rec, FST_NO)) return -1;

    rec.data_type = FST_TYPE_REAL;
    rec.data_bits = 32;
    rec.pack_bits = 32;
    rec.data = data_f;
    strcpy(rec.etiket, "FLOAT");
    if (!fst24_write(f, &rec, FST_NO)) return -1;

    rec.data_type = FST_TYPE_SIGNED;
    rec.data_bits = 32;
    rec.pack_bits = 32;
    rec.data = data_i32;
    strcpy(rec.etiket, "I32");
    if (!fst24_write(f, &rec, FST_NO)) return -1;

    rec.data_type = FST_TYPE_SIGNED;
    rec.data_bits = 32;
    rec.pack_bits = 12;
    rec.data = data_i32;
    strcpy(rec.etiket, "S32_12");
    if (!fst24_write(f, &rec, FST_NO)) return -1;

    rec.data_type = FST_TYPE_SIGNED;
    rec.data_type = FST_TYPE_SIGNED;
    rec.data_bits = 16;
    rec.pack_bits = 16;
    rec.data = data_i16;
    strcpy(rec.etiket, "I16");
    if (!fst24_write(f, &rec, FST_NO)) return -1;

    rec.data_type = FST_TYPE_SIGNED;
    rec.data_bits = 8;
    rec.pack_bits = 8;
    rec.data = data_i8;
    strcpy(rec.etiket, "I8");
    if (!fst24_write(f, &rec, FST_NO)) return -1;

    rec.data_type = FST_TYPE_UNSIGNED;
    rec.data_bits = 32;
    rec.pack_bits = 32;
    rec.data = data_u32;
    strcpy(rec.etiket, "U32");
    if (!fst24_write(f, &rec, FST_NO)) return -1;

    rec.data_type = FST_TYPE_UNSIGNED;
    rec.data_bits = 16;
    rec.pack_bits = 16;
    rec.data = data_u16;
    strcpy(rec.etiket, "U16");
    if (!fst24_write(f, &rec, FST_NO)) return -1;

    rec.data_type = FST_TYPE_UNSIGNED;
    rec.data_bits = 8;
    rec.pack_bits = 8;
    rec.data = data_u8;
    strcpy(rec.etiket, "U8");
    if (!fst24_write(f, &rec, FST_NO)) return -1;

    rec.data_type = FST_TYPE_UNSIGNED;
    rec.data_bits = 32;
    rec.pack_bits = 1;
    rec.data = data_mask;
    strcpy(rec.etiket, "MASK");
    if (!fst24_write(f, &rec, FST_NO)) return -1;

    // Reset data_bits to 0
    fst24_close(f);
    f = fst24_open(filename, options);
    fst_record crit = default_fst_record;
    strcpy(crit.etiket, "MASK");
    fst_query* q = fst24_new_query(f, &crit, NULL);
    fst24_find_next(q, &rec);
    rec.data_bits = 0;
    // rec.ip1 = 5;
    if (!fst24_write(f, &rec, FST_META)) return -1;

    App_Log(APP_INFO, "Done writing records\n");

    if (!fst24_close(f)) {
        App_Log(APP_ERROR, "Unable to close file!\n");
        return -1;
    }

    return 0;
}

static int test_read_into_bigger_size(const int is_rsf) {
    App_Log(APP_INFO, "Testing %s\n", is_rsf ? "RSF" : "XDF");
    if (create_file(is_rsf) != 0) return -1;

    const char* filename = is_rsf ? rsf_filename : xdf_filename;
    fst_file* f = fst24_open(filename, "R/O");
    if (f == NULL) {
        App_Log(APP_ERROR, "Unable to open file %s for reading \n", filename);
        return -1;
    }

    fst_record criteria = default_fst_record;
    fst_record rec = default_fst_record;

    // ------------ DOUBLE -----------------
    {
        strcpy(criteria.etiket, "DOUBLE");
        fst_query* q = fst24_new_query(f, &criteria, NULL);
        if (!fst24_find_next(q, &rec)) {
            App_Log(APP_ERROR, "Unable to find record with criterion %s\n", criteria.etiket);
            return -1;
        }

        // DOUBLE, same size
        if (fst24_read_record(&rec) <= 0) {
            App_Log(APP_ERROR, "Unable to read record with regular size (%s)\n", criteria.etiket);
            return -1;
        }

        {
            double* d = rec.data;
            for (int i = 0; i < DATA_SIZE * DATA_SIZE; i++) {
                if (d[i] != data_d[i]) {
                    App_Log(APP_ERROR, "Data is not identical!!!\n");
                    return -1;
                }
            }
        }

        // DOUBLE, trying for 32 bits
        rec.data_bits = 32;
        if (fst24_read_record(&rec) <= 0) {
            App_Log(APP_ERROR, "Unable to read record with regular size (%s)\n", criteria.etiket);
            return -1;
        }

        {
            double* d = rec.data;
            for (int i = 0; i < DATA_SIZE * DATA_SIZE; i++) {
                if (d[i] != data_d[i]) {
                    App_Log(APP_ERROR, "Data is not identical!!!\n");
                    return -1;
                }
            }
        }

        fst24_query_free(q);
    }
    // ------------ END DOUBLE -----------------

    // ------------ FLOAT -----------------
    {
        strcpy(criteria.etiket, "FLOAT");
        fst_query* q = fst24_new_query(f, &criteria, NULL);
        if (!fst24_find_next(q, &rec)) {
            App_Log(APP_ERROR, "Unable to find record with criterion %s\n", criteria.etiket);
            return -1;
        }

        // FLOAT, same size
        if (fst24_read_record(&rec) <= 0) {
            App_Log(APP_ERROR, "Unable to read record with regular size (%s)\n", criteria.etiket);
            return -1;
        }

        {
            float* d = rec.data;
            for (int i = 0; i < DATA_SIZE * DATA_SIZE; i++) {
                if (d[i] != data_f[i]) {
                    App_Log(APP_ERROR, "Data is not identical!!! [%4d] %f - expected %f\n",
                            i, d[i], data_f[i]);
                    return -1;
                }
            }
        }

        // FLOAT, to double
        rec.data_bits = 64;
        if (!fst24_read_record(&rec)) {
            App_Log(APP_ERROR, "Unable to read record with double size (%s)\n", criteria.etiket);
            return -1;
        }

        {
            double* d = rec.data;
            int same = 1;
            for (int i = 0; i < DATA_SIZE * DATA_SIZE; i++) {
                if (d[i] != (float)data_d[i]) {
                    App_Log(APP_ERROR, "Data is not identical!!! [%4d] %f - expected %f\n",
                            i, d[i], data_d[i]);
                    same = 0;
                    // return -1;
                }
            }
            if (!same) return -1;
        }

        fst24_query_free(q);
    }
    // --- END FLOAT ---

    // ------- SIGNED 32 ---------------
    {
        strcpy(criteria.etiket, "I32");
        fst_query* q = fst24_new_query(f, &criteria, NULL);
        if (!fst24_find_next(q, &rec)) {
            App_Log(APP_ERROR, "Unable to find record with criterion %s\n", criteria.etiket);
            return -1;
        }

        // int 32, same size
        if (fst24_read_record(&rec) <= 0) {
            App_Log(APP_ERROR, "Unable to read record with regular size (%s)\n", criteria.etiket);
            return -1;
        }
        compare_int_arrays(rec.data, 32, data_i32, 32, DATA_SIZE * DATA_SIZE);

        // int 32, to 64
        rec.data_bits = 64;
        if (!fst24_read_record(&rec)) {
            App_Log(APP_ERROR, "Unable to read record with double size (%s)\n", criteria.etiket);
            return -1;
        }
        compare_int_arrays(rec.data, 64, data_i64, 64, DATA_SIZE * DATA_SIZE);

        fst24_query_free(q);
    }
    // ------- SIGNED 32 ---------------

    // ---------- SIGNED 16 --------------
    {
        strcpy(criteria.etiket, "I16");
        fst_query* q = fst24_new_query(f, &criteria, NULL);
        if (!fst24_find_next(q, &rec)) {
            App_Log(APP_ERROR, "Unable to find record with criterion %s\n", criteria.etiket);
            return -1;
        }

        // int 16, same size
        if (fst24_read_record(&rec) <= 0) {
            App_Log(APP_ERROR, "Unable to read record with regular size (%s)\n", criteria.etiket);
            return -1;
        }
        compare_int_arrays(rec.data, 16, data_i16, 16, DATA_SIZE * DATA_SIZE);

        // int 16, to 32
        rec.data_bits = 32;
        if (!fst24_read_record(&rec)) {
            App_Log(APP_ERROR, "Unable to read record with double size (%s)\n", criteria.etiket);
            return -1;
        }
        compare_int_arrays(rec.data, 32, data_i32, 32, DATA_SIZE * DATA_SIZE);

        // int 16, to 64
        rec.data_bits = 64;
        if (!fst24_read_record(&rec)) {
            App_Log(APP_ERROR, "Unable to read record with double size (%s)\n", criteria.etiket);
            return -1;
        }
        compare_int_arrays(rec.data, 64, data_i64, 64, DATA_SIZE * DATA_SIZE);

        fst24_query_free(q);
    }
    // ---------- END SIGNED 16 --------------

    // ---------- SIGNED 8 --------------
    {
        strcpy(criteria.etiket, "I8");
        fst_query* q = fst24_new_query(f, &criteria, NULL);
        if (!fst24_find_next(q, &rec)) {
            App_Log(APP_ERROR, "Unable to find record with criterion %s\n", criteria.etiket);
            return -1;
        }

        // int 8, same size
        if (fst24_read_record(&rec) <= 0) {
            App_Log(APP_ERROR, "Unable to read record with regular size (%s)\n", criteria.etiket);
            return -1;
        }
        compare_int_arrays(rec.data, 8, data_i8, 8, DATA_SIZE * DATA_SIZE);

        // int 8, to 16
        rec.data_bits = 16;
        if (fst24_read_record(&rec) <= 0) {
            App_Log(APP_ERROR, "Unable to read record with regular size (%s)\n", criteria.etiket);
            return -1;
        }
        compare_int_arrays(rec.data, 16, data_i16, 16, DATA_SIZE * DATA_SIZE);

        // int 8, to 32
        rec.data_bits = 32;
        if (!fst24_read_record(&rec)) {
            App_Log(APP_ERROR, "Unable to read record with double size (%s)\n", criteria.etiket);
            return -1;
        }
        compare_int_arrays(rec.data, 32, data_i32, 32, DATA_SIZE * DATA_SIZE);

        // int 8, to 64
        rec.data_bits = 64;
        if (!fst24_read_record(&rec)) {
            App_Log(APP_ERROR, "Unable to read record with double size (%s)\n", criteria.etiket);
            return -1;
        }
        compare_int_arrays(rec.data, 64, data_i64, 64, DATA_SIZE * DATA_SIZE);

        fst24_query_free(q);
    }
    // ---------- END SIGNED 8 --------------

    // ------- UNSIGNED 32 ---------------
    {
        strcpy(criteria.etiket, "U32");
        fst_query* q = fst24_new_query(f, &criteria, NULL);
        if (!fst24_find_next(q, &rec)) {
            App_Log(APP_ERROR, "Unable to find record with criterion %s\n", criteria.etiket);
            return -1;
        }

        // uint 32, same size
        if (fst24_read_record(&rec) <= 0) {
            App_Log(APP_ERROR, "Unable to read record with regular size (%s)\n", criteria.etiket);
            return -1;
        }
        compare_int_arrays(rec.data, 32, data_u32, 32, DATA_SIZE * DATA_SIZE);

        // uint 32, to 64
        rec.data_bits = 64;
        if (!fst24_read_record(&rec)) {
            App_Log(APP_ERROR, "Unable to read record with double size (%s)\n", criteria.etiket);
            return -1;
        }
        compare_int_arrays(rec.data, 64, data_u64, 64, DATA_SIZE * DATA_SIZE);

        fst24_query_free(q);
    }
    // ------- UNSIGNED 32 ---------------

    // ---------- UNSIGNED 16 --------------
    {
        strcpy(criteria.etiket, "U16");
        fst_query* q = fst24_new_query(f, &criteria, NULL);
        if (!fst24_find_next(q, &rec)) {
            App_Log(APP_ERROR, "Unable to find record with criterion %s\n", criteria.etiket);
            return -1;
        }

        // uint 16, same size
        if (fst24_read_record(&rec) <= 0) {
            App_Log(APP_ERROR, "Unable to read record with regular size (%s)\n", criteria.etiket);
            return -1;
        }
        compare_int_arrays(rec.data, 16, data_u16, 16, DATA_SIZE * DATA_SIZE);

        // uint 16, to 32
        rec.data_bits = 32;
        if (!fst24_read_record(&rec)) {
            App_Log(APP_ERROR, "Unable to read record with double size (%s)\n", criteria.etiket);
            return -1;
        }
        compare_int_arrays(rec.data, 32, data_u32, 32, DATA_SIZE * DATA_SIZE);

        // uint 16, to 64
        rec.data_bits = 64;
        if (!fst24_read_record(&rec)) {
            App_Log(APP_ERROR, "Unable to read record with double size (%s)\n", criteria.etiket);
            return -1;
        }
        compare_int_arrays(rec.data, 64, data_u64, 64, DATA_SIZE * DATA_SIZE);

        fst24_query_free(q);
    }
    // ---------- END UNSIGNED 16 --------------

    // ---------- UNSIGNED 8 --------------
    {
        strcpy(criteria.etiket, "U8");
        fst_query* q = fst24_new_query(f, &criteria, NULL);
        if (!fst24_find_next(q, &rec)) {
            App_Log(APP_ERROR, "Unable to find record with criterion %s\n", criteria.etiket);
            return -1;
        }

        // uint 8, same size
        if (fst24_read_record(&rec) <= 0) {
            App_Log(APP_ERROR, "Unable to read record with regular size (%s)\n", criteria.etiket);
            return -1;
        }
        compare_int_arrays(rec.data, 8, data_u8, 8, DATA_SIZE * DATA_SIZE);

        // uint 8, to 16
        rec.data_bits = 16;
        if (fst24_read_record(&rec) <= 0) {
            App_Log(APP_ERROR, "Unable to read record with regular size (%s)\n", criteria.etiket);
            return -1;
        }
        compare_int_arrays(rec.data, 16, data_u16, 16, DATA_SIZE * DATA_SIZE);

        // uint 8, to 32
        rec.data_bits = 32;
        if (!fst24_read_record(&rec)) {
            App_Log(APP_ERROR, "Unable to read record with double size (%s)\n", criteria.etiket);
            return -1;
        }
        compare_int_arrays(rec.data, 32, data_u32, 32, DATA_SIZE * DATA_SIZE);

        // uint 8, to 64
        rec.data_bits = 64;
        if (!fst24_read_record(&rec)) {
            App_Log(APP_ERROR, "Unable to read record with double size (%s)\n", criteria.etiket);
            return -1;
        }
        compare_int_arrays(rec.data, 64, data_u64, 64, DATA_SIZE * DATA_SIZE);

        fst24_query_free(q);
    }
    // ---------- END UNSIGNED 8 --------------

    // ---------- MASK (1 BIT) --------------
    {
        strcpy(criteria.etiket, "MASK");
        fst_query* q = fst24_new_query(f, &criteria, NULL);
        if (!fst24_find_next(q, &rec)) {
            App_Log(APP_ERROR, "Unable to find record with criterion %s\n", criteria.etiket);
            return -1;
        }

        // 1 to 32 bits (default)
        if (fst24_read_record(&rec) <= 0) {
            App_Log(APP_ERROR, "Unable to read record with regular size (%s)\n", criteria.etiket);
            return -1;
        }
        compare_int_arrays(rec.data, 32, data_mask, 32, DATA_SIZE * DATA_SIZE);

        // 1 to 8 bits
        rec.data_bits = 8;
        if (fst24_read_record(&rec) <= 0) {
            App_Log(APP_ERROR, "Unable to read record with regular size (%s)\n", criteria.etiket);
            return -1;
        }
        compare_int_arrays(rec.data, 8, data_mask, 32, DATA_SIZE * DATA_SIZE);

        // 1 to 16 bits
        rec.data_bits = 16;
        if (fst24_read_record(&rec) <= 0) {
            App_Log(APP_ERROR, "Unable to read record with regular size (%s)\n", criteria.etiket);
            return -1;
        }
        compare_int_arrays(rec.data, 16, data_mask, 32, DATA_SIZE * DATA_SIZE);

        // 1 to 32 bits
        rec.data_bits = 32;
        if (fst24_read_record(&rec) <= 0) {
            App_Log(APP_ERROR, "Unable to read record with regular size (%s)\n", criteria.etiket);
            return -1;
        }
        compare_int_arrays(rec.data, 32, data_mask, 32, DATA_SIZE * DATA_SIZE);

        // 1 to 64 bits
        rec.data_bits = 64;
        if (fst24_read_record(&rec) <= 0) {
            App_Log(APP_ERROR, "Unable to read record with regular size (%s)\n", criteria.etiket);
            return -1;
        }
        compare_int_arrays(rec.data, 64, data_mask, 32, DATA_SIZE * DATA_SIZE);
    }
    // ---------- END MASK (1 BIT) --------------

    // ---------- INT 32 compressed to 12 --------------
    {
        strcpy(criteria.etiket, "S32_12");
        fst_query* q = fst24_new_query(f, &criteria, NULL);
        if (!fst24_find_next(q, &rec)) {
            App_Log(APP_ERROR, "Unable to find record with criterion %s\n", criteria.etiket);
            return -1;
        }

        // 32 bits (default)
        if (fst24_read_record(&rec) <= 0) {
            App_Log(APP_ERROR, "Unable to read record with regular size (%s)\n", criteria.etiket);
            return -1;
        }
        compare_int_arrays(rec.data, 32, data_i32, 32, DATA_SIZE * DATA_SIZE);

        // 16 bits
        rec.data_bits = 16;
        if (fst24_read_record(&rec) <= 0) {
            App_Log(APP_ERROR, "Unable to read record with regular size (%s)\n", criteria.etiket);
            return -1;
        }
        compare_int_arrays(rec.data, 16, data_i16, 16, DATA_SIZE * DATA_SIZE);

        // 8 bits
        rec.data_bits = 8;
        if (fst24_read_record(&rec) <= 0) {
            App_Log(APP_ERROR, "Unable to read record with regular size (%s)\n", criteria.etiket);
            return -1;
        }
        compare_int_arrays(rec.data, 8, data_i8, 8, DATA_SIZE * DATA_SIZE);
    }

    // ---------- END INT 32 compressed to 12 --------------

    fst24_record_free(&rec);

    if (!fst24_close(f)) {
        App_Log(APP_ERROR, "Unable to close file %s\n", filename);
        return -1;
    }

    return 0;
}

int main(void) {

    // int8_t a[6] = {1, -2, 3, -4, 5, -6};
    // int16_t b[6];
    // int32_t c[6];
    // int32_t d[6];

    // // Copy_items_l2r(a, 1, b, 2, 6);
    // upgrade_size(b, 16, a, 8, 6, 1);
    // upgrade_size(c, 32, a, 8, 6, 1);
    // upgrade_size(d, 32, b, 16, 6, 1);
    // for (int i = 0; i < 6; i++) {
    //     fprintf(stderr, "a[%d] = %d\n", i, (int32_t)a[i]);
    //     fprintf(stderr, "b[%d] = %d\n", i, (int32_t)b[i]);
    //     fprintf(stderr, "c[%d] = %d\n", i, (int32_t)c[i]);
    //     fprintf(stderr, "d[%d] = %d\n", i, (int32_t)d[i]);
    // }

    init_data(DATA_SIZE);
    if (test_read_into_bigger_size(1) != 0) return -1;
    if (test_read_into_bigger_size(0) != 0) return -1;
    clear_data();

    return 0;
}
