#include <math.h>

#include <App.h>
#include <rmn.h>

const char* test_filename = "compression.fst";

// const int NUM_DATA_X = 16;
// const int NUM_DATA_Y = 8;
// const int NUM_DATA_Z = 4;
const int NUM_DATA_X = 64;
const int NUM_DATA_Y = 64;
const int NUM_DATA_Z = 2;

static float* data_f = NULL;
static double* data_d = NULL;

double gen_value_real(const int i, const int j, const int k, const int num_x, const int num_y, const int num_z) {
    double val = sin((double)i / ((double)num_x / 2)) + cos((double)j / ((double)num_y / 4)) + pow(2.0, (double)k / num_z);
    if (!isfinite(val)) {
        App_Log(APP_ERROR, "%s: Woahhh got a NaN at [%d, %d, %d]\n", __func__, i, j, k);
        exit(-1);
    }
    if (fabs(val) > 4.0) {
        App_Log(APP_WARNING, "%s: val is so large! %f\n", val);
    }
    return val;
}

void make_data() {
    if (data_f != NULL || data_d != NULL) return;
    data_f = (float*) malloc(NUM_DATA_X * NUM_DATA_Y * NUM_DATA_Z * sizeof(float));
    data_d = (double*) malloc(NUM_DATA_X * NUM_DATA_Y * NUM_DATA_Z * sizeof(double));

    if (data_f == NULL || data_d == NULL) {
        App_Log(APP_ERROR, "%s: Unable to allocated enough space\n", __func__);
        exit(-1);
    }

    for (int i = 0; i < NUM_DATA_X; i++) {
        for (int j = 0; j < NUM_DATA_Y; j++) {
            for (int k = 0; k < NUM_DATA_Z; k++) {
                const int index = (i * NUM_DATA_Y + j) * NUM_DATA_Z + k;
                data_d[index] = gen_value_real(i, j, k, NUM_DATA_X, NUM_DATA_Y, NUM_DATA_Z);
                data_f[index] = (float)data_d[index];
                // fprintf(stderr, "%6.3f ", data_f[index]);
                // if (index % 16 == 15) fprintf(stderr, "\n");
            }
        }
    }
    // fprintf(stderr, "\n");
}

int compare_data_f(const float* a, const float* b, const int num_x, const int num_y, const int num_z,
                   const float tol, const float max_tol) {
    double total_diff = 0.0;
    double total_a = 0.0;
    double max_diff = 0.0;
    double max_diff_a = 0.0;
    double max_diff_b = 0.0;
    for (int i = 0; i < num_x; i++) {
        for (int j = 0; j < num_y; j++) {
            for (int k = 0; k < num_z; k++) {
                const int index = (i * num_y + j) * num_z + k;
                const double diff = a[index] - b[index];
                total_diff += diff * diff;
                total_a += a[index] * a[index];
                if (fabsf(diff / a[index]) > max_diff) {
                    max_diff = fabsf(diff / a[index]);
                    max_diff_a = a[index];
                    max_diff_b = b[index];
                }
            }
        }
    }

    const double diff_norm = sqrt(total_diff);
    const double a_norm = sqrt(total_a);
    const double rel_diff = diff_norm / a_norm;

    if (rel_diff > tol || max_diff > max_tol) {
        App_Log(APP_ERROR, "%s: Relative difference = %.2e (diff norm %.2e, a norm %.2e)\n",
                __func__, rel_diff, diff_norm, a_norm);
        App_Log(APP_ERROR, "%s: Max diff = %.2e (a = %.3f, b = %.3f)\n", __func__, max_diff, max_diff_a, max_diff_b);


        char* buffer = malloc(40000);
        char* ptr = buffer;
        const int NUM_COL = num_x > 16 ? 16 : num_x;
        const int NUM_ROW = num_y * num_z > 33 ? 33 : num_y * num_z;
        const int COL_WIDTH = 7;
        for (int j = 0; j < NUM_ROW; j++) {
            sprintf(ptr, "%2d a: ", j);
            ptr += 6;
            for (int i = 0; i < NUM_COL; i++) {
                sprintf(ptr, " %6.3f", a[i + j * num_x]);
                ptr += COL_WIDTH;
            }
            ptr[0] = '\n'; ptr++;
            sprintf(ptr, "   b: ");
            ptr += 6;
            for (int i = 0; i < NUM_COL; i++) {
                sprintf(ptr, " %6.3f", b[i + j * num_x]);
                ptr += COL_WIDTH;
            }
            ptr[0] = '\n'; ptr++;
        }
        ptr[0] = '\0';

        App_Log(APP_ERROR, "Values: \n%s\n", buffer);

        free(buffer);
        return -1;
    }

    return 0;
}

int compare_data_d(const double* a, const double* b, const int num_x, const int num_y, const int num_z,
                   const double tol, const double max_tol) {
    double total_diff = 0.0;
    double total_a = 0.0;
    double max_diff = 0.0;
    double max_diff_a = 0.0;
    double max_diff_b = 0.0;
    for (int i = 0; i < num_x; i++) {
        for (int j = 0; j < num_y; j++) {
            for (int k = 0; k < num_z; k++) {
                const int index = (i * num_y + j) * num_z + k;
                const double diff = a[index] - b[index];
                total_diff += diff * diff;
                total_a += a[index] * a[index];
                if (!isfinite(total_a)) {
                    App_Log(APP_ERROR, "%s: total A = %f, a[%d, %d, %d] = %f\n",
                            __func__, total_a, i, j, k, a[index]);
                    exit(-1);
                }
                if (!isfinite(total_diff)) {
                    App_Log(APP_ERROR, "%s: total diff = %f, diff[%d, %d, %d] = %f, a = %f, b = %f\n",
                            __func__, total_diff, i, j, k, diff, a[index], b[index]);
                    exit(-1);
                }
                if (fabsf(diff / a[index]) > max_diff) {
                    max_diff = fabsf(diff / a[index]);
                    max_diff_a = a[index];
                    max_diff_b = b[index];
                }
            }
        }
    }

    const double diff_norm = sqrt(total_diff);
    const double a_norm = sqrt(total_a);
    const double rel_diff = diff_norm / a_norm;

    if (rel_diff > tol || max_diff > max_tol) {
        App_Log(APP_ERROR, "%s: Relative difference = %.2e (diff norm %.2e, a norm %.2e)\n",
                __func__, rel_diff, diff_norm, a_norm);
        App_Log(APP_ERROR, "%s: Max diff = %.2e (a = %.3f, b = %.3f)\n", __func__, max_diff, max_diff_a, max_diff_b);


        char* buffer = malloc(40000);
        char* ptr = buffer;
        const int NUM_COL = num_x > 16 ? 16 : num_x;
        const int NUM_ROW = num_y * num_z > 33 ? 33 : num_y * num_z;
        const int COL_WIDTH = 7;
        for (int j = 0; j < NUM_ROW; j++) {
            sprintf(ptr, "%2d a: ", j);
            ptr += 6;
            for (int i = 0; i < NUM_COL; i++) {
                sprintf(ptr, " %6.3f", a[i + j * num_x]);
                ptr += COL_WIDTH;
            }
            ptr[0] = '\n'; ptr++;
            sprintf(ptr, "   b: ");
            ptr += 6;
            for (int i = 0; i < NUM_COL; i++) {
                sprintf(ptr, " %6.3f", b[i + j * num_x]);
                ptr += COL_WIDTH;
            }
            ptr[0] = '\n'; ptr++;
        }
        ptr[0] = '\0';

        App_Log(APP_ERROR, "Values: \n%s\n", buffer);

        free(buffer);
        return -1;
    }

    return 0;
}

int test_compression(const int is_rsf) {
    remove(test_filename);

    const char* options = is_rsf ? "RSF" : "XDF";
    fst_file* test_file = fst24_open(test_filename, options);
    if (test_file == NULL) {
        App_Log(APP_ERROR, "Unable to open new test file with name %s and options %s\n", test_filename, options);
        return -1;
    }

    fst_record rec = default_fst_record;
    rec.ni = NUM_DATA_X;
    rec.nj = NUM_DATA_Y;
    rec.nk = NUM_DATA_Z;
    rec.dateo= 458021600;
    rec.deet = 300;
    rec.npas = 0;
    rec.ip1  = 1;
    rec.ip2  = 10;
    rec.ip3  = 100;
    strcpy(rec.typvar, "P");
    strcpy(rec.nomvar, "WAVE");
    strcpy(rec.etiket, "float");
    strcpy(rec.grtyp, "X");
    rec.ig1   = 0;
    rec.ig2   = 0;
    rec.ig3   = 0;
    rec.ig4   = 0;

    typedef struct {
        void* data;
        int data_type;
        int data_size;
        int pack_size;
        int nk;
        double tol;
        double max_tol;
    } test_params;

    // const int NUM_CASES = 16;
    #define NUM_CASES 16
    test_params params[NUM_CASES] = {
        {.data = data_f, .data_type = FST_TYPE_REAL, .data_size = 32, .pack_size = 32, .nk = NUM_DATA_Z, .tol = 0.0, .max_tol = 0.0},
        {.data = data_f, .data_type = FST_TYPE_REAL, .data_size = 32, .pack_size = 16, .nk = NUM_DATA_Z, .tol = 0.004, .max_tol = 0.008},
        {.data = data_f, .data_type = FST_TYPE_REAL, .data_size = 32, .pack_size = 8,  .nk = NUM_DATA_Z, .tol = 0.45, .max_tol = 0.75},
        {.data = data_f, .data_type = FST_TYPE_REAL | FST_TYPE_TURBOPACK, .data_size = 32, .pack_size = 32, .nk = NUM_DATA_Z, .tol = 0.0, .max_tol = 0.0},
        {.data = data_f, .data_type = FST_TYPE_REAL | FST_TYPE_TURBOPACK, .data_size = 32, .pack_size = 32, .nk = 1, .tol = 0.0, .max_tol = 0.0},
        {.data = data_f, .data_type = FST_TYPE_REAL | FST_TYPE_TURBOPACK, .data_size = 32, .pack_size = 24, .nk = 1, .tol = 2e-5, .max_tol = 4e-5},
        {.data = data_f, .data_type = FST_TYPE_REAL | FST_TYPE_TURBOPACK, .data_size = 32, .pack_size = 16, .nk = 1, .tol = 0.004, .max_tol = 0.008},
        {.data = data_f, .data_type = FST_TYPE_REAL | FST_TYPE_TURBOPACK, .data_size = 32, .pack_size = 14, .nk = 1, .tol = 0.015, .max_tol = 0.035},
        {.data = data_f, .data_type = FST_TYPE_REAL | FST_TYPE_TURBOPACK, .data_size = 32, .pack_size = 10, .nk = 1, .tol = 0.2, .max_tol = 0.35},
        {.data = data_f, .data_type = FST_TYPE_REAL | FST_TYPE_TURBOPACK, .data_size = 32, .pack_size = 9, .nk = 1, .tol = 0.3, .max_tol = 0.5},
        {.data = data_f, .data_type = FST_TYPE_REAL | FST_TYPE_TURBOPACK, .data_size = 32, .pack_size = 8, .nk = 1, .tol = 0.45, .max_tol = 0.8},
        {.data = data_d, .data_type = FST_TYPE_REAL, .data_size = 64, .pack_size = 64, .nk = NUM_DATA_Z, .tol = 0.0, .max_tol = 0.0},
        // {.data = data_d, .data_type = FST_TYPE_REAL, .data_size = 64, .pack_size = 32, .nk = NUM_DATA_Z, .tol = 0.0, .max_tol = 0.0},
        {.data = data_f, .data_type = FST_TYPE_IEEE_16, .data_size = 32, .pack_size = 32, .nk = NUM_DATA_Z, .tol = 0.0, .max_tol = 0.0},
        {.data = data_f, .data_type = FST_TYPE_IEEE_16, .data_size = 32, .pack_size = 24, .nk = NUM_DATA_Z, .tol = 1e-7, .max_tol = 2e-4},
        {.data = data_f, .data_type = FST_TYPE_IEEE_16, .data_size = 32, .pack_size = 16, .nk = NUM_DATA_Z, .tol = 1e-5, .max_tol = 2e-3},
        {.data = data_f, .data_type = FST_TYPE_IEEE_16, .data_size = 32, .pack_size = 12, .nk = NUM_DATA_Z, .tol = 2e-4, .max_tol = 0.06},
    };

    for (int i = 0; i < NUM_CASES; i++) {
        rec.data  = params[i].data;
        rec.datyp = params[i].data_type;
        rec.dasiz = params[i].data_size;
        rec.npak  = -params[i].pack_size;
        rec.nk    = params[i].nk;
        if (fst24_write(test_file, &rec, 0) <= 0) {
            App_Log(APP_ERROR, "%s: Could not write record to test file\n");
            fst24_record_print(&rec);
            return -1;
        }
    }

    if (fst24_close(test_file) <= 0) {
        App_Log(APP_ERROR, "Unable to close new file %s\n", test_filename);
        return -1;
    }

    free(test_file);

    test_file = fst24_open(test_filename, "R/O");
    if (test_file == NULL) {
        App_Log(APP_ERROR, "Unable to open recently-created test file with name %s\n", test_filename);
        return -1;
    }

    fst_record rec_read;
    for (int i = 0; i < NUM_CASES; i++) {
        if (fst24_read_next(test_file, &rec_read) <= 0) {
            App_Log(APP_ERROR, "Unable to read record from file\n");
            return -1;
        }

        if (params[i].data_size == 64) {
            if (compare_data_d(params[i].data, rec_read.data, NUM_DATA_X, NUM_DATA_Y, params[i].nk,
                            params[i].tol, params[i].max_tol)
                != 0)
            {
                App_Log(APP_ERROR, "%s: Data read is not the same (type %d, pack %d, size %d)\n",
                        __func__, rec_read.datyp, -rec_read.npak, rec_read.dasiz);
                return -1;
            }
        }
        else {
            if (compare_data_f(params[i].data, rec_read.data, NUM_DATA_X, NUM_DATA_Y, params[i].nk,
                            params[i].tol, params[i].max_tol)
                != 0)
            {
                App_Log(APP_ERROR, "%s: Data read is not the same (type %d, pack %d, size %d)\n",
                        __func__, rec_read.datyp, -rec_read.npak, rec_read.dasiz);
                return -1;
            }
        }
    }

    // fst24_record_free(&rec);
    fst24_record_free(&rec_read);

    if (fst24_close(test_file) <= 0) {
        App_Log(APP_ERROR, "Unable to close new file %s\n", test_filename);
        return -1;
    }
    free(test_file);

    return 0;
}

int main(void) {

    make_data();

    App_Log(APP_INFO, "Testing XDF\n");
    if (test_compression(0) != 0) return -1;

    App_Log(APP_INFO, "Testing RSF\n");
    if (test_compression(1) != 0) return -1;

    free(data_f);
    free(data_d);

    return 0;
}