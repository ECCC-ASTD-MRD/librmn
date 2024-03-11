#include <math.h>

#include <App.h>
#include <rmn.h>

const char* test_filename = "compression.fst";

// const int NUM_DATA_X = 16;
// const int NUM_DATA_Y = 8;
// const int NUM_DATA_Z = 4;
const int NUM_DATA_X = 4;
const int NUM_DATA_Y = 5;
const int NUM_DATA_Z = 8;

static float* data_f = NULL;
static double* data_d = NULL;

double gen_value_real(const int i, const int j, const int k, const int num_x, const int num_y, const int num_z) {
    double val = sin((double)i / ((double)num_x / 2)) + cos((double)j / ((double)num_y / 4)) + pow(2.0, (double)k / num_z);
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
        const int NUM_COL = num_x * num_y;
        const int NUM_ROW = num_z;
        const int COL_WIDTH = 7;
        for (int j = 0; j < NUM_ROW; j++) {
            sprintf(ptr, "a: ");
            ptr += 3;
            for (int i = 0; i < NUM_COL; i++) {
                sprintf(ptr, " %6.3f", a[i + j * NUM_COL]);
                ptr += COL_WIDTH;
            }
            ptr[0] = '\n'; ptr++;
            sprintf(ptr, "b: ");
            ptr += 3;
            for (int i = 0; i < NUM_COL; i++) {
                sprintf(ptr, " %6.3f", b[i + j * NUM_COL]);
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

    fst_record* rec_f = fst24_record_new(data_f, FST_TYPE_REAL, 32, NUM_DATA_X, NUM_DATA_Y, NUM_DATA_Z);
    if (rec_f == NULL) {
        App_Log(APP_ERROR, "%s: Unable to create new record\n", __func__);
        return -1;
    }

    rec_f->npak = -32;
    rec_f->dateo= 458021600;
    rec_f->deet = 300;
    rec_f->npas = 0;
    rec_f->ip1  = 1;
    rec_f->ip2  = 10;
    rec_f->ip3  = 100;
    strcpy(rec_f->typvar, "P");
    strcpy(rec_f->nomvar, "WAVE");
    strcpy(rec_f->etiket, "float");
    strcpy(rec_f->grtyp, "X");
    rec_f->ig1   = 0;
    rec_f->ig2   = 0;
    rec_f->ig3   = 0;
    rec_f->ig4   = 0;

    if (fst24_write(test_file, rec_f, 0) <= 0) {
        App_Log(APP_ERROR, "Could not write record (f 32) to test file\n");
        return -1;
    }

    rec_f->npak = -16;
    if (fst24_write(test_file, rec_f, 0) <= 0) {
        App_Log(APP_ERROR, "Could not write record (f 16) to test file\n");
        return -1;
    }

    rec_f->npak = -8;
    if (fst24_write(test_file, rec_f, 0) <= 0) {
        App_Log(APP_ERROR, "Could not write record (f 8) to test file\n");
        return -1;
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
    if (fst24_read_next(test_file, &rec_read) <= 0) {
        App_Log(APP_ERROR, "Unable to read record from file\n");
        return -1;
    }

    if (compare_data_f(data_f, rec_read.data, NUM_DATA_X, NUM_DATA_Y, NUM_DATA_Z, 0.f, 0.f) != 0) {
        App_Log(APP_ERROR, "Data read is not the same (f32)!\n");
        return -1;
    }

    if (fst24_read_next(test_file, &rec_read) <= 0) {
        App_Log(APP_ERROR, "Unable to read record from file\n");
        return -1;
    }

    if (compare_data_f(data_f, rec_read.data, NUM_DATA_X, NUM_DATA_Y, NUM_DATA_Z, 0.005, 0.01) != 0) {
        App_Log(APP_ERROR, "Data read is not the same (f16)!\n");
        return -1;
    }

    if (fst24_read_next(test_file, &rec_read) <= 0) {
        App_Log(APP_ERROR, "Unable to read record from file\n");
        return -1;
    }

    if (compare_data_f(data_f, rec_read.data, NUM_DATA_X, NUM_DATA_Y, NUM_DATA_Z, 0.5, 0.75) != 0) {
        App_Log(APP_ERROR, "Data read is not the same (f8)!\n");
        return -1;
    }

    fst24_record_free(rec_f);
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

    App_Log(APP_INFO, "Testing RSF\n");
    if (test_compression(1) != 0) return -1;

    App_Log(APP_INFO, "Testing XDF\n");
    if (test_compression(0) != 0) return -1;

    free(data_f);
    free(data_d);

    return 0;
}
