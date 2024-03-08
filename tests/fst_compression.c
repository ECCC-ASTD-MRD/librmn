#include <math.h>

#include <App.h>
#include <rmn.h>

const char* test_filename = "compression.fst";

double gen_value_real(const int i, const int j, const int k, const int num_x, const int num_y, const int num_z) {
    double val = sin((double)i / ((double)num_x / 2)) + cos((double)j / ((double)num_y / 4)) + pow(2.0, (double)k / num_z);
}

int test_compression(const int is_rsf) {
    remove(test_filename);

    const char* options = is_rsf ? "RSF" : "XDF";
    fst_file* test_file = fst24_open(test_filename, options);
    if (test_file == NULL) {
        App_Log(APP_ERROR, "Unable to open new test file with name %s and options %s\n", test_filename, options);
        return -1;
    }

    const int NUM_DATA_X = 10;
    const int NUM_DATA_Y = 10;
    const int NUM_DATA_Z = 10;

    float data_f[NUM_DATA_X][NUM_DATA_Y][NUM_DATA_Z];
    for (int i = 0; i < NUM_DATA_X; i++) {
        for (int j = 0; j < NUM_DATA_Y; j++) {
            for (int k = 0; k < NUM_DATA_Z; k++) {
                data_f[i][j][k] = (float) gen_value_real(i, j, k, NUM_DATA_X, NUM_DATA_Y, NUM_DATA_Z);
            }
        }
    }

    fst_record* rec = fst24_record_new(data_f, FST_TYPE_REAL, 32, NUM_DATA_X, NUM_DATA_Y, NUM_DATA_Z);
    rec->npak = -16;
    rec->dateo= 458021600;
    rec->deet = 300;
    rec->npas = 0;
    rec->ip1  = 1;
    rec->ip2  = 10;
    rec->ip3  = 100;
    strcpy(rec->typvar, "P");
    strcpy(rec->nomvar, "WAVE");
    strcpy(rec->etiket, "float");
    strcpy(rec->grtyp, "X");
    rec->ig1   = 0;
    rec->ig2   = 0;
    rec->ig3   = 0;
    rec->ig4   = 0;

    if (fst24_write(test_file, rec, 0) <= 0) {
        App_Log(APP_ERROR, "Could not write record to test file\n");
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

    // double total_diff_sq = 0.0;
    // for (int i = 0; i < NUM_DATA_X; i++) {
    //     for (int j = 0; j < NUM_DATA_Y; j++) {
    //         for (int k = 0; k < NUM_DATA_Z; k++) {
    //             const double diff = data_f[i][j][k] - ((float*)rec_read.data)[(i * NUM_DATA_Y + j) * NUM_DATA_Z + k];
    //             total_diff_sq += diff * diff;
    //         }
    //     }
    // }
    // const double diff_norm = sqrt(total_diff_sq);
    // App_Log(APP_WARNING, "Diff norm = %e\n", diff_norm);

    fst24_record_free(rec);
    fst24_record_free(&rec_read);

    if (fst24_close(test_file) <= 0) {
        App_Log(APP_ERROR, "Unable to close new file %s\n", test_filename);
        return -1;
    }
    free(test_file);

    return 0;
}

int main(void) {

    App_Log(APP_INFO, "Testing RSF\n");
    if (test_compression(1) != 0) return -1;

    App_Log(APP_INFO, "Testing XDF\n");
    if (test_compression(0) != 0) return -1;

    return 0;
}
