
#include <App.h>
#include <rmn/fst24_file.h>

#define NUM_X 8
#define NUM_Y 8
#define NUM_Z 5

static const char* test_filename = "cube.fst";
static int32_t initial_data[NUM_Z][NUM_X][NUM_Y];

int create_file() {
    for (int i_level = 0; i_level < NUM_Z; i_level++) {
        for (int i = 0; i < NUM_X; i++) {
            for (int j = 0; j < NUM_Y; j++) {
                const int value = (i_level * NUM_X + i) * NUM_Y + j;
                initial_data[i_level][i][j] = value;
            }
        }
    }

    remove(test_filename);

    fst_file* test_file = fst24_open(test_filename, "RSF");
    if (test_file == NULL) {
        App_Log(APP_ERROR, "Unable to open test file for creation\n");
        return -1;
    }

    fst_record rec = default_fst_record;

    rec.ni = NUM_X;
    rec.nj = NUM_Y;
    rec.nk = 1;
    rec.datyp = FST_TYPE_SIGNED;
    rec.dasiz = 32;
    rec.npak = -32;

    rec.dateo = 0;
    rec.ip1 = 1;
    rec.ip2 = 1;
    rec.ip3 = 1;
    rec.deet = 0;
    rec.npas = 0;
    rec.ig1 = 0;
    rec.ig2 = 0;
    rec.ig3 = 0;
    rec.ig4 = 0;

    for (int i_level = 0; i_level < NUM_Z; i_level++) {
        rec.data = &(initial_data[i_level][0][0]);
        rec.ig1 = i_level + 1;
        if (fst24_write(test_file, &rec, 0) < 0) {
            App_Log(APP_ERROR, "Unable to write slice\n");
            return -1;
        }
    }

    fst24_close(test_file);
    free(test_file);

    return 0;
}

int check_result(int32_t data[NUM_Z][NUM_X][NUM_Y]) {
    for (int k = 0; k < NUM_Z; k++) {
        for (int i = 0; i < NUM_X; i++) {
            for (int j = 0; j < NUM_Y; j++) {
                if (data[k][i][j] != initial_data[k][i][j]) {
                    App_Log(APP_ERROR, "Data is not the same!\n");

                    for (int i_lvl = 0; i_lvl < NUM_Z; i_lvl++) {
                        for (int i_row = 0; i_row < NUM_X; i_row++) {
                            for (int i_col = 0; i_col < NUM_Y; i_col++) {
                                fprintf(stderr, "%3d ", data[i_lvl][i_row][i_col]);
                            }
                            fprintf(stderr, "  ");
                            for (int i_col = 0; i_col < NUM_Y; i_col++) {
                                fprintf(stderr, "%3d ", initial_data[i_lvl][i_row][i_col]);
                            }
                            fprintf(stderr, "\n");
                        }
                        fprintf(stderr, "\n");
                    }

                    return -1;
                }
            }
        }
    }

    return 0;
}

int main(void) {

    if (create_file() < 0) return -1;

    int32_t read_data[NUM_Z][NUM_X][NUM_Y];
    memset(read_data, 0, sizeof(read_data));

    fst_file* test_file = fst24_open(test_filename, NULL);
    if (test_file == NULL) {
        App_Log(APP_ERROR, "Unable to open test file for reading\n");
        return -1;
    }

    fst_record rec = default_fst_record;
    fst_record criteria = default_fst_record;
    for (int i_level = 0; i_level < NUM_Z; i_level++) {
        rec.data = &(read_data[i_level][0][0]);
        criteria.ig1 = i_level + 1;
        if (fst24_read(test_file, &criteria, NULL, &rec) <= 0) {
            App_Log(APP_ERROR, "Unable to read slice %d\n", i_level);
            return -1;
        }
    }

    if (check_result(read_data) < 0) return -1;

    fst24_close(test_file);
    free(test_file);

    App_Log(APP_INFO, "Test successful\n");

    return 0;
}
