
#include <math.h>
#include <stdio.h>
#include <string.h>

#include <rmn/fst_file.h>

const char* test_file_name = "fst23_interface.fst";

int main(void) {

    // Create some arbitrary data field
    const int DATA_SIZE = 1024;
    float data[DATA_SIZE][DATA_SIZE];
    for (int i = 0; i < DATA_SIZE; i++) {
        for (int j = 0; j < DATA_SIZE; j++) {
            const float powi = pow((1.0 * i / DATA_SIZE), 2);
            const float powj = pow((1.0 * j / DATA_SIZE), 2);
            const float numerator = 1 + cos(12 * sqrt( powi + powj ));
            const float denominator = 0.5 * ( powi + powj ) + 2;
            data[i][j] = numerator / denominator;
            // printf("domain[%d][%d] = %f\n", i, j, domain[i][j]);
        }
    }

    remove(test_file_name);
    fst_file* test_file = fst23_open(test_file_name, "RND+R/W+RSF");

    fst_record record = default_fst_record;
    record.data = data;
    record.num_packed_bits = -32;
    record.num_elem_i = DATA_SIZE;
    record.num_elem_j = DATA_SIZE;
    record.num_elem_k = 1;
    record.date = 20220610;
    record.timestep_length = 300;
    record.timestep_num    = 0;
    record.ip1 = 1;
    record.ip2 = 1;
    record.ip3 = 1;
    strcpy(record.field_type, "P");
    strcpy(record.var_name, "WAVE");
    strcpy(record.label, "float");
    strcpy(record.projection_type, "X");
    record.grid_desc_1 = 0;
    record.grid_desc_2 = 0;
    record.grid_desc_3 = 0;
    record.grid_desc_4 = 0;
    record.datatype = FSTD_TYPE_IEEE;

    fst23_write(test_file, &record);
    fst23_close(test_file);

    return 0;
}
