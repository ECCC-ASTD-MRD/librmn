
#include <mpi.h>

#include <App.h>
#include <rmn.h>

const char* filename_rsf = "multi_write.rsf";
const char* filename_xdf = "multi_write.xdf";

static int rank = -1;
static int size = -1;

static int run_test(const int is_rsf) {
    
    // Determine the filename and options based on the file type
    const char* filename = is_rsf ? filename_rsf : filename_xdf;
    const char* options = is_rsf ? "RSF+R/W" : "XDF+R/W";

    if (rank == 0) remove(filename);

    // ------------------------------
    MPI_Barrier(MPI_COMM_WORLD);

    fst_file* test_file = fst24_open(filename, options);

    if (test_file == NULL) {
        App_Log(APP_ERROR, "%s: Rank %d failed to open file %s with options %s\n", __func__, rank, filename, options);
        MPI_Abort(MPI_COMM_WORLD, -1);
    }

    int dummy_data = 0;
    fst_record rec = default_fst_record;
    rec.data_type = FST_TYPE_SIGNED;
    rec.data_bits = 32;
    rec.pack_bits = 32;
    rec.data = &dummy_data;
    rec.ni = 1;
    rec.nj = 1;
    rec.nk = 1;
    rec.dateo = 0;
    rec.deet = 0;
    rec.npas = 0;
    rec.ip1 = 0;
    rec.ip2 = 0;
    rec.ip3 = 0;
    rec.ig1 = 0;
    rec.ig2 = 0;
    rec.ig3 = 0;
    rec.ig4 = 0;
    sprintf(rec.etiket, "RANK%03d", rank);

    if (fst24_write(test_file, &rec, FST_NO) != TRUE) {
        App_Log(APP_ERROR, "%s: Rank %d failed to write record\n", __func__, rank);
        MPI_Abort(MPI_COMM_WORLD, -1);
    }

    if (fst24_close(test_file) != TRUE) {
        App_Log(APP_ERROR, "%s: Rank %d failed to close file %s\n", __func__, rank, filename);
        MPI_Abort(MPI_COMM_WORLD, -1);
    }

    // ------------------------------
    MPI_Barrier(MPI_COMM_WORLD);

    if (rank == 0) {
        test_file = fst24_open(filename, NULL);
        if (test_file == NULL) {
            App_Log(APP_ERROR, "%s: Could not open %s to check content at end of test\n", __func__, filename);
            MPI_Abort(MPI_COMM_WORLD, -1);
        }

        const int num_records = (int)fst24_get_num_records(test_file);
        if (num_records != size) {
            App_Log(APP_ERROR,
                "%s: Did not write the expected number of records (= number of procs). "
                "Got %d records, expected %d\n", __func__, num_records, size);
            MPI_Abort(MPI_COMM_WORLD, -1);
        }
    }

    
    return 0;
}

int main(int argc, char** argv) {

    MPI_Init(&argc, &argv);
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);
    MPI_Comm_size(MPI_COMM_WORLD, &size);

    if (size < 2) {
        if (rank == 0) {
            fprintf(stderr, "Error: This program requires at least 2 MPI processes (4+ is better).\n");
            MPI_Abort(MPI_COMM_WORLD, 1);
        }
    }

    if (run_test(1) != 0) MPI_Abort(MPI_COMM_WORLD, -1);

    // Feature unavailable for XDF files
    // if (run_test(0) != 0) MPI_Abort(MPI_COMM_WORLD, -1);

    MPI_Finalize();

    return 0;
}
