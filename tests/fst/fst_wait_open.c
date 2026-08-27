
#include <mpi.h>

#include <App.h>
#include <rmn.h>

static const char* test_filename_xdf = "write_wait.xdf";
static const char* test_filename_rsf = "write_wait.rsf";

static int rank = -1;


static int run_test(const int is_rsf) {
    
    const char* filename = is_rsf ? test_filename_rsf : test_filename_xdf;
    const char* options0 = is_rsf ? "RSF+R/W" : "XDF+R/W";

    fst_file* test_file = NULL;

    // Make sure we start from scratch
    if (rank == 0) {
        remove(filename);
    }

    // -------------------------
    MPI_Barrier(MPI_COMM_WORLD);

    ///////////////////////////////////////////////////////
    // TEST 1: newly created file
    ///////////////////////////////////////////////////////
    if (rank == 0) {
        test_file = fst24_open(filename, options0);
        if (test_file == NULL) {
            App_Log(APP_ERROR, "%s: Failed to open file %s with options %s\n", __func__, filename, options0);
            MPI_Abort(MPI_COMM_WORLD, -1);
        }
    }

    // ------------------------------
    MPI_Barrier(MPI_COMM_WORLD);

    if (rank == 1) {
        test_file = fst24_open(filename, NULL);
        if (test_file == NULL) {
            App_Log(APP_ERROR, "%s: Unable to open '%s' in read mode\n", __func__, filename);
            MPI_Abort(MPI_COMM_WORLD, -1);
        }
    }

    // ------------------------------
    MPI_Barrier(MPI_COMM_WORLD);

    if (fst24_close(test_file) != TRUE) {
        App_Log(APP_ERROR, "%s: Rank %d could not close file %s\n", __func__, rank, filename);
        MPI_Abort(MPI_COMM_WORLD, -1);
    }

    // ------------------------------
    MPI_Barrier(MPI_COMM_WORLD);

    ///////////////////////////////////////////////////////
    // Test 2: existing file (let 1 second timeout expire)
    ///////////////////////////////////////////////////////
    
    if (rank == 1) MPI_Barrier(MPI_COMM_WORLD);

    test_file = fst24_open(filename, "R/W");

    if (rank == 0) {
        if (test_file == NULL) {
            App_Log(APP_ERROR, "%s: Rank %d Failed to reopen %s\n", __func__, rank, filename);
            MPI_Abort(MPI_COMM_WORLD, -1);
        }
    }
    else { // Expecting rank 1 to fail
        if (test_file != NULL) {
            App_Log(APP_ERROR, "%s: Rank %d opened %s even though timeout has been reached...\n",
                __func__, rank, filename);
            MPI_Abort(MPI_COMM_WORLD, -1);
        }
    }

    if (rank == 0) {
        MPI_Barrier(MPI_COMM_WORLD);
        sleep_us(1 * 1000 * 1000); // Wait for timeout on opening (from rank 1) to expire
    }


    if (rank == 0 && fst24_close(test_file) != TRUE) {
        App_Log(APP_ERROR, "%s: Rank %d could not close file %s\n", __func__, rank, filename);
        MPI_Abort(MPI_COMM_WORLD, -1);
    }

    // ------------------------------
    MPI_Barrier(MPI_COMM_WORLD);

    ///////////////////////////////////////////////////////
    // Test 3: existing file (do NOT let 1 second timeout expire)
    ///////////////////////////////////////////////////////
    
    if (rank == 1) MPI_Barrier(MPI_COMM_WORLD);

    test_file = fst24_open(filename, "R/W");

    if (test_file == NULL) {
        App_Log(APP_ERROR, "%s: Rank %d Failed to reopen %s\n", __func__, rank, filename);
        MPI_Abort(MPI_COMM_WORLD, -1);
    }

    if (rank == 0) MPI_Barrier(MPI_COMM_WORLD);

    if (fst24_close(test_file) != TRUE) {
        App_Log(APP_ERROR, "%s: Rank %d could not close file %s\n", __func__, rank, filename);
        MPI_Abort(MPI_COMM_WORLD, -1);
    }

    // ------------------------------
    MPI_Barrier(MPI_COMM_WORLD);

    return 0;
}

int main(int argc, char** argv) {
    MPI_Init(&argc, &argv);

    int size;
    MPI_Comm_size(MPI_COMM_WORLD, &size);
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);

    if (size != 2) {
        App_Log(APP_ERROR, "%s: Need exactly 2 processes for this test", __func__);
        return MPI_Abort(MPI_COMM_WORLD, 1);
    }

    if (run_test(1) != 0) return MPI_Abort(MPI_COMM_WORLD, 1);

    // Feature unavailable for XDF files
    // if (run_test(0) != 0) return MPI_Abort(MPI_COMM_WORLD, 1);

    MPI_Finalize();

    App_Log(APP_ALWAYS, "Test successful\n");
    return 0;
}
