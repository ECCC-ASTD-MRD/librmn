
#include <mpi.h>

#include <App.h>
#include <rmn.h>

//! Test how several processes can try to open the same file concurrently
//! - Only one should be able to open it in write mode.
//! - If the file is already open in write mode, other processes should be able to open it in read-only mode.
int main(int argc, char** argv) {
    MPI_Init(&argc, &argv);

    int rank;
    int num_procs;
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);
    MPI_Comm_size(MPI_COMM_WORLD, &num_procs);

    const char* filename = "multi_open.rsf";

    int status = 0;
    if (rank == 0) {
        App_Log(APP_ALWAYS, "Running with %d procs\n", num_procs);
        remove(filename);
        fst_file* tmp = fst24_open(filename, "RSF+R/W");
        if (tmp == NULL) {
            App_Log(APP_ERROR, "Unable to create test file '%s'\n", filename);
            status = -1;
        }
        else {
            if (fst24_close(tmp) != TRUE) {
                App_Log(APP_ERROR, "Unable to close test file '%s' after creation\n", filename);
                status = -1;
            }
        }
    }

    MPI_Bcast(&status, 1, MPI_INT, 0, MPI_COMM_WORLD);
    if (status != 0) return -1;

    // Trigger FST + RSF initialization for every PE. Otherwise it's always rank 0 that will open in the next step
    // and there won't be any contention.
    {
        fst_file* tmp = fst24_open(filename, NULL);
        if (tmp) fst24_close(tmp);
        MPI_Barrier(MPI_COMM_WORLD);
    }

    fst_file* f = fst24_open(filename, "R/W");

    int is_open = 0;
    int num_open = 0;

    if (f != NULL) is_open = 1;

    MPI_Allreduce(&is_open, &num_open, 1, MPI_INT, MPI_SUM, MPI_COMM_WORLD);

    if (rank == 0) {
        App_Log(APP_ALWAYS, "%d file(s) were open\n", num_open);
    }

    if (is_open) { App_Log(APP_ALWAYS, "Rank %d opened the file\n", rank); }

    if (num_open != 1) return -1;

    // Open in read-only mode for the other PEs
    if (!is_open) {
        f = fst24_open(filename, "R/O");
        if (f == NULL) {
            App_Log(APP_ERROR,
                    "Process rank %d should be able to open test file '%s' in read-only mode\n", rank, filename);
        }
        else {
            is_open = 1;
        }
    }

    MPI_Allreduce(&is_open, &num_open, 1, MPI_INT, MPI_SUM, MPI_COMM_WORLD);
    MPI_Finalize();

    if (num_open != num_procs) {
        if (rank == 0) {
            App_Log(APP_ERROR, "All other processes should be able to open the file in read-only mode\n");
        }
        return -1;
    }


    return 0;
}
