#include <omp.h>

#include <stdlib.h>
#include <string.h>

#include <App.h>
#include <rmn.h>

//! Test the OPEN_WAIT_TIME feature with several threads opening the same RSF file
//! in (exclusive) write mode at the same time.
//!
//! An RSF file opened in R/W mode (without the PARALLEL option) is locked for
//! exclusive write: only one thread can hold it open in write mode at a time.
//! When another thread tries to open the same file in write mode while it is
//! already locked, it waits (polling) for up to OPEN_WAIT_TIME seconds for the
//! file to be released, then opens it.
//!
//! This test:
//!  - creates an empty RSF file,
//!  - has NUM_THREADS threads race to open it in write mode (RSF+R/W) at the
//!    same time; only one holds the file at a time, the others wait,
//!  - records the wall-clock time each thread spent in fst24_open (including
//!    the time spent waiting for the lock to be released),
//!  - has each thread write one record of about 10 MB (content is irrelevant)
//!    and records the wall-clock time of that write,
//!  - verifies that every thread managed to open the file and that all records
//!    were written,
//!  - closes every instance.
//!
//! OPEN_WAIT_TIME is set through the FST_OPTIONS environment variable (see the
//! CMake test registration). The test does not fail based on the measured times;
//! it only reports them (min / max / average) so they can be inspected.

const char* test_filename = "open_write_threads.rsf";

//! About 10 MB of 32-bit floats per record: 2560 * 1024 elements * 4 bytes = 10 MiB
const int32_t NUM_ELEM_X = 2560;
const int32_t NUM_ELEM_Y = 1024;
const size_t RECORD_BYTES = (size_t)NUM_ELEM_X * NUM_ELEM_Y * sizeof(float);

int main(int argc, char** argv) {
    (void)argc;
    (void)argv;

    remove(test_filename);

    // Create the file up front so that every thread is opening an existing file
    // (the contention is on the open, not on the file creation).
    {
        fst_file* f = fst24_open(test_filename, "RSF+R/W");
        if (f == NULL) {
            App_Log(APP_ERROR, "%s: Unable to create test file '%s'\n", __func__, test_filename);
            return -1;
        }
        if (fst24_close(f) != TRUE) {
            App_Log(APP_ERROR, "%s: Unable to close test file '%s' after creation\n", __func__, test_filename);
            return -1;
        }
    }

    const char* options = "RSF+R/W";

    // Number of threads is chosen at runtime; it honours OMP_NUM_THREADS (and
    // other OpenMP environment variables) via omp_get_max_threads().
    const int num_threads = omp_get_max_threads();
    double* open_times = (double*)malloc(num_threads * sizeof(double));
    double* write_times = (double*)malloc(num_threads * sizeof(double));
    if (open_times == NULL || write_times == NULL) {
        App_Log(APP_ERROR, "%s: Could not allocate timing arrays for %d threads\n", __func__, num_threads);
        free(open_times);
        free(write_times);
        return -1;
    }
    App_Log(APP_ALWAYS, "%s: Using %d threads (set OMP_NUM_THREADS to change)\n", __func__, num_threads);

    int num_errors = 0;
    int num_open = 0;

#pragma omp parallel shared(open_times, write_times, num_errors, num_open, options)
    {
        const int thread_id = omp_get_thread_num();

        // Allocate and initialize the record data before opening the file, so
        // that the time the file is held open (locked) is minimized
        float* data = (float*)malloc(RECORD_BYTES);
        if (data == NULL) {
            App_Log(APP_ERROR, "%s: Thread %d could not allocate %zu bytes\n", __func__, thread_id, RECORD_BYTES);
            #pragma omp atomic
            num_errors++;
        }
        else {
            for (size_t i = 0; i < RECORD_BYTES / sizeof(float); i++) data[i] = 1.0f;

            const double t_start = omp_get_wtime();
            fst_file* f = fst24_open(test_filename, options);
            const double t_end = omp_get_wtime();

            if (f == NULL) {
                App_Log(APP_ERROR, "%s: Thread %d failed to open '%s' in write mode (options '%s')\n",
                        __func__, thread_id, test_filename, options);
                #pragma omp atomic
                num_errors++;
            }
            else {
                open_times[thread_id] = (t_end - t_start) * 1000.0; // ms
                #pragma omp atomic
                num_open++;

                fst_record rec = default_fst_record;
                rec.data = data;
                rec.data_type = FST_TYPE_REAL_IEEE;
                rec.data_bits = 32;
                rec.pack_bits = 32;
                rec.ni = NUM_ELEM_X;
                rec.nj = NUM_ELEM_Y;
                rec.nk = 1;
                rec.dateo = 0;
                rec.datev = 0;
                rec.deet = 300;
                rec.npas = 0;
                rec.ip1 = 1;
                rec.ip2 = thread_id;
                rec.ip3 = 0;
                rec.ig1 = 0;
                rec.ig2 = 0;
                rec.ig3 = 0;
                rec.ig4 = 0;
                strcpy(rec.typvar, "P");
                strcpy(rec.nomvar, "WAVE");
                strcpy(rec.etiket, "float");
                strcpy(rec.grtyp, "X");

                const double w_start = omp_get_wtime();
                if (fst24_write(f, &rec, FST_NO) <= 0) {
                    App_Log(APP_ERROR, "%s: Thread %d failed to write its record\n", __func__, thread_id);
                    #pragma omp atomic
                    num_errors++;
                }
                const double w_end = omp_get_wtime();
                write_times[thread_id] = (w_end - w_start) * 1000.0; // ms

                // Simulate doing a lot of stuff with the file open, to increase the chance of contention with other threads
                sleep_us(1000 * 100);

                fst24_close(f);
            }
            free(data);
        }
    }

    if (num_errors > 0) {
        App_Log(APP_ERROR, "%s: %d thread(s) failed to open the file in write mode\n", __func__, num_errors);
        return -1;
    }
    if (num_open != num_threads) {
        App_Log(APP_ERROR, "%s: Expected %d concurrent write opens, got %d\n", __func__, num_threads, num_open);
        return -1;
    }

    // Verify that all records were written
    {
        fst_file* f = fst24_open(test_filename, "R/O");
        if (f == NULL) {
            App_Log(APP_ERROR, "%s: Unable to reopen '%s' to verify the records\n", __func__, test_filename);
            return -1;
        }
        const int64_t num_records = fst24_get_num_records(f);
        fst24_close(f);
        if (num_records != num_threads) {
            App_Log(APP_ERROR, "%s: Expected %d records in the file, found %lld\n",
                    __func__, num_threads, (long long)num_records);
            return -1;
        }
    }

    // Report the measured open and write times
    double min_open = open_times[0];
    double max_open = open_times[0];
    double total_open = 0.0;
    double min_write = write_times[0];
    double max_write = write_times[0];
    double total_write = 0.0;
    for (int i = 0; i < num_threads; i++) {
        App_Log(APP_ALWAYS, "%s: Thread %3d: open = %6.1f ms, write = %6.1f ms\n",
                __func__, i, open_times[i], write_times[i]);
        if (open_times[i] < min_open) min_open = open_times[i];
        if (open_times[i] > max_open) max_open = open_times[i];
        total_open += open_times[i];
        if (write_times[i] < min_write) min_write = write_times[i];
        if (write_times[i] > max_write) max_write = write_times[i];
        total_write += write_times[i];
    }
    App_Log(APP_ALWAYS, "%s: %d concurrent write opens of '%s' succeeded\n", __func__, num_open, test_filename);
    App_Log(APP_ALWAYS, "%s: open  time: min = %5.1f ms, max = %5.1f ms, avg = %5.1f ms\n",
            __func__, min_open, max_open, total_open / num_threads);
    App_Log(APP_ALWAYS, "%s: write time: min = %5.1f ms, max = %5.1f ms, avg = %5.1f ms\n",
            __func__, min_write, max_write, total_write / num_threads);

    free(open_times);
    free(write_times);
    return 0;
}
