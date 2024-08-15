#include <errno.h>
#include <stdlib.h>

#include <omp.h>

#include <App.h>
#include <rmn.h>

const char* tmp_dir = NULL;

void print_bitmap(void);

int clear_test_files() {
    // Split the call, because rm fails if the argument list is too long
    char command[1024];
    for (int i = 0; i < 10; i++) {
        sprintf(command, "rm -rf %s/.abc*%d.test", tmp_dir, i);
        App_Log(APP_INFO, "Removing with \"%s\"\n", command);
        int status = system(command);
        if (status != 0) {
            App_Log(APP_ERROR, "Could not remove test files\n");
            return -1;
        }
    }

    sprintf(command, "rm -rf %s/abc*.test", tmp_dir);
    int status = system(command);
    if (status != 0) {
        App_Log(APP_ERROR, "Could not remove test files\n");
        return -1;
    }

    return 0;
}

void get_filename(char* name, const int index) {
    sprintf(name, "%s/.abc%08d.test", tmp_dir, index);
}

// Check that the iun are indeed unique
int check_iuns(const int32_t* iun_list, const int num_iuns) {

    int8_t usage[num_iuns * 2];
    for (int i = 0; i < num_iuns * 2; i++) usage[i] = 0;

    for (int i = 0; i < num_iuns; i++) {
        const int iun = iun_list[i];
        if (usage[iun] != 0) {
            App_Log(APP_ERROR, "iun %d is used more than once!\n", iun);
            return -1;
        }
        usage[iun] = 1;
    }

    return 0;
}

int check_just_fnom(const int num_files) {
    // Open max number of files
    int num_errors = 0;
    int32_t iun_list[num_files];

    App_Log(APP_INFO, "Testing fnom only\n");

    #pragma omp parallel for num_threads(8) reduction(+:num_errors)
    for (int i = 0; i < num_files; i++) {
        if (i % 5000 == 0 && i > 0) {
            App_Log(APP_INFO, "i = %d\n", i);
            // print_bitmap();
            // return -1;
        }
        // iun_list[i] = 0;
        // int iun = 0;
        // App_Log(APP_INFO, "%2ld: i = %6d\n", omp_get_thread_num(), i);
        char filename[256];
        get_filename(filename, i);
        iun_list[i] = 0;
        const int err = c_fnom(&iun_list[i], filename, "RND", 0);
        if (err != 0) {
            // if (num_errors == 0)
            App_Log(APP_ERROR, "Unable to fnom (i = %d)\n", i);
            num_errors++;
        }
    }

    if (num_errors > 0) {
        App_Log(APP_ERROR, "%d error(s) with simple fnom\n", num_errors);
        return -1;
    }

    if (check_iuns(iun_list, num_files) < 0) return -1;

    // Passing an int instead of a pointer
    {
        int64_t iun = 50;

        // print_bitmap();
        App_Log(APP_INFO, "Expecting next fnom call to fail\n");
        if (c_fnom((int* const)iun, "abc.test", "RND", 0) == 0) {
            App_Log(APP_ERROR, "Should not be able to fnom unit %d\n", iun);
            // print_bitmap();
            return -1;
        }

        if (c_fclos(iun) != 0) {
            App_Log(APP_ERROR, "Unable to close unit %d\n", iun);
            return -1;
        }

        if (c_fnom((int* const)iun, "abc.test", "RND", 0) != 0) {
            App_Log(APP_ERROR, "Unable to fnom unit %d\n", iun);
            return -1;
        }
    }
    
    // Trying to get a iun that's already taken. Opening with a given iun
    {
        int32_t iun = 51;

        App_Log(APP_INFO, "Expecting next fnom call to fail\n");
        if (c_fnom(&iun, "abc.test", "RND", 0) == 0) {
            App_Log(APP_ERROR, "Should not be able fnom %d.\n", iun);
            return -1;
        }

        if (c_fclos(iun) != 0) {
            App_Log(APP_ERROR, "Unable to close unit %d\n", iun);
            return -1;
        }

        if (c_fnom(&iun, "abc.test", "RND", 0) != 0) {
            App_Log(APP_ERROR, "Unable to fnom unit %d\n", iun);
            return -1;
        }
    }

    // Close everyone
    for (int i = 0; i < num_files; i++) {
        if (c_fclos(iun_list[i]) != 0) {
            App_Log(APP_ERROR, "Could not close iun %d\n", iun_list[i]);
            return -1;
        }
    }

    return 0;
}

int check_xdf(const int num_files) {

    App_Log(APP_INFO, "Testing XDF (fst24)\n");

    fst_file* files[num_files];

    int num_errors = 0;

    #pragma omp parallel num_threads(8) reduction(+:num_errors)
    {
        const int thread_id = omp_get_thread_num();
        // const int thread_id = 0;

        char filename[100];
        get_filename(filename, thread_id);

        const int num_threads = omp_get_num_threads();

        // Open a lot (more than max), one at a time
        #pragma omp for
        for (int i = 0; i < num_files * 5; i++) {
            // App_Log(APP_VERBATIM, "Opening XDF (%d at a time) %d\n", num_threads, i);
            files[thread_id] = fst24_open(filename, "XDF+R/W");
            if (files[thread_id] == NULL) {
                App_Log(APP_ERROR, "Unable to open XDF file %d\n", i);
                num_errors++;
            }

            if (!fst24_close(files[thread_id])) { num_errors++; }
        }
    }

    if (num_errors > 0) return -1;

    App_Log(APP_INFO, "Open XDF multi\n");
    // print_bitmap();


    num_errors = 0;

    // Open max, all at once (XDF)
    #pragma omp parallel for num_threads(9) reduction(+:num_errors)
    for (int i = 0; i < num_files; i++) {
        char filename[100];
        get_filename(filename, i);
        // App_Log(APP_VERBATIM, "Opening XDF %d (all at once)\n", i);
        files[i] = fst24_open(filename, "XDF+R/W");
        if (files[i] == NULL) {
            App_Log(APP_ERROR, "Unable to open XDF file %d\n", i);
            num_errors++;
        }
    }

    if (num_errors > 0) return -1;

    App_Log(APP_INFO, "Closing XDF multi\n");

    num_errors = 0;

    #pragma omp parallel for num_threads(7) reduction(+:num_errors)
    for (int i = 0; i < num_files; i++) {
        if (!fst24_close(files[i])) {
            App_Log(APP_ERROR, "Unable to close XDF file %d\n", i);
            num_errors++;
        }
    }

    if (num_errors > 0) return -1;

    return 0;
}

int check_rsf(const int num_files) {

    fst_file* files[num_files];

    App_Log(APP_INFO, "Testing RSF read-write (fst24)\n");

    int num_errors = 0;
    // Open max, all at once (RSF)
    #pragma omp parallel for num_threads(9) reduction(+:num_errors)
    for (int i = 0; i < num_files; i++) {
        char filename[100];
        get_filename(filename, i);
        files[i] = fst24_open(filename, "RSF+R/W+NEW");
        if (files[i] == NULL) {
            App_Log(APP_ERROR, "Unable to open RSF file %d\n", i);
            num_errors++;
        }
    }

    if (num_errors > 0) {
        return -1;
    }

    #pragma omp parallel for num_threads(10) reduction(+:num_errors)
    for (int i = 0; i < num_files; i++) {
        if (!fst24_close(files[i])) {
            App_Log(APP_ERROR, "Unable to close RSF file %d\n", i);
            num_errors++;
        }
    }

    if (num_errors > 0) {
        return -1;
    }

    App_Log(APP_INFO, "Testing RSF read-only (fst24)\n");

    num_errors = 0;

    // Open max, all at once (RSF)
    #pragma omp parallel for num_threads(7) reduction(+:num_errors)
    for (int i = 0; i < num_files; i++) {
        char filename[100];
        get_filename(filename, i);
        files[i] = fst24_open(filename, "RSF+R/O");
        if (files[i] == NULL) {
            App_Log(APP_ERROR, "Unable to open RSF file %d\n", i);
            num_errors++;
        }
    }

    if (num_errors > 0) return -1;

    num_errors = 0;
    #pragma omp parallel for num_threads(8) reduction(+:num_errors)
    for (int i = 0; i < num_files; i++) {
        if (!fst24_close(files[i])) {
            App_Log(APP_ERROR, "Unable to close RSF file %d\n", i);
            num_errors++;
        }
    }

    if (num_errors > 0) {
        return -1;
    }

    return 0;
}

int check_burp(const int num_files) {

    App_Log(APP_INFO, "Testing BURP (open)\n");

    int32_t iun_list[num_files];

    int num_errors = 0;

    // Open max, all at once (BURP)
    #pragma omp parallel for num_threads(9) reduction(+:num_errors)
    for (int i = 0; i < num_files; i++) {
        iun_list[i] = 0;
        char filename[100];
        get_filename(filename, i);
        if (c_fnom(&iun_list[i], filename, "RND", 0) != 0) {
            App_Log(APP_ERROR, "Unable to fnom %d (BURP)\n", i);
            num_errors++;
        }
        if (c_mrfopn(iun_list[i], "CREATE") != 0) {
            App_Log(APP_ERROR, "Unable to open (create) BURP file %d, iun = %d\n", i, iun_list[i]);
            num_errors++;
        }
    }

    if (num_errors > 0) return -1;

    App_Log(APP_INFO, "Testing BURP (close)\n");

    #pragma omp parallel for num_threads(8) reduction(+:num_errors)
    for (int i = 0; i < num_files; i++) {
        if (c_mrfcls(iun_list[i]) != 0) {
            App_Log(APP_ERROR, "Unable to mrfcls BURP file %d (iun %d)\n", i, iun_list[i]);
            num_errors++;
        }

        if (c_fclos(iun_list[i]) != 0) {
            App_Log(APP_ERROR, "Unable to fclos BURP file %d (iun %d)\n", i, iun_list[i]);
            num_errors++;
        }
    }

    if (num_errors > 0) return -1;

    return 0;
}

int main(void) {

    // First set the system limit for number of open files
    {
        struct rlimit limit;
  
        limit.rlim_cur = 5000;
        limit.rlim_max = 5000;
        if (setrlimit(RLIMIT_NOFILE, &limit) != 0) {
            App_Log(APP_ERROR, "setrlimit() failed with errno = %d\n", errno);
            return -1;
        }
    }

    tmp_dir = getenv("TMPDIR");
    if (tmp_dir == NULL) tmp_dir = ".";
    App_Log(APP_INFO, "TMPDIR = %s\n", tmp_dir);

    const int MAX_OPEN_FILES = get_max_open_files();
    const int TRY_OPEN = MAX_OPEN_FILES - 11;

    App_Log(APP_ALWAYS, "Max number of open files = %d\n", MAX_OPEN_FILES);

    if (clear_test_files() < 0) return -1;

    if (check_just_fnom(TRY_OPEN) < 0) return -1;

    if (clear_test_files() < 0) return -1;

    if (check_xdf(TRY_OPEN) < 0) return -1;

    if (clear_test_files() < 0) return -1;

    if (check_rsf(TRY_OPEN) < 0) return -1;

    if (clear_test_files() < 0) return -1;

    if (check_burp(TRY_OPEN) < 0) return -1;

    if (clear_test_files() < 0) return -1;

    App_Log(APP_INFO, "Test successful\n");

    return 0;
}
