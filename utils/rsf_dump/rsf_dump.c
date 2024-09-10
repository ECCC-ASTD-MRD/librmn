
#include <rmn/rsf.h>
#include <App.h>

void usage(char **argv) {
    App_Log(APP_VERBATIM, "Usage : %s rsf_file [verbosity level]\n", argv[0]);
}

int main(int argc, char **argv) {

    if(argc < 2) {
        usage(argv);
        return 1;
    }

    int64_t verbose = 0;
    if(argc > 2) verbose = atoi(argv[2]);

    // Print ls information for that file
    char command[1024];
    snprintf(command, sizeof(command), "ls -l %s", argv[1]);
    system(command);

    if (verbose >= 0) {
        RSF_Dump(argv[1], verbose);
    } else {
        const int32_t meta_dim = 0;
        RSF_handle h1 = RSF_Open_file(argv[1], RSF_RO, meta_dim, "demo", NULL);
        App_Log(APP_INFO, "file '%s', meta_dim = %d\n", argv[1], meta_dim) ;
        for (int i = 0; i < 5; i++) {
            uint64_t key = i + 1;
            key += 0x100000000ul;  // simulate file slot 0 for this file
            void* p = RSF_Get_record(h1, key, 0, NULL);
            if (p) free(p);
        }

        RSF_Dump_vdir(h1);
    }

    return 0;
}
