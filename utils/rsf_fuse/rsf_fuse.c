
#include <App.h>
#include <rmn/fst24_file.h>

//! Utility that fuses the segments of an RSF file. It simply opens the file in "fuse" mode, then closes it.

int main(int argc, char** argv) {

    if (argc < 2) {
        App_Log(APP_WARNING, "Need to give a filename\n");
        return 1;
    }

    char command[2048];
    snprintf(command, sizeof(command), "ls -l %s", argv[1]);
    system(command);

    RSF_handle h = RSF_Open_file(argv[1], RSF_FUSE, 0, "STDF", NULL);  // open file

    if (h.p == NULL) {
        App_Log(APP_ERROR, "Unable to open file %s for fusing\n", argv[1]);
        return -1;
    }

    RSF_Close_file(h);

    return 0;
}
