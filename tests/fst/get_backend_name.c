#include <stdio.h>

#include <App.h>
#include <rmn.h>

//! Print the backend name (RSF or XDF) of the given FST file to stdout.
//!
//! This is a helper executable, not a standalone test: it is built (via gen_test_target with DISABLE)
//! and invoked by a multi-executable test, which compares the printed name to the expected backend.
//!
//! Usage: get_backend_name <file>
//!
//! On success, prints the backend name ("RSF" or "XDF") to stdout and returns 0.
//! On any failure (missing argument, file cannot be opened, backend cannot be determined),
//! prints a message to stderr and returns 1.
int main(int argc, char** argv) {

    if (argc < 2) {
        App_Log(APP_ERROR, "Usage: %s <file>\n", argv[0]);
        return 1;
    }

    const char* filename = argv[1];

    fst_file* file = fst24_open(filename, "R/O");
    if (file == NULL) {
        App_Log(APP_ERROR, "Unable to open file '%s'\n", filename);
        return 1;
    }

    const char* name = fst24_backend_name(file);
    if (name == NULL) {
        App_Log(APP_ERROR, "Unable to determine the backend of file '%s'\n", filename);
        fst24_close(file);
        return 1;
    }

    printf("%s\n", name);
    fst24_close(file);
    return 0;
}
