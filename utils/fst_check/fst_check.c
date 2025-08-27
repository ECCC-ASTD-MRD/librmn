
#include <unistd.h>

#include <App.h>
#include <rmn.h>

int main(int argc, char** argv) {

    if (argc < 2) {
        App_Log(APP_ALWAYS, "Must provide a file name.\n");
        return -2;
    }

    const char* filename = argv[1];

    if (access(filename, F_OK) != 0) {
        App_Log(APP_ERROR, "File '%s' does not seem to exist\n", filename);
        return -3;
    }

    if (fst24_is_valid(filename) != TRUE) {
        App_Log(APP_INFO, "File '%s' is not a valid RPN Standard file\n", filename);
        return -1;
    }

    fst_file* file = fst24_open(filename, "R/W");
    if (file == NULL) {
        App_Log(APP_INFO, "File '%s' is an RPN Standard file, but cannot be opened for writing\n", filename);
        return 1;
    }
    fst24_close(file);

    App_Log(APP_INFO, "File '%s' is a valid RPN Standard file\n", filename);

    return 0;
}
