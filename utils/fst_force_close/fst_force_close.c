
#include <App.h>
#include <rmn.h>

int main(int argc, char** argv) {
    
    if (argc < 2) {
        App_Log(APP_ALWAYS, "Usage: %s file_path\n", argv[0]);
        return 1;
    }

    const char* filename = argv[1];
    if (fst24_force_close(filename) != TRUE) {
        App_Log(APP_ERROR, "Unable to force close file %s\n", filename);
        return -1;
    }

    return 0;
}


