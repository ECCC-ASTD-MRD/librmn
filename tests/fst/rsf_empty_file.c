#include <App.h>
#include <rmn/fst24_file.h>

int main(void) {

    const char* filename = "dummy.rsf";

    remove(filename);

    fst_file* f = fst24_open(filename, "RSF+R/W");
    fst24_close(f);
    free(f);

    f = fst24_open(filename, NULL);
    if (f == NULL) {
        App_Log(APP_ERROR, "Unable to open newly-created empty file\n");
        return -1;
    }

    fst24_close(f);

    return 0;
}
