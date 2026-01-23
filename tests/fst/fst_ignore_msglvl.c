#include <App.h>
#include <rmn.h>

const char* filename = "file_that_does_not_exist";

int main(void) {

    remove(filename);
    fst_file* f = fst24_open(filename, "RSF+R/W");
    fst24_close(f);

    App_Log(APP_ALWAYS, "Expecting a warning\n");
    f = fst24_open(filename, "RSF+R/W");
    fst24_close(f);


    c_fstopi("MSGLVL", APP_ERROR, 0);
    App_Log(APP_ALWAYS, "Expecting no warning\n");
    f = fst24_open(filename, "RSF+R/W");
    fst24_close(f);

    c_fstopc("MSGLVL", "WARNING", 0);
    App_Log(APP_ALWAYS, "Expecting a warning\n");
    f = fst24_open(filename, "RSF+R/W");
    fst24_close(f);

    return 0;
}
