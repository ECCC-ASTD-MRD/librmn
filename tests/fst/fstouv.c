#include <App.h>
#include <rmn.h>

int main(void) {

    int iun;

    // File that does not exist
    remove("abcde");
    iun = 0;
    c_fnom(&iun, "abcde", "RND", 0);

    int nbr = c_fstnbr(iun);
    if (nbr >= 0) {
        App_Log(APP_ERROR, "non-existing file should give error code\n");
        return -1;
    }

    c_fclos(iun);

    // Emtpy file
    iun = 0;
    c_fnom(&iun, "abcde", "RND+OLD", 0);
    nbr = c_fstnbr(iun);
    if (nbr >= 0) {
        App_Log(APP_ERROR, "empty file (not XDF) should give error code. %d\n", nbr);
        return -1;
    }
    c_fclos(iun);

    // XDF file
    remove("abcde.xdf");
    iun = 0;
    c_fnom(&iun, "abcde.xdf", "RND+NEW", 0);
    c_fstouv(iun, "RND+XDF");
    if (c_fst_is_rsf(iun)) {
        App_Log(APP_ERROR, "Should NOT be an RSF file!\n");
        return -1;
    }
    c_fstfrm(iun);
    c_fclos(iun);

    iun = 0;
    c_fnom(&iun, "abcde.xdf", "RND+OLD", 0);
    nbr = c_fstnbr(iun);
    if (nbr != 0) {
        App_Log(APP_ERROR, "There should be exactly 0 record in the file, not %d\n", nbr);
        return -1;
    }
    c_fstouv(iun, "RND+XDF+R/W");
    c_fstecr(&iun, NULL, 1, iun, 0, 0, 0, 1, 1, 1, 0, 0, 0, "  ", "    ", "           ", " ", 0, 0, 0, 0, FST_TYPE_BINARY, 0);
    c_fstfrm(iun);

    nbr = c_fstnbr(iun);
    if (nbr != 1) {
        App_Log(APP_ERROR, "There should be exactly 1 record in the file, not %d\n", nbr);
        return -1;
    }

    c_fclos(iun);

    iun = 0;
    c_fnom(&iun, "abcde.xdf", "RND+OLD", 0);
    nbr = c_fstnbr(iun);
    if (nbr != 1) {
        App_Log(APP_ERROR, "There should be exactly 1 record in the file, not %d\n", nbr);
        return -1;
    }
    c_fclos(iun);
    
    // RSF file
    remove("abcde.rsf");
    iun = 0;
    c_fnom(&iun, "abcde.rsf", "RND+NEW", 0);
    c_fstouv(iun, "RND+RSF");
    if (!c_fst_is_rsf(iun)) {
        App_Log(APP_ERROR, "Should be an RSF file!\n");
        return -1;
    }
    c_fstecr(&iun, NULL, 1, iun, 0, 0, 0, 1, 1, 1, 0, 0, 0, "  ", "    ", "           ", " ", 0, 0, 0, 0, FST_TYPE_BINARY, 0);
    c_fstfrm(iun);
    c_fclos(iun);

    iun = 0;
    c_fnom(&iun, "abcde.rsf", "RND+OLD", 0);
    nbr = c_fstnbr(iun);
    if (nbr != 1) {
        App_Log(APP_ERROR, "There should be exactly 1 record in the file, not %d\n", nbr);
        return -1;
    }
    c_fclos(iun);


    return 0;
}
