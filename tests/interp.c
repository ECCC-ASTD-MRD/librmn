#include <stdint.h>
#include <stdio.h>

#include <rmn/base.h>
#include <rmn/fst98.h>
#include <rmn/fnom.h>
#include <rmn/ezscint.h>


int main(int argc, char * argv[]) {
    const char fileName[] = "ezscint.fst";

    const int32_t ni = 2800;
    const int32_t nj = 1000;
    const int32_t nk = 1;

    const int npac = -32;
    const int deet = 0;
    const int npas = 0;
    const int ip1 = 0;
    const int ip2 = 0;
    const int ip3 = 0;
    const int datyp = 5;
    const int rewrit = 0;

    const char grtyp[] = "L";
    const float xg1 = 17.0;
    const float xg2 = -167.0;
    const float xg3 = 0.05;
    const float xg4 = 0.05;
    int ig1;
    int ig2;
    int ig3;
    int ig4;
    // Convert grid parameters to their integer representation required for c_fstecr()
    f77name(cxgaig)(grtyp, &ig1, &ig2, &ig3, &ig4, &xg1, &xg2, &xg3, &xg4, 1);

    int32_t gid = c_ezgdef_fmem(ni, nj, "L", "", xg1, xg2, xg3, xg4, NULL, NULL);

    float fld[nj][ni];
    for (int32_t j = 0; j < nj; j++) {
        for (int32_t i = 0; i < ni; i++) {
            fld[j][i] = (j + i) % 2 == 0 ? -20 : 20;
        }
    }

    // Open file for writing
    const char optStr[] = "STD,R/W";
    int iun = 0;
    int res = c_fnom(&iun, fileName, optStr, 0);
    res = c_fstouv(iun, optStr);

    // newdate's mode 3 converts from human readable to the cmc timestamp format
    const int mode = 3;
    // Dates must be given as integer with the YYYYMMDD format
    int dateo = 20250201;
    int stampo;
    int thour = 0;
    newdate_c(&stampo, &dateo, &thour, &mode);
    res = c_fstecr(fld, NULL, npac, iun, stampo, deet, npas, ni, nj, nk, ip1, ip2, ip3,
        "P", "TT", "src", grtyp, ig1, ig2, ig3, ig4, datyp, rewrit);

    c_fstfrm(iun);

    return 0;
}
