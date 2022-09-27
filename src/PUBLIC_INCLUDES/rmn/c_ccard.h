#ifndef C_CCARD_H
#define C_CCARD_H

#define CCARD_NCARMAX 256
#define CCARD_NKLEMAX 100

void c_ccard(
    const char ** const argv,
    const int argc,
    const char ** const keyNames,
    char val[][CCARD_NCARMAX],
    const char ** const def,
    const int nbKeys,
    int * const npos
);

#endif
