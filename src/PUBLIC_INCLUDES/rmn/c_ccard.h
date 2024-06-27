#ifndef C_CCARD_H
#define C_CCARD_H

//! Maximum key value length
#define CCARD_NCARMAX 256
//! Maximum number of keys
#define CCARD_NKLEMAX 100

#ifdef __cplusplus
extern "C" {
#endif

void c_ccard(
    char **argv,
    const int argc,
    char **names,
    char vals[][CCARD_NCARMAX],
    char ** const defs,
    const int nbKeys,
    int * const npos
);

#ifdef __cplusplus
}
#endif

#endif
