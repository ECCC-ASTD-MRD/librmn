#ifndef C_CCARD_H
#define C_CCARD_H

#define CCARD_NCARMAX 256
#define CCARD_NKLEMAX 100


void c_ccard(
    char ** argv,
    int argc,
    char **  keyNames,
    char val[][CCARD_NCARMAX],
    char **  def,
    int nbKeys,
    int *  npos
);

#endif
