#ifndef ARMN_COMPRESS_32_H
#define ARMN_COMPRESS_32_H

int  c_armn_compress32(unsigned char *, float *, int, int, int, int);
int  c_armn_uncompress32(float *fld, unsigned char *zstream, int ni, int nj, int nk, int nchiffres_sign);

#endif
