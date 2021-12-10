#ifndef ARMN_COMPRESS_32_H
#define ARMN_COMPRESS_32_H

void unpackTokensParallelogram(unsigned short ufld[], unsigned int z[], int ni, int nj, int nbits, int istep, uint32_t *header);
int c_fstzip_parallelogram32(unsigned int *zfld, int *zlng, unsigned int *fld, int ni, int nj, int step, int nbits, int32_t *header);

#endif
