#ifndef ARMN_COMPRESS_H
#define ARMN_COMPRESS_H

typedef void *(*PackFunctionPointer)(
    void *unpackedArrayOfFloat,
    void *packedHeader,
    void *packedArrayOfInt,
    int elementCount,
    int bitSizeOfPackedToken,
    int off_set,
    int stride,
    int opCode,
    int hasMissing,
    void *missingTag
);

int armn_compress(unsigned char *fld, int ni, int nj, int nk, int nbits, int op_code);

int compact_integer(
    void *unpackedArrayOfInt,
    void *packedHeader,
    void *packedArrayOfInt,
    int elementCount,
    int bitSizeOfPackedToken,
    int off_set,
    int stride,
    int opCode
);

void *compact_float(
    void *unpackedArrayOfFloat,
    void *packedHeader,
    void *packedArrayOfInt,
    const int elementCount,
    const int packedTokenBitSize,
    const int offset,
    const int stride,
    const int opCode,
    const int hasMissing,
    const void * const missingTag
);

void *compact_double(
    void *unpackedArrayOfFloat,
    void *packedHeader,
    void *packedArrayOfInt,
    const int elementCount,
    const int packedTokenBitSize,
    const int offset,
    const int stride,
    const int opCode,
    const int hasMissing,
    const void * const missingTag
);

#endif
