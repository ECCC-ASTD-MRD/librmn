#ifndef COMPACT_IEEEBLOCK_H
#define COMPACT_IEEEBLOCK_H

void *compact_IEEEblock_float(
    void *unpackedArrayOfFloat,
    void *packedHeader,
    void *packedArrayOfInt,
    int elementCount,
    int bitSizeOfPackedToken,
    int bitSizeOfPackedExpo,
    int off_set,
    int stride,
    int opCode,
    int hasMissing,
    void *missingTag
);

void *compact_IEEEblock_double(
    void *unpackedArrayOfFloat,
    void *packedHeader,
    void *packedArrayOfInt,
    int elementCount,
    int bitSizeOfPackedToken,
    int bitSizeOfPackedExpo,
    int off_set,
    int stride,
    int opCode,
    int hasMissing,
    void *missingTag
);

#endif
