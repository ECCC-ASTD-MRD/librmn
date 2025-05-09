#ifndef RMN_PACKERS_H__
#define RMN_PACKERS_H__

int compact_char(void *unpackedArrayOfBytes, void *packedHeader, void *packedArrayOfInt, int elementCount,
                  int bitSizeOfPackedToken, int off_set, int stride, int opCode);
int compact_p_integer(
    const void * const unpackedArrayOfInt,
    void * const packedHeader,
    void * const packedArrayOfInt,
    int intCount,
    int bitSizeOfPackedToken,
    int offset,
    int stride,
    const int sign
);
int compact_u_integer(
    void * const unpackedArrayOfInt,
    const void * const packedHeader,
    const void * const packedArrayOfInt,
    int intCount,
    int bitSizeOfPackedToken,
    int offset,
    int stride,
    const int sign
);
int compact_p_short(
    const void * const unpackedArray,
    void * const packedHeader,
    void * const packedArray,
    int intCount,
    const int bitSizeOfPackedToken,
    const int offset,
    const int stride
);
int compact_u_short(
    void * const unpackedArray,
    void * const packedHeader,
    const void * const packedArray,
    int intCount,
    const int bitSizeOfPackedToken,
    const int offset,
    const int stride
);
int compact_rle(void *unpackedArrayOfInt, void *packedHeader, void *packedArrayOfInt, int max, int min,
                 int elementCount, int bitSizeOfPackedToken, int off_set, int stride, int opCode);

int32_t c_float_packer(float *source, int32_t nbits, int32_t *header, int32_t *stream, int32_t npts);
void c_float_packer_params(int32_t *header_size, int32_t *stream_size, int32_t *p1, int32_t *p2, int32_t npts);
int32_t c_float_unpacker(float *dest, int32_t *header, int32_t *stream, int32_t npts, int32_t *nbits );

#endif // RMN_PACKERS_H__
