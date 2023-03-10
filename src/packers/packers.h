#ifndef RMN_PACKERS_H__
#define RMN_PACKERS_H__

int  compact_char(void *unpackedArrayOfBytes, void *packedHeader, void *packedArrayOfInt, int elementCount,
                  int bitSizeOfPackedToken, int off_set, int stride, int opCode);
int  compact_integer(void *unpackedArrayOfInt, void *packedHeader, void *packedArrayOfInt, int elementCount,
                     int bitSizeOfPackedToken, int off_set, int stride, int opCode);
int  compact_short(void *unpackedArrayOfShort, void *packedHeader, void *packedArrayOfInt, int elementCount,
                   int bitSizeOfPackedToken, int off_set, int stride, int opCode);
                      
int32_t c_float_packer(float *source, int32_t nbits, int32_t *header, int32_t *stream, int32_t npts);
void c_float_packer_params(int32_t *header_size, int32_t *stream_size, int32_t *p1, int32_t *p2, int32_t npts);
int32_t c_float_unpacker(float *dest, int32_t *header, int32_t *stream, int32_t npts, int32_t *nbits );

#endif // RMN_PACKERS_H__
