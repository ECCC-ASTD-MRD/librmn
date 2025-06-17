#ifndef FST_MISSING_H
#define FST_MISSING_H

#ifdef __cplusplus
extern "C" {
#endif

int missing_value_used(void);

int ForceMissingValueUsage(
    const int enable
);

void SetMissingValueMapping(
    const int mode,
    const int datatype,
    const int datasize,
    const void * const processor_
 );

void set_missing_value_flags(
    const float * const missingFloatVal,
    const int * const missingIntVal,
    const unsigned int * const missingUIntVal,
    const double * const missingDoubleVal,
    const short * const missingShortVal,
    const unsigned short * const missingUShortVal,
    const signed char * const missingByteVal,
    const unsigned char * const missingUByteVal
);

int get_missing_value_flags(
    float * const missingFloatVal,
    int * const missingIntVal,
    unsigned int * const missingUIntVal,
    double * const missingDoubleVal,
    short * const missingShortVal,
    unsigned short * const missingUShortVal,
    signed char * const missingByteVal,
    unsigned char * const missingUByteVal
);

int EncodeMissingValue(
    void * const dst,
    const void * const src,
    const int nElems,
    const int datatype,
    const int datasize,
    const int npak
);

void DecodeMissingValue(
    void * const field,
    const int nElems,
    const int datatype,
    const int datasize
);

#ifdef __cplusplus
}
#endif

#endif
