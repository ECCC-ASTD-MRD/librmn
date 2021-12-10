#if !defined (_RMNLIB_C_SYMBOLS_)
#define _RMNLIB_C_SYMBOLS_ _rmnlib_c_symbols_

#include <stdint.h>
#include <rpnmacros.h>
#include <ftn2c_helper.h>

typedef void *(*PackFunctionPointer)(void *unpackedArrayOfFloat, void *packedHeader,
                                void *packedArrayOfInt, int elementCount,
                                int bitSizeOfPackedToken, int off_set, int stride,
                                int opCode, int hasMissing, void *missingTag );

void *compact_float(void *unpackedArrayOfFloat, void *packedHeader, void *packedArrayOfInt,
                       int elementCount, int bitSizeOfPackedToken, int off_set, int stride,
                       int opCode, int hasMissing, void *missingTag );

void *compact_double(void *unpackedArrayOfFloat, void *packedHeader, void *packedArrayOfInt,
                        int elementCount, int bitSizeOfPackedToken, int off_set, int stride,
                        int opCode, int hasMissing, void *missingTag );

int  compact_integer( void *unpackedArrayOfInt, void *packedHeader, void *packedArrayOfInt,
                        int elementCount, int bitSizeOfPackedToken, int off_set,
                        int stride, int opCode);

void *compact_IEEEblock_float(void *unpackedArrayOfFloat, void *packedHeader,
                              void *packedArrayOfInt,
                              int elementCount, int bitSizeOfPackedToken, int bitSizeOfPackedExpo,
                              int off_set, int stride,
                              int opCode, int hasMissing, void *missingTag );

void *compact_IEEEblock_double(void *unpackedArrayOfFloat, void *packedHeader,
                               void *packedArrayOfInt,
                               int elementCount, int bitSizeOfPackedToken, int bitSizeOfPackedExpo,
                               int off_set, int stride,
                               int opCode, int hasMissing, void *missingTag );

void f77name (iipak) (void *xunpacked, void *xpacked, int32_t *ni, int32_t *nj, int32_t *nBits,
                 int32_t *NB, int32_t *OP);

void f77name (xxpak) (void *xunpacked, void *xpacked, int32_t *ni, int32_t *nj, int32_t *nBits,
                      int32_t *NB, int32_t *OP);


int c_fretour(int iun);
int32_t f77name(fretour)(int32_t *fiun);
void f77name(d_fgfdt)();
int c_fnom(int *iun, char *nom, char *type, int lrec);
int32_t f77name(fnom)(int32_t *iun, char *nom, char *type, int32_t *flrec, F2Cl l1, F2Cl l2);
int c_fclos(int iun);
int32_t f77name(fclos)(int32_t *fiun);
int32_t f77name(qqqfnom)(int32_t *iun, char *nom, char *type, int32_t *flrec, F2Cl l1, F2Cl l2);
void c_waopen(int iun);
int c_waopen2(int iun);
int32_t f77name(waopen2)(int32_t *fiun);
void f77name(waopen)(int32_t *fiun);
void c_waclos(int iun);
int c_waclos2(int iun);
int32_t f77name(waclos2)(int32_t *fiun);
void f77name(waclos)(int32_t *fiun);
void c_wawrit(int iun, void *buf, unsigned int adr, int nmots);
int c_wawrit2(int iun, void *buf, unsigned int adr, int nmots);
void f77name(wawrit)(int32_t *fiun, void *buf, uint32_t *fadr, int32_t *fnmots);
int32_t f77name(wawrit2)(int32_t *fiun, void *buf, uint32_t *fadr, int32_t *fnmots);
void c_waread(int iun, void *buf, unsigned int adr, int nmots);
int c_waread2(int iun, void *buf, unsigned int adr, int nmots);
void f77name(waread)(int32_t *fiun, void *buf, uint32_t *fadr, int32_t *fnmots);
int32_t f77name(waread2)(int32_t *fiun, void *buf, uint32_t *fadr, int32_t *fnmots);
int32_t c_wasize(int iun);
int32_t f77name(wasize)(int32_t *fiun);
int32_t c_numblks(int iun);
int32_t f77name(numblks)(int32_t *fiun);
int32_t f77name(existe)(char *nom, F2Cl lng);
void c_openda(int iun);
void f77name(openda)(int32_t *iun);
void c_closda(int iun);
void f77name(closda)(int32_t *iun);
void c_checda(int iun);
void f77name(checda)(int32_t *iun);
void c_readda(int iun, int *bufptr, int ns, int is);
void f77name(readda)(int32_t *iun, int32_t *bufptr, int32_t *ns, int32_t *is);
void c_writda(int iun, int *bufptr, int ns, int is);
void f77name(writda)(int32_t *iun, int32_t *bufptr, int32_t *ns, int32_t *is);
int c_getfdsc(int iun);
int32_t f77name(getfdsc)( int32_t *iun);
void c_sqopen(int iun);
void f77name(sqopen)(int32_t *iun);
void c_sqclos(int iun);
void f77name(sqclos)(int32_t *iun);
void c_sqrew(int iun);
void f77name(sqrew)(int32_t *iun);
void c_sqeoi(int iun);
void f77name(sqeoi)(int32_t *iun);
int c_sqgetw(int iun, int32_t *bufptr, int nmots);
int32_t f77name(sqgetw)(int32_t *iun, int32_t *bufptr, int32_t *nmots);
int c_sqputw(int iun, int32_t *bufptr, int nmots);
int32_t f77name(sqputw)(int32_t *iun, int32_t *bufptr, int32_t *nmots);
int c_sqgets(int iun, char *bufptr, int nchar);
int32_t f77name(sqgets)(int32_t *iun, char  *bufptr, int32_t *nchar, F2Cl lbuf);
int c_sqputs(int iun, char *bufptr, int nchar);
int32_t f77name(sqputs)(int32_t *iun, char  *bufptr, int32_t *nchar, F2Cl lbuf);
void f77name(d_wafdt)();
uint32_t f77name(hrjust) (uint32_t *moth, int32_t *ncar);
uint32_t f77name(hljust) (uint32_t *moth, int32_t *ncar);
uint32_t f77name(check_host_id)();

#endif
