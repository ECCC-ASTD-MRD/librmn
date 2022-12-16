#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>

#include <App.h>
#include <rmn/rpnmacros.h>

typedef union {
 int32_t i;
 float f;
} floatint;

/* =====================================================================================================

   this set of routines will work only if the values use the 32 BIT IEEE floating point format.
   maximum number of floating values in a record = 32768*65535 ( 2 Giga values )

   packed record format :

   +-------------+-----------------+-----------------+  ...    +-----------------+-----------------------+
   | main header | header block 1  | header block 2  |         | header block n  |        D A T A        |
   +-------------+-----------------+-----------------+  ...    +-----------------+-----------------------+
   there is 1 header block for each group of 32768 values,
   the last header block may represent a smaller group

   32 bit main header format :

     12 bits   4 bits      16 bits
   +---------+-------+-----------------+
   | 0xEFF   |nbits-1|  nb of blocks   |
   +---------+-------+-----------------+

   64 bit header block format :

         32 bits          8 bits   8 bits      16 bits
   +--------------------+--------+--------+-----------------+
   | Minimum value      | MaxExp | Shift  |  nb of values   |
   +--------------------+--------+--------+-----------------+

   data : one 16 bit token for each float value

   ===================================================================================================== */
/*
    SINGLE BLOCK floating point unpacker
    dest    : pointer to output array of floating point numbers
    nbits   : pointer to number of useful bits in token
    header  : pointer to 64 bit header for this block
    stream  : pointer to packed stream (16 bits per token, 32 bit aligned at start)
    npts    : pointer to number of values to unpack  ( max 32768)

    return value is 0 if there is no error, the number of point discrepancy otherwise

*/

int32_t float_unpacker_1(float *dest, int32_t *header, int32_t *stream, int32_t npts)
{
  floatint temp,temp2;
  int32_t n;
  int32_t MaxExp, Mantis, Sgn, Minimum, Shift2, Fetch, Accu;

  Minimum = header[0];                     /* get Minimum, MaxExp, Shift2 from header */
  MaxExp = (header[1] >> 24) & 0xFF;
  Shift2 = (header[1] >> 16) & 0xFF;
  if (npts != (header[1] & 0xFFFF)) {     /* verify that the number of points is consistent with header */
    Lib_Log(APP_LIBRMN,APP_ERROR,"%s: inconsistent number of points\n",__func__);
    return npts - (header[1] & 0xFFFF);   /* return discrepancy */
    }

  n=npts;
  if (MaxExp == 0) {
  	while (n--) *dest++ = 0.0;
    return (0);
    }
  Accu = *stream++;                                    /* get first 32 bit token from stream */
  Fetch = 0;
  while(n--){
    Mantis = (Accu >> 16) & 0xFFFF;                    /* get upper 16 bits of token */
    Mantis = Mantis << Shift2;
    Mantis = Mantis + Minimum;                         /* regenerate mantissa, possibly not normalized */
    Sgn = (Mantis >> 31) & 1;
    if(Sgn) Mantis =- Mantis;                          /* need absolute value of Mantis */
    temp.i = (Mantis & (~(-1<<23))) | (MaxExp << 23);  /* eliminate bit 23 (hidden 1) and add exponent */
    temp.i = temp.i | (Sgn << 31);                     /* add sign in proper position */
    if(Mantis & (1<<23)) {
      *dest++ = temp.f;                                /* hidden 1 is genuine */
    }else{
      temp2.i= MaxExp << 23;                           /* subtract this bogus hidden 1 */
      temp2.i = temp2.i | (Sgn << 31);                 /* add sign in proper position */
      temp2.i = temp2.i & ( ~( (Mantis << 8) >> 31 ) );/* non zero only if hidden 1 is not present */
      *dest++ = temp.f - temp2.f;                      /* hidden 1 was not present, subtract it */
      }
    Accu = Accu << 16;                                 /* token must be in upper part of 32 bit word */
    if(Fetch) Accu = *stream++;                        /* new 32 bit word every other trip in loop */
    Fetch = Fetch ^ 1;                                 /* toggle Fetch */
    }
  return 0;
}

/* =====================================================================================================
    SINGLE BLOCK floating point packer
    source  : pointer to input array of floating point numbers
    nbits   : pointer to number of useful bits in token
    header  : pointer to 64 bit header for this block
    stream  : pointer to packed stream (16 bits per token, 32 bit aligned at start)
    npts    : pointer to number of values to unpack  ( max 32768)

    return value is 0 if there is no error, the number of point discrepancy otherwise

   ===================================================================================================== */

int32_t float_packer_1(float *source, int32_t nbits, int32_t *header, int32_t *stream, int32_t npts)
{
  float *z=source;
  int32_t *intsrc= (int32_t *)source;
  floatint fmin,fmax;
  int32_t n;
  int32_t MaxExp, Exp, Mask, Mantis, Shift, Minimum, Maximum, Src, Shift2, Store, Accu, Round;

  if (npts > 32768) {     /* verify that the number of points is not too large for a single block */
    Lib_Log(APP_LIBRMN,APP_ERROR,"%s: number of points is too large (max 32768)\n",__func__);
    return npts - 32768;
    }
  n=npts;
  fmin.f = *z;
  fmax.f = *z;
  while(n--){                            /* get min and max value of field */
    fmin.f = fmin.f > *z ? *z : fmin.f;
    fmax.f = fmax.f < *z ? *z : fmax.f;
    z++;
    }
  MaxExp = (fmax.i >> 23) & 0xFF;         /* extract IEEE float 32 exponent */
  Exp    = (fmin.i >> 23) & 0xFF;         /* extract IEEE float 32 exponent */
  MaxExp = MaxExp > Exp ? MaxExp : Exp;   /* MaxExp is largest of the two exponents, used for normalization */

  Src    = fmax.i;                           /* dissect Maximum value */
  Mantis = (1 << 23) | ( 0x7FFFFF & Src );   /* get IEEE mantissa, restore hidden 1 */
  Exp    = (fmax.i >> 23) & 0xFF;            /* extract IEEE float 32 exponent */
  Shift  = MaxExp - Exp;                     /* normalize mantissa to largest exponent */
  if (Shift > 31) Shift = 31;
  Mantis = Mantis >> Shift;
  if( Src >> 31 ) Mantis = - Mantis;
  Maximum= Mantis;
  if (Exp < 1) Maximum = 0;

  Src    = fmin.i;                           /* dissect Minimum value */
  Mantis = (1 << 23) | ( 0x7FFFFF & Src );
  Exp    = (fmin.i >> 23) & 0xFF;
  Shift  = MaxExp - Exp;
  if (Shift > 31) Shift = 31;
  Mantis = Mantis >> Shift;
  if( Src >> 31 ) Mantis = - Mantis;
  Minimum= Mantis;
  if (Exp < 1) Minimum = 0;

  Maximum = Maximum - Minimum;              /* largest integer left after subtracting minimum mantissa */
  Shift2 = 0;
  Round  = 1;                               /* rounding quantity */
  Mask   = ~( -1 << nbits);                /* right mask of nbits bits */
  while ( Maximum > Mask ) {               /* Maximum must fit within *nbits bits */
    Maximum = Maximum >> 1;
    Round = Round << 1;
    Shift2++;
    }
  Round = Round >> 1;                       /* this bit ends up vis a vis last bit shifted out */
  header[0] = Minimum;                      /* store minimum, maxexp, shift2, npts into header (64 bits) */
  header[1] = ((MaxExp & 0xFF) << 24) | ((Shift2 & 0xFF) << 16) | (0xFFFF & npts);

//  Lib_Log(APP_LIBRMN,APP_DEBUG,"%s: MaxExp=%d\n",__func__,MaxExp);
//  Lib_Log(APP_LIBRMN,APP_DEBUG,"%s: min=%f fmin.i=%X max=%f Minimum=%d Maximum=%d\n",__func__,fmin.f,fmin.i,fmax.f,Minimum,Maximum);
  Store = 0;
  n=npts;
  while(n--){                               /* transform input floating point into 16 bit integers */
    Src = *intsrc++;
    Mantis = (1 << 23) | ( 0x7FFFFF & Src );
    Exp    = (Src >> 23) & 0xFF;
    Shift  = MaxExp - Exp;
    if (Shift > 31) Shift = 31;
    Mantis = Mantis >> Shift;
    if( Src >> 31 ) Mantis = - Mantis;
    Mantis = Mantis - Minimum;              /* subtract minimum from mantissa */
    Mantis = Mantis + Round;                /* add rounding term */
    Mantis = Mantis >> Shift2;              /* force to fit within nbits bits */
    if (Mantis > Mask) Mantis = Mask;
    Accu   = (Accu << 16) | Mantis;         /* insert into stream as 16 bit token */
    if(Store) *stream++ = Accu;             /* store every other trip in the loop */
    Store = Store ^ 1;
    }
  if(Store) *stream++ = Accu << 16;         /* must store last ? (odd number of trips in loop) */
  return 0;
}

/* =====================================================================================================
    floating point unpacker (works by making multiple calls to the single block unpacker)
    dest    : pointer to output array of floating point numbers
    header  : pointer to 64 bit header for this block
    stream  : pointer to packed stream (16 bits per token, 32 bit aligned at start)
    npts    : pointer to number of values to unpack
    nbits   : pointer to number of useful bits in token (output)

    pointers are used where values could have been to make this routine FORTRAN callable

    subroutine float_unpacker(VALUES,HEADER,STREAM,NPTS,NBITS)
    integer *4 NPTS, HEADER(2), STREAM(NPTS/2, NBITS)
    real *4 VALUES(NPTS)
    return value is zero if OK, error code from float_unpacker_1 otherwise
   ===================================================================================================== */

int32_t c_float_unpacker(float *dest, int32_t *header, int32_t *stream, int32_t npts, int32_t *nbits)
{
  int32_t npoints,npointsleft,ierror;

  *nbits = ( (header[0]>>16) & 0xF) + 1 ;
  if(0xEFF != ( (header[0]>>20) & 0xFFF)) {
    Lib_Log(APP_LIBRMN,APP_ERROR,"%s: invalid header\n",__func__);
    return -1;
    }
  npoints = (npts + 32767) / 32768;      /* number of blocks */
  if(npoints != (header[0] & 0xFFFF)) {
    Lib_Log(APP_LIBRMN,APP_ERROR,"%s: inconsistent number of points (header/request mismatch)\n",__func__);
    return -1;
    }
  header++;
  npointsleft=npts;
  while(npointsleft > 0){                  /* pack in groups of <= 32768 points */
    npoints = npointsleft < 32768 ? npointsleft : 32768;
    npointsleft = npointsleft - npoints;
    ierror = float_unpacker_1(dest, header, stream, npoints);
    if(ierror) return ierror;
    dest = dest + 32768;
    header = header + 2;
    stream = stream + 16384;
    }
  return 0;
}

int32_t f77name(float_unpacker)(float *dest, int32_t *header, int32_t *stream, int32_t *npts, int32_t *nbits)
{
  return c_float_unpacker(dest, header, stream, *npts, nbits);
}

/* =====================================================================================================
    floating point packer (works by making multiple calls to the single block packer)
    source  : pointer to input array of floating point numbers
    nbits   : pointer to number of useful bits in token
    header  : pointer to 64 bit header for this block
    stream  : pointer to packed stream (16 bits per token, 32 bit aligned at start)
    npts    : pointer to number of values to unpack

    pointers are used where values could have been to make this routine FORTRAN callable

    integer function float_packer(VALUES,NBITS,HEADER,STREAM,NPTS)
    integer *4 NPTS, HEADER(2), NBITS, STREAM(NPTS/2)
    real *4 VALUES(NPTS)
    return value is 0 if there was no error, -1 if error occurred
   ===================================================================================================== */

int32_t c_float_packer(float *source, int32_t nbits, int32_t *header, int32_t *stream, int32_t npts)
{
  int32_t npoints,npointsleft;

  if(nbits > 16 || nbits < 1) {
    Lib_Log(APP_LIBRMN,APP_ERROR,"%s: nbits must be > 0 and <= 16 ,nbits = %d\n",__func__,nbits);
    return -1;
    }
  header[0] = ((npts + 32767) / 32768) & 0xFFFF;      /* number of blocks */
  header[0] = header[0] | ( 0xEFF << 20 );
  header[0] = header[0] | ( ( nbits - 1 ) << 16 );
  header++;
  npointsleft=npts;
  while(npointsleft > 0){                  /* pack in groups of <= 32768 points */
    npoints = npointsleft < 32768 ? npointsleft : 32768;
    npointsleft = npointsleft - npoints;
    if( float_packer_1(source, nbits, header, stream, npoints) ) return -1;  /* return -1 on error */
    source = source + 32768;
    header = header + 2;
    stream = stream + 16384;
    }
  return  0 ;   /* return 0 if no error */
}
int32_t f77name(float_packer)(float *source, int32_t *nbits, int32_t *header, int32_t *stream, int32_t *npts)
{
  return c_float_packer(source,*nbits,header,stream,*npts);
}

/* =====================================================================================================
   get lengths of various elements of packed data
   header_size  : pointer to size of header part
   stream_size  : pointer to size of stream part
   npts         : pointer to number of values
   p1,p2        : reserved for future expansion, a value of zero is returned now

   subroutine float_packer_params(HEADER_SIZE,STREAM_SIZE,P1,P2,NPTS)
   integer *4 NPTS,HEADER_SIZE,STREAM_SIZE,P1,P2

  ===================================================================================================== */
void c_float_packer_params(int32_t *header_size, int32_t *stream_size, int32_t *p1, int32_t *p2, int32_t npts)
{
  *header_size = 1+2*((npts+32767)/32768) ; /* size used for header 2 int32_ts per 32768 values block plus 1 */
  *header_size = *header_size * sizeof(int32_t);
  *stream_size = (npts + 1) / 2 ;           /* size used for stream, 1 int32_t per 2 values */
  *stream_size = *stream_size * sizeof(int32_t);
  *p1 = 0;
  *p2 = 0;
}
void f77name(float_packer_params)(int32_t *header_size, int32_t *stream_size, int32_t *p1, int32_t *p2, int32_t *npts)
{
  c_float_packer_params(header_size,stream_size,p1,p2,*npts);
}
#ifdef TEST
/* test program to verify that results are identical on all machines */
#define NPTS (1+1000*1000)
main()
{
  float source[NPTS];
  float source2[NPTS];
  double error,errormax,errorabs,erroravg;
  int32_t nbits=14;
  int32_t NBITS;
  int32_t npts=NPTS;
  int32_t header[1+2*((NPTS+32767)/32768)], stream[(NPTS+1)/2];
  int32_t signature;
  int i,j,nhead;
  int32_t p1,p2,header_size,stream_size;

  f77name(float_packer_params)(&header_size, &stream_size, &p1, &p2, &npts);
  printf("header_size,stream_size=%d,%d\n",header_size,stream_size);

  for ( i=0 ; i<NPTS ; i++ ) { source[i]=i*1.234-1123.123; };
  printf("source[0],source[1],source[2],source[NPTS-1]=%f,%f,%f,%f\n",source[0],source[1],source[2],source[NPTS-1]);
  f77name(float_packer)(source, &nbits, header, stream, &npts);

  f77name(float_unpacker)(source2, header, stream, &npts, &NBITS);
  printf("source2[0],source2[1],source2[2],source2[NPTS-1]=%f,%f,%f,%f\n",source2[0],source2[1],source2[2],source2[NPTS-1]);
  printf("nbits = %d ,nbits from unpacker = %d\n",nbits,NBITS);
  signature=0;
  for ( i=0 ; i< (1+2*((NPTS+32767)/32768))  ; i++ ) {
    signature=signature^header[i];
    /* printf(" %x",header[i]);  */
    }
  printf("\nafter packing signature=%x\n",signature);
  signature=0;
  for ( i=0 ; i<  ((NPTS+1)/2) ; i++ ) signature=signature^stream[i];
  printf("after packing signature=%x\n",signature);

  errormax=0;
  errorabs=0;
  erroravg=0;
  for ( i=0 ; i<NPTS ; i++ ) {
    error = source2[i]-source[i];
    erroravg=erroravg+error;
    if(error<0) error=-error ;
    errorabs=errorabs+error ;
    errormax=error>errormax?error:errormax;
    }
  printf("after packing errormax=%f,erroravg=%f, errorabs avg=%f\n",errormax,erroravg/NPTS,errorabs/NPTS);

  for ( i=0 ; i<NPTS ; i++ ) { source[i]=source2[i] ; }

  for ( j=0 ; j< 99 ; j++ ) {     /* perform repacking-unpacking cycles to verify stability */
    f77name(float_packer)(source2, &nbits, header, stream, &npts);
/*
    for ( i=0 ; i<NPTS ; i++ ) source2[i]=0;
*/
    f77name(float_unpacker)(source2, header, stream, &npts, &NBITS);
    }

  errormax=0;
  erroravg=0;
  for ( i=0 ; i<NPTS ; i++ ) {
    error = source2[i]-source[i];
    erroravg=erroravg+error;
    if(error<0) error=-error ;
    errormax=error>errormax?error:errormax;
    }
  printf("after REpacking errormax=%f,erroravg=%f\n",errormax,erroravg/NPTS);  /* better be zero */
}
#endif
