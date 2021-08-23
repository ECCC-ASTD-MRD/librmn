//! \file rpnmacros_global.h Shared definitions.  This file should never be included directly; use rpnmacros.h

#if !defined (_RPN_MACROS_GLOBAL_)
#define _RPN_MACROS_GLOBAL_ _rpn_macros_global_

//! \deprecated This macro will be removed from future releases.  Please use the types defined stdint.h instead.
#define wordint int32_t
//! \deprecated This macro will be removed from future releases.  Please use the types defined stdint.h instead.
#define ftnword int32_t
//! \deprecated This macro will be removed from future releases.  Please use the types defined stdint.h instead.
#define ftnfloat float
//! \deprecated This macro will be removed from future releases.  Please use the types defined stdint.h instead.
#define wordfloat float

#define bytesperword 4

// Used only in c_fnom defined in c_baseio.c and passed to the qqqf7op function
#define D77MULT 4

#define F77_INTEGER   int32_t
#define F77_REAL      float
#define F77_INTEGER8  int64_t
#define F77_REAL8     double
#define F77_CHARACTER char

#define F77_LOGICAL_ARG(X)   void *X

#define CONCAT(A,B)          A##B
#define F77STRLEN(X)       CONCAT(X,_length)
#define HIDDENLEN(X)       , F2Cl F77STRLEN(X)

#define UNIX unix

#if defined(_AIX)
#define __AIX__ 
#define AIX IBM_AIX
#endif

#define FLOAT_PACK 1
#define FLOAT_UNPACK 2

#if defined (mips) || defined (__mips)
#    define C910
#endif

#if defined (__hp9000s800) || defined (__hp9000s700)
#    define HP
#endif

#if defined(__amd64__) || defined(__amd64) || defined(__x86_64) || defined(__x86_64__)
#    define Amd64 Amd64
#endif

#if defined(__alpha__) || defined(__alpha)
#    define Alpha Alpha
#endif

#if defined (_FLOAT1)
    error " wrong float mode : must be FLOAT0"
#endif
#if defined (_FLOAT2)
    error " wrong float mode : must be FLOAT0"
#endif
#if defined(_FLOAT0)
#    define NEC
#    define NEC32 nec32
#endif

#define word uint32_t

#if defined(Little_Endian)
    typedef struct {
        uint32_t mantis3:29, mantis2:3, mantis1:20, expo:11, sign:1;
    } IEEE_FLOAT_DOUBLE;
    typedef struct {
        uint32_t mantis:23, expo:8 , sign:1;
    } IEEE_FLOAT;
#else
    typedef struct {
        uint32_t sign:1, expo:11 , mantis1:20,mantis2:3,mantis3:29;
    } IEEE_FLOAT_DOUBLE;
    typedef struct {
        uint32_t sign:1, expo:8 , mantis:23;
    } IEEE_FLOAT;
#endif
typedef struct {
    uint32_t sign:1, expo:7 , mantis:24;
} IBM_FLOAT;
typedef struct {
    uint32_t sign:1, expo:7 , mantis1:24 , mantis2:32;
} IBM_FLOAT_DOUBLE;
typedef union {
    float X;
    double XD;
    uint32_t U;
    IEEE_FLOAT_DOUBLE MD;
    IEEE_FLOAT M;
    IBM_FLOAT I;
    IBM_FLOAT_DOUBLE ID;
} ALL_FLOAT;





/*****************************************************************************
 *                                                                           *
 *  Objective : extract a token from a stream of packed token                *
 *  Arguments :                                                              *
 *      OUT     packedToken           token extracted                        *
 *   IN/OUT     packedWordPtr         pointer to the token to be extracted   *
 *   IN         wordSize              size of a word                         *
 *   IN         bitSizeOfPackedToken  size of a token in bit                 *
 *   IN/OUT     packedWord            word holding the desired token         *
 *   IN/OUT     bitPackInWord         no. of bits remained packed            *
 *                                    in the packedWord                      * 
 *                                                                           *
 ****************************************************************************/
#define extract( packedToken, packedWordPtr, wordSize, bitSizeOfPackedToken,                 \
                 packedWord, bitPackInWord)                                                  \
  {                                                                                          \
        /**  obtain the integer representation  **/                                          \
        if (  bitPackInWord >= bitSizeOfPackedToken )                                        \
          {                                                                                  \
                                                                                             \
            packedToken = (packedWord >> ( wordSize - bitSizeOfPackedToken ) );              \
            packedWord <<= bitSizeOfPackedToken;                                             \
            bitPackInWord -= bitSizeOfPackedToken;                                           \
          }                                                                                  \
        else                                                                                 \
          {                                                                                  \
            packedToken = (packedWord >> ( wordSize - bitSizeOfPackedToken ));               \
            packedWordPtr++;                                                                 \
            packedWord = *packedWordPtr;                                                     \
            packedToken |= ( packedWord >> ( wordSize - (bitSizeOfPackedToken-bitPackInWord)));\
            packedWord <<= ( bitSizeOfPackedToken - bitPackInWord );                         \
            bitPackInWord = wordSize - (bitSizeOfPackedToken - bitPackInWord);               \
          }; /* if */                                                                        \
          /*    handle special condition    */                                               \
        if ( bitPackInWord == 0 )                                                            \
          {                                                                                  \
            packedWordPtr++;                                                                 \
            packedWord = *packedWordPtr;                                                     \
            bitPackInWord = wordSize;                                                        \
          };/* if */                                                                         \
  }                                                                                          \

/*****************************************************************************
 *                                                                           *
 *  Objective : discard unwanted bits                                        *
 *  Arguments :                                                              *
 *   IN/OUT     packedWordPtr         pointer to the token to be extracted   *
 *   IN         wordSize              size of a word                         *
 *   IN         discardBit            bit to be discarded                    *
 *   IN/OUT     packedWord            word holding the desired token         *
 *   IN/OUT     bitPackInWord         no. of bits remained packed            *
 *                                    in the packedWord                      * 
 *                                                                           *
 ****************************************************************************/
#define discard(packedWordPtr, wordSize, discardBit, packedWord, bitPackInWord)       \
  {                                                                                   \
            if ( bitPackInWord > discardBit )                                         \
              {                                                                       \
                packedWord <<= discardBit;                                            \
                bitPackInWord -= discardBit;                                          \
              }                                                                       \
            else                                                                      \
              {                                                                       \
                packedWordPtr++;                                                      \
                packedWord = *packedWordPtr;                                          \
                packedWord <<= ( discardBit - bitPackInWord);                         \
                bitPackInWord = wordSize - ( discardBit - bitPackInWord );            \
              };                                                                      \
            /*******  special condition ********/                                     \
            if ( bitPackInWord == 0 )                                                 \
              {                                                                       \
                packedWordPtr++;                                                      \
                packedWord = *packedWordPtr;                                          \
                bitPackInWord = wordSize;                                             \
              };                                                                      \
  }                                                                                   \

/*************************************************************************************
 *                                                                                   *
 *  Objective : pack a token into a stream of packed token                           *
 *  Arguments :                                                                      *
 *   IN         token                 token to be packed                             *
 *   IN/OUT     availableWordPtr      pointer to the word position available for     *
 *                                    packing                                        *
 *   IN         wordSize              size of a word                                 *
 *   IN         bitSizeOfPackedToken  bit size of the token                          *
 *   IN/OUT     lastWordShifted       word designated to hold the token              *
 *   IN/OUT     spaceInLastWord       no. of bits available for packing in last word *
 *                                                                                   *
 ************************************************************************************/
#define stuff(token, availableWordPtr, wordSize, bitSizeOfPackedToken, lastWordShifted,    \
              spaceInLastWord)                                                             \
  {                                                                                        \
              if ( spaceInLastWord >= bitSizeOfPackedToken )                               \
                /* integer token fits into space left */                                   \
                {                                                                          \
                  lastWordShifted = ( lastWordShifted << bitSizeOfPackedToken ) | token;   \
                  spaceInLastWord = spaceInLastWord - bitSizeOfPackedToken;                \
                }                                                                          \
              else                                                                         \
                /* integer token can not fit into space left */                            \
                {                                                                          \
                  *availableWordPtr = ((lastWordShifted << spaceInLastWord) |              \
                                             ( token >>                                    \
                                               ( bitSizeOfPackedToken - spaceInLastWord)));\
                  lastWordShifted = token &                                                \
                    ( -1 >> ( wordSize - ( bitSizeOfPackedToken - spaceInLastWord)));      \
                  spaceInLastWord = wordSize - ( bitSizeOfPackedToken - spaceInLastWord ); \
                  availableWordPtr++;                                                      \
                  lastSlot++;                                                              \
                }; /* if */                                                                \
  }
#define swap_word_endianness(mot) { register uint32_t tmp =(uint32_t)mot; \
   mot = (tmp>>24) | (tmp<<24) | ((tmp>>8)&0xFF00) | ((tmp&0xFF00)<<8); }
#define swap_buffer_endianness(buff,nwds) {\
    uint32_t *buf=(uint32_t *)buff ;\
   register int32_t nwords=nwds ;\
   while(nwords--) { swap_word_endianness(*buf);buf++; };\
   }

#endif
