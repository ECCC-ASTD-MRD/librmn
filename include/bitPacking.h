#if !defined (_BITPACKING_H_)
#define _BITPACKING_H_

#include <stdint.h>

#define FLOAT_PACK 1
#define FLOAT_UNPACK 2

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
#define extract( packedToken, packedWordPtr, wordSize, bitSizeOfPackedToken, packedWord, bitPackInWord) {\
    /* Obtain the integer representation */\
    if (  bitPackInWord >= bitSizeOfPackedToken ) {\
        packedToken = (packedWord >> ( wordSize - bitSizeOfPackedToken ) );\
        packedWord <<= bitSizeOfPackedToken;\
        bitPackInWord -= bitSizeOfPackedToken;\
    } else {\
        packedToken = (packedWord >> ( wordSize - bitSizeOfPackedToken ));\
        packedWordPtr++;\
        packedWord = *packedWordPtr;\
        packedToken |= ( packedWord >> ( wordSize - (bitSizeOfPackedToken-bitPackInWord)));\
        packedWord <<= ( bitSizeOfPackedToken - bitPackInWord );\
        bitPackInWord = wordSize - (bitSizeOfPackedToken - bitPackInWord);\
    };\
    /* Special case */\
    if ( bitPackInWord == 0 ) {\
        packedWordPtr++;\
        packedWord = *packedWordPtr;\
        bitPackInWord = wordSize;\
    };\
}\


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
#define discard(packedWordPtr, wordSize, discardBit, packedWord, bitPackInWord) {\
    if ( bitPackInWord > discardBit ) {\
        packedWord <<= discardBit;\
        bitPackInWord -= discardBit;\
    } else {\
        packedWordPtr++;\
        packedWord = *packedWordPtr;\
        packedWord <<= ( discardBit - bitPackInWord);\
        bitPackInWord = wordSize - ( discardBit - bitPackInWord );\
    };\
    /* Special case */\
    if ( bitPackInWord == 0 ) {\
        packedWordPtr++;\
        packedWord = *packedWordPtr;\
        bitPackInWord = wordSize;\
    };\
}\


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
#define stuff(token, availableWordPtr, wordSize, bitSizeOfPackedToken, lastWordShifted, spaceInLastWord) {\
    if ( spaceInLastWord >= bitSizeOfPackedToken ) {\
        /* integer token fits into space left */\
        lastWordShifted = ( lastWordShifted << bitSizeOfPackedToken ) | token;\
        spaceInLastWord = spaceInLastWord - bitSizeOfPackedToken;\
    } else {\
        /* integer token can not fit into space left */\
        *availableWordPtr = ((lastWordShifted << spaceInLastWord) | ( token >> ( bitSizeOfPackedToken - spaceInLastWord)));\
        lastWordShifted = token & ( -1 >> ( wordSize - ( bitSizeOfPackedToken - spaceInLastWord)));\
        spaceInLastWord = wordSize - ( bitSizeOfPackedToken - spaceInLastWord );\
        availableWordPtr++;\
        lastSlot++;\
    };\
}\

#endif
