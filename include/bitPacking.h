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


//! \brief Extract a token from a stream of packed tokens
//! \param [out] packedToken Extracted token
//! \param [in,out] packedWordPtr Pointer to the token to be extracted
//! \param [in] wordSize Size of words
//! \param [in] bitSizeOfPackedToken Token size in bits
//! \param [in,out] packedWord Word holding the desired token
//! \param [in,out] bitPackInWord Number of remaining bits in packedWord
#define extract(packedToken, packedWordPtr, wordSize, bitSizeOfPackedToken, packedWord, bitPackInWord) {\
    /* Obtain the integer representation */\
    if ( bitPackInWord >= bitSizeOfPackedToken ) {\
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
    }\
    /* Special case */\
    if ( bitPackInWord == 0 ) {\
        packedWordPtr++;\
        packedWord = *packedWordPtr;\
        bitPackInWord = wordSize;\
    }\
}\


//! \brief Discard unwanted bits
//! \param [in,out] packedWordPtr Pointer to the token to be extracted
//! \param [in] wordSize Size of words
//! \param [in] discardBit Bit to be discarded
//! \param [in,out] packedWord Word holding the desired token
//! \param [in,out] bitPackInWord Number of remaining bits in packedWord
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


//! \brief Pack a token into a stream of packed token
//! \param [in] token Token to be packed
//! \param [in,out] availableWordPtr Pointer to the word position available for packing
//! \param [in] wordSize The word size in bits
//! \param [in] bitSizeOfPackedToken Token size in bits
//! \param [in,out] lastWordShifted Word designated to hold the token
//! \param [in,out] spaceInLastWord Number of remaining bits available for packing in word
#define stuff(token, availableWordPtr, wordSize, bitSizeOfPackedToken, lastWordShifted, spaceInLastWord) {\
    if ( spaceInLastWord >= bitSizeOfPackedToken ) {\
        /* integer token fits into space left */\
        lastWordShifted = ( lastWordShifted << bitSizeOfPackedToken ) | token;\
        spaceInLastWord = spaceInLastWord - bitSizeOfPackedToken;\
    } else {\
        /* integer token can not fit into space left */\
        *availableWordPtr = ( lastWordShifted << spaceInLastWord ) | ( token >> ( bitSizeOfPackedToken - spaceInLastWord ) );\
        lastWordShifted = token & ( -1 >> ( wordSize - ( bitSizeOfPackedToken - spaceInLastWord ) ) );\
        spaceInLastWord = wordSize - ( bitSizeOfPackedToken - spaceInLastWord );\
        availableWordPtr++;\
    };\
}\

#endif
