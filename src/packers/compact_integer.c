/* RMNLIB - Library of useful routines for C and FORTRAN programming
 * Copyright (C) 1975-2001  Division de Recherche en Prevision Numerique
 *                          Environnement Canada
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation,
 * version 2.1 of the License.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the
 * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.
 */

#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>

#include <bitPacking.h>

#include <App.h>


//! \file


typedef struct {
#if defined(Little_Endian)
    uint32_t numOfBitsPerToken : 6, SHIFT : 6, unused : 12, ID : 8;
#else
    uint32_t ID : 8, unused : 12, SHIFT : 6, numOfBitsPerToken : 6;
#endif
    uint32_t numOfPackedToken : 32;
    uint32_t minValue         : 32;
    uint32_t maxValue         : 32;
} integer_header;


//! Find min and max of an integer array
//!\param [in] arrayOfUnpacked Array
//!\param [out] min Minimum
//!\param [out] max Maximum
//!\param [in] intCount Number of elements in array
//!\param [in] stride Stride between array elements
#define FindMinMax(arrayOfUnpacked, min, max, intCount, stride) {\
    min = arrayOfUnpacked[0];\
    max = arrayOfUnpacked[0];\
    for(int i = stride; i < intCount * stride ; i += stride) {\
        if ( arrayOfUnpacked[i] < min ) {\
            min = arrayOfUnpacked[i];\
        } else if ( arrayOfUnpacked[i] > max ) {\
            max = arrayOfUnpacked[i];\
        }\
    }\
}\


// ! Obtain and stuff a chain of 32 bit word into an array
//!\param [in] arrayOfUnpacked Array
//!\param [out] min Minimum
//!\param [in] intCount Number of elements in array
//!\param [in] stride Stride between array elements
#define pack32Bit(arrayOfUnpacked, min, intCount, stride, packHeader, shiftRequired, positiveMask, arrayPtr)\
{\
            int i = intCount;\
            if ( packHeader != NULL ) {\
                while ( i-- ) {\
                    *arrayPtr = ((*arrayOfUnpacked - min ) >> shiftRequired );\
                    arrayPtr++;\
                    arrayOfUnpacked += stride;\
                }\
            } else {\
                while ( i-- ) {\
                    *arrayPtr = (uint32_t)*arrayOfUnpacked + (uint32_t)positiveMask;\
                    arrayPtr++;\
                    arrayOfUnpacked += stride;\
                }\
            }\
}\


//! Obtain and stuff a chain of word(<32 bit) into an array
//! \param [in] arrayOfUnpacked Unpacked integer array
//! \param [in] min Minimum value
#define packBit(arrayOfUnpacked, min, intCount, stride, wordSize, bitSizeOfPackedToken, packHeader, shiftRequired, positiveMask, arrayPtr, lastWordShifted, spaceInLastWord, cleanupMask)\
        {\
            for ( int i = 0; i < intCount * stride; i += stride ) {\
                uint32_t tempInt = packHeader != NULL ? (arrayOfUnpacked[i] - min) >> shiftRequired : arrayOfUnpacked[i] + positiveMask;\
                tempInt &= cleanupMask;\
                stuff(tempInt, arrayPtr, wordSize, bitSizeOfPackedToken, lastWordShifted, spaceInLastWord);\
            }\
        }\


//! Pack a chain of word into an array
//! \param [in] arrayOfUnpacked Unpacked integer array
//! \param [in] min Minimum value
//! \param [in] wordSize Word size
//! \param [in] bitSizeOfPackedToken Packed token size in bits
#define Pack(arrayOfUnpacked, arrayOfPacked, min, intCount, offset, stride, wordSize, bitSizeOfPackedToken, packHeader, shiftRequired, positiveMask, cleanupMask)\
{\
    int lastPackBit = offset;\
    int spaceInLastWord = wordSize - ( lastPackBit % wordSize );\
    int lastSlot = ( lastPackBit / wordSize );\
\
    uint32_t lastWordShifted = spaceInLastWord == wordSize ? 0 : arrayOfPacked[lastSlot] >> spaceInLastWord; \
    uint32_t * arrayPtr = &arrayOfPacked[lastSlot];\
    if (( spaceInLastWord == wordSize ) && ( bitSizeOfPackedToken == wordSize )) {\
        pack32Bit(arrayOfUnpacked, min, intCount, stride, packHeader, shiftRequired, positiveMask, arrayPtr);\
    } else {\
        packBit(arrayOfUnpacked, min, intCount, stride, wordSize, bitSizeOfPackedToken, packHeader, shiftRequired, positiveMask, arrayPtr, lastWordShifted, spaceInLastWord, cleanupMask);\
    }\
\
    /* squeezes hole left in the integer array */\
    if ( spaceInLastWord < wordSize ) {\
        *arrayPtr = ( lastWordShifted << spaceInLastWord) | ( *arrayPtr & ~(-1 << spaceInLastWord));\
    }\
    return bitSizeOfPackedToken;\
}\


//! Construct pack header
//! \param [in] arrayOfUnpacked Array
//! \param [out] min Minimum
//! \param [out] max Maximum
//! \param [in] intCount Number of elements in array
//! \param [in] stride Stride between array elements
//! \param [out] packHeader Pack header
#define constructHeader(arrayOfUnpacked, min, max, intCount, stride, packHeader, shiftRequired, bitSizeOfPackedToken)\
{\
    uint32_t tempUnsignedMax, tempUnsignedMin;\
    uint32_t maxRange;\
    int bitRequiredForRange;\
    FindMinMax(arrayOfUnpacked, min, max, intCount, stride);\
    if ( (max > 0) && (min < 0) ) {\
        /* prevent signed overflow */\
        tempUnsignedMax = max;\
        tempUnsignedMin = -min;\
        maxRange = tempUnsignedMax + tempUnsignedMin;\
    } else {\
        maxRange = max - min;\
    }\
    /* compute shift required */\
    bitRequiredForRange = 0;\
    while ( maxRange != 0 ) {\
        maxRange = maxRange >> 1;\
        bitRequiredForRange++;\
    }\
\
    shiftRequired = 0;\
    if ( bitSizeOfPackedToken == -1 ) {\
        bitSizeOfPackedToken = bitRequiredForRange;\
    } else {\
        while ( (bitRequiredForRange - bitSizeOfPackedToken) > 0 ) {\
            shiftRequired++;\
            bitRequiredForRange--;\
        }\
    }\
\
    /* construct pack header */\
    packHeader[0] = 0xFD000000 | (shiftRequired << 6) | bitSizeOfPackedToken;\
    packHeader[1] = intCount;\
    packHeader[2] = min;\
    packHeader[3] = max;\
}\


//! Unpack an array
//! \param [out] arrayOfUnpacked Unpacked array
//! \param [in] requiredShift Shift required
//! \param [in] tokenSize Packed token size
//! \param [in] min Minimum value in array
//! \param [in] intCount Number of elements in the packed array
//! \param [in] offset Unpack integer spacing
//! \param [in] wordSize Word size
//! \param [in,out] positiveMask Mask for sign bit
//! \param [in] packHeader Pack header
//! \param [in] bitSizeOfPackedToken Size of packed token in bits
#define Unpack(arrayOfUnpacked, arrayOfPacked, requiredShift, tokenSize, min, intCount, offset, stride, wordSize, positiveMask, packHeader, bitSizeOfPackedToken)\
{\
    int firstPackBit = offset;\
    int bitPackInFirstWord =  wordSize - ( firstPackBit % wordSize );\
    int currentSlot = ( firstPackBit / wordSize );\
    int significantBit = tokenSize > wordSize ? wordSize : tokenSize;\
    int inSignificantBit = tokenSize > wordSize ? tokenSize - wordSize : 0;\
    uint32_t currentWord = arrayOfPacked[currentSlot] << ( wordSize - bitPackInFirstWord );\
\
    positiveMask = (positiveMask == 0x80000000) ? positiveMask : -positiveMask;\
\
    /* unpack integer numbers */\
    uint32_t * arrayPtr = &arrayOfPacked[currentSlot];\
    uint32_t packInt;\
    for ( int i = 0; i < intCount * stride; i += stride) {\
        extract(packInt, arrayPtr, wordSize, significantBit, currentWord, bitPackInFirstWord);\
        /* truncate extra bit */\
        if ( inSignificantBit > 0 ) {\
            discard(arrayPtr, wordSize, inSignificantBit, currentWord, bitPackInFirstWord);\
        }\
        arrayOfUnpacked[i] = packHeader != NULL ? (packInt << requiredShift) + min : packInt + positiveMask;\
    }\
    return bitSizeOfPackedToken;\
}\


//! Pack integers
int compact_p_integer(
    //! [in,out] Unpacked integers array
    const void * const unpackedArrayOfInt,
    //! [in,out] Compaction header
    void * const packedHeader,
    //! [in,out] Packed integer array
    void * const packedArrayOfInt,
    //! [in] Number of unpacked integers
    int intCount,
    //! [in] Packed integer size in bits. -1 for auto detection
    int bitSizeOfPackedToken,
    //! [in] packing : the last bit of integer packed inside array, unpacking : the first bit of integer packed inside array
    int offset,
    //! [in] Unpack integer spacing
    int stride,
    //! [in] 0 for unsigned, 1 for signed
    // Used to be Operation mode: 1 pack unsigned, 2 unpack unsigned, 3 pack signed, 4 unpack signed
    const int sign
) {
    //! \return Number of bits needed for each packed integer

    if ( bitSizeOfPackedToken != -1 && (bitSizeOfPackedToken < 1 || bitSizeOfPackedToken > 32) ) {
        Lib_Log(APP_LIBRMN, APP_ERROR,
                "%s: Can only (un)compact integers with size <= 32 bits (bitSizeOfPackedToken = %d)\n",
                __func__, bitSizeOfPackedToken);
        return 0;
    }

    const int wordSize = 8 * sizeof(uint32_t);
    uint32_t * const packHeader = (uint32_t *)packedHeader;
    uint32_t * arrayOfUnsignedUnpacked = (uint32_t *)unpackedArrayOfInt;
    int32_t * arrayOfSignedUnpacked = (int32_t *)unpackedArrayOfInt;
    uint32_t * arrayOfPacked = (uint32_t  *)packedArrayOfInt;

    uint32_t minUnsignedInteger = 0, maxUnsignedInteger = 0;
    int minSignedInteger = 0, maxSignedInteger = 0;
    int shiftRequired = 0;
    if ( packHeader != NULL ) {
        if ( sign ) {
            constructHeader(arrayOfSignedUnpacked, minSignedInteger, maxSignedInteger, intCount, stride, packHeader, shiftRequired, bitSizeOfPackedToken);
        } else {
            constructHeader(arrayOfUnsignedUnpacked, minUnsignedInteger, maxUnsignedInteger, intCount, stride, packHeader, shiftRequired, bitSizeOfPackedToken);
        }
    } else {
        // pack header not required, X itself is used as packInt, determines bitSizeOfPackedToken, if not available
        if ( bitSizeOfPackedToken == -1 ) {
            // obtain minimum, maximun, span
            uint32_t maxSpan = 0;
            if ( sign ) {
                // signed integer number
                FindMinMax(arrayOfSignedUnpacked, minSignedInteger, maxSignedInteger, intCount, stride);
                maxSpan = ( abs(minSignedInteger) > maxSignedInteger ) ? abs(minSignedInteger) : maxSignedInteger;
            } else {
                // unsigned integer number
                maxSpan = arrayOfUnsignedUnpacked[0];
                for(int i = stride; i < intCount * stride ; i += stride) {
                    maxSpan |= arrayOfUnsignedUnpacked[i];
                }
            }

            bitSizeOfPackedToken = 0;
            while ( maxSpan != 0 ) {
                maxSpan = maxSpan >> 1;
                bitSizeOfPackedToken++;
            }
            if ( sign ) {
                // accomodate the signed bit
                bitSizeOfPackedToken++;
            }
        }
    }
    uint32_t cleanupMask = ((uint32_t)(~0) >> (wordSize - bitSizeOfPackedToken));
    int32_t positiveMask = sign ? ( 1 << ( bitSizeOfPackedToken - 1 )) : 0;
    if ( sign ) {
        Pack(arrayOfSignedUnpacked, arrayOfPacked, minSignedInteger, intCount, offset, stride, wordSize, bitSizeOfPackedToken, packHeader, shiftRequired, positiveMask, cleanupMask);
    } else {
        Pack(arrayOfUnsignedUnpacked, arrayOfPacked, minUnsignedInteger, intCount, offset, stride, wordSize, bitSizeOfPackedToken, packHeader, shiftRequired, positiveMask, cleanupMask);
    }
}


//! Unpack integers
int compact_u_integer(
    //! [in,out] Unpacked integers array
    void * const unpackedArrayOfInt,
    //! [in,out] Compaction header
    const void * const packedHeader,
    //! [in,out] Packed integer array
    const void * const packedArrayOfInt,
    //! [in] Number of unpacked integers
    int intCount,
    //! [in] Packed integer size in bits. -1 for auto detection
    int bitSizeOfPackedToken,
    //! [in] packing : the last bit of integer packed inside array, unpacking : the first bit of integer packed inside array
    int offset,
    //! [in] Unpack integer spacing
    int stride,
    //! [in] 0 for unsigned, 1 for signed
    // Used to be Operation mode: 1 pack unsigned, 2 unpack unsigned, 3 pack signed, 4 unpack signed
    const int sign
) {
    //! \return Number of bits needed for each packed integer

    if ( bitSizeOfPackedToken != -1 && (bitSizeOfPackedToken < 1 || bitSizeOfPackedToken > 32) ) {
        Lib_Log(APP_LIBRMN, APP_ERROR,
                "%s: Can only (un)compact integers with size <= 32 bits (bitSizeOfPackedToken = %d)\n",
                __func__, bitSizeOfPackedToken);
        return 0;
    }

    const int wordSize = 8 * sizeof(uint32_t);
    const uint32_t * const packHeader = (uint32_t *)packedHeader;
    uint32_t * arrayOfUnsignedUnpacked = (uint32_t *)unpackedArrayOfInt;
    int32_t * arrayOfSignedUnpacked = (int32_t *)unpackedArrayOfInt;
    uint32_t cleanupMask = ((uint32_t)(~0) >> (wordSize - bitSizeOfPackedToken));
    uint32_t * arrayOfPacked = (uint32_t  *)packedArrayOfInt;

    int tokenSize, ShiftIntended;
    int minSigned;
    uint32_t minUnsigned;
    if ( packedHeader != NULL ) {
        const integer_header * const theHeader = (integer_header *)packedHeader;
        tokenSize     = theHeader->numOfBitsPerToken;
        ShiftIntended = theHeader->SHIFT;
        intCount      = theHeader->numOfPackedToken;
        minSigned     = theHeader->minValue;
        minUnsigned   = theHeader->minValue;
    } else {
        if ( (bitSizeOfPackedToken < 1 || bitSizeOfPackedToken > 32) ) {
            Lib_Log(APP_LIBRMN, APP_ERROR,
                    "%s: Can not uncompact integers without header without 0 < bitSizeOfPackedToken size <= 32 (bitSizeOfPackedToken = %d)!\n",
                    __func__, bitSizeOfPackedToken);
            return 0;
        }

        tokenSize     = bitSizeOfPackedToken;
        ShiftIntended = 0;
        minSigned     = 0;
        minUnsigned   = 0;
    }
    int32_t positiveMask = sign ? ( 1 << ( bitSizeOfPackedToken - 1 )) : 0;
    if ( sign ) {
        Unpack(arrayOfSignedUnpacked, arrayOfPacked, ShiftIntended, tokenSize, minSigned, intCount, offset, stride, wordSize, positiveMask, packHeader, bitSizeOfPackedToken);
    } else {
        Unpack(arrayOfUnsignedUnpacked, arrayOfPacked, ShiftIntended, tokenSize, minUnsigned, intCount, offset, stride, wordSize, positiveMask, packHeader, bitSizeOfPackedToken);
    }
}

//! Pack unsigned short integers
int compact_p_short(
    //! [in,out] Unpacked integers array
    const void * const unpackedArray,
    //! [in,out] Compaction header
    void * const packedHeader,
    //! [in,out] Packed integer array
    void * const packedArray,
    //! [in] Number of unpacked integers
    int intCount,
    //! [in] Packed integer size in bits. -1 for auto detection
    int bitSizeOfPackedToken,
    //! [in] packing : the last bit of integer packed inside array, unpacking : the first bit of integer packed inside array
    const int offset,
    //! [in] Unpack integer spacing
    const int stride
) {
    if ( bitSizeOfPackedToken == 0 || bitSizeOfPackedToken > 16 || bitSizeOfPackedToken < -1 ) {
        Lib_Log(APP_LIBRMN, APP_ERROR, "%s: bitSizeOfPackedToken (%d given) must be between 1 and 16 or -1 for auto detection!\n", __func__, bitSizeOfPackedToken);
        return 0;
    }

    int wordSize = 8 * sizeof(uint32_t);
    unsigned short * arrayOfUnsignedShort = (unsigned short *)unpackedArray;
    uint32_t * const packHeader = (uint32_t *)packedHeader;
    uint32_t * arrayOfPacked = (uint32_t  *)packedArray;

    int shiftRequired = 0;
    if ( packedHeader != NULL ) {
        uint32_t minUnsignedInteger = 0, maxUnsignedInteger = 0;
        constructHeader(arrayOfUnsignedShort, minUnsignedInteger, maxUnsignedInteger, intCount, stride, packHeader, shiftRequired, bitSizeOfPackedToken);
    } else {
        if ( bitSizeOfPackedToken == -1 ) {
            uint32_t maxSpan = 0;
            // unsigned integer number
            maxSpan = arrayOfUnsignedShort[0];
            for ( int i = stride; i < intCount * stride ; i += stride) {
                maxSpan |= arrayOfUnsignedShort[i];
            }

            bitSizeOfPackedToken = 0;
            while ( maxSpan != 0 ) {
                maxSpan = maxSpan >> 1;
                bitSizeOfPackedToken++;
            }
        }
    }
    int positiveMask = 0;
    uint32_t cleanupMask = ((uint32_t)(~0) >> (wordSize - bitSizeOfPackedToken));
    Pack(arrayOfUnsignedShort, arrayOfPacked, 0, intCount, offset, stride, wordSize, bitSizeOfPackedToken, packHeader, shiftRequired, positiveMask, cleanupMask);
}


//! Unpack unsigned short integers
int compact_u_short(
    //! [in,out] Unpacked integers array
    void * const unpackedArray,
    //! [in,out] Compaction header
    void * const packedHeader,
    //! [in,out] Packed integer array
    const void * const packedArray,
    //! [in] Number of unpacked integers
    int intCount,
    //! [in] Packed integer size in bits. -1 for auto detection
    const int bitSizeOfPackedToken,
    //! [in] packing : the last bit of integer packed inside array, unpacking : the first bit of integer packed inside array
    const int offset,
    //! [in] Unpack integer spacing
    const int stride
) {
    int wordSize = 8 * sizeof(uint32_t);
    unsigned short * arrayOfUnsignedShort = (unsigned short *)unpackedArray;
    uint32_t * const packHeader = (uint32_t *)packedHeader;
    uint32_t cleanupMask = ((uint32_t)(~0) >> (wordSize - bitSizeOfPackedToken));
    uint32_t * arrayOfPacked = (uint32_t  *)packedArray;

    int tokenSize, ShiftIntended;
    if ( packHeader != NULL ) {
        integer_header * theHeader = (integer_header *)packedHeader;
        tokenSize     = theHeader->numOfBitsPerToken;
        ShiftIntended = theHeader->SHIFT;
        intCount      = theHeader->numOfPackedToken;
    } else {
        tokenSize     = bitSizeOfPackedToken;
        ShiftIntended = 0;
    }

    if ( tokenSize == 0 || tokenSize > 16 || tokenSize < -1 ) {
        Lib_Log(APP_LIBRMN, APP_ERROR, "%s: tokenSize (%d given) must be between 1 and 16 or -1 for auto detection!\n", __func__, tokenSize);
        return 0;
    }

    int positiveMask = 0;
    Unpack(arrayOfUnsignedShort, arrayOfPacked, ShiftIntended, tokenSize, 0, intCount, offset, stride, wordSize, positiveMask, packHeader, bitSizeOfPackedToken);
}


int compact_char(
    //! [in,out] Unpacked bytes array
    void * const unpackedArrayOfBytes,
    //! [in,out] Compaction header
    void * const packedHeader,
    //! [in,out] Packed integer array
    void * const packedArrayOfInt,
    //! [in] Number of elements in array
    int intCount,
    //! [in] Packed integer size in bits. -1 for auto detection
    int bitSizeOfPackedToken,
    //! [in] For packing, the last bit of integer packed inside the array. For unpacking, the first bit of integer packed inside the array
    const int offset,
    //! [in] Unpack integer spacing
    const int stride,
    //! [in] 9: unsigned char pack, 10: unsigned char unpack
    const int opCode
) {
    if ( bitSizeOfPackedToken == 0 || bitSizeOfPackedToken > 8 || bitSizeOfPackedToken < -1 ) {
        Lib_Log(APP_LIBRMN, APP_ERROR, "%s: bitSizeOfPackedToken (%d given) must be between 1 and 8 or -1 for auto detection!\n", __func__, bitSizeOfPackedToken);
        return 0;
    }

    int wordSize = 8 * sizeof(uint32_t);
    unsigned char * arrayOfUnsignedChar = (unsigned char *)unpackedArrayOfBytes;
    uint32_t * const packHeader = (uint32_t *)packedHeader;
    uint32_t cleanupMask = ((uint32_t)(~0) >> (wordSize - bitSizeOfPackedToken));
    uint32_t * arrayOfPacked = (uint32_t  *)packedArrayOfInt;

    if (opCode == 9) {
        int shiftRequired = 0;
        if ( packedHeader != NULL ) {
            uint32_t minUnsignedInteger = 0, maxUnsignedInteger = 0;
            constructHeader(arrayOfUnsignedChar, minUnsignedInteger, maxUnsignedInteger, intCount, stride, packHeader, shiftRequired, bitSizeOfPackedToken);
        } else {
            // pack header not required, X itself is used as packInt, determines bitSizeOfPackedToken, if not available
            if ( bitSizeOfPackedToken == -1 ) {
                // obtain minimum, maximun, span
                uint32_t maxSpan = 0;
                if ( opCode == 9 ) {
                    // unsigned integer number
                    maxSpan = arrayOfUnsignedChar[0];

                    for (int i = stride; i < intCount * stride; i += stride) {

                        maxSpan |= arrayOfUnsignedChar[i];
                    }
                }

                bitSizeOfPackedToken = 0;
                while ( maxSpan != 0 ) {
                    maxSpan = maxSpan >> 1;
                    bitSizeOfPackedToken++;
                }
                if ( opCode == 3 ) {
                    // accomodate the signed bit
                    bitSizeOfPackedToken++;
                }
                cleanupMask = ((uint32_t)(~0) >> (wordSize - bitSizeOfPackedToken));
            }
        }
        uint32_t positiveMask = ( opCode < 11 ) ? 0 : ( 1 << ( bitSizeOfPackedToken - 1 ));
        Pack(arrayOfUnsignedChar, arrayOfPacked, 0, intCount, offset, stride, wordSize, bitSizeOfPackedToken, packHeader, shiftRequired, positiveMask, cleanupMask);
    } else if ( opCode == 10 ) {
        int tokenSize, ShiftIntended;
        if ( packHeader != NULL ) {
            integer_header * const theHeader = (integer_header *)packedHeader;
            tokenSize     = theHeader->numOfBitsPerToken;
            ShiftIntended = theHeader->SHIFT;
            intCount      = theHeader->numOfPackedToken;
        } else {
            tokenSize     = bitSizeOfPackedToken;
            ShiftIntended = 0;
        }
        uint32_t positiveMask = ( opCode < 11 ) ? 0 : ( 1 << ( bitSizeOfPackedToken - 1 ));
        Unpack(arrayOfUnsignedChar, arrayOfPacked, ShiftIntended, tokenSize, 0, intCount, offset, stride, wordSize, positiveMask, packHeader, bitSizeOfPackedToken);
    } else {
        Lib_Log(APP_LIBRMN, APP_ERROR, "%s: opCode (%d) is not defined\n", __func__, opCode);
        return 0;
    }
}
