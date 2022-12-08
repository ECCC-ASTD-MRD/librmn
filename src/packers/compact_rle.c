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
#include <bitPacking.h>

#include <App.h>

#define powerSpan  33
static double powerOf2s[powerSpan];
static int powerOf2sInitialized = 0;


typedef struct {
#if defined(Little_Endian)
    uint32_t RESERVED :16,RLE_TYP :4,LNG :4,ID :8,numOfPackedToken :26,numOfBitsPerToken :6;
#else
    uint32_t ID :8,LNG :4,RLE_TYP :4,RESERVED :16,numOfBitsPerToken :6,numOfPackedToken :26;
#endif
    uint32_t minInteger :32,maxRange :32;
} rle_header;


//! Pack or unpack integer array with RLE
int  compact_rle(
    //! [in,out] Unpacked integer array
    void *unpackedArrayOfInt,
    //! [in,out] Information about the packed integer array
    void *packedHeader,
    //! [in,out] Packed integer array
    void *packedArrayOfInt,
    //! [in] Maximum value in integer array.  Ignored when equal to min
    int max,
    //! [in] Mnimum value in integer array.  Ignored when equal to max
    int min,
    //! [in] Total number of elements in unpacked integer array
    int elementCount,
    //! [in] Packed integer size in bit
    int bitSizeOfPackedToken,
    //! [in] When packing, the last bit of integer packed inside array; When unpacking, the first bit of integer packed inside array
    int off_set,
    //! [in] Unpacked integer spacing
    int stride,
    //! [in] Pack when 1, unpack when 2
    int opCode
) {
    int wordSize;
    uint32_t *arrayOfUnpacked;
    uint32_t *arrayOfPacked;
    int i, k;
    int intCount;

    // Variables used by the packer
    int lastPackBit, spaceInLastWord, lastSlot;
    uint32_t lastWordShifted, tempInt;

    uint32_t minInteger, maxInteger;
    uint32_t maxRange;
    uint32_t currentToken, previousToken;

    int previousTokenCount, currentTokenCount;

    int currentPosition, totalPackedElementCount, maxRepeatPerToken;
    uint32_t *packHeader;
    int lastTokenPosition;

    uint32_t *arrayPosition;


    // Variables used by the unpacker
    int firstPackBit, bitPackInFirstWord, currentSlot;
    uint32_t currentWord, packInt;
    int significantBit, inSignificantBit;
    uint32_t *tempPackedArray;
    rle_header *theHeader;
    int repeatCount;


    if ( bitSizeOfPackedToken < 3 ) {
        return 0;
    }

    // Initialize the power ladder of base 2
    if ( ! powerOf2sInitialized ) {
        powerOf2s[0] = 1.0;
        for ( i = 1; i < powerSpan; i++) {
            powerOf2s[i] = 2.0 *powerOf2s[i-1];
        }
        powerOf2sInitialized = 1;
    }

    // Determine wordsize and others
    wordSize        = 8 * sizeof(uint32_t);
    arrayOfUnpacked = (uint32_t *)unpackedArrayOfInt;
    packHeader      = (uint32_t *) packedHeader;
    arrayOfPacked   = (uint32_t *)packedArrayOfInt;
    intCount        = elementCount;

    if ( opCode == 1 ) {
        // Obtain minimum, maximun, range to derive maxRepeatPerToken
        if ( max == min ) {
            maxInteger = arrayOfUnpacked[0];
            minInteger = arrayOfUnpacked[0];
            tempInt = intCount*stride;
            i = stride;
            while ( i < tempInt ) {
                if ( arrayOfUnpacked[i] < minInteger ) {
                    minInteger = arrayOfUnpacked[i];
                } else if ( arrayOfUnpacked[i] > maxInteger ) {
                    maxInteger = arrayOfUnpacked[i];
                }
                i += stride;
            }
        } else {
            maxInteger = max;
            minInteger = min;
        }
        maxRange = maxInteger - minInteger;
        maxRepeatPerToken = powerOf2s[bitSizeOfPackedToken] - 1 - maxRange;

        lastPackBit = off_set;
        spaceInLastWord =  wordSize - ( lastPackBit % wordSize );
        lastSlot = ( lastPackBit / wordSize );
        if ( spaceInLastWord == wordSize ) {
            lastWordShifted = 0;
        } else {
            lastWordShifted = arrayOfPacked[lastSlot] >> spaceInLastWord ;
        }

        totalPackedElementCount = 0;
        previousToken = -1;
        previousTokenCount = 1;
        lastTokenPosition = (intCount - 1) * stride;
        currentPosition = 0;

        arrayPosition = &arrayOfPacked[lastSlot];
        for ( i = 0; i < lastTokenPosition; i += stride ) {
            currentToken = arrayOfUnpacked[i] - minInteger;

            // Construct packing info
            if ( currentToken != previousToken ) {
                // New token encountered
                if (previousTokenCount == 1) {
                    stuff(currentToken, arrayPosition, wordSize, bitSizeOfPackedToken, lastWordShifted, spaceInLastWord);
                    totalPackedElementCount++;
                } else if ( previousTokenCount == 2 ) {
                    stuff(previousToken, arrayPosition, wordSize, bitSizeOfPackedToken,lastWordShifted, spaceInLastWord);
                    stuff(currentToken, arrayPosition, wordSize, bitSizeOfPackedToken,lastWordShifted, spaceInLastWord);
                    totalPackedElementCount += 2;
                } else {
                    tempInt = maxRange+previousTokenCount;
                    stuff(tempInt, arrayPosition, wordSize, bitSizeOfPackedToken, lastWordShifted, spaceInLastWord);
                    stuff(currentToken, arrayPosition, wordSize, bitSizeOfPackedToken,lastWordShifted, spaceInLastWord);
                    totalPackedElementCount += 2;
                }
                previousToken = currentToken;
                previousTokenCount = 1;
            } else {
                // Repeated token
                previousTokenCount++;
                if ( previousTokenCount == maxRepeatPerToken ) {
                    tempInt = maxRange + maxRepeatPerToken;
                    stuff(tempInt, arrayPosition, wordSize, bitSizeOfPackedToken, lastWordShifted, spaceInLastWord);
                    totalPackedElementCount++;
                    /* treat it same as starting condition */
                    previousToken = -1;
                    previousTokenCount = 1;
                }
            }
        }

        // Handle the last token
        currentToken = arrayOfUnpacked[lastTokenPosition] - minInteger;
        if ( currentToken != previousToken ){
            // New token
            stuff(currentToken, arrayPosition, wordSize, bitSizeOfPackedToken, lastWordShifted, spaceInLastWord);
        } else {
            previousTokenCount++;
            if ( previousTokenCount == 2 ) {
                stuff(currentToken, arrayPosition, wordSize, bitSizeOfPackedToken,lastWordShifted, spaceInLastWord);
            } else {
                tempInt = maxRange + previousTokenCount;
                stuff(tempInt, arrayPosition, wordSize, bitSizeOfPackedToken, lastWordShifted, spaceInLastWord);
            }
        }
        totalPackedElementCount++;

        // Squeeze hole left in the integer array
        if ( spaceInLastWord < wordSize ) {
            *arrayPosition = ( lastWordShifted << spaceInLastWord) | ( *arrayPosition & ~(-1 << spaceInLastWord));
        }

        // Construct pack header
        packHeader[0] = 0xF0000000 | (1 << (32-8-4));
        packHeader[1] = ( bitSizeOfPackedToken << 26 ) | totalPackedElementCount;
        packHeader[2] = minInteger;
        packHeader[3] = maxRange;

        return totalPackedElementCount;
    } else if ( opCode == 2 ) {
        // Unpack
        firstPackBit = off_set;
        bitPackInFirstWord =  wordSize - ( firstPackBit % wordSize );
        currentSlot = ( firstPackBit / wordSize );
        currentWord = arrayOfPacked[currentSlot] << ( wordSize - bitPackInFirstWord );
        intCount = elementCount;

        // Recover info from the header
        theHeader = (rle_header *)packedHeader;
        minInteger = theHeader->minInteger;
        maxInteger = minInteger + theHeader->maxRange;
        totalPackedElementCount = theHeader->numOfPackedToken;
        maxRange = maxInteger - minInteger;

        significantBit = theHeader->numOfBitsPerToken;

        currentPosition = 0;
        arrayPosition   = &arrayOfPacked[currentSlot];
        for ( i = 0; i < totalPackedElementCount; i++) {
            extract( tempInt, arrayPosition, wordSize, significantBit, currentWord, bitPackInFirstWord);
            currentToken = tempInt + minInteger;
            if (tempInt <= maxRange ) {
                arrayOfUnpacked[currentPosition] = currentToken;
                currentPosition += stride;
            } else {
                repeatCount = tempInt-maxRange-1;
                while ( repeatCount-- ) {
                    arrayOfUnpacked[currentPosition] = previousToken;
                    currentPosition += stride;
                }
            }
            previousToken = currentToken;
        }

        return totalPackedElementCount;
    } else {
        Lib_Log(APP_LIBRMN,APP_ERROR,"%f: opCode (%d) is not defined\n",__func__,opCode);
        return NULL;
    }
}
