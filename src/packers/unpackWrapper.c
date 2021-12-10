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

#include <stdint.h>
#include <stdio.h>

#include <rmnlib.h>


#define NOTSPECIFIED -1


//! Convinience function to unpack data
void unpackWrapper(
    //! [out] Array of unpacked numbers
    void *unpackedArray,
    //! [in] Format information of packed integer numbers
    void *packedHeader,
    //! [in] Array of packed integers
    void *packedArray,
    //! [in] Spacing indicator
    int stride,
    //! [in] Missing value identifier
    void *missingValueTag
) {
    int elementCount, bitSizeOfPackedToken, bitSizeOfPackedExpo, off_set, opCode, hasMissing;
    int min, max;
    uint32_t  headerType;
    uint32_t *packHeader;

    elementCount         = NOTSPECIFIED;
    bitSizeOfPackedToken = NOTSPECIFIED;
    off_set              = 128;
    min                  = NOTSPECIFIED;
    max                  = NOTSPECIFIED;
    packHeader           = (uint32_t *)packedHeader;
    headerType           = packHeader[0] >> 24;

    if (( headerType == 0x0000007f ) || ( headerType == 0x000000ff )) {
        // Floating point, without missing value
        opCode     = 2;
        hasMissing = 0;
        compact_float(unpackedArray, packedHeader, packedArray,
                        elementCount, bitSizeOfPackedToken, off_set, stride, opCode,
                        hasMissing, missingValueTag);
    } else if (( headerType == 0x0000007e ) || ( headerType == 0x000000fe )) {
        // Floating point, with missing value
        opCode     = 2;
        hasMissing = 1;
        compact_float(unpackedArray, packedHeader, packedArray,
                        elementCount, bitSizeOfPackedToken, off_set, stride, opCode,
                        hasMissing, missingValueTag);
    } else if ( headerType == 0x000000fb ) {
        // IEEE block
        opCode              = 2;
        hasMissing          = 0;
        bitSizeOfPackedExpo = NOTSPECIFIED;
        off_set             = 0;
        compact_IEEEblock_float(unpackedArray, packedHeader, packedArray,
                                elementCount, bitSizeOfPackedToken, bitSizeOfPackedExpo,
                                off_set, stride, opCode, hasMissing, missingValueTag);
    } else if ( headerType == 0x000000fd ) {
        // Integer
        opCode = 2;

        compact_integer(unpackedArray, packedHeader, packedArray,
                        elementCount, bitSizeOfPackedToken, off_set, stride,
                        opCode);
    } else if ( headerType == 0x000000f0 ) {
        // Run length encoding
        opCode = 2;
        compact_rle(unpackedArray, packedHeader, packedArray,
                    max, min,
                    elementCount, bitSizeOfPackedToken, off_set,
                    stride, opCode);
    } else {
        printf("\n %8.8x not a valid header \n", headerType);
    }
}
