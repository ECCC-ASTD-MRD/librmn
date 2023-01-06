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

#include <App.h>
#include <armn_compress.h>

#include "compact_IEEEblock.h"


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
    uint32_t *packHeader = (uint32_t *)packedHeader;
    uint32_t headerType = packHeader[0] >> 24;

    if (( headerType == 0x0000007f ) || ( headerType == 0x000000ff )) {
        // Floating point, without missing value
        compact_float(unpackedArray, packedHeader, packedArray, -1, -1, 128, stride, 2, 0, missingValueTag);
    } else if (( headerType == 0x0000007e ) || ( headerType == 0x000000fe )) {
        // Floating point, with missing value
        compact_float(unpackedArray, packedHeader, packedArray, -1, -1, 128, stride, 2, 1, missingValueTag);
    } else if ( headerType == 0x000000fb ) {
        compact_IEEEblock_float(unpackedArray, packedHeader, packedArray, -1, -1, -1, 0, stride, 2, 0, missingValueTag);
    } else if ( headerType == 0x000000fd ) {
        compact_integer(unpackedArray, packedHeader, packedArray, -1, -1, 128, stride, 2);
    } else if ( headerType == 0x000000f0 ) {
        compact_rle(unpackedArray, packedHeader, packedArray, -1, -1, -1, -1, 128, stride, 2);
    } else {
        Lib_Log(APP_LIBRMN,APP_ERROR,"%s: %8.8x not a valid header\n",__func__,headerType);
    }
}
