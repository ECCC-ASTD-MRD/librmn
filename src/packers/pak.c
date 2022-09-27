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

#include <stddef.h>
#include <stdint.h>
#include <stdlib.h>

#include <rmn/rpnmacros.h>
#include <armn_compress.h>


//! FORTRAN interface to integer packer/unpacker written in C
void f77name(iipak)(
    //! [in,out] Unpacked integer array
    void * const xunpacked,
    //! [in,out] Packed integer array
    void * const xpacked,
    //! [in] Number of elements in the unpacked integer array
    const int32_t * const ni,
    //! [in] Unpacked integer spacing indicator
    const int32_t * const nj,
    //! [in] Packed integer size in bit
    const int32_t * const nBits,
    //! In packing mode, the last bit of integer packed inside array
    //! In unpacking mode : the first bit of integer packed inside array
    const int32_t * const offset,
    //! Operation mode:
    //! | -----:| :-----------------------: |
    //! |     1 | Unsigned pack             |
    //! |     2 | Unsigned unpack           |
    //! |     3 | Signed pack               |
    //! |     4 | Signed unpack             |
    //! |   +10 | Use offset                |
    //! |  +100 | NINJ = nj and stride = nj |
    //! | +1000 | Use pack header           |
    const int32_t * const mode
) {
    // fix lnBits
    int lnBits;
    if (*nBits > 1) {
        if (1 > (32 / *nBits)) {
            lnBits = 1;
        } else {
            lnBits = (32 / *nBits);
        }
    } else if (*nBits < 0) {
        lnBits = *nBits * (-1);
    } else {
        lnBits = 32;
    }

    // Check if pack header is used
    int oper = *mode;
    int packHeaderUsed;
    if ( (oper % 10000) >= 1000 ) {
        packHeaderUsed = 1;
    } else {
        packHeaderUsed = 0;
    }
    oper = oper % 1000;

    // handle the stride and offset specification from FORTRAN program
    int stride;
    int ninj;
    if ((oper % 1000) >= 100) {
        ninj = *ni;
        stride = *nj;
        oper = oper % 100;
    } else {
      ninj = (*ni) * (*nj);
      stride = 1;
    }

    int loffset;
    if (oper >= 10) {
        loffset = *offset;
        oper = (oper % 10);
    } else {
        loffset = 0;
    }

    uint32_t  *ptrXpacked = xpacked;

    if ( xunpacked == xpacked ) {
        // In place
        int lengthOfPackedArray = ((ninj * lnBits) + 31) / 32;
        if (packHeaderUsed) lengthOfPackedArray += 4;

        uint32_t *tempPackedArray = ( uint32_t *) malloc(lengthOfPackedArray*sizeof(uint32_t));

        if ( (oper == 1) || (oper == 3) ) {
            // Pack
            if ( packHeaderUsed ) {
                compact_integer(xunpacked, tempPackedArray, tempPackedArray, ninj, lnBits, 128, stride, oper);
            } else {
                compact_integer(xunpacked, NULL, tempPackedArray, ninj, lnBits, loffset, stride, oper);
            }

            for (int i = 0; i < lengthOfPackedArray; i++) {
                ptrXpacked[i] = tempPackedArray[i];
            }
        } else if ( (oper == 2) || (oper == 4) ) {
            // Unpack
            for (int i = 0; i < lengthOfPackedArray; i++) {
                tempPackedArray[i] = ptrXpacked[i] ;
            }

            if ( packHeaderUsed ){
                compact_integer(xunpacked, tempPackedArray, tempPackedArray, ninj, lnBits, 128, stride, oper);
            } else {
                compact_integer(xunpacked, NULL, tempPackedArray, ninj, lnBits, loffset, stride, oper);
            }
        }

        free(tempPackedArray);
        tempPackedArray = NULL;
    } else {
        // Not in place
        if ( packHeaderUsed ) {
            compact_integer(xunpacked, xpacked, xpacked, ninj, lnBits, 128, stride, oper);
        } else {
            compact_integer(xunpacked, NULL, xpacked, ninj, lnBits, loffset, stride, oper);
        }
    }
}


void f77name(xxpak) (
    //! [in,out] Unpacked floating point array
    void * const xunpacked,
    //! [in,out] Packed integer array
    void * const xpacked,
    //! [in] Number of elements in the unpacked integer array
    const int32_t * const ni,
    //! [in] Unpacked integer spacing indicator
    const int32_t * const nj,
    //! [in] Packed integer size in bit
    const int32_t * const nBits,
    //! \deprecated Unused
    const int32_t * const unused,
    //! Operation mode:
    //! | -:| :-----------: |
    //! | 1 | Pack float    |
    //! | 2 | Unpack float  |
    //! | 3 | Pack double   |
    //! | 4 | Unpack double |
    const int32_t * const mode
) {
    const int stride = 1;
    const int offset = 24;
    const int ninj = ((*ni) * (*nj));
    int oper = *mode;
    const double tempFloat = 9999.0000;

    int lnBits = (*nBits);
    if (*nBits > 1) {
        if (1 > (32 / *nBits)) {
            lnBits = 1;
        } else {
            lnBits = (32 / (int) *nBits);
        }
    } else if (*nBits < 0) {
        lnBits = (int) *nBits * (-1);
    } else {
        lnBits = 32;
    }

    // determine function pointer and make oper uniform for pack and unpack
    PackFunctionPointer pfp = oper > 3 ? &compact_double : &compact_float;
    oper = oper > 3 ? oper - 4 : oper;

    uint32_t *ptrXpacked = xpacked;

    // call the appropriate pack/unpack routine written in C
    if ( xunpacked == ptrXpacked ) {
        // In place
        int lengthOfIntArray = (ninj * lnBits) / 32 + 6;
        uint32_t *tempIntArray = ( uint32_t *) malloc(lengthOfIntArray * sizeof(uint32_t));

      if (oper == 1) {
            // Pack
            (*pfp)(xunpacked, &tempIntArray[0], &tempIntArray[3], ninj, lnBits, offset, stride, oper, 0, &tempFloat);
            for (int i = 0; i < lengthOfIntArray; i++) {
                ptrXpacked[i] = tempIntArray[i];
            }
        } else if ( oper == 2 ) {
            // Unpack
            for (int i = 0; i < lengthOfIntArray; i++) {
                tempIntArray[i] = ptrXpacked[i] ;
            }
            (*pfp)(xunpacked, &tempIntArray[0], &tempIntArray[3], ninj, lnBits, offset, stride, oper, 0, &tempFloat);
        }

        free(tempIntArray);
    } else {
        (*pfp)(xunpacked, &ptrXpacked[0], &ptrXpacked[3], ninj, lnBits, offset, stride, oper, 0, &tempFloat);
    }
}
