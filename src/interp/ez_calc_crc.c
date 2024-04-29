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


//! Computer a CRC of an int array and optionally 2 float arrays
unsigned int ez_calc_crc(
    //! [in] Pointer of the array
    const int * const ptr,
    //! [in] Length of the array in multiples of 4
    const int * const flen,
    //! [in] Pointer to a array of floats. Ignored if NULL
    const float * const ax,
    //! [in] Pointer to a array of floats. Ignored if NULL
    const float * const ay,
    //! [in] Size of the ax array
    const int ni,
    //! [in] Size of the ay array
    const int nj
) {
    register unsigned int hold = 0;

    const int * lptr = ptr;
    int len = *flen / 4;
    for (int i = 0; i < len; i++, lptr++) {
        hold ^= *lptr;
    }

    const unsigned int *p2 = (unsigned int *) ax;
    if (p2 != NULL) {
        for (int i = 0; i < ni; i++, p2++) {
            hold ^= *p2;
        }
    }

    const unsigned int *p3 = (unsigned int *) ay;
    if (p3 != NULL) {
        for (int i = 0; i < nj; i++, p3++) {
            hold ^= *p3;
        }
    }
    //! \return Computed CRC
    return hold;
}

