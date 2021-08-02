/* RMNLIB - Library of useful routines for C and FORTRAN programming
 * Copyright (C) 1975-2007  Environnement Canada
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
#include <stdlib.h>

//! FORTRAN style trimmed source to arbitrarily padded destination copy
//! If destination pointer is NULL, no copy takes place.
//! @return Trimmed source length
int ftn2c_string_copy(
    //! Source string
    unsigned char* src,
    //! Destination string
    unsigned char* dest,
    //! Length of source string
    int lsrc,
    //! Length of destination string
    int ldest,
    //! Character used for padding
    unsigned char pad
) {
    int i;
    // If there is a null before lsrc characters in the source string, act as if it was terminated at null
    for (i = 0 ; src[i] != 0 && i < lsrc; i++);
    lsrc = i;
    // Ignore trailing blanks in source string
    while (src[lsrc - 1] == ' ') lsrc--;
    // No destination, just return trimmed source length
    if (dest == NULL) return lsrc;
    // Trimmed source longer than destination!
    if (lsrc > ldest) return -1;
    // Not enough space for padding!
    if (pad == 0 && lsrc == ldest) return -1;
    // Copy src to dest
    for (i = 0; i < lsrc; i++) dest[i] = src[i];
    if (pad) {
        // Pad destination
        while(i < ldest) dest[i++] = pad;
    } else {
        dest[i] = pad;
    }
    // return number of significant characters copied
    return lsrc;
}


//! C String array to Fortran String Array
int ftn2c_cstra_fstra(
    unsigned char** src,
    unsigned char* dest,
    int lsrc,
    int ldest,
    int nitems,
    unsigned char pad
) {
    int ii;
    if (nitems <= 0) return -1;
    for (ii = 0; ii < nitems; ii++) {
        if (ftn2c_string_copy(src[ii], dest, lsrc, ldest, pad) < 0) return -1;
        dest += ldest;
    }
    return 0;
}


//! Fortran String array to C String Array
int ftn2c_fstra_cstra(
    unsigned char *src,
    unsigned char **dest,
    int lsrc,
    int ldest,
    int nitems,
    unsigned char pad
) {
    int ii;
    if (nitems <= 0) return -1;
    for (ii = 0; ii < nitems; ii++) {
        if (ftn2c_string_copy(src, dest[ii], lsrc, ldest, pad) < 0) return -1;
        src += lsrc;
    }
    return 0;
}
