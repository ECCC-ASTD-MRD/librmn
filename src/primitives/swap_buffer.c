/* RMNLIB - Library of useful routines for C and FORTRAN programming
 * Copyright (C) 1975-2022  Division de Recherche en Prevision Numerique
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


#include <stdlib.h>


#include <rmn/swap_buffer.h>


//! Swap the endianess of a 32 bits word
static inline void swap_word_endianness(
    //! [in,out] Word to convert
    uint32_t * const mot
) {
    register uint32_t tmp = (uint32_t) *mot;
    *mot = (tmp >> 24) | (tmp << 24) | ((tmp >> 8) & 0xFF00) | ((tmp & 0xFF00) << 8);
}


//! Swap the endianess of 32 bits words in a buffer
void swap_buffer_endianness(
    //! [in,out] Bouffer containing the words to swap
    uint32_t * const buffer,
    //! [in] Number of words in buffer
    const int32_t nwds
) {
    for (int32_t i = 0; i < nwds; i++) {
        swap_word_endianness(&buffer[i]);
    }
}
