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


unsigned int ez_calc_crc(
    int *p,
    int *flen,
    float *ax,
    float *ay,
    int ni,
    int nj
) {
    // crc computed so far
    register unsigned int hold = 0;

    int len = *flen / 4;
    for (int i = 0; i < len; i++, p++) {
        hold ^= *p;
    }

    unsigned int *p2 = (unsigned int *) ax;;
    if (p2 != NULL) {
        for (int i = 0; i < ni; i++, p2++) {
            hold ^= *p2;
        }
    }

    p2 = (unsigned int *) ay;
    if (p2 != NULL) {
        for (int i = 0; i < nj; i++, p2++) {
            hold ^= *p2;
        }
    }
    return hold;
}

