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

#include <rmn/rpnmacros.h>

void f77name(rah2char)(char *chaine, int32_t *f_entier, const int32_t * const f_nc, F2Cl lng) {
    int nc = *f_nc;
    int entier = *f_entier;

    if (nc > lng) {
        fprintf(stderr, "rah2char ERROR: nc(%d) > lng(%d) using lng\n", nc, lng);
        nc = lng;
    }
    for (int i = 0; i < nc; i++) {
        entier <<= ((8 * sizeof(entier)) - (nc * 8));
        *chaine = (entier >> 8 * sizeof(entier) - 8) & 0xFF;
        entier <<= 8;
        // printf("DEBUG *chaine='%c'\n",*chaine);
        chaine++;
    }
}


void f77name(char2rah)(char *chaine, int32_t *entier, const int32_t * const f_nc, F2Cl lng){
    int nc = *f_nc;

    if (nc > lng) {
        fprintf(stderr, "char2rah ERROR: nc(%d) > lng(%d) using lng\n", nc, lng);
        nc = lng;
    }

    for (int i = 0; i < nc; i++) {
        *entier <<= 8;
        *entier |= chaine[i];
        // printf("DEBUG chaine[i]='%c' *entier=%x\n",chaine[i],*entier);
    }
}
