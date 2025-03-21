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


#include <stdlib.h>
#include <string.h>

#include <rmn/ezscint.h>
#include "ez_funcdef.h"

#ifdef MUTEX
//JP
extern pthread_mutex_t EZ_MTX;
#endif


void EliminerGrille(int32_t gdid) {
#ifdef MUTEX
    pthread_mutex_lock(&EZ_MTX);
#endif

    int32_t gdrow_id, gdcol_id;
    c_gdkey2rowcol(gdid,  &gdrow_id,  &gdcol_id);

    if (Grille[gdrow_id][gdcol_id].access_count > 0) {
        Grille[gdrow_id][gdcol_id].access_count--;
    }

    if (Grille[gdrow_id][gdcol_id].access_count == 0) {
        if (Grille[gdrow_id][gdcol_id].flags & LAT) {
            if (Grille[gdrow_id][gdcol_id].lat != NULL) {
                free(Grille[gdrow_id][gdcol_id].lat);
                free(Grille[gdrow_id][gdcol_id].lon);
            }
        }

        if (Grille[gdrow_id][gdcol_id].flags & AX) {
            if (Grille[gdrow_id][gdcol_id].ax != NULL) {
                free(Grille[gdrow_id][gdcol_id].ax);
                free(Grille[gdrow_id][gdcol_id].ay);
            }
            }

        if (Grille[gdrow_id][gdcol_id].ncx != NULL) {
            if (Grille[gdrow_id][gdcol_id].ncx != NULL) {
                free(Grille[gdrow_id][gdcol_id].ncx);
                free(Grille[gdrow_id][gdcol_id].ncy);
           }
        }

        for (int32_t i = 0; i < Grille[gdrow_id][gdcol_id].n_gdin_for; i++) {
            int32_t index = ez_find_gdin_in_gset(gdid, Grille[gdrow_id][gdcol_id].gdin_for[i]);
            c_ezfreegridset(Grille[gdrow_id][gdcol_id].gdin_for[i], index);
        }

        gr_list[Grille[gdrow_id][gdcol_id].grid_index] = (_Grille *) NULL;
        memset(&(Grille[gdrow_id][gdcol_id]), 0, sizeof(_Grille));
        nGrilles--;
    }

#ifdef MUTEX
    pthread_mutex_unlock(&EZ_MTX);
#endif
}


int32_t f77name(gdrls)(int32_t *gdin) {
    return c_gdrls(*gdin);
}

int32_t c_gdrls(int32_t gdin) {
    EliminerGrille(gdin);

    return 0;
}
