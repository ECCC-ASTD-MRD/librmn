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
#include <stdlib.h>
#include <string.h>

#include <rmn/ezscint.h>
#include "ez_funcdef.h"


int32_t c_ezgdef_ffile(
    int32_t ni,
    int32_t nj,
    char *grtyp,
    int32_t ig1,
    int32_t ig2,
    int32_t ig3,
    int32_t ig4,
    int32_t iunit
) {
    _Grille *gr, newgr;
    int32_t read;

    int32_t found = 0;
    char typeGrille = (char)grtyp[0];
    if (typeGrille != '#' && typeGrille != 'Y' && typeGrille != 'Z' && typeGrille != 'U' && typeGrille != ' ') {
        /* no need to look for grid descriptors */
        char grref[2];
        float *bidon = NULL;
        strcpy(grref, " ");
        return c_ezgdef_fmem(ni, nj, grtyp, grref, ig1, ig2, ig3, ig4, bidon, bidon);
    }

    if (nGrilles == 0) {
        gr_list = calloc(chunks_sq[cur_log_chunk], sizeof(_Grille *));
        Grille = (_Grille **)calloc(chunks[cur_log_chunk], sizeof(_Grille *));
        Grille[0] = (_Grille *)calloc(chunks[cur_log_chunk], sizeof(_Grille));
        for (int32_t i = 0; i < chunks[cur_log_chunk]; i++) {
            Grille[0][i].index = -1;
        }
    }

    memset(&newgr, (int)0, sizeof(_Grille));
    strcpy(newgr.grtyp, grtyp);
    /* incoming ni,nj specified by the user */
    newgr.ni = ni;
    newgr.nj = nj;
    newgr.fst.ig[IG1] = ig1;
    newgr.fst.ig[IG2] = ig2;
    newgr.fst.ig[IG3] = ig3;
    newgr.fst.ig[IG4] = ig4;
    newgr.idx_last_gdin = -1;
    read = 0;
    found=LireEnrPositionnels(&newgr, iunit, ig1, ig2, ig3, ig4, read);
    if (found < 0) {
        /* problems with finding grid descriptors */
        return found;
    }
    int32_t newgrsize = sizeof(_Grille);
    unsigned int grid_crc = ez_calc_crc((int *)&newgr, &newgrsize, newgr.ax, newgr.ay, newgr.ni, newgr.nj);
    int32_t grid_index = grid_crc % primes_sq[cur_log_chunk];

    int gdid;
    if (gr_list[grid_index] == NULL) {
        gdid = c_ez_addgrid(grid_index, &newgr);
    } else {
        gdid = c_ez_findgrid(grid_index, &newgr);
        if (gdid == -1) {
            gdid = c_ez_addgrid(grid_index, &newgr);
        } else {
            return gdid;
        }
    }

    /* define new grid */
    int32_t gdrow_in, gdcol_in;
    c_gdkey2rowcol(gdid, &gdrow_in, &gdcol_in);
    read = 1;
    switch(newgr.grtyp[0]) {
        case '#':
        case 'U':
            gr = &Grille[gdrow_in][gdcol_in];
            found=LireEnrPositionnels(gr, iunit, ig1, ig2, ig3, ig4, read);
            break;

        default:
            found=LireEnrPositionnels(&(Grille[gdrow_in][gdcol_in]),iunit, ig1, ig2, ig3, 0,read);
            break;
    }
    if (found < 0) {
        /* problems with reading grid descriptors */
        return found;
    }

    c_gdkey2rowcol(gdid, &gdrow_in, &gdcol_in);
    if (*grtyp == 'U') {
        return gdid;
    }
    ez_calcxpncof(gdid);
    Grille[gdrow_in][gdcol_in].i1 = 1;
    Grille[gdrow_in][gdcol_in].i2 = newgr.ni;
    Grille[gdrow_in][gdcol_in].j1 = 1;
    Grille[gdrow_in][gdcol_in].j2 = newgr.nj;
    if (*grtyp != 'Y') {
        c_ezdefxg(gdid);
        ez_calcntncof(gdid);
    } else {
        ez_calclatlon(gdid);
    }

    if (groptions.verbose > 0) {
        printf("gdid = %02d\n", gdid);
        printf("Grille[%02d].grtyp = '%c'\n", gdid, Grille[gdrow_in][gdcol_in].grtyp[0]);
        printf("Grille[%02d].ni    = %d\n",   gdid, Grille[gdrow_in][gdcol_in].ni);
        printf("Grille[%02d].nj    = %d\n",   gdid, Grille[gdrow_in][gdcol_in].nj);
        printf("Grille[%02d].ig1   = %d\n",   gdid, Grille[gdrow_in][gdcol_in].fst.ig[IG1]);
        printf("Grille[%02d].ig2   = %d\n",   gdid, Grille[gdrow_in][gdcol_in].fst.ig[IG2]);
        printf("Grille[%02d].ig3   = %d\n",   gdid, Grille[gdrow_in][gdcol_in].fst.ig[IG3]);
        printf("Grille[%02d].ig4   = %d\n",   gdid, Grille[gdrow_in][gdcol_in].fst.ig[IG4]);
    }

    return gdid;
}


int32_t f77name(ezgdef_ffile)(
    int32_t *ni,
    int32_t *nj,
    char *grtyp,
    int32_t *ig1,
    int32_t *ig2,
    int32_t *ig3,
    int32_t *ig4,
    int32_t *iunit,
    F2Cl lengrtyp
) {
    char lgrtyp[2];

    lgrtyp[0] = grtyp[0];
    lgrtyp[1] = '\0';

    return c_ezgdef_ffile(*ni, *nj, lgrtyp, *ig1, *ig2, *ig3, *ig4, *iunit);
}
