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


void c_ez_manageGrillesMemory() {
    int nchunks = nGrilles / (CHUNK * CHUNK);
    if (nchunks > 0 && 0 == (nGrilles % (CHUNK * CHUNK))) {
        Grille = (_Grille **) realloc(Grille, CHUNK * (nchunks+1) * sizeof(_Grille *));
    }

    if (0 == (nGrilles % (CHUNK))) {
        Grille[(nGrilles >> LOG2_CHUNK)] = (_Grille *) malloc(CHUNK * sizeof(_Grille));
    }
}


int32_t c_ezidentify_reg_grid(
    int32_t ni,
    int32_t nj,
    char* grtyp,
    int32_t ig1,
    int32_t ig2,
    int32_t ig3,
    int32_t ig4
) {
    int32_t newgrsize = sizeof(_Grille);
    if (nGrilles == 0) {
        gr_list = calloc(chunks_sq[cur_log_chunk], sizeof(_Grille *));
        // printf("c_ezidentify_reg_grid(..) gr_list=%p\n", gr_list);
        Grille = (_Grille **) calloc(chunks[cur_log_chunk], sizeof(_Grille *));
        Grille[0] = (_Grille *) calloc(chunks[cur_log_chunk], newgrsize);
        for (int32_t i = 0; i < chunks[cur_log_chunk]; i++) {
            Grille[0][i].index = -1;
        }
    }

    _Grille newgr;
    memset((void *)&newgr, 0, newgrsize);
    newgr.grtyp[0] = grtyp[0];
    newgr.grref[0] = (char) 0;
    newgr.ni = ni;
    newgr.nj = nj;
    newgr.fst.ig[IG1] = ig1;
    newgr.fst.ig[IG2] = ig2;
    newgr.fst.ig[IG3] = ig3;
    newgr.fst.ig[IG4] = ig4;
    newgr.fst.igref[IG1] = 0;
    newgr.fst.igref[IG2] = 0;
    newgr.fst.igref[IG3] = 0;
    newgr.fst.igref[IG4] = 0;


    unsigned int grid_crc = ez_calc_crc((int *)&newgr, &newgrsize, NULL, NULL, 0, 0);
    int32_t grid_index = grid_crc % primes_sq[cur_log_chunk];
    int32_t gdid = -1;
    if (gr_list[grid_index] == NULL) {
        // printf("c_ezidentify_reg_grid(..) grid_index=%d\n", grid_index);
        // printf("c_ezidentify_reg_grid(..) newgr=%p\n", &newgr);
        gdid = c_ez_addgrid(grid_index, &newgr);
        return gdid;
    } else {
        gdid = c_ez_findgrid(grid_index, &newgr);
        if (gdid == -1) {
            gdid = c_ez_addgrid(grid_index, &newgr);
            return gdid;
        } else {
            return gdid;
        }
    }
}


int32_t c_ezidentify_irreg_grid(
    int32_t ni,
    int32_t nj,
    char* grtyp,
    char* grref,
    int32_t ig1,
    int32_t ig2,
    int32_t ig3,
    int32_t ig4,
    float* ax,
    float* ay
) {
    if (nGrilles == 0) {
        gr_list = calloc(chunks_sq[cur_log_chunk], sizeof(_Grille *));
        Grille = (_Grille **) calloc(chunks[cur_log_chunk],sizeof(_Grille *));
        Grille[0] = (_Grille *) calloc(chunks[cur_log_chunk], sizeof(_Grille));
        for (int32_t i = 0; i < chunks[cur_log_chunk]; i++) {
            Grille[0][i].index = -1;
        }
    }

    _Grille newgr;
    memset(&newgr, 0, sizeof(_Grille));
    newgr.grtyp[0] = grtyp[0];
    newgr.grtyp[1] = '\0';
    newgr.grref[0] = grref[0];
    newgr.grref[1] = '\0';
    newgr.ni = ni;
    newgr.nj = nj;
    newgr.fst.ip1 = 0;
    newgr.fst.ip2 = 0;
    newgr.fst.ip3 = 0;
    newgr.fst.igref[IG1] = ig1;
    newgr.fst.igref[IG2] = ig2;
    newgr.fst.igref[IG3] = ig3;
    newgr.fst.igref[IG4] = ig4;
    newgr.fst.ig[IG1] = ig1;
    newgr.fst.ig[IG2] = ig2;
    newgr.fst.ig[IG3] = ig3;
    newgr.fst.ig[IG4] = ig4;
    newgr.fst.xg[IG1] = 0.0;
    newgr.fst.xg[IG2] = 0.0;
    newgr.fst.xg[IG3] = 0.0;
    newgr.fst.xg[IG4] = 0.0;
    newgr.nsubgrids = 0;
    strcpy(newgr.fst.nomvarx, "    ");
    strcpy(newgr.fst.nomvary, "    ");
    strcpy(newgr.fst.etiketx, "            ");
    strcpy(newgr.fst.etikety, "            ");
    strcpy(newgr.fst.typvarx, "  ");
    strcpy(newgr.fst.typvary, "  ");
    newgr.fst.deet = 0;
    newgr.fst.npas = 0;
    newgr.fst.nbits = 0;
    newgr.fst.date = 0;
    newgr.i1 = 1;
    newgr.i2 = ni;
    newgr.j1 = 1;
    newgr.j2 = nj;
    newgr.idx_last_gdin = -1;

    int32_t newgrsize = sizeof(_Grille);
    int32_t npts;
    char typeGrille = grtyp[0];
    unsigned int grid_crc;
    switch(typeGrille) {
        case '#':
            grid_crc = ez_calc_crc((int *)&newgr, &newgrsize, &(ax[ig3-1]), &(ay[ig4-1]), ni, nj);
            newgr.ax = ax;
            newgr.ay = ay;
            break;

        case 'Y':
            npts = ni * nj;
            grid_crc = ez_calc_crc((int *)&newgr, &newgrsize, ax, ay, npts, npts);
            newgr.ax = ax;
            newgr.ay = ay;
            break;

        case 'Z':
            f77name(cigaxg)(&(newgr.grref),
                &newgr.fst.xgref[XLAT1], &newgr.fst.xgref[XLON1], &newgr.fst.xgref[XLAT2], &newgr.fst.xgref[XLON2],
                &newgr.fst.igref[IG1],   &newgr.fst.igref[IG2],   &newgr.fst.igref[IG3],   &newgr.fst.igref[IG4],1);
            grid_crc = ez_calc_crc((int *)&newgr, &newgrsize, ax, ay, ni, nj);
            newgr.ax = ax;
            newgr.ay = ay;
            break;

        case 'G':
            grid_crc = ez_calc_crc((int *)&newgr, &newgrsize, NULL, NULL, 0, 0);
            break;

        default :
            fprintf(stderr, "c_ezidentify_irreg_grid : undefined grid type : %c\n", typeGrille);
            exit(13);
    }

    int32_t grid_index = grid_crc % primes_sq[cur_log_chunk];

    int32_t gdid = -1;
    if (gr_list[grid_index] == NULL) {
        gdid = c_ez_addgrid(grid_index, &newgr);
        return gdid;
    } else {
        gdid = c_ez_findgrid(grid_index, &newgr);
        if (gdid == -1) {
            gdid = c_ez_addgrid(grid_index, &newgr);
            return gdid;
        } else {
            return gdid;
        }
    }
}
