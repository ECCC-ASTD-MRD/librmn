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

#include <rmn/ezscint.h>
#include "ez_funcdef.h"

//! \file


//! Get grid parameters
int32_t c_ezgxprm(
    //! [in] Grid id
    int32_t gdid,
    //! [out] First dimension
    int32_t * const ni,
    //! [out] Second dimension
    int32_t * const nj,
    //! [out] Grid type
    char * const grtyp,
    //! [out] First grid parameter
    int32_t * const ig1,
    //! [out] Second grid parameter
    int32_t * const ig2,
    //! [out] Third grid parameter
    int32_t * const ig3,
    //! [out] Fourth grid parameter
    int32_t * const ig4,
    //! [out] Reference grid
    char * const grref,
    //! [out] Reference grid first grid parameter
    int32_t * const ig1ref,
    //! [out] Reference grid second grid parameter
    int32_t * const ig2ref,
    //! [out] Reference grid third grid parameter
    int32_t * const ig3ref,
    //! [out] Reference grid forth grid parameter
    int32_t * const ig4ref
) {
    //! \ingroup ezscint

    int32_t gdrow_id;
    int32_t gdcol_id;
    c_gdkey2rowcol(gdid, &gdrow_id, &gdcol_id);

    *ni       = Grille[gdrow_id][gdcol_id].ni;
    *nj       = Grille[gdrow_id][gdcol_id].nj;
    grtyp[0]  = Grille[gdrow_id][gdcol_id].grtyp[0];
    grtyp[1]  = '\0';
    grref[0]  = Grille[gdrow_id][gdcol_id].grref[0];
    grref[1]  = '\0';

    *ig1    = Grille[gdrow_id][gdcol_id].fst.ig[IG1];
    *ig2    = Grille[gdrow_id][gdcol_id].fst.ig[IG2];
    *ig3    = Grille[gdrow_id][gdcol_id].fst.ig[IG3];
    *ig4    = Grille[gdrow_id][gdcol_id].fst.ig[IG4];
    *ig1ref = Grille[gdrow_id][gdcol_id].fst.igref[IG1];
    *ig2ref = Grille[gdrow_id][gdcol_id].fst.igref[IG2];
    *ig3ref = Grille[gdrow_id][gdcol_id].fst.igref[IG3];
    *ig4ref = Grille[gdrow_id][gdcol_id].fst.igref[IG4];

    //! \return Always 0
    return 0;
}


int32_t f77name(ezgxprm)(int32_t *gdid, int32_t *ni, int32_t *nj, char *grtyp,
                     int32_t *ig1, int32_t *ig2, int32_t *ig3, int32_t *ig4, 
                     char *grref, int32_t *ig1ref, int32_t *ig2ref, 
                     int32_t *ig3ref, int32_t *ig4ref,
                     F2Cl lengrtyp, F2Cl lengrref)
{
    char lgrtyp[2];
    char lgrref[2];

    lgrtyp[0] = ' ';
    lgrref[0] = ' ';
    lgrtyp[1] = '\0';
    lgrref[1] = '\0';
    int32_t icode = c_ezgxprm(*gdid, ni, nj, lgrtyp, ig1, ig2, ig3, ig4, 
                        lgrref, ig1ref, ig2ref, ig3ref, ig4ref);
    grtyp[0] = lgrtyp[0];
    grref[0] = lgrref[0];

    return icode;
}
