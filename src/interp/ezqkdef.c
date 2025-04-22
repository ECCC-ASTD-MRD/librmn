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

//! \file

#include <rmn/ezscint.h>
#include "ez_funcdef.h"


//! Define a grid with the RPN standard file grid descriptors
int32_t c_ezqkdef(
    //! [in] Horizontal dimension of the grid along the x-axis
    const int32_t ni,
    //! [in] Horizontal dimension of the grid along the y-axis
    const int32_t nj,
    //! [in] Grid type ('A', 'B', 'E', 'G', 'L', 'N', 'S','Y', 'Z','U')
    const char * const grtyp,
    //! [in] First grid identifier
    const int32_t ig1,
    //! [in] Second grid identifier
    const int32_t ig2,
    //! [in] Third grid identifier
    const int32_t ig3,
    //! [in] Fourth grid identifier
    const int32_t ig4,
    //! [in] Fortran logical unit of the file containing the navigational records, if grtyp = 'Y' or 'Z' or U'.
    //! This parameters is not considered for the other grid types. Use 0 if you don't know what to enter.
    const int32_t iunit
) {
    //! \ingroup ezscint

    //! \deprecated Use \ref c_ezgdef_ffile or its Fortran equivalent instead

    //! Inserts a grid entry into the list of grids managed by the ezscint package.

    //! If the grid type is 'Y' or 'Z', the associated positional records ('^^') and ('>>') are automatically loaded.

    //! If the grid type is 'U', the information associated with its subgrids will be loaded.

    //! If the positional records are defined from memory rather than in a file, you will need to call \ref c_ezgdef_fmem for 'Y' and 'Z' grids.

    //! For defining 'U' grids from memory, you must use \ref c_ezgdef_fmem for its subgrids and then complete its
    //! definition with \ref c_ezgdef_supergrid.

    //! This function is applicable to both regular and irregular (‘Y’, ‘Z’, 'U','L','Y','#',etc.) grids.

    //! \note 'U' grid is a concatenation of two Z grids. For U grids: ni and nj are the dimensions of the data fields,
    //! not the dimensions of '^>' grid descriptor field. If in doubt, give the values -1 for ni and nj (this is only allowed for U grids)

    //! \return Grid identifier token that can be used in later calls to other ezscint functions
    return c_ezgdef_ffile(ni, nj, grtyp, ig1, ig2, ig3, ig4, iunit);
}


//! \copydoc c_ezqkdef
int32_t f77name(ezqkdef)(
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
    char cgrtyp[2];

    cgrtyp[0] = grtyp[0];
    cgrtyp[1] = '\0';
    return c_ezqkdef(*ni, *nj, cgrtyp, *ig1, *ig2, *ig3, *ig4, *iunit);
}
