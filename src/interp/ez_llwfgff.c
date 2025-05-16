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

#include "rmn/base.h"
#include "rmn/ezscint.h"
#include <rmn/f_ezscint.h>
#include "ez_funcdef.h"
#include "base/base.h"


//! \file


//! Convert wins from a rotated grid to a non-rotated one
void c_ezllwfgff(
    //! [out] Rotated U wind component
    float * const uullout,
    //! [out] Rotated V wind component
    float * const vvllout,
    //! [in] Real latitudes
    const float * const latin,
    //! [in] Real longitudes
    const float * const lonin,
    //! [in] Latitudes on the rotated grid
    const float * const xlatingf,
    //! [in] Longitudes on the rotated grid
    const float * const xloningf,
    //! [in] Number of points in the first dimension
    const int32_t * const ni,
    //! [in] Number of points in the second dimension
    const int32_t * const nj,
    //! [in] Grid type
    const char * const grtyp,
    //! [in] First integer grid parameter. See \ref cxgaig for parameter description
    const int32_t * const ig1,
    //! [in] Second integer grid parameter
    const int32_t * const ig2,
    //! [in] Third integer grid parameter
    const int32_t * const ig3,
    //! [in] Fourth integer grid parameter
    const int32_t * const ig4
) {
    //! \ingroup ezscint

    int32_t npts = *ni * *nj;
    int32_t trois = 3;
    float r[9], ri[9], xlon1, xlat1, xlon2, xlat2;

    float * uvcart = (float *) malloc(3*npts*sizeof(float));
    float * xyz    = (float *) malloc(3*npts*sizeof(float));

    f77name(cigaxg)(grtyp, &xlat1, &xlon1, &xlat2, &xlon2, ig1, ig2, ig3, ig4, 1);
    f77name(ez_crot)(r, ri, &xlon1, &xlat1, &xlon2, &xlat2);

    f77name(ez_uvacart)(xyz, uullout, vvllout, xloningf, xlatingf, ni, nj);
    f77name(mxm)(ri, &trois, xyz, &trois, uvcart, &npts);
    f77name(ez_cartauv)(uullout, vvllout, uvcart, lonin, latin, ni, nj);

    free(uvcart);
    free(xyz);
}


//! \copydoc c_ezllwfgff
void f77name(ez_llwfgff)(
    float * const uullout,
    float * const vvllout,
    const float * const latin,
    const float * const lonin,
    const float * const xlatingf,
    const float * const xloningf,
    const int32_t * const ni,
    const int32_t * const nj,
    const char * const grtyp,
    const int32_t * const ig1,
    const int32_t * const ig2,
    const int32_t * const ig3,
    const int32_t * const ig4,
    F2Cl lengrtyp
) {
    c_ezllwfgff(uullout, vvllout, latin, lonin, xlatingf, xloningf, ni, nj, grtyp, ig1, ig2, ig3, ig4);
}
