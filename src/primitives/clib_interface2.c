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
#include <glob.h>

#ifdef __linux__
#include <linux/limits.h>
#endif

#ifdef __APPLE__ && __MACH__
#include <sys/limits.h>
#endif

#include <rmn/rpnmacros.h>
#include <rmn/ftn2c_helper.h>

#define CLIB_OK    1
#define CLIB_ERROR -1

#define CLIB_F77NAME(a) f77_name(a##_schhide)

//! Get the list of files in PWD that match a pattern)
//! \return CLIB_OK on success, CLIB_ERROR otherwise
F77_INTEGER CLIB_F77NAME(clib_glob)(
    F77_CHARACTER *filelist,
    F77_INTEGER *nfiles,
    F77_CHARACTER *pattern,
    F77_INTEGER *maxnfiles,
    F2Cl filelist_length,
    F2Cl pattern_length
) {
    glob_t globbuf;
    char pattern_c[PATH_MAX];
    F77_INTEGER status = CLIB_ERROR;

    // Translate to C strings
    if (FTN2C_FSTR2CSTR(pattern, pattern_c, pattern_length, PATH_MAX) < 0) {
        return CLIB_ERROR;
    }

    // Call C function
    *nfiles = 0;
    if (!glob(pattern_c, GLOB_NOSORT, NULL, &globbuf)) {
        if ((F77_INTEGER)globbuf.gl_pathc <= *maxnfiles) {
            *nfiles = (F77_INTEGER)globbuf.gl_pathc;
            if (FTN2C_CSTR2FSTR_A(globbuf.gl_pathv, filelist, PATH_MAX, (int)filelist_length, (int)*nfiles) >= 0) {
                status = CLIB_OK;
            }
        }
    }
    globfree(&globbuf);
    return status;
}

