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
#include <string.h>
#include <unistd.h>

#include <App.h>
#include <rmn/rpnmacros.h>


int32_t f77name(remove_c)(
    const char * const filename,
    F2Cl lng1)
{
    char fname[256];

    if (lng1 > 256) {
        Lib_Log(APP_LIBRMN,APP_ERROR,"%s: file name > 256 char\n",__func__);
        return((int32_t) -1);
    }
    int llng1 = lng1;
    while (filename[llng1 - 1] == ' ' && llng1 > 0) llng1--;
    strncpy(fname, filename, llng1);
    fname[llng1] = '\0';
    int rcode = unlink(fname);
    if (rcode == -1) perror("remove_c error");
    return (int32_t)rcode;
}
