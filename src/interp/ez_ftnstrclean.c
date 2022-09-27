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

#include <string.h>

#include <rmn/ezscint.h>
#include "ez_funcdef.h"


//! Remove spaces around a string
int32_t ftnstrclean(
    char str[],
    int32_t lenstr
){
    int32_t leadingBlanks = 0;
    while (str[leadingBlanks] == ' ' && leadingBlanks < lenstr) leadingBlanks++;

    if (leadingBlanks != 0) {
        strcpy(str, str + leadingBlanks);
    }

    int32_t jinit = lenstr - leadingBlanks - 1;
    int32_t traillingBlanks = jinit;
    while (str[traillingBlanks] == ' ' && traillingBlanks >= 0) traillingBlanks--;

    if (traillingBlanks != jinit) {
        str[traillingBlanks + 1] = '\0';
    }

    return 0;
}
