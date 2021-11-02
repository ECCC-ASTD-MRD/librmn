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
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <rpnmacros.h>


//! Get environement variable value
void f77name(getenvc) (
    //! [in] Name of the environment variable to get
    char *name,
    //! [out] Value of the environement variable if found.  Blank otherwise.
    char *value,
    //! [in] Name length
    F2Cl nameLen,
    //! [in] Value length
    F2Cl valueLen
) {
    int tempLen = nameLen + 1;

    // Transfert de name dans une chaine
    char *temp = (char *) malloc( tempLen ) ;

    int realNameLen;
    for ( realNameLen = 0 ; realNameLen < nameLen && name[realNameLen] != ' ' ; realNameLen++ ) {
        temp[realNameLen] = name[realNameLen];
    }
    temp[realNameLen] = '\0';

    char *envVal = getenv( temp );
    free( temp );

    for ( int i = 0 ; i < valueLen ; i++ ) {
        value[i] = ' ';
    }

    if (envVal != NULL) {
        if ( tempLen != 1 ) {
            //! \todo Throw a warning when the value length is to small for the environement variable's value

            int envValLen = strlen(envVal);
            for ( int i = 0; i < envValLen && i < valueLen; i++ ) {
                value[i] = envVal[i];
            }
        }
    } else {
        printf("getnevc - %s Not found in environement!\n", temp);
    }
}
