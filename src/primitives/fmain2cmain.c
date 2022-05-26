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

#include <rpnmacros.h>
#include <string.h>
#include <stdlib.h>

#if defined(HP)
  #define IARGC iargc_
  #define GETARG getarg_
#else
  #define IARGC f77_name(f_iargc)
  #define GETARG f77_name(f_getarg)
#endif

#ifdef SELFTEST
c_main(int argc, char **argv) {
    while (argc--) {
        printf("Argument %d = :%s:\n", argc, argv[argc]);
    }
}
#endif

/*
   fmain2cmain is a FORTRAN callable module used to load
   a C main program that uses FORTRAN modules from a
   library

   example:
        program bidon
        external liburp
        call fmain2cmain(true_main)
        stop
        end


#include <rpnmacros.h>
f77name(true_main)(int argc, **argv) {
 . . . .
 . . . .
}

  fmain2cmain calls FORTRAN library modules iargc  and getarg
  to get the number of program arguments and the argument strings

*/

int IARGC();

void f77name(fmain2cmain)(void (*the_main)() ) {
    // get number of arguments
    int argc = 1 + IARGC();
    char *argv[4096];
    char buffer[4096];

    argv[argc] = 0;

    // get arg[0] thru arg[argc-1]
    for (int i = 0; i < argc ; i++) {
        // get string for argument i
        GETARG(&i, buffer, 4096);

        // get rid of trailing spaces
        int j = 4096 - 1;
        while ((j >= 0) && (buffer[j] == ' ')) {
            buffer[j] = 0;
            j--;
        }
        // copy into argument pointer list
        strcpy(argv[i] = malloc(1 + strlen(buffer)) , buffer);
    }

#ifdef SELFTEST
    // Call test main
    c_main(argc,argv);
#else
    // call actual C "main" program
    (*the_main)(argc,argv);
#endif
}
