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
#include <stdlib.h>

#include <rmn/rpnmacros.h>

#define BUFSZ 4096

// Prototype for Fortran functions declared in ftn_prog_args.f90
#define GETARG f77_name(f_getarg)
void GETARG(int * pos, const char * val, F2Cl valLen);
int iargc();


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


#include <rmn/rpnmacros.h>
f77name(true_main)(int argc, **argv) {
 . . . .
 . . . .
}

  fmain2cmain calls FORTRAN library modules iargc  and getarg
  to get the number of program arguments and the argument strings

*/


void f77name(fmain2cmain)(void (*the_main)() ) {
    // get number of arguments
    int argc = 1 + iargc();
    char *argv[BUFSZ];
    char buffer[BUFSZ];

    argv[argc] = 0;

    // get arg[0] thru arg[argc-1]
    for (int i = 0; i < argc ; i++) {
        // get string for argument i
        GETARG(&i, buffer, BUFSZ);

        // get rid of trailing spaces
        int j = BUFSZ - 1;
        while ((j >= 0) && (buffer[j] == ' ')) {
            buffer[j] = 0;
            j--;
        }
        // copy into argument pointer list
        strcpy(argv[i] = malloc(1 + strlen(buffer)) , buffer);
    }

#ifdef SELFTEST
    // Call test main
    c_main(argc, argv);
#else
    // call actual C "main" program
    (*the_main)(argc, argv);
#endif
}
