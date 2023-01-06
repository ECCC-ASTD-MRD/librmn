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

/* auteur: M. valin */
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>

#include <App.h>
#include <rmn/rpnmacros.h>

static int fd = -1;
void f77name(open_status_file)() {
    if ( (fd=open("./status.dot",O_RDWR+O_CREAT,0755)) <0 ) {
        Lib_Log(APP_LIBRMN,APP_ERROR,"%s: Cannot open status file\n",__func__);
        exit(1);
    }
}


void f77name(write_status_file)(char *msg, F2Cl lmsg) {
    write(fd, msg, lmsg);
}


void f77name(close_status_file)() {
    close(fd);
}
