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
#include <rpnmacros.h>
#include <stdio.h>
#include <errno.h>
#include <unistd.h>
void f77name(set_run_dir)(ftnword *mype)
{
char buffer[1024];
int pe=*mype;
sprintf(buffer,"./process/%d",pe);
if ( chdir(buffer) ) {   /* OOPS */
perror("set_run_dir: cannot change to specidied directory");
exit(1);
}
}
void f77name(set_run_dir_xy)(ftnword *mypex, ftnword *mypey)
{
char buffer[1024];
int pex=*mypex;
int pey=*mypey;
sprintf(buffer,"./process/%02d-%02d",pex,pey);
if ( chdir(buffer) ) {   /* OOPS */
fprintf(stderr,"cannot change to specidied directory:%s:\n",buffer);
perror("set_run_dir_xy: cannot change to specidied directory");
exit(1);
}
}

