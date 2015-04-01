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
#include <stdlib.h>
#include <stdio.h>
#include <fcntl.h>
static char line[8192];

void f77name(resetenv)()
{
  FILE *stream;
  char *var = line;
  char *newvar;
  int lng;

  stream = fopen(".resetenv","r");
  if (stream == NULL) {
    fprintf(stdout,"Debug resetenv: fichier .resetenv inexistant\n");
    return;
  }
  while (fgets(var,2048,stream)) {
    lng = strlen(var);
    if (lng > 8192) {
      fprintf(stderr,
	      "*** ERREUR: resetenv, debordement du buffer lng=%d\n",lng);
      exit(22);
    }
    var[lng-1]='\0';
    if (putenv(var) < 0)
      perror("resetenv");
    var += lng;
  }
  /*   newvar = getenv("ARMNLIB");
  fprintf(stdout,"Debug ARMNLIB=%s\n",newvar); */
}
