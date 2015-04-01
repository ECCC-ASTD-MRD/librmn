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
#include <string.h>
// extern int c_fst_version();
main ()
{
 int v;
 char nom1[4] = {"AB  "};
 char nom2[4] = {"AB"};
 char **n1, **n2;
 *n1 = nom1;
 *n2 = nom2;

// v = c_fst_version();
 printf("version =%d\n",v);
 if (strncmp(nom1,nom2,4) == 0) printf("comparaison avec blancs, nom1=nom2\n");
 printf("nom1 strippe-->%s<--\n",strtok(nom1," "));
 if (strncmp(strtok(nom1," "),strtok(nom2," "),4) == 0) printf("comparaison strippe nom1=nom2\n");
 printf("a la fin nom1-->%s<-- et nom2-->%s<--\n",nom1,nom2);
}
