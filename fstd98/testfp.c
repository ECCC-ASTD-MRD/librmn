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
#include <stdlib.h>
#include <time.h>
main()
{
 typedef  double tata;
  double *a;
register  double x,y,z,t,u;
time_t now;
struct tm *date;
char s[80];
int i,j;

x=1.2;
y=1.3;
z=1.4 ;
t=1.5;
u=1.6;
a = (double  *) malloc(16000*sizeof(tata));
for (j=0;j<8000;j++){
a[j]=0.0;
}
i=sizeof(tata);
printf("length of tata is %ld\n",i);
time(&now);
date=localtime(&now);
strftime(s,80,"%c",date);
printf("start at:%s\n",s);
for (i=0;i<320000;i++){
for (j=0;j<1000;j++){
   a[j]=a[j]+(((x*y)+z)*x+t)*x+i;
}
}
y=2.345;
printf("point 2,y= %G\n",a[0]);
time(&now);
date=localtime(&now);
strftime(s,80,"%c",date);
printf("end at:%s after 112 Mflops\n",s);
}
