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

#include "envrecv.h"

static char ch[129];
static char nullstr[1];

char * c_cmcrlog(fp,update)
FILE *fp;
int update;
{
long int frptr;
long int fwptr;
long int flptr;
int i;

nullstr[0] = '\0';

#ifndef __CYGWIN__
lockf(fileno(fp),F_LOCK,0L);
#endif
fscanf(fp,"%ld\n",&frptr);
fseek(fp,WRIT_PTR,SEEK_SET);
fscanf(fp,"%ld\n",&fwptr);
fseek(fp,LIMT_PTR,SEEK_SET);
fscanf(fp,"%ld\n",&flptr);

if ( fwptr == frptr)
	{
         fseek(fp,0L,SEEK_SET);
#ifndef __CYGWIN__
         lockf(fileno(fp),F_ULOCK,0L);
#endif
	 return(nullstr);
	}


fseek(fp,frptr,SEEK_SET);
fgets(ch,(int)flptr,fp);
ch[ENTRYSIZE] = '\0';

frptr = frptr +  ENTRYSIZE;
if ((frptr <= fwptr)&& (update)) { 
   fseek(fp,READ_PTR,SEEK_SET);
   fprintf(fp,"%-19ld\n",frptr);
}

fseek(fp,0L,SEEK_SET);
#ifndef __CYGWIN__
lockf(fileno(fp),F_ULOCK,0L);
#endif

return (ch);
}
