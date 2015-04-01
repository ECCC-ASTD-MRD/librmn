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

#include "ezscint.h"
#include "ez_funcdef.h"


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
wordint f77name(ezsetval)(char *option, ftnfloat *fvalue, F2Cl lenoption)
{
   wordint i, icode;
   wordint longueur_option, longueur_value;

   char local_opt[32];
   wordint l_lenoption;

   l_lenoption = (wordint) lenoption;
   memset(local_opt, (unsigned char)'\0', 32);
   longueur_option = f77name(longueur)(option, l_lenoption);
   longueur_option = longueur_option < 32 ? longueur_option : 31;

   for (i=0; i < longueur_option; i++)
     {
     local_opt[i] = option[i];
     }

   icode = c_ezsetval(local_opt, *fvalue);
   return icode;

}

wordint c_ezsetval(char *option, ftnfloat fvalue)
{
   char local_opt[32];
   wordint i;
   
   strcpy(local_opt, option);
   
   for (i=0; i < strlen(local_opt); i++)
     local_opt[i] = (char) tolower((int)local_opt[i]);
   
   if (0 == strcmp(local_opt, "extrap_value"))
      {
      groptions.valeur_extrap = fvalue;
      }
   
   return 0;
}

wordint c_ezsetval2(char *option, ftnfloat *fvalue)
{
   char local_opt[32];
   wordint i;
   
   strcpy(local_opt, option);
   
   for (i=0; i < strlen(local_opt); i++)
     local_opt[i] = (char) tolower((int)local_opt[i]);
   
   if (0 == strcmp(local_opt, "extrap_value"))
      {
      groptions.valeur_extrap = *fvalue;
      }
   
   return 0;
}
