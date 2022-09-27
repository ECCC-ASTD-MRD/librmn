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


#include <ctype.h>
#include <string.h>

#include <rmn/ftnStrLen.h>
#include <rmn/ezscint.h>

#include "ez_funcdef.h"

int32_t f77name(ezsetfval)(char *option, float *fvalue, F2Cl lenoption);
int32_t f77name(ezsetval)(char *option, float *fvalue, F2Cl lenoption);
int32_t f77name(ezsetival)(char *option, int32_t *ivalue, F2Cl lenoption);
int32_t c_ezsetfval(char *option, float fvalue);
int32_t c_ezsetval(char *option, float fvalue);
int32_t c_ezsetival(char *option, int32_t ivalue);


int32_t f77name(ezsetfval)(char *option, float *fvalue, F2Cl llenoption)
   {
   int32_t i, icode;
   int32_t longueur_option;
   F2Cl lenoption=llenoption;

   char local_opt[32];

   memset(local_opt, (unsigned char)'\0', 32);
   longueur_option = ftnStrLen(option, lenoption);
   longueur_option = longueur_option < 32 ? longueur_option : 31;

   for (i=0; i < longueur_option; i++)
     {
     local_opt[i] = option[i];
     }

   icode = c_ezsetfval(local_opt, *fvalue);
   return icode;
}

int32_t f77name(ezsetval)(char *option, float *fvalue, F2Cl lenoption)
   {
   return f77name(ezsetfval)(option, fvalue, lenoption);
   }

int32_t f77name(ezsetival)(char *option, int32_t *ivalue, F2Cl llenoption)
{
   int32_t i, icode;
   int32_t longueur_option;
   F2Cl lenoption=llenoption;

   char local_opt[32];

   memset(local_opt, (unsigned char)'\0', 32);
   longueur_option = ftnStrLen(option, lenoption);
   longueur_option = longueur_option < 32 ? longueur_option : 31;

   for (i=0; i < longueur_option; i++)
     {
     local_opt[i] = option[i];
     }

   icode = c_ezsetival(local_opt, *ivalue);
   return icode;
}


int32_t c_ezsetfval(char *option, float fvalue)
   {
   char local_opt[32];
   int32_t i;

   strcpy(local_opt, option);

   for (i=0; i < strlen(local_opt); i++)
      {
      local_opt[i] = (char) tolower((int)local_opt[i]);
      }

   if (0 == strcmp(local_opt, "extrap_value"))
      {
      groptions.valeur_extrap = fvalue;
      }

   if (0 == strcmp(local_opt, "missing_gridpt_distance"))
      {
      groptions.msg_gridpt_dist = fvalue;
      }

   if (0 == strcmp(local_opt, "missing_distance_threshold"))
      {
      groptions.msg_dist_thresh = fvalue;
      }

   return 0;
   }

int32_t c_ezsetval(char *option, float fvalue)
   {
   return c_ezsetfval(option, fvalue);
   }


int32_t c_ezsetival(char *option, int32_t ivalue)
   {
   char local_opt[32];
   int32_t i;

   strcpy(local_opt, option);

   for (i=0; i < strlen(local_opt); i++)
     local_opt[i] = (char) tolower((int)local_opt[i]);

   if (0 == strcmp(local_opt, "weight_number"))
      {
      groptions.wgt_num = ivalue;
      }

    if (0 == strcmp(local_opt, "missing_points_tolerance"))
      {
      groptions.msg_pt_tol = ivalue;
      }

   if (0 == strcmp(local_opt, "subgridid"))
      {
      groptions.valeur_1subgrid = ivalue;
      }

   return 0;
   }

int32_t c_ezsetval2(char *option, float *fvalue)
{
   char local_opt[32];
   int32_t i;

   strcpy(local_opt, option);

   for (i=0; i < strlen(local_opt); i++)
     local_opt[i] = (char) tolower((int)local_opt[i]);

   if (0 == strcmp(local_opt, "extrap_value"))
      {
      groptions.valeur_extrap = *fvalue;
      }

   return 0;
}
