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


wordint c_ezfreegridset(wordint selectedset)
{
   if (gridset[selectedset].x)
      {
      free(gridset[selectedset].x);
      gridset[selectedset].x  = NULL;
      }

   if (gridset[selectedset].y)
      {
      free(gridset[selectedset].y);
      gridset[selectedset].y  = NULL;
      }

   if (gridset[selectedset].gemin.lat_rot)
      {
      free(gridset[selectedset].gemin.lat_rot);
      free(gridset[selectedset].gemin.lon_rot);
      free(gridset[selectedset].gemin.sinlat_rot);
      free(gridset[selectedset].gemin.coslat_rot);
      free(gridset[selectedset].gemin.sinlon_rot);
      free(gridset[selectedset].gemin.coslon_rot);
      free(gridset[selectedset].gemin.sinlat_true);
      free(gridset[selectedset].gemin.coslat_true);
      free(gridset[selectedset].gemin.sinlon_true);
      free(gridset[selectedset].gemin.coslon_true);
      memset(&gridset[selectedset].gemin, (int) NULL, sizeof(_geminfo));
      }

   if (gridset[selectedset].gemout.lat_rot)
      {
      free(gridset[selectedset].gemout.lat_rot);
      free(gridset[selectedset].gemout.lon_rot);
      free(gridset[selectedset].gemout.sinlat_rot);
      free(gridset[selectedset].gemout.coslat_rot);
      free(gridset[selectedset].gemout.sinlon_rot);
      free(gridset[selectedset].gemout.coslon_rot);
      free(gridset[selectedset].gemout.sinlat_true);
      free(gridset[selectedset].gemout.coslat_true);
      free(gridset[selectedset].gemout.sinlon_true);
      free(gridset[selectedset].gemout.coslon_true);
      memset(&gridset[selectedset].gemout, (int) NULL, sizeof(_geminfo));
      }


  return 0;
}
