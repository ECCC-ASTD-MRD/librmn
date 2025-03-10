*/* RMNLIB - Library of useful routines for C and FORTRAN programming
* * Copyright (C) 1975-2001  Division de Recherche en Prevision Numerique
* *                          Environnement Canada
* *
* * This library is free software; you can redistribute it and/or
* * modify it under the terms of the GNU Lesser General Public
* * License as published by the Free Software Foundation,
* * version 2.1 of the License.
* *
* * This library is distributed in the hope that it will be useful,
* * but WITHOUT ANY WARRANTY; without even the implied warranty of
* * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
* * Lesser General Public License for more details.
* *
* * You should have received a copy of the GNU Lesser General Public
* * License along with this library; if not, write to the
* * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
* * Boston, MA 02111-1307, USA.
* */


!> Compute latitude and longitude of each point of a gaussian grid
      SUBROUTINE GRGG(XLAT,XLON,NI,NJ,ROOTS,MODE)
      use rmn_base_const, only: dgtord, global
      REAL XLAT(NI,NJ), XLON(NI,NJ)
      REAL ROOTS(NJ)

*ARGUMENTS
*   OUT   - XLAT  - CHAMP DE LATITUDES (NI,NJ). (NIVEAU 1 OU 2)
*   OUT   - XLON  - CHAMP DE LONGITUDES (NI,NJ). (NIVEAU 1 OU 2)
*   IN    - NI    - NOMBRE DE POINTS PAR CERCLE DE LATITUDE.
*   IN    - NJ    - NOMBRE DE CERCLES DE LONGITUDE.
*   IN    - ROOTS - TABLE SERVANT A CALCULER LES RACINES DES
*                   POLYNOMES DE LEGENDRE SERVANT A DEFINIR
*                   LA LATITUDE DES NOEUDS DE LA GRILLE GAUSSIENNE.
*   IN    - MODE  - SORTE DE GRILLE UTILISEE
*                   = 0, GLOBALE
*                   = 1, NORD
*                   = 2, SUD

      DLON = 360. / NI
      NPOLY = NJ

      IF (MODE .NE. GLOBAL) NPOLY = NJ*2
      CALL DGAUSS(NPOLY,ROOTS,MODE)

      DO J = 1, NJ
        XLA = 90. - RDTODG * ACOS(ROOTS(NJ+1-J))
        DO I = 1, NI
          XLAT(I,J) = XLA
          XLON(I,J) = (I-1) * DLON
        END DO
      END DO
      END
