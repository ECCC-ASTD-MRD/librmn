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


***S/R XYFLL - COMPUTES THE GRID CO-ORDINATES OF A POINT
*
      SUBROUTINE XYFLL(X,Y,DLAT,DLON,D60,DGRW,NHEM)
      use rmn_base_const, only: dgtord, south

*         - COMPUTES THE GRID CO-ORDINATES MEASURED FROM THE POLE OF A
*           POINT, GIVEN THE LATITUDE AND LONGITUDE IN DEGREES.
*ARGUMENTS
*   OUT   - X    - X-CO-ORDINATE OF THE POINT AS MEASURED WITH POLE
*                  AS ORIGIN
*         - Y    - Y-CO-ORDINATE OF THE POINT AS MEASURED WITH POLE
*                  AS ORIGIN
*   IN    - DLAT - LATITUDE IN DEGREES (-90 TO +90, POSITIVE N)
*         - DLON - LONGITUDE IN DEGREES (-180 TO +180, POSITIVE E)
*         - D60  - GRID LENGTH (IN METRES) OF THE POLAR STEREOGRAPHIC
*                  GRID AT 60 DEGREES
*         - DGRW - ORIENTATION OF GREENWICH MERIDIAN WITH RESPECT TO
*                  THE GRID (IN DEGREES)
*         - NHEM - 1 FOR NORTHERN HEMISPHERE
*                  2 FOR SOUTHERN HEMISPHERE
*
*NOTES    - THE COMPANION ROUTINE LLFXY, WHICH COMPUTES THE LATITUDE
*           AND LONGITUDE GIVEN THE GRID-COORDINATES, IS ALSO AVAILABLE.
*
*------------------------------------------------------------------------------
*
*     * 1.866025=(1+SIN60),   6.371E+6=EARTH RADIUS IN METERS.
*
      RE = 1.866025 * 6.371E+6 / D60

      GLON = DLON
      IF(NHEM .EQ. SOUTH) GLON = -DLON
      GLAT=DLAT
      IF(NHEM .EQ. SOUTH) GLAT = -DLAT

      RLON = DGTORD * (GLON + DGRW)
      RLAT = DGTORD * GLAT
      SINLAT = SIN(RLAT)
      R = RE * SQRT((1.0 - SINLAT) / (1.0 + SINLAT))
      X = R * COS(RLON)
      Y = R * SIN(RLON)
      END

