!/* RMNLIB - Library of useful routines for C and FORTRAN programming
! * Copyright (C) 1975-2001  Division de Recherche en Prevision Numerique
! *                          Environnement Canada
! *
! * This library is free software; you can redistribute it and/or
! * modify it under the terms of the GNU Lesser General Public
! * License as published by the Free Software Foundation,
! * version 2.1 of the License.
! *
! * This library is distributed in the hope that it will be useful,
! * but WITHOUT ANY WARRANTY; without even the implied warranty of
! * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
! * Lesser General Public License for more details.
! *
! * You should have received a copy of the GNU Lesser General Public
! * License along with this library; if not, write to the
! * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
! * Boston, MA 02111-1307, USA.
! */

!> \file

!> Computes the grid co-ordinates measured from the pole of a point, given the latitude and longitude in degrees
subroutine xyfll(x, y, dlat, dlon, d60, dgrw, nhem)
    implicit none

    !> X-coordinate of the point as measured with pole as origin
    real, intent(out) :: x
    !> Y-coordinate of the point as measured with pole as origin
    real, intent(out) :: y
    !> Latitude in degrees (-90 to +90, positive n)
    real, intent(in) :: dlat
    !> Longitude in degrees (-180 to +180, positive e)
    real, intent(in) :: dlon
    !> Grid length (in metres) of the polar stereographic grid at 60 degrees
    real, intent(in) :: d60
    !> Orientation of greenwich meridian with respect to the grid (in degrees)
    real, intent(in) :: dgrw
    !> Hemisphere: 1 for North, 2 for South
    integer, intent(in) :: nhem

    !> \sa llfxy

#include "pi.cdk"

    real :: glat, glon, r, re, rlat, rlon, sinlat

    ! 1.866025 = (1+sin60),   6.371e+6 = earth radius in meters.
    re = 1.866025 * 6.371e+6 / d60

    glon = dlon
    if (nhem == 2) glon = -dlon
    glat = dlat
    if (nhem == 2) glat = -dlat

    rlon = dgtord * (glon + dgrw)
    rlat = dgtord * glat
    sinlat = sin(rlat)
    r = re * sqrt((1.0 - sinlat) / (1.0 + sinlat))
    x = r * cos(rlon)
    y = r * sin(rlon)
end