! RMNLIB - Library of useful routines for C and FORTRAN programming
! Copyright (C) 1975-2001  Division de Recherche en Prevision Numerique
!                          Environnement Canada
!
! This library is free software; you can redistribute it and/or
! modify it under the terms of the GNU Lesser General Public
! License as published by the Free Software Foundation,
! version 2.1 of the License.
!
! This library is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
! Lesser General Public License for more details.
!
! You should have received a copy of the GNU Lesser General Public
! License along with this library; if not, write to the
! Free Software Foundation, Inc., 59 Temple Place - Suite 330,
! Boston, MA 02111-1307, USA.


!> \file


!> Compute latitude and longitude of a point in a polar stereographic grid from grid coordinates measured from the pole
subroutine llfxy(dlat, dlon, x, y, d60, dgrw, nhem)
    use rmn_base_const, only: rdtodg
    implicit none

    !> Latitude in degrees (-90 to +90, positive n)
    real, intent(out) :: dlat
    !> Longitude in degrees (-180 to +180, positive e)
    real, intent(out) :: dlon
    !> X coordinates of the point with the pole as origin
    real, intent(in) :: x
    !> Y coordinates of the point with the pole as origin
    real, intent(in) :: y
    !> Gird spacing in meters at 60 degrees of latitude
    real, intent(in) :: d60
    !> Angle of the X axis in reference to the greenwich meridian (degrees)
    real, intent(in) :: dgrw
    !> Hemisphere: 1 for North, 2 for South
    integer, intent(in) :: nhem

    !> \see xyfll to compute the grif coordinates from the latitude and longitude

    real :: r2, re, re2

    re = 1.866025 * 6.371e+6 / d60
    re2 = re ** 2
    ! if point is at pole set coord to (0., 90.).
    dlat = 90.0
    dlon = 0.0
    if (x /= 0. .or. y /= 0.) then
        ! calculate longitude in map coordinates.
        if (x == 0.0) dlon = sign(90.0, y)
        if (x /= 0.0) dlon = atan(y / x) * rdtodg
        if (x < 0.0) dlon = dlon + sign(180.0, y)
        ! adjust longitude for grid orientation.
        dlon = dlon - dgrw
        if (dlon > +180.0) dlon = dlon - 360.0
        if (dlon < -180.0) dlon = dlon + 360.0
        ! calculate latitude.
        r2 = x ** 2 + y ** 2
        dlat = (re2 - r2) / (re2 + r2)
        dlat = asin(dlat) * rdtodg
    end if

    ! change signs if in southern hemisphere.
    if(nhem == 2) dlat = -dlat
    if(nhem == 2) dlon = -dlon
end
