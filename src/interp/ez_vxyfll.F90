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


!> Compute the grid coordinates measured from the pole of a point, given the latitude and longitude in degrees
subroutine ez_vxyfll(x, y, dlat, dlon, npts, d60, dgrw, pi, pj, nhem)
    use iso_fortran_env, only: real64
    use rmn_base_const, only: dgtord, nord, sud
    implicit none

    integer, intent(in) :: npts
    !> X coordinate of the point as measured with pole as origin
    real, intent(out) :: x(npts)
    !> Y coordinate of the point as measured with pole as origin
    real, intent(out) :: y(npts)
    !> Latitude in degrees (-90 to +90, positive n)
    real, intent(in) :: dlat(npts)
    !> Longitude in degrees (-180 to +180, positive e)
    real, intent(in) :: dlon(npts)
    !> Grid length (in metres) of the polar stereographic grid at 60 degrees
    real, intent(in) :: d60
    !> Orientation of greenwich meridian with respect to the grid (in degrees)
    real, intent(in) :: dgrw
    real, intent(in) :: pi
    real, intent(in) :: pj
    ! Hemisphere : 1 = north, 2 = south
    integer, intent(in) :: nhem

    !> \ingroup ezscint

    !> \see llfxy which computes the latitude and longitude given the grid-coordinates

    real(kind = real64) :: re, rlon, rlat, sinlat, r
    integer :: i

    re = 1.866025d0 * 6.371d+6 / d60
    if (nhem .eq. NORD) then
        do i = 1, npts
            rlon = dgtord * (dlon(i) + dgrw)
            rlat = dgtord * dlat(i)
            sinlat = sin(rlat)
            r = re * sqrt((1.d0 - sinlat) / (1.d0 + sinlat))
            x(i) = r * cos(rlon) + pi
            y(i) = r * sin(rlon) + pj
        end do
        return
    endif

    if (nhem .eq. SUD) then
        do i = 1, npts
            rlon = dlon(i)
            if (rlon .gt. 180.0d0) rlon = rlon - 360.0d0
            rlon = dgtord * ( - rlon + dgrw)
            rlat = dgtord * ( - dlat(i))
            sinlat = sin(rlat)
            r = re * sqrt((1.d0 - sinlat) / (1.d0 + sinlat))
            x(i) = r * cos(rlon) + pi
            y(i) = r * sin(rlon) + pj
        end do
    endif
end
