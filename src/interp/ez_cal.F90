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


!> Compute spherical coordinates from cartesian coordinates
subroutine ez_cal(lon, lat, xyz, n)
    implicit none

    integer, intent(in) :: n
    real, intent(in) :: xyz(3, n)
    real, intent(out) :: lon(n), lat(n)

    !> \ingroup ezscint

    real, parameter :: rad = 180.0 / acos(-1.00)
    integer i

        do i = 1, n
            lat(i) = asin(max(-1.00, min(1.0, xyz(3, i)))) * rad
            lon(i) = atan2( xyz(2, i), xyz(1, i) ) * rad
            lon(i) = amod( lon(i), 360.0 )
            if (lon(i) < 0.0) lon(i) = lon(i) + 360.0
    end do
end 
