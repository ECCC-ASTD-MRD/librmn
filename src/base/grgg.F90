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


!> Compute latitude and longitude of each point of a gaussian grid
subroutine grgg(xlat, xlon, ni, nj, roots, mode)
    use rmn_base_const, only: global, rdtodg
    implicit none

    !> Number of points on the X axis
    integer, intent(in) :: ni
    !> Number of points on the Y axis
    integer, intent(in) :: nj
    !> Latitude field
    real, intent(out) :: xlat(ni, nj)
    !> Longitude field
    real, intent(out) :: xlon(ni, nj)
    !> Legendre polynomial roots used to compute the latitude of points of gaussian grids
    real, intent(out) :: roots(nj)
    !> Area on which to operate (GLOBAL, NORTH, SOUTH)
    integer, intent(in) :: mode

    external :: dgauss

    integer :: i, j, npoly
    real :: dlon, xla

    dlon = 360.0 / ni

    if (mode /= global) then
        npoly = nj * 2
    else
        npoly = nj
    end if
    call dgauss(npoly, roots, mode)

    do j = 1, nj
        xla = 90.0 - rdtodg * acos(roots(nj + 1 - j))
        do i = 1, ni
            xlat(i, j) = xla
            xlon(i, j) = (i - 1) * dlon
        end do
    end do
end
