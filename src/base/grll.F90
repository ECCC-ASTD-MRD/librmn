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


!> Compute the latitude and longitude of a lat-lon grid
subroutine grll(xlat, xlon, ni, nj, xla0, xlo0, dla0, dlo0)
    implicit none

    !> Number of points per latitude circle
    integer, intent(in) :: ni
    !> Number of latitude circles
    integer, intent(in) :: nj
    !> Latitudes
    real, intent(out) :: xlat(ni, nj)
    !> Longitudes
    real, intent(out) :: xlon(ni, nj)
    !> Lower left corner latitude (degrees)
    real, intent(in) :: xla0
    !> Lower left corner longitude (degrees)
    real, intent(in) :: xlo0
    !> Latitude spacing (degrees)
    real, intent(in) :: dla0
    !> Longitude spacing (degrees)
    real, intent(in) :: dlo0

    integer :: i, j
    real :: xla

    do j = 1, nj
        xla = xla0 + (j - 1) * dla0

        do i = 1, ni
            xlat(i, j) = xla
            xlon(i, j) = amod(xlo0 + (i - 1) * dlo0, 360.0)
        end do
    end do
end
