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


!> Compute the winds in the cartesian space from the components
subroutine ez_uvacart(xyz, u, v, lon, lat, ni, nj)
    use iso_fortran_env, only: real64
    implicit none

    !> E-W dimension of the grid
    integer, intent(in) :: ni
    !> N-S dimension of the grid
    integer, intent(in) :: nj
    !> Unrotated U component of the wind
    real, intent(in) :: u(ni, nj)
    !> Unrotated V component of the wind
    real, intent(in) :: v(ni, nj)
    !> Unrotated winds in cartesian space
    real, intent(out) :: xyz(3, ni * nj)
    !> Grid longitudes in the unrotated system of coordinates
    real, intent(in) :: lon(ni, nj)
    !> Grid latitudes in the unrotated system of coordinates
    real, intent(in) :: lat(ni, nj)

    !> \ingroup ezscint

    integer :: i, j, k 
    real(kind = real64) :: a, b, c, d, dar

    dar = acos(-1.0) / 180.0
    k = 0

    do j = 1, nj
        do i = 1, ni
            k = k + 1
            a = sin(dar * lon(i, j))
            b = cos(dar * lon(i, j))
            c = sin(dar * lat(i, j))
            d = cos(dar * lat(i, j))
            xyz(1, k) = real( -(u(i, j) * a) - (v(i, j) * b * c) )
            xyz(2, k) = real( (u(i, j) * b) - (v(i, j) * a * c) )
            xyz(3, k) = real( v(i, j) * d )
        end do
    end do
end
