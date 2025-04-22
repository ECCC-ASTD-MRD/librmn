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


!> Compute the components of the winds in the rotated system of coordinates from the winds in the rotated cartesian space
subroutine ez_cartauv(u, v, uvcart, lon, lat, ni, nj)
    use iso_fortran_env, only: real64
    implicit none

    !> E-W dimension of the grid
    integer, intent(in) :: ni
    !> N-S dimension of the grid
    integer, intent(in) :: nj
    !> Rotated winds in cartesian space
    real, intent(in) :: uvcart(3, ni * nj)
    !> Rotated component u of the wind
    real, intent(out) :: u(ni, nj)
    !> Rotated component v of the wind
    real, intent(out) :: v(ni, nj)
    !> Longitudes of the grid in the rotated system of coordinates
    real, intent(in) :: lon(ni, nj)
    !> Latitudes of the grid in the rotated system of coordinates
    real, intent(in) :: lat(ni, nj)

    !> \ingroup ezscint

    real(kind = real64), parameter :: dar = acos(-1.0) / 180.0

    integer :: i, j, k
    real(kind = real64) :: a, b, c, d, e, f

    k = 0

    do j = 1, nj
        do i = 1, ni
            k      = k+1
            a      = cos(dar * lon(i, j))
            b      = sin(dar * lon(i, j))
            e      = cos(dar * lat(i, j))
            f      = sin(dar * lat(i, j))
            u(i, j) = (uvcart(2, k) * a) - (uvcart(1, k) * b)
            c      = (uvcart(1, k) * a) + (uvcart(2, k) * b)
            d      = sqrt(c**2 + uvcart(3, k)**2 )
            v(i, j) = sign(real(d), real((uvcart(3, k) * e) - (c * f)))
        end do
    end do
end
