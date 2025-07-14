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


!> Computes the coriolis parameter
subroutine coriol(ff, d60, pi, pj, ni, nj)
    implicit none

    !> Output field X dimension
    integer, intent(in) :: ni
    !> Output field Y dimension
    integer, intent(in) :: nj
    !> Coriolis parameter
    real, intent(out) :: ff(ni, nj)
    !> Grid spacing at 60 degrees of latitude in metres
    real, intent(in) :: d60
    !> X coordinate of pole relative to bottom left-hand corner of grid
    real, intent(in) :: pi
    !> Y coordinate of pole relative to bottom left-hand corner of grid
    real, intent(in) :: pj

    !> Computes the coriolis parameter at all points of a grid of
    !> uniform mesh-length. The grid is assumed to be superimposed
    !> on a polar stereographic projection of the earth true at
    !> latitude 60 degrees.

    ! - sin(lat) = (re2-r2) / (re2+r2)
    ! - re = (earth radius) * (1+sin(60))/d60
    ! - mean earth radius = 6371 km.
    ! - cf = 2. * omega
    ! - note that the pole need not be located on a grid point

    real, parameter :: cf = 1.458e-4

    real :: r2, re, re2, x, y, y2
    integer :: i, j

    re = 1.866025 * 6.371e6 / d60
    re2 = re * re

    do j = 1, nj
        y = float(j) - pj
        y2 = y * y

        do i = 1, ni
            x = float(i) - pi
            r2 = x * x + y2
            ff(i, j) = cf * (re2 - r2) / (re2 + r2)
        end do
    end do
end
