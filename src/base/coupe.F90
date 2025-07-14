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


!> Transverse cut of a bi-dimensional field
subroutine coupe(r, np, z, ni, nj, x1, y1, x2, y2, ok)
    implicit none

    !> Number of points in the transverse cut
    integer, intent(in) :: np
    !> X dimension of field z
    integer, intent(in) :: ni
    !> Y dimension of field z
    integer, intent(in) :: nj
    !> Output field
    real, intent(out) :: r(np)
    !> Input field
    real, intent(in) :: z(ni, nj)
    !> X coordinate of the left corner of the cut
    real, intent(in) :: x1
    !> Y coordinate of the left corner of the cut
    real, intent(in) :: y1
    !> X coordinate of the right corner of the cut
    real, intent(in) :: x2
    !> Y coordinate of the right corner of the cut
    real, intent(in) :: y2
    !> True if the coordinates are within the z fields limits', false otherwise
    logical, intent(out) :: ok

    real, external :: bilin

    integer :: i
    real :: deltax, deltay, X, Y

    ok = .true.
    deltax = (x2 - x1) / (np - 1)
    deltay = (y2 - y1) / (np - 1)

    do i = 1, np
        x = x1 + (i - 1) * deltax
        y = y1 + (i - 1) * deltay
        r(i) = bilin(z, ni, nj, x, y, ok)
    end do
end
