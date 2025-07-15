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


!> Bi-linear interpolation
real function bilin(z, ni, nj, x, y, ok)
    implicit none

    !> X dimension of field z
    integer, intent(in) :: ni
    !> Y dimension of field z
    integer, intent(in) :: nj
    !> Input field
    real, intent(in) :: z(ni, nj)
    !> X coordinates of the point where to interpolate
    real, intent(in) :: x
    !> Y coordinates of the point where to interpolate
    real, intent(in) :: y
    !> True if the coordinates are within the limit of the z field, false otherwise
    logical, intent(inout) :: ok

    integer :: i, j
    real :: dx, dy, z1, z2

    ok = ok .and. x >= 1 .and. x <= ni .and. y >= 1 .and. y <= nj
    if (.not. ok) bilin = 0.0
    if (.not. ok) return

    i = int(x)
    j = int(y)

    if (i == ni) i = ni - 1
    if (j == nj) j = nj - 1
    dx = x - i
    dy = y - j

    z1 = z(i, j) + dy * (z(i, j + 1) - z(i, j))
    z2 = z(i + 1, j) + dy * (z(i + 1, j + 1) -  z(i + 1, j))
    bilin = z1 + dx * (z2 - z1)
end
