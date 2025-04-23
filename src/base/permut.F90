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


!> Mirror matrix along second dimension
subroutine permut(z, ni, nj)
    implicit none

    !> First dimension
    integer, intent(in) :: ni
    !> Second dimension
    integer, intent(in) :: nj
    !> Matrix
    real, intent(inout) :: z(ni, nj)

    integer :: i, j
    real :: t

    do j = 1, nj / 2
        do i = 1, ni
            t = z(i, nj + 1 - j)
            z(i, nj + 1 - j) = z(i, j)
            z(i, j) = t
        end do
    end do
end
