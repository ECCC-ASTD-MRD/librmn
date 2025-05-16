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


!> Covert wind from u, v components to speed and direction
subroutine rgll2gd(spdo, psio, xlon, li, lj, grtyp, ig1, ig2, ig3, ig4)
    use rmn_base_const, only: dgtord
    implicit none

    !> First dimension of the spgo, psio fields
    integer, intent(in) :: li
    !> Second dimension of the spgo, psio fields
    integer, intent(in) :: lj
    !> Must contain the speed when called. Will contain to U component on return.
    real, intent(inout) :: spdo(li, lj)
    !> Must contain the direction when called. Will contain to V component on return.
    real, intent(inout) :: psio(li, lj)
    real, intent(in) :: xlon(li, lj)
    !> Grid type
    character(len = 1), intent(in) :: grtyp
    !> First integer grid descriptor
    integer, intent(in) :: ig1
    !> Second integer grid descriptor
    integer, intent(in) :: ig2
    !> Third integer grid descriptor
    integer, intent(in) :: ig3
    !> Forth integer grid descriptor
    integer, intent(in) :: ig4

    !> \ingroup ezscint

    external cigaxg

    integer i, j
    real psi, u, v
    real xg1, xg2, xg3, xg4

    if (grtyp == 'N') then
        call cigaxg(grtyp, xg1, xg2, xg3, xg4, ig1, ig2, ig3, ig4)

        do i = 1, li
            do j = 1, lj
                psi = xlon(i, j) + xg4 - psio(i, j)
                u = cos(psi * dgtord) * spdo(i, j)
                v = sin(psi * dgtord) * spdo(i, j)
                spdo(i, j) = u
                psio(i, j) = v
            end do
        end do
        return
    endif

    if (grtyp == 'S') then
        call cigaxg(grtyp, xg1, xg2, xg3, xg4, ig1, ig2, ig3, ig4)
        do i = 1, li
            do j = 1, lj
                psi = 180.0 - xlon(i, j) + xg4 - psio(i, j)
                u = cos(psi * dgtord) * spdo(i, j)
                v = sin(psi * dgtord) * spdo(i, j)
                spdo(i, j) = u
                psio(i, j) = v
            end do
        end do
        return
    endif

    if (grtyp == 'A' .or. grtyp == 'B' .or. grtyp == 'G' .or. grtyp == 'L') then
        do i = 1, li
            do j = 1, lj
                psi = 270.0 - psio(i, j)
                u = cos(psi * dgtord) * spdo(i, j)
                v = sin(psi * dgtord) * spdo(i, j)
                spdo(i, j) = u
                psio(i, j) = v
            end do
        end do
    endif
end
