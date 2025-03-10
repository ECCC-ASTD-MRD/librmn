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


!> Convert speed and psi components to u, v according to the grid type
subroutine ez_gdwfllw(z1, z2, xlon, li, lj, grtyp, ig1, ig2, ig3, ig4)
    use rmn_base_const, only: dgtord
    implicit none

    !> First dimension of the spd and psi fields
    integer, intent(in) :: li
    !> Second dimension of the spd and psi fields
    integer, intent(in) :: lj
    !> Wind speed on input, u component as output
    real, intent(inout) :: z1(li, lj)
    !> Wind direction on input, v component as output
    real, intent(inout) :: z2(li, lj)
    !> 
    real, intent(in) :: xlon(li, lj)
    !> Grid type
    character(len = 1), intent(in) :: grtyp
    !> First integer grid parameter
    integer, intent(in) :: ig1
    !> Second integer grid parameter
    integer, intent(in) :: ig2
    !> Third integer grid parameter
    integer, intent(in) :: ig3
    !> Fourth integer grid parameter
    integer, intent(in) :: ig4

    external cigaxg

    integer :: i, j
    real :: psi, u, v
    real :: xg1, xg2, xg3, xg4
    real :: x1(2 * li * lj), y1(2 * li * lj), lat(2 * li * lj)

    if (grtyp .eq. '!') then
        call ez_lamb_gdwfllw(z1, z2, xlon, li, lj, grtyp, ig1, ig2, ig3, ig4, x1, y1, lat)
    endif

    if (grtyp .eq. 'N') then
        call cigaxg(grtyp, xg1, xg2, xg3, xg4, ig1, ig2, ig3, ig4)

        do j = 1, lj
            do i = 1, li
                psi = xlon(i, j) + xg4 - z2(i, j)
                u = cos(psi * dgtord) * z1(i, j)
                v = sin(psi * dgtord) * z1(i, j)
                z1(i, j) = u
                z2(i, j) = v
            enddo
        enddo
        return
    endif

    if (grtyp .eq. 'S') then
        call cigaxg(grtyp, xg1, xg2, xg3, xg4, ig1, ig2, ig3, ig4)
        do j = 1, lj
            do i = 1, li
                psi = 180.0 - xlon(i, j) + xg4 - z2(i, j)
                u = cos(psi * dgtord) * z1(i, j)
                v = sin(psi * dgtord) * z1(i, j)
                z1(i, j) = u
                z2(i, j) = v
            enddo
        enddo
        return
    endif

    if (grtyp .eq. 'A' .or. grtyp .eq. 'B' .or. grtyp .eq. 'G' .or. grtyp .eq. 'L') then
        do j = 1, lj
            do i = 1, li
                psi = 270.0 - z2(i, j)
                u = cos(psi * dgtord) * z1(i, j)
                v = sin(psi * dgtord) * z1(i, j)
                z1(i, j) = u
                z2(i, j) = v
            enddo
        enddo
        return
    endif
end
