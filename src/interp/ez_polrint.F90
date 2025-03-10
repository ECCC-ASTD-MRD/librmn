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


subroutine ez_polrint(vpolnor, vpolsud, zi, ni, nj, grtyp, grref, hem, vecteur, ax, ay)
    implicit none

    integer, intent(in) :: ni
    integer, intent(in) :: nj
    real, intent(in) :: zi(ni, nj)
    !> Unused
    integer, intent(in) :: hem
    !> Unused
    real, intent(in) :: ax(ni)
    !> Unused
    real, intent(in) :: ay(nj)
    character(len = 1), intent(in) :: grtyp, grref
    logical, intent(in) :: vecteur
    real, intent(out) :: vpolnor, vpolsud

    real :: sum
    integer :: i

    if(vecteur) then
        return
    endif

    if(grtyp .eq. 'L' .or. grtyp .eq. 'N' .or. grtyp .eq. 'S' .or. grtyp .eq. '!' .or. (grtyp .eq. 'Z' .and. grref .ne. 'E')) then
        return
    endif

    if (grtyp .eq. 'B') then
        vpolnor = zi(1, nj)
        vpolsud = zi(1, 1)
        return
    endif

    if(grtyp .eq. 'A' .or. grtyp .eq. 'G' .or. (grtyp .eq. 'Z'.and.grref .eq. 'E')) then
        sum = 0.0
        do i = 1, ni
            sum = sum + zi(i, nj)
        enddo
        vpolnor = sum / (1.0 * ni)

        sum = 0.0
        do i = 1, ni
            sum = sum + zi(i, 1)
        enddo
        vpolsud = sum / (1.0 * ni)
    endif
end
