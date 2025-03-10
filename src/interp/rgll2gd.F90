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

    integer li, lj
    real spdo(li, lj), psio(li, lj), xlon(li, lj)
    character(len = 1) :: grtyp
    integer ig1, ig2, ig3, ig4

    external cigaxg

!arguments
!  in/out - spd   - a l'entree contient la vitesse du vent et
!                   a la sortie la composante u.
!  in/out - psi   - a l'entree contient la direction du vent et
!                   a la sortie la composante v.
!   in    - li    - premiere dimension des champs spd et psi
!   in    - lj    - deuxieme dimension des champs spd et psi
!   in    - igtyp  - type de grille (voir ouvrir)
!   in    - xg1   - ** descripteur de grille (reel),
!   in    - xg2   -    igtyp = 'n', pi, pj, d60, dgrw
!   in    - xg3   -    igtyp = 'l', lat0, lon0, dlat, dlon,
!   in    - xg4   -    igtyp = 'a', 'b', 'g', xg1 = 0. global,
!                                                 = 1. nord
!                                                 = 2. sud **

    integer i, j
    real psi, u, v
    real xg1, xg2, xg3, xg4

    if (grtyp .eq. 'N') then
        call cigaxg(grtyp, xg1, xg2, xg3, xg4, ig1, ig2, ig3, ig4)

        do i=1, li
            do j=1, lj
                psi =xlon(i, j)+xg4-psio(i, j)
                u = cos(psi*dgtord)*spdo(i, j)
                v = sin(psi*dgtord)*spdo(i, j)
                spdo(i, j) = u
                psio(i, j) = v
            end do
        end do
        return
    endif

    if (grtyp .eq. 'S') then
        call cigaxg(grtyp, xg1, xg2, xg3, xg4, ig1, ig2, ig3, ig4)
        do i=1, li
            do j=1, lj
                psi =180.0 - xlon(i, j)+xg4-psio(i, j)
                u = cos(psi*dgtord)*spdo(i, j)
                v = sin(psi*dgtord)*spdo(i, j)
                spdo(i, j) = u
                psio(i, j) = v
            end do
        end do
        return
    endif

    if (grtyp .eq. 'A' .or. grtyp .eq. 'B' .or. grtyp .eq. 'G' .or. grtyp .eq. 'L') then
        do i=1, li
            do j=1, lj
                psi = 270.0 - psio(i, j)
                u = cos(psi*dgtord)*spdo(i, j)
                v = sin(psi*dgtord)*spdo(i, j)
                spdo(i, j) = u
                psio(i, j) = v
            end do
        end do
    endif
end
