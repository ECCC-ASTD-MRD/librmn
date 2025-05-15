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


!> Find the index of the element equal or closest to "val"
integer function ez_cherche(val, tableau, nbelem)
    implicit none

    !> Value to search for
    real, intent(in) :: val
    !> Number of elements in array
    integer, intent(in) :: nbelem
    !> Sorted array in ascending order
    real, intent(in) :: tableau(nbelem)

    !> \ingroup ezscint

    integer :: debut, milieu, fin

    debut = 1
    fin = nbelem
    milieu = int((debut + fin) * 0.5)

    do while (milieu /= debut)
        if (val <= tableau(milieu)) then
              fin   = milieu
        else
              debut = milieu
        endif
        milieu = int((debut + fin) * 0.5)
    end do
    ez_cherche = milieu
end
