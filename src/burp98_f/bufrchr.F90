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


!> Find the index of an element in an array
integer function bufrchr(elem, tableau, nele)
    implicit none

    !> Id of the element to find
    integer, intent(in) :: elem
    !> Number of elements in the array
    integer, intent(in) :: nele
    !> Sorted array of elements
    integer, intent(in) :: tableau(3, nele)

    integer :: fin, debut, milieu

    bufrchr = -1
    debut = 0
    fin = nele + 1
    milieu  = (debut + fin) / 2

    do while (debut /= milieu)
        if (elem == tableau(1, milieu)) then
            bufrchr = milieu
            return
        else
            ! Continue the binary search
            if (elem > tableau(1, milieu)) then
                debut = milieu
            else
                fin = milieu 
            endif
            milieu  = (debut + fin) / 2
        endif
    end do
end
