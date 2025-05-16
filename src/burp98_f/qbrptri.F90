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


!> Sort element table
subroutine qbrptri(tableau, ni, nj)
    implicit none

    !> First array dimension
    integer, intent(in) :: ni
    !> Second array dimension
    integer, intent(in) :: nj
    !> Array to sort
    integer, intent(inout) :: tableau(ni, nj)

    integer :: i, j, k, m, num

    m = nj
    !> \todo Implement this without goto
 10   if(m .gt. 1) then
         m = (m + 2) / 3
         do 40 i = m + 1, nj
            do j = i, m + 1, -m
               if (tableau(1, j - m) .lt. tableau(1, j)) go to 40
               do k = 1, ni
                  num = tableau(k, j)
                  tableau(k, j) = tableau(k, j - m)
                  tableau(k, j - m) = num
               end do
            end do
 40         continue
         go to 10
      endif
end
