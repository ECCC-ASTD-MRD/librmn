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


!> Check if a value is in the provided range (min >= ichk <= max)
logical function valide(nom, ichk, minv, maxv)
    implicit none

    !> Variable name (for output message when not in range)
    character (len = *), intent(in) :: nom
    !> Value to check
    integer, intent(in) :: ichk
    !> Lower bound (included)
    integer, intent(in) :: minv
    !> Upper bound (included)
    integer, intent(in) :: maxv

    valide = .true.
    if (ichk < minv .or. ichk > maxv) then
        valide = .false.
        write(6, '("MAUVAISE VALEUR POUR",A10,"VALEUR=",I10,"MINIMUM=",I10,"MAXIMUM=",I10)') nom, ichk, minv, maxv
    endif
END
