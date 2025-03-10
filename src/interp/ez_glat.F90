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



!> Compute latitudes of a gaussian grid
subroutine ez_glat(latroots, groots, nj, hem)
    use rmn_base_const, only: rdtodg
    implicit none

    integer, intent(in) :: nj
    integer, intent(in) :: hem
    real, intent(out) :: latroots(*)
    real :: groots(*)

    external dgauss

#include "ez_def_shared.h"

    integer :: j, npoly
    real :: temp

    if (hem .ne. GLOBAL) then
        npoly = nj * 2
    else
        npoly = nj
    endif
    call dgauss(npoly, groots, GLOBAL)

    do j = 1, npoly / 2
        temp = groots(j)
        groots(j) = groots(npoly + 1 - j)
        groots(npoly + 1 - j) = temp
    enddo

    if (hem .ne. NORD) then
        do j = 1, nj
            latroots(j) = 90.0 - rdtodg * acos(groots(j))
        enddo

    endif

    if (hem .eq. NORD) then
        do j = 1, nj
            latroots(j) = 90.0 - rdtodg * acos(groots(j + nj))
    end do
    endif
end
