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


!> Check if element is repeating
integer function mrbrpt(element)
    use app
    use rmn_burp, only: erelem, maxrep, rpetitif
    implicit none

    !> Element to check
    integer, intent(in) :: element

    !> Sets rpetitif in the rmn_burp module

    !> \return 1 if element is repeating, 0 if element is non-repeating, <0 if the element code is invalid

    ! For BITMOT, GETBIT, RMASK
#include <ftnmacros.hf>

    if (element < 1 .or. element > maxrep * BITMOT) then
        write(app_msg,*) 'mrbrpt: nom d''element non valide'
        call lib_log(app_libfst, app_warning, app_msg)
        mrbrpt = erelem
    else
        mrbrpt = GETBIT(rpetitif, element, 1)
    endif
end
