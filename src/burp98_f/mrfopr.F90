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


!> Set a real option
integer function mrfopr(optnom, opvalr)
    use app
    use rmn_burp, only: eroptn, manque
    implicit none

    !> Name of the option to set
    character(len = *), intent(in) :: optnom
    !> Value
    real, intent(in) :: opvalr

    !> \return 0 on success, -1 otherwise
    !> The only option unrecognized is _MISSING_

    mrfopr = -1
    if(index(optnom, 'MISSING') .ne. 0) then
        manque = opvalr
        mrfopr = 0
    else
        write(app_msg,*) 'MRFOPR: Nom d''option inconnu'
        call Lib_Log(APP_LIBFST, APP_ERROR, app_msg)
        mrfopr = eroptn
    endif
end
