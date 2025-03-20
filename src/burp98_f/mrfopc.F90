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


!> Set a character option
integer function mrfopc(optnom, opvalc)
    use app
    implicit none

    !> Name of the option to set
    character(len = *), intent(in) :: optnom
    !> Value of the option
    character(len = *), intent(in) :: opvalc

    !> \return Always 0
    !> The only option currently supported is _MSGLVL_

    integer :: lvl

    if(index(optnom, 'MSGLVL') .ne. 0) then
        lvl = lib_loglevel(app_libfst, opvalc)
    endif

    mrfopc = 0
end
