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


!> Get the number of records in the file
integer function mrfnbr( iun )
    use rmn_burp, only: nstatmax
    implicit none

    !> Unit number
    integer, intent(in) :: iun

    integer, external :: xdfsta

    integer :: stat(nstatmax), prii, auxx
    character(len = 4) :: appl, versn

    mrfnbr = -1

    mrfnbr = xdfsta(iun, stat, nstatmax, prii, 0, auxx, 0, versn, appl)
    if(mrfnbr >= 0) mrfnbr = stat(12)
end
