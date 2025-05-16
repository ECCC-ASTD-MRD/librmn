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


!> Get the size of the longest record in the file
integer function mrfmxl( iun )
    use rmn_burp, only: unites
    implicit none

    !> Unit number
    integer, intent(in) :: iun

    ! For BITMOT
#include <ftnmacros.hf>

    integer, external :: xdfsta
    integer :: stat(6), pridef, auxdef
    character(len = 4) :: versn, appl

    mrfmxl = xdfsta(iun, stat, 6, pridef, 0, auxdef, 0, versn, appl)

    if (mrfmxl < 0) return

    ! obtenir la longueur de stat(6) et la convertir en mots-hote
    mrfmxl = stat(6) * unites / BITMOT
end
