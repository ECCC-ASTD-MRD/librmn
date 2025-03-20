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


!> \brief Fermer un fichier BURP
integer function mrfcls( iun )
    use app
    use rmn_burp, only: badtbl
    implicit none

    !> numéro d'unité associé au fichier
    integer, intent(in) :: iun

    integer, external :: qdfmsig
    integer, external :: xdfcls

    mrfcls = 0
    if (badtbl /= 0) then
        mrfcls = qdfmsig(iun, 'bRp0')
    end if
    if (mrfcls < 0) then
        return
    end if

    mrfcls = xdfcls( iun )
    if (mrfcls < 0) then 
        return
    end if

    write(app_msg, *) 'mrfcls: unite ', iun, ' fichier rapport est ferme'
    call lib_log(app_libfst, app_trivial, app_msg)
end
