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


!> Delete a record from if burp file
integer function mrfdel( handle )
    use app
    implicit none

    !> Handle of the record to delete
    integer, intent(in) :: handle

    integer, external :: xdfdel, mrfprm

    character(*), parameter :: fmt = &
        "('MRFDEL: Efface - STNID=',A9,' IDTYP=',I3, ' LAT=',I5,' LON=',I5,' DX=',i4,' DY=',i4,' DATE=',I8, ' TEMPS=',I4,' FLGS=',i8,' LNGR=',i6)"

    character(len = 9) :: istnid
    integer :: iidtyp, ilat, ilon, idate, itemps, isup(1), insup
    integer :: idx, idy, iflgs, ilngr, irien

    mrfdel = -1

    ! obtenir les parametres descripteurs de l'enregistrement
    if (lib_loglevel(app_libfst,' ') >= app_info) then
        irien = handle
        insup = 0
        irien = mrfprm(irien, istnid, iidtyp, ilat, ilon, idx, idy, idate, itemps, iflgs, isup, insup, ilngr)
    endif

    ! effacer l'enregistrement
    mrfdel = xdfdel( handle )

    if (mrfdel >= 0) then
        write(app_msg, fmt) istnid, iidtyp, ilat, ilon, idx,idy, idate, itemps, iflgs, ilngr
        call lib_log(app_libfst, app_debug, app_msg)
    endif
end
