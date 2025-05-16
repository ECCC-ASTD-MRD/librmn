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


!> Find a report
integer function mrfloc(iun, handle, stnid, idtyp, lat, lon, datein, temps, sup, nsup)
    use app
    use rmn_burp, only: enforc8, erclef, errdat, npridef, nprisup, npritot
    implicit none

    !> Numéro d'unité du fichier
    integer, intent(in) :: iun
    !> Pointeur de l'enregistrement d'où part la recherche, commencer la recherche au début du fichier si 0
    integer, intent(in) :: handle
    !> Identificateur de la station, ignoré si '*'
    character(len = *), intent(in) :: stnid
    !> Type de rapport, ignoré si -1
    integer, intent(in) :: idtyp
    !> Latitude de la station, ignoré si -1
    integer, intent(in) :: lat
    !> Longitude de la station, ignoré si -1
    integer, intent(in) :: lon
    !> Date de validité du rapport, ignoré si -1
    integer, intent(in) :: datein
    !> Heure de l'observation, ignoré si -1
    integer, intent(in) :: temps
    !> Nombre de clés supplémentaires
    integer, intent(inout) :: nsup
    !> Tableau de clés de recherche supplémentaires
    integer, dimension(*), intent(in) :: sup

    external char2rah
    integer, external :: xdfloc
    integer, external :: mrfprm

    character(len = 9) :: istnid
    integer :: iidtyp, ilat, ilon, idate, itemps, insup
    integer, dimension(1) :: isup
    integer :: idx, idy, iflgs, ilngr, irien

    integer, dimension(npritot) :: pri
    integer :: npri, i
    integer :: annee, aa, mm, jj, date

    date = datein
    mrfloc = -1
    npri = npridef

    ! Pour la version 1990, les clés supplementaires ne sont pas permises
    IF (nsup > nprisup) THEN
        write(app_msg, *) 'MRFLOC: Il y a trop de clefs primaires supplementaires'
        call Lib_Log(app_libfst, app_warning, app_msg)
        mrfloc = erclef
        nsup = nprisup
    END IF

    ! Composer les clefs a partir du stnid
    istnid = stnid(1:9)
    DO i = 1,9
        IF (istnid(i:i) /= '*') THEN
            CALL char2rah(istnid(i:i), pri(i), 1)
        ELSE
            pri(i) = -1
        ENDIF
    END DO

    ! Composer le reste des clefs de recherche
    pri(10) = -1
    pri(11) = lat
    pri(12) = lon
    IF ((enforc8) .AND. (date /= -1)) THEN
        IF (date < 999999) THEN
            write(app_msg,*) 'MRFLOC: La date doit etre en format AAAAMMJJ'
            call lib_log(app_libfst, app_error, app_msg)
            mrfloc = errdat
        end if
    end if
    if (date > 999999) then
        annee = date / 10000
        aa = mod((date / 10000), 100)
        mm = (((annee - 1900) / 100) * 12) + mod((date / 100),100)
        jj = mod(date, 100)
        date = (aa * 10000) + (mm * 100) + jj
    end if
    pri(13) = date
    pri(14) = -1
    pri(15) = idtyp
    pri(16) = -1
    if (temps == -1) then
        pri(17) = -1
    else
        pri(17) = temps / 100
    end if
    pri(18) = -1

    ! trouver l'enregistrement
    mrfloc = xdfloc(iun, handle, pri, npri)
    if (lib_loglevel(app_libfst,' ') >= app_info) then
        if (mrfloc < 0) then
            write(app_msg, 1000) stnid, idtyp, lat, lon, datein, temps
            call lib_log(app_libfst,app_info,app_msg)       
        else
            irien = mrfloc
            insup = 0
            irien = mrfprm(irien,istnid, iidtyp, ilat, ilon, idx,idy, idate, itemps,iflgs, isup, insup,ilngr)
            write(app_msg, 1100) istnid, iidtyp, ilat, ilon, idx, idy, idate, itemps, iflgs, ilngr
            call lib_log(app_libfst, app_info, app_msg)
        end if
    end if

    1000 FORMAT('MRFLOC: INEXISTANT - STNID=',A9,' IDTYP=',I3, ' LAT=',I5,' LON=',I5,' DATE=',I8,' TEMPS=',I4)
    1100 FORMAT('MRFLOC: TROUVE - STNID=',A9,' IDTYP=',I3, ' LAT=',I5,' LON=',I5,' DX=',i4,' DY=',i4,' DATE=',I8, ' TEMPS=',I4,' FLGS=',i8,' LNGR=',i6)
end
