!/* RMNLIB - Library of useful routines for C and FORTRAN programming
! * Copyright (C) 1975-2001  Division de Recherche en Prevision Numerique
! *                          Environnement Canada
! *
! * This library is free software; you can redistribute it and/or
! * modify it under the terms of the GNU Lesser General Public
! * License as published by the Free Software Foundation,
! * version 2.1 of the License.
! *
! * This library is distributed in the hope that it will be useful,
! * but WITHOUT ANY WARRANTY; without even the implied warranty of
! * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
! * Lesser General Public License for more details.
! *
! * You should have received a copy of the GNU Lesser General Public
! * License along with this library; if not, write to the
! * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
! * Boston, MA 02111-1307, USA.
! */


!> \brief Trouver un rapport
INTEGER FUNCTION mrfloc(iun, handle, stnid, idtyp, lat, lon, datein, temps, sup, nsup)
    use app
    use rmn_burp_defi
    use rmn_burpopt

    IMPLICIT NONE

    !> Numéro d'unité du fichier
    INTEGER, INTENT(IN) :: iun
    !> Pointeur de l'enregistrement d'où part la recherche, commencer la recherche au début du fichier si 0
    INTEGER, INTENT(IN) :: handle
    !> Identificateur de la station, ignoré si '*'
    CHARACTER(len = *), INTENT(IN) :: stnid
    !> Type de rapport, ignoré si -1
    INTEGER, INTENT(IN) :: idtyp
    !> Latitude de la station, ignoré si -1
    INTEGER, INTENT(IN) :: lat
    !> Longitude de la station, ignoré si -1
    INTEGER, INTENT(IN) :: lon
    !> Date de validité du rapport, ignoré si -1
    INTEGER, INTENT(IN) :: datein
    !> Heure de l'observation, ignoré si -1
    INTEGER, INTENT(IN) :: temps
    !> Nombre de clés supplémentaires
    INTEGER, INTENT(INOUT) :: nsup
    !> Tableau de clés de recherche supplémentaires
    INTEGER, DIMENSION(*), INTENT(IN) :: sup

!AUTEUR  J. CAVEEN   OCTOBRE 1990
!REV 001 Y. BOURASSA MARS    1995 RATFOR @ FTN77
!REV 002 j. caveen   sept.   1995 ajout d'un appel a mrfprm pour produire un message plus explicite
!REV 003 M. Lepine   sept    1997 nouveau format de date AAAAMMJJ (an 2000)
!REV 004 M. Lepine   Avr     2000 appel a char2rah au lieu de read et hrjust
!REV 005 M. Lepine   Jan     2003 date est un argument d'entree seulement

#include <rmn/codes.cdk>
#include "enforc8.cdk"

    EXTERNAL char2rah
    INTEGER, EXTERNAL :: xdfloc
    INTEGER, EXTERNAL :: mrfprm

    CHARACTER(len = 9) :: istnid
    INTEGER :: iidtyp, ilat, ilon, idate, itemps, insup
    INTEGER, DIMENSION(1) :: isup
    INTEGER :: idx, idy, iflgs, ilngr, irien

    INTEGER, DIMENSION(npritot) :: pri
    INTEGER :: npri, i
    INTEGER :: annee, aa, mm, jj, date

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
    istnid = stnid
    DO i = 1,9
        IF (istnid(i:i) /= '*') THEN
            ! READ(istnid(i:i),'(a1)') pri(i)
            ! pri(i) = hrjust(pri(i), 1)
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
        END IF
    END IF
    IF (date > 999999) THEN
        annee = date / 10000
        aa = mod((date / 10000), 100)
        mm = (((annee - 1900) / 100) * 12) + mod((date / 100),100)
        jj = mod(date, 100)
        date = (aa * 10000) + (mm * 100) + jj
    END IF
    pri(13) = date
    pri(14) = -1
    pri(15) = idtyp
    pri(16) = -1
    IF (temps == -1) THEN
        pri(17) = -1
    ELSE
        pri(17) = temps / 100
    END IF
    pri(18) = -1

    ! Trouver l'enregistrement
    mrfloc = xdfloc(iun, handle, pri, npri)
    IF (lib_loglevel(APP_LIBFST,' ') >= APP_INFO) THEN
        IF (mrfloc < 0) THEN
            WRITE(app_msg, 1000) stnid, idtyp, lat, lon, datein, temps
            call lib_log(app_libfst,app_info,app_msg)       
        ELSE
            irien = mrfloc
            insup = 0
            irien = mrfprm(irien,istnid, iidtyp, ilat, ilon, idx,idy, idate, itemps,iflgs, isup, insup,ilngr)
            write(app_msg, 1100) istnid, iidtyp, ilat, ilon, idx, idy, idate, itemps, iflgs, ilngr
            call lib_log(app_libfst, app_info, app_msg)
        END IF
    END IF

    1000 FORMAT('MRFLOC: INEXISTANT - STNID=',A9,' IDTYP=',I3, ' LAT=',I5,' LON=',I5,' DATE=',I8,' TEMPS=',I4)
    1100 FORMAT('MRFLOC: TROUVE - STNID=',A9,' IDTYP=',I3, ' LAT=',I5,' LON=',I5,' DX=',i4,' DY=',i4,' DATE=',I8, ' TEMPS=',I4,' FLGS=',i8,' LNGR=',i6)
END
