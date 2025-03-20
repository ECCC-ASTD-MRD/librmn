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


!> Set report keys
integer function mrbupd(iun, buf, temps, flgs, stnid, idtyp, lati, long, dx, dy, elev, drcv, datein, oars, run, sup, nsup, xaux, nxaux)
    use app
    use rmn_burp, only: enforc8, erclef, errdat, nauxtot, npritot, nauxdef, nauxsup, npridef, nprisup
    implicit none

    !> Unit number
    integer, intent(in) :: iun
    !> Buffered that will contain the data
    integer, intent(inout) :: buf(*)
    !> Time difference between the validity and synoptic time
    integer, intent(in) :: temps
    !> Observation flags
    integer, intent(in) :: flgs
    !> Station identifier
    character(len = *), intent(in) :: stnid
    !> Report type
    integer, intent(in) :: idtyp
    !> Latitude in hundreth of degrees
    integer, intent(in) :: lati
    !> Longitude in hundreth of degrees
    integer, intent(in) :: long
    !> Box x dimension
    integer, intent(in) :: dx
    !> Box y dimension
    integer, intent(in) :: dy
    !> Station elevation in meters
    integer, intent(in) :: elev
    !> Reception delay
    integer, intent(in) :: drcv
    !> Synoptic validity date (aammjjhh)
    integer, intent(in) :: datein
    !> Reserved for objective analysis
    integer, intent(in) :: oars
    !> Operational run identifier
    integer, intent(in) :: run
    !> Supplementary primary keys (none for version 1990)
    integer, intent(in) :: sup(*)
    !> Number of supplementary primary keys (0 for version 1990)
    integer, intent(inout) :: nsup
    !> Supplementary auxiliary keys (none for version 1990)
    integer, intent(in) :: xaux(*)
    !> Number of supplementary auxiliary keys (0 for version 1990)
    integer, intent(inout) :: nxaux

    !     mise a jour de l'entete d'un rapport. seules les clefs qui
    !     n'ont pas pour valeur -1 seront mise a jour.  il en va de meme
    !     pour chaque caractere de stnid s'il est different de '*'.

    integer, external :: getbuf8
    integer, external :: xdfupd
    external :: char2rah

    integer :: klprim(npritot), nklprim, i, nklaux, typrec, klaux(nauxtot)
    integer :: aa, mm, jj, annee, date
    character(len = 9) :: istnid

    date = datein
    mrbupd  = -1
    nklprim = npridef
    nklaux  = nauxdef
    typrec  = 1

    ! pour la version 1990, nsup et nxaux doivent etre egal a zero
    if(nsup .gt. nprisup) then
        write(app_msg, *) 'mrbupd: il y a trop de clefs primaires supplementaires'
        call lib_log(app_libfst, app_warning, app_msg)
        mrbupd = erclef
        nsup = nprisup
    endif
    if(nxaux .gt. nauxsup) then
        write(app_msg, *) 'mrbini: il y a trop de clefs auxiliaires supplementaires'
        call lib_log(app_libfst, app_warning, app_msg)
        mrbupd = erclef
        nxaux = nauxsup
    endif

    ! transformer chaque caractere de stnid en un entier
    istnid = stnid(1:9)
    do i = 1, 9
        if(istnid(i:i).ne.'*') then
            call char2rah(istnid(i:i), klprim(i), 1)
        else
            klprim(i) = -1
        endif
    enddo

    ! composer les autres clefs primaires
    klprim(11) = lati
    klprim(12) = long
    klprim(10) = flgs
    if ((enforc8) .and. (date .ne. -1)) then
        if (date .lt. 999999) then
            write(app_msg, *) 'mrbupd: la date doit etre en format aaaammjj'
            call lib_log(app_libfst, app_error, app_msg)
            mrbupd = errdat
        endif
    endif
    if (date .gt. 999999) then
        annee = date/10000
        aa = mod((date/10000), 100)
        mm = (((annee - 1900) /100) * 12) + mod((date/100), 100)
        jj = mod(date, 100)
        date = (aa * 10000) + (mm * 100) + jj
    endif
    klprim(13) = date
    klprim(14) = dx
    klprim(15) = idtyp
    klprim(16) = dy
    if(temps .eq. -1) then
        klprim(17) = -1
        klprim(18) = -1
    else
        klprim(17) = temps/100
        klprim(18) = mod(temps, 100)
    endif

    ! composer les clefs auxiliaires
    klaux(1) = getbuf8(buf)
    klaux(2) = oars
    klaux(3) = elev
    klaux(4) = drcv
    klaux(5) = run

    ! initialiser le tout
    mrbupd = xdfupd(iun, buf, typrec, klprim, nklprim, klaux, nklaux)
end
