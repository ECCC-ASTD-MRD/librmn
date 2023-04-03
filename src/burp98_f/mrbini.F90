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

!> Intialize report header
!> This must be done before using the report
function mrbini(iun, buf, temps, flgs, stnid, idtyp, lati, long, dx, dy, elev, idrcv, datein, oars, run, sup, nsup, xaux, nxaux) result(retval)
    use app
    implicit none

    !> Numéro d'unité associé au fichier
    integer, intent(in) :: iun
    integer, intent(in) :: temps
    !> Marqueurs globaux
    integer, intent(in) :: flgs
    !> Indentificateur de la station
    character(len = *), intent(in) :: stnid
    !> Report type
    integer, intent(in) :: idtyp
    !> Station latitude in centidegres
    integer, intent(in) :: lati
    !> Station longitude in centidegres
    integer, intent(in) :: long
    !> X dimension of a box
    integer, intent(in) :: dx
    !> Y dimension of a box
    integer, intent(in) :: dy
    !> Station altitude in metres
    integer, intent(in) :: elev
    !> Reception delay
    integer, intent(in) :: idrcv
    !> Synoptic validity date (AAMMJJHH)
    integer, intent(in) :: datein
    !> Reserved for objective analysis
    integer, intent(in) :: oars
    !> Operational run identifier
    integer, intent(in) :: run
    !> Number of additionnal primary keys.  Must be 0 for verion 1990.
    integer, intent(inout) :: nsup
    !> Number of additionnal auxilary keys
    integer, intent(inout) :: nxaux
    !> Additionnal primary keys
    integer, dimension(nsup), intent(in) :: sup
    !> Data array that will contain the records
    integer, dimension(*), intent(out) :: buf
    !> Additionnal auxilary keys
    integer, dimension(nxaux), intent(in) :: xaux

    integer :: retval

#include "defi.cdk"
#include "codes.cdk"
#include "enforc8.cdk"

    external xdfini, char2rah
    ! Type d'enregistrement
    integer :: typrec
    integer :: xdfini, nklaux, nklprim, i
    integer, dimension(npritot) :: klprim
    integer, dimension(nauxtot) :: klaux
    integer :: aa, mm, jj, annee, date
    character(len = 9) :: istnid

    date = datein
    retval  = -1
    nklprim = npridef
    nklaux  = nauxdef
    typrec  = 1

    ! Pour la version 1990, nsup et nxaux doivent etre egal a zero
    if (nsup > nprisup) then
        write(app_msg,*) 'MRBINI: Il y a trop de clefs primaires supplementaires'
        call Lib_Log(APP_LIBFST,APP_WARNING,app_msg)       
        retval = erclef
        nsup = nprisup
    end if
    if (nxaux > nauxsup) then
        write(app_msg,*) 'MRBINI: Il y a trop de clefs auxiliaires supplementaires'
        call Lib_Log(APP_LIBFST,APP_WARNING,app_msg)       
        retval = erclef
        nxaux = nauxsup
    end if

    ! Transformer chaque caractere de stnid en un entier
    istnid = stnid
    do i = 1, 9
        call char2rah(istnid(i:i), klprim(i), 1)
    end do

    ! Composer les autres clefs primaires
    klprim(10) = flgs
    klprim(11) = lati
    klprim(12) = long
    if (enforc8) then
        if (date < 999999) then
            write(app_msg,*) 'MRBINI: La date doit etre en format AAAMMJJ'
            call Lib_Log(APP_LIBFST,APP_ERROR,app_msg)       
            retval = errdat
        end if
    end if
    if (date > 999999) then
        annee = date / 10000
        aa = mod((date / 10000), 100)
        mm = (((annee - 1900) / 100) * 12) + mod((date / 100), 100)
        jj = mod(date, 100)
        date = (aa * 10000) + (mm * 100) + jj
    end if
    klprim(13) = date
    klprim(14) = dx
    klprim(15) = idtyp
    klprim(16) = dy
    if(temps == -1) then
        klprim(17) = -1
        klprim(18) = -1
    else
        klprim(17) = temps / 100
        klprim(18) = mod(temps, 100)
    end if

    ! Ajouter les clefs primaires supplementaires
    if (nsup > 0) then
        do i = 1, nsup
            klprim(npridef + i) = sup(i)
        end do
        nklprim = nklprim + nsup
    end if

    ! Composer les clefs auxiliaires
    klaux(1) = 0
    klaux(2) = oars
    klaux(3) = elev
    klaux(4) = idrcv
    klaux(5) = run

    ! Ajouter les clefs auxiliaires supplementaires
    if (nxaux > 0) then
        do i = 1, nxaux
            klaux(nauxdef + i) = xaux(i)
        end do
        nklaux = nklaux + nxaux
    end if

    ! Initialiser le tout
    retval = xdfini(iun, buf, typrec, klprim, nklprim, klaux, nklaux)

    call buf89a0(buf)
end
