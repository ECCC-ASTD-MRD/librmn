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


!> Get the main record descriptors
integer function mrfprm(handle, stnid, idtyp, lat, lon, dx, dy, date, temps, flgs, sup, nsup, lonenr)
    use app
    use rmn_burp, only: enforc8, unites, npridef, npritot, nprisup, erclef
    implicit none

    !> Record handle
    integer, intent(in) :: handle
    !> Station id
    character(len = 9), intent(out) :: stnid
    !> Report type
    integer, intent(out) :: idtyp
    !> Latitude in centi-degrees from the south pole
    integer, intent(out) :: lat
    !> Longitude in centi-degrees (0-35999)
    integer, intent(out) :: lon
    !> Box x dimension
    integer, intent(out) :: dx
    !> Box y dimension
    integer, intent(out) :: dy
    !> Validity date (aammjjhh)
    integer, intent(out) :: date
    !> Observation hour
    integer, intent(out) :: temps
    !> Global flags
    integer, intent(out) :: flgs
    !> Supplementary descriptors (unused)
    integer, intent(out) :: sup(*)
    !> Number of supplementary descriptors
    integer, intent(inout) :: nsup
    !> Record size in host machine words
    integer, intent(out) :: lonenr

    !> \return 0 on success, error code otherwise

! For BITMOT
#include <ftnmacros.hf>

    external :: rah2char
    integer, external :: xdfprm
    integer :: addr, lngr, typrec, pri(npritot), npri,   i
    integer :: annee, mois, aa, mm, jj

    npri = npridef

    ! pour la cuvee 90, nsup doit etre egal a zero
    if (nsup > nprisup) then
        write(app_msg, *) 'mrbini: il y a trop de clefs primaires supplementaires'
        call lib_log(app_libfst, app_warning, app_msg)
        mrfprm = erclef
        nsup = nprisup
    endif

    ! pour les versions subsequentes, nsup peut etre plus grand que zero.
    ! on ajoute alors les clefs primaires supplementaires au vecteur pri.
    if (nsup > 0) npri = npri + nsup

    ! obtenir les clefs primaires
    mrfprm = xdfprm(handle, addr, lngr, typrec, pri, npri)
    if (mrfprm < 0) return

    ! calcul de la longueur
    lonenr = lngr * unites / BITMOT

    ! decomposer les clefs primaires
    do i = 1, 9
        call rah2char(stnid(i:i), pri(i), 1)
    end do

    flgs  = pri(10)
    lat   = pri(11)
    lon   = pri(12)
    date  = pri(13)
    if ((mod((date / 100), 100) > 12) .or. (enforc8)) then
        ! retourner la date en format aaaammjj
        aa = mod((date / 10000), 100)
        mm = mod((date / 100), 100)
        jj = mod(date, 100)
        annee = 1900 + aa + (((mm - 1) / 12) * 100)
        mois = 1 + mod(mm - 1, 12)
        date = (annee * 10000) + (mois * 100) + jj
    endif
    dx    = pri(14)
    idtyp = pri(15)
    dy    = pri(16)
    temps = (pri(17) * 100) + pri(18)

    mrfprm = 0
end
