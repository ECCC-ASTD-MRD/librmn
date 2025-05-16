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


!> Convert lat-lon coordinates to grid
subroutine ez_ll2rgd(px, py, xlat, xlon, npts, ni, nj, grtyp, ig1, ig2, ig3, ig4, sym, lroots)
    implicit none

#include "ez_def_shared.h"

    integer, intent(in) :: npts, ni, nj
    real, intent(out) :: px(npts), py(npts)
    real, intent(inout) :: xlat(npts), xlon(npts)
    character(len = 1), intent(in) :: grtyp
    integer, intent(in) :: ig1, ig2, ig3, ig4
    logical, intent(in) :: sym ! Unused

    external cigaxg, ez_vxyfll, ez_llll2gd, ez_vtxyfll, ez_ggll2gd, ez_ll2ergd, ez_lambfll

    real pi, pj, d60, dgrw
    real dellat, dellon, xlat0, xlon0
    real lroots(nj)
    real clat, clon
    integer i

    if (grtyp == 'N') then
        call cigaxg(grtyp, pi, pj, d60, dgrw, ig1, ig2, ig3, ig4)
        call ez_vxyfll(px, py, xlat, xlon, npts, d60, dgrw, pi, pj, NORD)
        return
    endif

    if (grtyp == 'S')  then
        call cigaxg(grtyp, pi, pj, d60, dgrw, ig1, ig2, ig3, ig4)
        call ez_vxyfll(px, py, xlat, xlon, npts, d60, dgrw, pi, pj, SUD)
        return
    endif

    if (grtyp == 'T') then
        call cigaxg(grtyp, d60, dgrw, clat, clon, ig1, ig2, ig3, ig4)
        call ez_vtxyfll(px, py, xlat, xlon, clat, clon, d60, dgrw, ni, nj, npts)
        return
    endif

    if (grtyp == 'A') then
        dellon = 360.0 / real(ni)
        xlon0 = 0.0
        if (ig1 == GLOBAL) then
            dellat = 180.0 / real(nj)
            xlat0 = -90.0 + dellat * 0.5
        endif

        if (ig1 == NORD) then
            dellat = 90.0 / real(nj)
            xlat0 =  dellat * 0.5
        endif

        if (ig1 == SUD) then
            dellat = 90.0 / real(nj)
            xlat0 = -90.0 + dellat * 0.5
        endif

        do i = 1, npts
            if (xlon(i) < 0.0) xlon(i) = xlon(i) + 360.0
        enddo

        call ez_llll2gd(px, py, xlat, xlon, npts, xlat0, xlon0, dellat, dellon, 0.0)
        return
    endif

    if (grtyp == 'B') then
        dellon = 360.0 / real(ni - 1)
        xlon0 = 0.0
        if (ig1 == GLOBAL) then
            dellat = 180.0 / real(nj - 1)
            xlat0 = -90.0
        endif

        if (ig1 == NORD) then
            dellat = 90.0 / real(nj - 1)
            xlat0 =  0.0
        endif

        if (ig1 == SUD) then
            dellat = 90.0 / real(nj - 1)
            xlat0 = -90.0
        endif

        do i = 1, npts
            if (xlon(i) < 0.0) xlon(i) = xlon(i) + 360.0
        enddo
        call ez_llll2gd(px, py, xlat, xlon, npts, xlat0, xlon0, dellat, dellon, 0.0)
        return
    endif

    if (grtyp == 'G') then
        dellon = 360.0 / real(ni)
        xlon0 = 0.0
        do i = 1, npts
            if (xlon(i) < 0.0) xlon(i) = xlon(i) + 360.0
        enddo
        if (ig1 == GLOBAL) then
            call ez_ggll2gd(px, py, xlat, xlon, npts, ni, nj, ig1, lroots)
        else if  (ig1 == NORD) then
            dellat = 90.0 / real(nj)
            xlat0 =  dellat * 0.5
            call ez_llll2gd(px, py, xlat, xlon, npts, xlat0, xlon0, dellat, dellon, 0.0)
        else
            dellat = 90.0 / real(nj)
            xlat0 = -90.0 + dellat * 0.5
            call ez_llll2gd(px, py, xlat, xlon, npts, xlat0, xlon0, dellat, dellon, 0.0)
        endif
        return
    endif

    if (grtyp == 'L') then
        call cigaxg(grtyp, xlat0, xlon0, dellat, dellon, ig1, ig2, ig3, ig4)

        do i = 1, npts
            if (xlon(i) < xlon0) then
                xlon(i) = xlon(i) + 360.0
            endif
            if (xlon(i) > (xlon0 + ni * dellon)) then
                xlon(i) = xlon(i) - 360.0
            endif
        end do

        call ez_llll2gd(px, py, xlat, xlon, npts, xlat0, xlon0, dellat, dellon, 0.0)
        return
    endif

    if (grtyp == 'E') then
        call ez_ll2ergd(px, py, xlat, xlon, npts, ni, nj, grtyp, ig1, ig2, ig3, ig4)
        return
    endif

    if (grtyp == '!') then
        call ez_lambfll(px, py, xlat, xlon, npts, grtyp, ig1, ig2, ig3, ig4)
        return
    endif

    print *, '<ez_ll2rgd> bad grid type for type: ', grtyp
    print *, '         any further processing will create scrap'
    print *, '         stopping immediately !'
    stop
end
