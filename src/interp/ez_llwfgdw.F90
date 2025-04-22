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


!> Convert from grid wind components to std meteorological speed and direction, regardless of the geographical projection
subroutine ez_llwfgdw(z1, z2, xlon, li, lj, grtyp, ig1, ig2, ig3, ig4)
    use rmn_base_const, only: rdtodg
    implicit none

    !> X dimension of the grid
    integer, intent(in) :: li
    !> Y dimension of the grid
    integer, intent(in) :: lj
    !> Must contain to U component of the wind when called. Wind speed on return
    real, intent(inout) :: z1(li, lj)
    !> Must contain to V component of the wind when called. Wind direction on return
    real, intent(inout) :: z2(li, lj)
    !> Longitudes
    real, intent(in) :: xlon(li, lj)
    !> Grid type
    character(len = 1), intent(in) :: grtyp
    !> First integer grid parameter
    integer, intent(in) :: ig1
    !> Second integer grid parameter
    integer, intent(in) :: ig2
    !> Third integer grid parameter
    integer, intent(in) :: ig3
    !> Fourth integer grid parameter
    integer, intent(in) :: ig4

    !> \ingroup ezscint

    !> On output, a 270 degree wind is a westerly wind, meaning U is +ve and V is zero.

    external cigaxg

    real xg1, xg2, xg3, xg4
    integer i, j
    real spd0, dir0
    real x1(2*li*lj), y1(2*li*lj), lat(2*li*lj)

!  les #define qui suivent rendent le code plus lisible
#define uu   z1
#define vv   z2
#define spd  z1
#define dir  z2

    if (grtyp .eq. '!') then
        call ez_lamb_llwfgdw(uu, vv, xlon, li, lj, grtyp, ig1, ig2, ig3, ig4, x1, y1, lat)
        return
    endif


    if (grtyp.eq. 'N')then
        call cigaxg(grtyp, xg1, xg2, xg3, xg4, ig1, ig2, ig3, ig4)
        do j=1, lj
            do i=1, li
                spd0=sqrt(uu(i, j)*uu(i, j)+vv(i, j)*vv(i, j))
                if (spd0.eq. 0.0)then
                    dir0= 0.0
                else
                    if (uu(i, j).eq. 0.0)then
                        if (vv(i, j).ge. 0.0)then
                            dir0= xlon(i, j)+xg4-90.0
                        else
                            dir0= xlon(i, j)+xg4+90.0
                        endif
                    else
                        dir0=xlon(i, j)+xg4-rdtodg*atan2(vv(i, j), uu(i, j))
                    endif
                endif
                dir0=amod(amod(dir0, 360.0)+360.0, 360.0)
                spd(i, j)=spd0
                dir(i, j)=dir0
            enddo
        enddo
        return
    endif

    if (grtyp.eq. 'S')then
        call cigaxg(grtyp, xg1, xg2, xg3, xg4, ig1, ig2, ig3, ig4)
        do j=1, lj
            do i=1, li
                spd0=sqrt(uu(i, j)*uu(i, j)+vv(i, j)*vv(i, j))
                if (spd0.eq. 0.0)then
                    dir0 = 0.0
                else
                    if (uu(i, j).eq. 0.0)then
                        if (vv(i, j).ge. 0.0)then
                            dir0= 90.0 - xlon(i, j)+xg4
                        else
                            dir0= 270.0 - xlon(i, j)+xg4
                        endif
                    else
                        dir0=180.0-xlon(i, j)+xg4-rdtodg*atan2(vv(i, j), uu(i, j))
                    endif
                endif
                dir0=amod(amod(dir0, 360.0)+360.0, 360.0)
                spd(i, j)=spd0
                dir(i, j)=dir0
            enddo
        enddo
        return
    endif

    if (grtyp.eq.'A'.or.grtyp.eq.'B'.or.grtyp.eq.'G'.or.grtyp.eq.'L')then
        do j=1, lj
            do i=1, li
                spd0 = sqrt(uu(i, j)*uu(i, j)+vv(i, j)*vv(i, j))
                if (spd0.eq. 0.0)then
                    dir0 = 0.0
                else
                    if (uu(i, j).eq. 0.0)then
                        if (vv(i, j).ge. 0.0)then
                            dir0= 180.0
                        else
                            dir0= 0.0
                        endif
                    else
                        dir0=270.0 - rdtodg*atan2(vv(i, j), uu(i, j))
                    endif
                endif
                dir0 = amod(amod(dir0, 360.0)+360.0, 360.0)
                spd(i, j) = spd0
                dir(i, j) = dir0
            enddo
        enddo
        return
    endif
    write(6, "('0', ' erreur, bad grid type (llwfgdw) - grtyp = ', a11)") grtyp
end

