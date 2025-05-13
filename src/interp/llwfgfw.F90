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


!> Converts from u-v (grid) wind components to standard meteorological speed and direction
subroutine llwfgfw(spd, dir, xlat, xlon, li, lj, grtyp, ig1, ig2, ig3, ig4)
    implicit none

    !> First dimension of the spd and dir fields
    integer, intent(in) :: li
    !> Second dimension of the spd and dir fields
    integer, intent(in) :: lj
    !> U component when called, wind speed on the return
    real, intent(inout) :: spd(li, lj)
    !> V component when called, wind direction on return
    real, intent(inout) :: dir(li, lj)
    !> Longitude on the unrotated coordinate system
    real, intent(out) :: xlat(li, lj)
    !> Latitude on the unrotated coordinate system
    real, intent(out) :: xlon(li, lj)
    !> Grid type
    character(len = 1), intent(in) :: grtyp
    !> First integer grid descriptor. See \see cxgaig for descriptor meanings
    integer, intent(in) :: ig1
    !> Second integer grid descriptor
    integer, intent(in) :: ig2
    !> Third integer grid descriptor
    integer, intent(in) :: ig3
    !> Forth integer grid descriptor
    integer, intent(in) :: ig4

    external :: cigaxg, ez_crot, ez_gfxyfll, ez_vrotf2, ez_llwfgdw
    real :: r(3, 3), ri(3, 3)
    real :: xlat1, xlon1, xlat2, xlon2
    real :: xlatgf(li, lj), xlongf(li, lj)
    real :: uvcart(3, li, lj), xyz(3, li, lj)

    call cigaxg(grtyp, xlat1, xlon1, xlat2, xlon2, ig1, ig2, ig3, ig4)
    call ez_crot(r, ri, xlon1, xlat1, xlon2, xlat2)
    call ez_gfxyfll(xlon, xlat, xlongf, xlatgf, li * lj, xlat1, xlon1, xlat2, xlon2)
    call ez_vrotf2(spd, dir, xlon, xlat, xlongf, xlatgf, ri, xyz, uvcart, li, lj)
    call ez_llwfgdw(spd, dir, xlongf, li, lj, 'L', 0, 0, 0, 0)
end
