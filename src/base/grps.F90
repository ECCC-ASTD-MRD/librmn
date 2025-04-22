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


!> Compute the latitude and longitude of a polar stereographic grid
subroutine grps(xlat, xlon, ni, nj, pi, pj, d60, dgrw, hem)
    implicit none

    !> Number of points along the X axis
    integer, intent(in) :: ni
    !> Number of points along the Y axis
    integer, intent(in) :: nj
    !> Latitudes
    real, intent(out) :: xlat(ni, nj)
    ! Greater done Longitudes
    real, intent(out) :: xlon(ni, nj)
    !> X coordinates of the pole
    real, intent(in) :: pi
    !> Y coordinates of the pole
    real, intent(in) :: pj
    !> Distance in meters between the grid points at 60 degrees of latitude
    real, intent(in) :: d60
    !> Angle between the X axis and greenwhich meridian
    real, intent(in) :: dgrw
    !> Hemisphere: 1 for North, 2 for South
    real, intent(in) :: hem

    integer :: i, j
    real :: y, xla, xlo

    do j = 1, nj
        y = j - pj

        do i = 1, ni
            call llfxy(xla, xlo, i - pi, y, d60, dgrw, hem)
            xlat(i, j) = xla
            if (xlo < 0) xlo = xlo + 360.0
            xlon(i, j) = xlo
        end do
    end do
end
