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


!> Compute the grid coordinates of a point on a gaussian grid
subroutine ez_ggll2gd(x, y, xlat, xlon, npts, ni, nj, hem, lroots)
    implicit none

    integer, intent(in) :: npts
    integer, intent(in) :: ni
    integer, intent(in) :: nj
    real, intent(out) :: x(npts)
    real, intent(out) :: y(npts)
    real, intent(in) :: xlat(npts)
    real, intent(in) :: xlon(npts)
    real, intent(in) :: lroots(nj)

    integer, external :: ez_cherche

    integer :: i, hem, indy
    real :: dellon, xlon0

    dellon = 360.0 / real(ni)
    xlon0 = 0.0

    do i = 1, npts
        x(i) = (xlon(i) - xlon0) / dellon + 1.0
    end do

    do i = 1, npts
        indy = ez_cherche(xlat(i), lroots, nj)
        if (indy >= nj) indy = nj - 1

        y(i) = real(indy) + (xlat(i) - lroots(indy)) / (lroots(indy + 1) - lroots(indy))
    enddo
end
