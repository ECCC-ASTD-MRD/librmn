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


!> Convert julian day to year, month and day
!> \authors Fliegel, Flandern
!> \date 1979
!> Copied from "Communications of the ACM" (1968), page 657
!> It covers a period of 7980 years with day 1 starting at year=-4713, month=11, day=25
pure subroutine datec(julian_day, year, month, day)
    implicit none

    !> Unique integer which maps 1-to-1 onto triples of integers representing year, month, day of month
    integer, intent(in) :: julian_day
    !> Year [-4713, 3267]
    integer, intent(out) :: year
    !> Month [1, 12]
    integer, intent(out) :: month
    !> Day of month [1, 31]
    integer, intent(out) :: day

    integer :: l, n

    l = julian_day + 68569
    n = 4 * l / 146097
    l = l - (146097 * n + 3) / 4
    year = 4000 * (l + 1) / 1461001
    l = l - 1461 * year / 4 + 31
    month = 80 * l / 2447
    day = l - 2447 * month / 80
    l = month / 11
    month = month + 2 - 12 * l
    year = 100 * (n - 49) + year + l
end
