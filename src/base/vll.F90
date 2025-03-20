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


!> Interpolate vectors from a LL grid
subroutine vll(spd, psi, lambda, phi, u, v, li, lj, xla0, xlo0, dlat, dlon)
    use rmn_base_const, only: dgtord, rdtodg
    implicit none

    !> Wind speed
    real, intent(out) :: spd
    !> When direction
    real, intent(out) :: psi
    !> Point's longitude
    real, intent(in) :: lambda
    !> Point's colatitude (latitude - 90.0)
    real, intent(in) :: phi
    !> First dimension of the u and v fields
    integer, intent(in) :: li
    !> Second dimension of the u and v fields
    integer, intent(in) :: lj
    !> First wind vector component
    real, intent(in) :: u(li, lj)
    !> Second wind vector component
    real, intent(in) :: v(li, lj)
    !> Latitude in degrees of the lower left corner
    real, intent(in) :: xla0
    !> Longitude in degrees of the lower left corner
    real, intent(in) :: xlo0
    !> Latitude spacing in degrees
    real, intent(in) :: dlat
    !> Longitude spacing in degrees
    real, intent(in) :: dlon

    real, external :: sll

    real :: arbpsi, ufin, vfin, uf, vf, cc, ss

    arbpsi = 99999.0

    ufin = sll(lambda, phi, u, li, lj, xla0, xlo0, dlat, dlon)
    vfin = sll(lambda, phi, v, li, lj, xla0, xlo0, dlat, dlon)

    spd = sqrt(ufin ** 2 + vfin ** 2)

    psi = arbpsi
    ss = -sin(lambda * dgtord)
    cc = cos(lambda * dgtord)
    uf = ss * ufin - cc * vfin
    vf = cc * ufin + ss * vfin
    if (spd /= 0) psi = rdtodg * atan2(vf, uf)
    if (psi < 0.0) psi = psi + 360.0
end
