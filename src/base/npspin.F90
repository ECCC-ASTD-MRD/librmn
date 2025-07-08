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


module npspin_helper
    implicit none
    contains

    ! POLIN FUNCTION(9/8 OF UV1 LESS 1/8 OF UV2) / NO OF PTS
    pure function polin(uv1, uv2, i) result(res)
        implicit none

        real, intent(in) :: uv1, uv2
        integer, intent(in) :: i
        real :: res

        res = (uv1 * 1.125 - uv2 * 0.125) / float(i)
    end function polin
end module npspin_helper


! Compute direction and speed at pole north or south using allpts on the 2 closests latitude circle
subroutine npspin (speed, psi, u, v, li, lj, kase, phi, arbpsi, dellam)
    use rmn_base_const, only: dgtord, rdtodg, global
    use npspin_helper
    implicit none

    !> Wind speed in knots
    real, intent(out) :: speed
    !> Angle between the wind and greenwich
    real, intent(out) :: psi
    !> Size of u and v fields along the first dimension
    integer, intent(in) :: li
    !> Size of u and v fields along the second dimension
    integer, intent(in) :: lj
    !> Hemisphere (1 for global, 2 for north, 3 for south)
    integer, intent(in) :: kase
    !> Vector fields on lat lon
    real, intent(in) :: u(li, lj), v(li, lj)
    !> Latitude of pt u or v
    real, intent(in) :: phi
    !> Arbitrary wind direction when speed is zero (psi = arbpsi)
    real, intent(in) :: arbpsi
    !> Longitude difference between two grid points
    real, intent(in) :: dellam

    integer :: i, j1, j2
    real :: uv2j1, uv2j2, vv2j1, vv2j2
    real :: clambi, c, s, ui, vi, upole, vpole

    j1 = 1
    j2 = 2
    if (phi > 90.0 .and. kase == global) j1 = lj
    if (phi > 90.0 .and. kase == global) j2 = lj - 1

    uv2j1 = 0.0
    uv2j2 = 0.0
    vv2j1 = 0.0
    vv2j2 = 0.0

    do i = 1, li
        clambi = (i - 1) * dellam * dgtord
        s = -sin(clambi)
        c =  cos(clambi)
        ui = s * u(i, j1) - c * v(i, j1)
        vi = c * u(i, j1) + s * v(i, j1)
        uv2j1 = uv2j1 + ui
        vv2j1 = vv2j1 + vi
        ui = s * u(i, j2) - c * v(i, j2)
        vi = c * u(i, j2) + s * v(i, j2)
        uv2j2 = uv2j2 + ui
        vv2j2 = vv2j2 + vi
    end do

    upole = polin (uv2j1, uv2j2, li)
    vpole = polin (vv2j1, vv2j2, li)
    speed = sqrt(upole**2 + vpole**2)
    psi = arbpsi
    if (speed /= 0.0) psi = rdtodg * atan2(vpole, upole)
    if (psi < 0.0) psi = psi + 360.0
END
