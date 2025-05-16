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


subroutine ez_lambxyfll99(x, y, lat, lon, npts, latin1, latin2, yaxislat, yaxislon)
    implicit none

    integer, intent(in) :: npts
    real, intent(out) :: x(npts), y(npts)
    real, intent(in) :: lat(npts), lon(npts)
    real, intent(in) :: latin1, latin2, yaxislat, yaxislon

    real :: rn, rphi1, rphi2, f, rtan, rho, theta, rhozero, dlon, tmplat
    integer :: i

    real, parameter :: pisur4 = atan(1.0)
    real, parameter :: pi = 4.0 * pisur4
    real, parameter :: d2r = pi / 180.0
    real, parameter :: r = 6370997.0

    rphi1 = d2r * latin1
    rphi2 = d2r * latin2

    if (rphi1 == rphi2) then
        rn = sin(rphi1)
    else
        rn = log(cos(rphi1) / cos(rphi2))
        rn = rn / log((tan(pisur4 + 0.5 * rphi2)) / tan(pisur4 + 0.5 * rphi1))
    endif

    rtan = tan(pisur4 + rphi1 * 0.5)

    f = (cos(rphi1) * (rtan ** rn)) / rn
    rhozero = r * f / (tan(pisur4 + yaxislat * d2r * 0.5) ** rn)

    do i = 1, npts
        tmplat = lat(i)
        if (tmplat > 90.0) tmplat = 89.95
        rho   = r * f / (tan(pisur4 + tmplat * 0.5 * d2r) ** rn)
        dlon = lon(i) - yaxislon

        if (dlon < -180) then
            dlon = dlon + 360.0
        else if (dlon > 180.) then
            dlon = dlon - 360.0
        endif

        theta = rn * (d2r * dlon)

        x(i) = rho * sin(theta)
        y(i) = rhozero - rho * cos(theta)
    enddo
end
