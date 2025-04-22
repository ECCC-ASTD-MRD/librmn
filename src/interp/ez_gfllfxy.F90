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


!> Compute a latitude and longitude on the true earth from x and y coordinates on a rotated latitude/longitude frame of reference.
subroutine ez_gfllfxy(lonp, latp, lon, lat, n, xlat1, xlon1, xlat2, xlon2)
    implicit none

    !> Number of points
    integer, intent(in) :: n
    !> Longitude on the unrotated coordinate system corresponding to the point (lat,lon) of the rotated coordinate system
    real, dimension(n), intent(out) :: lonp
    !> Latitude on the unrotated coordinate system corresponding to the point (lat,lon) of the rotated coordinate system
    real, dimension(n), intent(out) :: latp
    !> Longitude on the rotated spherical coordinate system
    real, dimension(n), intent(in) :: lon
    !> Latitude on the rotated spherical coordinate system
    real, dimension(n), intent(in) :: lat
    !> Latitude on the unrotated coordinate system corresponding to the point (lat,lon)=(0,180) of the rotated coordinate system
    real, intent(in) :: xlat1
    !> Longitude on the unrotated coordinate system corresponding to the point (lat,lon)=(0,180) of the rotated coordinate system
    real, intent(in) :: xlon1
    !> Latitude on the unrotated coordinate system corresponding to a point (lat,lon) located on the equator of the rotated coordinate system
    real, intent(in) :: xlat2
    !> Latitude on the unrotated coordinate system corresponding to a point (lat,lon) located on the equator of the rotated coordinate system
    real, intent(in) :: xlon2

    real, dimension(3,3) :: r, ri

    call ezgfllfxy(lonp, latp, lon, lat, r, ri, n, xlat1, xlon1, xlat2, xlon2)
end

