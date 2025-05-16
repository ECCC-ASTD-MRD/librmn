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


!> Transformation from a set of points (lat,lon) in the spherical coordinate system to cartesian space
subroutine ez_lac(xyz, lon, lat, nbpts)
    implicit none

    !> Number of points
    integer, intent(in) :: nbpts
    !> Coordinates in cartesian space
    real, dimension(3, nbpts), intent(out) :: xyz
    !> Longitudes in spherical coordinates
    real, dimension(nbpts), intent(in) :: lon
    !> Latitudes in spherical coordinates
    real, dimension(nbpts), intent(in) :: lat

    !> \ingroup ezscint

    real, parameter :: dar = acos(-1.0) / 180.0
    integer :: i
    real :: cosdar

    do i = 1, nbpts
        cosdar = cos(dar * lat(i))
        xyz(1,i) = cosdar * cos(dar * lon(i))
        xyz(2,i) = cosdar * sin(dar * lon(i))
        xyz(3,i) = sin(dar * lat(i))
    end do
end subroutine
