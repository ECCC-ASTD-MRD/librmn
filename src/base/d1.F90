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


module d1_dn
    implicit none

    real, parameter :: C1 = -21.0
    real, parameter :: C2 = +13.0
    real, parameter :: C3 = +17.0
    real, parameter :: C4 = - 9.0
    real, parameter :: R20 = 0.05
end


!> One-sided estimate of the derivative at the first point, 4-th order accurate
real function d1(y, h, n)
    use d1_dn
    implicit none

    ! A 1-dimensional spline is a curve constrained to pass through certain given points, varying
    ! cubically between the given points. The different cubic segments join up smoothly, with
    ! continuous 1st and 2nd derivatives. The 2-dimensional spline varies bicubically in grid squares,
    ! passes through all the original points, and has continuous first and second derivatives,
    ! (as far as f(xxyy)).)

    ! The spacing of the points is arbitrary for this set of programs.
    ! (the 2-dimensional grid is assumed to have orthogonal coordinate axes).

    !> Number of points in y
    integer, intent(in) :: n
    !> Equally spaced points
    real, intent(in) :: y(n)
    !> Separation
    real, intent(in) :: h

    d1 = (c1 * y(1) + c2 * y(2) + c3 * y(3) + c4 * y(4)) * r20 / h
end


!> One-sided estimate of the derivative at the last point, 4-th order accurate
real function dn(y, h, n)
    use d1_dn
    implicit none

    !> Number of points in y
    integer, intent(in) :: n
    !> Equally spaced points
    real, intent(in) :: y(n)
    !> Separation
    real, intent(in) :: h

    dn = -1.0 * ( c1 * y(n) + c2 * y(n - 1) + c3 * y(n - 2) + c4 * y(n - 3)) * r20 / h
end
