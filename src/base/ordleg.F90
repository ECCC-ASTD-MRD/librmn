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


!> Subset of belousovs algorithm used to calculate ordinary legendre polynomials
subroutine ordleg(sx, coa, ir)
    implicit none

    !> Legendre polynomial evaluated at coa
    real, intent(out) :: sx
    !> Cosine of colatitude
    real, intent(in) :: coa
    !> Wave number
    integer, intent(in) :: ir

    integer :: irpp, irppm, k, kk, n, n1
    real :: a, ang, b, c1, c4, delta, fk, fn, fn2, fn2sq, s1, sia, sqr2, theta

    sqr2 = sqrt(2.0)
    irpp = ir + 1
    irppm = irpp - 1
    delta = acos(coa)
    sia = sin(delta)

    theta = delta
    c1 = sqr2

    do n = 1, irppm
      fn = float(n)
      fn2 = 2.0 * fn
      fn2sq = fn2 * fn2
      c1 = c1 * sqrt(1.0 - 1.0 / fn2sq)
    end do

    n = irppm
    ang = fn * theta
    s1 = 0.0
    c4 = 1.0
    a = -1.0
    b = 0.0
    n1 = n + 1

    do kk = 1, n1, 2
        k = kk - 1
        if (k == n) c4 = 0.5 * c4
        s1 = s1 + c4 * cos(ang)
        a = a + 2.0
        b = b + 1.0
        fk = float(k)
        ang = theta * (fn - fk - 2.0)
        c4 = (a * (fn - b + 1.0) / (b * (fn2 - a))) * c4
    end do

    sx = s1 * c1
end
