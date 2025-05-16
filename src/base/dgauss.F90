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


!> Calculates the zeroes of the ordinary legendre polynomial of order n,  i.e. define gaussian grid
subroutine dgauss (n, roots, kase)
    use rmn_base_const, only: pie, south
    implicit none

    !> Order of the polynomials
    integer, intent(in) :: n
    !> Zeroes of the ordinary Legendre polynomials
    real, intent(out) :: roots(*)
    !> Area: 0 = global, 1 = North, 2 = South
    integer, intent(in) :: kase

    !> The positive roots are approximated by the best asymptotic formula available to the author, found in
    !> abramowitz and stegun "handbook of mathematical functions".
    !> chapter 22 formula 22.16.6.
    !> Newton's method is used to refine the guess to precision defined by the constant tol.  since the roots are of order
    !> of magnitude unity, absolute precision is adequate, rather than a relative test.
    !> A standard identity is used to determine the derivative of the polynomial in terms of the values of p(n;x), p(n-1;x).
    !> (x**2-1.0)*(dp/dx)=n*(x*p(n;, x)-p(n-1;x)).
    !> See abramowitz and stegun formula 22.8.5
    !> Note that in contrast to other formulas this requires only 2 evaluations of a legendre polynomial per iteration.
    !> Note that the coordinate used is conventionally referred to as mu=cos(theta), running from +1 to -1, for theta from 0 to
    !> pi. The negative roots are  filled by symmetry. for kase=global, all n roots are found, while for
    !> dase=north/south only the +ve/-ve roots are found, (including 0 if n is odd)  i.e. n/2+mod(n, 2) roots.

#if defined (ALL64)
    real, parameter :: tol = 1.0E-13
#else
    real, parameter :: tol = 1.0E-6
#endif

    real :: normn, normnm
    integer :: i, j, l, irt
    real :: delta, g, gm, pn, pnm, rdpdx, t

    !  ordleg returns polynomials normalized to unit integral.
    !  normn, normnmn restore the convention normalization, p(n;1.0)=1.0.
    normn = sqrt(2.0 / (2.0 * n + 1.0))
    normnm = sqrt(2.0 / (2.0 * n - 1.0))
    l = n / 2

    ! calculate asymptotic approximation
    do i = 1, l
        if (kase /= south) j = i
        if (kase == south) j = i + l + mod(n, 2)
        t = (4 * j - 1) * pie / float(4 * n + 2)
        if (kase /= south) irt = i
        if (kase == south) irt = i + mod(n, 2)
        roots(irt) = cos(t + 1.0 / (8.0 * float(n**2) * tan(t)))
    end do

    do i = 1, l
        ! Repeat 1 newton iteration
        delta = huge(0.0)
        do while (abs(delta) > tol)
            call ordleg(g, roots(i), n)
            call ordleg(gm, roots(i), n - 1)
            pn = normn * g
            pnm = normnm * gm
            rdpdx = (roots(i) ** 2 - 1.0) / (n * (roots(i) * pn - pnm))
            delta = -pn * rdpdx
            roots(i) = roots(i) + delta
        end do

        roots(n + 1 - i) = -roots(i)
    end do

    if (mod(n, 2) == 0) return
    if (kase /= south) irt = l + 1
    if (kase == south) irt = 1
    roots(irt) = 0.0
end
