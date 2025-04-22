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


!> Bicubic interpolation of points from a gaussian grid
subroutine ez_gggdint_w(zo, px, py, npts, ay, z, ni, j1, j2, wrap)
    use iso_fortran_env, only: real64
    implicit none

    !> Number of points to interpolate
    integer, intent(in) :: npts
    !> Source grid size along the X axis
    integer, intent(in) :: ni
    !> Source grid start index on the Y axis
    integer, intent(in) :: j1
    !> Source grid end index on the Y axis
    integer, intent(in) :: j2
    !> Interpolated values
    real, intent(out) :: zo(npts)
    !> X positions where to interpolate
    real, intent(in) :: px(npts)
    !> Y positions where to interpolate
    real, intent(in) :: py(npts)
    !> Y point positions
    real, intent(in) :: ay(j1:j2)
    !> Source grid values
    real z(ni, j1:j2)
    integer, intent(in) :: wrap

    !> \ingroup ezscint

    !  *   *   *   *
    !
    !  *   *   *   *
    !        #         == >   pt (x, y)
    !  *  (=)  *   *   == > = pt (i, j)
    !
    !  *   *   *   *

    !  cy(i, 1) = 1.0 / (x2-x1)
    !  cy(i, 2) = 1.0 / (x3-x1)
    !  cy(i, 3) = 1.0 / (x3-x2)
    !  cy(i, 4) = 1.0 / (x4-x1)
    !  cy(i, 5) = 1.0 / (x4-x2)
    !  cy(i, 6) = 1.0 / (x4-x3)
    !
    !  structure identique pour cy(j, 1..6)

    integer :: i, j, n
    integer :: imoins1, iplus1, iplus2, limite
    !> \note cy used to be real but was up-classed to real64 for computation. It is now defined as a real64. This might slightly change the results.
    real(kind = real64) :: cy(6)
    real(kind = real64) :: y, y1, y2, y3, y4
    real(kind = real64) :: y11, y12, y13, y14
    real(kind = real64) :: ay1, ay2, ay3, ay4
    real(kind = real64) :: dx

    do n = 1, npts
        limite = ni + 2 - wrap
        i = min(ni - 2 + wrap, max(1, max(2 - wrap, ifix(px(n)))))
        j = min(j2 - 2, max(j1, ifix(py(n))))

        imoins1 = i - 1
        iplus1 = i + 1
        iplus2 = i + 2

        if (wrap > 0 .and. (i <= 1) .or. i >= (ni - 1)) then
            imoins1 = mod(limite + i - 1, limite)
            iplus1 = mod(limite + i + 1, limite)
            iplus2 = mod(limite + i + 2, limite)

            if (imoins1 == 0) imoins1 = ni
            if (i == 0) i = ni
            if (iplus1 == 0) iplus1 = ni
            if (iplus2 == 0) iplus2 = ni

            if (wrap == 1) then
                if (iplus2 == ni) iplus2 = 2
                if (imoins1 == ni) imoins1 = ni - 1
            endif
        endif

        dx = px(n) - i

        y1 = cubic(dble(z(imoins1, j - 1)), dble(z(i, j - 1)), dble(z(iplus1, j - 1)), dble(z(iplus2, j - 1)), dx)
        y2 = cubic(dble(z(imoins1, j  )), dble(z(i, j  )), dble(z(iplus1, j  )), dble(z(iplus2, j  )), dx)
        y3 = cubic(dble(z(imoins1, j + 1)), dble(z(i, j + 1)), dble(z(iplus1, j + 1)), dble(z(iplus2, j + 1)), dx)
        y4 = cubic(dble(z(imoins1, j + 2)), dble(z(i, j + 2)), dble(z(iplus1, j + 2)), dble(z(iplus2, j + 2)), dx)

        y = ay(j) + (ay(j + 1) - ay(j)) * (py(n) - j)

        ! interpolation finale selon y
        ay1 = ay(j - 1)
        ay2 = ay(j)
        ay3 = ay(j + 1)
        ay4 = ay(j + 2)

        cy(1) = 1.0 / (ay2 - ay1)
        cy(2) = 1.0 / (ay3 - ay1)
        cy(3) = 1.0 / (ay3 - ay2)
        cy(4) = 1.0 / (ay4 - ay1)
        cy(5) = 1.0 / (ay4 - ay2)
        cy(6) = 1.0 / (ay4 - ay3)

        y11 = y1
        y12 = fa2(cy(1), y1, y2)
        y13 = fa3(cy(1), cy(2), cy(3), y1, y2, y3)
        y14 = fa4(cy(1), cy(2), cy(3), cy(4), cy(5), cy(6), y1, y2, y3, y4)
        zo(n) = real(fa(y11, y12, y13, y14, y, ay1, ay2, ay3))
    enddo
    return
    contains
#include "cubic8.cdk"
#include "fa8.cdk"
end
