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


!> Bi-cubic interpolation from a irregular grid
subroutine ez_irgdint_3_wnnc(zo, px, py, npts, ax, ay, z, ni, j1, j2, wrap)
    use iso_fortran_env, only: real64
    implicit none

    !> Number of points to interpolate
    integer, intent(in) :: npts
    integer, intent(in) :: ni
    integer, intent(in) :: j1
    integer, intent(in) :: j2
    integer, intent(in) :: wrap
    !> Interpolated values
    real, intent(out) :: zo(npts)
    !> X positions where to interpolate
    real, intent(in) :: px(npts)
    !> Y positions where to interpolate
    real, intent(in) :: py(npts)
    !> Point positions on the X axis
    real, intent(in) :: ax(ni)
    !> Point positions on the Y axis
    real, intent(in) :: ay(j1:j2)
    !> Source grid values
    real, intent(in) :: z(ni, j1:j2)

    !> \ingroup ezscint

    !     *   *   *   *
    !
    !     *   *   *   *
    !           #        .eq.>   pt (x, y)
    !     *  (=)  *   *  .eq.> = pt (i, j)
    !
    !     *   *   *   *
    !
    !     cx(i, 1) = 1.0 / (x2-x1)
    !     cx(i, 2) = 1.0 / (x3-x1)
    !     cx(i, 3) = 1.0 / (x3-x2)
    !     cx(i, 4) = 1.0 / (x4-x1)
    !     cx(i, 5) = 1.0 / (x4-x2)
    !     cx(i, 6) = 1.0 / (x4-x3)
    !
    !  structure identique pour cy(j, 1..6)

    integer imoins1, iplus1, iplus2
    integer :: i, j, n, limite
    real(kind = real64) :: a11, a12, a13, a14, a21, a22, a23, a24
    real(kind = real64) :: a31, a32, a33, a34, a41, a42, a43, a44
    real(kind = real64) :: b1, b2, b3, b4, b11, b12, b13, b14
    real(kind = real64) :: x1, x2, x3, x4, y1, y2, y3, y4
    real(kind = real64) :: x, y
    real(kind = real64) :: cx(6), cy(6)
    real(kind = real64) :: z1, z2, z3, z4

    limite = ni+2-wrap
    do n = 1, npts
        i = min(ni-2+wrap, max(1, max(2-wrap, ifix(px(n)))))
        j = min(j2-2, max(j1+1, ifix(py(n))))

        imoins1 = i-1
        iplus1 = i+1
        iplus2 = i+2

        if (wrap == 1 .and. (i <= 1 .or. i >= (ni-wrap))) then
            if (i == 1) then
                imoins1 = ni-1
                iplus1 = 2
                iplus2 = 3
                x1 = ax(ni-1) - 360.0
                x2 = ax(1)
                x3 = ax(2)
                x4 = ax(3)
            endif
            if (i == (ni-1)) then
                imoins1 = ni-2
                iplus1 = ni
                iplus2 = 1
                x1 = ax(ni-2)
                x2 = ax(ni-1)
                x3 = ax(ni)
                x4 = ax(2)+360.0
            endif
        elseif (wrap == 2 .and. (i <= 1 .or. i > (ni-wrap))) then
            if (i == 1) then
                imoins1 = ni
                iplus1 = 2
                iplus2 = 3
                x1 = ax(ni) - 360.0
                x2 = ax(1)
                x3 = ax(2)
                x4 = ax(3)
            endif

            if (i == (ni-1)) then
                imoins1 = ni - 2
                iplus1 = ni
                iplus2 = 1
                x1 = ax(ni-2)
                x2 = ax(ni-1)
                x3 = ax(ni)
                x4 = ax(1) + 360.0
            endif

            if (i == ni) then
                imoins1 = ni - 1
                iplus1 = 1
                iplus2 = 2
                x1 = ax(ni-1)
                x2 = ax(ni)
                x3 = ax(1) + 360.0
                x4 = ax(2) + 360.0
            endif

            if (i /= 1 .and. i /= (ni-1) .and. i /= ni) then
            print *, 'Maudit probleme'
            print *, 'i, ni, x = ', i, ni, x
            endif
        else
            x1 = ax(imoins1)
            x2 = ax(i)
            x3 = ax(iplus1)
            x4 = ax(iplus2)
        endif

        x = x2 + (x3-x2)*(px(n)-i)
        y = ay(j) + (ay(j+1)-ay(j))*(py(n)-j)

        cx(1) = 1.0/(x2-x1)
        cx(2) = 1.0/(x3-x1)
        cx(3) = 1.0/(x3-x2)
        cx(4) = 1.0/(x4-x1)
        cx(5) = 1.0/(x4-x2)
        cx(6) = 1.0/(x4-x3)

        y1 = ay(j-1)
        y2 = ay(j)
        y3 = ay(j+1)
        y4 = ay(j+2)

        ! Interpolation 1ere rangee selon x
        z1 = z(imoins1, j-1)
        z2 = z(i  , j-1)
        z3 = z(iplus1, j-1)
        z4 = z(iplus2, j-1)

        a11 = z1
        a12 = fa2(cx(1), z1, z2)
        a13 = fa3(cx(1), cx(2), cx(3), z1, z2, z3)
        a14 = fa4(cx(1), cx(2), cx(3), cx(4), cx(5), cx(6),         z1, z2, z3, z4)
        b1 = fa(a11, a12, a13, a14, x, x1, x2, x3)

        ! Interpolation 2eme rangee selon x
        z1 = z(imoins1, j)
        z2 = z(i  , j)
        z3 = z(iplus1, j)
        z4 = z(iplus2, j)

        a21 = z1
        a22 = fa2(cx(1), z1, z2)
        a23 = fa3(cx(1), cx(2), cx(3), z1, z2, z3)
        a24 = fa4(cx(1), cx(2), cx(3), cx(4),         cx(5), cx(6), z1, z2, z3, z4)
        b2 = fa(a21, a22, a23, a24, x, x1, x2, x3)

        ! Interpolation 3eme rangee selon x
        z1 = z(imoins1, j+1)
        z2 = z(i  , j+1)
        z3 = z(iplus1, j+1)
        z4 = z(iplus2, j+1)

        a31 = z1
        a32 = fa2(cx(1), z1, z2)
        a33 = fa3(cx(1), cx(2), cx(3), z1, z2, z3)
        a34 = fa4(cx(1), cx(2), cx(3), cx(4),         cx(5), cx(6), z1, z2, z3, z4)
        b3 = fa(a31, a32, a33, a34, x, x1, x2, x3)

        ! Interpolation 4eme rangee selon x
        z1 = z(imoins1, j+2)
        z2 = z(i  , j+2)
        z3 = z(iplus1, j+2)
        z4 = z(iplus2, j+2)

        a41 = z1
        a42 = fa2(cx(1), z1, z2)
        a43 = fa3(cx(1), cx(2), cx(3), z1, z2, z3)
        a44 = fa4(cx(1), cx(2), cx(3), cx(4),         cx(5), cx(6), z1, z2, z3, z4)
        b4 = fa(a41, a42, a43, a44, x, x1, x2, x3)

        ! Interpolation finale selon y
        cy(1) = 1.0 / (y2-y1)
        cy(2) = 1.0 / (y3-y1)
        cy(3) = 1.0 / (y3-y2)
        cy(4) = 1.0 / (y4-y1)
        cy(5) = 1.0 / (y4-y2)
        cy(6) = 1.0 / (y4-y3)

        b11 = b1
        b12 = fa2(cy(1), b1, b2)
        b13 = fa3(cy(1), cy(2), cy(3), b1, b2, b3)
        b14 = fa4(cy(1), cy(2), cy(3), cy(4),         cy(5), cy(6), b1, b2, b3, b4)
        zo(n) = real(fa(b11, b12, b13, b14, y, y1, y2, y3))
    enddo

    return
    contains
#include "fa8.cdk"
end
