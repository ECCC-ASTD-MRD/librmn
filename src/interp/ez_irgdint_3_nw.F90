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
subroutine ez_irgdint_3_nw(zo, px, py, npts, ax, ay, cx, cy, z, i1, i2, j1, j2)
    use iso_fortran_env, only: real64
    implicit none

    !> Number of points to interpolate
    integer, intent(in) :: npts
    integer, intent(in) :: i1
    integer, intent(in) :: i2
    integer, intent(in) :: j1
    integer, intent(in) :: j2
    !> Interpolated values
    real, intent(out) :: zo(npts)
    !> X position of the points to interpolate
    real, intent(in) :: px(npts)
    !> Y position of the points to interpolate
    real, intent(in) :: py(npts)
    !> Point position on the X axis
    real, intent(in) :: ax(i1:i2)
    !> Point position on the Y axis
    real, intent(in) :: ay(j1:j2)
    !> Differences on X
    real, intent(in) :: cx(i1:i2, 6)
    !> Differences on Y
    real, intent(in) :: cy(j1:j2, 6)
    !> Values on the source grid
    real z(i1:i2, j1:j2)

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

    integer             :: i, j, n
    real(kind = real64) :: a11, a12, a13, a14, a21, a22, a23, a24
    real(kind = real64) :: a31, a32, a33, a34, a41, a42, a43, a44
    real(kind = real64) :: b1, b2, b3, b4, b11, b12, b13, b14
    real(kind = real64) :: x, y
    real(kind = real64) :: x1, x2, x3, x4, y1, y2, y3, y4
    real(kind = real64) :: z1, z2, z3, z4

    do n=1, npts
        i = min(i2-2, max(i1+1, ifix(px(n))))
        j = min(j2-2, max(j1+1, ifix(py(n))))

        x = ax(i) + (ax(i+1)-ax(i))*(px(n)-i)
        y = ay(j) + (ay(j+1)-ay(j))*(py(n)-j)

        x1=ax(i-1)
        x2=ax(i)
        x3=ax(i+1)
        x4=ax(i+2)

        y1=ay(j-1)
        y2=ay(j)
        y3=ay(j+1)
        y4=ay(j+2)

        ! Interpolation 1ere rangee selon x
        z1=z(i-1, j-1)
        z2=z(i  , j-1)
        z3=z(i+1, j-1)
        z4=z(i+2, j-1)

        a11 = z1
        a12 = fa2(dble(cx(i, 1)), z1, z2)
        a13 = fa3(dble(cx(i, 1)), dble(cx(i, 2)), dble(cx(i, 3)), z1, z2, z3)
        a14 = fa4(dble(cx(i, 1)), dble(cx(i, 2)), dble(cx(i, 3)), dble(cx(i, 4)), dble(cx(i, 5)), dble(cx(i, 6)), z1, z2, z3, z4)
        b1  = fa(a11, a12, a13, a14, x, x1, x2, x3)

        ! Interpolation 2eme rangee selon x
        z1=z(i-1, j)
        z2=z(i  , j)
        z3=z(i+1, j)
        z4=z(i+2, j)

        a21 = z1
        a22 = fa2(dble(cx(i, 1)), z1, z2)
        a23 = fa3(dble(cx(i, 1)), dble(cx(i, 2)), dble(cx(i, 3)), z1, z2, z3)
        a24 = fa4(dble(cx(i, 1)), dble(cx(i, 2)), dble(cx(i, 3)), dble(cx(i, 4)), dble(cx(i, 5)), dble(cx(i, 6)), z1, z2, z3, z4)
        b2  = fa(a21, a22, a23, a24, x, x1, x2, x3)

        ! Interpolation 3eme rangee selon x
        z1=z(i-1, j+1)
        z2=z(i  , j+1)
        z3=z(i+1, j+1)
        z4=z(i+2, j+1)

        a31 = z1
        a32 = fa2(dble(cx(i, 1)), z1, z2)
        a33 = fa3(dble(cx(i, 1)), dble(cx(i, 2)), dble(cx(i, 3)), z1, z2, z3)
        a34 = fa4(dble(cx(i, 1)), dble(cx(i, 2)), dble(cx(i, 3)), dble(cx(i, 4)), dble(cx(i, 5)), dble(cx(i, 6)), z1, z2, z3, z4)
        b3  = fa(a31, a32, a33, a34, x, x1, x2, x3)

        ! Interpolation 4eme rangee selon x
        z1=z(i-1, j+2)
        z2=z(i  , j+2)
        z3=z(i+1, j+2)
        z4=z(i+2, j+2)

        a41 = z1
        a42 = fa2(dble(cx(i, 1)), z1, z2)
        a43 = fa3(dble(cx(i, 1)), dble(cx(i, 2)), dble(cx(i, 3)), z1, z2, z3)
        a44 = fa4(dble(cx(i, 1)), dble(cx(i, 2)), dble(cx(i, 3)), dble(cx(i, 4)), dble(cx(i, 5)), dble(cx(i, 6)), z1, z2, z3, z4)
        b4  = fa(a41, a42, a43, a44, x, x1, x2, x3)

        ! Interpolation finale selon y
        b11 = b1
        b12 = fa2(dble(cy(j, 1)), b1, b2)
        b13 = fa3(dble(cy(j, 1)), dble(cy(j, 2)), dble(cy(j, 3)), b1, b2, b3)
        b14 = fa4(dble(cy(j, 1)), dble(cy(j, 2)), dble(cy(j, 3)), dble(cy(j, 4)), dble(cy(j, 5)), dble(cy(j, 6)), b1, b2, b3, b4)
        zo(n) = fa(b11, b12, b13, b14, y, y1, y2, y3)
    enddo

    return
    contains
#include "fa8.cdk"
end
