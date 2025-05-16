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


!> Interpolation bi-cubique de points a partir d'une grille source irreguliere.
subroutine ez_iaxint(zo, px, npts, ax, z, ni, i1, i2, ordint)
    use interp_mod, only: fa32, fa32_2, fa32_3, fa32_4, zlin32
    implicit none

    !> Number of points to interpolate
    integer, intent(in) :: npts
    !> ???
    integer, intent(in) :: ni
    !> Source grid X start index
    integer, intent(in) :: i1
    !> Source grid X end index
    integer, intent(in) :: i2
    !> Interpolation method (LINEAR or CUBIC)
    integer, intent(in) :: ordint
    !> Interpolated values
    real, intent(out) :: zo(npts)
    !> X positions where to interpolate
    real, intent(in) :: px(npts)
    !> X point positions
    real, intent(in) :: ax(i1:i2)
    !> Source grid values
    real, intent(in) :: z(ni)

    !> \ingroup ezscint

    !  *   *   *   *
    !
    !  *   *   *   *
    !        #        .eq.>   pt (x, y)
    !  *  (=)  *   *  .eq.> = pt (i, j)
    !
    !  *   *   *   *

    !  structure identique pour cy(j, 1..6)

    integer :: i, n
    real :: a11, a12, a13, a14
    real :: x, dx

#include "ez_def_shared.h"

    if (ordint == CUBIQUE) then
        do n = 1, npts
            i = min(ni - 2, max(2, ifix(px(n))))

            x = ax(i) + (ax(i + 1) - ax(i)) * (px(n) - i)

            a11 = z(i - 1)
            a12 = fa32_2((1.0 / (ax(i) - ax(i - 1))), z(i - 1), z(i))
            a13 = fa32_3((1.0 / (ax(i) - ax(i - 1))), 1.0 / (ax(i + 1) - ax(i - 1)), 1.0 / (ax(i + 1) - ax(i)), z(i - 1), z(i), z(i + 1))
            a14 = fa32_4((1.0 / (ax(i) - ax(i - 1))), 1.0 / (ax(i + 1) - ax(i - 1)), 1.0 / (ax(i + 1) - ax(i)), 1.0 / (ax(i + 2) - ax(i - 1)), 1.0 / (ax(i + 2) - ax(i)), 1.0 / (ax(i + 2) - ax(i + 1)), z(i - 1), z(i), z(i + 1), z(i + 2))
            zo(n) = fa32(a11, a12, a13, a14, x, ax(i - 1), ax(i), ax(i + 1))
        end do
    endif

    if (ordint == LINEAIRE) then
        do n = 1, npts
            i = min(i2 - 1, max(i1, ifix(px(n))))
            x = ax(i) + (ax(i + 1) - ax(i)) * (px(n) - i)
            dx = (x - ax(i)) / (ax(i + 1) - ax(i))
            zo(n) = zlin32((z(i)), z(i + 1), dx)
        end do
    endif

    if (ordint == VOISIN) then
        do n = 1, npts
            i = min(i2, max(i1, nint(px(n))))
            zo(n) = z(i)
        end do
    endif
end
