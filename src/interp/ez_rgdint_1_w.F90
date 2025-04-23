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


subroutine ez_rgdint_1_w(zo, px, py, npts, z, ni, j1, j2, wrap)
    use iso_fortran_env, only: real64
    implicit none

    integer, intent(in) :: npts, ni, j1, j2, wrap
    real, intent(out) :: zo(npts)
    real, intent(in) :: px(npts), py(npts)
    real, intent(in) :: z(ni, j1:j2)

    integer :: i, j, n, limite, iplus1
    real(kind = real64) :: dx, dy, y2, y3

    limite = ni + 2 - wrap
    do n = 1, npts
        i = min(ni - 2 + wrap, max(1, ifix(px(n))))
        j = min(j2 - 1, max(j1, ifix(py(n))))

        iplus1 = i + 1
        if (wrap > 0 .and. (i == (ni - 2 + wrap))) then
            iplus1 = mod(limite + i + 1, limite)
        endif

        dx = px(n) - i
        dy = py(n) - j

        y2 = zlin(dble(z(i, j)), dble(z(iplus1, j)), dx)
        y3 = zlin(dble(z(i, j + 1)), dble(z(iplus1, j + 1)), dx)

        zo(n) = real(zlin(y2, y3, dy))
    enddo

    return
    contains
#include "zlin8.cdk"
end
