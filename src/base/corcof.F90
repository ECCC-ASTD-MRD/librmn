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


! \file


!> Compute correlation coefficient
subroutine corcof(cc, fi, ff, fv, w, ni, nj, iw1, iw2, nw1, nw2)
    use iso_fortran_env, only: real64
    implicit none

    !> X dimension
    integer, intent(in) :: ni
    !> Y dimension
    integer, intent(in) :: nj
    !> Bottom left corner of x-coordinate of window
    integer, intent(in) :: iw1
    !> Bottom left corner of y-coordinate of window
    integer, intent(in) :: iw2
    !> Upper right corner of x-coordinate of window
    integer, intent(in) :: nw1
    !> Upper right corner of y-coordinate of window
    integer, intent(in) :: nw2
    !> Initial field
    real, intent(in) :: fi(ni, nj)
    !> Forecast field
    real, intent(in) :: ff(ni, nj)
    !> Verification field
    real, intent(in) :: fv(ni, nj)
    !> Weight field
    real, intent(in) :: w(ni, nj)
    ! > Correlation coefficient
    real, intent(out) :: cc

    !> Compute the correlation coefficient given an initial field, a forecast and a verifying field

    !> If the coordinates of the window are outside the limits of the arrays, cc is set to 99999.

    !> The correlation coefficient is between the actual change in the field (da = fv-fi) and the
    !> predicted change in the field (df = ff-fi).

    !> To obtain cc, the (w-weighted) means from da and df is removed and then
    !> cc = mean(da*df) / sqrt(mean(da)**2 * mean(df)**2) is computed.

    !> If da, df or w are pathological, cc is set to 99999.

    real(kind = real64) :: r(5), ffi, da, df, dw, ftw, x, y, a, b, c
    integer :: i, j

    cc = 99999.0
    do i = 1, 5
        r(i) = 0.0
    end do

    !  test to see if the window co-ordinates are inside the
    !  limits of the arrays dimensioned (ni*nj)

    if (ni .lt. 1 .or. ni .lt. iw1 .or. ni .lt. nw1) return
    if (nj .lt. 1 .or. nj .lt. iw2 .or. nj .lt. nw2) return
    if (nw1 .lt. iw1 .or. nw2 .lt. iw2) return

    ftw = 0.0

    do j = iw2, nw2
        do i = iw1, nw1
            ffi = fi(i, j)
            da = fv(i, j) - ffi
            df = ff(i, j) - ffi
            dw = w(i, j)
            ftw = ftw + dw
            x = dw * da
            y = dw * df
            r(1) = r(1) + (x * df)
            r(2) = r(2) + (y * df)
            r(3) = r(3) + (x * da)
            r(4) = r(4) + (y)
            r(5) = r(5) + (x)
        end do
    end do

    if (ftw .eq. 0) return

    do i = 1, 5
        r(i) = r(i) / ftw
    end do

    a = r(1) - r(4) * r(5)
    b = r(2) - r(4) * r(4)
    c = r(3) - r(5) * r(5)
    if (b .eq. 0. .or. c .eq. 0.) return
    cc = real(a / sqrt(b * c))
end
