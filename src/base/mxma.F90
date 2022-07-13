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


!> Multiply 2 matrices with arbitrary element spacing
!> \author Martin J. McBride
!> \date 1985-08-21
!> General Electric CRD, Information System Operation.
!> Converted to Fortran 2008 by S. Gilbert 2022-07-13
subroutine mxma8(a, na, iad, b, nb, ibd, c, nc, icd, nar, nac, nbc)
    use iso_fortran_env
    implicit none

    integer, intent(in) :: na
    integer, intent(in) :: iad
    integer, intent(in) :: nb
    integer, intent(in) :: ibd
    integer, intent(in) :: nc
    integer, intent(in) :: icd
    integer, intent(in) :: nar
    integer, intent(in) :: nac
    integer, intent(in) :: nbc

    !> First matrix to multiply
    real(kind = REAL64), dimension(nar * na, nac * iad), intent(in) :: a
    !> Second matrix to multiply
    real(kind = REAL64), dimension(nac * nb, nbc * ibd), intent(in) :: b
    !> Result matrix
    real(kind = REAL64), dimension(nar * nc, nbc * icd), intent(out) :: c

    ! Local variables
    integer :: ac
    integer :: ar
    integer :: bc
    integer :: br
    integer :: cc
    integer :: cr
    integer :: i
    integer :: j
    integer :: k

    bc = 1
    cc = 1
    do j = 1, nbc
        cr = 1
        ar = 1
        do i = 1, nar
            c(cr, cc) = 0.0
            ac = 1
            br = 1
            do k = 1, nac
                c(cr, cc) = c(cr, cc) + a(ar, ac) * b(br, bc)
                ac = ac + iad
                br = br + nb
            end do
            cr = cr + nc
            ar = ar + na
        end do
        cc = cc + icd
        bc = bc + ibd
    end do
end subroutine mxma8
