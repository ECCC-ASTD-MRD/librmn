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
!/


!> \file


!> Get the 16 bit representation of an element
integer function mrbcov( elem )
    use rmn_burp, only: bp2b, bp6b, msk2b, msk6b
    implicit none

    !> Decimal value of an element
    integer, intent(in) :: elem

    !     pour un element ayant le format decimal  abbccc, (a,b,c de 0 a 9)
    !     on retourne un entier contenant a sur deux bits, bb sur six bits
    !     et ccc sur huit bits

    integer :: ielem, iibit, vibit, viiibit

    ielem   = elem
    iibit   = ielem / 100000
    ielem   = mod(ielem, 100000)
    vibit   = ielem / 1000
    viiibit = mod(ielem, 1000)
    mrbcov  = viiibit
    mrbcov  = ior(mrbcov, lshift(iand(vibit, msk6b), bp6b))
    mrbcov  = ior(mrbcov, lshift(iand(iibit, msk2b), bp2b))
end
