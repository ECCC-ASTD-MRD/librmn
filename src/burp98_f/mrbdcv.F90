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


!> Get the decimal value of a 16 bits encoded element
integer function mrbdcv( elem )
    use rmn_burp, only: bp2b, bp6b, msk2b, msk6b, msk8b
    implicit none

    !> 16 bit encoded element
    integer, intent(in) :: elem

    !     pour un element, on retourne sa valeur sous format decimal  abbccc,
    !                                                        (a,b,c de 0 a 9)
    !     ou a    provient des bits 14 et 15 de l'element
    !        bb       "    des bits 8 a 13 de l'element
    !        ccc      "    des bits 0 a 7  de l'element

    integer :: ielem, iibit, vibit, viiibit

    ielem   = elem
    viiibit = iand(ielem, msk8b)
    vibit   = iand(rshift(ielem, bp6b), msk6b)
    iibit   = iand(rshift(ielem, bp2b), msk2b)
    mrbdcv  = iibit * 100000 + vibit * 1000 + viiibit
end
