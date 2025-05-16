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


!> Encode the elements in a list
integer function mrbcol(liste, cliste, nele)
    use rmn_burp, only: bp2b, bp6b, msk2b, msk6b
    implicit none

    !> Number of elements in the list
    integer, intent(in) :: nele
    !> Input list of elements to encode
    integer, intent(in) :: liste(nele)
    !> Output list of encoded elements
    integer, intent(out) :: cliste(nele)

    !> Encode each elements such as that it occupies 16 bits
    !> For an element having the decimal format abbccc, (a,b,c being digits from 0 to 9),
    !> an integer with a encoded on 2 bits, bb on 6 bits, and ccc on 8 bits is returned.

    integer :: i, iibit, vibit, viiibit, ielem

    mrbcol = -1
    do i = 1,nele
        ielem     = liste(i)
        iibit     = ielem / 100000
        ielem     = mod(ielem, 100000)
        vibit     = ielem / 1000
        viiibit   = mod(ielem, 1000)
        cliste(i) = viiibit
        cliste(i) = ior(cliste(i), lshift(iand(vibit, msk6b), bp6b))
        cliste(i) = ior(cliste(i), lshift(iand(iibit, msk2b), bp2b))
    end do

    mrbcol = 0
end

!> Decode the elements in a list
integer function mrbdcl(cliste, liste, nele)
    use rmn_burp, only: bp2b, bp6b, msk2b, msk6b, msk8b
    implicit none

    !> Number of elements in the list
    integer, intent(in) :: nele
    !> Output list of decoded elements
    integer, intent(out) :: liste(nele)
    !> Input list of encoded elements
    integer, intent(in) :: cliste(nele)

    !     pour un element, on retourne sa valeur sous format decimal  abbccc,
    !                                                       (a,b,c de 0 a 9)
    !     ou a    provient des bits 14 et 15 de l'element
    !        bb       "    des bits 8 a 13 de l'element
    !        ccc      "    des bits 0 a 7  de l'element

    integer :: i, iibit, vibit, viiibit, ielem

    mrbdcl = -1
    do i = 1, nele
        ielem    = cliste(i)
        viiibit  = iand(ielem, msk8b)
        vibit    = iand(rshift(ielem, bp6b), msk6b)
        iibit    = iand(rshift(ielem, bp2b), msk2b)
        liste(i) = (iibit * 100000) + (vibit * 1000) + viiibit
    end do

    mrbdcl = 0
end
