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


!> extraire les parametres descripteurs d'un bloc
integer function mrbprm(buf, bkno, nele, nval, nt, bfam, bdesc, btyp, nbit, bit0, datyp)
    use rmn_burp, only: diment, lfmdsc, bpnele16, bpbit0, bptyp, bpnbit, bpdatyp, bpfmdsc, bpnele, &
        bpnt, bpnt16, bpnval, bpnval16, gronele, lbit0, ldatyp, lnbit, lnele, lnele16, lnt, lnt16, &
        lnval, lnval16, ltyp, nbentb
    implicit none

    !> Array containing the block to be decoded
    integer, intent(in) :: buf(*)
    !> Block sequence number
    integer, intent(in) :: bkno
    !> Number of elements
    integer, intent(out) :: nele
    !> Number of values per element
    integer, intent(out) :: nval
    !> Number of nele*nval values
    integer, intent(out) :: nt
    !> Block family (12 bits)
    integer, intent(out) :: bfam
    !> Block descriptor (set to 0)
    integer, intent(out) :: bdesc
    !> Block type
    integer, intent(out) :: btyp
    !> Number of bits per value
    integer, intent(out) :: nbit
    !> First bit of the value array
    integer, intent(out) :: bit0
    !> Compaction type
    integer, intent(out) :: datyp

    !> \return 0 on success, error code otherwise

    ! For BITMOT, GETBIT, RMASK
#include <ftnmacros.hf>

    integer, external :: xdfxtr

    integer  entete(diment), bitpos
    integer famdesc

    mrbprm = -1

    ! extraire l'entete du bloc
    bitpos = (bkno - 1) * nbentb
    mrbprm = xdfxtr(buf, entete, bitpos, diment, BITMOT, 0)
    !> \todo Shoudln't the return value of xdfxtr be checked!?

    ! extraire du bloc les differents parametres
    famdesc = GETBIT(entete, bpfmdsc, lfmdsc)
    btyp   = GETBIT(entete, bptyp,   ltyp)
    nbit   = GETBIT(entete, bpnbit,  lnbit) + 1
    bit0   = GETBIT(entete, bpbit0,  lbit0)
    datyp  = GETBIT(entete, bpdatyp, ldatyp)
    nele   = GETBIT(entete, bpnele,  lnele)

    if(nele .ge. gronele) then
        nele = GETBIT(entete, bpnele16, lnele16)
        nval = GETBIT(entete, bpnval16, lnval16)
        nt   = GETBIT(entete, bpnt16,   lnt16)
    else
        nval = GETBIT(entete, bpnval, lnval)
        nt   = GETBIT(entete, bpnt,   lnt)
    endif

    ! construire bfam a partir de famdesc 
    ! (interchange 6 bits du bas avec 6 bits du haut)
    bfam = lshift(iand(famdesc, RMASK(6)),6)
    bfam = ior(bfam, iand(rshift(famdesc, 6), RMASK(6)))

    bdesc = 0

    mrbprm = 0
end
