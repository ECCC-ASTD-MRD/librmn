!/* RMNLIB - Library of useful routines for C and FORTRAN programming
! * Copyright (C) 1975-2001  Division de Recherche en Prevision Numerique
! *                          Environnement Canada
! *
! * This library is free software; you can redistribute it and/or
! * modify it under the terms of the GNU Lesser General Public
! * License as published by the Free Software Foundation,
! * version 2.1 of the License.
! *
! * This library is distributed in the hope that it will be useful,
! * but WITHOUT ANY WARRANTY; without even the implied warranty of
! * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
! * Lesser General Public License for more details.
! *
! * You should have received a copy of the GNU Lesser General Public
! * License along with this library; if not, write to the
! * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
! * Boston, MA 02111-1307, USA.
! */


!> \file


!> Extract descriptor parameters from all blocks
integer function mrbprml(buf, inbkno, tblprm, nprm, inblocs)
    use app
    use rmn_burp, only: bpbit0, bpdatyp, bpfmdsc, bpnbit, bpnele, bpnele16, bpnt, bpnt16, bpnval, bpnval16, &
        bptyp, diment, ernprm, gronele, lbit0, ldatyp, lfmdsc, lnbit, lnele, lnele16, lnt, lnt16, lnval, lnval16, &
        ltyp, nbentb, nprmmax
    implicit none

    integer, intent(in) :: buf(*)
    integer, intent(in) :: inbkno
    integer, intent(in) :: nprm
    integer, intent(in) :: inblocs
    integer, intent(out) :: tblprm(nprm, inblocs)

    !     FONCTION SERVANT A RETOURNER DANS LE TABLEAU tblprm
    !     LES PARAMETRES DESCRIPTEURS DES inblocs BLOCS A PARTIR
    !     DU BLOC SUIVANT LE BLOC NUMERO bkno.
    !ARGUMENTS
    !     buf        ENTREE    VECTEUR CONTENANT LE RAPPORT
    !     inbkno        "      NUMERO D'ORDRE DU PREMIER BLOC
    !     nprm          "      NOMBRE DE PARAMETRES A EXTRAIRE (DIM 1 DE tblprm)
    !     inblocs       "      NOMBRE DE BLOCS DONT ON VEUT LES PARAMETRES
    !     tblprm     SORTIE    TABLEAU CONTENANT LES PARAMETRES DES inblocs
    !
    !     STRUCTURE DE tblprm(nprm, inblocs)
    !     tblprm(1, I) - NUMERO DU BLOC I
    !     tblprm(2, I) - NOMBRE D'ELEMENTS DANS LE BLOC I  (NELE)
    !     tblprm(3, I) - NOMBRE DE VALEURS  PAR ELEMENT    (NVAL)
    !     tblprm(4, I) - NOMBRE DE PAS DE TEMPS            (NT)
    !     tblprm(5, I) - FAMILLE DU BLOC                   (BFAM) (12 bits)
    !     tblprm(6, I) - DESCRIPTEUR DE BLOC               (BDESC) (mis a zero)
    !     tblprm(7, I) - TYPE DU BLOC                      (BTYP)
    !     tblprm(8, I) - NOMBRE DE BITS PAR ELEMENT        (NBIT)
    !     tblprm(9, I) - NUMERO DU PREMIER BIT             (BIT0)
    !     tblprm(10, I)- TYPE DE DONNEES POUR COMPACTION   (DATYP)

    ! For BITMOT, GETBIT, RMASK
#include <ftnmacros.hf>

    integer, external :: xdfxtr, getbuf8

    integer :: entete(diment), bitpos, nblocs, bkno, nobl
    integer :: famdesc

    mrbprml = -1

    !  s'assurer que les dimensions du tableau sont adequates
    if (nprm .ne. nprmmax) then
        write(app_msg, *) 'MRBPRML: Dimensions de TBLPRM incorrectes'
        call lib_log(app_libfst, app_error, app_msg)
        mrbprml = ernprm
        return
    endif

    ! bloc de depart
    bkno = max(inbkno, 0)

    !  nombre de blocs a extraire
    nblocs = min(inblocs, getbuf8(buf))

    ! extraire toutes les entetes de blocs
    do nobl = 1, nblocs
        ! adresse du bloc
        bitpos = bkno * nbentb
        ! extraction de l'entete du bloc
        mrbprml = xdfxtr(buf, entete, bitpos, diment, BITMOT, 0)
        tblprm(1, nobl)  = bkno + 1
        famdesc         = GETBIT(entete, bpfmdsc,   lfmdsc)
        tblprm(6, nobl)  = 0
        tblprm(7, nobl)  = GETBIT(entete, bptyp,   ltyp)
        tblprm(8, nobl)  = GETBIT(entete, bpnbit,  lnbit) + 1
        tblprm(9, nobl)  = GETBIT(entete, bpbit0,  lbit0)
        tblprm(10, nobl) = GETBIT(entete, bpdatyp, ldatyp)
        tblprm(2, nobl)  = GETBIT(entete, bpnele,  lnele)
        if (tblprm(2, nobl) .ge. gronele) then
            tblprm(2, nobl) = GETBIT(entete, bpnele16, lnele16)
            tblprm(3, nobl) = GETBIT(entete, bpnval16, lnval16)
            tblprm(4, nobl) = GETBIT(entete, bpnt16,   lnt16)
        else
            tblprm(3, nobl) = GETBIT(entete, bpnval, lnval)
            tblprm(4, nobl) = GETBIT(entete, bpnt,   lnt)
        endif

        ! construire bfam a partir de famdesc
        ! (interchange 6 bits du bas avec 6 bits du haut)

        tblprm(5, nobl) = lshift(iand(famdesc, RMASK(6)), 6)
        tblprm(5, nobl) = ior(tblprm(5, nobl), (iand(rshift(famdesc, 6), RMASK(6))))

         bkno = bkno + 1
    end do

    mrbprml = nblocs
end
