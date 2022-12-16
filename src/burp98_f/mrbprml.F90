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

!> Extract descriptor parameters from all blocks
integer function mrbprml(buf, inbkno, tblprm, nprm, inblocs)
    use app
    implicit none

    integer, intent(in) :: buf(*)
    integer, intent(in) :: inbkno
    integer, intent(in) :: nprm
    integer, intent(in) :: inblocs
    integer, intent(out) :: tblprm(nprm, inblocs)
!
!AUTEUR  J. CAVEEN   OCTOBRE 1990
!REV 001 Y. BOURASSA MARS    1995 RATFOR @ FTN77
!rev 002 j. caveen   sept    1995 elimine bdsec et bfam passe a 12 bits
!
!OBJET( mrbprml )
!     FONCTION SERVANT A RETOURNER DANS LE TABLEAU tblprm
!     LES PARAMETRES DESCRIPTEURS DES inblocs BLOCS A PARTIR 
!     DU BLOC SUIVANT LE BLOC NUMERO bkno.
!                                                                       
!ARGUMENTS
!     buf        ENTREE    VECTEUR CONTENANT LE RAPPORT
!     inbkno        "      NUMERO D'ORDRE DU PREMIER BLOC
!     nprm          "      NOMBRE DE PARAMETRES A EXTRAIRE (DIM 1 DE tblprm)
!     inblocs       "      NOMBRE DE BLOCS DONT ON VEUT LES PARAMETRES
!     tblprm     SORTIE    TABLEAU CONTENANT LES PARAMETRES DES inblocs
!
!     STRUCTURE DE tblprm(nprm, inblocs)
!     tblprm(1,I) - NUMERO DU BLOC I
!     tblprm(2,I) - NOMBRE D'ELEMENTS DANS LE BLOC I  (NELE)
!     tblprm(3,I) - NOMBRE DE VALEURS  PAR ELEMENT    (NVAL)
!     tblprm(4,I) - NOMBRE DE PAS DE TEMPS            (NT)
!     tblprm(5,I) - FAMILLE DU BLOC                   (BFAM) (12 bits)
!     tblprm(6,I) - DESCRIPTEUR DE BLOC               (BDESC) (mis a zero)
!     tblprm(7,I) - TYPE DU BLOC                      (BTYP)
!     tblprm(8,I) - NOMBRE DE BITS PAR ELEMENT        (NBIT)
!     tblprm(9,I) - NUMERO DU PREMIER BIT             (BIT0)
!     tblprm(10,I)- TYPE DE DONNEES POUR COMPACTION   (DATYP)
!
!IMPLICITES
#include "defi.cdk"
#include "bpl.cdk"
#include "codes.cdk"
#include <ftnmacros.hf>

    EXTERNAL XDFXTR, getbuf8
    INTEGER  XDFXTR, getbuf8

    integer :: ENTETE(DIMENT), BITPOS, nblocs, bkno, nobl
    integer :: famdesc

    mrbprml = -1

    !  S'ASSURER QUE LES DIMENSIONS DU TABLEAU SONT ADEQUATES
    IF (nprm .NE. NPRMMAX) THEN
        write(app_msg,*) 'MRBPRML: Dimensions de TBLPRM incorrectes'
        call Lib_Log(APP_LIBFST,APP_ERROR,app_msg)       
        mrbprml = ERNPRM
        RETURN
    ENDIF

    ! BLOC DE DEPART
    bkno = MAX(inbkno, 0)

    !  NOMBRE DE BLOCS A EXTRAIRE
    nblocs = MIN(inblocs, getbuf8(buf))

    ! EXTRAIRE TOUTES LES ENTETES DE BLOCS
    DO nobl = 1, nblocs
        ! ADRESSE DU BLOC
        BITPOS = bkno * NBENTB
        ! EXTRACTION DE L'ENTETE DU BLOC
        mrbprml = XDFXTR(buf, ENTETE, BITPOS, DIMENT, BITMOT, 0)
        tblprm(1,nobl)  = bkno + 1
        famdesc         = GETBIT(ENTETE, BPFMDSC,   LFMDSC)
        tblprm(6,nobl)  = 0
        tblprm(7,nobl)  = GETBIT(ENTETE, BPTYP,   LTYP)
        tblprm(8,nobl)  = GETBIT(ENTETE, BPNBIT,  LNBIT) + 1
        tblprm(9,nobl)  = GETBIT(ENTETE, BPBIT0,  LBIT0)
        tblprm(10,nobl) = GETBIT(ENTETE, BPDATYP, LDATYP)
        tblprm(2,nobl)  = GETBIT(ENTETE, BPNELE,  LNELE)
        IF (tblprm(2,nobl) .GE. GRONELE) THEN
            tblprm(2,nobl) = GETBIT(ENTETE, BPNELE16, LNELE16)
            tblprm(3,nobl) = GETBIT(ENTETE, BPNVAL16, LNVAL16)
            tblprm(4,nobl) = GETBIT(ENTETE, BPNT16,   LNT16)
        ELSE
            tblprm(3,nobl) = GETBIT(ENTETE, BPNVAL, LNVAL)
            tblprm(4,nobl) = GETBIT(ENTETE, BPNT,   LNT)
        ENDIF

        ! construire bfam a partir de famdesc 
        ! (interchange 6 bits du bas avec 6 bits du haut)

        tblprm(5,nobl) = LSHIFT(IAND(famdesc,RMASK(6)),6)
        tblprm(5,nobl) = IOR(tblprm(5,nobl),(IAND(RSHIFT(famdesc,6),RMASK(6))))
 
         bkno = bkno + 1
    end do

        mrbprml = nblocs
END
