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
!.S MRBCOL
module mrb_col_dcl
      use rmn_burp_defi
#include <ftnmacros.hf>
#include "masques.cdk"
end module
!**S/P - MRBCOL - CODER LES ELEMENTS D'UNE LISTE
      integer FUNCTION MRBCOL( LISTE,       CLISTE,       NELE)
      use mrb_col_dcl
      IMPLICIT NONE
      INTEGER  NELE, LISTE(NELE), CLISTE(NELE)

!AUTEUR:  J. CAVEEN   FEVRIER 1991
!REV 001  Y. BOURASSA MARS    1995 RATFOR & FTN77
!    
!OBJET( MRBCOL )
!     SOUS-PROGRAMME RETOURNANT UNE LISTE D'ELEMENTS CODES DE TELLE SORTE
!     QUE CHAQUE ELEMENT OCCUPE SEIZE BITS.
!     POUR UN ELEMENT AYANT LE FORMAT DECIMAL  ABBCCC, (A,B,C DE 0 A 9)
!     ON RETOURNE UN ENTIER CONTENANT A SUR DEUX BITS, BB SUR SIX BITS
!     ET CCC SUR HUIT BITS
!
!ARGUMENTS
!     LISTE   ENTREE  LISTE DES ELEMENTS A CODER
!     CLISTE  SORTIE  LISTE DES ELEMENTS CODES
!     NELE    ENTREE  NOMBRE D'ELEMENTS A CODER
!
      INTEGER I, IIBIT, VIBIT, VIIIBIT, IELEM
!
!IMPLICITES

!
!*
      MRBCOL = -1
      DO 10 I = 1,NELE
         IELEM     = LISTE(I)
         IIBIT     = IELEM/100000
         IELEM     = MOD(IELEM,100000)
         VIBIT     = IELEM/1000
         VIIIBIT   = MOD(IELEM,1000)
         CLISTE(I) = VIIIBIT
         CLISTE(I) = IOR(CLISTE(I), LSHIFT(IAND(VIBIT, MSK6B), BP6B))
         CLISTE(I) = IOR(CLISTE(I), LSHIFT(IAND(IIBIT, MSK2B), BP2B))
   10    CONTINUE

      MRBCOL = 0
      RETURN
      end

!**S/P MRBDCL - DECODER LES ELEMENTS D'UNE LISTE
      integer function   MRBDCL(CLISTE, LISTE, NELE)
      use mrb_col_dcl
      IMPLICIT NONE
      INTEGER  NELE, LISTE(NELE), CLISTE(NELE)
!
!OBJET(MRBDCL)
!     RETOURNE UNE LISTE D'ELEMENTS DECODES
!
!     POUR UN ELEMENT, ON RETOURNE SA VALEUR SOUS FORMAT DECIMAL  ABBCCC,
!                                                       (A,B,C DE 0 A 9)
!     OU A    PROVIENT DES BITS 14 ET 15 DE L'ELEMENT
!        BB       "    DES BITS 8 A 13 DE L'ELEMENT
!        CCC      "    DES BITS 0 A 7  DE L'ELEMENT
!
!ARGUMENT
!     CLISTE   ENTREE    LISTE DES ELEMENTS A CODER
!     LISTE    SORTIE    LISTE DES ELEMENTS CODES
!     NELE     ENTREE    NOMBRE D'ELEMENTS A CODER
!*
      INTEGER I, IIBIT, VIBIT, VIIIBIT, IELEM

      MRBDCL = -1
      DO 20 I = 1, NELE
         IELEM    = CLISTE(I)
         VIIIBIT  = IAND(IELEM, MSK8B)
         VIBIT    = IAND(RSHIFT(IELEM, BP6B), MSK6B)
         IIBIT    = IAND(RSHIFT(IELEM, BP2B), MSK2B)
         LISTE(I) = (IIBIT * 100000) + (VIBIT * 1000) + VIIIBIT
   20    CONTINUE

        MRBDCL = 0

        RETURN
        END
