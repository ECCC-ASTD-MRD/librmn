*/* RMNLIB - Library of useful routines for C and FORTRAN programming
* * Copyright (C) 1975-2001  Division de Recherche en Prevision Numerique
* *                          Environnement Canada
* *
* * This library is free software; you can redistribute it and/or
* * modify it under the terms of the GNU Lesser General Public
* * License as published by the Free Software Foundation,
* * version 2.1 of the License.
* *
* * This library is distributed in the hope that it will be useful,
* * but WITHOUT ANY WARRANTY; without even the implied warranty of
* * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
* * Lesser General Public License for more details.
* *
* * You should have received a copy of the GNU Lesser General Public
* * License along with this library; if not, write to the
* * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
* * Boston, MA 02111-1307, USA.
* */
#include <ftnmacros.hf>

***S/P IEEEPAK - COMPACTION / DECOMPACTION DE REELS ("format ieee")
*
      SUBROUTINE IEEEPAK(IFLD,IPK,NI,NJ,NPAK,serpas,KIND)
      IMPLICIT NONE
      INTEGER NI,NJ,IFLD(0:*),IPK(*),NPAK,KIND,serpas
*
*AUTHOR    - M. LEPINE  - Nov 91
*
*LANGUAGE - FORTRAN
*
*OBJET(IEEEPAK)
*         - COMPACTER OU DECOMPACTER DES NOMBRES REELS 
*           en format ieee  (1 signe 8 exposant 32-nbits mantisse)
*
*PRECISION
*         - L'ALGORITHME DE COMPACTION GARDE LES NBITS BITS LES
*           MOINS SIGNIFICATIFS.
*
*LIBRAIRIES
*
*ARGUMENTS
* IN/OUT  - IFLD - INTEGER ARRAY, UNPACKED (NI,NJ)
* IN/OUT  - IPK  - THE SAME ARRAY PACKED
*   IN    - NI   - X-DIMENSION
*   IN    - NJ   - Y-DIMENSION
*   IN    - NPAK - NPAK > 0, DENSITE DE COMPACTION
*                  NPAK = 0 OU 1, PAS DE COMPACTION
*                  NPAK < 0, NOMBRE DE BITS A GARDER PAR NOMBRE
*   IN    - kind - 1 = PACK
*                  2 = UNPACK
*   in    - serpas sert pas !!
**
*
#include "xxpakc.cdk"
*
*
      INTEGER I,INDX,ITEMP,LEFT,ITEM,NINJ,INEXT
      INTEGER RBITS, RLEFT, ISAVE
*
      XXPERR = 0
      IF (NPAK .GT. 1) THEN
        NBITS = MAX(1,BITMOT/NPAK)
      ELSE IF (NPAK .LT. 0) THEN
        NBITS = MIN(64,-NPAK)
      ELSE
        NBITS = BITMOT
      ENDIF
*
      NINJ = NI*NJ
*
      RBITS = RMASK(NBITS)
      NWR = (NINJ*NBITS+BITMOT-1) / BITMOT
*

#if defined (ALL64)
      IF (NBITS .NE. 32) THEN
         print *,'IEEEPAK ERROR: NBITS MUST BE 32, NBITS=',nbits
         stop 'IEEEPAK' 
      ENDIF
      IF (KIND .EQ. 1) THEN
*                                               COMPACTER
         CALL MOVR8R4(IFLD,IPK,NINJ)
      ELSE
*                                               DECOMPACTER
         CALL MOVR4R8(IPK,IFLD,NINJ)
      ENDIF
#else

      IF (KIND .EQ. 1) THEN
*                                               COMPACTER
        IF (NBITS .GE. BITMOT) THEN
           IF (NBITS .EQ. BITMOT) THEN
              DO 10 I = 0,NWR-1
                 IPK(I+1) = IFLD(I)
 10           CONTINUE
           ELSEIF (NBITS .EQ. 64) THEN
              CALL MOVE6432(IFLD,IPK,NINJ)
           ELSE
              print *,'IEEEPAK ERROR: NBITS MUST BE <= 32 OR 64, NBITS='
     %         ,nbits
              stop 'IEEEPAK' 
           ENDIF
        ELSE
          INDX = 0
          LEFT = BITMOT
          ITEMP = 0
          DO 100 I=0,NINJ-1
            ITEM = IAND(RSHIFT(IFLD(I),32-NBITS) , RBITS)
            IF (NBITS .LE. LEFT) THEN
              ITEMP = IOR(LYSHIFT(ITEMP,NBITS) , ITEM)
              LEFT = LEFT - NBITS
            ELSE
              INDX = INDX+1
              IPK(INDX) = IOR(LYSHIFT(ITEMP,LEFT) ,
     %                        RSHIFT(ITEM,NBITS-LEFT))
              ITEMP = IAND(ITEM , RMASK(NBITS-LEFT))
              LEFT = LEFT - NBITS + BITMOT
            ENDIF
 100      CONTINUE

          IF (LEFT .LT. BITMOT) THEN
            INDX = INDX + 1
            IPK(INDX) = IOR(LYSHIFT(ITEMP,LEFT),
     %                  IAND(IPK(INDX),RMASK(LEFT)))
          ENDIF
        ENDIF

      ELSE
*                                               DECOMPACTER
        IF (NBITS .GE. BITMOT) THEN
           IF (NBITS .EQ. BITMOT) THEN
              DO 110 I = 0,NWR-1
                 IFLD(I) = IPK(I+1)
 110          CONTINUE
           ELSEIF (NBITS .EQ. 64) THEN
              CALL MOVE6432(IPK,IFLD,NINJ)
           ELSE
              print *,'IEEEPAK ERROR: NBITS MUST BE <= 32 OR 64, NBITS='
     %               ,nbits
              stop 'IEEEPAK' 
           ENDIF
        ELSE
          ITEMP = 0
          ISAVE = IPK(1+ITEMP)
          DO 120 I=NWR,2,-1
            IFLD(I-NWR+NINJ-1) = IPK(I+ITEMP)
 120      CONTINUE
          INDX = (NINJ-NWR) + 1
          LEFT = BITMOT
          ITEMP = LXSHIFT(ISAVE,BITMOT-LEFT)
          INEXT = IFLD(INDX)

          DO 200 I=0,NINJ-1
            IF (NBITS .LE. LEFT) THEN
              IFLD(I) = IAND(RYSHIFT(ITEMP,BITMOT-NBITS),RBITS)
              ITEMP = LYSHIFT(ITEMP,NBITS)
              LEFT = LEFT - NBITS
            ELSE
              RLEFT = RMASK(NBITS-LEFT)
              IFLD(I)=IOR(IAND(IAND(RYSHIFT(ITEMP,BITMOT-NBITS),RBITS),
     %            NOT(RLEFT)),IAND(RYSHIFT(INEXT,BITMOT-(NBITS-LEFT)),
     %            RLEFT))
              INDX = INDX+1
              ITEMP = LYSHIFT(INEXT,NBITS-LEFT)
              INEXT = IFLD(INDX)
              LEFT = LEFT - NBITS + BITMOT
            ENDIF
            IFLD(I) = LSHIFT(IFLD(I),32-NBITS)
 200      CONTINUE
        ENDIF
      ENDIF
*
#endif
      RETURN
      END
