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
#if defined (OLD_32BIT_CODE)
      SUBROUTINE MEMOIR8 (Z,IX,N8)
C
C     HAL RITCHIE AND MICHEL VALIN.     MARCH 1992.
C     C.BEAUDOIN MODIFICATIONS AVRIL 93
C
C     MEMORY ALLOCATER FOR REAL*8 SPACE.
C
C     ARGUMENTS
C     Z   = BASE ADDRESS
C     IX  = POINTER FOR BEGINNING OF SPACE
C     N8  = NUMBER OF REAL*8 ITEMS FOR ALLOCATION
C
      IMPLICIT NONE
      REAL*8 Z(*)
      REAL*8 ZW(2)
      INTEGER IX,N8,II,IXX
      INTEGER ADJ
      EXTERNAL MEMOIR, qqexit
      
#if !defined (OLD_32BIT_CODE)
	print *,'****************************************************'
	print *,'* ERROR: MEMOIR8                                   *'
	print *,'* This code is obsolete, will not work on a 64     *'
	print *,'* bit architecture and should not be used.         *'
	print *,'* allocate shoud be used instead                   *'
	print *,'* EXITING                                          *'
	print *,'****************************************************'
	call qqexit(44)
#else
C
C     ADJ = 1 (NEC 64 BITS)
C     ADJ = 2 (NEC 32 BITS)
C     ADJ = 8 (SERVEUR 32 BITS)
C
      ADJ = LOC(ZW(2))-LOC(ZW(1))
C
      II=LOC(Z)
      IF (MOD(II,ADJ).NE.0) THEN
           PRINT*,' '
           PRINT*,' STOP IN MEMOIR8 DUE TO BAD BASE ADDRESS'
           STOP
      ENDIF
C
      IF (N8.EQ.0) THEN
           CALL MEMOIR (Z(IX),1,0)
      ELSEIF (ADJ.EQ.1) THEN
           CALL MEMOIR (Z,IX,N8)
      ELSE
           CALL MEMOIR (Z,IXX,2*N8)
           IX=((IXX-1)/2)+1
      ENDIF
C
      RETURN
#endif
      END
#endif