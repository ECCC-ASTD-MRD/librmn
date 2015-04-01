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
      PROGRAM YOYO
      COMMON /XXX/TAB1(10),TAB2(10),TAB3(10)
      COMMON /YYY/ NTAB1,NTAB2,NTAB3,NSUB1,NSUB2
      INTEGER TAB1,TAB2,TAB3,dummy

      EXTERNAL SUB1,SUB2, YOUPI
      integer*8 loc_sub
      external loc_sub

c      IER = FNOM(6,'$OUTPUT','SEQ',0)
      CALL qlxins(TAB1,'TAB1',NTAB1,9,1)
      CALL qlxins(INDICE,'IND',NNN,1,1)
      CALL qlxins(TAB2,'TAB2',NTAB2,4,1)
      CALL qlxins(TAB3,'TAB3',NTAB3,7,1)
      CALL qlxins(22,'CONST22',dummy,1,0)
      CALL qlxins(55,'CONST55',dummy,1,0)

      CALL qlxinx(SUB1,'SUB1',NSUB1,0104,2)
      CALL qlxinx(SUB2,'SUB2',NSUB2,0305,2)
      CALL qlxinx(YOUPI,'SUB3',NSUB3,0000,2)
C
      write(*,77) loc_sub(sub1),loc_sub(sub2)
 77   format(' *** debug sub1 = ',z16.16,' sub2 = ',z16.16)
      PRINT *,' Avant READLX - input=inp_readlx'
      IER = FNOM(5,'INP_READLX','SEQ',0)
      CALL READLX(5,KND,KRR)
      PRINT *,' APRES READLX - KND,KRR ',KND,KRR
C     CALL READLX(5,KND,KRR)
C     PRINT *,' APRES READLX - KND,KRR ',KND,KRR

C     PRINT *,' NTAB1,NTAB2MNTAB3 -',NTAB1,NTAB2,NTAB3
C     WRITE(6,'(3X,4Z20)')TAB1
C     WRITE(6,'(3X,4Z20)')TAB2
C     WRITE(6,'(3X,4Z20)')TAB3
      STOP
      END
      SUBROUTINE SUB1(A,B,C,D)
      COMMON /YYY/ NTAB1,NTAB2,NTAB3,NSUB1,NSUB2
      INTEGER A(*),B(*),C(*),D(*),ARGDIMS,ARGDOPE,ND
      INTEGER LISTE(5)
      PRINT *,' PASSE PAR SUB1'
      PRINT *,' NB D ARGUMENTS =',NSUB1
      GOTO(1,2,3,4)NSUB1
4     PRINT 101,D(1),D(1),LOC(D),(D(I),I=1,ARGDIMS(4))
      ND = ARGDOPE(4,LISTE,5)
      PRINT 102,ND,LISTE
3     PRINT 101,C(1),C(1),LOC(C),(C(I),I=1,ARGDIMS(3))
      ND = ARGDOPE(3,LISTE,5)
      PRINT 102,ND,LISTE
2     PRINT 101,B(1),B(1),LOC(B),(B(I),I=1,ARGDIMS(2))
      ND = ARGDOPE(2,LISTE,5)
      PRINT 102,ND,LISTE
1     PRINT 101,A(1),A(1),LOC(A),(A(I),I=1,ARGDIMS(1))
      ND = ARGDOPE(1,LISTE,5)
      PRINT 102,ND,LISTE
101   FORMAT(3X,Z20,I10,5Z20)
 102  FORMAT(1X,' ND=',I2,' LISTE=',5Z12)
      END
      SUBROUTINE SUB2(A,B,C,D,E)
      COMMON /YYY/ NTAB1,NTAB2,NTAB3,NSUB1,NSUB2
      INTEGER A,B,C,D,E
      PRINT *,' PASSE PAR SUB2'
      PRINT *,' NB D ARGUMENTS =',NSUB2
      GOTO(1,2,3,4,5)NSUB2
5     PRINT *,E,LOC(E)
4     PRINT *,D,LOC(D)
3     PRINT *,C,LOC(C)
2     PRINT *,B,LOC(B)
1     PRINT *,A,LOC(A)
      RETURN
      END
      SUBROUTINE YOUPI
      PRINT *,' <><><> Y O U P I <><><>'
      RETURN
      END
