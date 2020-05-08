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

***PROGRAMME TESTPAK  TESTER LES "PACKERS" ENTIER ET REEL
*
      PROGRAM TESTPAK
      IMPLICIT INTEGER(A-Z)
*
*AUTEUR   M. LEPINE  1988
*
*LANGAGE  RATFOR
*
*OBJET(TESTPAK)
*         CE PROGRAMME CONSTITUE UN TEST POUR IIPAK ET XXPAK
*         CRAY, C170 ET C180.
*
*MODULES
*          XXPAK ,IIPAK
*
*

      PARAMETER (NFOIS=100)

*     float
      REAL T1, T2, T3, SECOND, TPAK, TUNPAK, MAXDIF
*     double
      REAL*8 DTPAK, DTUNPAK

      INTEGER tempII

      INTEGER IWK(2200), IWK2(2200), IWK3(2000)

*     float
      INTEGER OP, OPCODE
*     double
      INTEGER DOP, DOPCODE

      INTEGER BITS(0:63),II,MAXBITS,OFFSET,MBITS, oneLess

*     float
      REAL ERRMAX, WK1(2200), WK2(2000), WK3(2200)
*     double
      REAL*8 DWK1(2200), DWK2(2000), DWK3(2200)
      INTEGER theMode

      CHARACTER * 8 MSG (0:1)
      LOGICAL ERR


*     lib function declearation
      INTEGER qqqr8sz
      EXTERNAL qqqr8sz


*     detect whether it is in 32 bits or 64 bits mode
      theMode = qqqr8sz()


      DATA MSG /'        ','*ERREUR*'/
*     float
      OP  = 1
      OPCODE = 2
*     double 
      DOP = 5
      DOPCODE = 6


      

      WRITE(*,1)
1     FORMAT(1H1,30X,'TESTPAK 1.0',//,
     %   ' POUR CONFIRMER REUSSITE DU TEST,',/,
     %   ' VOIR MESSAGE *TESTPAK REUSSI* A LA FIN',/,
     %   ' ET VERIFIER VISUELLEMENT TESTS PRECEDES DE ---',//)

      ERR = .FALSE.
      II = 1
      BITS(0) = II
      MAXBITS = 1
      DO 22 I = 1, 31
*47
        II = II + II
        MAXBITS = MAXBITS + 1
        BITS(I) = II
        IF ( II .LT. 0 ) GOTO 33
  22  CONTINUE
  33  CONTINUE






      WRITE(*,*) '---TEST 15.0A (IIPAK) PAQUETER DE 1 A ',MAXBITS,
     %           ' BITS, WITHOUT HEADER'

      DO 23000 NBITS =1,MAXBITS,1
*      DO 23000 NBITS =32,32,1
*         DO 23002 I = 1,20
         DO 23002 I = 1,2000
            IWK(I)  = BITS(MOD(I,NBITS))
            IWK2(I) = -1
            IWK3(I) = -1
23002    CONTINUE


C     GOTO 333

*        CALL IIPAK(IWK,IWK2(2),18,1,-NBITS,1017,01)
*        CALL IIPAK(IWK3,IWK2(2),18,1,-NBITS,1017,02)
*         WRITE(*,*) ' before yah'
         T1=SECOND()
         DO 23004 I=1,NFOIS
            CALL IIPAK(IWK,IWK2,2000,1,-NBITS,1017,01)
23004    CONTINUE

*         WRITE(*,*) ' yah'

         T2=SECOND()
         DO 23006 I=1,NFOIS
            CALL IIPAK(IWK,IWK2,2000,1,-NBITS,1017,02)
23006    CONTINUE
*         WRITE(*,*) ' after yah'
         T3=SECOND()

         TPAK = T2-T1
         TUNPAK = T3-T2

         IMAXDIF = 0
         DO 23008 I = 1,2000
*         DO 23008 I = 1,20 
            IF( IWK(I).NE. BITS(MOD(I,NBITS)) )THEN
               WRITE(*,*) I, IWK(I), BITS(MOD(I,NBITS))  
               ERR = .TRUE.
               IMAXDIF = IMAXDIF + 1
*               WRITE(*,902) NBITS,BITS(MOD(I,NBITS)),I,IWK(I)
902            FORMAT(' NBITS = ',I2,' VALEUR THEORIQUE = ',Z16,
     %            ' IWK(',I4,') = ',Z16)
            ENDIF
23008    CONTINUE
         WRITE(*,802) NBITS,TPAK/NFOIS,TUNPAK/NFOIS,
     %                MSG(MAX(0,MIN(1,IMAXDIF)))
802      FORMAT(' NBITS=',I2.2,5X,' PACK: ',E8.2,' S ',
     %          ' UNPACK: ',E8.2,' S ',T71,A8)

23000 CONTINUE

      IF( (ERR))THEN
         CALL ERREUR
      ELSE
         CALL TESTOK
      ENDIF

      IF (MAXBITS .EQ. 32) THEN
        MBITS = MAXBITS -1
      ELSE
        MBITS = MAXBITS
      ENDIF
      WRITE(*,*)




      WRITE(*,*) '---TEST 15.0B (IIPAK) PAQUETER DE 1 A ',MAXBITS,
     %     ' BITS, with header'

      DO 93000 NBITS =1,MAXBITS,1
*      DO 93000 NBITS =MAXBITS,MAXBITS,1
*         DO 93002 I = 1,20
         DO 93002 I = 1,2000
            IWK(I) = BITS(MOD(I,NBITS))
            IWK2(I) = -1
93002    CONTINUE


C     GOTO 333

*        CALL IIPAK(IWK,IWK2,20,1,-NBITS,1017,1001)
*        CALL IIPAK(IWK,IWK2,20,1,-NBITS,1017,1002)

         

         T1=SECOND()
         DO 93004 I=1,NFOIS
            CALL IIPAK(IWK,IWK2,2000,1,-NBITS,1017,1001)
93004    CONTINUE



         T2=SECOND()
         DO 93006 I=1,NFOIS
            CALL IIPAK(IWK,IWK2,2000,1,-NBITS,1017,1002)
93006    CONTINUE

         T3=SECOND()

         TPAK = T2-T1
         TUNPAK = T3-T2

         IMAXDIF = 0
         DO 93008 I = 1,2000
*         DO 93008 I = 1,20
            IF( IWK(I).NE. BITS(MOD(I,NBITS)) )THEN
               WRITE(*,*) I, IWK(I), BITS(MOD(I,NBITS))  
               ERR = .TRUE.
               IMAXDIF = IMAXDIF + 1
C              WRITE(*,9902) NBITS,BITS(MOD(I,NBITS)),I,IWK(I)
 9902          FORMAT(' NBITS = ',I2,' VALEUR THEORIQUE = ',Z16,
     %            ' IWK(',I4,') = ',Z16)
            ENDIF
93008    CONTINUE
         WRITE(*,9802) NBITS,TPAK/NFOIS,TUNPAK/NFOIS,
     %        MSG(MAX(0,MIN(1,IMAXDIF)))
 9802    FORMAT(' NBITS=',I2.2,5X,' PACK: ',E8.2,' S ',
     %        ' UNPACK: ',E8.2,' S ',T71,A8)

93000 CONTINUE

      IF( (ERR))THEN
         CALL ERREUR
      ELSE
         CALL TESTOK
      ENDIF

      IF (MAXBITS .EQ. 32) THEN
        MBITS = MAXBITS -1
      ELSE
        MBITS = MAXBITS
      ENDIF
      WRITE(*,*)



     % '---TEST 15.1A (IIPAK( + ET - )) PAQUETER DE 2 A ',MBITS,
     % ' BITS (SUR PLACE, WITHOUT HEADER)'

      ERR = .FALSE.

      DO 23014 NBITS = 2,MBITS
*      DO 23014 NBITS = 4,4
*         DO 23016 I = 1,10
         DO 23016 I = 1,200
            IWK(I) = BITS(MOD(I,NBITS-1))
            IF( (MOD(I,2) .EQ. 0))THEN
               IWK(I) = -IWK(I)

            ENDIF
            IWK2(I) = IWK(I)

*            PRINT *,'IWK2',IWK2(I), I

23016    CONTINUE

*         CALL IIPAK(IWK,IWK,10,1,-NBITS,0,3)
*         CALL IIPAK(IWK,IWK,10,1,-NBITS,0,4)

         CALL IIPAK(IWK,IWK,200,1,-NBITS,0,3)
         CALL IIPAK(IWK,IWK,200,1,-NBITS,0,4)
*         WRITE(*,*) '---6---', IWK(6)
         IMAXDIF = 0

         DO 23020 I = 1,200
*         DO 23020 I = 1,10
*               WRITE(*,*) I, IWK(I), IWK2(I)
            IF( (IWK(I).NE. IWK2(I)))THEN
               WRITE(*,*) I, IWK(I), IWK2(I)
               ERR = .TRUE.
               IMAXDIF = IMAXDIF + 1
C              WRITE(*,903) NBITS,IWK2(I),I,IWK(I)
903            FORMAT(' NBITS = ',I2,' VALEUR THEORIQUE = ',Z16,
     %            ' IWK(',I3,') = ',Z16)
            ENDIF
23020    CONTINUE
         IF (IMAXDIF .GT. 0) THEN
           WRITE(*,803) NBITS,IMAXDIF,MSG(MAX(0,MIN(1,IMAXDIF)))
         ENDIF
803      FORMAT(' NBITS=',I2.2,5X,I3.3,'/200',T71,A8)
23014 CONTINUE

      IF( (ERR))THEN
         CALL ERREUR
      ELSE
         CALL TESTOK

      ENDIF

      WRITE(*,*)



     % '---TEST 15.1B (IIPAK( + ET - )) PAQUETER DE 2 A ',MBITS,
     % ' BITS (SUR PLACE, with header)'

      ERR = .FALSE.

      DO 8014 NBITS = 2,MBITS
*      DO 8014 NBITS = 4,4
*         DO 23016 I = 1,10
         DO 8016 I = 1,200
            IWK(I) = BITS(MOD(I,NBITS-1))
            IF( (MOD(I,2) .EQ. 0))THEN
               IWK(I) = -IWK(I)

            ENDIF
            IWK2(I) = IWK(I)

*            PRINT *,'IWK2',IWK2(I), I

 8016    CONTINUE
         
*         CALL IIPAK(IWK,IWK,10,1,-NBITS,0,3)
*         CALL IIPAK(IWK,IWK,10,1,-NBITS,0,4)
*         oneLess = NBITS - 1
         CALL IIPAK(IWK,IWK,200,1,-NBITS,0,1003)
         CALL IIPAK(IWK,IWK,200,1,-NBITS,0,1004)
*         WRITE(*,*) '---6---', IWK(6)
         IMAXDIF = 0

         DO 8020 I = 1,200
*         DO 23020 I = 1,10
*               WRITE(*,*) I, IWK(I), IWK2(I)
            IF( (IWK(I).NE. IWK2(I)))THEN
               WRITE(*,*) I, IWK(I), IWK2(I)
               ERR = .TRUE.
               IMAXDIF = IMAXDIF + 1
C              WRITE(*,8903) NBITS,IWK2(I),I,IWK(I)
 8903          FORMAT(' NBITS = ',I2,' VALEUR THEORIQUE = ',Z16,
     %            ' IWK(',I3,') = ',Z16)
            ENDIF
 8020    CONTINUE
         IF (IMAXDIF .GT. 0) THEN
           WRITE(*,8803) NBITS,IMAXDIF,MSG(MAX(0,MIN(1,IMAXDIF)))
         ENDIF
 8803    FORMAT(' NBITS=',I2.2,5X,I3.3,'/200',T71,A8)
 8014  CONTINUE

      IF( (ERR))THEN
         CALL ERREUR
      ELSE
         CALL TESTOK

      ENDIF

      WRITE(*,*)




     % '---TEST 15.2 IIPAK (OFFSET ET STRIDE) PAQUETER DE 1 A ',
     % MAXBITS,' BITS'

*      DO 250 JJJ = 0, 1
      DO 250 JJJ = 0,MAXBITS-1
      ERR = .FALSE.
      PRINT *,' OFFSET = ',JJJ
*      DO 250 NBITS = 4, 4
      DO 250 NBITS = 1,MAXBITS
         IMAXDIF = 0
         DO 201 I = 1,2000
            IWK(I) = BITS(MOD(I,NBITS))

 2345       FORMAT(1X,I2,2X,I4,2X,I2,2X,Z16)
            IWK3(I) = -1
 201     CONTINUE

         OFFSET = JJJ
         CALL IIPAK(IWK,IWK2(1),513,1,-NBITS,OFFSET,11)
         OFFSET = NBITS * 513 + JJJ
         CALL IIPAK(IWK(514),IWK2(1),1487,1,-NBITS,OFFSET,11)
         
         OFFSET = JJJ
         CALL IIPAK(IWK3(1),IWK2,511,1,-NBITS,OFFSET,112)
         OFFSET = NBITS * 511 + JJJ
         CALL IIPAK(IWK3(512),IWK2,1489,1,-NBITS,OFFSET,112)

*         OFFSET = JJJ
*         CALL IIPAK(IWK,IWK2(1),3,1,-NBITS,OFFSET,11)
*         OFFSET = NBITS * 3 + JJJ
*         CALL IIPAK(IWK(4),IWK2(1),17,1,-NBITS,OFFSET,11)

*         OFFSET = JJJ
*         CALL IIPAK(IWK3(1),IWK2,10,1,-NBITS,OFFSET,112)

*         OFFSET = JJJ
*         CALL IIPAK(IWK3(1),IWK2,1,1,-NBITS,OFFSET,112)
*         OFFSET = NBITS * 1 + JJJ
*         CALL IIPAK(IWK3(2),IWK2(1),19,1,-NBITS,OFFSET,112)

         DO 230 K = 1,2000
*         DO 230 K = 1,10
*            WRITE(*,*) K, IWK(K), IWK3(K)
             IF (IWK(K) .NE. IWK3(K)) THEN
                WRITE(*,*) 'different', K, IWK(K), IWK3(K)
                ERR = .TRUE.
                IMAXDIF = IMAXDIF + 1

904            FORMAT(' NBITS = ',I2,' VALEUR THEORIQUE = ',Z16,
     %               ' IWK3(',I4,') = ',Z16)
             ENDIF
230      CONTINUE

*         WRITE(*,*) '  '

         IF (IMAXDIF .GT. 0) THEN
           WRITE(*,804) NBITS,IMAXDIF,MSG(MAX(0,MIN(1,IMAXDIF)))
         ENDIF
804      FORMAT(' NBITS=',I2.2,5X,I4.4,'/2000',T71,A8)
250   CONTINUE

      IF( (ERR))THEN
         CALL ERREUR
      ELSE
         CALL TESTOK
      ENDIF

333   CONTINUE


      WRITE(*,*) '---TEST 15.3'
      CALL cmainii
      WRITE(*,*)




      WRITE(*,*) '---TEST 15.4'
      WRITE(*,*) '---TEST of over/under indexing with XXPAK'

      NBITS = 16
      MAXDIF = 0.0

      WK1(1) = 100.0
      WK1(2) = 0.9999999
      WK2(1) = 100.0
      WK2(2) = -1.0
      IWK3(1) = -1
      IWK3(2) = -1

      DO 23528 I = 3, 8
         WK1(I) = REAL(I)/(2000.0+10.0) - 0.5
         WK2(I) = -1.0
         WK3(I) = -1
23528 CONTINUE


      WK1(9) = 100.0
      WK2(9) = 100.0
      IWK3(9) = -1
      WK1(10) = 100.0
      WK2(10) = 100.0
      IWK3(10) = -1

      CALL XXPAK(WK1(2),IWK3(2),7,1,-NBITS,0,OP)
      CALL XXPAK(WK2(2),IWK3(2),7,1,-NBITS,0,OPCODE)




      DO 23536 I = 1, 10
           
         WRITE(*,961) NBITS,  I, WK1(I), I, WK2(I), IWK3(I)
 961     FORMAT(' NBITS=',I2, ' WK1(',I3,
     %        ')=',F10.6,   ' WK2(',I3,')=',F10.6, ' IWK3=',Z8)
 
23536    CONTINUE

         WRITE(*,*)
         IF( (WK1(1).eq.WK2(1)) .AND. (WK1(9).eq.WK2(9)) .AND. (WK1(1)
     %   .eq. 100) .AND. (WK1(9).eq.100) ) THEN        
           WRITE(*,*) '************* Passed ************'
         ELSE
           WRITE(*,*) '************* Failed ************'
           STOP
         ENDIF
         WRITE(*,*)
         WRITE(*,*)
         WRITE(*,*) '=================================================='


         


      WRITE(*,*)
      WRITE(*,*) '---TEST 15.5 (XXPAK) champ constant (0)'
      do i = 1,10
         wk1(i) = 0.
         wk2(i) = -1.0
      enddo
      call xxpak(wk1,wk1,10,1,-16,0,1)
      call xxpak(wk2,wk1,10,1,-16,0,2)
      err = .false.
      do i = 1,10
         if (wk2(i) .ne. 0.) then
            print *,'Erreur wk2(',i,')=',wk2(i)
            err = .true.
         endif
      enddo
      if (err) then
         call erreur()
         STOP
      else
         call testok()
      endif

      WRITE(*,*) '=================================================='
      WRITE(*,*)
      WRITE(*,*)
      WRITE(*,*)

      tempII = -1
      IF ( tempII .EQ. -1 ) THEN
      
      IF ( theMode .EQ. 2 ) THEN



      
        
      WRITE(*,*) '---TEST 16.0_A (XXPAK) on **float** PAQUETER DE 1 A ',MAXBITS,
     %     ' BITS'
      ERR = .FALSE.
*      DO 23026 NBITS = 1, 5
      DO 23026 NBITS = 1, MAXBITS
*      DO 23026 NBITS = 32, MAXBITS 
        MAXDIF = 0.0
         WK1(1) = 0.0
         WK1(2) = 0.9999999
         WK2(1) = WK1(1)
         WK2(2) = WK1(2)
         DO 23028 I = 3, 2000
            WK1(I) = REAL(I)/(2000.0+10.0) - 0.5
            WK2(I) = WK1(I)
23028    CONTINUE

         T1=SECOND()
         DO 23030 I=1,NFOIS
*         DO 23030 I=1,2
            CALL XXPAK(WK1,WK3,2000,1,-NBITS,0,OP)
*            CALL XXPAK(WK1,WK3,20,1,-NBITS,0,OP)
23030    CONTINUE

         T2=SECOND()

         DO 23032 I = 1,2000
            WK1(I) = -99999.
23032    CONTINUE
         DO 23034 I=1,NFOIS
*         DO 23034 I=1,2
*            CALL XXPAK(WK1,WK3,20,1,-NBITS,0,OPCODE)
            CALL XXPAK(WK1,WK3,2000,1,-NBITS,0,OPCODE)
23034    CONTINUE
         T3=SECOND()

         TPAK = T2-T1
         TUNPAK = T3-T2
         IF (MAXBITS .EQ. 32) THEN
           ERRMAX = 2.1 * 2.0 ** (-(MIN(NBITS,24) ))
         ELSE
           ERRMAX = 2.1 * 2.0 ** (-(NBITS ))
         ENDIF
         DO 23036 I = 1, 2000
*         DO 23036 I = 1, 20
            MAXDIF = MAX(MAXDIF,ABS(WK1(I)-WK2(I)))
            IMAXDIF = 0

            IF( (ABS(WK1(I)-WK2(I)) .GT. ERRMAX))THEN
              WRITE(*,901) NBITS, ERRMAX, I, WK1(I), I, WK2(I)
901            FORMAT(' NBITS=',I2, ' ERRMAX=',F20.16,' WK1(',I3,
     %         ')=',F20.16,   ' WK2(',I3,')=',F20.16)
               ERR = .TRUE.
               IMAXDIF = 1
            ENDIF
23036    CONTINUE
      WRITE(*,801) NBITS,TPAK/NFOIS,TUNPAK/NFOIS,ERRMAX,MAXDIF,
     %             MSG(IMAXDIF)
801   FORMAT(' NBITS=',I2.2,' PACK: ',E8.2,' S ',
     %       ' UNPACK: ',E8.2,' S ',' ERR OK= ',E10.3,' ERR MAX=',
     %       E10.3,T100,A8)
23026 CONTINUE

      IF( (ERR ))THEN
         CALL ERREUR
      ENDIF
      CALL TESTOK

      ENDIF


      WRITE(*,*) '---TEST 16.0_B (XXPAK) on *double* PAQUETER DE 1 A ',
     %     MAXBITS, ' BITS'
      ERR = .FALSE.
*      DO 23026 NBITS = 3, 3
      DO 33026 NBITS = 1, MAXBITS
*      DO 23026 NBITS = 32, MAXBITS 
        MAXDIF = 0.0
         DWK1(1) = 0.0
         DWK1(2) = 0.9999999
         DWK2(1) = DWK1(1)
         DWK2(2) = DWK1(2)
         DO 33028 I = 3, 2000
            DWK1(I) = REAL(I)/(2000.0+10.0) - 0.5
            DWK2(I) = DWK1(I)
33028    CONTINUE

         T1=SECOND()
*         DO 33030 I=1,NFOIS
         DO 33030 I=1,1
            CALL XXPAK(DWK1,DWK3,2000,1,-NBITS,0,DOP)
33030    CONTINUE

         T2=SECOND()

         DO 33032 I = 1,2000
            DWK1(I) = -99999.
33032    CONTINUE
*         DO 33034 I=1,NFOIS
         DO 33034 I=1,1
            CALL XXPAK(DWK1,DWK3,2000,1,-NBITS,0,DOPCODE)
33034    CONTINUE
         T3=SECOND()

         DTPAK = T2-T1
         DTUNPAK = T3-T2
         IF (MAXBITS .EQ. 32) THEN
           ERRMAX = 2.1 * 2.0 ** (-(MIN(NBITS,24) ))
         ELSE
           ERRMAX = 2.1 * 2.0 ** (-(NBITS ))
         ENDIF
         DO 33036 I = 1, 2000
            MAXDIF = MAX(MAXDIF,REAL(ABS(DWK1(I)-DWK2(I))))
            IMAXDIF = 0

            IF( (ABS(DWK1(I)-DWK2(I)) .GT. ERRMAX))THEN
C              WRITE(*,3901) NBITS, ERRMAX, I, WK1(I), I, WK2(I)
3901            FORMAT(' NBITS=',I2, ' ERRMAX=',F20.16,' DWK1(',I3,
     %         ')=',F20.16,   ' DWK2(',I3,')=',F20.16)
               ERR = .TRUE.
               IMAXDIF = 1
            ENDIF
33036    CONTINUE
      WRITE(*,3801) NBITS,TPAK/NFOIS,TUNPAK/NFOIS,ERRMAX,MAXDIF,
     %             MSG(IMAXDIF)
3801   FORMAT(' NBITS=',I2.2,' PACK: ',E8.2,' S ',
     %       ' UNPACK: ',E8.2,' S ',' ERR OK= ',E10.3,' ERR MAX=',
     %       E10.3,T100,A8)
33026 CONTINUE

      IF( (ERR ))THEN
         CALL ERREUR
      ENDIF
      CALL TESTOK






      IF ( theMode .EQ. 2 ) THEN

      WRITE(*,*) '---TEST 16.1_A (XXPAK) **float** on PAQUETER DE 1 A ',
     %     MAXBITS, ' BITS SUR PLACE'
      ERR = .FALSE.
*      DO 44026 NBITS = 3, 3
      DO 44026 NBITS = 1, MAXBITS
*      DO 44026 NBITS = 1,32
         MAXDIF = 0.0
         WK1(1) = 0.0
         WK1(2) = 0.9999999
         WK2(1) = WK1(1)
         WK2(2) = WK1(2)
         DO 44028 I = 3, 2000
            WK1(I) = REAL(I)/(2000.0+10.) - 0.5
            WK2(I) = WK1(I)
44028    CONTINUE
         
*         IF(NBITS .EQ. 3) THEN
*            DO I=1,2000
*               PRINT *,'WK1',WK1(I),I
*            ENDDO
*         ENDIF

         CALL XXPAK(WK1,WK1,2000,1,-NBITS,0,OP)
         CALL XXPAK(WK1,WK1,2000,1,-NBITS,0,OPCODE)

         IF (MAXBITS .EQ. 32) THEN
           ERRMAX = 2.1 * 2.0 ** (-(MIN(NBITS,24) ))
         ELSE
           ERRMAX = 2.1 * 2.0 ** (-(NBITS ))
         ENDIF
         DO 44036 I = 1, 2000
            MAXDIF = MAX(MAXDIF,ABS(WK1(I)-WK2(I)))
            IMAXDIF = 0
            IF( (ABS(WK1(I)-WK2(I)) .GT. ERRMAX))THEN
*               PRINT *,'ERREUR',ABS(WK1(I)-WK2(I)),ERRMAX
*             WRITE(*,951) NBITS, ERRMAX, I, WK1(I), I, WK2(I)
951            FORMAT(' NBITS=',I2, ' ERRMAX=',F20.16,' WK1(',I3,
     %         ')=',F20.16,   ' WK2(',I3,')=',F20.16)
               ERR = .TRUE.
               IMAXDIF = 1
            ENDIF
44036    CONTINUE
      WRITE(*,981) NBITS,ERRMAX,MAXDIF,
     %             MSG(IMAXDIF)
981   FORMAT(' NBITS=',I2.2,
     %       ' ERR OK= ',E10.3,' ERR MAX=',
     %       E10.3,T60,A8)
44026 CONTINUE
      IF( (ERR))THEN
         CALL ERREUR
      ELSE
         CALL TESTOK
      ENDIF

      ENDIF



      WRITE(*,*) '---TEST 16.1_B (XXPAK) *double* on PAQUETER DE 1 A ',
     %     MAXBITS, ' BITS SUR PLACE'
      ERR = .FALSE.
*      DO 44026 NBITS = 3, 3
      DO 54026 NBITS = 1, MAXBITS
*      DO 44026 NBITS = 1,32
         MAXDIF = 0.0
         DWK1(1) = 0.0
         DWK1(2) = 0.9999999
         DWK2(1) = DWK1(1)
         DWK2(2) = DWK1(2)
         DO 54028 I = 3, 2000
            DWK1(I) = REAL(I)/(2000.0+10.) - 0.5
            DWK2(I) = DWK1(I)
54028    CONTINUE
         
*         IF(NBITS .EQ. 3) THEN
*            DO I=1,2000
*               PRINT *,'WK1',WK1(I),I
*            ENDDO
*         ENDIF

         CALL XXPAK(DWK1,DWK1,2000,1,-NBITS,0,DOP)
         CALL XXPAK(DWK1,DWK1,2000,1,-NBITS,0,DOPCODE)

         IF (MAXBITS .EQ. 32) THEN
           ERRMAX = 2.1 * 2.0 ** (-(MIN(NBITS,24) ))
         ELSE
           ERRMAX = 2.1 * 2.0 ** (-(NBITS ))
         ENDIF
         DO 54036 I = 1, 2000
            MAXDIF = MAX(MAXDIF,REAL(ABS(DWK1(I)-DWK2(I))))
            IMAXDIF = 0
            IF( (ABS(DWK1(I)-DWK2(I)) .GT. ERRMAX))THEN
*               PRINT *,'ERREUR',ABS(WK1(I)-WK2(I)),ERRMAX
*             WRITE(*,951) NBITS, ERRMAX, I, WK1(I), I, WK2(I)
5951            FORMAT(' NBITS=',I2, ' ERRMAX=',F20.16,' DWK1(',I3,
     %         ')=',F20.16,   ' DWK2(',I3,')=',F20.16)
               ERR = .TRUE.
               IMAXDIF = 1
            ENDIF
54036    CONTINUE
      WRITE(*,5981) NBITS,ERRMAX,MAXDIF,
     %             MSG(IMAXDIF)
5981   FORMAT(' NBITS=',I2.2,
     %       ' ERR OK= ',E10.3,' ERR MAX=',
     %       E10.3,T60,A8)
54026 CONTINUE
      IF( (ERR))THEN
         CALL ERREUR
      ELSE
         CALL TESTOK
      ENDIF

      ENDIF


*     
*     test of new features in the floating point packer
*
*
*











      WRITE(*,*) '---TEST 16.2 (cmain)'
      IF ( theMode .EQ. 2 ) THEN
      CALL cmain
      ENDIF

      CALL dcmain

C      WRITE(*,*) '---TEST 16.3 (rlemain)'
C      IF ( theMode .EQ.2) THEN
C      CALL mainrle
C      ENDIF


*     
*     IEEE block test
*
*
*
      WRITE(*,*)
      WRITE(*,*) '=======================================================
     %==============='
      WRITE(*,*)
      WRITE(*,*) '---TEST 17.1 (cmainIEEEBlock)'
      IF ( theMode .EQ. 2 ) THEN
      CALL cmainieee()
      ENDIF

      CALL cmainieeedouble()



*     
*     IBM mantisa shift test
*
*
*
      WRITE(*,*)
      WRITE(*,*) '=======================================================
     %==============='
      WRITE(*,*)
      WRITE(*,*) '---TEST 18.1 (min_1_2)'
      CALL min_1_2()

      WRITE(*,*)
      WRITE(*,*) '---TEST 18.2 (min_2_4)'
      CALL min_2_4()    

      WRITE(*,*)
      WRITE(*,*) '---TEST 18.3 (min_4_8)'
      CALL min_4_8()

      WRITE(*,*)
      WRITE(*,*) '---TEST 18.4 (min_8_16)'
      CALL min_8_16()


      WRITE(*,*) ' *TESTPAK REUSSI*'
      STOP
      END
      SUBROUTINE TESTOK
*   TRAITE LE RESULTAT D'UN TEST

      WRITE(*,10)
10    FORMAT(' ---REUSSI',/)
      RETURN
      ENTRY ERREUR
      WRITE(*,*) '$$$ERREUR$$$'
      STOP
      END


