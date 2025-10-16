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


!  MACRO ICHAMP(MOT,FIN,LG)   EXTRAIT UN CHAMP D'UN MOT

!   MOT   MOT QUI CONTIENT LE CHAMP
!   FIN   NUMERO DU DERNIER BIT (A DROITE) DU CHAMP, EN NUMEROTATION
!         GAUCHE > DROITE (LE BIT 0 EST A DROITE DU MOT).
!   LG    LONGUEUR, EN BITS, DU CHAMP



!  MACRO IUNPAK(BASE, BITPOS, LG)  OBTENIR UN CHAMP D'UN TABLEAU

!  BASE    TABLEAU CONTENANT LE CHAMP A EXTRAIRE
!  BITPOS  POSITION DU BIT DE DROITE DU CHAMP A EXTRAIRE
!          LE BIT 0 EST LE BIT SIGNE DU PREMIER MOT DU TABLEAU.
!  LG      EST LE NOMBRE DE BITS QU'OCCUPE LE CHAMP. (MAX 32 BITS)




!  MACRO GETBIT(BASE, BITPOS, LG)  OBTENIR UN CHAMP D'UN TABLEAU

!  BASE    TABLEAU CONTENANT LE CHAMP A EXTRAIRE
!  BITPOS  POSITION DU BIT DE DROITE DU CHAMP A EXTRAIRE
!          LE BIT 0 EST LE BIT SIGNE DU PREMIER MOT DU TABLEAU.
!  LG      EST LE NOMBRE DE BITS QU'OCCUPE LE CHAMP. (MAX 32 BITS)




!  MACRO INSERT(TABL,KWA,BITPOS,LONG)  INSERER UN CHAMP DANS UN TABLEAU

!  TABL    TABLEAU QUI CONTIENDRA LE CHAMP APRES INSERTION
!  KWA     MOT QUI CONTIENT LE CHAMP A INSERER JUSTIFIE A DROITE
!  BITPOS  POSITION DU DERNIER BIT (A DROITE) DU CHAMP A INSERER
!          LE BIT 0 EST LE BIT SIGNE DU PREMIER MOT DU TABLEAU
!  LONG    LONGUEUR EN BIT DU CHAMP A INSERER (PAS PLUS DE 32 BITS)



!  MACRO PUTBIT(TABL,KWA,BITPOS,LONG)  INSERER UN CHAMP DANS UN TABLEAU

!  TABL    TABLEAU QUI CONTIENDRA LE CHAMP APRES INSERTION
!  KWA     MOT QUI CONTIENT LE CHAMP A INSERER JUSTIFIE A DROITE
!  BITPOS  POSITION DU DERNIER BIT (A DROITE) DU CHAMP A INSERER
!          LE BIT 0 EST LE BIT SIGNE DU PREMIER MOT DU TABLEAU
!  LONG    LONGUEUR EN BIT DU CHAMP A INSERER (PAS PLUS DE 32 BITS)



!  MACRO CLRBIT(TABL,BITPOS,LONG)  METTRE A ZERO UN CHAMP DANS UN TABLEAU

!  TABL    TABLEAU
!  BITPOS  POSITION DU DERNIER BIT (A DROITE) DU CHAMP A NETTOYER
!          LE BIT 0 EST LE BIT SIGNE DU PREMIER MOT DU TABLEAU
!  LONG    LONGUEUR EN BIT DU CHAMP A NETTOYER (PAS PLUS DE 32 BITS)



!  MACRO PUTBITC(TABL,KWA,BITPOS,LONG)  INSERER UN CHAMP DANS UN TABLEAU
!                                       AVEC NETTOYAGE PRELIMINAIRE
!  TABL    TABLEAU QUI CONTIENDRA LE CHAMP APRES INSERTION
!  KWA     MOT QUI CONTIENT LE CHAMP A INSERER JUSTIFIE A DROITE
!  BITPOS  POSITION DU DERNIER BIT (A DROITE) DU CHAMP A INSERER
!          LE BIT 0 EST LE BIT SIGNE DU PREMIER MOT DU TABLEAU
!  LONG    LONGUEUR EN BIT DU CHAMP A INSERER (PAS PLUS DE 32 BITS)


module error_count
    use app
    implicit none

    integer :: num_errors = 0

    contains

    SUBROUTINE TESTIT(IER, expected_min, expected_max, expected_val, message)
        implicit none
        INTEGER, intent(in) :: IER
        integer, intent(in), optional :: expected_min
        integer, intent(in), optional :: expected_max
        integer, intent(in), optional :: expected_val
        character(len=*), intent(in), optional :: message

        integer :: l_expected_min, l_expected_max
        character(len=:), allocatable :: msg

        msg = ' '
        l_expected_min = 0
        l_expected_max = 0
        if (present(expected_min)) then
            l_expected_min = expected_min
            if (.not. present(expected_max)) l_expected_max = huge(l_expected_max)
        endif
        if (present(expected_max)) then
            l_expected_max = expected_max
            if (.not. present(expected_min)) l_expected_min = -(huge(l_expected_min))
        endif
        if (present(expected_val)) then
            l_expected_min = expected_val
            l_expected_max = expected_val
        endif
        if (present(message)) msg = message

        IF ((IER .LT. l_expected_min) .or. (IER .gt. l_expected_max)) THEN
            WRITE(app_msg,444) msg, IER, l_expected_min, l_expected_max
            call app_log(APP_ERROR, app_msg)
            num_errors = num_errors + 1
            error stop 1
        ELSE 
            call App_Log(APP_DEBUG,' --- REUSSI ---')
        ENDIF 
444     FORMAT(' <<< ERREUR >>> ', A, ', IER = ',I8, ', expected in range [' ,I8, ', ', I8, ']')
        RETURN
    END SUBROUTINE TESTIT

end module error_count

PROGRAM TESTBRP
    use ieee_arithmetic, only: ieee_is_nan
    use rmn_common
    use error_count
    IMPLICIT NONE


    INTEGER FNOM,MRFCLS,MRFGET,MRFLOC,MRFOPN,MRFPRM,MRFPUT,MRBCVT
    INTEGER MRBADD,MRBDEL,MRBHDR,MRBINI,MRBLEN,MRBLOC,MRBPRM
    INTEGER MRBREP,MRBXTR,MRBUPD,HRJUST,MRFVOI,HLJUST, MRBRPT,MRBSCT
    INTEGER MRFDEL,MRFMXL,MRBCOV,MRBDCV,MRFOPR,MRFOPC,MRFGOR,MRFGOC, qrbnbdt
    INTEGER MRBCOL, MRBDCL,MRFNBR, MRBLOCX, MRBTYP, MRBTBL, MRBPRML
    EXTERNAL REMPLI, qrbnbdt
    EXTERNAL FNOM,MRFCLS,MRFGET,MRFLOC,MRFOPN,MRFPRM,MRFPUT, MRBTBL
    EXTERNAL MRBADD,MRBDEL,MRBHDR,MRBINI,MRBLEN,MRBLOC,MRBPRM, MRBPRML
    EXTERNAL MRBREP,MRBXTR,MRBUPD,HRJUST,MRFVOI,HLJUST,CCARD
    EXTERNAL MRFDEL,MRFMXL,MRBCOV,MRBDCV,MRBCOL,MRBDCL,MRBCVT,MRFOPR
    EXTERNAL MRFOPC,MRFGOR,MRFGOC, MRBRPT,MRBSCT,MRFNBR, MRBLOCX, MRBTYP
    EXTERNAL RAH2CHAR

    INTEGER BUFA(8000),BUFB(5000),BUFC(5000),TBLVAL(8000),BUFD(8000)
    INTEGER BUFE(20000),ETBLVAL(8000)
    REAL RVAL(500)
    REAL rtablo(10), rtabloc(10)
    REAL(kind = real64) rtablo8(10), rtablo8c(10)
    complex ctablo(10), ctabloc(10)
    complex(kind = real64) ctablo8(10), ctablo8c(10)
    integer rliste8(20),cliste(20),cliste8(40)
    integer rliste8c(20),clistec(20),cliste8c(40)
    INTEGER BIT0,BLKNO,LSTELEA(10),LSTELEB(4),LSTELEC(3),LSTELEE(130)
    INTEGER CLISTEA(10),CLISTEB(4), CLISTEE(130)
    REAL T1,T2,CHRONO,OPVALR
    INTEGER LBITS,LEFT,IER,IER1,IER2,IER3,I,J,IPOS
    INTEGER SUP(2),XAUX(2),NSUP,NXAUX
    INTEGER TBLCOMP(500),LSTCOMP(10)
    CHARACTER(len = 9) :: STNID,OPVALC
    INTEGER TEMPS,FLGS,IDTYP,LATI,LONG,ELEV,DRCV,DATE,OARS,RUN,NBLK
    INTEGER DX, DY, ZEROCPL
    INTEGER NELE,NVAL,NT,BTYP,NBIT,DATYP,LONENR, LONMAX, BFAM,BDESC
    INTEGER BKTYP, BKSTP, BKNAT
    CHARACTER(len = 8) :: LISTE(5),DEF1(5),DEF2(5)
    INTEGER USRTBL(10), CUSRTBL(3,10), RELEM
    INTEGER TBLBURP(4,20)
    INTEGER TBLPRM(10,30)
    INTEGER ZERO

!#define IgnoreTypeKindRank rtablo, rtablo8, rtablo8c, rtabloc, ctablo, ctablo8, ctabloc, ctablo8c, rliste8, rliste8c, cliste, cliste8
!#include <rmn/IgnoreTypeKindRank.hf>

    DATA LISTE /'L','IMFICHE','IMFICHE2','CHRONO','QUICK'/
    DATA DEF1 /'OUTPUT','BRPFIL1','BRPFIL2','OUI','OUI'/
    DATA DEF2 /'OUTPUT','BRPFIL1','BRPFIL2','NUL','NUL'/


    DATA LSTELEA /2,1001,2121,2122,2129,4031,6011,10002,10061,20009/
    DATA LSTELEB /2,1001,2121,2122/
    DATA LSTELEC /2,1001,2121/
    DATA LSTELEE /1001,128*2121,2122/
    DATA USRTBL /63000, 63010, 63123, 63124, 63125,63200, 63201, 63222, 63234, 63255 /
    num_errors = 0

    ZERO = 0
    IPOS = -1
    CALL CCARD(LISTE,DEF1,DEF2,5,IPOS)
    IER = FNOM(6,DEF2(1),'SEQ+FTN+FMT',0)
    IER = FNOM(10,DEF2(2),'RND+R/W',0)
    IER = FNOM(20,DEF2(3),'RND+R/W',0)
    WRITE(6,333)0.0,'MRFOPC ERRTOLR MIS A SYSTEM'
    IER=MRFOPC('ERRTOLR','SYSTEM')
    CALL TESTIT(IER)
    WRITE(6,333)0.1,'MRFGOC ERRTOLR MIS A SYSTEM'
    IER=MRFGOC('ERRTOLR',OPVALC)
    WRITE(6,*)' ERRTOLR = ',OPVALC
    CALL TESTIT(IER)
    WRITE(6,333) 0.2, 'MRFOPC - MSGLVL MIS A TRIVIAL'
    IER = MRFOPC('MSGLVL','TRIVIAL')
    CALL TESTIT(IER)
    WRITE(6,333) 1.0,'MRFOPN EN MODE CREATE'
    IER = MRFOPN(10,'CREATE')
    CALL TESTIT(IER)
    WRITE(6,333) 1.1,'MRFCLS FICHIER 10'
    IER = MRFCLS(10)
    CALL TESTIT (IER)
    WRITE(6,333) 1.2,' MRFOPN EN MODE APPEND 10'
    IER = MRFOPN(10,'APPEND')
    CALL TESTIT (IER)
    BUFA(1) = 8000
    BUFB(1) = 5000
    BUFC(1) = 5000
    BUFD(1) = 8000
    BUFE(1) = 20000
    WRITE(6,333) 2.0,'MRBINI POUR 3 RAPPORTS'
    IER1 = MRBINI(10,BUFA,1129,ishft(-1,-(32-(8))),'STATION#1',2,1800,3600,0,0,975,42,910222,65535,1,SUP,ZERO,XAUX,ZERO)
    IER2 = MRBINI(10,BUFB,0222,ishft(-1,-(32-(5))),'STATION#2',3,900,1200,1500,1200,240,30,910223,60,1,SUP,ZERO,XAUX,ZERO)
    IER3 = MRBINI(10,BUFC,0333,ishft(-1,-(32-(3))),'STATION#3',4,450,600,0,0,100,68,910224,90,1,SUP,ZERO,XAUX,ZERO)
    CALL TESTIT(IER1+IER2+IER3)
    WRITE(6,333) 2.0,'MRBADD (20 FOIS) POUR RAPPORT BUFA'
    IER = 0
    DO I = 1,20
        CALL REMPLI(TBLVAL,10,5,7)
        IER1 = MRBADD(BUFA,BLKNO,10,5,7,5,12,I,12,BIT0,2,LSTELEA,TBLVAL)
        IER = IER + IER1
        PRINT *,' *** INFO BLKNO, BIT0 = ',BLKNO,BIT0
    END DO
    CALL TESTIT(IER)
    WRITE(6,333) 2.1,'MRBADD (10 FOIS) POUR RAPPORT BUFB'
    DO I = 1,10
        CALL REMPLI(TBLVAL,4,5,7)
        IER1 = MRBADD(BUFB,BLKNO,4,5,7,773,0,I,3,BIT0,2,LSTELEB, TBLVAL)
        IER = IER + IER1
        PRINT *,' *** INFO BLKNO, BIT0 = ',BLKNO,BIT0
    END DO
    CALL TESTIT(IER)
    WRITE(6,333) 2.2,'MRBADD (12 FOIS) POUR RAPPORT BUFC'
    DO I = 1,12
        DO J = 1,3
            TBLVAL(J) = I
        END DO
        IER1 = MRBADD(BUFC,BLKNO,3,1,1,5,12,I,32,BIT0,2,LSTELEC, TBLVAL)
        CALL TESTIT(IER1)
        IER = IER + IER1
        PRINT *,' *** INFO BLKNO, BIT0 = ',BLKNO,BIT0
    END DO
    CALL TESTIT(IER)
    WRITE(6,333) 3.0,'MRBLEN POUR RAPPORTS BUFA BUFB ET BUFC'
    IER1 = MRBLEN(BUFA,LBITS,LEFT)
    PRINT *,' *** INFO LBITS, LEFT = ',LBITS,LEFT
    IER2 = MRBLEN(BUFB,LBITS,LEFT)
    PRINT *,' *** INFO LBITS, LEFT = ',LBITS,LEFT
    IER3 = MRBLEN(BUFC,LBITS,LEFT)
    PRINT *,' *** INFO LBITS, LEFT = ',LBITS,LEFT
    CALL TESTIT(IER1+IER2+IER3)
    WRITE(6,333) 4.0,'MRBDEL POUR RAPPORT BUFB + MRBLEN'
    IER = MRBDEL(BUFB,2)
    IER1 = MRBLEN(BUFB,LBITS,LEFT)
    PRINT *,' *** INFO LBITS, LEFT = ',LBITS,LEFT
    CALL TESTIT(IER+IER1)
    WRITE(6,333) 5.0,'MRBXTR POUR BLOC5 BUFA'
    DO I=1,350
        TBLCOMP(I) = I*5
    END DO
    IER = MRBXTR(BUFA,5,LSTCOMP,TBLVAL)
    IER = 0
    WRITE(6,*)' LSTCOMP  LSTELEA'
    DO I = 1,10
        WRITE(6,*)LSTCOMP(I),LSTELEA(I)
        IF((LSTCOMP(I).NE. LSTELEA(I)))THEN
            IER =IER - 1
        ENDIF
    END DO
    CALL TESTIT(IER)
    IER = 0
    DO I= 1,350
        IF((TBLCOMP(I).NE. TBLVAL(I)))THEN
            IER = IER -1
        ENDIF
    END DO
    CALL TESTIT (IER)
    WRITE(6,333) 5.1,'MRBXTR POUR BLOC5 BUFC'
    IER = MRBXTR(BUFC,5,LSTCOMP,TBLVAL)
    WRITE(6,*)' CONTENU DE TBLVAL 3 ELEMENTS DE VALEUR 5'
    WRITE(6,*) (TBLVAL(I),I=1,4)
    CALL TESTIT(IER)
    WRITE(6,333) 5.2,'MRBDEL DU BLOC 5 BUFC'
    IER = MRBDEL(BUFC,5)
    CALL TESTIT(IER)
    WRITE(6,333) 5.3,'MRBXTR POUR BLOC5 BUFC'
    IER = MRBXTR(BUFC,5,LSTCOMP,TBLVAL)
    WRITE(6,*)' CONTENU DE TBLVAL 3 ELEMENTS DE VALEUR 6'
    WRITE(6,*) (TBLVAL(I),I=1,4)
    CALL TESTIT(IER)
    WRITE(6,333) 6.0,'MRBHDR BUFA'
    IER = MRBHDR(BUFA,TEMPS,FLGS,STNID,IDTYP,LATI,LONG,DX,DY,ELEV,DRCV,DATE,OARS,RUN,NBLK,SUP,NSUP,XAUX,NXAUX)
    WRITE(6,*)' TEMPS = ',TEMPS
    WRITE(6,*)' FLGS  = ',FLGS
    WRITE(6,400) STNID
    WRITE(6,*)' IDTYP = ',IDTYP
    WRITE(6,*)' LATI  = ',LATI
    WRITE(6,*)' LONG  = ',LONG
    WRITE(6,*)' DX    = ',DX
    WRITE(6,*)' DY    = ',DY
    WRITE(6,*)' ELEV  = ',ELEV
    WRITE(6,*)' DRCV  = ',DRCV
    WRITE(6,*)' DATE  = ',DATE
    WRITE(6,*)' OARS  = ',OARS
    WRITE(6,*)' RUN   = ',RUN
    WRITE(6,*)' NBLK  = ',NBLK
    CALL TESTIT(IER)
    WRITE(6,333) 6.1,'MRBHDR BUFB'
    IER = MRBHDR(BUFB,TEMPS,FLGS,STNID,IDTYP,LATI,LONG,DX,DY,ELEV,DRCV,DATE,OARS,RUN,NBLK,SUP,NSUP,XAUX,NXAUX)
    WRITE(6,*)' TEMPS = ',TEMPS
    WRITE(6,*)' FLGS  = ',FLGS
    WRITE(6,400) STNID
    WRITE(6,*)' IDTYP = ',IDTYP
    WRITE(6,*)' LATI  = ',LATI
    WRITE(6,*)' LONG  = ',LONG
    WRITE(6,*)' DX    = ',DX
    WRITE(6,*)' DY    = ',DY
    WRITE(6,*)' ELEV  = ',ELEV
    WRITE(6,*)' DRCV  = ',DRCV
    WRITE(6,*)' DATE  = ',DATE
    WRITE(6,*)' OARS  = ',OARS
    WRITE(6,*)' RUN   = ',RUN
    WRITE(6,*)' NBLK  = ',NBLK
    CALL TESTIT(IER)
    WRITE(6,333) 6.2,'MRBHDR BUFC'
    IER = MRBHDR(BUFC,TEMPS,FLGS,STNID,IDTYP,LATI,LONG,DX,DY,ELEV,DRCV,DATE,OARS,RUN,NBLK,SUP,NSUP,XAUX,NXAUX)
    WRITE(6,*)' TEMPS = ',TEMPS
    WRITE(6,*)' FLGS  = ',FLGS
    WRITE(6,400) STNID
    WRITE(6,*)' IDTYP = ',IDTYP
    WRITE(6,*)' LATI  = ',LATI
    WRITE(6,*)' LONG  = ',LONG
    WRITE(6,*)' DX    = ',DX
    WRITE(6,*)' DY    = ',DY
    WRITE(6,*)' ELEV  = ',ELEV
    WRITE(6,*)' DRCV  = ',DRCV
    WRITE(6,*)' DATE  = ',DATE
    WRITE(6,*)' OARS  = ',OARS
    WRITE(6,*)' RUN   = ',RUN
    WRITE(6,*)' NBLK  = ',NBLK
    CALL TESTIT(IER)

    ! -------------------------------------------------------------
    WRITE(6,333) 7.0,'MRBLOC BFAM =5,BDESC=12,BTYP=7 BUFA-BUFB-BUFC'
    BLKNO = 0
    IER=MRBLOC(BUFA,773,0,7,BLKNO)
    call testit(IER, expected_min = 1)
    WRITE(6,*)' BLKNO = ',IER
    IER1=MRBLOC(BUFB,5,12,7,BLKNO)
    call testit(IER1, expected_min = 1)
    WRITE(6,*)' BLKNO = ',IER1
    IER2=MRBLOC(BUFC,773,-1,7,BLKNO)
    call testit(IER2, expected_min = 1)
    WRITE(6,*)' BLKNO = ',IER2
    ! CALL TESTIT(IER+IER1+IER2)

    ! -------------------------------------------------------------
    WRITE(6,333) 7.1,'MRBLOC BFAM =3,BDESC=-1,BTYP=7 BUFA-BUFB-BUFC (do not exist)'
    IER=MRBLOC(BUFA,3,-1,7,BLKNO)
    WRITE(6,*)' BLKNO = ',IER
    IER1=MRBLOC(BUFB,3,-1,7,BLKNO)
    WRITE(6,*)' BLKNO = ',IER1
    IER2=MRBLOC(BUFC,3,-1,7,BLKNO)
    WRITE(6,*)' BLKNO = ',IER2
    IER = -IER
    IER1 = -IER1
    IER2 = -IER2
      CALL TESTIT(IER+IER1+IER2, expected_val = 3)

    ! -------------------------------------------------------------
    WRITE(6,333) 7.2,'MRBLOC BFAM =5,BDESC=0,BTYP=7 BUFA-BUFB-BUFC (do not exist)'
    !     n'existe pas

    IER=MRBLOC(BUFA,5,0,7,BLKNO)
    WRITE(6,*)' BLKNO = ',IER
    IER1=MRBLOC(BUFB,5,0,7,BLKNO)
    WRITE(6,*)' BLKNO = ',IER1
    IER2=MRBLOC(BUFC,5,0,7,BLKNO)
    WRITE(6,*)' BLKNO = ',IER2
    IER = -IER
    IER1 = -IER1
    IER2 = -IER2
      CALL TESTIT(IER+IER1+IER2, expected_val = 3)

    ! -------------------------------------------------------------
    WRITE(6,333) 8.0,'MRBPRM BLOC NO 2 BUFA-BUFB-BUFC'
    IER = MRBPRM(BUFA,2,NELE,NVAL,NT,BFAM,BDESC,BTYP,NBIT,BIT0,DATYP)
    WRITE(6,*)' NELE   NVAL   NT  BFAM   BDESC  BTYP   NBIT    BIT0   DATYP'
    WRITE(6,777) NELE,NVAL,NT,BFAM,BDESC,BTYP,NBIT, BIT0,DATYP
    IER1 = MRBPRM(BUFB,2,NELE,NVAL,NT,BFAM,BDESC,BTYP,NBIT,BIT0,DATYP)
    WRITE(6,777) NELE,NVAL,NT,BFAM,BDESC,BTYP,NBIT, BIT0,DATYP
    IER2 = MRBPRM(BUFC,2,NELE,NVAL,NT,BFAM,BDESC,BTYP,NBIT,BIT0,DATYP)
    WRITE(6,777) NELE,NVAL,NT,BFAM,BDESC,BTYP,NBIT, BIT0,DATYP

    ! -------------------------------------------------------------
    WRITE(6,333) 8.1,'MRBPRML BUFA TOUS LES BLOCS'
    IER = MRBPRML(BUFA,0,TBLPRM,10,30)
    WRITE(6,*)' NOMBRE DE BLOCS RETOURNES" ', IER
    WRITE(6,*)' BKNO   NELE   NVAL   NT  BFAM   BDESC  BTYP '
    write(6,*)   ' NBIT    BIT0   DATYP'
    DO I = 1,IER
        WRITE(6,778)(TBLPRM(J,I),J=1,10)
    END DO
    CALL TESTIT(IER, expected_val = 20)

    ! -------------------------------------------------------------
    WRITE(6,333) 8.2,'MRBPRML BUFA 10 BLOCS A PARTIR DE BKNO 3'
    IER = MRBPRML(BUFA,3,TBLPRM,10,10)
    WRITE(6,*)' NOMBRE DE BLOCS RETOURNES" ',IER
    WRITE(6,*)' BKNO   NELE   NVAL   NT  BFAM   BDESC  BTYP '
    write(6,*)   '  NBIT    BIT0   DATYP'
    DO I = 1,IER
        WRITE(6,778)(TBLPRM(J,I),J=1,10)
    END DO
    CALL TESTIT(IER, expected_min = 10, expected_max = 10)

    ! -------------------------------------------------------------
    WRITE(6,333) 9.0,'MRFPUT BUFA-BUFB-BUFC'
    IER = MRFPUT(10,0,BUFA)
    IER1= MRFPUT(10,0,BUFB)
    IER2= MRFPUT(10,0,BUFC)
    CALL TESTIT(IER+IER1+IER2)
    WRITE(6,333) 10.0,'MRFVOI FICHIER 10'
    IER = MRFVOI(10)
    CALL TESTIT(IER)
    WRITE(6,333) 11.0,'REMPLACER BLOC8 PAR BLOC5 DANS BUFA'
    IER = MRBREP(BUFA,8,TBLCOMP)
    IER = MRBXTR(BUFA,8,LSTCOMP,TBLVAL)
    IER = 0
    DO I = 1,10
        IF((LSTCOMP(I).NE. LSTELEA(I)))THEN
            IER =IER - 1
        ENDIF
    END DO
    CALL TESTIT(IER)
    IER = 0
    DO I= 1,350
        IF((TBLCOMP(I).NE. TBLVAL(I)))THEN
            IER = IER -1
        ENDIF
    END DO
    CALL TESTIT (IER)
    WRITE(6,333) 11.1,'REMPLACER BLOC1 PAR BLOC PLUS GROS BLOC19 POSITIF'
    IER = MRBXTR(BUFA,19,LSTCOMP,TBLCOMP)
    IER = MRBREP(BUFA,1,TBLCOMP)
    IER = MRBXTR(BUFA,1,LSTELEA,TBLVAL)
    IER = 0
    DO I = 1,10
        IF((LSTCOMP(I).NE. LSTELEA(I)))THEN
            IER =IER - 1
        ENDIF
    END DO
    CALL TESTIT (IER)
    IER = 0
    DO I= 1,350
        IF((TBLVAL(I).NE. TBLCOMP(I)))THEN
            IER = IER -1
        ENDIF
    END DO
    CALL TESTIT (IER)
    WRITE(6,333) 11.2,'REMPLACER BLOC4 PAR BLOC PLUS GROS AVEC NEGATIFS'
    IER = MRBXTR(BUFA,19,LSTCOMP,TBLCOMP)
    TBLCOMP(1) = -100
    TBLCOMP(100) = -345678
    TBLCOMP(250) = -456789
    IER = MRBREP(BUFA,4,TBLCOMP)
    IER = MRBXTR(BUFA,4,LSTELEA,TBLVAL)
    IER = 0
    DO I = 1,10
        IF((LSTCOMP(I).NE. LSTELEA(I)))THEN
            IER =IER - 1
        ENDIF
    END DO
    CALL TESTIT (IER)
    IER = 0
    DO I= 1,350
        IF((TBLVAL(I).NE. TBLCOMP(I)))THEN
            IER = IER -1
        ENDIF
    END DO
    CALL TESTIT (IER)
    WRITE(6,333) 11.3,'REMPLACER BLOC5 PAR BLOC GROS ENTIERS +-'
    IER = MRBXTR(BUFA,19,LSTCOMP,TBLCOMP)
    TBLCOMP(1) = 2147483646
    IER = MRBREP(BUFA,5,TBLCOMP)
    IER = MRBXTR(BUFA,5,LSTELEA,TBLVAL)
    IER = 0
    DO I = 1,10
        IF((LSTCOMP(I).NE. LSTELEA(I)))THEN
            IER =IER - 1
        ENDIF
    END DO
    CALL TESTIT (IER)
    IER = 0
    DO I= 1,350
        IF((TBLVAL(I).NE. TBLCOMP(I)))THEN
            IER = IER -1
        ENDIF
    END DO
    CALL TESTIT (IER)
    WRITE(6,333) 11.4,'REMPLACER BLOC8 PAR BLOC de manquants dans bufa'
    ZEROCPL =NOT(0)
    DO I= 1,350
        TBLCOMP(I) = ZEROCPL
    END DO
    IER = MRBREP(BUFA,8,TBLCOMP)
    IER = MRBXTR(BUFA,8,LSTCOMP,TBLVAL)
    IER = 0
    DO I = 1,10
        IF((LSTCOMP(I).NE. LSTELEA(I)))THEN
            IER =IER - 1
        ENDIF
    END DO
    CALL TESTIT (IER)
    IER = 0
    DO I= 1,350
        IF((TBLVAL(I).NE. ZEROCPL))THEN
            IER = IER -1
        ENDIF
    END DO
    CALL TESTIT (IER)
    WRITE(6,333) 12.0,'MRBUPD & MRFPUT DE BUFA ELEV DE 1 A 100'
    WRITE(6,333) 12.1,'MRBUPD STATION#4 A STATION#6'
    STNID = 'ST*T**N#1'
    IER2 = 0
    DO I =4,6
        !         I+48 EGAL VALEUR ASCII DE I
        !         WRITE(STNID(9:9),'(A1)') HLJUST(I+48,1)
        call rah2char(STNID(9:9),I+48,1)
        DO J = 1,100
            IER = MRBUPD(10,BUFA,1129,255,STNID,3,4523,26300,0,0,J,      32,901029,0,2,SUP,NSUP,XAUX,NXAUX)
            IER1 = MRFPUT(10,0,BUFA)
            IER2 = IER2 + IER + IER1
        END DO
    END DO
    CALL TESTIT(IER2)
    ! -------------------------------------------------------------
    WRITE(6,333) 13.0,'MRFCLS FICHIER 10'
    IER =MRFCLS(10)
    CALL TESTIT(IER)
    ! -------------------------------------------------------------
    WRITE(6,333) 14.0,'MRFVOI FICHIER 10 FERME'
    IER = MRFVOI(10)
    CALL TESTIT(IER)
    ! -------------------------------------------------------------
    WRITE(6,333) 15.0,'MRFOPN EN MODE READ'
    IER = MRFOPN(10,'READ')
    CALL TESTIT(IER, expected_val = 303)
    ! -------------------------------------------------------------
    WRITE(6,333) 16.0,'MRFLOC STATIO#6,DATE=901029,IDTYP=3'
    IER1 = MRFLOC(10,0,'STATION#6',3,4523,26300,901029,1118,SUP,ZERO)
    WRITE(6,*)' HANDLE = ',IER1
    CALL TESTIT(IER1, expected_min = 100)
    ! -------------------------------------------------------------
    WRITE(6,333) 17.0,'MRFGET DU RAPPORT A POSITION HANDLE'
    IER = MRFGET(IER1,BUFD)
    DO I=22,7180
        IF((BUFD(I).NE. BUFA(I)))THEN
            IER = IER -1
        ENDIF
    END DO
    CALL TESTIT(IER)
    ! -------------------------------------------------------------
    WRITE(6,333) 18.0,'MRFPRM DE L''ENREGISTREMENT LU'
    IER = MRFPRM(IER1,STNID,IDTYP,LATI,LONG,DX,DY,DATE,TEMPS,FLGS,SUP,NSUP,LONENR)
    CALL TESTIT(IER)
    ! -------------------------------------------------------------
    WRITE(6,850)
    WRITE(6,800)STNID,TEMPS,LATI,LONG,DX,DY,FLGS,DATE,IDTYP,LONENR
    WRITE(6,333) 18.1,' MRBLOC ET MRBPRM DE TOUS LES BLOCKS'
    BLKNO = 0
    BLKNO = MRBLOC(BUFD,-1,-1,-1,BLKNO)
    DO WHILE ((BLKNO.GE. 0))
        WRITE(6,*)' NO DE BLOCK = ',BLKNO
        IER = MRBPRM(BUFD,BLKNO,NELE,NVAL,NT,BFAM,BDESC,BTYP,NBIT,   BIT0,DATYP)
        WRITE(6,*)   ' NELE   NVAL   NT  BFAM   BDESC  BTYP   NBIT    BIT0   DATYP'
        WRITE(6,777) NELE,NVAL,NT,BFAM,BDESC,BTYP,NBIT, BIT0,DATYP
        BLKNO = MRBLOC(BUFD,-1,-1,-1,BLKNO)
    END DO
    ! -------------------------------------------------------------
    WRITE(6,333) 19.0,'MRFLOC PROCHAIN STATIO#6,DATE=901029,IDTYP=3'
    IER1 = MRFLOC(10,IER1,'STATION#6',3,4523,26300,901029,1143,SUP,ZERO)
    WRITE(6,*)' HANDLE = ',IER1
    CALL TESTIT(IER1, expected_min = 100)
    ! -------------------------------------------------------------
    WRITE(6,333) 20.0,'MRFGET DU RAPPORT A POSITION HANDLE'
    IER = MRFGET(IER1,BUFD)
    DO I=22,7180
        IF((BUFD(I).NE. BUFA(I)))THEN
            IER = IER -1
        ENDIF
    END DO
    CALL TESTIT(IER)
    ! -------------------------------------------------------------
    WRITE(6,333) 21.0,'MRFPRM DE L''ENREGISTREMENT LU'
    IER = MRFPRM(IER1,STNID,IDTYP,LATI,LONG,DX,DY,DATE,TEMPS,FLGS,SUP,NSUP,LONENR)
    CALL TESTIT(IER)
    ! -------------------------------------------------------------
    WRITE(6,850)
    WRITE(6,800)STNID,TEMPS,LATI,LONG,DX,DY,FLGS,DATE,IDTYP,LONENR
    WRITE(6,333) 21.1,'MRFMXL DU FICHIER 10'
    LONMAX = MRFMXL(10)
    WRITE(6,*)'LONMAX = ',LONMAX
    CALL TESTIT(LONMAX, expected_min = 0, expected_max = 100000)
    ! -------------------------------------------------------------
    WRITE(6,333) 21.2,' MRFOPC - MSGLVL MIS A FATAL'
    IER = MRFOPC('MSGLVL','FATAL')
    CALL TESTIT(IER)
    ! -------------------------------------------------------------
    WRITE(6,333) 22.0,'MRFOPN FICHIER 20 MODE CREATE'
    IER = MRFOPN(20,'CREATE')
    CALL TESTIT(IER)
    ! -------------------------------------------------------------
    ier1 = 0
    IF((DEF2(5).EQ. 'NUL'))THEN
        WRITE(6,333) 23.0,'MRBUPD-MRFPUT BUFB DATE = 1 A 8000'
        DO I=1,8000
            IER=IER + MRBUPD(20,BUFB,1200,122,'STATION#9',4,2300,      18934,1500,1200,1452,3,I,0,2,SUP,NSUP,XAUX,NXAUX)
            IER1=IER1+MRFPUT(20,0,BUFB)
        END DO
        CALL TESTIT(IER, message = 'mrbupd')
        CALL TESTIT(IER1, message = 'mrfput')
        IF((DEF2(4).NE. 'NUL'))THEN
            WRITE(6,333) 24.0,      'MRFLOC DATE =100,8000 INCR=100 BOUCLE=50'
            WRITE(6,*)'   DATE       HANDLE        CHRONO'
            DO I=100,8000,100
                DO J = 1,50
                    IER = MRFLOC(20,0,'STATION#9',4,-1,-1,I,-1,SUP,ZERO)
                END DO
                WRITE(6,1202)I,IER
            END DO
        ELSE
            I = 8000
            IER = MRFLOC(20,0,'STATION#9',4,-1,-1,I,-1,SUP,ZERO)
        ENDIF
    ELSE
        WRITE(6,333) 23.0,'MRBUPD-MRFPUT BUFB DATE = 1 A 200'
        DO I=1,200
            IER=IER + MRBUPD(20,BUFB,1200,122,'STATION#9',4,2300,      18934,1500,1200,1452,3,I,0,2,SUP,NSUP,XAUX,NXAUX)
            IER1=IER1+MRFPUT(20,0,BUFB)
        END DO
        CALL TESTIT(IER)
        CALL TESTIT(IER1)
        IF((DEF2(4).NE. 'NUL'))THEN
            WRITE(6,333) 24.0,      'MRFLOC DATE =100,200 INCR=100 BOUCLE=50'
            WRITE(6,*)'   DATE       HANDLE        CHRONO'
            DO I=100,200,100
                DO J = 1,50
                    IER = MRFLOC(20,0,'STATION#9',4,-1,-1,I,-1,SUP,ZERO)
                END DO
                CHRONO = (T2-T1)/50.0
                WRITE(6,1202)I,IER
            END DO
        ELSE
            I = 200
            IER = MRFLOC(20,0,'STATION#9',4,-1,-1,I,-1,SUP,ZERO)
        ENDIF
    ENDIF
    CALL TESTIT (IER, expected_min = 1)
    ! -------------------------------------------------------------
    WRITE(6,333) 24.1,' MRFOPC - MSGLVL MIS INFORMATIF'
    IER1 = MRFOPC('MSGLVL','INFORMATIF')
    CALL TESTIT (IER1)
    ! -------------------------------------------------------------
    WRITE(6,333) 24.2,'MRFDEL DU DERNIER ENREGISTREMENT'
    IER = MRFDEL(IER)
    CALL TESTIT(IER)

    !      WRITE(6,333) 25.0,'MRFCLS FICHIER 10'
    !      IER = MRFCLS(10)
    !      CALL TESTIT(IER)

    ! -------------------------------------------------------------
    WRITE(6,333) 25.1,'MRFCLS FICHIER 20'
    IER = MRFCLS(20)
    CALL TESTIT(IER)
    ! -------------------------------------------------------------
    WRITE(6,333) 26.0,'MRBCOL LSTELEA'
    IER =  MRBCOL(LSTELEA,CLISTEA,10)
    WRITE(6,*)' LSTELEA'
    WRITE(6,*)(LSTELEA(I),I=1,10)
    WRITE(6,*)' CLISTEA'
    WRITE(6,*)(CLISTEA(I),I=1,10)
    CALL TESTIT(IER)
    WRITE(6,333) 26.1,'MRBDCL CLISTEA'
    IER = MRBDCL(CLISTEA,LSTCOMP,10)
    DO I=1,10
        IF((LSTCOMP(I).NE. LSTELEA(I)))THEN
            IER = IER - 1
        ENDIF
    END DO
    CALL TESTIT(IER)
    ! -------------------------------------------------------------
    WRITE(6,333) 26.2,'MRBCOV LSTELEB'
    IER = 0
    DO I = 1,4
        CLISTEB(I) = MRBCOV(LSTELEB(I))
        IF((CLISTEB(I).NE. CLISTEA(I)))THEN
            IER = IER -1
        ENDIF
    END DO
    CALL TESTIT(IER)
    ! -------------------------------------------------------------
    WRITE(6,333) 26.3,'MRBDCV LSTELEB'
    IER = 0
    DO I = 1,4
        LSTCOMP(I) = MRBDCV(CLISTEB(I))
        IF((LSTCOMP(I).NE. LSTELEB(I)))THEN
            IER = IER -1
        ENDIF
    END DO
    CALL TESTIT(IER)
    ! -------------------------------------------------------------
    WRITE(6,333) 27.0,'MRFOPR MISSING = 99999.99'
    IER = MRFOPR('MISSING',99999.99)
    CALL TESTIT(IER)
    ! -------------------------------------------------------------
    WRITE(6,333) 27.1,'MRFGOR VALEUR DE MISSING'
    IER = MRFGOR('MISSING',OPVALR)
    WRITE(6,*)' MISSING = ',OPVALR
    CALL TESTIT(IER)
    ! -------------------------------------------------------------
    WRITE(6,333) 27.2,'MRBCVT LSTELEB DE BUFR A REEL'
    CALL REMPLI(TBLVAL,4,5,7)
    !     mettre des valeurs manquantes et negatives

    ZEROCPL =NOT(0)
    TBLVAL(2) = ZEROCPL
    TBLVAL(3) = ZEROCPL
    TBLVAL(4) = ZEROCPL
    TBLVAL(5) = -1
    TBLVAL(6) = -2
    TBLVAL(7) = -217
    TBLVAL(1) = -32
    WRITE(6,*)' TBLVAL'
    WRITE(6,*)(TBLVAL(I),I=1,140)
    IER = MRBCVT(CLISTEB,TBLVAL,RVAL,4,5,7,0)
    WRITE(6,*)' VALEUR DE IER APRES MRBCVT :', IER
    WRITE(6,*)' RVAL'
    WRITE(6,*)(RVAL(I),I=1,140)
    CALL TESTIT(IER)
    ! -------------------------------------------------------------
    WRITE(6,333) 27.3,'MRBCVT LSTELEB DE  REEL A BUFR '
    DO I =1,140
        TBLCOMP(I) = 0
    END DO
    IER =  MRBCVT(CLISTEB,TBLCOMP,RVAL,4,5,7,1)
    WRITE(6,*)' TBLCOMP'
    WRITE(6,*)(TBLCOMP(I),I=1,140)
    CALL TESTIT(IER)

    BLOCK
        INTEGER, dimension(1, 2, 2) :: table_int
        REAL, dimension(1, 2, 2) :: table_real
        INTEGER :: elem_id

        write(6, 333) 28.0, 'MRBCVT with missing element'

        elem_id = -123456
        table_int = 0
        table_real = 0.0

        IER = MRBCVT(elem_id, table_int, table_real, 1, 2, 2, 0)
        call TESTIT(IER)
        if (.not. all(ieee_is_nan(table_real(:, :, :)))) then
            write(6, *) 'Wrong values for element that was not found, MRBCVT'
            write(6, *) table_real
            error stop 1
         end if

        IER = MRBCVT(elem_id, table_int, table_real, 1, 2, 2, 1)
        call TESTIT(IER)
        if (.not. all(table_int(:, :, :) == -1)) then
            write(6, *) 'Wrong values for element that was not found, MRBCVT'
            write(6, *) table_int
            error stop 1
        end if
    END BLOCK

    ! -------------------------------------------------------------
    WRITE(6,333) 30.0,'MRBINI BUFE'
    IER3 = MRBINI(10,BUFE,0333,ishft(-1,-(32-(3))),'STATION#3',4,450,600,0,0,100,68,910224,90,1,SUP,ZERO,XAUX,ZERO)
    WRITE(6,333) 30.1,'MRBADD (3 FOIS) POUR RAPPORT BUFE DIM en 16 BITS'
    IER= 0
    CALL REMPLI(TBLVAL,130,5,7)
    IER1 = MRBADD(BUFE,BLKNO,130,5,7,5,12,1,32,BIT0,2,LSTELEE,TBLVAL)
    IER = IER + IER1
    PRINT *,' *** INFO BLKNO, BIT0 = ',BLKNO,BIT0
    CALL REMPLI(TBLVAL,4,268,7)
    IER1 = MRBADD(BUFE,BLKNO,4,268,7,5,12,2,32,BIT0,2,LSTELEE,TBLVAL)
    IER = IER + IER1
    PRINT *,' *** INFO BLKNO, BIT0 = ',BLKNO,BIT0
    WRITE(6,*)' TBLVAL BLOC3 CONTIENT DES VALEURS NEGATIVES'
    WRITE(6,*)' LE DATYP ET LE NBIT SPECIFIE SONT INADEQUATS'
    CALL REMPLI(TBLVAL,4,5,270)
    TBLVAL(1) = 0
    TBLVAL(2) = -1
    TBLVAL(3) = -2
    TBLVAL(4) = -3
    TBLVAL(5) = -4
    TBLVAL(6) = -5
    TBLVAL(7) = -6
    TBLVAL(8) = -7
    TBLVAL(9) = -8
    IER1 = MRBADD(BUFE,BLKNO,4,5,270,5,12,3,4,BIT0,2,LSTELEE,TBLVAL)
    IER = IER + IER1
    PRINT *,' *** INFO BLKNO, BIT0 = ',BLKNO,BIT0
    CALL TESTIT(IER)
    ! -------------------------------------------------------------
    WRITE(6,333) 30.2,'MRBXTR POUR BLOC3 BUFE'
    IER = MRBXTR(BUFE,3,CLISTEE,ETBLVAL)
    DO I = 1,4
        IF((CLISTEE(I).NE. LSTELEE(I)))THEN
            IER = IER - 1
        ENDIF
    END DO
    CALL TESTIT (IER)
    DO I = 1,5400
        IF((ETBLVAL(I).NE. TBLVAL(I)))THEN
            IER = IER - 1
        ENDIF
    END DO
    CALL TESTIT (IER)
    ! -------------------------------------------------------------
    WRITE(6,333) 30.3,'MRBPRM POUR BLOC 1 BUFE'
    IER = MRBPRM(BUFE,1,NELE,NVAL,NT,BFAM,BDESC,BTYP,NBIT,BIT0,DATYP)
    WRITE(6,*)' NELE   NVAL   NT  BFAM   BDESC  BTYP   NBIT    BIT0   DATYP'
    WRITE(6,777) NELE,NVAL,NT,BFAM,BDESC,BTYP,NBIT, BIT0,DATYP
    CALL TESTIT (IER)
    ! -------------------------------------------------------------
    WRITE(6,333) 30.4,'MRBPRM POUR BLOC 2 BUFE'
    IER = MRBPRM(BUFE,2,NELE,NVAL,NT,BFAM,BDESC,BTYP,NBIT,BIT0,DATYP)
    WRITE(6,*)' NELE   NVAL   NT  BFAM   BDESC  BTYP   NBIT    BIT0   DATYP'
    WRITE(6,777) NELE,NVAL,NT,BFAM,BDESC,BTYP,NBIT, BIT0,DATYP
    CALL TESTIT (IER)
    ! -------------------------------------------------------------
    WRITE(6,333) 30.5,'MRBPRM POUR BLOC 3 BUFE'
    IER = MRBPRM(BUFE,3,NELE,NVAL,NT,BFAM,BDESC,BTYP,NBIT,BIT0,DATYP)
    WRITE(6,*)' NELE   NVAL   NT  BFAM   BDESC  BTYP   NBIT    BIT0   DATYP'
    WRITE(6,777) NELE,NVAL,NT,BFAM,BDESC,BTYP,NBIT, BIT0,DATYP
    CALL TESTIT (IER)
    ! -------------------------------------------------------------
    WRITE(6,333) 30.5,'MRBDEL POUR BLOC 2 BUFE'
    IER = MRBDEL(BUFE,2)
    CALL TESTIT(IER)
    ! -------------------------------------------------------------
    WRITE(6,333) 31.0 ,' MRFNBR FICHIER 10 OUVERT'
    IER1 = MRFNBR(10)
    WRITE(6,*)' NOMBRE D''ENREGISTREMENTS=',IER1
    CALL TESTIT(IER1, expected_val = 303)
    ! -------------------------------------------------------------
    WRITE(6,333) 40.0,'MRFCLS FICHIER 10'
    IER = MRFCLS(10)
    CALL TESTIT(IER)
    ! -------------------------------------------------------------
    WRITE(6,333) 40.1 ,' MRFNBR FICHIER 10 FERME'
    IER = MRFNBR(10)
    WRITE(6,*)' NOMBRE D''ENREGISTREMENTS=',IER
    CALL TESTIT(IER, expected_min = IER1, expected_max = IER1)
    ! -------------------------------------------------------------
    WRITE(6,333) 41.0,'MRBSCT ELEMENTS DE L''USAGER'
    DO I = 1, 10
        CUSRTBL(1,I) = MRBCOV(USRTBL(I))
        CUSRTBL(2,I) = I
        CUSRTBL(3,I) = -I

        !     ON AJOUTE LE TABLEAU DE L'USAGER
    END DO
    IER = MRBSCT(CUSRTBL,10)
    CALL TESTIT(IER)
    !     VERIFIER SI CERTAINS ELEMENTS SONT REPETITIFS OU NON
    !     ELEMENT SANS CONVERSION : 20004 PAS REPETITIF
    !        "                      20003 (PAS?) REPETITIF
    !     ELEMENTS AVEC CONVERSION: 14192 REPETITIF
    !                               14193 REPETITIF
    !                               14194 REPETITIF
    !                               20192 PAS REPETITIF
    !                               20195 PAS REPETITIF

    ! -------------------------------------------------------------
    WRITE(6,333)42.0,' MRBRPT 20004 PAS REPETITIF'
    RELEM = MRBCOV(20004)
    IER = MRBRPT(RELEM)
    WRITE(6,*)' REPETE =', IER
    CALL TESTIT (IER)
    ! -------------------------------------------------------------
    WRITE(6,333)42.1,' MRBRPT 20003 PAS REPETITIF'
    RELEM = MRBCOV(20003)
    IER = MRBRPT(RELEM)
    WRITE(6,*)' REPETE =', IER
    CALL TESTIT (IER)
    ! -------------------------------------------------------------
    WRITE(6,333)42.2,' MRBRPT 14192 REPETITIF'
    RELEM = MRBCOV(14192)
    IER = MRBRPT(RELEM)
    WRITE(6,*)' REPETE =', IER, RELEM
    CALL TESTIT (IER, expected_val = 1)
    ! -------------------------------------------------------------
    WRITE(6,333)42.3,' MRBRPT 14193 REPETITIF'
    RELEM = MRBCOV(14193)
    IER = MRBRPT(RELEM)
    WRITE(6,*)' REPETE =', IER
    CALL TESTIT (IER, expected_val = 1)
    ! -------------------------------------------------------------
    WRITE(6,333)42.4,' MRBRPT 14194 REPETITIF'
    RELEM = MRBCOV(14194)
    IER = MRBRPT(RELEM)
    WRITE(6,*)' REPETE =', IER
    CALL TESTIT (IER, expected_val = 1)
    ! -------------------------------------------------------------
    WRITE(6,333)42.5,' MRBRPT 20192 PAS REPETITIF'
    RELEM = MRBCOV(20192)
    IER = MRBRPT(RELEM)
    WRITE(6,*)' REPETE =', IER
    CALL TESTIT (IER)
    ! -------------------------------------------------------------
    WRITE(6,333)42.6,' MRBRPT 20195 PAS REPETITIF'
    RELEM = MRBCOV(20195)
    IER = MRBRPT(RELEM)
    WRITE(6,*)' REPETE =', IER
    CALL TESTIT (IER)
    ! -------------------------------------------------------------
    WRITE(6,333)42.7,' MRBRPT 0  ELEMENT ILLEGAL'
    IER = MRBRPT(0)
    WRITE(6,*)' REPETE =', IER
    CALL TESTIT (IER, expected_val = 37)
    ! -------------------------------------------------------------
    WRITE(6,333)42.8,' MRBRPT 65700 ELEMENT ILLEGAL'
    IER = MRBRPT(65700)
    WRITE(6,*)' REPETE =', IER
    CALL TESTIT (IER, expected_val = 37)
    ! -------------------------------------------------------------
    WRITE(6,333) 43.0,' MRFOPC - MSGLVL MIS A TRIVIAL'
    IER = MRFOPC('MSGLVL','TRIVIAL')
    CALL TESTIT(IER)
    ! -------------------------------------------------------------
    WRITE(6,333) 43.1,' MRFOPN - FICHIER 10'
    IER = MRFOPN(10,'READ')
    CALL TESTIT(IER, expected_val = 303)
    ! -------------------------------------------------------------
    WRITE(6,333) 43.2, ' MRFCLS - FICHIER 10'
    IER = MRFCLS(10)
    CALL TESTIT(IER)
    ! -------------------------------------------------------------
    WRITE(6,333) 43.3,' MRFOPC - MSGLVL MIS A FATAL'
    IER = MRFOPC('MSGLVL','FATAL')
    CALL TESTIT(IER)
    ! -------------------------------------------------------------
    WRITE(6,333) 43.4,' MRFOPN - FICHIER 10'
    IER = MRFOPN(10,'READ')
    CALL TESTIT(IER, expected_val = 303)
    ! -------------------------------------------------------------
    WRITE(6,333) 43.5, ' MRFCLS - FICHIER 10'
    IER = MRFCLS(10)
    CALL TESTIT(IER)
    ! -------------------------------------------------------------
    WRITE(6,333)44.0, ' MRBINI BUFA - TEST AVEC ZERO BLOCKS'
    IER = MRFOPN(10,'APPEND')
    DO I= 2, 8000
        BUFA(I) = 0
    END DO
    IER1 = MRBINI(10,BUFA,1129,ishft(-1,-(32-(8))),'STATION12',2,1800,3600,0,0,975,42,920108,65535,1,SUP,ZERO,XAUX,ZERO)
    IER = MRBHDR(BUFA,TEMPS,FLGS,STNID,IDTYP,LATI,LONG,DX,DY,ELEV,DRCV,DATE,OARS,RUN,NBLK,SUP,NSUP,XAUX,NXAUX)
    WRITE(6,*)' TEMPS = ',TEMPS
    WRITE(6,*)' FLGS  = ',FLGS
    WRITE(6,400) STNID
    WRITE(6,*)' IDTYP = ',IDTYP
    WRITE(6,*)' LATI  = ',LATI
    WRITE(6,*)' LONG  = ',LONG
    WRITE(6,*)' DX    = ',DX
    WRITE(6,*)' DY    = ',DY
    WRITE(6,*)' ELEV  = ',ELEV
    WRITE(6,*)' DRCV  = ',DRCV
    WRITE(6,*)' DATE  = ',DATE
    WRITE(6,*)' OARS  = ',OARS
    WRITE(6,*)' RUN   = ',RUN
    WRITE(6,*)' NBLK  = ',NBLK
    IER = MRFPUT(10,0,BUFA)
    IER = MRFLOC(10,0,'S*A*I**12',-1,-1,-1,920108,-1,SUP,ZERO)
    WRITE(6,*)' HANDLE DE L''ENREGISTREMENT AVEC ZERO BLOCS:',IER
    CALL TESTIT(IER, expected_min = 1)
    DO I= 2, 8000
        BUFA(I) = 0
    END DO
    IER = MRFGET(IER,BUFA)
    IER = MRBHDR(BUFA,TEMPS,FLGS,STNID,IDTYP,LATI,LONG,DX,DY,ELEV,DRCV,DATE,OARS,RUN,NBLK,SUP,NSUP,XAUX,NXAUX)
    WRITE(6,*)' TEMPS = ',TEMPS
    WRITE(6,*)' FLGS  = ',FLGS
    WRITE(6,400) STNID
    WRITE(6,*)' IDTYP = ',IDTYP
    WRITE(6,*)' LATI  = ',LATI
    WRITE(6,*)' LONG  = ',LONG
    WRITE(6,*)' DX    = ',DX
    WRITE(6,*)' DY    = ',DY
    WRITE(6,*)' ELEV  = ',ELEV
    WRITE(6,*)' DRCV  = ',DRCV
    WRITE(6,*)' DATE  = ',DATE
    WRITE(6,*)' OARS  = ',OARS
    WRITE(6,*)' RUN   = ',RUN
    WRITE(6,*)' NBLK  = ',NBLK
    CALL TESTIT(IER)
    ! -------------------------------------------------------------
    WRITE(6,333) 45.0 ,'MRBTYP EN MODE -1'
    WRITE(6,*)' BKNAT =3, BKTYP = 92, BKSTP = 13'
    BKNAT = 3
    BKTYP = 92
    BKSTP = 13
    BTYP = MRBTYP(BKNAT, BKTYP, BKSTP, -1)
    WRITE(6,*) ' BTYP = ', BTYP
    CALL TESTIT(IER)
    ! -------------------------------------------------------------
    WRITE(6,333) 45.1,'MRBTYP EN MODE 1'
    IER = MRBTYP(BKNAT, BKTYP, BKSTP, BTYP)
    WRITE(6,*)' BKNAT = ',BKNAT,' BKTYP = ',BKTYP,' BKSTP = ', BKSTP
    IF( ((BKNAT.NE. 3).OR. (BKTYP.NE. 92).OR. (BKSTP.NE. 13)))THEN
        IER = -1
    ENDIF
    CALL TESTIT(IER)
    ! -------------------------------------------------------------
    WRITE(6,333) 46.0, 'MRBADD BUFA AVEC BTYP COMPOSITE'
    BKNAT = 3
    BKSTP = 13
    DO I = 1,20
        CALL REMPLI(TBLVAL,10,5,7)
        BKTYP = I
        IER1 = MRBADD(BUFA, BLKNO, 10, 5, 7, 5, 12, MRBTYP(BKNAT, BKTYP, BKSTP, -1), 32, BIT0, 2, LSTELEA, TBLVAL)
        IER = IER + IER1
        PRINT *,' *** INFO BLKNO, BIT0 = ',BLKNO,BIT0
    END DO
    CALL TESTIT(IER)
    ! -------------------------------------------------------------
    WRITE(6,333) 46.1,' MRBLOC  ET MRBLOCX DES MEMES ENREGISTREMENTS'
    IER2 = 0
    DO I = 1,20
        BKTYP = I
        BTYP = MRBTYP(BKNAT, BKTYP, BKSTP, -1)
        IER = MRBLOC(BUFA,5,12,BTYP,0)
        WRITE(6,*)' MRBLOC BLKNO = ',IER
        IER1 = MRBLOCX(BUFA,5,12,3,I,13,0)
        WRITE(6,*)' MRBLOCX BLKNO = ',IER1
        IF((IER.NE. IER1))THEN
            IER2 = IER2 -1
        ENDIF
    END DO
    CALL TESTIT(IER2)
    ! -------------------------------------------------------------
    WRITE(6,333) 46.2, ' MRBLOCX BKNAT=-1, BKTYP=10,BKSTP=13'
    IER1 = MRBLOCX(BUFA,5,12,-1,10,13,0)
    WRITE(6,*)' MRBLOCX BLKNO = ',IER1
    CALL TESTIT(IER1, expected_val = 10)
    ! -------------------------------------------------------------
    WRITE(6,333) 46.3, ' MRBLOCX BKNAT=3, BKTYP=-1,BKSTP=13'
    IER1 = MRBLOCX(BUFA,5,12,3,-1,13,0)
    WRITE(6,*)' MRBLOCX BLKNO = ',IER1
    CALL TESTIT(IER1, expected_val = 1)
    ! -------------------------------------------------------------
    WRITE(6,333) 46.4, ' MRBLOCX BKNAT=3, BKTYP=10,BKSTP=-1'
    IER1 = MRBLOCX(BUFA,5,12,3,10,-1,0)
    WRITE(6,*)' MRBLOCX BLKNO = ',IER1
    CALL TESTIT(IER1, expected_val = 10)
    ! -------------------------------------------------------------
    WRITE(6,333) 46.5, ' MRBLOCX BKNAT=-1, BKTYP=-1,BKSTP=-1'
    IER1 = MRBLOCX(BUFA,5,12,-1,-1,-1,0)
    WRITE(6,*)' MRBLOCX BLKNO = ',IER1
    CALL TESTIT(IER1, expected_val = 1)
    ! -------------------------------------------------------------
    WRITE(6,333) 46.6, ' MRBLOCX TOUT A -1 SAUF BKTYP = 10'
    IER1 = MRBLOCX(BUFA,-1,-1,-1,10,-1,0)
    WRITE(6,*)' MRBLOCX BLKNO = ',IER1
    CALL TESTIT(IER1, expected_val = 10)
    ! -------------------------------------------------------------
    WRITE(6,333) 46.7, ' MRBTYP MODE 0 TOUS A VALEUR MAX'
    BKNAT = 15
    BKTYP = 127
    BKSTP = 15
    BTYP = MRBTYP(BKNAT, BKTYP, BKSTP, -1)
    WRITE(6,*)' BTYP = ', BTYP
    CALL TESTIT (BTYP, expected_val = 32767)
    ! -------------------------------------------------------------
    WRITE(6,333) 46.8,' MRBTYP MODE 1 TOUS VALEUR MAX'
    IER = MRBTYP(BKNAT, BKTYP, BKSTP, BTYP)
    IF(((BKNAT.NE. 15).OR. (BKTYP.NE. 127).OR. (BKSTP.NE. 15)))THEN
        IER = -1
    ENDIF
    CALL TESTIT(IER)
    ! -------------------------------------------------------------
    WRITE(6,333) 46.9,' MRBTYP MODE -1 TOUS a zero'
    BKNAT = 0
    BKTYP = 0
    BKSTP = 0
    IER = MRBTYP(BKNAT, BKTYP, BKSTP, -1)
    IF((IER.NE. 0))THEN
        IER = -1
    ENDIF
    CALL TESTIT(IER)

    !     section pour tester les datyp 6,7,8 et 9

    !reel
    IER = MRFOPC('MSGLVL','TRIVIAL')


    ! -------------------------------------------------------------
    WRITE(6,333) 50.0,' MRBADD datyp = 6'
    IER1 = MRBINI(10,BUFA,1129,ishft(-1,-(32-(8))),'DATYP6789',2,1800,3600,0,0,975,42,920108,65535,1,SUP,ZERO,XAUX,ZERO)

    do i = 1,10
        rtablo(i) = float(i)
    enddo

    do i = 1,10
        rliste8(i) = mrbcov(lstelea(i))
    enddo

    BKNAT = 3
    BKSTP = 13
    IER1 = MRBADD(BUFA,BLKNO,10,1,1,1,0,MRBTYP(BKNAT, I, BKSTP, -1),32,BIT0,6,rliste8,rtablo)
    if(IER1 .eq. -16) goto 6789

    PRINT *,' *** INFO BLKNO, BIT0 = ',BLKNO,BIT0
    IER = MRBXTR(BUFA,blkno,rliste8c,rtabloc)

    write(6,*)' rtablo   rtabloc '
    ier = 0
    do i = 1, 10
        if(rliste8(i) .ne. rliste8c(i)) then
            ier = ier - 1
        endif
        write(6,*) rtablo(i), rtabloc(i)
        if(rtabloc(i) .ne. rtablo(i)) then
            ier = ier - 1
        endif
    enddo

    call testit(ier)


    !real*8
    ! -------------------------------------------------------------
    WRITE(6,333) 50.1,' MRBADD datyp = 7'
    do i = 1,10
        rtablo8(i) = float(i)
    enddo

    do i = 1,20,2
        rliste8(i) = mrbcov(lstelea(i/2 + 1))
    enddo

    do i = 4,20,2
        rliste8(i) = mrbcov(055204)
    enddo

    rliste8(2) = 0


    IER1 = MRBADD(BUFA,BLKNO,20,1,1,2,0,MRBTYP(BKNAT, I, BKSTP, -1),32,     BIT0,7,rliste8,rtablo8)

    PRINT *,' *** INFO BLKNO, BIT0 = ',BLKNO,BIT0
    IER = MRBXTR(BUFA,blkno,rliste8c,rtablo8c)


    ier = 0
    do i = 1, 20,2
        if(rliste8(i) .ne. rliste8c(i)) then
            ier = ier - 1
        endif
    enddo

    ier1 = mrbcov(055204)
    do i = 2, 20,2
        if( rliste8c(i) .ne. ier1) then
            write(6,*)' RLISTE8C(i) = ',rliste8c(i)
            ier = ier - 1
        endif
    enddo

    write(6,*)' rtablo8   rtablo8c '
    do i = 1, 10
        write(6,*) rtablo8(i), rtablo8c(i)
        if(rtablo8c(i) .ne. rtablo8(i)) then
            ier = ier - 1
        endif
    enddo

    call testit(ier)


    !complex
    ! -------------------------------------------------------------
    WRITE(6,333) 50.2,' MRBADD datyp = 8'

    do i = 1,10
        ctablo(i) = cmplx(float(i),float(i))
    enddo

    do i = 1,20,2
        cliste(i) = mrbcov(lstelea(i/2 + 1))
    enddo

    do i = 4,20,2
        cliste(i) = mrbcov(055205)
    enddo

    cliste(2)  = 0

    IER1 = MRBADD(BUFA,BLKNO,20,1,1,3,0,MRBTYP(BKNAT, I, BKSTP, -1),32,     BIT0,8,cliste,ctablo)

    PRINT *,' *** INFO BLKNO, BIT0 = ',BLKNO,BIT0
    IER = MRBXTR(BUFA,blkno,clistec,ctabloc)



    ier = 0
    do i = 1, 20,2
        if(cliste(i) .ne. clistec(i)) then
            ier = ier - 1
        endif
    enddo

    ier1 = mrbcov(055205)
    do i = 2, 20,2
        if( clistec(i) .ne. ier1) then
            write(6,*)' CLISTE8C(i) = ',cliste8c(i)
            ier = ier - 1
        endif
    enddo

    write(6,*)' ctablo  ctabloc '
    do i = 1, 10
        write(6,*) ctablo(i), ctabloc(i)
        if(ctabloc(i) .ne. ctablo(i)) then
            ier = ier - 1
        endif
    enddo

    call testit(ier)


    !complex*16
    ! -------------------------------------------------------------
    WRITE(6,333) 50.2,' MRBADD datyp = 9'

    do i = 1,10
        ctablo8(i) = cmplx(float(i),float(i), kind = real64)
    enddo

    do i = 1,40,4
        cliste8(i) = mrbcov(lstelea(i/4 + 1))
    enddo

    do i = 2,40,4
        cliste8(i) = mrbcov(055204)
    enddo


    do i = 3,40,4
        cliste8(i) = mrbcov(055206)
    enddo

    do i = 4,40,4
        cliste8(i) = mrbcov(055207)
    enddo


    IER1 = MRBADD(BUFA,BLKNO,40,1,1,4,0,MRBTYP(BKNAT, I, BKSTP, -1),32,     BIT0,9,cliste8,ctablo8)

    PRINT *,' *** INFO BLKNO, BIT0 = ',BLKNO,BIT0
    IER = MRBXTR(BUFA,blkno,cliste8c,ctablo8c)

    ier = 0
    do i = 1, 40,4
        if(cliste8(i) .ne. cliste8c(i)) then
            ier = ier - 1
        endif
    enddo

    ier1 = mrbcov(055204)
    do i = 2, 40,4
        if( cliste8c(i) .ne. ier1) then
            write(6,*)' CLISTE8C(i) = ',cliste8c(i)
            ier = ier - 1
        endif
    enddo

    ier1 = mrbcov(055206)
    do i = 3, 40,4
        if( cliste8c(i) .ne. ier1) then
            write(6,*)' CLISTE8C(i) = ',cliste8c(i)
            ier = ier - 1
        endif
    enddo

    ier1 = mrbcov(055207)
    do i = 4, 40,4
        if( cliste8c(i) .ne. ier1) then
            write(6,*)' CLISTE8C(i) = ',cliste8c(i)
            ier = ier - 1
        endif
    enddo

    write(6,*)' ctablo8  ctablo8c '
    do i = 1, 10
        write(6,*) ctablo8(i), ctablo8c(i)
        if(ctablo8c(i) .ne. ctablo8(i)) then
            ier = ier - 1
        endif
    enddo

    call testit(ier)

    WRITE(6,333) 50.3,' MRFPUT MRFLOC MRFGET datyp = 6789'
    IER = MRFPUT(10,0,BUFA)
    IER1 = MRFLOC(10,0,'DATYP6789',-1,-1,-1,-1,-1,SUP,ZERO)
    if(ier1 .lt. 0) then
        ier = ier -1
    else
        IER = MRFGET(IER1,BUFA)
    endif
    CALL TESTIT(IER)

    WRITE(6,333) 50.4,' MRBPRM des quatres blocs, datyp = 6789'

    WRITE(6,*)' NELE   NVAL   NT  BFAM   BDESC  BTYP   NBIT    BIT0   DATYP'
    IER = MRBPRM(BUFA,1,NELE,NVAL,NT,BFAM,BDESC,BTYP,NBIT,BIT0,DATYP)
    WRITE(6,777) NELE,NVAL,NT,BFAM,BDESC,BTYP,NBIT, BIT0,DATYP
    call testit(ier)

    IER = MRBPRM(BUFA,2,NELE,NVAL,NT,BFAM,BDESC,BTYP,NBIT,BIT0,DATYP)
    WRITE(6,777) NELE,NVAL,NT,BFAM,BDESC,BTYP,NBIT, BIT0,DATYP
    call testit(ier)

    IER = MRBPRM(BUFA,3,NELE,NVAL,NT,BFAM,BDESC,BTYP,NBIT,BIT0,DATYP)
    WRITE(6,777) NELE,NVAL,NT,BFAM,BDESC,BTYP,NBIT, BIT0,DATYP
    call testit(ier)

    IER = MRBPRM(BUFA,4,NELE,NVAL,NT,BFAM,BDESC,BTYP,NBIT,BIT0,DATYP)
    WRITE(6,777) NELE,NVAL,NT,BFAM,BDESC,BTYP,NBIT, BIT0,DATYP
    call testit(ier)


    ! -------------------------------------------------------------
    WRITE(6,333) 50.5,' MRBXTR datyp = 6'

    do i = 1,10
        rtablo(i) = float(i)
    enddo

    do i = 1,10
        rliste8(i) = mrbcov(lstelea(i))
    enddo

    IER = MRBXTR(BUFA,1,rliste8c,rtabloc)

    write(6,*)' rtablo   rtabloc '
    ier = 0
    do i = 1, 10
        if(rliste8(i) .ne. rliste8c(i)) then
            ier = ier - 1
        endif
        write(6,*) rtablo(i), rtabloc(i)
        if(rtabloc(i) .ne. rtablo(i)) then
            ier = ier - 1
        endif
    enddo

    call testit(ier)


    !real*8
    ! -------------------------------------------------------------
    WRITE(6,333) 50.6,' MRBXTR datyp = 7'
    do i = 1,10
        rtablo8(i) = float(i)
    enddo

    do i = 1,20,2
        rliste8(i) = mrbcov(lstelea(i/2 + 1))
    enddo

    IER = MRBXTR(BUFA,2,rliste8c,rtablo8c)

    ier = 0
    do i = 1, 20,2
        if(rliste8(i) .ne. rliste8c(i)) then
            ier = ier - 1
        endif
    enddo

    ier1 = mrbcov(055204)
    do i = 2, 20,2
        if( rliste8c(i) .ne. ier1) then
            write(6,*)' RLISTE8C(i) = ',rliste8c(i)
            ier = ier - 1
        endif
    enddo

    write(6,*)' rtablo8   rtablo8c '
    do i = 1, 10
        write(6,*) rtablo8(i), rtablo8c(i)
        if(rtablo8c(i) .ne. rtablo8(i)) then
            ier = ier - 1
        endif
    enddo

    call testit(ier)


    !complex
    ! -------------------------------------------------------------
    WRITE(6,333) 50.7,' MRBXTR datyp = 8'

    do i = 1,10
        ctablo(i) = cmplx(float(i),float(i))
    enddo

    do i = 1,20,2
        cliste(i) = mrbcov(lstelea(i/2 + 1))
    enddo

    IER = MRBXTR(BUFA,3,clistec,ctabloc)

    ier = 0
    do i = 1, 20,2
        if(cliste(i) .ne. clistec(i)) then
            ier = ier - 1
        endif
    enddo

    ier1 = mrbcov(055205)
    do i = 2, 20,2
        if( clistec(i) .ne. ier1) then
            write(6,*)' CLISTE8C(i) = ',cliste8c(i)
            ier = ier - 1
        endif
    enddo

    write(6,*)' ctablo  ctabloc '
    do i = 1, 10
        write(6,*) ctablo(i), ctabloc(i)
        if(ctabloc(i) .ne. ctablo(i)) then
            ier = ier - 1
        endif
    enddo

    call testit(ier)


    !complex*16
    ! -------------------------------------------------------------
    WRITE(6,333) 50.8,' MRBXTR datyp = 9'

    do i = 1,10
        ctablo8(i) = cmplx(float(i),float(i), kind = real64)
    enddo

    do i = 1,40,4
        cliste8(i) = mrbcov(lstelea(i/4 + 1))
    enddo

    IER = MRBXTR(BUFA,4,cliste8c,ctablo8c)

    ier = 0
    do i = 1, 40,4
        if(cliste8(i) .ne. cliste8c(i)) then
            ier = ier - 1
        endif
    enddo

    ier1 = mrbcov(055204)
    do i = 2, 40,4
        if( cliste8c(i) .ne. ier1) then
            write(6,*)' CLISTE8C(i) = ',cliste8c(i)
            ier = ier - 1
        endif
    enddo

    ier1 = mrbcov(055206)
    do i = 3, 40,4
        if( cliste8c(i) .ne. ier1) then
            write(6,*)' CLISTE8C(i) = ',cliste8c(i)
            ier = ier - 1
        endif
    enddo

    ier1 = mrbcov(055207)
    do i = 4, 40,4
        if( cliste8c(i) .ne. ier1) then
            write(6,*)' CLISTE8C(i) = ',cliste8c(i)
            ier = ier - 1
        endif
    enddo

    write(6,*)' ctablo8  ctablo8c '
    do i = 1, 10
        write(6,*) ctablo8(i), ctablo8c(i)
        if(ctablo8c(i) .ne. ctablo8(i)) then
            ier = ier - 1
        endif
    enddo

    call testit(ier)


    ! -------------------------------------------------------------
    WRITE(6,333) 51.0,' MRBREP datyp = 6'

    do i = 1,10
        rtablo(i) = float(i) * 2.0
    enddo

    do i = 1,10
        rliste8(i) = mrbcov(lstelea(i))
    enddo
    IER = MRBREP(BUFA,1,rtablo)

    IER = MRBXTR(BUFA,1,rliste8c,rtabloc)

    write(6,*)' rtablo   rtabloc '
    ier = 0
    do i = 1, 10
        if(rliste8(i) .ne. rliste8c(i)) then
            ier = ier - 1
        endif
        write(6,*) rtablo(i), rtabloc(i)
        if(rtabloc(i) .ne. rtablo(i)) then
            ier = ier - 1
        endif
    enddo

    call testit(ier)


    !real*8
    ! -------------------------------------------------------------
    WRITE(6,333) 51.1,' MRBREP datyp = 7'
    do i = 1,10
        rtablo8(i) = float(i) * 2.0
    enddo

    do i = 1,20,2
        rliste8(i) = mrbcov(lstelea(i/2 + 1))
    enddo

    IER = MRBREP(BUFA,2,rtablo8)
    IER = MRBXTR(BUFA,2,rliste8c,rtablo8c)

    ier = 0
    do i = 1, 20,2
        if(rliste8(i) .ne. rliste8c(i)) then
            ier = ier - 1
        endif
    enddo

    ier1 = mrbcov(055204)
    do i = 2, 20,2
        if( rliste8c(i) .ne. ier1) then
            write(6,*)' RLISTE8C(i) = ',rliste8c(i)
            ier = ier - 1
        endif
    enddo

    write(6,*)' rtablo8   rtablo8c '
    do i = 1, 10
        write(6,*) rtablo8(i), rtablo8c(i)
        if(rtablo8c(i) .ne. rtablo8(i)) then
            ier = ier - 1
        endif
    enddo

    call testit(ier)


    !complex
    ! -------------------------------------------------------------
    WRITE(6,333) 51.2,' MRBREP datyp = 8'

    do i = 1,10
        ctablo(i) = cmplx(float(i) *2.0 ,float(i) * 3.0)
    enddo

    do i = 1,20,2
        cliste(i) = mrbcov(lstelea(i/2 + 1))
    enddo

    IER = MRBREP(BUFA,3,ctablo)
    IER = MRBXTR(BUFA,3,clistec,ctabloc)

    ier = 0
    do i = 1, 20,2
        if(cliste(i) .ne. clistec(i)) then
            ier = ier - 1
        endif
    enddo

    ier1 = mrbcov(055205)
    do i = 2, 20,2
        if( clistec(i) .ne. ier1) then
            write(6,*)' CLISTE8C(i) = ',cliste8c(i)
            ier = ier - 1
        endif
    enddo

    write(6,*)' ctablo  ctabloc '
    do i = 1, 10
        write(6,*) ctablo(i), ctabloc(i)
        if(ctabloc(i) .ne. ctablo(i)) then
            ier = ier - 1
        endif
    enddo

    call testit(ier)


    !complex*16
    ! -------------------------------------------------------------
    WRITE(6,333) 51.3,' MRBREP datyp = 9'

    do i = 1,10
        ctablo8(i) = cmplx(float(i) * 2.0,float(i) *3.0, kind = real64)
    enddo

    do i = 1,40,4
        cliste8(i) = mrbcov(lstelea(i/4 + 1))
    enddo

    IER = MRBREP(BUFA,4,ctablo8)
    IER = MRBXTR(BUFA,blkno,cliste8c,ctablo8c)

    ier = 0
    do i = 1, 40,4
        if(cliste8(i) .ne. cliste8c(i)) then
            ier = ier - 1
        endif
    enddo

    ier1 = mrbcov(055204)
    do i = 2, 40,4
        if( cliste8c(i) .ne. ier1) then
            write(6,*)' CLISTE8C(i) = ',cliste8c(i)
            ier = ier - 1
        endif
    enddo

    ier1 = mrbcov(055206)
    do i = 3, 40,4
        if( cliste8c(i) .ne. ier1) then
            write(6,*)' CLISTE8C(i) = ',cliste8c(i)
            ier = ier - 1
        endif
    enddo

    ier1 = mrbcov(055207)
    do i = 4, 40,4
        if( cliste8c(i) .ne. ier1) then
            write(6,*)' CLISTE8C(i) = ',cliste8c(i)
            ier = ier - 1
        endif
    enddo

    write(6,*)' ctablo8  ctablo8c '
    do i = 1, 10
        write(6,*) ctablo8(i), ctablo8c(i)
        if(ctablo8c(i) .ne. ctablo8(i)) then
            ier = ier - 1
        endif
    enddo

    call testit(ier)

6789 continue
    ! -------------------------------------------------------------
    WRITE(6,333) 59.0, 'MRFCLS FICHIER 10'
    IER = MRFCLS(10)
    CALL TESTIT(IER)
    ! -------------------------------------------------------------
    WRITE(6,333) 60.0, 'MRBTBL AVEC TOUS LES ELEMENTS DE LSTELEA'
    DO I = 1, 10
        TBLBURP(1,I) = MRBCOV(LSTELEA(I))
    END DO
    IER = MRBTBL(TBLBURP,4,10)
    WRITE(6,*)' ELEMENT ELEM-CODE ECHELLE REFERENCE TRANSFORMABLE'
    DO I = 1, 10
        WRITE(6,446) LSTELEA(I),TBLBURP(1,I),TBLBURP(2,I),TBLBURP(3   ,I),TBLBURP(4,I)
    END DO
    CALL TESTIT (IER)
    ! -------------------------------------------------------------
    WRITE(6,333) 60.1, 'MRBTBL AVEC LES ELEMENTS DE USAGER'
    DO I = 1, 10
        TBLBURP(1,I) = MRBCOV(USRTBL(I))
    END DO
    IER = MRBTBL(TBLBURP,4,10)
    WRITE(6,*)' ELEMENT ELEM-CODE ECHELLE REFERENCE TRANSFORMABLE'
    DO I = 1, 10
        WRITE(6,446) USRTBL(I),TBLBURP(1,I),TBLBURP(2,I),TBLBURP(3,   I),TBLBURP(4,I)
    END DO
    CALL TESTIT (IER)


    !       tester l'allocation de bits
    ! -------------------------------------------------------------
    WRITE(6,333) 70.0, 'qrbnbdt datyp=2,sans manquants'
    do i = 1, 7
        bufa(i) = i
    enddo

    nbit = 0
    datyp = 2
    ier = qrbnbdt(nbit,datyp,bufa,7)
    call testit(ier, message = 'qrbnbdt')
    write(6,*)' nbit=',nbit,' datyp=',datyp

    !      if(nbit .ne. 3) ier = ier - 1
    if(nbit .ne. 4) ier = ier - 1
    if(datyp .ne. 2) ier = ier - 1
    call testit(ier)

    WRITE(6,333) 70.1, 'qrbnbdt datyp=2, avec manquants'
    bufa(3) = -1

    nbit = 0
    datyp = 2
    ier = qrbnbdt(nbit,datyp,bufa,7)
    write(6,*)' nbit=',nbit,' datyp=',datyp

    if(nbit .ne. 4) ier = ier - 1
    if(datyp .ne. 2) ier = ier - 1
    call testit(ier)

    WRITE(6,333) 70.2, 'qrbnbdt datyp=4,sans manquants'
    do i = 1, 7
        bufa(i) = i
    enddo

    bufa(3) = -4

    nbit = 0
    datyp = 2
    ier = qrbnbdt(nbit,datyp,bufa,7)
    write(6,*)' nbit=',nbit,' datyp=',datyp

    !      if(nbit .ne. 4) ier = ier - 1
    if(nbit .ne. 5) ier = ier - 1
    if(datyp .ne. 4) ier = ier - 1
    call testit(ier)

    WRITE(6,333) 70.3, 'qrbnbdt datyp=4,avec manquants'
    bufa(4) = -1

    nbit = 0
    datyp = 2
    ier = qrbnbdt(nbit,datyp,bufa,7)
    write(6,*)' nbit=',nbit,' datyp=',datyp

    if(nbit .ne. 5) ier = ier - 1
    if(datyp .ne. 4) ier = ier - 1
    call testit(ier)

    WRITE(6,333) 70.4, 'qrbnbdt datyp=2,avec trop de bits'
    do i = 1, 7
        bufa(i) = i
    enddo
    nbit = 24
    datyp = 2
    ier = qrbnbdt(nbit,datyp,bufa,7)
    write(6,*)' nbit=',nbit,' datyp=',datyp

    if(nbit .ne. 24) ier = ier - 1
    if(datyp .ne. 2) ier = ier - 1
    call testit(ier)

    WRITE(6,*)'*************** FIN DES TESTS ***************'
    WRITE(6, '(A, I12)') 'num_errors = ', num_errors
333   FORMAT(' --- TEST ',F4.1,2X,A,' ---')
400   FORMAT(1X,' STNID = ',3X,A9)
446   FORMAT(I8,1X,I8,2X,I7,1X,I9,3X,I7)
777   FORMAT(1X,9(I5,2X))
778   FORMAT(1X,10(I5,2X))
800   FORMAT(' ',A9,1X,I4,1X,4(I6,1X),3X,Z4,6X,I8,2X,I3,3X,I8,1X,I10,I10)
850   FORMAT('0  STATION  TEMPS LATI   LONG   DX   DY   FLGS(HEX)    DATE','  IDTYP  LNGR'/)
1202  FORMAT(3X,I5,7X,I9)

END PROGRAM


SUBROUTINE REMPLI(TABLEAU, NI, NJ, NK)
    INTEGER NI, NJ, NK
    INTEGER TABLEAU(NI*NJ*NK)

    INTEGER IJK, SEED
    SAVE SEED
    DATA SEED /1/


    DO IJK = 1,NI*NJ*NK
        TABLEAU(IJK) = IJK * SEED
    END DO
    SEED = SEED + 1
END
