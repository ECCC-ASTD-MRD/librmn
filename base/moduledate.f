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
***S/R INCDATR - INCREASE IDATE2 BY NHOURS
*
      SUBROUTINE INCDATR (IDATE1,IDATE2,NHOURS)
      IMPLICIT NONE
*
* ENTRY INCDATI - SAME AS INCDATR BUT IDATE2 AND NHOURS ARE ROUNDED
* ENTRY DIFDATI - SAME AS DIFDATR BUT DATE-TIME STAMPS ARE ROUNDED
* ENTRY DIFDATR - COMPUTES THE DIFFERENCE IN HOURS BETWEEN
*                 IDATE1 AND IDATE2.
*
*AUTHOR   - G. ALEXANDER  -  APR 75
*
*REVISION 001   C. THIBEAULT  -  NOV 79  DOCUMENTATION
*REVISION 002   E. BEAUCHESNE  -  JUN 96  NEW STYLE DATE
*REVISION 003   M. Lepine, B. Dugas - Aout 2009
*               Dates etendues, + tenir compte ou non des annees
*                           bissextiles dans les calculs de dates
*REVISION 004   B. Dugas - Novembre 2010
*               Correction au mode non-bissextile pour les
*               calculs mettant en cause de grands intervals
*
*LANGUAGE - fortran
*
*OBJECT   - INCDATR COMPUTES IDATE1=IDATE2+NHOURS 
*         - DIFDATR COMPUTES NHOURS=IDATE1-IDATE2
*         - INCDATI COMPUTES IDATE1=IDATE2+NHOURS
*           (IDATE2 AND NHOURS ROUNDED TO NEAREST HOUR)
*         - DIFDATI COMPUTES NHOURS=IDATE1-IDATE2
*           (IDATE1 AND IDATE2 ROUNDED TO NEAREST HOUR)
*
*USAGE    - CALL INCDATR(IDATE1,IDATE2,NHOURS)
*         - CALL DIFDATR(IDATE1,IDATE2,NHOURS)
*         - CALL INCDATI(IDATE1,IDATE2,NHOURS)
*         - CALL DIFDATI(IDATE1,IDATE2,NHOURS)
*
*ARGUMENTS
*         - IDATE1 - CMC DATE-TIME STAMP (OLD OR NEW STYLE)
*         - IDATE2 - CMC DATE-TIME STAMP (OLD OR NEW STYLE)
*         - NHOURS - NUMBER OF HOURS(REAL*8)
*
*NOTES    - IT IS RECOMMENDED TO ALWAYS USE NEWDATE TO MANIPULATE 
*           DATES
*         - IF INCDATR OR INCDATI RECEIVE BAD ARGUMENTS, THEY SEND
*           BACK IDATE1=101010101 (1910/10/10 10Z RUN 1)
*         - IF DIFDATR OR DIFDATI RECEIVE BAD ARGUMENTS, THEY SEND
*           BACK NHOURS=2**30
*         - THERE ARE THREE STYLES OF DATES (ALL USE INTEGERS):
*            -OLD: AN INTEGER(.LT.123 200 000) OF THE FOLLOWING 
*             FORM: MMDDYYZZR
*               MM = MONTH OF THE YEAR (1-12)
*               DD = DAY OF THE MONTH (1-31)
*               YY = YEAR(00-99)=>OLD STYLE ONLY GOOD BEFORE 2000/1/1 
*               ZZ = HOUR(00-23)
*               R  = RUN (0-9) KEPT FOR BACKWARD COMPATIBILITY
*            -NEW: AN INTEGER(.GE.123 200 000) THAT CONTAINS THE 
*             TRUE DATE(NUMBER OF 5 SECONDS INTERVALS SINCE 1980/1/1 
*             00H00), COMPUTED LIKE THIS:
*               FALSE_DATE=NEW_DATE_TIME_STAMP-123 200 000
*               TRUE_DATE=(FALSE_DATE/10)*8+MOD(FALSE_DATE,10)
*            -EXTENDED: AN UNSIGNED INTEGER(.GE.3 000 000 000) THAT
*             CONTAINS THE EXTENDED TRUE DATE (NUMBER OF HOURS SINCE
*             0000/1/1 00H), COMPUTED LIKE THIS:
*               EXT_FALSE_DATE=EXT_DATE_TIME_STAMP-3 000 000 000
*               EXT_TRUE_DATE=(EXT_FALSE_DATE/10)*8+MOD(EXT_FALSE_DATE,10)
*             AS THIS EXTENDED DATE IS STORED IN A SIGNED INTEGER,
*             THE STORED VALUE WILL BE A LARGE NEGATIVE ONE.
*         - td2235 = truedate of dec 31, 2235, 23h59
*         - td1900 = -504904320 = truedate of jan 1, 1900
*--------------------------------------------------------------------
*
      integer idate1,idate2
      real*8 nhours
      logical  adding,rounding
      external newdate, LeapYear_Adjust
      external Get_LeapYear_Status
      integer newdate,result,LeapYear_Adjust

      logical :: no_leap_years,goextend

      integer(8) addit
      integer tdate1,tdate2,runnum,ndays,pdate1,pdate2
      integer td1900, td2235
      data td1900 /-504904320/, td2235 /1615714548/

      rounding=.false.
      goto 4

      entry incdati(idate1,idate2,nhours)
      rounding=.true.

 4    adding=.true.
      goto 1

*
*  difdat computes nhours = idate1 - idate2
*
      entry difdati(idate1,idate2,nhours)
      rounding=.true.
      goto 3

      entry difdatr(idate1,idate2,nhours)
      rounding=.false.

 3    adding=.false.
!      print *,'Debug+ difdat ',idate1,idate2
      if (idate2 .lt. -1 .or. idate1 .lt. -1) then
        if (idate1 .gt. -1) then
          result=newdate(idate1,pdate1,pdate2,-3)
          if(result.ne.0) then 
             print *,'label 1,idate1:',idate1
             goto 2
          endif          
          result=newdate(tdate1,pdate1,pdate2,+7)
          if(result.ne.0) then 
             print *,'label 2,pdate1,pdate2:',pdate1,pdate2
             goto 2
          endif          
        else
          result=newdate(tdate1,idate1,runnum,6)
        endif
      else
        result=newdate(tdate1,idate1,runnum,1)
      endif
      if(result.ne.0) then 
         print *,'label 3,idate1:',idate1
         goto 2
      endif

 1    continue
      call Get_LeapYear_Status( no_leap_years )
      if (idate2 .lt. -1 .or.
     #   (idate1 .lt. -1 .and. .not.adding)) then
        if (idate2 .gt.-1) then
           result=newdate(idate2,pdate1,pdate2,-3)
           if(result.ne.0) then 
              print *,'label 4,idate2:',idate2
              goto 2
           endif          
           result=newdate(tdate2,pdate1,pdate2,+7)
           if(result.ne.0) then 
              print *,'label 5,pdate1,pdate2:',pdate1,pdate2
              goto 2
           endif          
        else
           result=newdate(tdate2,idate2,runnum,6)
        endif
        if(result.ne.0) then
           print *,'label 6,idate2:',idate2
           goto 2
        endif
        if (adding) then
          tdate1=tdate2+nint(nhours)
          if (no_leap_years) then
            ndays = LeapYear_Adjust(tdate1,tdate2,'E',adding)
            tdate1 = tdate1 + (ndays*24)
          endif
          result=newdate(tdate1,idate1,runnum,-6)
          if (result.ne.0)  then 
             print *,'after if adding,if rounding',tdate1
             goto 2
          endif
        else
          nhours=(tdate1-tdate2)
          if (no_leap_years) then
            ndays = LeapYear_Adjust(tdate1,tdate2,'E',adding)
            nhours = nhours - (ndays*24)
          endif
        endif
      else
        result=newdate(tdate2,idate2,runnum,1)
        if(result.ne.0) then
           print *,'label 1,idate2:',idate2
           goto 2
        endif
        if (adding) then
           goextend=.false.
           rounding=rounding.or.(tdate2.lt.0)
           if (rounding) then
              tdate2=(tdate2+sign(360,tdate2))/720*720
              addit = 720*nint(nhours,8)
           else
              addit = nint(720*nhours,8)
           endif
           if ((td1900-tdate2)*1_8 <= addit .and.  ! tdate2 + addit >= td1900  and 
     #         (td2235-tdate2)*1_8 >= addit) then  ! tdate2 + addit <= td2235, where
              tdate1=tdate2+addit                  ! addit can be a very large
              if (no_leap_years) then              ! integer*8 number 
                 ndays = LeapYear_Adjust(tdate1,tdate2,'B',adding)
                 tdate1 = tdate1 + (ndays*24*720)
              endif
              if ((tdate1 > td2235)
     #       .or. (tdate1 < td1900)) goextend = .true.
           else
              goextend = .true.
           endif
           if (goextend) then  ! exiting regular date range for extended range
             result=newdate(idate2,pdate1,pdate2,-3)
             if(result.ne.0) then 
               print *,'label 7,idate2:',idate2
               goto 2
             endif          
             result=newdate(tdate2,pdate1,pdate2,+7)
             if(result.ne.0) then 
                print *,'label 8,pdate1,pdate2:',pdate1,pdate2
                goto 2
             endif          
             tdate1=tdate2+nint(nhours)
             if (no_leap_years) then
               ndays = LeapYear_Adjust(tdate1,tdate2,'E',adding)
               tdate1 = tdate1 + (ndays*24)
             endif
             result=newdate(tdate1,idate1,runnum,-6)
           else 
             result=newdate(tdate1,idate1,runnum,-1)
           endif
           if (result.ne.0)  then 
              print *,'after if adding,if rounding',tdate1
              goto 2
           endif
        else
           if (rounding) then
              tdate1=(tdate1+sign(360,tdate1))/720*720
              tdate2=(tdate2+sign(360,tdate2))/720*720
              nhours=nint((tdate1-tdate2)/720.0)
           else
              nhours=(tdate1-tdate2)
              nhours=nhours/720.0
           endif
           if (no_leap_years) then
             ndays = LeapYear_Adjust(tdate1,tdate2,'B',adding)
             nhours = nhours - (ndays*24)
           endif
        endif
      endif
      return

 2    if (adding) then
         idate1=101010101
      else
         nhours=2.0**30
      endif
      return
      end
***FUNCTION IDATMG2 - CONSTRUCTS A CANADIAN METEOROLOGICAL CENTRE DATE-
*                    TIME STAMP USING THE OPERATIONAL CMC DATE-TIME
*                    GROUP.
*
      FUNCTION IDATMG2(IDATE)
      IMPLICIT NONE
*
*AUTHOR   - M. VALIN  -  MAR 79
*
*REVISION 001  C. THIBEAULT - NOV 79  DOCUMENTATION
*         002  P. CADIEUX   - FEV 83  VERSION CORRIGEE, PLUS EFFICACE
*         003  E. BEAUCHESNE - JUN 96  NEW DATE-TIME STAMP
*
*LANGUAGE - fortran
*
*OBJECT(IDATMG2)
*         - CONSTRUCTS A CMC DATE-TIME STAMP USING THE OPERATIONAL 
*           CMC DATE-TIME GROUP (WORDS 1-6) AND RETURNING THE STAMP 
*           IN WORD 14 AS WELL AS IN THE FUNCTION VALUE.
*
*USAGE    - IDAT = IDATMG2(IDATE)
*
*ARGUMENTS
*      IN - IDATE(1 TO 6) - ARRAY OF 14 WORDS WHICH HAS IN WORDS 1-6 
*                           THE INFORMATION NEEDED TO RECONSTRUCT THE 
*                           STAMP WHICH IS THEN PUT IN WORD 14 AS WELL 
*                           AS IN THE FUNCTION VALUE(SEE NOTES)
*     OUT - IDATE(14)     - CMC DATE-TIME STAMP (NEW, OLD or EXTENDED)
*NOTES    
*         - RETURNS IDATE(14)=101010101 IF INPUTS ARE INVALID
*         - IDATE(1)=DAY OF THE WEEK(1-7,SUNDAY=1) (OPTIONAL)
*         - IDATE(2)=MONTH (1-12)
*         - IDATE(3)=DAY   (1-31)                
*         - IDATE(4)=YEAR  (0-99,100-10000)    Note: can not work for extended dates between 0-99
*         - IDATE(5)=ZULU  (0-23)               
*         - IDATE(6)=HUNDREDTHS OF SECOND SINCE LAST HOUR (0-359 999)
*
*---------------------------------------------------------------------
*
      integer idate(14),idatmg2,newdate
      external newdate
      integer dtpr,tmpr,year,result
      year=idate(4)
      if ((year.ge.0) .and. (year.le.99)) then
         year=year+1900
      endif
      dtpr=year*10000+idate(2)*100+idate(3)
      tmpr=idate(5)*1000000+(idate(6)/6000)*10000+
     #     mod(idate(6)/100,60)*100
      result=newdate(idate(14),dtpr,tmpr,3)
      if(result.ne.0) idate(14)=101010101
      
      idatmg2 = idate(14)

      return
      end      
***S/R DATMGP2 - CREATES A DATE TIME GROUP.
*
      SUBROUTINE DATMGP2 (IDATE)
*
*AUTHOR   - D. SHANTZ
*
*REVISION 001   C. THIBEAULT  -  NOV 79   DOCUMENTATION
*         002   E. BEAUCHESNE  -  JUN 96  NEW DATE-TIME STAMP
*         003   M. Lepine - Avril 98 - Retourner l'annee a 4 chiffres
*
*LANGUAGE - fortran
*
*OBJECT(DATMGP2)
*         - CREATES A CANADIAN METEOROLOGICAL CENTRE DATE TIME GROUP
*           IN THE OPERATIONAL CMC FORMAT USING THE CMC DATE TIME STAMP
*
*USAGE    - CALL DATMGP2(IDATE)
*
*ALGORITHM
*         - CALLS NEWDATE TO CONVERT A DATE-TIME STAMP TO A PRINTABLE 
*           STAMP
*         - EXTRACTS INFORMATION OF IT
*         - IT THEN USES A TABLE LOOKUP TO CONSTRUCT THE MONTH AND
*           DAY OF THE WEEK CHARACTER STRINGS.
*         - ENCODE AND DECODE ARE THEN USED TO FORMAT THE CHARACTER
*           PART OF THE DATE TIME GROUP.
*
*ARGUMENTS
*  IN/OUT - IDATE - 14 WORDS INTEGER ARRAY. ON INPUT, WORD 14 IS SET 
*           TO THE DATE TIME STAMP. ON OUTPUT ALL 14 WORDS OF IDATE 
*           ARE SET TO THE DATE TIME GROUP WHICH CORRESPONDS TO THAT
*           DATE TIME STAMP.
*
*NOTES  
*         - IF IDATE(14) IS INVALID, THE OUTPUTS WILL CORRESPOND 
*           TO 1910/10/10 10Z
*         - IDATE(1)=DAY OF THE WEEK (1-7,SUNDAY=1)
*         - IDATE(2)=MONTH (1-12)
*         - IDATE(3)=DAY   (1-31)                
*         - IDATE(4)=YEAR  (0-10000)
*         - IDATE(5)=ZULU  (0-23)               
*         - IDATE(6)=100*NUMBER_OF_SECOND_SINCE_LAST_HOUR (0,359 999)
*         - IDATE(7-13)=DATE-TIME GROUP IN CHARACTER FORMAT (7A4)
*         - IDATE(14)=DATE-TIME STAMP(OLD, NEW OR EXTENDED)
*
*--------------------------------------------------------------------
*
      integer idate(14),dtpr,tmpr,result
      real xmonth(12),xday(7)
      character * 128 wrk
      data xmonth / 4HJAN ,4HFEB ,4HMAR ,4HAPR ,4HMAY ,4HJUN ,
     1              4HJUL ,4HAUG ,4HSEP ,4HOCT ,4HNOV ,4HDEC /
      data xday   / 4HSUN ,4HMON ,4HTUE ,4HWED ,4HTHU ,
     1              4HFRI ,4HSAT /
*
*---------------------------------------------------------------------
*
      jd(i,j,k) =k-32075+1461*(i+4800+(j-14)/12)/4
     #     +367*(j-2-(j-14)/12*12)/12 
     #     -3*((i+4900+(j-14)/12)/100)/4
*
      idt = idate(14)
*
      result=newdate(idt,dtpr,tmpr,-3)
      if (result.ne.0) then
         idt=101010101
         dtpr=19101010
         tmpr=10000000
      endif
      
      idate(2) = mod(dtpr/100,100)
      idate(3) = mod(dtpr,100)
      idate(4) = mod(dtpr/10000,10000)
      idate(5) = mod(tmpr/1000000,100)
      idate(6) = mod(tmpr/10000,100)*6000+mod(tmpr/100,100)*100
     #     +mod(tmpr,100)
      
      mon = idate(2)
      amonth = xmonth(mon)
      idate(1) = jd(idate(4),idate(2),idate(3))
      idate(1) = 1 + mod(idate(1)+1,7)
      iday = idate(1)
      aday = xday(iday)
      
      write(wrk,601) aday,amonth,(idate(i),i=3,5),idate(6)/6000,
     #     mod(idate(6)/100,60),mod(idate(6),100)
      read (wrk,501) (idate(i),i=7,13)
  501 format (7a4)
  601 format (1x,a3,1x,a3,i3.2,1x,i4.2,i3.2,'Z',i2.2,':',i2.2,'.',
     #     i2.2)
*
*---------------------------------------------------------------------
*
      return
      end

      subroutine Ignore_LeapYear()
      implicit none

      character(len=512) :: value 
      logical :: no_leap_year_status

      call NewDate_Options( 'year=365_day','set' )
      return

      entry Accept_LeapYear()

      call NewDate_Options( 'year=gregorian','set' )
      return

      entry Get_LeapYear_Status( no_leap_year_status )

      value='year' ; call NewDate_Options( value,'get' )

      if (value == '365_day' .or. value == '360_day') then
         no_leap_year_status = .true.
      else
         no_leap_year_status = .false.
      endif

      return

      end

      subroutine NewDate_Options( value,command )

      implicit none
      character*(*) value,command

      integer   ii
      logical,  save :: called_newdate_options=.false.
      logical,  save :: no_newdate_env_options=.true.
      logical,  save :: no_leap_years=.false.
      logical,  save :: cccvx_day=.false.
      character(512) :: evalue,string

      if (.not.called_newdate_options) then ! check environment once
         call getenvc( 'NEWDATE_OPTIONS',evalue )
         called_newdate_options = .true.
         if (evalue /= ' ') then ! variable was set
            call up2low( evalue,evalue )
            ii = index( evalue,'year=' )
            if (ii > 0) then ! found known option. check its value
               if (evalue(ii+5:ii+12)      == '365_day'  .or.
     .             evalue(ii+5:ii+12)      == '360_day') then
                  no_newdate_env_options   =  .false.
                  no_leap_years            =  .true.
                  if (evalue(ii+5:ii+12)   == '360_day')
     .               cccvx_day             =  .true. 
               else if (evalue(ii+5:ii+13) == 'gregorian') then
                  no_newdate_env_options   =  .false.
                  no_leap_years            =  .false.
                  cccvx_day                =  .false. 
               endif
            endif
         endif
      endif

      evalue = value   ; call up2low( evalue,evalue )
      string = command ; call up2low( string,string )

      if (string == 'get') then ! check for value of defined options
         if (evalue == 'year') then
            if (cccvx_day) then
               value = '360_day'
            else if (no_leap_years) then
               value = '365_day'
            else
               value = 'gregorian'
            endif
         endif
      else if (string == 'set' .and.        ! try to set known options, but
     .         no_newdate_env_options) then ! environment has precedence
         ii = index( evalue,'year=' )
         if (ii > 0) then
            if (evalue(ii+5:ii+12)      == '365_day'  .or.
     .          evalue(ii+5:ii+12)      == '360_day') then
               no_leap_years            =  .true.
               cccvx_day                =  .false. 
               if (evalue(ii+5:ii+12)   == '360_day')
     .            cccvx_day             =  .true. 
            else if (evalue(ii+5:ii+13) == 'gregorian') then
               no_leap_years            =  .false.
               cccvx_day                =  .false. 
            endif
         endif
      endif

      return
      end

      integer function LeapYear_Adjust(tdate1,tdate2,
     #                                 true_date_mode,adding)
      implicit none
      logical :: adding
      character(len=1) true_date_mode ! (B)asic or (E)xtended true dates
      integer, parameter :: limite = 23595500
      integer :: true2print,print2true
      integer :: ier,tdate1,tdate2,inc,m1,m2
      integer :: year,annee,y1,y1L,y2,p1a,p1b,p2a,p2b
      integer :: ndays,tdate1L,tdate28f,tdate29f,addit
      integer, dimension(14) :: cdate1,cdate2
      logical :: bissextile
      integer :: newdate
      external newdate,datmgp2

      bissextile(year) =  ( ( (MOD(year,4)   == 0)
     +                   .and.(MOD(year,100) /= 0) )
     +                   .or. (MOD(year,400) == 0) )

      addit=0 ! If adding, will hold a day in units of True Dates

      if (true_date_mode == 'B') then     ! Basic true date mode
         true2print=-2
         print2true=+2
         if (adding) addit=17280
      elseif (true_date_mode == 'E') then ! Extended true date mode
         true2print=-7
         print2true=+7
         if (adding) addit=24
      endif

      tdate1L = tdate1 ! Local value of tdat1; if adding, it will gradually
                       ! evolve to its real value as leap days are found

      ier = newdate(tdate1,p1a,p1b,true2print) ! true date to printable, but this
      y1 =      p1a / 10000                    ! may still accounts for leap days
      m1 = mod( p1a / 100 , 100 )
      ier = newdate(tdate2,p2a,p2b,true2print)
      y2 =      p2a / 10000
      m2 = mod( p2a / 100 , 100 )
!!!   print *,'Dans LeapYear_Adjust...'
CCC   print *,'Debut=',mod(p2a,100),m2,y2
CCC   print *,'Fin  =',mod(p1a,100),m1,y1
      ndays = 0 ; inc = 1
      if (y2 > y1 .or. (y1 == y2 .and. m2 > m1)) inc=-1
      do annee = y2,y1,inc
        if (bissextile(annee)) then
          ier = newdate(tdate28f,annee*10000+0228,limite,print2true)
          ier = newdate(tdate29f,annee*10000+0229,0,print2true)
          if (tdate29f <= tdate28f) print *,'Error tdate29f < tdate28f'
          if (inc > 0) then
            if ((tdate2 <= tdate28f) .and. (tdate1L >= tdate29f)) then
              ndays = ndays+inc
              tdate1L = tdate1L+addit*inc
CCC           write(6,*) annee, ' ndays=',ndays
            else
CCC           print *,annee,' exclue'
            endif
          else
            if ((tdate2 >= tdate28f) .and. (tdate1L <= tdate29f)) then
              ndays = ndays+inc
              tdate1L = tdate1L+addit*inc
            endif
          endif
        endif
      enddo
      ier = newdate(tdate1L,p1a,p1b,true2print)
      y1L = p1a / 10000
!!!   print *,'FinL =',mod(p1a,100),mod(p1a/100,100),y1L
      do annee = y1+inc,y1L,inc
        if (bissextile(annee)) then
          ier = newdate(tdate28f,annee*10000+0228,limite,print2true)
          ier = newdate(tdate29f,annee*10000+0229,0,print2true)
          if (tdate29f <= tdate28f) print *,'Error tdate29f < tdate28f'
          if (inc > 0) then
            if ((tdate2 <= tdate28f) .and. (tdate1L >= tdate29f)) then
              ndays = ndays+inc
              tdate1L = tdate1L+addit*inc
            endif
          else
            if ((tdate2 >= tdate28f) .and. (tdate1L <= tdate29f)) then
              ndays = ndays+inc
              tdate1L = tdate1L+addit*inc
            endif
          endif
        endif
      enddo

      LeapYear_Adjust = ndays
      return
      end

***FUNCTION NEWDATE : CONVERTS DATES BETWEEN TWO OF THE FOLLOWING 
*FORMATS: PRINTABLE DATE, CMC DATE-TIME STAMP, TRUE DATE
*
      FUNCTION NEWDATE(DAT1,DAT2,DAT3,MODE)
      IMPLICIT NONE
      INTEGER NEWDATE,DAT1,DAT2(*),DAT3,MODE
*
*AUTHOR   - E. BEAUCHESNE  -  JUN 96
*
*REVISION 001   M. Lepine, B.dugas - automne 2009/janvier 2010 -
*               Ajouter le support des dates etendues (annees 0
*               a 10000) via les nouveaux modes +- 5, 6 et 7.
*REVISION 002   B.dugas - novembre 2010 - Correction au mode -7.
*
*LANGUAGE - fortran
*
*OBJECT(NEWDATE)
*         - CONVERTS A DATE BETWEEN TWO OF THE FOLLOWING FORMATS: 
*           PRINTABLE DATE, CMC DATE-TIME STAMP(OLD OR NEW), TRUE DATE
*
*USAGE    - CALL NEWDATE(DAT1,DAT2,DAT3,MODE)
*
*ARGUMENTS
* MODE CAN TAKE THE FOLLOWING VALUES:-7,-6,-5,-4,-3,-2,-1,1,2,3,4,5,6,7
* MODE=1 : STAMP TO (TRUE_DATE AND RUN_NUMBER)
*     OUT - DAT1 - THE TRUEDATE CORRESPONDING TO DAT2
*      IN - DAT2 - CMC DATE-TIME STAMP (OLD OR NEW STYLE)
*     OUT - DAT3 - RUN NUMBER OF THE DATE-TIME STAMP
*      IN - MODE - SET TO 1  
* MODE=-1 : (TRUE_DATE AND RUN_NUMBER) TO STAMP
*      IN - DAT1 - TRUEDATE TO BE CONVERTED
*     OUT - DAT2 - CMC DATE-TIME STAMP (OLD OR NEW STYLE)
*      IN - DAT3 - RUN NUMBER OF THE DATE-TIME STAMP
*      IN - MODE - SET TO -1
* MODE=2 : PRINTABLE TO TRUE_DATE
*     OUT - DAT1 - TRUE_DATE
*      IN - DAT2 - DATE OF THE PRINTABLE DATE (YYYYMMDD)
*      IN - DAT3 - TIME OF THE PRINTABLE DATE (HHMMSSHH)
*      IN - MODE - SET TO 2
* MODE=-2 : TRUE_DATE TO PRINTABLE
*      IN - DAT1 - TRUE_DATE 
*     OUT - DAT2 - DATE OF THE PRINTABLE DATE (YYYYMMDD)
*     OUT - DAT3 - TIME OF THE PRINTABLE DATE (HHMMSSHH)
*      IN - MODE - SET TO -2
* MODE=3 : PRINTABLE TO STAMP
*     OUT - DAT1 - CMC DATE-TIME STAMP (OLD OR NEW STYLE)
*      IN - DAT2 - DATE OF THE PRINTABLE DATE (YYYYMMDD)
*      IN - DAT3 - TIME OF THE PRINTABLE DATE (HHMMSSHH)
*      IN - MODE - SET TO 3
* MODE=-3 : STAMP TO PRINTABLE
*      IN - DAT1 - CMC DATE-TIME STAMP (OLD OR NEW STYLE)
*     OUT - DAT2 - DATE OF THE PRINTABLE DATE (YYYYMMDD)
*     OUT - DAT3 - TIME OF THE PRINTABLE DATE (HHMMSSHH)
*      IN - MODE - SET TO -3
* MODE=4 : 14 word old style DATE array TO STAMP and array(14)
*     OUT - DAT1 - CMC DATE-TIME STAMP (OLD OR NEW STYLE)
*      IN - DAT2 - 14 word old style DATE array
*      IN - DAT3 - UNUSED
*      IN - MODE - SET TO 4
* MODE=-4 : STAMP TO 14 word old style DATE array
*      IN - DAT1 - CMC DATE-TIME STAMP (OLD OR NEW STYLE)
*     OUT - DAT2 - 14 word old style DATE array
*      IN - DAT3 - UNUSED
*      IN - MODE - SET TO -4
* MODE=5    PRINTABLE TO EXTENDED STAMP (year 0 to 10,000)
*     OUT - DAT1 - EXTENDED DATE-TIME STAMP (NEW STYLE only)
*      IN - DAT2 - DATE OF THE PRINTABLE DATE (YYYYMMDD)
*      IN - DAT3 - TIME OF THE PRINTABLE DATE (HHMMSSHH)
*      IN - MODE - SET TO 5
* MODE=-5   EXTENDED STAMP (year 0 to 10,000) TO PRINTABLE
*      IN - DAT1 - EXTENDED DATE-TIME STAMP (NEW STYLE only)
*     OUT - DAT2 - DATE OF THE PRINTABLE DATE (YYYYMMDD)
*     OUT - DAT3 - TIME OF THE PRINTABLE DATE (HHMMSSHH)
*      IN - MODE - SET TO -5
* MODE=6 :  EXTENDED STAMP TO EXTENDED TRUE_DATE (in hours)
*     OUT - DAT1 - THE TRUEDATE CORRESPONDING TO DAT2
*      IN - DAT2 - CMC DATE-TIME STAMP (OLD OR NEW STYLE)
*     OUT - DAT3 - RUN NUMBER, UNUSED (0)
*      IN - MODE - SET TO 6  
* MODE=-6 : EXTENDED TRUE_DATE (in hours) TO EXTENDED STAMP
*      IN - DAT1 - TRUEDATE TO BE CONVERTED
*     OUT - DAT2 - CMC DATE-TIME STAMP (OLD OR NEW STYLE)
*      IN - DAT3 - RUN NUMBER, UNUSED
*      IN - MODE - SET TO -6
* MODE=7  - PRINTABLE TO EXTENDED TRUE_DATE (in hours)
*     OUT - DAT1 - EXTENDED TRUE_DATE
*      IN - DAT2 - DATE OF THE PRINTABLE DATE (YYYYMMDD)
*      IN - DAT3 - TIME OF THE PRINTABLE DATE (HHMMSSHH)
*      IN - MODE - SET TO 7
* MODE=-7 : EXTENDED TRUE_DATE (in hours) TO PRINTABLE
*      IN - DAT1 - EXTENDED TRUE_DATE 
*     OUT - DAT2 - DATE OF THE PRINTABLE DATE (YYYYMMDD)
*     OUT - DAT3 - TIME OF THE PRINTABLE DATE (HHMMSSHH)
*      IN - MODE - SET TO -7
*NOTES    - IT IS RECOMMENDED TO ALWAYS USE THIS FUNCTION TO 
*           MANIPULATE DATES  
*         - IF MODE ISN'T IN THESE VALUES(-7,..,-2,-1,1,2,...,7) OR IF 
*           ARGUMENTS AREN'T VALID, NEWDATE HAS A RETURN VALUE OF 1
*         - A TRUE DATE IS AN INTEGER (POSSIBLY NEGATIVE) THAT 
*           CONTAINS THE NUMBER OF 5 SECONDS INTERVALS SINCE 
*           1980/01/01 00H00. NEGATIVE VALUES ARISE AS
*           THIS CONCEPT APPLIES FROM 1900/01/01.
*         - AN EXTENDED TRUE DATE IS AN INTEGER THAT CONTAINS
*           THE NUMBER OF HOURS SINCE YEAR 00/01/01
*         - SEE INCDATR FOR DETAIL ON CMC DATE-TIME STAMP
*     
*         useful constants
*         17280 = nb of 5 sec intervals in a day
*         288   = nb of 5 min intervals in a day
*         jd1900 = julian day for jan 1, 1900       (2415021)
*         jd1980 = julian day for jan 1, 1980       (2444240)
*         jd2236 = julian day for jan 1, 2236       (2537742)
*         jd0    = julian day for jan 1, 0          (1721060)
*         jd10k  = julian day for jan 1, 10,000     (5373485)
*         td1900 = truedate  for  jan 1, 1900 , 00Z (-504904320)
*         td2000 = truedate  for  jan 1, 2000 , 00Z (+126230400)
*         tdstart = base for newdates ( jan 1, 1980, 00Z)
*         max_offset = (((jd10k-jd0)*24)/8)*10      (109572750)
*         exception = extended truedate for jan 1, 1901, 01Z
*         troisg = 3 000 000 000
*WARNING  - IF NEWDATE RETURNS 1, OUTPUTS CAN TAKE ANY VALUE
*
**     
      integer tdate,runnb,stamp,tmpr,dtpr,td1900,td2000
      integer year,month,day,zulu,second,minute, max_offset
      integer tdstart,jd2236,jd1980,jd1900,jd0,jd10k,exception
      integer , dimension(12) :: mdays
      integer*8 date_unsigned,troisg,stamp8
      equivalence (stamp,stamp8)
      external idatmg2, datmgp2
      integer idatmg2
      data tdstart /123200000/,jd1980 /2444240/,jd1900 /2415021/
      data jd0 /1721060/,jd10k /5373485/, max_offset /109572750/
      data jd2236 /2537742/, exception /16663825/
      data td2000 /126230400/, td1900 /-504904320/
      data troisg /Z'B2D05E00'/
      data mdays /31,29,31,30,31,30,31,31,30,31,30,31/ 
*
      integer :: jd
      logical :: bissextile,validtd,validtm,validtme
*
*     big endian when w16(1) is zero
      integer       w32
      integer*2     w16(2)
      equivalence ( w16(1) , w32 )
      data          w32/1/
*
*     calculates julian calendar day
*     see CACM letter to editor by Fliegel and Flandern 1968
*     page 657
*
      jd(year,month,day)=day-32075+1461*(year+4800+(month-14)/12)/4
     #     +367*(month-2-(month-14)/12*12)/12 
     #     -3*((year+4900+(month-14)/12)/100)/4
*
*     check that date > jan 1, 1980 if 5 sec interval, else > jan 1,1900
*
      validtd(tdate)=((tdate.ge.0) .or. ((tdate.lt.0) .and. 
     #     (tdate >= td1900).and.(mod(tdate-td1900,720) == 0)))
*
*     check that year,month,day,zulu have valid values
*
      validtm(year,month,day,zulu)=(
     #     (year.ge.1900) .and. (year.lt.2236) .and.
     #     (month.le.12) .and. (day.le.mdays(month))
     #     .and. (zulu.le.23) .and.
     #     (month.gt.0) .and. (day.gt.0) .and. (zulu.ge.0))
*
      validtme(year,month,day,zulu)=(
     #     (year  >= 0) .and. (year  <  10000) .and.
     #     (month >  0) .and. (month <= 12)    .and.
     #     (day   >  0) .and. (day   <= mdays(month))    .and.
     #     (zulu  >= 0) .and. (zulu  <= 23) )
*
      bissextile(year) =  ( ( (MOD(year,4)   == 0)
     +                   .and.(MOD(year,100) /= 0) )
     +                   .or. (MOD(year,400) == 0) )
*
      if (abs(mode).gt.7 .or. mode.eq.0) goto 4 
      newdate=0 ; stamp8 = 0
      goto (106,104,103,101,1,2,3,4,5,6,7,100,102,105,107),(mode+8)
*     
*     mode=-3 : from stamp(old or new) to printable
*     
 1    stamp=dat1
*     stamp .lt. -1 means extended stamp
      if (stamp.lt.-1) goto 103
      dat2(1)=0
      dat3=0
      if (stamp.ge.tdstart) then
*     stamp is a new date-time stamp 
         tdate=(stamp-tdstart)/10*8+mod(stamp-tdstart,10)
         call datec(jd1900+(tdate-td1900)/17280,year,month,day)
         zulu=mod(tdate-td1900,17280)/720
         second=(mod(tdate-td1900,17280)-zulu*720)*5
         dtpr=year*10000+month*100+day
         tmpr=zulu*1000000+(second/60)*10000+mod(second,60)*100
      else
*     stamp is an old date-time stamp
         zulu=mod(stamp/10,100) 
         year=mod(stamp/1000,100)+1900 
         day=mod(stamp/100000,100)  
         month=mod(stamp/10000000,100) 
         dtpr=year*10000+month*100+day
         tmpr=zulu*1000000
      endif
      if (.not.validtm(year,month,day,zulu)) goto 4
      if ((month .eq. 2) .and. (day .eq. 29)) then
         if (.not. bissextile(year)) goto 4
      endif
      dat2(1)=dtpr
      dat3=tmpr
      return
      
*     
*     mode=3 : from printable to stamp
*     
 7    dtpr=dat2(1)
      tmpr=dat3
      dat1=0
      year=mod(dtpr/10000,10000)
*     dtpr,tmpr=19010101,01000000 will be encoded extended stamp
*     as the corresponding old date-time stamp is used as an
*     error indicator by INCDATR/IDATMG2/DATMGP2
      if (dtpr == 19010101 .and. tmpr == 01000000) goto 102
*     years not in [ 1900,2235 ] will be encoded extended stamp
      if (year < 1900 .or. year > 2235) goto 102
      month=mod(dtpr/100,100)
      day=mod(dtpr,100)
      zulu=mod(tmpr/1000000,100)
      second=mod(tmpr/10000,100)*60+mod(tmpr/100,100)
      if (.not.validtm(year,month,day,zulu)) goto 4
      if ((month .eq. 2) .and. (day .eq. 29)) then
         if (.not. bissextile(year)) goto 4
      endif
      tdate=(jd(year,month,day)-jd1980)*17280+zulu*720+second/5
      if (year.ge.2000 .or. (year.ge.1980 .and. second.ne.0)) then
*        encode it in a new date-time stamp
         stamp=tdstart+(tdate/8)*10+mod(tdate,8)
      else
*        encode it in an old date-time stamp
         tdate=(tdate-td1900)/720*720+td1900
         call datec(jd1900+(tdate-td1900)/17280,year,month,day)
         zulu=mod(tdate-td1900,17280)/720
         stamp=month*10000000+day*100000+(year-1900)*1000+zulu*10
      endif
      dat1=stamp
      return
      
*     
*     mode=-2 : from true_date to printable
*     
 2    tdate=dat1
      if (.not.validtd(tdate)) goto 4
      call datec(jd1900+(tdate-td1900)/17280,year,month,day)
      zulu=mod(tdate-td1900,17280)/720
      second=(mod(tdate-td1900,17280)-zulu*720)*5
      dat2(1)=year*10000+month*100+day
      dat3=zulu*1000000+second/60*10000+mod(second,60)*100
      return
      
*     
*     mode=2 : from printable to true_date
*     
 6    dtpr=dat2(1)
      tmpr=dat3
*     dtpr,tmpr=19010101,01000000 will be encoded extended true_date
*     as the corresponding old date-time stamp is used as an
*     error indicator by INCDATR/IDATMG2/DATMGP2
      if (dtpr == 19010101 .and. tmpr == 01000000) goto 107
      year=mod(dtpr/10000,10000)
      month=mod(dtpr/100,100)
      day=mod(dtpr,100)
      zulu=mod(tmpr/1000000,100)
      second=mod(tmpr/10000,100)*60+mod(tmpr/100,100)
      if (.not.validtm(year,month,day,zulu)) goto 4
      if ((month .eq. 2) .and. (day .eq. 29)) then
         if (.not. bissextile(year)) goto 4
      endif
      dat1=(jd(year,month,day)-jd1980)*17280+zulu*720+second/5
      return
      
*     
*     mode=-1 : from (true_date and run_number) to stamp
*     
 3    tdate=dat1
      runnb=dat3
 33   if((runnb.gt.9) .or. (.not.validtd(tdate))) goto 4
*     use new stamp if > jan 1, 2000 or fractional hour
      if (tdate.ge.td2000 .or. mod(tdate,720).ne.0) then
*     encode it in a new date-time stamp, ignore run nb
         stamp=tdstart+(tdate/8)*10+mod(tdate,8)
      else
*     encode it in an old date-time stamp
         call datec(jd1900+(tdate-td1900)/17280,year,month,day)
         tdate=(tdate-td1900)/720*720+td1900
         zulu=mod(tdate-td1900,17280)/720
         stamp=month*10000000+day*100000+(year-1900)*1000+zulu*10
     #        +runnb
      endif
      dat2(1)=stamp
      return
      
*     
*     mode=1 : from stamp(old or new) to (true_date and run_number)
*     
 5    stamp=dat2(1)
      if (stamp.ge.tdstart) then
*     stamp is a new date-time stamp 
         tdate=(stamp-tdstart)/10*8+mod(stamp-tdstart,10)
         runnb=0
      else if (stamp .lt. -1) then
        print *,'newdate error: mode 1, negative stamp'
        goto 4
      else
*     stamp is an old date-time stamp
         runnb=mod(stamp,10)
         zulu=mod(stamp/10,100) 
         year=mod(stamp/1000,100)+1900
         day=mod(stamp/100000,100)  
         month=mod(stamp/10000000,100) 
         tdate=(jd(year,month,day)-jd1980)*17280+zulu*720
      endif
      if (.not.validtd(tdate)) goto 4
      dat1=tdate
      dat3=runnb
      return
            
*     
*     mode=4 : from 14 word old style DATE array TO STAMP and array(14)
*     
100   dat1=idatmg2(dat2)
      return
*     
*     mode=-4 : from STAMP TO 14 word old style DATE array
*     
101   dat2(14)=dat1
      call datmgp2(dat2)
      return
*     
*     mode=5 : from printable to extended stamp
*     
102   continue
      dtpr=dat2(1)
      tmpr=dat3
      dat1=0
      year=mod(dtpr/10000,10000)
      month=mod(dtpr/100,100)
      day=mod(dtpr,100)
      zulu=mod(tmpr/1000000,100)
      minute=mod(tmpr/10000,100)
      if (.not.validtme(year,month,day,zulu)) goto 4
      if ((month .eq. 2) .and. (day .eq. 29)) then
         if (.not. bissextile(year)) goto 4
      endif
      tdate=jd(year,month,day)
      if (tdate < jd0 .or. tdate >= jd10k) then
         print *,'newdate error: date outside of supported range, ',
     +           'date =',dtpr
         goto 4
      endif
      tdate=(tdate-jd0)*24+zulu+minute/60
*        encode it in a new date-time stamp
      stamp=(tdate/8)*10+mod(tdate,8)
      date_unsigned=stamp + troisg
      dat1=date_unsigned
      return
*     
*     mode=-5 : from extended stamp to printable
*     
103   continue
      stamp = dat1
      dat2(1)= 0 ; dat3=0
      date_unsigned = stamp8
      if (w16(1).eq.0) date_unsigned = ishft(stamp8,-32)
      if (date_unsigned <  troisg .or. 
     +    date_unsigned >= troisg + max_offset) then
        print *,'newdate error: invalid stamp for mode -5, stamp=',stamp
        goto 4
      endif
      stamp=date_unsigned - troisg
      tdate=stamp/10*8+mod(stamp,10)
      call datec(jd0+tdate/24,year,month,day)
      zulu=mod(tdate,24)
      minute=0
      if (.not.validtme(year,month,day,zulu)) goto 4
      if ((month .eq. 2) .and. (day .eq. 29)) then
         if (.not. bissextile(year)) goto 4
      endif
      dat2(1)=year*10000+month*100+day
      dat3=zulu*1000000+minute*10000
      return
*     
*     mode=-6 : from extended true date to stamp
*     
104   continue
      tdate=dat1
      if (tdate == exception .or.        ! 1901010101
     +   (tdate/24+jd0) <  jd1900 .or.
     +   (tdate/24+jd0) >= jd2236) then ! extended stamp
         stamp=(tdate/8)*10+mod(tdate,8)
         date_unsigned=stamp + troisg
         dat2(1)=date_unsigned
      else                               ! (new or old) stamp
         runnb=0
         zulu=mod(tdate,24)
         tdate=(tdate/24+jd0-jd1980)*17280+zulu*720
         goto 33
      endif
      return
*     
*     mode=6 : from stamp to extended true date
*     
105   continue
      stamp=dat2(1)
      if (stamp .lt. -1) then
        dat1=0
        dat3=0
        date_unsigned = stamp8
        if (w16(1).eq.0) date_unsigned = ishft(stamp8,-32)
        if (date_unsigned < troisg .or. 
     +      date_unsigned > troisg + max_offset) then
           print *,'newdate error: invalid stamp for mode -6, ',
     +             'stamp=',stamp
           goto 4
        endif
        stamp=date_unsigned - troisg
        tdate=stamp/10*8+mod(stamp,10)
        dat1=tdate
        dat3=0
      else
        if (stamp.ge.tdstart) then
*     stamp is a new date-time stamp 
          tdate=(stamp-tdstart)/10*8+mod(stamp-tdstart,10)
          call datec(jd1900+(tdate-td1900)/17280,year,month,day)
          zulu=mod(tdate-td1900,17280)/720
          runnb=0
          tdate=(jd(year,month,day)-jd0)*24+zulu
!         print *,'Debug stamp > tdstart tdate=',tdate,
!     %           ' zulu=',zulu
        else
*     stamp is an old date-time stamp
           runnb=mod(stamp,10)
           zulu=mod(stamp/10,100) 
           year=mod(stamp/1000,100)+1900
           day=mod(stamp/100000,100)  
           month=mod(stamp/10000000,100) 
           tdate=(jd(year,month,day)-jd0)*24+zulu
!           print *,'Debug old date stamp tdate=',tdate
        endif
        if (.not.validtd(tdate)) goto 4
        dat1=tdate
        dat3=runnb
      endif
      return
*     
*     mode=-7 : from extended true_date to printable
*     
106   tdate=dat1
      if (.not.validtd(tdate)) goto 4
      call datec(jd0+tdate/24,year,month,day)
      zulu=mod(tdate,24)
      if (.not.validtme(year,month,day,zulu)) goto 4
      if ((month .eq. 2) .and. (day .eq. 29)) then
         if (.not. bissextile(year)) goto 4
      endif
      minute=0
      dat2(1)=year*10000+month*100+day
      dat3=zulu*1000000+minute*10000
      return
*     
*     mode=7 : from printable to extended true_date
*     
107   dtpr=dat2(1)
      tmpr=dat3
      year=mod(dtpr/10000,10000)
      if (year < 0 .or. year >= 10 000) then
         print *,'newdate error: date outside of supported range, ',
     +           'date =',dtpr
         goto 4
      endif
      month=mod(dtpr/100,100)
      day=mod(dtpr,100)
      zulu=mod(tmpr/1000000,100)
      second=mod(tmpr/10000,100)*60+mod(tmpr/100,100)
      if (.not.validtme(year,month,day,zulu)) goto 4
      if ((month .eq. 2) .and. (day .eq. 29)) then
         if (.not. bissextile(year)) goto 4
      endif
      dat1=(jd(year,month,day)-jd0)*24+zulu
      return
*     
*     error: bad mode or bad arguments 
*     
 4    newdate=1
      return
      end
 
