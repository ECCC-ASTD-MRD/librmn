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
*         - THERE ARE TWO STYLES OF DATES (BOTH USE INTEGERS):
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
*--------------------------------------------------------------------
*
      integer idate1,idate2
      real*8 nhours
      logical  adding,rounding
      external newdate
      integer newdate,result
      integer tdate1,tdate2,runnum
      integer tdstart, jd1980
      data  tdstart /123200000/, jd1980 /2444240/

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
      result=newdate(tdate1,idate1,runnum,1)
      if(result.ne.0) then 
         print *,'label 3'
         goto 2
      endif

 1    result=newdate(tdate2,idate2,runnum,1)
      if(result.ne.0) then
         print *,'label 1,idate2:',idate2
         goto 2
      endif
      if (adding) then
         rounding=rounding.or.(tdate2.lt.0)
         if (rounding) then
            tdate2=(tdate2+sign(360,tdate2))/720*720
            tdate1=tdate2+720*nint(nhours)
         else
            tdate1=tdate2+nint(720*nhours)
         endif
         result=newdate(tdate1,idate1,runnum,-1)
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
      endif   
      return

 2    if (adding) then
         idate1=101010101
      else
         nhours=2.0**30
      endif
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
*LANGUAGE - fortran
*
*OBJECT(NEWDATE)
*         - CONVERTS A DATE BETWEEN TWO OF THE FOLLOWING FORMATS: 
*           PRINTABLE DATE, CMC DATE-TIME STAMP(OLD OR NEW), TRUE DATE
*
*USAGE    - CALL NEWDATE(DAT1,DAT2,DAT3,MODE)
*
*ARGUMENTS
* MODE CAN TAKE THE FOLLOWING VALUES:-3,-2,-1,1,2,3
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
*NOTES    - IT IS RECOMMENDED TO ALWAYS USE THIS FUNCTION TO 
*           MANIPULATE DATES  
*         - IF MODE ISN'T IN THESE VALUES(-3,-2,-1,1,2,3) OR IF 
*           ARGUMENTS AREN'T VALID, NEWDATE HAS A RETURN VALUE OF 1
*         - A TRUE DATE IS AN INTEGER (POSSIBLY NEGATIVE) THAT 
*           CONTAINS THE NUMBER OF 5 SECONDS INTERVALS SINCE 
*           1980/01/01 00H00
*         - SEE INCDATR FOR DETAIL ON CMC DATE-TIME STAMP
*     
*         useful constants
*         17280 = nb of 5 sec intervals in a day
*         126230400 = truedate for jan 1, 2000 , 00Z
*         -504904320 = truedate of jan 1, 1900
*         jd1900 = julian day for jan 1, 1900
*         jd1980 = julian day for jan 1, 1980
*         tdstart = base for newdates ( jan 1, 1980, 00Z)
*WARNING  - IF NEWDATE RETURNS 1, OUTPUTS CAN TAKE ANY VALUE
*
**     
      logical validtm,validtd
      integer tdate,runnb,stamp,tmpr,dtpr
      integer year,month,day,zulu,second
      integer jd,tdstart,jd1980,jd1900
      external idatmg2, datmgp2
      integer idatmg2
      data tdstart /123200000/,jd1980 /2444240/,jd1900 /2415021/
*
*     calculates julian calendar day
*     see cacm letter to editor by fliegel and flandern 1968
*     page 657
*
      jd(year,month,day)=day-32075+1461*(year+4800+(month-14)/12)/4
     #     +367*(month-2-(month-14)/12*12)/12 
     #     -3*((year+4900+(month-14)/12)/100)/4
*
*     check that date > jan 1, 1980 if 5 sec interval, else > jan 1,1900
*
      validtd(tdate)=((tdate.ge.0) .or. ((tdate.lt.0) .and. 
     #     (tdate.ge.-504904320).and.(mod(tdate+504904320,720).eq.0)))
*
*     check that year,month,day,zulu have valid values
*
      validtm(year,month,day,zulu)=((year.ge.1900) .and. 
     #     (month.le.12) .and. (day.le.31) .and. (zulu.le.23) .and.
     #     (month.gt.0) .and. (day.gt.0) .and. (zulu.ge.0))
*
      if (abs(mode).gt.4 .or. mode.eq.0) goto 4 
      newdate=0
      goto (101,1,2,3,4,5,6,7,100),(mode+5)
*     
*     mode=-3 : from stamp(old or new) to printable
*     
 1    stamp=dat1
      dat2(1)=0
      dat3=0
      if (stamp.ge.tdstart) then
*     stamp is a new date-time stamp 
         tdate=(stamp-tdstart)/10*8+mod(stamp-tdstart,10)
         call datec(jd1900+(tdate+504904320)/17280,year,month,day)
         zulu=mod(tdate+504904320,17280)/720
         second=(mod(tdate+504904320,17280)-zulu*720)*5
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
      month=mod(dtpr/100,100)
      day=mod(dtpr,100)
      zulu=mod(tmpr/1000000,100)
      second=mod(tmpr/10000,100)*60+mod(tmpr/100,100)
      if (.not.validtm(year,month,day,zulu)) goto 4
      tdate=(jd(year,month,day)-jd1980)*17280+zulu*720+second/5
      if (year.ge.2000 .or. second.ne.0) then
*        encode it in a new date-time stamp
         stamp=tdstart+(tdate/8)*10+mod(tdate,8)
      else
*        encode it in an old date-time stamp
         tdate=(tdate+504904320)/720*720-504904320
         call datec(jd1900+(tdate+504904320)/17280,year,month,day)
         zulu=mod(tdate+504904320,17280)/720
         stamp=month*10000000+day*100000+(year-1900)*1000+zulu*10
      endif
      dat1=stamp
      return
      
*     
*     mode=-2 : from true_date to printable
*     
 2    tdate=dat1
      if (.not.validtd(tdate)) goto 4
      call datec(jd1900+(tdate+504904320)/17280,year,month,day)
      zulu=mod(tdate+504904320,17280)/720
      second=(mod(tdate+504904320,17280)-zulu*720)*5
      dat2(1)=year*10000+month*100+day
      dat3=zulu*1000000+second/60*10000+mod(second,60)*100
      return
      
*     
*     mode=2 : from printable to true_date
*     
 6    dtpr=dat2(1)
      tmpr=dat3
      year=mod(dtpr/10000,10000)
      month=mod(dtpr/100,100)
      day=mod(dtpr,100)
      zulu=mod(tmpr/1000000,100)
      second=mod(tmpr/10000,100)*60+mod(tmpr/100,100)
      if (.not.validtm(year,month,day,zulu)) goto 4
      dat1=(jd(year,month,day)-jd1980)*17280+zulu*720+second/5
      return
      
*     
*     mode=-1 : from (true_date and run_number) to stamp
*     
 3    tdate=dat1
      runnb=dat3
      if((runnb.gt.9) .or. (.not.validtd(tdate))) goto 4
*     use new stamp if > jan 1, 2000 or fractional hour
      if (tdate.ge.126230400 .or. mod(tdate,720).ne.0) then
*     encode it in a new date-time stamp, ignore run nb
         stamp=tdstart+(tdate/8)*10+mod(tdate,8)
      else
*     encode it in an old date-time stamp
         call datec(jd1900+(tdate+504904320)/17280,year,month,day)
         tdate=(tdate+504904320)/720*720-504904320
         zulu=mod(tdate+504904320,17280)/720
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
*     error: bad mode or bad arguments 
*     
 4    newdate=1
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
*     OUT - IDATE(14)     - CMC DATE-TIME STAMP
*NOTES    
*         - RETURNS IDATE(14)=101010101 IF INPUTS ARE INVALID
*         - IDATE(1)=DAY OF THE WEEK(1-7,SUNDAY=1) (OPTIONAL)
*         - IDATE(2)=MONTH (1-12)
*         - IDATE(3)=DAY (1-31)                
*         - IDATE(4)=YEAR (0-99,1900-2100)
*         - IDATE(5)=ZULU (0-23)               
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
      if(result.ne.0) then
         idate(14)=101010101
      endif
      
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
*         - IDATE(3)=DAY (1-31)                
*         - IDATE(4)=YEAR (0-99,1900-2100)
*         - IDATE(5)=ZULU (0-23)               
*         - IDATE(6)=100*NUMBER_OF_SECOND_SINCE_LAST_HOUR (0,359 999)
*         - IDATE(7-13)=DATE-TIME GROUP IN CHARACTER FORMAT (7A4)
*         - IDATE(14)=DATE-TIME STAMP(OLD OR NEW)
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
