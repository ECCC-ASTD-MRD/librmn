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
      subroutine min_1_2
      real array(400,200), work(400*200+10), unpacked(400,200)
      character *1024 ARMNLIBREP

      call getenv('ARMNLIB',ARMNLIBREP)
C      open(11,file='/usr/local/env/armnlib/data/SAMPLES/min_1_2.txt',
      open(11,file=trim(ARMNLIBREP)//'/data/SAMPLES/min_1_2.txt',
     %form='formatted',access='sequential')
      read(11,*) array
      print *,'Debug array = '
      call MMMEAN(array,400,200,0)

      call xxpak(array,work,400,200,-22,0,1)
      do i =1,20
	write(6,88) work(i)
 88     format(2x,z8.8)
      enddo	
      call xxpak(unpacked,work,400,200,-22,0,2)

      print *,'Debug unpacked array  = '
      call MMMEAN(unpacked,400,200,0)

      do i =1,20
        work(i) = 0
      enddo	

      call xxpak(array,work,400,200,-22,0,1)
      do i =1,20
	write(6,88) work(i)
      enddo	
*      stop
      end

***FUNCTION mmMEAN - CALCULATES THE MEAN OF ARRAY A
*
      subroutine mmMEAN (A,NI,NJ,N)
      REAL A(NI,NJ)
*
*AUTHOR   - HERSH MITCHELL
*
*REVISION 001:  C. THIBEAULT - JUL 79  DOCUMENTATION AND CALL TO VECLIB
*REVISION 002:  C. THIBEAULT - MAR 83  CONVERSION AU CODE CRAY
*
*LANGUAGE  - FORTRAN
*
*OBJECT(AMEAN)
*         - CALCULATES MEAN OF A(NI,NJ) OMITTING N BORDER ROWS
*
*LIBRARIES
*         - SOURCE   RMNSOURCELIB,ID=RMNP     DECK=AMEAN
*         - OBJECT   RMNLIB,ID=RMNP
*
*USAGE    - FUNCTION AMEAN(A,NI,NJ,N)
*
*ARGUMENTS
*  IN     - A  - ARRAY WHERE THE MEAN IS CALCULATED FROM
*         - NI - X-DIMENSION
*         - NJ - Y-DIMENSION
*         - N  - NUMBER OF BORDER ROWS TO BE OMITTED
*
*
*
*-------------------------------------------------------------------------------
*
      real mini,maxi
      SUM = 0.
*
*
*  WHEN OMITTING N BORDER ROWS, SET INDICES
*
      IL=1+N
      JL=1+N
      IH=NI-N
      JH=NJ-N
      IF (N.EQ.0) JH=1
      IF (N.EQ.0) IH=NI*NJ
      PTS=(IH-IL+1)*(JH-JL+1)
*
      mini = a(1,1)
      maxi = a(1,1)
      DO 10 J=JL,JH
      DO 10 I=IL,IH
      SUM = SUM + A(I,J)
      mini = min(mini,a(i,j))
      maxi = max(maxi,a(i,j))
   10 CONTINUE
*
      AMEAN=SUM/PTS
      write(6,88) ' MMMEAN   min=',mini,' max=',maxi,'   mean=',amean
 88   format(a,g16.5,a,g16.5,a,g16.5)
*
*-------------------------------------------------------------------------------
*
      RETURN
      END
      subroutine min_2_4
      real array(400,200), work(400*200+10), unpacked(400,200)
      character *1024 ARMNLIBREP

      call getenv('ARMNLIB',ARMNLIBREP)

C      open(11,file='/usr/local/env/armnlib/data/SAMPLES/min_2_4.txt',
      open(11,file=trim(ARMNLIBREP)//'/data/SAMPLES/min_2_4.txt',
     %form='formatted',access='sequential')
      read(11,*) array
      print *,'Debug array = '
      call MMMEAN(array,400,200,0)

      call xxpak(array,work,400,200,-22,0,1)
      do i =1,20
	write(6,88) work(i)
 88     format(2x,z8.8)
      enddo	
      call xxpak(unpacked,work,400,200,-22,0,2)

      print *,'Debug unpacked array  = '
      call MMMEAN(unpacked,400,200,0)

      do i =1,20
        work(i) = 0
      enddo	

      call xxpak(array,work,400,200,-22,0,1)
      do i =1,20
	write(6,88) work(i)
      enddo	
*      stop
      end
      subroutine min_4_8
      real array(400,200), work(400*200+10), unpacked(400,200)
      character *1024 ARMNLIBREP

      call getenv('ARMNLIB',ARMNLIBREP)

C      open(11,file='/usr/local/env/armnlib/data/SAMPLES/min_4_8.txt',
      open(11,file=trim(ARMNLIBREP)//'/data/SAMPLES/min_4_8.txt',
     %form='formatted',access='sequential')
      read(11,*) array
      print *,'Debug array = '
      call MMMEAN(array,400,200,0)

      call xxpak(array,work,400,200,-22,0,1)
      do i =1,20
	write(6,88) work(i)
 88     format(2x,z8.8)
      enddo	
      call xxpak(unpacked,work,400,200,-22,0,2)

      print *,'Debug unpacked array  = '
      call MMMEAN(unpacked,400,200,0)

      do i =1,20
        work(i) = 0
      enddo	

      call xxpak(array,work,400,200,-22,0,1)
      do i =1,20
	write(6,88) work(i)
      enddo	
*      stop
      end
      subroutine min_8_16
      real array(400,200), work(400*200+10), unpacked(400,200)
      character *1024 ARMNLIBREP

      call getenv('ARMNLIB',ARMNLIBREP)

C      open(11,file='/usr/local/env/armnlib/data/SAMPLES/min_8_16.txt',
      open(11,file=trim(ARMNLIBREP)//'/data/SAMPLES/min_8_16.txt',
     %form='formatted',access='sequential')
      read(11,*) array
      print *,'Debug array = '
      call MMMEAN(array,400,200,0)

      call xxpak(array,work,400,200,-22,0,1)
      do i =1,20
	write(6,88) work(i)
 88     format(2x,z8.8)
      enddo	
      call xxpak(unpacked,work,400,200,-22,0,2)

      print *,'Debug unpacked array  = '
      call MMMEAN(unpacked,400,200,0)

      do i =1,20
        work(i) = 0
      enddo	

      call xxpak(array,work,400,200,-22,0,1)
      do i =1,20
	write(6,88) work(i)
      enddo	
*      stop
      end
