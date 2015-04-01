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
**************************************************************************
*                                                                        *
*  Author   : Jianhui He                                                 *
*                                                                        *
*  Objective: compute the min and max of an array of float on NEC        *
*                                                                        *
*  Arguments:                                                            *
*    IN    floats   array of float                                       *
*    IN    count    total count of array of float                        *
*    IN    str      spacing information of array of float                *
*    OUT   xmax     max float                                            *
*    OUT   xmin     min float                                            *
*    IN    miss     1: indicate missing value exsist                     *
*    IN    FLG      missing value identifier                             *
*                                                                        *
**************************************************************************
      SUBROUTINE gggmima2 (floats, count, str, xmax, xmin, 
     % miss, FLG )
      


      INTEGER count, str, miss
      REAL*8 xmax, xmin
      REAL FLG
      REAL floats(str, count)      
      INTEGER i, j

      IF ( miss .EQ. 0 ) THEN
         xmax = floats(1, 1)
         xmin = floats(1, 1)

         DO i = 2, count
            xmin = min (xmin,floats(1,i))
            xmax = max (xmax,floats(1,i))
         END DO
         
      ELSE IF ( miss .EQ. 1 ) THEN
         i = 0

         DO WHILE ( floats(1,i) .EQ. FLG .AND. i .LT. count )
            i = i + 1
         END DO

         xmax = floats(1, i)
         xmin = floats(1, i)

         DO j = i, count
            IF ( floats(1, j) .ne. FLG ) then
               xmin = min (xmin,floats(1,j))
               xmax = max (xmax,floats(1,j))
            END IF
         END DO 

      END IF
      
     

      END


**************************************************************************
*                                                                        *
*  Author   : Jianhui He                                                 *
*                                                                        *
*  Objective: compute the min and max of an array of double on NEC       *
*                                                                        *
*  Arguments:                                                            *
*    IN    floats   array of double                                      *
*    IN    count    total count of array of double                       *
*    IN    str      spacing information of array of double               *
*    OUT   xmax     max double                                           *
*    OUT   xmin     min double                                           *
*    IN    miss     1: indicate missing value exsist                     *
*    IN    FLG      missing value identifier                             *
*                                                                        *
**************************************************************************


      SUBROUTINE gggmima3 (floats, count, str, xmax, xmin, 
     % miss, FLG )
      


      INTEGER count, str, miss
      REAL*8 xmax, xmin
      REAL FLG
      REAL*8 floats(str, count)      
      INTEGER i, j

      IF ( miss .EQ. 0 ) THEN
         xmax = floats(1, 1)
         xmin = floats(1, 1)

         DO i = 2, count
            xmin = min (xmin,floats(1,i))
            xmax = max (xmax,floats(1,i))
         END DO
         
      ELSE IF ( miss .EQ. 1 ) THEN
         i = 0
         DO WHILE ( floats(1,i) .EQ. FLG .AND. i .LT. count )
            i = i + 1
         END DO

         xmax = floats(1, i)
         xmin = floats(1, i)

         DO j = i, count
            IF ( floats(1, j) .ne. FLG ) then
               xmin = min (xmin,floats(1,j))
               xmax = max (xmax,floats(1,j))
            END IF
         END DO 

      END IF
 

      END


**********************************************************************************
*                                                                                *
*   Author    : Jianhui He                                                       *
*                                                                                *
*   Objective : obtain an array of float from an array of integer on NEC         *
*                                                                                *
*   Arguments :                                                                  *
*     IN    IPAK     array of integer                                            *
*     OUT   IUNP     array of float                                              *
*     IN    stride   spacing information on array of float                       *
*     IN    count    total count of element in array of integer                  *
*     IN    hasMiss  1: indicate missing value exsits                            *
*     IN    mToken   missing value identifier in array of integer                *
*     IN    missTag  missing value identifier in array of float                  *
*     IN    minFlt   min float                                                   *
*     IN    mulFact  multiplication factor to scale the final float              *
*                                                                                *
**********************************************************************************
      SUBROUTINE aakk1 (IPAK, IUNP, stride, count, 
     % hasMiss, mToken, missTag, minFlt, mulFact)

      INTEGER stride, count
      INTEGER IPAK(stride, count)
      REAL    IUNP(stride, count)
      INTEGER hasMiss, mToken
      REAL    missTag
      REAL*8  minFlt, mulFact
      INTEGER i



      IF ( hasMiss .EQ. 1) THEN
*************************************
*   when missing value is           *
*         present                   *
*************************************
         DO i = 1, count

            IF (IPAK(1,i) .EQ. mToken ) THEN
               IUNP(1,i) = missTag
            ELSE IF ( IPAK(1,i) .EQ. 0 ) THEN
               IUNP(1,i) = minFlt
            ELSE
               IUNP(1,i)=IPAK(1,i)*mulFact*
     % 1.0000000000000001 +minFlt
            END IF

         END DO

      ELSE
*************************************
*     no missing values             *
*                                   *
*************************************
         DO i = 1, count
            IUNP(1,i) = IPAK(1,i) * mulFact *
     % 1.0000000000000001 + minFlt      
         END DO
   
      END IF

      END
***********************************************************************************
*                                                                                 *
*   Author    : Jianhui He                                                        *
*                                                                                 *
*   Objective : obtain an array of double from an array of integer on NEC         *
*                                                                                 *
*   Arguments :                                                                   *
*     IN    IPAK     array of integer                                             *
*     OUT   IUNP     array of double                                              *
*     IN    stride   spacing information on array of double                       *
*     IN    count    total count of element in array of integer                   *
*     IN    hasMiss  1: indicate missing value exsits                             *
*     IN    mToken   missing value identifier in array of integer                 *
*     IN    missTag  missing value identifier in array of double                  *
*     IN    minFlt   min double                                                   *
*     IN    mulFact  multiplication factor to scale the final double              *
*                                                                                 *
***********************************************************************************
      SUBROUTINE aakk2 (IPAK, IUNP, stride, count, 
     % hasMiss, mToken, missTag, minFlt, mulFact)

      INTEGER stride, count
      INTEGER IPAK(stride, count)
      REAL*8    IUNP(stride, count)
      INTEGER hasMiss, mToken
      REAL*8    missTag, minFlt, mulFact
      INTEGER i



      IF ( hasMiss .EQ. 1) THEN
*************************************
*   when missing value is           *
*         present                   *
*************************************
         DO i = 1, count

            IF (IPAK(1,i) .EQ. mToken ) THEN
               IUNP(1,i) = missTag
            ELSE IF ( IPAK(1,i) .EQ. 0 ) THEN
               IUNP(1,i) = minFlt
            ELSE
               IUNP(1,i)=IPAK(1,i)*mulFact*
     % 1.0000000000000001 +minFlt
            END IF

         END DO

      ELSE
*************************************
*     no missing values             *
*                                   *
*************************************
         DO i = 1, count
            IUNP(1,i) = IPAK(1,i) * mulFact *
     % 1.0000000000000001 + minFlt      
*       WRITE(*,*) IPAK(1,i), mulFact, minFlt

         END DO
   
      END IF

      END
***********************************************************************************
*                                                                                 *
*  Author    : Jianhui He                                                         *
*                                                                                 *
*  Objective : transform an array of integer into a continguous bit stream on NEC *
*              when bit size of packed integer is 16                              *
*                                                                                 *
*  Arguments :                                                                    *
*    IN    IUNP     unpacked integer array                                        *
*    OUT   IPAK     packed integer array                                          *
*    IN    tokenSi  bit size of the packed integer                                *
*    IN    wordSi   bit size of a word                                            *
*    IN    offset   last bit of integer packed into the packed integer array      *
*    IN    stride   spacing information of the unpacked integer array             *
*    IN    count    total count of element                                        *
*                                                                                 *
***********************************************************************************
      SUBROUTINE aaMM1 (IUNP, IPAK, tokenSi, wordSi, offset, 
     % stride, count)

      INTEGER tokenSi, wordSi, offset, stride, count
      INTEGER IUNP(stride, 2, count)
      INTEGER IPAK(count)
      INTEGER myMask
      INTEGER i, depth


 
******************************
*                            *
*   construct mask to        *
*    chop off the            * 
*     desired bit            *
*                            *
******************************
      myMask = ILS(-1, offset)
      IPAK(1) = IAND(IPAK(1), myMask)




*****************************************
*                                       *
*  decide how depth of array            *
*  which contains meaningful numbers    *
*                                       *
*****************************************

      depth = count / 2



***********************************************
*                                             *
*   retrieve the integer representation       *
***********************************************
      IF ( offset .GT. 16 ) THEN
         
         DO i = 1, depth

            IPAK(i) = IOR(IOR(IPAK(i),ILS(IUNP(1,1,i), 
     % offset-16)), IRL(IUNP(1,2,i),wordSi-offset ))
            IPAK(i+1) = ILS(IUNP(1,2,i), offset)
         END DO            
      ELSE IF ( offset .LT. 16 ) THEN
         DO i = 1, depth 
            IPAK(i) = IOR(IPAK(i), IRL(IUNP(1,1,i),16-offset))
            IPAK(i+1) = IOR(ishft(IUNP(1,1,i),wordSi-(16-offset)), 
     % ILS(IUNP(1,2,i), offset))

         END DO
      ELSE IF ( offset .EQ. 0 ) THEN
         DO i = 1, depth
            IPAK(i) = IUNP(1,2,i)
            IPAK(i) = IOR(IRL(IUNP(1,1,i), 16), IPAK(i))
         END DO
      ELSE IF ( offset .EQ. 16 ) THEN
         DO i = 1, depth
            IPAK(i)   = IOR(IPAK(i), IUNP(1,1,i))
            IPAK(i+1) = ILS(IUNP(1,2,i), 16) 
         END DO
      END IF

      
      IF ( IAND(count,1) .ne. 0 ) THEN
         IF ( offset .GT. 16 ) THEN
         
            IPAK(depth+1) = IOR(IOR(IPAK(depth+1),ILS(IUNP(1,1,depth+1), 
     %           offset-16)), 0 )
            IPAK(depth+2) = 0
         ELSE IF ( offset .LT. 16 ) THEN
            IPAK(depth+1) = IOR(IPAK(depth+1), 
     %                      IRL(IUNP(1,1,depth+1),16-offset))
            IPAK(depth+2)=IOR(ILS(IUNP(1,1,depth+1),
     %                    wordSi-(16-offset)),0)
         ELSE IF ( offset .EQ. 0 ) THEN
            IPAK(depth+1) = IOR(IRL(IUNP(1,1,depth+1), 16), 0)
         ELSE IF ( offset .EQ. 16 ) THEN
            IPAK(depth+1)   = IOR(IPAK(depth+1), IUNP(1,1,depth+1))
            IPAK(depth+2)   = 0
         ENDIF
      ENDIF

      RETURN
      END


***********************************************************************************
*                                                                                 *
*  Author    : Jianhui He                                                         *
*                                                                                 *
*  Objective : transform a continguous bit stream to an array of integer on NEC   *
*              when bit size of packed integer is 16                              *
*                                                                                 *
*  Arguments :                                                                    *
*    IN    IUNP     unpacked integer array                                        *
*    OUT   IPAK     packed integer array                                          *
*    IN    tokenSi  bit size of the packed integer                                *
*    IN    wordSi   bit size of a word                                            *
*    IN    offset   last bit of integer packed into the packed integer array      *
*    IN    stride   spacing information of the unpacked integer array             *
*    IN    count    total count of element                                        *
*                                                                                 *
***********************************************************************************
      SUBROUTINE aaZZ1 (IUNP, IPAK, tokenSi, wordSi, offset, 
     % stride, count)

      INTEGER tokenSi, wordSi, offset, stride, count
      INTEGER IUNP(stride, 2, count/(stride*2)+1)
      INTEGER IPAK(count)
      INTEGER crossML, crossMR, contMsk
      INTEGER i, depth


      

      IF ( offset .EQ. 0 ) THEN
         crossMR = IRL(-1, 16)
         crossML = ILS(-1, 16)
      ELSE IF ( offset .EQ. 16 ) THEN 
         crossML = IRL(-1, 16)
         crossMR = ILS(-1, 16)
      ELSE IF ( offset .GT. 16 ) THEN
         crossML = IRL(-1, wordSi-(offset-16))
         crossMR = ILS(-1, offset)
      ELSE IF ( offset .LT. 16 ) THEN
         crossML = IRL(-1, wordSi-offset)
         crossMR = ILS(-1, wordSi-(tokenSi-offset))
      END IF




      contMsk = NOT(IOR(crossML, crossMR))

      depth = count / 2
    
      IF ( offset .EQ. 0 ) THEN
         DO i = 1, depth
            IUNP(1,1,i) = IRL(IAND(IPAK(i), crossML), 16)
            IUNP(1,2,i) = IAND(IPAK(i), crossMR)
         END DO
      ELSE IF ( offset .EQ. 16 ) THEN
         DO i = 1, depth
            IUNP(1,1,i) = IAND(IPAK(i), crossML)
            IUNP(1,2,i) = IRL(IAND(IPAK(i+1), crossMR), 16)            
         END DO
      ELSE IF ( offset .GT. 16 ) THEN
         DO i = 1, depth
            IUNP(1,1,i)=IRL((IAND(IPAK(i),contMsk)), 
     % offset-16)
            IUNP(1,2,i)=IOR(ILS(IAND(IPAK(i),crossML),wordSize-offset),
     % IRL(IAND(IPAK(i+1), crossMR), offset) )
         END DO
      ELSE IF ( offset .LT. 16 ) THEN
         DO i = 1, depth 
            IUNP(1,1,i)= IOR(ILS(IAND(IPAK(i),crossML),
     % tokenSi-offset), 
     % IRL(IAND(IPAK(i+1), crossMR), wordSi-tokenSi+offset))
            IUNP(1,2,i) = IRL(IAND(IPAK(i+1), contMsk), offset)
         END DO        
      END IF
     
      IF ( IAND(count,1) .ne. 0 ) THEN
         IF ( offset .EQ. 0 ) THEN
            IUNP(1,1,DEPTH+1) = IRL(IAND(IPAK(DEPTH+1), crossML), 16)
         ELSE IF ( offset .EQ. 16 ) THEN
            IUNP(1,1,DEPTH+1) = IAND(IPAK(DEPTH+1), crossML)
         ELSE IF ( offset .GT. 16 ) THEN
            IUNP(1,1,DEPTH+1)=IRL((IAND(IPAK(DEPTH+1),contMsk)), 
     %           offset-16)
         ELSE IF ( offset .LT. 16 ) THEN
            IUNP(1,1,DEPTH+1)= IOR(ILS(IAND(IPAK(DEPTH+1),crossML),
     %         tokenSi-offset), 
     %         IRL(IAND(IPAK(DEPTH+2), crossMR), wordSi-tokenSi+offset))
         END IF
      ENDIF

      RETURN
      END
