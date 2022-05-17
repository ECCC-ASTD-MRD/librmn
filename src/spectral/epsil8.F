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
      module alpnrt_comlrlm
        ! NOTE: AC, AD, AE may get used as 2D arrays
        ! in that case they are imported as AAC, AAD, AAE and 'pointer reshaped'
        integer, save :: LRLM = 0
        real(kind=8), dimension(:), pointer  ::  EPSI => NULL()
        real(kind=8), dimension(:), pointer  ::  AC => NULL()
        real(kind=8), dimension(:), pointer  ::  AD => NULL()
        real(kind=8), dimension(:), pointer  ::  AE => NULL()
      end module

      SUBROUTINE EPSIL8 (LALP,LM,KHEM) 
      use alpnrt_comlrlm

      IMPLICIT none

***    ****   OCT 1975  -  JOHN D. HENDERSON  ****

***    Modifie le 10 janvier 1994 par B.Dugas, RPN...
***    Calcul des constantes c,d,e pour le nouvel agorithme
***    des ALP. Les resultats sont places dans le bloc /ALPNRT/.

***    CALCULATES EPSILON AND OTHER CONSTANTS NEEDED IN LEGENDRE
***    POLYNOMIAL RECURSIVE CALCULATIONS FOR N FROM 0 TO LALP(M)-1,
***    AND M FROM 0 TO LM-1.

****   got rid of common blocks and Cray pointers  M.Valin 2022
****   introduced module alpnrt_comlrlm

      INTEGER  LM,KHEM,LALP(LM) 

      REAL*8    FNUM,FDEN, c,d,e      ! AC,AD,AE imported from alpnrt_comlrlm
      INTEGER   M,N,MN,    IR,IER     ! LRLM imported from alpnrt_comlrlm
      INTEGER   MULT,MR,   N1,LR,     MS,NS

      c(m,n) = sqrt( dble( (2*n+1)*(n+m-1)*(n+m-3) )/
     +               dble( (2*n-3)*(n+m  )*(n+m-2) ))
      d(m,n) = sqrt( dble( (2*n+1)*(n+m-1)*(n-m+1) )/
     +               dble( (2*n-1)*(n+m  )*(n+m-2) ))
      e(m,n) = sqrt( dble( (2*n+1)*(n-m) )/
     +               dble( (2*n-1)*(n+m) ))

*-----------------------------------------------------------------------
                     MULT = 1
      IF (KHEM.NE.0) MULT = 2

      IR = LALP(1)*MULT-1
      MR = 0

      IF (LRLM .NE. (LALP(1)*1000+LM))                         THEN

          IF (LRLM.NE.0)                                       THEN

***            DE-ALLOUER LA VIELLE MEMOIRE.
              if(associated(AC))   deallocate(AC)
              if(associated(AD))   deallocate(AD)
              if(associated(AE))   deallocate(AE)
              if(associated(EPSI)) deallocate(EPSI)

          END IF

***        ALLOUER L'ESPACE POUR LES AC,AD,AE ET POUR EPSI.

          allocate( AC(LM*LALP(1)*MULT) )
          allocate( AD(LM*LALP(1)*MULT) )
          allocate( AE(LM*LALP(1)*MULT) )
          allocate( EPSI(LM*LALP(1)*MULT) )

      END IF

      DO  M=1,LM

          MS = M-1
                       N1 = 1
          IF (MS.EQ.0) N1 = 2

          LR = LALP(M)*MULT 

          DO  N=N1,LR 

              MN   = MR+N 
              NS   = MS+N-1 
              FNUM =   NS**2 - MS**2
              FDEN = 4*NS**2 - 1 

              EPSI(MN) = SQRT( FNUM/FDEN )

          END DO

          MR = MR+LR

      END DO

      EPSI(1) = 0.

      LRLM   = LALP(1)*1000+LM

***    REMPLIR AC,AD ET AE AVEC LES C(M,N),D(M,N) ET E(M,N)
***    SANS LES CALCULER POUR M=0,1.

      MN = 0
      DO  N=0,IR

          MN = MN+2

          DO  M=2,LM-1
              MN     = MN+1
              AC(MN) = c(M,M+N)
              AD(MN) = d(M,M+N)
              AE(MN) = e(M,M+N)
          END DO

      END DO

      RETURN
*-----------------------------------------------------------------------

      END 
