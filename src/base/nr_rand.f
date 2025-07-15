! RMNLIB - Library of useful routines for C and FORTRAN programming
! Copyright (C) 1975-2001  Division de Recherche en Prevision Numerique
!                          Environnement Canada
!
! This library is free software; you can redistribute it and/or
! modify it under the terms of the GNU Lesser General Public
! License as published by the Free Software Foundation,
! version 2.1 of the License.
!
! This library is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
! Lesser General Public License for more details.
!
! You should have received a copy of the GNU Lesser General Public
! License along with this library; if not, write to the
! Free Software Foundation, Inc., 59 Temple Place - Suite 330,
! Boston, MA 02111-1307, USA.

      module nr_rand_data
      integer, save :: iseed = -1
      integer, save :: iff = 0
      integer, save :: inext,inextp,ma(55)
      end

      INTEGER FUNCTION nr_rand_seed(idum)
      use nr_rand_data
      implicit none
      integer idum

      iseed=idum
      nr_rand_seed=idum
      end

      integer function nr_rand_i()
      use nr_rand_data
      implicit none
      INTEGER MBIG,MSEED,MZ
      PARAMETER (MBIG=1000000000,MSEED=161803398,MZ=0)
      INTEGER i,ii,k
      INTEGER mj,mk

      if(iseed.lt.0.or.iff.eq.0)then
        iff=1
        mj=MSEED-iabs(iseed)
        mj=mod(mj,MBIG)
        ma(55)=mj
        mk=1
        do i=1,54
          ii=mod(21*i,55)
          ma(ii)=mk
          mk=mj-mk
          if(mk.lt.MZ)mk=mk+MBIG
          mj=ma(ii)
        end do
        do k=1,4
          do i=1,55
            ma(i)=ma(i)-ma(1+mod(i+30,55))
            if(ma(i).lt.MZ)ma(i)=ma(i)+MBIG
          end do
        end do
        inext=0
        inextp=31
        iseed=1
      endif
      inext=inext+1
      if(inext.eq.56)inext=1
      inextp=inextp+1
      if(inextp.eq.56)inextp=1
      mj=ma(inext)-ma(inextp)
      if(mj.lt.MZ)mj=mj+MBIG
      ma(inext)=mj
      nr_rand_i=mj
      end

      real function nr_rand_r()
      use iso_fortran_env, only: real64
      implicit none
      integer nr_rand_i
      external nr_rand_i
      real(kind = real64) :: FAC,MBIG
      PARAMETER (MBIG=1000000000,FAC=1./MBIG)
      nr_rand_r=FAC*nr_rand_i()
      END

      real(kind = real64) function nr_rand_d()
      use iso_fortran_env, only: real64
      implicit none
      integer nr_rand_i
      external nr_rand_i
      real(kind = real64) :: FAC,MBIG
      PARAMETER (MBIG=1000000000,FAC=1./MBIG)
      nr_rand_d=FAC*nr_rand_i()
      END
