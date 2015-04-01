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
      PROGRAM tfstd97
      implicit none
      
      INTEGER MI,MJ,MK,DI,DJ,DK
      PARAMETER (MI=8,MJ=3,MK=2)
      PARAMETER (DI=6,DJ=4,DK=3)

      INTEGER F1(MI,MJ,MK), F1LU(MI,MJ,MK)
      REAL F2(DI,DJ,DK), F3(DI,DJ,DK), F4(DI,DJ,DK), work(4096)
      REAL F2LU(DI,DJ,DK), F3LU(DI,DJ,DK), F4LU(DI,DJ,DK)

      INTEGER dateo, datev, prntdat, timed, deet, npas
      INTEGER ni, nj, nk, ig1, ig2, ig3, ig4, ip1, ip2, ip3
      CHARACTER * 12 etiket
      CHARACTER *4 nomvar
      CHARACTER *2 typvar
      CHARACTER *1 grtyp

      INTEGER ier, err1, err2, err3, err4, i, j, k, date, datyp
      INTEGER handle
      REAL seed
      REAL *8 deltat
*modules
      INTEGER fnom,fstouv,fstecr,fstinf,fstluk,fstfrm
      INTEGER fstrwd,fstopc,fstlir
      EXTERNAL fnom,fstouv,fstecr,fstinf,fstluk,fstfrm
      EXTERNAL fstrwd,fstopc,fstlir
      EXTERNAL newdate

c      dateo = 201522050
c      call newdate(dateo,prntdat,timed,-3)
c      print *,'Debug for dateo = ',dateo,' prntdat = ',
c     %        prntdat,' timed = ',timed
c      dateo = 120589003
c      call newdate(dateo,prntdat,timed,-3)
c      print *,'Debug for dateo = ',dateo,' prntdat = ',
c     %        prntdat,' timed = ',timed
c      DEET = 2
c      NPAS = 900
c      deltat = (DEET*NPAS + 1800.)/3600.
c      CALL INCDATr(DATEV, DATEo, deltat)
c      print *,'Debug dateo = ',dateo
c      print *,'Debug datev = ',datev
c      call newdate(datev,prntdat,timed,-3)
c      print *,'Debug for datev = ',datev,' prntdat = ',
c     %        prntdat,' timed = ',timed

      seed = 1.0
      do i =1,MI
         do j=1,MJ
            do k=1,MK
c               F1(i,j,k) = i*100 + j*10 +k
               F1(i,j,k) = -1
            enddo
         enddo
      enddo
      do i =1,DI
         do j=1,DJ
            do k=1,DK
               F2(i,j,k) = seed*1000. + i*100. + j*10. +k
               F3(i,j,k) = seed*1000. + i*100. + j*10. +k
               F4(i,j,k) = seed*1000. + i*100. + j*10. +k
c               write(6,*) 'Debug F2 F3 F4 =',f2(i,j,k),f3(i,j,k),
c     %                     f4(i,j,k)
               seed = seed+1.0
            enddo
         enddo
      enddo

      call xxpak(f2,work,DI,DJ*DK,-24,0,1)
      call xxpak(f2lu,work,DI,DJ*DK,-24,0,2)
      err2 = 0
      do i =1,DI
         do j=1,DJ
            do k=1,DK
               if(F2(i,j,k) .ne. F2LU(i,j,k)) then
                  write(6,790) i,j,k,F2LU(i,j,k),i,j,k,F2(i,j,k)
                  err2=err2+1
               endif
            enddo
         enddo
      enddo
      call testerr(err2,0)
      
      write(6,*) 'TEST 1.0 - FSTOUV - fichier 10 (RND)'
      ier = fnom(10,'fic_rnd_fstd98','STD+RND',0)
      err1 = fstouv(10,'RND')
      call testerr(err1,0)
      write(6,*) 'TEST 1.1 - FSTOPC - msglvl=debug'
      err1 = FSTOPC('MSGLVL','DEBUG',.FALSE.)
      call testerr(err1,0)
      write(6,*) 'TEST 1.2 - FSTOPC - printopt'
      err1 = FSTOPC('PRINTOPT','LEVEL+NODEET+NONPAS+DATEO',.FALSE.)
      call testerr(err1,0)
      write(6,*) 'TEST 2.0 - FSTECR - iun=10 (i24, r24, r32, x32)'
      etiket = 'TEST-FSTD98'
      nomvar = 'Var1'
      typvar = 'OA'
      grtyp = 'G'
      date = 19971230
      timed = 0
      datyp = 4
      ip1 = 11
      ip2 = 22 
      ip3 = 33
      ig1 = 40
      ig2 = 50
      ig3 = 60
      ig4 = 70
      deet = 4
      npas = 1800
      call newdate(dateo,date,timed,3)
      print *,'Debug for date = ',date,' dateo = ',dateo
      deltat = (DEET*NPAS)/3600.
      CALL INCDATr(DATEV, DATEo, deltat)
      print *,'Debug dateo = ',dateo
      print *,'Debug datev = ',datev
      call newdate(datev,prntdat,timed,-3)
      write(6,777) 'Debug for datev = ',datev,' prntdat = ',
     %        prntdat,' timed = ',timed
 777  format(a,i9,a,i8,a,i8.8)
      err1 = fstecr(F1,work,-24,10,dateo,deet,npas,MI,MJ,MK,ip1,
     %              ip2,ip3,typvar,nomvar,etiket,grtyp,
     %              ig1,ig2,ig3,ig4,datyp,.true.)
      err1 = fstecr(F1,work,-24,10,dateo,deet,npas,MI,MJ,MK,ip1,
     %              ip2,ip3,typvar,nomvar,etiket,grtyp,
     %              ig1,ig2,ig3,ig4,datyp,.true.)
      datyp = 1
      err2 = fstecr(F2,work,-24,10,dateo,deet,npas,DI,DJ,DK,ip1+1,
     %              ip2,ip3,typvar,nomvar,etiket,grtyp,
     %              ig1,ig2,ig3,ig4,datyp,.false.)
      err3 = fstecr(F3,work,-32,10,dateo,deet,npas,DI,DJ,DK,ip1+2,
     %              ip2,ip3,typvar,nomvar,etiket,grtyp,
     %              ig1,ig2,ig3,ig4,datyp,.false.)
      datyp = 0
      err4 = fstecr(F4,work,-32,10,dateo,deet,npas,DI,DJ,DK,ip1+3,
     %              ip2,ip3,typvar,nomvar,etiket,grtyp,
     %              ig1,ig2,ig3,ig4,datyp,.false.)
      call testerr(err1+err2+err3+err4,0)
      write(6,*) 'TEST 2.1 - FSTRWD - sur fichier random'
      err2 = fstrwd(10)
      call testerr(err2,-37)
      write(6,*) 'TEST 3.0 - FSTFRM - fichier 10'
      err1 = fstfrm(10)
      call testerr(err1,0)
      write(6,*) 'TEST 4.0 - FSTOUV - reouverture fichier 10 (RND)'
      err1 = fstouv(10,'RND')
      call testerr(err1,4)
      write(6,*) 'TEST 5.0 - FSTINF - fichier 10'
      handle = fstinf(10,ni,nj,nk,datev,etiket,
     %              ip1,ip2,ip3,typvar,nomvar)
      call testerr(handle,1)
      write(6,*) 'TEST 6.0 - FSTLUK - fichier 10 champ #1'
      err1 = fstluk(F1LU,handle,ni,nj,nk)
      call testerr(err1,handle)
      write(6,*) 'TEST 6.1 - verification FSTLUK - (i24)'
      err2 = 0
      do i =1,MI
         do j=1,MJ
            do k=1,MK
               if(F1(i,j,k) .ne. F1LU(i,j,k)) then
                  write(6,789) i,j,k,F1LU(i,j,k),i,j,k,F1(i,j,k)
 789              format('F1LU(',i1,i1,i1,')=',i8,' F1(',i1,i1,i1,
     %                   ') =',i8)
                  err2=err2+1
               endif
            enddo
         enddo
      enddo
      call testerr(err2,0)
      write(6,*) 'TEST 7.0 - FSTLIR - fichier 10 champ #2'
      handle = fstlir(F2LU,10,ni,nj,nk,datev,etiket,
     %              ip1+1,ip2,ip3,typvar,nomvar)
      call testerr(handle,129)
      write(6,*) 'TEST 7.1 - verification FSTLIR - (r24)'
 8888 CONTINUE
      err2 = 0
      do i =1,DI
         do j=1,DJ
            do k=1,DK
               if(F2(i,j,k) .ne. F2LU(i,j,k)) then
                  write(6,790) i,j,k,F2LU(i,j,k),i,j,k,F2(i,j,k)
 790              format('F2LU(',i1,i1,i1,')=',F9.1,' F2(',i1,i1,i1,
     %                   ') =',F9.1)
                  err2=err2+1
               endif
            enddo
         enddo
      enddo
      call testerr(err2,0)
      write(6,*) 'TEST 8.0 - FSTLIR - fichier 10 champ #3'
      handle = fstlir(F3LU,10,ni,nj,nk,datev,etiket,
     %              ip1+2,ip2,ip3,typvar,nomvar)
      call testerr(handle,257)
      write(6,*) 'TEST 8.1 - verification FSTLIR - (r32)'
      err2 = 0
      do i =1,DI
         do j=1,DJ
            do k=1,DK
               if(F3(i,j,k) .ne. F3LU(i,j,k)) then
                  write(6,791) i,j,k,F3LU(i,j,k),i,j,k,F3(i,j,k)
 791              format('F3LU(',i1,i1,i1,')=',f9.1,' F3(',i1,i1,i1,
     %                   ') =',f9.1)
                  err2=err2+1
               endif
            enddo
         enddo
      enddo
      call testerr(err2,0)
      write(6,*) 'TEST 9.0 - FSTLIR - fichier 10 champ #4'
      handle = fstlir(F4LU,10,ni,nj,nk,datev,etiket,
     %              ip1+3,ip2,ip3,typvar,nomvar)
      call testerr(handle,385)
      write(6,*) 'TEST 9.1 - verification FSTLIR - (x32)'
      err2 = 0
      do i =1,DI
         do j=1,DJ
            do k=1,DK
               if(F4(i,j,k) .ne. F4LU(i,j,k)) then
                  write(6,792) i,j,k,F4LU(i,j,k),i,j,k,F4(i,j,k)
 792              format('F4LU(',i1,i1,i1,')=',f9.1,' F4(',i1,i1,i1,
     %                   ') =',f9.1)
                  err2=err2+1
               endif
            enddo
         enddo
      enddo
      call testerr(err2,0)
      write(6,*) 'TEST 10.0 - FSTFRM - fichier 10'
      err1 = fstfrm(10)
      call testerr(err1,0)
      stop
      end

      SUBROUTINE testerr(errcode,errcod2)
      integer errcode,errcod2
*   TRAITE LE RESULTAT D'UN TEST

      if (errcode .ne. errcod2) then
         WRITE(6,888) '<<<<<<<<<<<<<<<<   ERREUR  >>>>>>>>>>>>>>>>>>',
     %              ' errcode =',errcode,' code attendu =',errcod2
 888     format(a,a,i8,a,i8)
c         STOP 'Program aborts'
         return
      else
         WRITE(6,10)
      endif
10    FORMAT(' ---REUSSI',/)
      RETURN
      END
