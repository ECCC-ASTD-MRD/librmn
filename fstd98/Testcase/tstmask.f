      program testmask
      implicit none
      integer unf,key1
      integer dateo,deet,npas,ni,nj,nk,nbits,datyp,ip1,ip2,ip3
      integer ig1,ig2,ig3,ig4,swa,lng,dltf,ubc
      integer extra1,extra2,extra3,datev,mip1,mip2,mip3
      integer ier,imsq,key1,ni1,nj1,nk1
      integer fstinf,fstprm,fnom,fstouv,fstmsq,fstfrm,fstsui
      external fstinf,fstprm,fnom,fstouv,fstmsq,fstfrm,fstsui

      character *12 etiket,metiq
      character *4 nomvar
      character *2 grtyp, typvar

      unf = 10
      ier=fnom(unf,
*     %'/usr/local/env/armnlib/data/SAMPLES/fstd_files/latlon.fst',
     %'/users/dor/armn/lib/data3/VOIR/pmnew',
     %           'std+rnd+R/O',0)
      print *,'Debug fnom=',ier
      ier = fstouv(unf,'RND')
      IMSQ=FSTMSQ(10,268435455, -1, -1, '1***56789ABC',.FALSE.)
      IMSQ=FSTMSQ(10,MIP1,MIP2,MIP3,METIQ,.TRUE.)
      WRITE(6,63) MIP1, MIP2, MIP3, METIQ
63    FORMAT(' MIP1,MIP2,MIP3=',3(1x,z8.8),' METIQ -->',A12,'<--')

      key1 = fstinf (unf,ni1,nj1,nk1,-1,'G?B',54525955,-1,-1,' ',' ')

      DO 
         if (key1 .lt. 0) EXIT
         ier = fstprm(key1,DATEO,DEET, NPAS, NI, NJ, NK, NBITS, DATYP,
     %        IP1,IP2, IP3, TYPVAR, NOMVAR, ETIKET, GRTYP, IG1, IG2,
     %        IG3,IG4, SWA, LNG, DLTF, UBC, DATEV, EXTRA2, EXTRA3)
         print *,'Debug nom=',nomvar,'typ=',typvar,'etiket=',etiket,
     %        ' ip1-2-3',ip1,ip2,ip3
         key1 = fstsui(unf,ni1,nj1,nk1)

      END DO

      ier = fstfrm(10)

      stop
      end
