      program testca
      integer unf,key1
      integer dateo,deet,npas,ni,nj,nk,nbits,datyp,ip1,ip2,ip3
      integer ig1,ig2,ig3,ig4,swa,lng,dltf,ubc
      integer extra1,extra2,extra3,datev
      integer fstinf,fstprm
      external fstinf,fstprm

      character *8 etiket
      character *2 nomvar
      character *1 grtyp, typvar

      unf = 10
      ier=fnom(unf,
     %'/usr/local/env/armnlib/data/SAMPLES/fstd_files/sample_fstd98',
     %           'std+rnd',0)
      print *,'Debug fnom=',ier
      ier = fstouv(unf,'RND')
      key1 = fstinf (unf,ni1,nj1,nk1,-1,' ',-1,-1,-1,' ','PHI')
      print*, key1

      ier = fstprm(key1,DATEO,DEET, NPAS, NI, NJ, NK, NBITS, DATYP, IP1,
     %         IP2, IP3, TYPVAR, NOMVAR, ETIKET, GRTYP, IG1, IG2, IG3,
     %         IG4, SWA, LNG, DLTF, UBC, DATEV, EXTRA2, EXTRA3)
      print *,'Debug apres fstprm dateo=',dateo,' datev=',datev

      ier = fstfrm(10)

      stop
      end
