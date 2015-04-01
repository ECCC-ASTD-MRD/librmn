      program testca
      integer iun_in,iun_out,key1,iun_out2
      integer dateo,deet,npas,ni,nj,nk,nbits,datyp,ip1,ip2,ip3
      integer ig1,ig2,ig3,ig4,swa,lng,dltf,ubc
      integer extra1,extra2,extra3,datev
      integer fstinf,fstprm,fstsui,fstecr,fstouv,fnom,fstluk
      logical rewrit_flag
      parameter (NMAX=1500)
      integer liste(NMAX)
      external fstinf,fstprm,fstecr,ccard,fstsui,fstouv,fnom,fstluk

      real, allocatable, dimension(:) :: buf
      character *8 etiket
      character *2 nomvar
      character *4 nom_list(4)
      character *1 grtyp, typvar
      character *12 etik_list(3)

      character*128 cle(2)
      character*128 def(2),val(2)

      data cle /'fstin.', 'fstout.'/
      data def /'void',    'void' /
      data val /'void',    'void' /
      data etik_list /'PASLA','R2428V4N','Label003'/
      data nom_list /'ES','UU','VV','HU'/

      rewrit_flag = .false.
      ipos = 0
      call ccard(cle,def,val, 2, ipos)
      iun_in = 10
      iun_out = 11
      iun_out2 = 12
!      ier=fnom(unf,
!     %'/usr/local/env/armnlib/data/SAMPLES/fstd_files/sample_fstd98',
!     %           'std+rnd',0)
      ier = fnom(iun_in,val(1),'STD+RND+R/O+OLD',0)
!      ier = fnom(iun_out, val(2), 'STD+RND+R/W', 0)
      ier = fstouv(iun_in,'RND')
!      ier = fstouv(iun_out,'RND')
      ier = fstopc('MSGLVL','WARNIN',.false.)
      key1 = fstinf (iun_in,ni1,nj1,nk1,-1,' ',(/ip1_all(0.555,1),ip1_all(0.3840,1)/),-1,-1,' ',' ')
      suivant: do
        print *,'Debug key1=',key1
        ier = fstprm(key1,DATEO,DEET, NPAS, NI, NJ, NK, NBITS, DATYP, IP1,&
                     IP2, IP3, TYPVAR, NOMVAR, ETIKET, GRTYP, IG1, IG2, IG3,&
                     IG4, SWA, LNG, DLTF, UBC, DATEV, EXTRA2, EXTRA3)
        if (ier < 0) then
          print *,'Debug fstprm errno',ier
          exit
        endif
        print *,'Debug apres fstprm dateo=',dateo,' datev=',datev,' nomvar=',&
                nomvar,' ip1=',ip1,' ni nj nk =',ni,nj,nk

!        allocate(buf(ni*nj*nk))
!        ier = fstluk(buf,key1,ni,nj,nk)
!        if (ier < 0) then
!          print *,'Debug fstluk errno',ier
!          exit
!        endif
!        ier = FSTECR(buf, buf, -nbits, iun_out, dateo, deet, npas, ni, nj,&
!                     nk, ip1, ip2, ip3, typvar, nomvar, etiket, grtyp, ig1, ig2,&
!                     ig3, ig4, datyp+0, rewrite_flag)
!        if (ier < 0) then
!          print *,'Debug fstecr errno',ier
!          exit
!        endif
        key1 = fstsui(iun_in,ni,nj,nk)
        print *,'Debug fstsui key=',key1
!        deallocate(buf)
        if (key1 .lt. 0) exit suivant
      end do suivant

!      ier = fstvoi(iun_out2,'RND')
      ier = fstfrm(iun_in)
!      ier = fstfrm(iun_out)

      stop
      end
