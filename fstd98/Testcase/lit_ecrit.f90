      program testca
      integer iun_in,iun_out,key1,iun_out2
      integer dateo,deet,npas,ni,nj,nk,nbits,datyp,ip1,ip2,ip3
      integer ig1,ig2,ig3,ig4,swa,lng,dltf,ubc
      integer extra1,extra2,extra3,datev
      integer fstinf,fstprm,fstinl,fstecr,fstouv,fnom,fstluk
      integer force_datyp, force_nbits
      logical rewrit_flag
      integer ip_list(2)
      parameter (NMAX=1500)
      integer liste(NMAX)
      external fstinf,fstprm,fstecr,ccard,fstinl,fstouv,fnom,fstluk

      real, allocatable, dimension(:) :: buf
      character *12 etiket
      character *4 nomvar
      character *4 nom_list(4)
      character *1 grtyp
      character *2 typvar
      character *12 etik_list(3)

      character*128 cle(4)
      character*128 def(4),val(4)

      data ip_list / -1,750 /
      data cle /'fstin.', 'fstout.', 'datyp', 'nbits'/
      data def /'void',    'void', '-1', '-1' /
      data val /'void',    'void', '-1', '-1' /
      data etik_list /'PASLA','R2428V4N','Label003'/
      data nom_list /'ES','UU','VV','HU'/

      rewrit_flag = .false.
      ipos = 0
      call ccard(cle,def,val, 4, ipos)
      read(val(3),'(I)') force_datyp
      read(val(4),'(I)') force_nbits
      iun_in = 10
      iun_out = 11
      iun_out2 = 12
!      ier=fnom(unf,
!     %'/usr/local/env/armnlib/data/SAMPLES/fstd_files/sample_fstd98',
!     %           'std+rnd',0)
      ier = fnom(iun_in,val(1),'STD+RND+R/O+OLD',0)
      print *,'Debug fnom=',ier
      ier = fnom(iun_out, val(2), 'STD+RND+R/W', 0)
!      ier = fnom(iun_out2, 'Turbo_comp_'//val(2), 'STD+RND+R/W', 0)
      ier = fstouv(iun_in,'RND')
      ier = fstouv(iun_out,'RND')
!      ier = fstouv(iun_out2,'RND')
!      call desire_nomvar(0,0,nom_list,4)
!      call desire_ip1(1,1,ip_list,-2,-1)
!      call desire_etiquette(1,1,etik_list,3)
!      call desire_ip1(0,1,ip_list,3,-1)
!      call desire_etiquette(0,1,etik_list,3)
      ier = fstopc('MSGLVL','WARNIN',.false.)
      key1 = fstinl (iun_in,ni1,nj1,nk1,-1,' ',-1,-1,-1,' ',' ',&
                     liste,infon,NMAX)
      print *,'Debug infon=',infon
      do i=1,infon
        key1=liste(i)
!        print *,' i=',i,' key1 = ',key1
        ier = fstprm(key1,DATEO,DEET, NPAS, NI, NJ, NK, NBITS, DATYP, IP1,&
                     IP2, IP3, TYPVAR, NOMVAR, ETIKET, GRTYP, IG1, IG2, IG3,&
                     IG4, SWA, LNG, DLTF, UBC, DATEV, EXTRA2, EXTRA3)
        if (ier < 0) then
          print *,'Debug fstprm errno',ier
          exit
        endif
!       print *,'Debug apres fstprm dateo=',dateo,' datev=',datev,' nomvar=',&
!                nomvar,' ip1=',ip1,' ni nj nk =',ni,nj,nk

        allocate(buf(ni*nj*nk))
        ier = fstluk(buf,key1,ni,nj,nk)
        if (ier < 0) then
          print *,'Debug fstluk errno',ier
          exit
        endif
        if (force_datyp .ne. -1) then
          datyp = force_datyp
          print *,'Debug datyp force a =',force_datyp
        endif
        if (force_nbits .ne. -1) then
          nbits = force_nbits
          print *,'Debug nbits force a ',force_nbits
        endif
        ier = FSTECR(buf, buf, -nbits, iun_out, dateo, deet, npas, ni, nj,&
                     nk, ip1, ip2, ip3, typvar, nomvar, etiket, grtyp, ig1, ig2,&
                     ig3, ig4, datyp, rewrite_flag)
!        ier = FSTECR(buf, buf, -nbits, iun_out2, dateo, deet, npas, ni, nj,&
!                     nk, ip1, ip2, ip3, typvar, nomvar, etiket, grtyp, ig1, ig2,&
!                     ig3, ig4, datyp+128, rewrite_flag)
        if (ier < 0) then
          print *,'Debug fstecr errno',ier
          exit
        endif
!       key = fstsui(lnkdiun(1),ni,nj,nk)
        deallocate(buf)
      enddo

!      ier = fstvoi(iun_out2,'RND')
      ier = fstfrm(iun_in)
      ier = fstfrm(iun_out)
!      ier = fstfrm(iun_out2)

      stop
      end
