      program testca
      implicit none
      integer iun_in,iun_out,key1,iun_out2,i,ipos,ier,j
      integer dateo,deet,npas,ni,nj,nk,nbits,datyp,ip1,ip2,ip3
      integer ig1,ig2,ig3,ig4,swa,lng,dltf,ubc
      integer extra1,extra2,extra3,datev
      integer fstinf,fstprm,fstinl,fstecr,fstouv,fnom,fstluk,fstecr_s,fstlir_s
      integer fstlir_h,fstlir_b,fstecr_h,fstecr_b,fstvoi,fstfrm,fstopc,fstlir
      
      logical rewrite_flag
      integer ip_list(2)
      integer *2 array_of_short(22,10), ref_short(22,10)
      integer *1 array_of_byte(22,10), ref_byte(22,10)
      integer array_of_int(22,10), ref_int(22,10)
      integer nmax
      parameter (NMAX=1500)
      integer liste(NMAX)
      external fstinf,fstprm,fstecr,ccard,fstinl,fstouv,fnom,fstluk,fstecr_s,fstlir_s
      external fstlir_h,fstlir_b,fstecr_h,fstecr_b,fstvoi,fstfrm,fstopc,fstlir
      
      real, allocatable, dimension(:) :: buf
      character *12 etiket
      character *4 nomvar
      character *4 nom_list(4)
      character *1 grtyp
      character *2 typvar
      character *12 etik_list(3)

      character*128 cle(2)
      character*128 def(2),val(2)
      character *80 roman,charbuf
      
      data roman /'Voici un petit bout de roman pour tester fstecr_s avec une string comme argument'/
      data charbuf /'Un peu de remplissage de n importe quoi pour verifier que ca fonctionne'/

      data ip_list / -1,750 /
      data cle /'fstin.', 'fstout.'/
      data def /'void',    'void' /
      data val /'void',    'void' /
      data etik_list /'PASLA','R2428V4N','Label003'/
      data nom_list /'ES','UU','VV','HU'/

      do i=1,22
       do j = 1,10
        array_of_short(i,j) = 0
        array_of_byte(i,j)  = 0
        array_of_int(i,j) = 0
        ref_short(i,j) = i
        ref_byte(i,j)  = i
        ref_int(i,j) = i
       enddo
      enddo
      rewrite_flag = .false.
      ipos = 0
      call ccard(cle,def,val, 2, ipos)
      iun_in = 10
      iun_out = 11
      iun_out2 = 12
      ier = fnom(iun_out, val(2), 'STD+RND+R/W', 0)
      ier = fstouv(iun_out,'RND')
      ier = fstopc('MSGLVL','INFORM',.false.)

        ni = 80
        nj = 1
        nk = 1
        allocate(buf(ni*nj*nk))
        
        nomvar = 'TesT'
        typvar = 'cc'
        etiket = 'String'
        datyp=7
        print *,'Debug appel fstecr_s'
        ier = FSTECR_S(roman, buf, -8, iun_out, dateo, deet, npas, ni, nj,&
                     nk, ip1, ip2, ip3, typvar, nomvar, etiket, grtyp, ig1, ig2,&
                     ig3, ig4, datyp, rewrite_flag)
        if (ier < 0) then
          print *,'Debug fstecr_s errno',ier
!          exit
        endif
        ier = FSTLIR_S(charbuf,iun_out,ni,nj,nk,datev,etiket,ip1,ip2,ip3,typvar,nomvar)
        if (ier < 0) then
          print *,'Debug fstlir_s errno',ier
        endif
        print *,'roman  =',roman
        print *,'charbuf=',charbuf
!       key = fstsui(lnkdiun(1),ni,nj,nk)
!        deallocate(buf)
!      enddo
 800    continue
        nomvar = 'TesT'
        typvar = 'hh'
        etiket = 'ShortComp'
        datyp=130
        ni = 22
        nj = 10
        nk = 1
        ier = FSTECR_H(ref_short, buf, -14, iun_out, dateo, deet, npas, ni, nj,&
                     nk, ip1, ip2, ip3, typvar, nomvar, etiket, grtyp, ig1, ig2,&
                     ig3, ig4, datyp, rewrite_flag)
        if (ier < 0) then
          print *,'Debug fstecr_s errno',ier
!          exit
        endif
        ier = FSTLIR_H(array_of_short,iun_out,ni,nj,nk,datev,etiket,ip1,ip2,ip3,typvar,nomvar)
        if (ier < 0) then
          print *,'Debug fstlir_s errno',ier
        endif
        do j = 1,10
        do i = 1,22
          if (array_of_short(i,j) .ne. ref_short(i,j)) then
            write(6,77) i,j,ref_short(i,j),i,j,array_of_short(i,j)
  77        format('>>> ref_short[',i2.2,',',i2.2,']=',z4.4,' array_of_short[',i2.2,',',i2.2,']=',z4.4)
          endif
        enddo
        enddo
        
        nomvar = 'TesT'
        typvar = 'bb'
        etiket = 'ByteComp'
        datyp=130
        ni = 22
        ier = FSTECR_B(ref_byte, buf, -8, iun_out, dateo, deet, npas, ni, nj,&
                     nk, ip1, ip2, ip3, typvar, nomvar, etiket, grtyp, ig1, ig2,&
                     ig3, ig4, datyp, rewrite_flag)
        if (ier < 0) then
          print *,'Debug fstecr_s errno',ier
!          exit
        endif
        ier = FSTLIR_B(array_of_byte,iun_out,ni,nj,nk,datev,etiket,ip1,ip2,ip3,typvar,nomvar)
        if (ier < 0) then
          print *,'Debug fstlir_s errno',ier
        endif
        do j =1,10
        do i = 1,22
          if (array_of_byte(i,j) .ne. ref_byte(i,j)) then
            write(6,88) i,j,ref_byte(i,1),i,j,array_of_byte(i,1)
  88        format('>>> ref_byte[',i2.2,',',i2.2,']=',z4.4,' array_of_byte[',i2.2,',',i2.2,']=',z4.4)
          endif
        enddo
        enddo

        nomvar = 'TesT'
        typvar = 'ii'
        etiket = 'CompInteger'
        datyp=130
        ni = 22
        nj = 10
        nk = 1
        ier = FSTECR(ref_int, buf, -16, iun_out, dateo, deet, npas, ni, nj,&
                     nk, ip1, ip2, ip3, typvar, nomvar, etiket, grtyp, ig1, ig2,&
                     ig3, ig4, datyp, rewrite_flag)
        if (ier < 0) then
          print *,'Debug fstecr_s errno',ier
!          exit
        endif
        ier = FSTLIR(array_of_int,iun_out,ni,nj,nk,datev,etiket,ip1,ip2,ip3,typvar,nomvar)
        if (ier < 0) then
          print *,'Debug fstlir_s errno',ier
        endif
        do i = 1,22
         do j = 1,10
          if (array_of_int(i,j) .ne. ref_int(i,j)) then
            write(6,99) i,j,ref_int(i,j),i,j,array_of_int(i,j)
  99        format('>>> ref_int[',i2.2,',',i2.2,']=',z4.4,' array_of_int[',i2.2,',',i2.2']=',z4.4)
          endif
         enddo
        enddo
        
      ier = fstvoi(iun_out,'RND')
!      ier = fstfrm(iun_in)
      ier = fstfrm(iun_out)

      stop
      end
