!!!s/p fststatm
!
      program fsttweak
      implicit none
!
!AUTHOR   Yves Chartier                      July 1993
! 
!REVISION
!
!LANGUAGE:  fortran 77
!
!OBJECT (fststatm)
!
!FILES
!     tape1: TSF file
!     tape10-49: RPN standard files 
!
!ARGUMENTS 
!
!IMPLICIT     
!
!MODULES
      external ccard,fstlnk      
      
      character*128 cle(45)
      character*128 def(45),val(45)
      
      data cle /'fstout.', 'nomvar', 'ip2', 'delta_t', 'etiksrt', 40*'fstin.'/
      data def /'void',    'void',   'void', 'void',   'void',    40*'void'/
      data val /'void',    'void',   'void', 'void',   'void',    40*'void'/
      

      integer fnom,ier,fstouv,fstopi,fstopc,iunout, delta_npas
      logical flag
      
      integer date,ip1,ip2,ip3
      character*12 etiket, etiket2
      character*4 nomvar
      character*2 typvar

      integer i,ipos,nf,level, delta_t
      integer ni,nj,nk
      external fstinf,memoirh,fstprm,fstluk,fstsui,fstopl, fstfrm, fclos
      integer fstprm,fstinf,fstsui,fstluk,fstopl, fstfrm, fclos
      character*1 grtyp
      
      integer key, date0, deet, npas, nbits, datyp 
      integer swa, lng, dltf, ubc, datev, iun, newdate, ip2srt, dateo, fstecr
      integer ig1, ig2, ig3, ig4, extra1, extra2, extra3
      logical rewrite_flag

      real, allocatable, dimension(:) :: buf
      real*8 rdt
      character*12 etiksrt

      integer lnkdiun(40)
      data lnkdiun /40 * 0 /
      
      rewrite_flag = .false.
      call ccard(cle,def,val, 40, ipos)
      
      level = 6
      flag = .false.
!     ier = fstopc('MSGLVL', 'ERRORS', flag)

      nf = 6
 33   if (val(nf).ne.def(nf).and.nf.le.46) then
         nf = nf +1
         goto 33
      endif
      
      iunout = 1
      ier = fnom(iunout, val(1), 'RND+R/W', 0)
          if (ier.lt. 0) then
            print *, '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
            print *, ' probleme d''ouverture avec le fichier ',val(1)
            print *, '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
            stop         
          
         endif
    ier = fstouv(iunout, 'RND')
     
      if (val(2).eq.'void') then
         nomvar = '    '
      else
        nomvar = val(2)(1:4)
      endif
      print *, 'nomvar :', nomvar
      
      if (val(3).eq.'void') then
        ip2 = -1
      else
        read(val(3), *) ip2
      endif
      print *, ip2
      
       if (val(4).eq.'void') then
          print *, 'On doit specifier deltaT'
          stop
       else
          read(val(4), *) delta_t
       endif
             
      if (val(5).eq.'void') then
          etiksrt = '            '
      else
          etiksrt = val(5)
      endif
       
      nf = nf -6
      do  i=1, nf
         ier = fnom(lnkdiun(i),val(i+5),'RND+OLD+R/O',0)
         if (ier.lt. 0) then
            print *, '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
            print *, ' probleme avec fichier ',val(i),' inexistant - ' 
            print *, '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
            stop
         endif
      enddo
      
      
      do i=1,nf
         ier = fstouv(lnkdiun(i), 'RND')
         if (ier.lt.0) then
            print *, '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
            print *, '! le fichier #',val(i+5), 'n''est pas standard random'
            print *, '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
            stop
         endif
      enddo
      datev = -1
      ip1   = -1
      ip2   = -1
      ip3   = -1
      etiket = '            '
      typvar = '  '

      call fstlnk(lnkdiun, nf)   
!******************************************************************************
      key = fstinf(lnkdiun(1), ni, nj, nk,  datev, etiket, ip1,ip2,ip3,typvar,nomvar)

 10   if (key.ge.0) then
         ier = fstprm(key, dateo, deet, npas, ni, nj, nk, nbits, &
             datyp, ip1, ip2, ip3, typvar, nomvar, etiket, grtyp, &
             ig1, ig2, ig3, ig4, swa, lng, dltf, ubc, &
             extra1, extra2, extra3)
         ip2srt = ip2 + delta_t
         delta_npas = int(3600*delta_t/deet)
         npas = npas + delta_npas
         rdt = delta_npas * deet / 3600.0
!         call incdatr(newdate, dateo, rdt)
         if (ip2srt.lt.0) then
            print *, 'Maudit ptobleme!'
         endif
         allocate(buf(ni*nj*nk))
         ier = fstluk(buf,key,ni,nj,nk)
         if (val(5).eq.'void') then
         etiksrt = etiket
         endif  
        ier = FSTECR(buf, buf, -nbits, iunout, dateo, deet, npas, ni, nj, &
              nk, ip1, ip2srt, ip3, typvar, nomvar, etiksrt, grtyp, ig1, ig2, ig3, ig4, datyp, rewrite_flag)
         
         key = fstsui(lnkdiun(1),ni,nj,nk)
         deallocate(buf)
         goto 10
      endif
!******************************************************************************      
      ier = fstfrm(iunout)
      ier = fclos(iunout)
      return 
      
      stop
      end


      
