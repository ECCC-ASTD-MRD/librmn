   subroutine ez_avg_axay(zout, xx, yy, ni_dst, nj_dst, zin, ax_src, ay_src, ni_src, nj_src)
   implicit none

   integer udst
   integer ni_src, nj_src, ni_dst, nj_dst
   real, dimension(ni_src, nj_src) :: zin
   real, dimension(ni_src) :: ax_src
   real, dimension(nj_src) :: ay_src
   real, dimension(ni_dst, nj_dst) :: zout, xx, yy
   integer avg


   real, dimension(:), allocatable :: x, y, x_low, x_high, y_low, y_high, ax, ay

   integer i, j, nix, njx, nkx, imid, jmid
   integer lcl_avg

   real dxcore, dycore, dxcoarse, dycoarse
   real total_area, xtile_min, ytile_min, xtile_max, ytile_max, xfrac, yfrac, area
   integer ez_cherche,ii,jj
   integer istart, iend, jstart, jend, compression_code, usr_datyp

   allocate(ax(ni_dst), ay(nj_dst))
   allocate(x_low(ni_dst))
   allocate(y_low(nj_dst))
   allocate(x_high(ni_dst))
   allocate(y_high(nj_dst))
   allocate(x(ni_dst),y(nj_dst))

   x = xx(:,1)
   y = yy(1,:)

   do i=1,ni_dst-1
      ix = int(x(i))
      ax(i) = ax_src(ix) + fraction(x(i))*(ax_src(ix+1)-ax_src(ix))
   enddo
   ix = int(x(ni_dst))
   ax(ni_dst) = ax_src(ix) + (1.0-fraction(x(i)))*(ax_src(ix)-ax_src(ix-1))

   do j=1,nj_dst
      iy = int(y(i))
      ay(j) = ay_src(iy) + fraction(y(j))*(ay_src(iy+1)-ay_src(iy))
   enddo
   iy = int(y(nj_dst))
   ay(nj_dst) = ay_src(iy) + (1.0-fraction(y(j)))*(ay_src(iy)-ay_src(iy-1))

   if (x(1) > real(ni_src-1)) x(1) = 1.0
   x_low(1) = max(1.0,x(1)-0.5*(x(2)-x(1)))
   do i=2,ni_dst
      x_low(i) = 0.5*(x(i)+x(i-1))
   enddo

   y_low(1) = max(1.0,y(1)-0.5*(y(2)-y(1)))
   do j=2,nj_dst
      y_low(j) = 0.5*(y(j)+y(j-1))
   enddo

   x_high(ni_dst) = min(1.0*ni_src,x(ni_dst)+0.5*(x(ni_dst)+x(ni_dst-1)))
   do i=1,ni_dst-1
      x_high(i) = 0.5 * (x(i+1)+x(i))
   enddo

   y_high(nj_dst) = min(1.0*nj_src,y(nj_dst)+0.5*(y(nj_dst)+y(nj_dst-1)))
   do j=1,nj_dst-1
      y_high(j) = 0.5 * (y(j+1)+y(j))
   enddo


   do j=2,nj_dst-1
      jstart = int(y_low(j))
      jend   = nint(y_high(j))
      if ((0.5+real(jstart)) < y_low(j)) jstart = jstart + 1
!      if (real(jend) < y_high(j)) jend = jend + 1
      do i=2,ni_dst-1
         zout(i,j) = 0.0
         total_area = 0.0
         istart = int(x_low(i))
         iend   = nint(x_high(i))
         if ((0.5+real(istart)) < x_low(i)) istart = istart + 1
!         if (real(iend) < x_high(j)) iend = iend + 1
         do jj=jstart, jend
            ytile_min = real(jj)-0.5
            ytile_max = real(jj)+0.5
            yfrac = 1.0
            if (ytile_min < y_low(j)) then
               yfrac = ytile_max - y_low(j)
            endif
            if (ytile_max > y_high(j)) then
               yfrac = y_high(j) - ytile_min
            endif
            do ii=istart, iend
               xtile_min = real(ii)-0.5
               xtile_max = real(ii)+0.5
               xfrac = 1.0
               if (xtile_min < x_low(i)) then
                  xfrac = xtile_max - x_low(i)
               endif
               if (xtile_max > x_high(i)) then
                  xfrac = x_high(i) - xtile_min
               endif
               area =  xfrac*yfrac
               total_area = total_area + area
               zout(i,j) = zout(i,j) + zin(ii,jj) * area
            enddo
         enddo
         if (total_area /= 0.0) zout(i,j) = zout(i,j)/total_area
      enddo
   enddo

! Moyenne 1e rangee

  j = 1
  jstart = int(y_low(1))
  jend = nint(y_high(1))
  if (real(jstart) > y_low(1)) jstart = jstart - 1
  do i=2,ni_dst-1
      zout(i,j) = 0.0
      total_area = 0.0
      istart = int(x_low(i))
      iend   = nint(x_high(i))
      if ((0.5+real(istart)) < x_low(i)) istart = istart + 1
      if (real(iend) < x_high(i)) iend = iend + 1
      do jj=jstart, jend

         if (jj == 1) then
           ytile_min = 1.0
         else
           ytile_min = real(jj)-0.5
         endif

         ytile_max = real(jj)+0.5
         yfrac = ytile_max - ytile_min
         if (ytile_min < y_low(j)) then
            yfrac = ytile_max - y_low(j)
         endif
         if (ytile_max > y_high(j)) then
            yfrac = y_high(j) - ytile_min
         endif

         do ii=istart, iend
            xtile_min = real(ii)-0.5
            xtile_max = real(ii)+0.5
            xfrac = 1.0
            if (xtile_min < x_low(i)) then
               xfrac = xtile_max - x_low(i)
            endif
            if (xtile_max > x_high(i)) then
               xfrac = x_high(i) - xtile_min
            endif
            area =  xfrac*yfrac
            total_area = total_area + area
            zout(i,j) = zout(i,j) + zin(ii,jj) * area
         enddo
      enddo
         if (total_area /= 0.0) zout(i,j) = zout(i,j)/total_area
   enddo


! Moyenne rangee du haut

   j = nj_dst
   jstart = int(y(nj_dst))
   jend = nint(y_high(nj_dst))
   do i=2,ni_dst-1
      zout(i,j) = 0.0
      total_area = 0.0
      istart = int(x_low(i))
      if ((0.5+real(istart)) < x_low(i)) istart = istart + 1
      iend   = nint(x_high(i))
      do jj=jstart, jend
         if (jj == nj_src) then
            ytile_max = real(nj_src)
         else
            ytile_max = real(jj)+0.5
         endif
         ytile_min = real(jj)-0.5
         yfrac = ytile_max - ytile_min
         if (ytile_min < y_low(j)) then
            yfrac = ytile_max - y_low(j)
         endif
         if (ytile_max > y_high(j)) then
            yfrac = y_high(j) - ytile_min
         endif

         do ii=istart, iend
            xtile_min = real(ii)-0.5
            xtile_max = real(ii)+0.5
            xfrac = 1.0
            if (xtile_min < x_low(i)) then
               xfrac = xtile_max - x_low(i)
            endif
            if (xtile_max > x_high(i)) then
               xfrac = x_high(i) - xtile_min
            endif
            area =  xfrac*yfrac
            total_area = total_area + area
            zout(i,j) = zout(i,j) + zin(ii,jj) * area
         enddo
      enddo
         if (total_area /= 0.0) zout(i,j) = zout(i,j)/total_area
   enddo

! Moyenne 1e colonne

   do j=1,nj_dst
      jstart = int(y_low(j))
      jend   = nint(y_high(j))
      if (y_low(j) < 1.0) then
        jstart = 1
      endif
      if (y_high(j) > (1.0*nj_dst)) then
        jend = nj_src
      endif
      i=1
      zout(i,j) = 0.0
      total_area = 0.0
      istart = int(x_low(1))
      iend   = nint(x_high(1))
      do jj=jstart, jend
         if (jstart > 1) then
            ytile_min = real(jj)-0.5
         else
            ytile_min = 1.0
         endif
         if (jend < nj_src) then
            ytile_max = real(jj)+0.5
         else
           ytile_max = 1.0 * nj_src
         endif
         yfrac = ytile_max - ytile_min
         if (ytile_min < y_low(j)) then
            yfrac = ytile_max - y_low(j)
         endif
         if (ytile_max > y_high(j)) then
            yfrac = y_high(j) - ytile_min
         endif
         do ii=istart, iend
            if (ii == 1) then
               xtile_min = 1.0
            else
               xtile_min = real(ii)-0.5
            endif
            xtile_max = real(ii)+0.5
            xfrac = xtile_max - xtile_min
            if (xtile_max > x_high(i)) then
               xfrac = x_high(i) - xtile_min
            endif
            area =  xfrac*yfrac
            total_area = total_area + area
            zout(i,j) = zout(i,j) + zin(ii,jj) * area
         enddo

!          istart = int(x_high(ni_dst))
!          iend   = nint(x_high(ni_dst)+0.5*(x(ni_dst)-x(ni_dst-1))
!          if (iend > ni_src) iend = ni_src
!          do ii=istart, iend
!             xtile_min = real(ii)-0.5
!             xtile_max = real(ii)+0.5
!             xfrac = xtile_max - xtile_min
!             if (xtile_max > x_high(i)) then
!                xfrac = x_high(i) - xtile_min
!             endif
!             area =  xfrac*yfrac
!             total_area = total_area + area
!             zout(i,j) = zout(i,j) + zin(ii,jj) * area
!          enddo
      enddo
         if (total_area /= 0.0) zout(i,j) = zout(i,j)/total_area
   enddo

! Moyenne derniere colonne

   do j=1,nj_dst
      jstart = int(y_low(j))
      jend   = nint(y_high(j))
      if (j == 1) then
         jstart = 1
      endif
      if (j == nj_dst) then
         jend = nj_src
      endif
      i=ni_dst
      zout(i,j) = 0.0
      total_area = 0.0
      istart = int(x_low(ni_dst))
      iend   = nint(x_high(ni_dst))
      do jj=jstart, jend
         if (jstart > 1) then
            ytile_min = real(jj)-0.5
         else
            ytile_min = 1.0
         endif
         if (jend < nj_src) then
            ytile_max = real(jj)+0.5
         else
            ytile_max = real(nj_src)
         endif
         yfrac = ytile_max - ytile_min
         if (ytile_min < y_low(j)) then
            yfrac = ytile_max - y_low(j)
         endif
         if (ytile_max > y_high(j)) then
             yfrac = y_high(j) - ytile_min
         endif
         do ii=istart, iend
            xtile_min = real(ii)-0.5
            if (ii == ni_src) then
               xtile_max = real(ni_src)
            else
               xtile_max = real(ii)+0.5
            endif
            xfrac = xtile_max - xtile_min
            if (xtile_max > x_high(i)) then
               xfrac = x_high(i) - xtile_min
            endif
            area =  xfrac*yfrac
            total_area = total_area + area
            zout(i,j) = zout(i,j) + zin(ii,jj) * area
            enddo
         enddo
         if (total_area /= 0.0) zout(i,j) = zout(i,j)/total_area
   enddo

   deallocate(x_low, y_low, x_high, y_high, x, y)

   return
  end