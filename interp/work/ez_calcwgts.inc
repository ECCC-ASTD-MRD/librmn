   subroutine ez_calcwgts(wts, idxs, gdout_lat, gdout_lon, gdin_lat, gdin_lon, masque, ni_src, nj_src, ni_dst, nj_dst, num_wts)
   implicit none

   integer ni_src, nj_src, ni_dst, nj_dst, num_wts
   real :: gdin_lon(ni_src,nj_src),gdin_lat(ni_src,nj_src), dist, total_wgt
   real :: gdout_lon(ni_dst, nj_dst), gdout_lat(ni_dst, nj_dst)
   real, dimension(:,:,:) :: wts(ni_dst, nj_dst, num_wts)
   real, dimension(:) :: tmpwgts(num_wts)
   integer :: idxs(ni_dst, nj_dst, num_wts), masque(ni_dst,nj_dst)
   real rx, ry
   integer i,j,k,ii,jj,iz
   real lon_min, lon_max, lat_min, lat_max
   integer valmax, locmax
   integer i1, j1, k1, exception

   lon_min = minval(gdin_lon)
   lon_max = maxval(gdin_lon)
   lat_min = minval(gdin_lat)
   lat_max = maxval(gdin_lat)
   masque = 1
   where (gdout_lon < lon_min) masque = 0
   where (gdout_lon > lon_max) masque = 0
   where (gdout_lat < lat_min) masque = 0
   where (gdout_lat > lat_max) masque = 0

   wts = 1.0e30
   idxs = 0

   do j=1,nj_dst
      do i=1,ni_dst
         k = i + (j-1) * ni_dst
         if (masque(i,j) /= 0) then
            tmpwgts = 1.0e30
            locmax=1
            do jj=1,nj_src
               do ii=1,ni_src
                  rx = (gdout_lon(i,j)- gdin_lon(ii,jj))
                  ry = (gdout_lat(i,j)- gdin_lat(ii,jj))
                  if (abs(rx) > 180.0) then
                     rx = abs(abs(rx) - 360.0)
                  endif
                  if (abs(ry) < 3.0 .and. abs(rx) < 3.0) then
                     dist = rx*rx + ry*ry
!                      call ez_calcdist(dist, gdout_lat(i,j), gdout_lon(i,j), &
!                         gdin_lat(ii,jj), gdin_lon(ii,jj))
                     if (dist < tmpwgts(locmax)) then
                        tmpwgts(locmax) = dist
                        idxs(i,j,locmax) = ii + (jj-1)*ni_src
                        locmax = maxloc(tmpwgts, 1)
                     endif
                  endif
               enddo
            enddo
            do iz=1,num_wts
               wts(i,j,iz) = tmpwgts(iz)
            enddo
            if (wts(i,j,1) > 6371000.0) then
               wts(i,j,1:num_wts) = 1.0e30
               masque(i,j) = 0
            else
               total_wgt = 0.0
               do iz=1,num_wts
                  if (wts(i,j,iz) < 1.0e-10) then
                     print *, 'wts(i,j,iz) < 1.0e-10',i,j,iz,wts(i,j,iz)
                  endif
                  wts(i,j,iz) = max(wts(i,j,iz), 1.0e-10)
                  wts(i,j,iz) = 1.0 / wts(i,j,iz)
               enddo
               do iz=1,num_wts
                  total_wgt = total_wgt + wts(i,j,iz)
               enddo
               do iz=1,num_wts
                  wts(i,j,iz) = wts(i,j,iz) / total_wgt
               enddo
            endif
         endif
         if (i == (ni_dst-1) .and. j > (nj_dst-2)) then
            print *, i,j
            print *, (wts(i,j,iz),iz=1,num_wts)
         endif
         if (masque(i,j) == 1) then
            exception = 0
            do iz=1,num_wts
               k1 = idxs(i,j,iz)
               j1 = k1/ni_src + 1
               i1 = k1 - (j1-1)*ni_src
!              Recherche points a l'ouest
               if (gdout_lon(i,j) > gdin_lon(i1,j1)) then
                  exception = 1
                  exit
               endif
            enddo
            if (exception == 0) then
               masque(i,j) = 0
            endif
         endif

         if (masque(i,j) == 1) then
            exception = 0
            do iz=1,num_wts
               k1 = idxs(i,j,iz)
               j1 = k1/ni_src + 1
               i1 = k1 - (j1-1)*ni_src
!              Recherche points a l'est
               if (gdout_lon(i,j) < gdin_lon(i1,j1)) then
                  exception = 1
                  exit
               endif
            enddo
            if (exception == 0) then
               masque(i,j) = 0
            endif
         endif

         if (masque(i,j) == 1) then
            exception = 0
            do iz=1,num_wts
               k1 = idxs(i,j,iz)
               j1 = k1/ni_src + 1
               i1 = k1 - (j1-1)*ni_src
!              Recherche points au sud
               if (gdout_lat(i,j) > gdin_lat(i1,j1)) then
                  exception = 1
                  exit
               endif
            enddo
            if (exception == 0) then
               masque(i,j) = 0
            endif
         endif

         if (masque(i,j) == 1) then
            exception = 0
            do iz=1,num_wts
               k1 = idxs(i,j,iz)
               j1 = k1/ni_src + 1
               i1 = k1 - (j1-1)*ni_src
!              Recherche points au nord
               if (gdout_lat(i,j) < gdin_lat(i1,j1)) then
                  exception = 1
                  exit
               endif
            enddo
            if (exception == 0) then
               masque(i,j) = 0
            endif
         endif
      enddo
      print *, 'ezcalcwgts', j
   enddo

   return
   end subroutine ez_calcwgts
