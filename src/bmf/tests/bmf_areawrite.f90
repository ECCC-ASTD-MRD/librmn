!/* RMNLIB - Library of useful routines for C and FORTRAN programming
! * Copyright (C) 1975-2005  Environnement Canada
! *
! * This library is free software; you can redistribute it and/or
! * modify it under the terms of the GNU Lesser General Public
! * License as published by the Free Software Foundation,
! * version 2.1 of the License.
! *
! * This library is distributed in the hope that it will be useful,
! * but WITHOUT ANY WARRANTY; without even the implied warranty of
! * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
! * Lesser General Public License for more details.
! *
! * You should have received a copy of the GNU Lesser General Public
! * License along with this library; if not, write to the
! * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
! * Boston, MA 02111-1307, USA.
! */
subroutine bmf_areawrite(field,name,minx,maxx,miny,maxy,nk,time1,time2,dtyp)
  use bmf_area
  implicit none
  integer minx,maxx,miny,maxy,nk,time1,time2,dtyp
  integer field((dtyp/10/4),minx:maxx,miny:maxy,nk)
  character(len=4) name

  integer, allocatable :: sendbuf(:)
  integer ndata,indata,jndata
  integer hgrid,vgrid,scat,size
  integer i,j,ii,ss,k

  size = dtyp/10/4
  indata=(iend-istart+1)
  jndata=(jend-jstart+1)
  ndata=indata*jndata*nk
  hgrid=0
  vgrid=0
  scat=0

  
  if(in_the_game) then
     allocate(sendbuf(ndata*nk))
     ii=0
     do k=1,nk
        do j=jstart,jend
           do i=istart,iend
              do ss = 1, size
                 ii=ii+1
                 sendbuf(ii)=field(ss,i,j,k)
              enddo
           enddo
        enddo
     enddo
     call bmf_write(fileunit,name,lin,istart,iend, &
          ljn,jstart,jend,nk,1,nk,time1,time2,hgrid, &
          vgrid,dtyp,scat,ndata,sendbuf)
     deallocate(sendbuf)
  endif
  
end subroutine bmf_areawrite
