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
subroutine bmf_areastart(g_i0,g_j0,g_in,g_jn,g_ni,g_nj,g_lon,g_lat,&
  l_i0,l_j0,l_ni,l_nj,path,prefix,date,hour,minu,sec)
  use bmf_area
  implicit none
!
  integer g_i0,g_j0,g_in,g_jn,l_i0,l_j0
  integer g_ni,g_nj,l_ni,l_nj,nk
  integer date, hour,minu,sec
  real*8 g_lon(g_ni),g_lat(g_nj)
  character(len=2) unit, prefix
  character* (*) path
!
!  integer npex,npey,me,mex,mey,sizex,sizey,ismaster 
!  integer mymaster, mybloc, myblocx,myblocy,blocme
  integer ierr, size, num, numlen, sendcount(2)
  integer npex,npey,me,mex,mey,sizexy(2),ismaster,mymaster
  integer mybloc,myblocx,myblocy,blocme,i,j, hhmmssdd
  integer boutblocs(2), boutblocr(2)
  integer prog_filename, fnom
  integer, allocatable, dimension(:,:) :: recvij
  character(len=12) domm
!
! Est-ce que la tuile/bloc comprend un bout a ecrire?
! 
  lin = l_i0+l_ni-1
  ljn = l_j0+l_nj-1

  istart = -1
  jstart = -1
  iend = -1
  jend = -1

  in_the_game=.false.
  bloc_in_the_game=.false.
  nowrite=.true.
!
! Avons-nous un bout a exporter?
!
  if((g_i0.le.lin).and.(g_in.ge.l_i0)) then
     if((g_j0.le.ljn).and.(g_jn.ge.l_j0)) then
        in_the_game=.true.
        istart = max(g_i0,l_i0)
        jstart = max(g_j0,l_j0)
        iend = min(g_in,lin)
        jend = min(g_jn,ljn)
     endif
  endif
!
!
  call RPN_COMM_allreduce(in_the_game,bloc_in_the_game,1,&
       'MPI_logical','MPI_LOR', 'BLOC',ierr)
!
  sendcount = 0
  if(in_the_game) then
     sendcount(1) = (iend-istart+1)
     sendcount(2) = (jend-jstart+1)
  endif
  if(bloc_in_the_game) then
     num=1
     numlen=-1
     unit=''
     call RPN_COMM_carac(npex,npey,me,mex,mey,sizexy(1),sizexy(2), &
          ismaster, mymaster, mybloc, myblocx,myblocy,blocme,domm)

     ierr=prog_filename(filename,prefix,date,hour,minu,sec, &
                     myblocx,myblocy,num,numlen,unit)

     call RPN_COMM_size('BLOC',size,ierr)
     if(allocated(recvcountv)) then
        write(*,*) 'BMF_areastart error: trying to restart without &
          &   a call to BMF_areastop, ABORT'
        stop
     else
        allocate(recvij(2,size))
        allocate(recvcountv(size))
     endif

     call RPN_COMM_gather(sendcount, 2, 'MPI_INTEGER', recvij, &
          2, 'MPI_INTEGER', 0, 'BLOC', ierr)
     do i=1,size
        recvcountv(i)= recvij(1,i)*recvij(2,i)*nk
     enddo
     boutblocs(1)=lin
     boutblocs(2)=ljn
     call RPN_COMM_reduce(boutblocs,boutblocr,2,'MPI_Integer','MPI_MAX', &
          0,'BLOC',ierr)
!
! The write sequence begin
!
     fileunit=0
     if(ismaster.eq.1) then
!
! Ici on utilise la propriete que le blocmaster est au sud-ouest
! de son bloc
!        
        bloci0=max(l_i0, g_i0)
        blocj0=max(l_j0, g_j0)
        blocin=min(g_in,boutblocr(1))
        blocjn=min(g_jn,boutblocr(2))
        write(*,*) 'BL',myblocx,myblocy, bloci0,blocin,blocj0,blocjn
        ierr=FNOM(fileunit,path//filename,'SEQ/UNF',0)
        hhmmssdd=1000000*hour+10000*minu+100*sec
        call bmf_write(fileunit,'#f#f',1,2,2,1,1,1,1,1,1,date,hhmmssdd &
             ,0,0,41,0,2,sizexy)
        call bmf_write(fileunit,'LONG',blocin-bloci0+1,bloci0,blocin, &
             1,1,1,1,1,1,date,hhmmssdd,0,0,81,0,blocin-bloci0+1, &
             g_lon(bloci0))
        call bmf_write(fileunit,'LATI',blocjn-blocj0+1,blocj0,blocjn, &
             1,1,1,1,1,1,date,hhmmssdd,0,0,81,0,blocjn-blocj0+1, &
             g_lat(blocj0))
     endif

  endif
end subroutine bmf_areastart

