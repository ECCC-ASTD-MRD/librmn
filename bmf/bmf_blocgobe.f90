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
function bmf_blocgobe(path,prefix,date,hour,& 
     min,sec) result(length)
  use bmf_mod
! now standard in f90...
  implicit none
  include 'mpif.h'
  integer, intent(IN) :: date,hour,min,sec
  character(len=2), intent(IN) :: prefix
  character* (*), intent(IN)::  path
!
! If PE is a blocmaster, read files of my bloc and send to my
! neighbours. Else, just grab the info...
!
  type sdata
     sequence
     character*4 :: nom
     integer :: padscrap,ni,istart,iend,nj,jstart,jend,nk,kstart,kend
     integer :: time1,time2
     integer :: hgrid,vgrid,dtyp,scat
     integer :: ndata
  end type sdata       
!
  integer :: npex,npey,me,mex,mey,sizex,sizey,ismaster,medomm
  integer :: mymaster, mybloc, myblocx,myblocy,blocme
  integer :: longx, longy, i,j,n, ntotal, size
  integer, allocatable, dimension(:) :: tabtotal , vsize
!
  integer buflen,wordlen,word(2), destpe,tag, ierr
  integer pend_req(10),PEND_STAT(MPI_STATUS_SIZE), length
  logical send_in_progress

  character(len=2048) ::  pe_file
  character(len=12) domm
  type(bmf_liste), pointer :: envoi  
  type(sdata), allocatable, dimension(:) :: sendata

  integer bmf_gobe
  external bmf_gobe

  send_in_progress = .false.

  call bmf_clear

  call RPN_COMM_carac(npex,npey,me,medomm,mex,mey,sizex,sizey,ismaster, &
       mymaster, mybloc, myblocx,myblocy,blocme,domm)
!  write(*,*) me,'ME',ismaster,medomm
!  call flush(6)
  longx=npex/sizex
  longy=npey/sizey
  wordlen=loc(word(2))-loc(word(1))
  if(ismaster.eq.1) then
     do j=0,longy-1
        do i=0,longx-1
!
! case i+j==0 will be handled later
!
!           write(*,*) me,mex,mey,i,j
           if((i+j)/=0) then
              call bmf_init
              call bmf_splitname ( pe_file,mex+i,mey+j,path,prefix, &
                   date,hour,min,sec )
              if(send_in_progress) then
                 call MPI_WAIT( pend_req(1), pend_stat,ierr )
              endif
              if(allocated(sendata)) deallocate(sendata)
              if(allocated(vsize)) deallocate(vsize)
!              write(*,*) me,'LA',pe_file(1:30),i,j,mex,mey,longx,longy
              length=bmf_gobe(pe_file)
              allocate(sendata(length),vsize(0:length), stat=ierr)
              if(ierr.ne.0) then
                 write(*,*) 'BMF_BLOCGOBE ERROR: memory allocation failed',ierr
                 stop
              endif
              buflen = length*(loc(sendata(1)%ndata)-loc(sendata(1)%nom)+ wordlen)/wordlen
              envoi => liste
              vsize(0)=0
             
              ntotal=0
              do n=1,length
                 sendata(n)%nom=envoi%bmf_champ%nom
                 sendata(n)%ni=envoi%bmf_champ%ni
                 sendata(n)%istart=envoi%bmf_champ%istart
                 sendata(n)%iend=envoi%bmf_champ%iend
                 sendata(n)%nj=envoi%bmf_champ%nj
                 sendata(n)%jstart=envoi%bmf_champ%jstart
                 sendata(n)%jend=envoi%bmf_champ%jend
                 sendata(n)%nk=envoi%bmf_champ%nk
                 sendata(n)%kstart=envoi%bmf_champ%kstart
                 sendata(n)%kend=envoi%bmf_champ%kend
                 sendata(n)%time1=envoi%bmf_champ%time1
                 sendata(n)%time2=envoi%bmf_champ%time2
                 sendata(n)%hgrid=envoi%bmf_champ%hgrid
                 sendata(n)%vgrid=envoi%bmf_champ%vgrid
                 sendata(n)%dtyp=envoi%bmf_champ%dtyp
                 sendata(n)%scat=envoi%bmf_champ%scat
                 sendata(n)%ndata=envoi%bmf_champ%ndata
                 size = sendata(n)%dtyp/10/wordlen
                 vsize(n)=size*sendata(n)%ni*sendata(n)%nj*sendata(n)%nk
                 ntotal=ntotal+vsize(n)
                 envoi=> envoi%champ_suivant
              enddo
!              write(*,*) me,'ICI4.1',me+i+j*npex,medomm+i+j*npex,'AA',i,j
!               write(*,*) me,'ICI4.1A',length, me+i+j*npex+111111
!              call flush(6)
              call RPN_COMM_isend(length,1,"mpi_integer", &
                   medomm+i+j*npex, me+i+j*npex+111111, "GRID", pend_req(3), ierr)
!               write(*,*) me,'ICI4.11',me+i+j*npex,medomm+i+j*npex,'AA',i,j
!              call flush(6)
             call RPN_COMM_isend(sendata(1),buflen,"mpi_integer", &
                   medomm+i+j*npex, me+i+j*npex, "GRID", pend_req(1), ierr)
!             write(*,*) me,'ICI4.111',me+i+j*npex,medomm+i+j*npex,'AA',i,j
!              call flush(6)
              if(send_in_progress) then
                  call MPI_WAIT( pend_req(2), pend_stat,ierr )
                 if(allocated(tabtotal)) deallocate(tabtotal)
              endif
              send_in_progress=.true.
              allocate(tabtotal(ntotal), stat=ierr)
              if(ierr.ne.0) then
                 write(*,*) 'BMF_BLOCGOBE ERROR: memory allocation failed',ntotal
                 call flush(6)
                 stop
              endif
              ntotal = 0
              envoi => liste
              do n=1,length
                 tabtotal(ntotal+1:ntotal+vsize(n))=reshape(envoi%bmf_champ%tableau,(/vsize(n)/))
                 envoi=> envoi%champ_suivant
                 ntotal=ntotal+vsize(n)
              enddo
!              write(*,*) me,'ICI4.13',me+i+j*npex,medomm+i+j*npex,'AA',i,j
!              call flush(6)
              call RPN_COMM_isend(tabtotal,ntotal,"mpi_integer", &
                   medomm+i+j*npex, me+i+j*npex+999999, "GRID", pend_req(2), ierr)              
!             write(*,*) me,'ICI4.12',me+i+j*npex,medomm+i+j*npex,'AA',i,j
!              call flush(6)
           endif
           call bmf_clear
        enddo
     enddo
!
! Case i,j = 0
!
     call bmf_init
     call bmf_splitname ( pe_file,mex,mey,path,prefix, &
          date,hour,min,sec )
     length = bmf_gobe(pe_file)
     envoi => liste
     do i=1,length
        envoi => envoi%champ_suivant
     enddo

     bmf_length = length
     if(send_in_progress) then
        call MPI_WAIT( pend_req(1), pend_stat,ierr )
        deallocate(sendata)
     endif

  else
     if(.not.bmf_liste_started) then
        nullify(liste)
        bmf_liste_started=.true.
        bmf_length=0
     endif
!     write(*,*) me,'ICI4.2',mymaster,me+111111
!     call flush(6)
     call RPN_COMM_recv( length, 1, "mpi_integer", mymaster,me+111111,"grid", pend_stat,ierr )
!     write(*,*) me,'ICI4.3',allocated(sendata),allocated(vsize)
!     call flush(6)
   
     bmf_length = length
     allocate(sendata(length),vsize(length),stat=ierr)
!     write(*,*) me,'ICI4.31'
!     call flush(6)
     if(ierr.ne.0) then
        write(*,*) 'BMF_BLOCGOBE ERROR: memory allocation failed'
        stop
     endif
     buflen = length*(loc(sendata(1)%ndata)-loc(sendata(1)%nom)+ wordlen)/wordlen
!     write(*,*) me,'ICI4.35',buflen,mymaster
     call RPN_COMM_recv( sendata, buflen, "mpi_integer", mymaster,me,"grid", pend_stat,ierr )
!     write(*,*) me,'ICI4.4'
!     call flush(6)
     ntotal=0
     do n=1,length
        allocate(envoi,stat=ierr)
        if(ierr.ne.0) then
           write(*,*) 'BMF_BLOCGOBE ERROR: memory allocation failed'
           stop
        endif
        envoi%bmf_champ%nom = sendata(n)%nom
        envoi%bmf_champ%ni = sendata(n)%ni
        envoi%bmf_champ%istart = sendata(n)%istart
        envoi%bmf_champ%iend =  sendata(n)%iend
        envoi%bmf_champ%nj = sendata(n)%nj
        envoi%bmf_champ%jstart = sendata(n)%jstart
        envoi%bmf_champ%jend = sendata(n)%jend
        envoi%bmf_champ%nk = sendata(n)%nk
        envoi%bmf_champ%kstart = sendata(n)%kstart
        envoi%bmf_champ%kend = sendata(n)%kend
        envoi%bmf_champ%time1 = sendata(n)%time1
        envoi%bmf_champ%time2 = sendata(n)%time2
        envoi%bmf_champ%hgrid = sendata(n)%hgrid
        envoi%bmf_champ%vgrid =  sendata(n)%vgrid
        envoi%bmf_champ%dtyp = sendata(n)%dtyp
        envoi%bmf_champ%scat = sendata(n)%scat
        envoi%bmf_champ%ndata =  sendata(n)%ndata
        size = sendata(n)%dtyp/10/wordlen
        vsize(n)=size*sendata(n)%ni*sendata(n)%nj*sendata(n)%nk
        envoi%champ_suivant => liste
        liste=>envoi
        ntotal=ntotal+vsize(n)
     enddo
     allocate(tabtotal(ntotal), stat=ierr)
     if(ierr.ne.0) then
        write(*,*) 'BMF_BLOCGOBE ERROR: memory allocation failed'
        stop
     endif
     call RPN_COMM_recv(tabtotal,ntotal, "mpi_integer", mymaster,me+999999,"grid", pend_stat,ierr )
     envoi => liste
!     write(*,*) me,'ICI4.5'
!     call flush(6)
     do n=length,1,-1
        size = sendata(n)%dtyp/10/wordlen
        allocate(envoi%bmf_champ%tableau(envoi%bmf_champ%ni*size,envoi%bmf_champ%nj,envoi%bmf_champ%nk))
        if(ierr.ne.0) then
           write(*,*) 'BMF_BLOCGOBE ERROR: memory allocation failed'
           stop
        endif
        envoi%bmf_champ%tableau = reshape(tabtotal(ntotal-vsize(n)+1:ntotal), &
             (/size*sendata(n)%ni,sendata(n)%nj,sendata(n)%nk/))
        ntotal= ntotal-vsize(n)
        envoi=>envoi%champ_suivant
     enddo
     deallocate(tabtotal)
     
  endif
!  write(*,*) 'FIN',me
!  call flush(6)

  return
end function bmf_blocgobe
