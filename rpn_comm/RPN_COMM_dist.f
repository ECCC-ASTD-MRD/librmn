*/* RMNLIB - Library of useful routines for C and FORTRAN programming
* * Copyright (C) 1975-2001  Division de Recherche en Prevision Numerique
* *                          Environnement Canada
* *
* * This library is free software; you can redistribute it and/or
* * modify it under the terms of the GNU Lesser General Public
* * License as published by the Free Software Foundation,
* * version 2.1 of the License.
* *
* * This library is distributed in the hope that it will be useful,
* * but WITHOUT ANY WARRANTY; without even the implied warranty of
* * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
* * Lesser General Public License for more details.
* *
* * You should have received a copy of the GNU Lesser General Public
* * License along with this library; if not, write to the
* * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
* * Boston, MA 02111-1307, USA.
* */

	subroutine RPN_COMM_dist(garr,gmini,gmaxi,gminj,
     %          gmaxj,nig,njg,nk,ghalox,ghaloy,size,
     %          larr,mini,maxi,minj,maxj,halox,haloy,
     %          periodx,periody,status)
	use rpn_comm
	implicit none
*
!	include 'rpn_comm.h'
	include 'mpif.h'
*
	integer ghalox,ghaloy,gmini,gmaxi,gmaxj,gminj
	integer nig,njg,size,mini,maxi,minj,maxj,nk,status
	integer garr(size,gmini:gmaxi,gminj:gmaxj,nk),halox,haloy
	real reel,lreel
	integer larr(size,mini:maxi,minj:maxj,nk)
	logical periodx,periody

	integer dimtemp(2),dt1,dt2,ierr
	
	dimtemp(1)=maxi-mini+1
	dimtemp(2)=maxj-minj+1
! should be pe_id(x,x)
	if(pe_tot.gt.1) then
	 call RPN_COMM_bcast(dimtemp,2,"MPI_INTEGER",0,"GRID",ierr)
	endif
	dt1=dimtemp(1)
	dt2=dimtemp(2)
	call RPN_COMM_dist2(garr,gmini,gmaxi,gminj,
     %          gmaxj,nig,njg,nk,ghalox,ghaloy,size,
     %          larr,mini,maxi,minj,maxj,halox,haloy,
     %          periodx,periody,status,dt1,dt2)
	return
	end

***S/R RPN_COMM_dist  Global distribution of data
	subroutine RPN_COMM_dist2(garr,gmini,gmaxi,gminj,
     %          gmaxj,nig,njg,nk,ghalox,ghaloy,size,
     %          larr,mini,maxi,minj,maxj,halox,haloy,
     %          periodx,periody,status,dt1,dt2)
	use rpn_comm
	implicit none
*
!	include 'rpn_comm.h'
	include 'mpif.h'
*
	integer ghalox,ghaloy,gmini,gmaxi,gmaxj,gminj
	integer nig,njg,size,mini,maxi,minj,maxj,nk,status
	integer garr(size,gmini:gmaxi,gminj:gmaxj,nk),halox,haloy
	real reel,lreel
	integer larr(size,mini:maxi,minj:maxj,nk)
	logical periodx,periody
	integer dt1,dt2
*
*arguments
*  I	garr	array containing data to distribute, USED ONLY by PE 0
*  I	nig,njg,nk
*		dimensions of garr
*  I	size	size of data elements (the size of an integer is 1)
*  O	larr	local array that will receive the part of the global
*		domain that belongs to the local PE
*  I	mini:maxi,minj:maxj
*		dimensions of larr
*  I	gmini:gmaxi,gminj:gmaxj
*		dimensions of garr
*  I    ghalox,ghaloy,halox,haloy
*               size of halos of garr and larr
*  I    periodx,periody
*               logical, periodicity along x and y axis.
*  O	status	status code upon exit
**
	integer MAX_PENDING
	parameter (MAX_PENDING=4)
	logical distribute,slot_free(0:MAX_PENDING-1)
	integer i,j,k,ierr,nslots,nwords,i0,j0,level,nil,njl,islot
	integer clientpe,isz,j1,client,jlocal,islot0
	integer ipe, jpe, istatus(MPI_STATUS_SIZE),target
	integer PEND_STAT(MPI_STATUS_SIZE)
	integer PEND_REQ(0:MAX_PENDING-1)
	integer, dimension(min(mini,gmini):max(gmaxi,maxi)) :: ilst
	integer, dimension(size,dt1,dt2,nk,0:max_pending-1) :: temp
	logical east,west,north,south,flag
	integer bxmin, bxmax, bymin,bymax
        integer ixmin,ixmax,iymin,iymax
	integer lmini,lmaxi,lminj,lmaxj
	integer count(pe_nx+pe_ny)
	integer depl(pe_nx+pe_ny)
	logical alongx

	integer rpn_comm_limit

	distribute = .true.
1	status=MPI_ERROR
	do  i=0,MAX_PENDING-1
	   slot_free(i)= .true.
	enddo

	alongx = .true.
	ierr =  RPN_COMM_limit(pe_mex, pe_nx, 1, nig , lmini,
     &     lmaxi,count, depl)

	nil=lmaxi-lmini+1

	alongx = .false.
	ierr =  RPN_COMM_limit(pe_mey, pe_ny, 1, njg , lminj,
     &    lmaxj,count, depl)

	njl=lmaxj-lminj+1

	if(pe_medomm .eq.0) then
*
*	PE # 0, get own stuff, distribute rest to others
*
	 j0=0
	 islot = 0

	 do jpe=0,pe_ny-1
	  i0=0
	  do ipe=0,pe_nx-1
*
*	get own part of global array
*
c       Computation of local bounds
	     bxmin=1-halox
	     bxmax=nil+halox
	     bymin=1-haloy
	     bymax=njl+haloy

	     east=(pe_xtab(pe_id(ipe,jpe)).eq.(pe_nx-1))
     &              .and.(.not.periodx)      
	     west=(pe_xtab(pe_id(ipe,jpe)).eq.0)
     &              .and. (.not.periodx)
	     north=(pe_ytab(pe_id(ipe,jpe)).eq.(pe_ny-1))
     &              .and.(.not.periody)      
	     south=(pe_ytab(pe_id(ipe,jpe)).eq.0)
     &             .and. (.not.periody)
	     if(north) bymax=njg-(pe_ny-1)*njl+haloy
	     if(east) bxmax=nig-(pe_nx-1)*nil+halox
	     if(east.and.(ghalox.lt.halox)) then
		bxmax=nig-(pe_nx-1)*nil+ghalox
	     endif
	     if(west.and.(ghalox.lt.halox)) then
		bxmin=1-ghalox
	     endif
	     if(north.and.(ghaloy.lt.haloy)) then
		bymax=njg-(pe_ny-1)*njl+ghaloy
	     endif
	     if(south.and.(ghaloy.lt.haloy)) then
		bymin=1-ghaloy
	     endif
	     if(pe_id(ipe,jpe) .eq. 0)then
	  	do j=bymin,bymax
	          do i=bxmin,bxmax
	            ilst(i)=i0+i
		    if(periodx) then
		       if(ilst(i).gt.nig) ilst(i)=ilst(i)-nig		
		       if(ilst(i).lt.1)   ilst(i)=ilst(i)+nig
		    endif
	          enddo
	          jlocal=j0+j
		  if(periody) then
	            if(jlocal.gt.njg) jlocal=jlocal-njg
		    if(jlocal.lt.1)   jlocal=jlocal+njg
		  endif
	          do isz=1,size
	          do k=1,nk
	          do i=bxmin,bxmax
	            larr(isz,i,j,k)=garr(isz,ilst(i),jlocal,k)
	          enddo
	          enddo
	          enddo
	        enddo		
*
*	distribute to others using a pipelined method
*

	      else
	        islot0=mod(islot,MAX_PENDING)
c		write(rpn_u,*) slot_free(islot0)
		if(.not.slot_free(islot0)) then 
c		   write(rpn_u,*) 'on attend',PEND_REQ(islot0)
	           call MPI_WAIT(
     %	              PEND_REQ(islot0),
     %	              PEND_STAT,ierr)
c		   write(rpn_u,*) 'ok',PEND_REQ(islot0)
        	endif
		slot_free(islot0)=.false.
	        do j=bymin,bymax
	          do i=bxmin,bxmax
	            ilst(i)=i0+i
		    if(periodx) then
	              if(ilst(i).gt.nig) ilst(i)=ilst(i)-nig
	              if(ilst(i).lt.1)   ilst(i)=ilst(i)+nig
		   endif
	          enddo
	          jlocal=j0+j
		  if(periody) then
	            if(jlocal.gt.njg) jlocal=jlocal-njg
	            if(jlocal.lt.1)   jlocal=jlocal+njg
	          endif
		  do isz=1,size
	          do k=1,nk
	          do i=bxmin,bxmax
	            temp(isz,i-bxmin+1,j-bymin+1,k,islot0)=
     .                      garr(isz,ilst(i),jlocal,k)
	          enddo
	          enddo
	          enddo
	        enddo
	   
	        call MPI_ISEND(temp(1,1,1,1,islot0),
     %	               size*nk*dt1*dt2,
     %	               MPI_INTEGER,pe_id(ipe,jpe),pe_id(ipe,jpe),
     %	               PE_DEFCOMM,
     %	               PEND_REQ(islot0),ierr)

                islot=islot+1
	      endif
	      i0=i0+nil
	    enddo
	    j0=j0+njl
	  enddo
*       On attend la fin de toutes les transmissions
	  do i=max(0,islot-MAX_PENDING-1),islot-1
	    call MPI_WAIT(PEND_REQ(mod(i,MAX_PENDING)),
     %	         PEND_STAT,ierr)
	  enddo
	else
*
*	NOT pe # 0, passive receive
*
	       east=(pe_mex.eq.(pe_nx-1)).and.(.not.periodx)      
	       west=(pe_mex.eq.0) .and. (.not.periodx)
	       north=(pe_mey.eq.(pe_ny-1)).and.(.not.periody)      
	       south=(pe_mey.eq.0) .and. (.not.periody)

	       bxmin=1-halox
	       bxmax=nil+halox
	       bymin=1-haloy
	       bymax=njl+haloy
c	       write(rpn_u,*) 'bornes',bxmin,bxmax,bymin,bymax
	       if(east.and.(ghalox.lt.halox)) then
		  bxmax=nil+ghalox
	       endif
	       if(west.and.(ghalox.lt.halox)) then
		  bxmin=1-ghalox
	       endif
	       if(north.and.(ghaloy.lt.haloy)) then
		  bymax=njl+ghaloy
	       endif
	       if(south.and.(ghaloy.lt.haloy)) then
		  bymin=1-ghaloy
	       endif
	  call MPI_RECV(temp(1,1,1,1,0)
     %                 ,size*dt1*dt2*nk,
     %	                MPI_INTEGER,0,
     %	                pe_medomm,PE_DEFCOMM,istatus,ierr)
	  do isz=1,size
	     do k=1,nk
	     do j=bymin,bymax
	     do i=bxmin,bxmax
		larr(isz,i,j,k)=temp(isz,i-bxmin+1,j-bymin+1,k,0)
	     enddo
	     enddo
	     enddo
	  enddo
	endif
	status=MPI_SUCCESS
	return
*
1111	status =  MPI_ERROR

	return
	end

