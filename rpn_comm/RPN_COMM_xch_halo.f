!/* RMNLIB - Library of useful routines for C and FORTRAN programming
! * Copyright (C) 1975-2001  Division de Recherche en Prevision Numerique
! *                          Environnement Canada
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

	SUBROUTINE RPN_COMM_xch_halo(g,minx,maxx,miny,maxy,
     %             ni,nj,nk,halox,haloy,periodx,periody,
     %             gni,npol_row)
	use rpn_comm
	implicit none
!
!	exchange a halo with neighbours
!

	integer minx,maxx,miny,maxy,ni,nj,nk,halox,haloy
	integer gni,npol_row
	logical periodx,periody
!	integer *8 mem_time, exch_time, ewtime
	integer g(minx:maxx,miny:maxy,nk)
!
	
	include 'mpif.h'
!
!	integer *8 time_base,temp_time

!
	integer i, j, k, m
	integer nwds
	integer sendtag, gettag, ierr
	integer status(MPI_STATUS_SIZE)
	logical east,west,north,south
	integer eastpe,westpe,northpe,southpe
!
	integer globalni,polarrows, nilmax, jmin,jmax
        integer RPN_COMM_topo, mini,maxi,nil,ni0
!
	integer land_fill
	real r_land_fill
	equivalence(land_fill,r_land_fill)
!
	globalni=abs(gni)
	polarrows=npol_row

1	continue
	
	call RPN_COMM_tmg_in
	east=(bnd_east) .and. (.not.periodx)
	eastpe=pe_id(pe_mex+1,pe_mey)
	west=(bnd_west) .and. (.not.periodx)
	westpe=pe_id(pe_mex-1,pe_mey)
	north=(bnd_north) .and. (.not.periody)
	northpe=pe_id(pe_mex,pe_mey+1)
	south=(bnd_south) .and. (.not.periody)
	southpe=pe_id(pe_mex,pe_mey-1)

	jmin = 1
	jmax = nj 
	if(rpn_ew_ext_L) then
	   if(north) jmax = nj+haloy
	   if(south) jmin = 1-haloy
	endif
!
	if(pe_opcv(1) .ne. ' ') then !  fill halo option present
           
           r_land_fill=pe_oprv(1)
           if(pe_opcv(1) .eq. 'BAND') then
              if(iand(1,pe_opiv(1)) .ne. 0) then ! south band
                 do j=miny,0
                    do k=1,nk
                       do i=minx,maxx
                          g(i,j,k)=land_fill
                       enddo
                    enddo
                 enddo
              endif
	      if(iand(2,pe_opiv(1)) .ne. 0) then ! east band
                 do i=ni+1,maxx
                    do j=miny,maxy
                       do k=1,nk
                          g(i,j,k)=land_fill
                       enddo
                    enddo
                 enddo
	      endif
	      if(iand(4,pe_opiv(1)) .ne. 0) then ! north band
                 do j=nj+1,maxy
                    do k=1,nk
                       do i=minx,maxx
                          g(i,j,k)=land_fill
                       enddo
                    enddo
                 enddo
	      endif
	      if(iand(8,pe_opiv(1)) .ne. 0) then ! west band
                 do i=minx,0
                    do j=miny,maxy
                       do k=1,nk
                          g(i,j,k)=land_fill
                       enddo
                    enddo
                 enddo
	      endif
           endif
           if(pe_opcv(1) .eq. 'EDGE') then
              if(iand(1,pe_opiv(1)) .ne. 0) then ! south edge
                 do i=minx,0
                    do k=1,nk
                       g(i,1,k)=land_fill
                    enddo
                 enddo
                 do i=ni+1,maxx
                    do k=1,nk
                       g(i,1,k)=land_fill
                    enddo
                 enddo
	      endif
	      if(iand(2,pe_opiv(1)) .ne. 0) then ! east edge
                 do j=miny,0
                    do k=1,nk
                       g(ni,j,k)=land_fill
                    enddo
                 enddo
                 do j=nj+1,maxy
                    do k=1,nk
                       g(ni,j,k)=land_fill
                    enddo
                 enddo
	      endif
	      if(iand(4,pe_opiv(1)) .ne. 0) then ! north edge
                 do i=minx,0
                    do k=1,nk
                       g(i,nj,k)=land_fill
                    enddo
                 enddo
                 do i=ni+1,maxx
                    do k=1,nk
                       g(i,nj,k)=land_fill
                    enddo
                 enddo
	      endif
	      if(iand(8,pe_opiv(1)) .ne. 0) then ! west edge
                 do j=miny,0
                    do k=1,nk
                       g(1,j,k)=land_fill
                    enddo
                 enddo
                 do j=nj+1,maxy
                    do k=1,nk
                       g(1,j,k)=land_fill
                    enddo
                 enddo
	      endif
           endif
        endif

!
!       if no halo along x, bypass
!	call tmg_start(90,'RPN_COMM_haloew')
	if (halox .gt. 0) then
           if (.not.(min(pe_mey+1,pe_ny-pe_mey).le.polarrows)) then
              call RPN_COMM_xch_haloew(g,minx,maxx,miny,maxy,
     %             ni,jmin,jmax,nk,halox,haloy,periodx,periody)
              endif
        endif
!	call tmg_stop(90)
!	call tmg_start(91,'RPN_COMM_halons')
	if (haloy .gt. 0) then
              call RPN_COMM_xch_halons(g,minx,maxx,miny,maxy,
     %             ni,nj,nk,halox,haloy,periodx,periody)
        endif
!	call tmg_stop(91)
        if (min(pe_mey+1,pe_ny-pe_mey).le.polarrows) then
           ierr = RPN_COMM_topo(gni,mini,maxi,nil,nilmax,
     %          halox,ni0,.TRUE.,.FALSE.)
!	   call tmg_start(92,'RPN_COMM_xch_halosl')
           call RPN_COMM_xch_halosl(g,minx,maxx,miny,maxy,
     %             ni,nj,nk,halox,haloy,periodx,periody,
     %             gni,npol_row,nilmax)
!	   call tmg_stop(92)

        endif
	call RPN_COMM_tmg_out
	return


	entry xch_halo(g,minx,maxx,miny,maxy,
     %             ni,nj,nk,halox,haloy,periodx,periody)
	globalni=ni
	polarrows=0

	goto 1
	end
