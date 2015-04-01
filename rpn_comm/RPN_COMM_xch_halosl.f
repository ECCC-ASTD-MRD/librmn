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

      SUBROUTINE RPN_COMM_xch_halosl(g,minx,maxx,miny,maxy,
     %             ni,nj,nk,halox,haloy,periodx,periody,
     %             gni,npol_row,nimax)
      use rpn_comm
      implicit none
      include 'mpif.h'
*     
*     exchange a halo with neighbours, semi-lagrangien fashion
*
      integer minx,maxx,miny,maxy,ni,nj,nk,halox,haloy
      integer gni,npol_row
      logical periodx,periody
      integer g(minx:maxx,miny:maxy,nk)
      integer nimax
      integer tempo(nimax,miny:maxy,nk,0:1)

      integer i,j,k, proc, nijk
      integer gmin, gmax
      integer count(pe_nx + pe_ny)
      integer depl(pe_nx + pe_ny)
      

      integer status(MPI_STATUS_SIZE)
      integer sendtag, recvtag, sendpe, recvpe, sendi, recvi
      integer procx, ierr, RPN_COMM_limit


      ierr = RPN_COMM_limit(pe_mex, pe_nx, 1, gni, gmin, gmax,
     &                    count, depl)
      if(pe_nx.eq.1) goto 100

! We put our piece in place
      if(pe_mex.gt.0) then
         do k=1,nk
            do j=miny,maxy
               do i=1,ni
                  g(i+pe_mex*nimax,j,k) = g(i,j,k)
               enddo
            enddo
         enddo
      endif

! We put our piece in temp buffer

      do k=1,nk
         do j=miny,maxy
            do i=1,ni
               tempo(i,j,k,0) = g(i+pe_mex*nimax,j,k)
            enddo
         enddo
      enddo
! Musical chair...
      sendi = 0
      recvi = 1
      sendpe = pe_id(pe_mex-1,pe_mey)
      recvpe = pe_id(pe_mex+1,pe_mey)
      procx = mod(pe_mex + 1, pe_nx)
      nijk=nimax*(maxy-miny+1)*nk
      sendtag= pe_me
      recvtag= recvpe

      do proc = 1,pe_nx-1 
*	 call tmg_start(93,'COMM SEMI-LAG')
         call mpi_sendrecv(tempo(1,miny,1,sendi),nijk,MPI_integer, 
     $                     sendpe,sendtag,
     $                     tempo(1,miny,1,recvi),nijk,MPI_integer,
     $                     recvpe,recvtag, PE_DEFCOMM,status,ierr)
*	 call tmg_stop(93)
         do k=1,nk
            do j=miny,maxy
               do i=1,count(procx+1)
                  g(i+procx*nimax,j,k)=tempo(i,j,k,recvi)
               enddo
            enddo
         enddo
         procx = mod(procx + 1, pe_nx)
         sendi = recvi
         recvi = mod(sendi+1,2)
      enddo
! E-W periodicity
 100  continue
      if(periodx) then
         do k=1,nk
            do j = miny,maxy
               do i=1,halox
                  g(-halox+i,j,k) = g(gni-halox+i,j,k) 
                  g(gni+i,j,k) = g(i,j,k)
               enddo
            enddo
         enddo
      endif
      return
      end
