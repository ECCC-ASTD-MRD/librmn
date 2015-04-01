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

      integer function RPN_COMM_bloc(nblocx,nblocy)
	use rpn_comm
      implicit none
      integer nblocx, nblocy
*arguments
*     I	nblocx, nblocy: number of blocks on the subgrid in x-y direction
*     O RPN_COMM_bloc : error status (-1 if error, else 0)
!      include 'rpn_comm.h'
      include 'mpif.h'
*
      integer nblocs, ierr, indices(nblocx*nblocy)
      integer longx, longy, i,j,n
      integer mybloc
      
      nblocs=nblocx*nblocy
*     
*     if only one bloc, the initialization has been done in
*     RPN_COMM_init.f
*     
      RPN_COMM_bloc = -1
*
      if(nblocs.eq.1.and.(BLOC_SIZEX*BLOC_SIZEY.eq.1)) then
         RPN_COMM_bloc = 0
         return
      endif
*
*     test if the dimensions are suitable
*
      if(mod(pe_nx,nblocx).ne.0) then
         write(rpn_u,*) 'ERROR: mod(pe_nx,blocx).ne.0'
         return
      endif
      if(mod(pe_ny,nblocy).ne.0) then
         write(rpn_u,*) 'ERROR: mod(pe_ny,blocy).ne.0'
         return
      endif

      longx=pe_nx/nblocx
      longy=pe_ny/nblocy

      BLOC_master = 0
      BLOC_EXIST = .true.
      BLOC_SIZEX   = nblocx
      BLOC_SIZEY   = nblocy
      BLOC_myblocx = pe_mex / longx
      BLOC_myblocy = pe_mey / longy
      BLOC_mybloc  = BLOC_myblocx + nblocx*BLOC_myblocy
      BLOC_me      = pe_mex-BLOC_myblocx*longx +
     %     longx*(pe_mey-BLOC_myblocy*longy)
      BLOC_corner  = pe_medomm-(pe_mex-BLOC_myblocx*longx)
     %     -pe_nx*(pe_mey-BLOC_myblocy*longy)
      BLOC_comm_world = pe_indomm
      BLOC_comm_row   = pe_myrow
      BLOC_comm_col   = pe_mycol
      
      if(BLOC_corner==pe_medomm) BLOC_master=1
      
      pe_bloc = mpi_comm_null
      call MPI_COMM_SPLIT(BLOC_comm_world,
     %     BLOC_mybloc,BLOC_me,pe_bloc,ierr)
      
      n=1
      do j=1,nblocy
         do i=1,nblocx
!            indices(n) = pe_pe0+(i-1)*longx+pe_nx*longy*(j-1)
            indices(n) = (i-1)*longx+pe_nx*longy*(j-1)
            n=n+1
         enddo
      enddo
      call MPI_Group_incl(pe_gr_indomm, nblocs, indices,
     %     pe_gr_blocmaster, ierr)
      call MPI_Group_rank(pe_gr_bloc,mybloc,ierr)
      
      call MPI_Comm_create(BLOC_comm_world, pe_gr_blocmaster,
     %     pe_blocmaster, ierr)
      

      RPN_COMM_bloc = 0

        
      end

      
