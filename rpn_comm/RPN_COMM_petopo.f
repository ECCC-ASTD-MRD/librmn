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

      integer function RPN_COMM_petopo(pex,pey)
      use rpn_comm
      implicit none
      include 'mpif.h'
      integer pex,pey

      integer count, ierr,i,j

      pe_nx=pex
      pe_ny=pey
      RPN_COMM_petopo = -1

      if(pex*pey .ne. pe_dommtot) then
        write(rpn_u,*) pe_me
        write(rpn_u,*) 
     &           'RPN_COMM_petopo: invalid distribution, pe_nx*pe_ny'
        write(rpn_u,*) '                 is not equal to pe_dommtot ',
     &            pex*pey,pe_dommtot
        return
      endif

      count = pe_pe0
      
      if(allocated(pe_id))      deallocate(pe_id)
      if(allocated(pe_xtab))    deallocate(pe_xtab)
      if(allocated(pe_ytab))    deallocate(pe_ytab)
      if(allocated(ord_tab))    deallocate(ord_tab)

      allocate(pe_id(-1:pe_nx,-1:pe_ny))
      allocate(pe_xtab(0:pe_tot),pe_ytab(0:pe_tot))
      allocate(ord_tab(0:pe_tot))

      do i=0,pe_tot
         pe_xtab(i)=-1
         pe_ytab(i)=-1
         ord_tab(i)=-1
      enddo
      ord_max=0
      do j=0,pe_ny-1
         do i=0,pe_nx-1
            pe_id(i,j)=count-pe_pe0
            pe_xtab(count)=i
            pe_ytab(count)=j
            ord_tab(ord_max)=pe_pe0+ord_max
            ord_max=ord_max+1
            count=count+1
         enddo
      enddo
      ord_max = ord_max -1
*     
*     fill processor topology matrix, including a border used
*     to find "neighbors" in case of periodic domain
*     
      do j=-1,pe_ny
         pe_id(-1,j) = pe_id(pe_nx-1,j)
         pe_id(pe_nx,j) = pe_id(0,j)
      enddo
      do i=-1,pe_nx
         pe_id(i,-1) = pe_id(i,pe_ny-1)
         pe_id(i,pe_ny) =  pe_id(i,0)
      enddo
      if(pe_me.eq.pe_pe0)then
         print *,'PE MATRIX :'
         do j=pe_ny,-1,-1
	    print 101,(pe_id(i,j),i=-1,pe_nx)
 101        format(30I4)
         enddo
         print *,'PE_xtab :'
         print 101,(pe_xtab(i),i=0,pe_tot-1)
         print *,'PE_ytab :'
         print 101,(pe_ytab(i),i=0,pe_tot-1)
         print *,'ordinals table'
         print 101,(ord_tab(i),i=0,ord_max)
      endif
*     
*     compute position of PE in the computational grid
*     a value of -1 for X or Y means "OUT OF GRID"
*     

      pe_mex=mod(pe_me-pe_pe0,pe_nx)
      pe_mey=(pe_me-pe_pe0)/pe_nx
      pe_extra=0
*     
*     are we on a boundary ?
*     
      bnd_south=pe_mey.eq.0
      bnd_north=pe_mey.eq.pe_ny-1
      bnd_west=pe_mex.eq.0
      bnd_east=pe_mex.eq.pe_nx-1    
*     
*     split communication domain into rows
*     
      pe_myrow=MPI_COMM_NULL
!
      call MPI_COMM_SPLIT(pe_indomm,pe_mey+1,pe_mex+1,pe_myrow,ierr)	     
!
      call MPI_COMM_rank(pe_myrow,i,ierr)
      pe_mycol=MPI_COMM_NULL
      call MPI_COMM_SPLIT(pe_indomm,pe_mex+1,pe_mey+1,pe_mycol,ierr)
      RPN_COMM_petopo = 0
      return
      end
      
