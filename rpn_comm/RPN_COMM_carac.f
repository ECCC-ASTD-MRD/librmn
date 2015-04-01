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

      subroutine RPN_COMM_carac(npex,npey,me,medomm,mex,mey,sizex,sizey,
     %     ismaster, mymaster, mybloc, myblocx,myblocy,blocme,domname)
	use rpn_comm
      implicit none
      integer, intent(out) :: npex,npey,me,mex,mey,sizex,sizey,ismaster
      integer, intent(out) :: mymaster, mybloc, myblocx,myblocy,blocme
      integer, intent(out) :: medomm
      character(len=*), intent(out) :: domname
*arguments
*     I nblocx, nblocy: number of blocks on the subgrid in x-y direction
*     O RPN_COMM_bloc : error status (-1 if error, else 0)
!      include 'rpn_comm.h'
      include 'mpif.h'
*
      npex     = pe_nx
      npey     = pe_ny
      me       = pe_me
      medomm   = pe_medomm
      mex      = pe_mex
      mey      = pe_mey
      sizex    = BLOC_sizex
      sizey    = BLOC_sizey
      ismaster = BLOC_master
      mymaster = BLOC_corner
      mybloc   = BLOC_mybloc
      myblocx  = BLOC_myblocx
      myblocy  = BLOC_myblocy
      blocme   = BLOC_me
      domname  = pe_domains(domm_num)%nom
      return
      end
