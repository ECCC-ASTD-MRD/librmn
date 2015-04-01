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

	SUBROUTINE RPN_COMM_transpose(
     %             za,min1,max1,n1g,n2,min3,max3,n3g,zb,type,size)
	use rpn_comm
	implicit none
	integer min1,max1,n1g,n2,min3,max3,n3g,type,size
	integer za(size,min1:max1,n2,n3g)
	integer zb(size,n2,min3:max3,n1g)
*
!	include 'rpn_comm.h'
	include 'mpif.h'
*
	integer n3partiel,n1partiel,npe,pecomm
*
	if(abs(type).eq.1) then   ! transpose along X
	  npe=pe_nx
	  pecomm=pe_myrow
	else                      ! transpose along Y
	  npe=pe_ny
	  pecomm=pe_mycol
	endif
	n3partiel=(n3g+npe-1)/npe
	n1partiel=(n1g+npe-1)/npe
*
*	check that min1>0, max1>=n1partiel
*	check that min3>0, max3>=n3partiel
*	check that size = 1 or 2 (integer/real, real*8)
*
	if(type.gt.0) then  ! forward transpose
*
*	  call tmg_start(96,'RPN_COMM_Xpose1')
	  call RPN_COMM_Xpose1(n3partiel,npe,pecomm,
     %       n1partiel,
     %       za,min1,max1,n1g,n2,min3,max3,n3g,zb,size)
*	  call tmg_stop(96)
*
	else ! backward transpose
*
*	  call tmg_start(98,'RPN_COMM_Xpose2')
	  call RPN_COMM_Xpose2(n3partiel,npe,pecomm,
     %       n1partiel,
     %       za,min1,max1,n1g,n2,min3,max3,n3g,zb,size)
*	  call tmg_stop(98)
*
	endif
*
	return
	end
