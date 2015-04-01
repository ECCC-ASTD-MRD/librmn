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

	SUBROUTINE RPN_COMM_optn(op_type,op_ival,op_rval,op_cval)
	use rpn_comm
	implicit none
	character(len=*) op_type, op_cval
	integer op_ival
	real *4 op_rval
**
!        include 'rpn_comm.h'
        include 'mpif.h'
*
	integer i
*
	do i=1,MAX_OPTN
	  if(op_type .eq. pe_optn(i)) then
	    pe_opiv(i) = op_ival
	    pe_oprv(i) = op_rval
	    pe_opcv(i) = op_cval
	    return
	  endif
	enddo
*
	print *,'ERROR(RPN_COMM_optn) option ',op_type,' unknown'
*
	return
	end
