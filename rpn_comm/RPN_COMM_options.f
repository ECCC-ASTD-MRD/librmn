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

	integer function RPN_COMM_option_L(name_S,value_L)
	use rpn_comm
	implicit none
	character(len=*) name_S
	character(len=32) name_min
	logical value_L
*
	integer i
*
	RPN_COMM_option_L = 0
	call RPN_COMM_up2low(name_S,name_min)
	if(name_min(1:11).eq.'halo_ew_ext') then
	   rpn_ew_ext_L=value_L
	   return
	endif

	RPN_COMM_option_L = -1
	write(rpn_u,*) 'ERROR(RPN_COMM_option_L) option '
     &   , name_S, ' unknown'
*
	return
	end


	integer function RPN_COMM_option(name_S,value)
	use rpn_comm
	implicit none
	character(len=*) name_S
	character(len=32) name_min
	integer value
*
	integer i
*
	RPN_COMM_option = 0
	call RPN_COMM_up2low(name_S,name_min)
	if(name_min(1:6).eq.'stdout') then
	   rpn_u=value
	   return
	endif

	RPN_COMM_option = -1
	write(rpn_u,*) 'ERROR(RPN_COMM_option) option '
     &    , name_S, ' unknown'
*
	return
	end
