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

	integer function RPN_COMM_mype(Me,Me_x,Me_y)
*
	use rpn_comm
	implicit none
	integer Me,Me_x,Me_y
*arguments
*  O	Me	PE rank (number)
*  O	Me_x	X coordinate of PE in computational grid
*  O	Me_y	Y coordinate of PE in computational grid
**
!	include 'rpn_comm.h'
	include 'mpif.h'
*
	Me = pe_medomm
	Me_x = pe_mex
	Me_y = pe_mey
*
	RPN_COMM_mype=MPI_SUCCESS
*
	return
	end
