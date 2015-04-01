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

	integer function RPN_COMM_split(mex,nx,nxg,minx,maxx,nxl,nxlmax,
     %                   halox,nx0,fill)
	use rpn_comm
	implicit none
	integer nxg,minx,maxx,nxl,nxlmax,halox,nx0
	logical fill
*
*arguments
*  I	nxg	Global dimension (1D) of data
*  O	minx,maxx
*		dimensions for local array
**
*
	integer nx, mex,count(nx),depl(nx), nxn, ierr
	integer RPN_COMM_limit
*
	RPN_COMM_split = -1

	ierr = RPN_COMM_limit(mex, nx, 1, nxg, nx0,
     &     nxn,count, depl)
	if(ierr.ne.0) then
	   write(rpn_u,*) 'RPN_COMM_split: invalid distribution, ABORT'
	   return
	endif

	minx = 1 - halox
	nxl = nxn-nx0+1
	nxlmax = count(1)
	maxx = nxlmax + halox
	if(fill) maxx = maxx + 1 - mod(nxlmax,2)
*
	RPN_COMM_split = 0

	return
	end
