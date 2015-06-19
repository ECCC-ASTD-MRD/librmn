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
	subroutine small_alloc(iptr,nitems,ierr,icode)
	implicit none
	integer iptr,nitems,ierr,icode
	external qqexit
	integer very_big
	parameter (very_big=1000000)
	integer mg_token
	parameter (mg_token=252645135)
	integer isc
	common /yoyoyo/ isc(very_big)
	integer nw,icur,last_nw
	save icur, last_nw
	data icur /1/
	
#if !defined (OLD_32BIT_CODE)
	print *,'****************************************************'
	print *,'* ERROR: SMALL_ALLOC                               *'
	print *,'* This code is obsolete, will not work on a 64     *'
	print *,'* bit architecture and should not be used.         *'
	print *,'* allocate shoud be used instead                   *'
	print *,'* EXITING                                          *'
	print *,'****************************************************'
	call qqexit(44)
#else
	if (icur .ne. 1) then
	   if (isc(icur-1) .ne. mg_token) then
	      print *,
     %    'FATAL ERROR, small_alloc: forward sentinel marker destroyed'
	      print *,'icur-1=',icur-1,'  isc(icur-1)=',isc(icur-1)
	      stop
	   endif
	   if (isc(icur-last_nw-2) .ne. mg_token) then
	      print *,
     %   'FATAL ERROR, small_alloc: backward sentinel marker destroyed'
	      print *,'icur=',icur,' last_nw=',last_nw,
     %             '  isc(icur-last_nw-2)=',isc(icur-last_nw-2)
	      stop
	   endif
	endif
	nw=nitems
	if(icode.eq.8)nw=2*nw
	isc(icur) = mg_token
	iptr=loc(isc(icur+1))
	icur=icur+nw+2
	isc(icur-1) = mg_token
	last_nw = nw
	if(icur .gt. very_big) then
	   print *,'FATAL ERROR, small_alloc: not enough memory'
	   stop
	endif
	return
#endif
	end
