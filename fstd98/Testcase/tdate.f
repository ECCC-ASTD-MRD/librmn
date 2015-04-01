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
	program tdate
	integer date,datev,datenew,deet,npas

	deet = 2
	npas = 900
	 DATE = 052289003
         CALL INCDAT(DATEV, DATE, (DEET*NPAS + 1800)/3600)
*        CALL INCDAT(DATEV, DATE, 24)
	print *,'DATE, DATEV = ',DATE, DATEV
	call newdate(DATEV,datenew,nn,-3)
	print *,'datenew = ',datenew
	stop
	end

