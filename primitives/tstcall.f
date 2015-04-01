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
      program tstcall
      integer a(10)
      integer *8 argum(41)
      data a /10,20,30,40,50,60,70,80,90,100/

      external subr
      integer*8 adrsub, loc_sub

      adrsub = loc_sub(subr)
      write(*,99) 'Debug tstcall adrsub=',adrsub
 99   format(a,z16.16)

      do i = 1,41
         argum(i) = loc(a(mod(i,10)))
      enddo

      call rmtcall(adrsub,argum)

      call peek(loc(a(3)),1,ival)
      print *,'Debug ival = ',ival

      stop
      end

      subroutine subr(i1,i2,i3,i4,i5)
      print *,i1,i2,i3,i4,i5
      return
      end
