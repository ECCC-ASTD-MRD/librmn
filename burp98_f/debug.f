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
      program testit
      call ttt(20000210)
      stop
      end

      subroutine ttt(date)
      integer date,annee,aa,mm,jj
      
      print *,' en entrant date=',date
      if (date .gt. 999999) then
         annee = date/10000
         AA = mod((date/10000),100)
         MM = (((annee - 1900) /100) * 12) + mod((date/100),100)
         JJ = mod(date,100)
         date = (AA * 10000) + (MM * 100) + JJ
         print *,' annee=',annee,' aa=',aa,' mm=',mm,' jj=',jj
         print *,' date =',date
      endif
      return
      end
