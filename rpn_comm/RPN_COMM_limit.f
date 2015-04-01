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
      integer function RPN_COMM_limit(my_id, npe, gmin, gmax, lmini,
     &     lmaxi,count, depl)
      implicit none
*
*     Fonction qui fait le partitionnement du domaine global
*
      integer, intent(IN) ::  my_id, npe, gmin, gmax
      integer, intent(OUT) :: lmini,lmaxi
      integer, intent(OUT) :: count(npe),depl(npe)
      logical alongx

      integer gtot
      integer val1, val2, i

      RPN_COMM_limit = -1

      gtot = gmax - gmin + 1
      val1 = (gtot + npe - 1)/npe
      val2 = gtot - (npe-1)*val1
      if (val2 .lt. 0)  then
         print *, 'RPN_COMM_limit: invalid decomposition'
         print *, 'Nb of elements =', gtot
         print *, 'Nb of pe =', npe
         return
      end if
      count(1:npe-1) = val1
      count(npe) = val2
      do i= 1, npe
         depl(i) = (i-1)*val1
      end do 
      lmini = gmin + depl(my_id+1)
      lmaxi = min(gmax,lmini+count(my_id+1)-1)
      
      RPN_COMM_limit = 0
      return
      
      end
