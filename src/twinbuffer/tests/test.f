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
      program tst
      implicit none

      integer NB

      parameter(NB=2000)
      
      external open_db_file,write_db_file,read_db_file,rewind_db_file
      integer  open_db_file,write_db_file,read_db_file,rewind_db_file

      integer fnom
      integer iun, ier, fd, i, erreur
      integer buffer, ibuffer
      POINTER (pbuffer, buffer(NB * 4))
      POINTER (ipbuffer, ibuffer(NB * 4))
            
      iun = 2
      
      ier = fnom(iun,'essai.dat','RND',0)
      if(ier .ne. 0) then
         print *,'erreur a l''association du fichier'
      endif
      
      call hpalloc(pbuffer,NB * 4,ier,0)
      call hpalloc(ipbuffer,NB * 4,ier,0)
      
      
      ier = open_db_file(iun)
      
      do i=1,1000
         buffer(i) = 0
      enddo
      

      do i=1,2000
         buffer(i) = i * 2 - 1
         ibuffer(i) = buffer(i)
      enddo
      
      ier = write_db_file(iun,buffer, 1980)
      print *,'ier1 = ',ier

      ier = rewind_db_file(iun)

      do i=1,1000
         buffer(i) = 0
      enddo

      ier = read_db_file(iun, buffer, 1959)
      print *,'ier2 = ',ier
      
      erreur = 0
      do i=1, 1959
         if (buffer(i) .ne. ibuffer(i)) then
            erreur = erreur + 1
            print *,'erreur i = ',i,' buffer ', buffer(i),
     %              ' derait etre = ',ibuffer(i)
         endif
      enddo
      if (erreur .gt. 0) then
         print *,'Nombre en erreur sur 1959 elements = ',erreur
      else
         print *,'Test reussi'
      endif

      
      call close_db_file(iun)
      
      call hpdeallc(ipbuffer,ier,0)
      call hpdeallc(pbuffer,ier,0)
      
      end

