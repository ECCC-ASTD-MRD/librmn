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
      program infoliste
      integer unf,key1
      integer date0,deet,npas,ni,nj,nk,nbits,datyp,ip1,ip2,ip3
      integer ig1,ig2,ig3,ig4,swa,lng,dltf,ubc
      integer extra1,extra2,extra3
      integer INFON
      parameter (nmax=5000)
      integer liste(nmax)
      integer fstinf,fstprm,fstinl
      external fstinf,fstprm,fstinl

      character *8 etiket
      character *2 nomvar
      character *1 grtyp, typvar

      unf = 10
      ier=fnom(unf,
     %  '/usr/local/env/armnlib/data/SAMPLES/fstd_files/sample_fstd98',
     %  'std+rnd+old+R/O',0)
      print *,'Debug fnom=',ier
      ier = fstouv(unf,'RND')
      key1 = fstinl (unf,ni1,nj1,nk1,-1,' ',-1,-1,-1,' ',' ',
     %                liste,INFON,nmax)
      print*,'Debug fstinl=', key1,' INFON=',INFON
      ier = fstfrm(10)

      stop
      end
