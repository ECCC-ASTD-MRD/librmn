!/* RMNLIB - Library of useful routines for C and FORTRAN programming
! * Copyright (C) 1975-2001  Division de Recherche en Prevision Numerique
! *                          Environnement Canada
! *
! * This library is free software; you can redistribute it and/or
! * modify it under the terms of the GNU Lesser General Public
! * License as published by the Free Software Foundation,
! * version 2.1 of the License.
! *
! * This library is distributed in the hope that it will be useful,
! * but WITHOUT ANY WARRANTY; without even the implied warranty of
! * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
! * Lesser General Public License for more details.
! *
! * You should have received a copy of the GNU Lesser General Public
! * License along with this library; if not, write to the
! * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
! * Boston, MA 02111-1307, USA.
! */
!**s/r ez_cartauv - compute the components of the winds in the rotated system of
!                coordinates from the winds in the rotated cartesian space
!
      subroutine ez_cartauv_turbo(u, v, uvcart, sinlat, sinlon, coslat, coslon, ni, nj)
      implicit none
      integer ni, nj 
      real    uvcart(3,ni*nj), u(ni,nj), v(ni,nj)
      real sinlon(ni,nj), sinlat(ni,nj), coslon(ni,nj), coslat(ni,nj)
!
!author michel roch - april 90
!
!arguments
!    out    u       - rotated component  u of the wind
!           v       - rotated component  v of the wind
!    in     xyz     - rotated winds in cartesian space
!           lon     - longitudes of the grid in the rotated system of coordinates
!           lat     - latitudes of the grid in the rotated system of coordinates
!           ni      - e-w dimension of the grid
!           nj      - n-s dimension of the grid
!
!*

      integer i, j, k 
      real*8    c, d

      do 20 j=1,nj
         do 10 i=1,ni
            u(i,j) = (uvcart(2,1)*coslon(i,j)) - (uvcart(1,1)*sinlon(i,j))
            c      = (uvcart(1,1)*coslon(i,j)) + (uvcart(2,1)*sinlon(i,j))
            d      = sqrt(c**2 + uvcart(3,1)**2 )
            v(i,j) = sign(d, (uvcart(3,1)*coslat(i,j))-(c*coslat(i,j)))
 10         continue
 20      continue

      return
      end 
