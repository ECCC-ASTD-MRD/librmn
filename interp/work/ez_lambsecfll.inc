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
       subroutine ez_lambsecfll(xi,xj,alat,elon,alat1,elon1,dx,elonv,alatan)
!$$$   subprogram  documentation  block
!
! subprogram:  w3fb11        lat/lon to lambert(i,j) for grib
!   prgmmr: stackpole        org: nmc42       date:88-11-28
!
! abstract: converts the coordinates of a location on earth given in
!   the natural coordinate system of latitude/longitude to a grid
!   coordinate system overlaid on a lambert conformal tangent cone
!   projection true at a given n or s latitude. w3fb11 is the reverse
!   of w3fb12. uses grib specification of the location of the grid
!
! program history log:
!   88-11-25  original author:  stackpole, w/nmc42
!
! usage:  call lambfll (xi,xj,alat,elon,alat1,elon1,dx,elonv,alatan)
!   input argument list:
!     alat     - latitude in degrees (negative in southern hemis)
!     elon     - east longitude in degrees, real*4
!     alat1    - latitude  of lower left point of grid (point (1,1))
!     elon1    - longitude of lower left point of grid (point (1,1))
!                all real*4
!     dx       - mesh length of grid in meters at tangent latitude
!     elonv    - the orientation of the grid.  i.e.,
!                the east longitude value of the vertical meridian
!                which is parallel to the y-axis (or columns of
!                of the grid) along which latitude increases as
!                the y-coordinate increases.  real*4
!                this is also the meridian (on the back side of the
!                tangent cone) along which the cut is made to lay
!                the cone flat.
!     alatan   - the latitude at which the lambert cone is tangent to
!                (touching) the spherical earth.
!                 set negative to indicate a
!                 southern hemisphere projection.
!
!   output argument list:
!     xi       - i coordinate of the point specified by alat, elon
!     xj       - j coordinate of the point; both real*4
!
!   remarks: formulae and notation loosely based on hoke, hayes,
!     and renninger's "map projections and grid systems...", march 1981
!     afgwc/tn-79/003
!
! attributes:
!   language: ibm vs fortran
!   machine:  nas
!
!$$$
!
         data  rerth /6.3712e+6/, pi/3.14159/
!
!        preliminary variables and redifinitions
!
!        h = 1 for northern hemisphere; = -1 for southern
!
         if(alatan.gt.0) then
           h = 1.
         else
           h = -1.
         endif
!
         radpd = pi/180.0
         rebydx = rerth/dx
         alatn1 = alatan * radpd
         an = h * sin(alatn1)
         cosltn = cos(alatn1)
!
!        make sure that input longitudes do not pass through
!        the cut zone (forbidden territory) of the flat map
!        as measured from the vertical (reference) longitude.
!
         elon1l = elon1
         if((elon1 - elonv).gt.180.)
     &     elon1l = elon1 - 360.
         if((elon1 - elonv).lt.(-180.))
     &     elon1l = elon1 + 360.
!
         elonl = elon
         if((elon  - elonv).gt.180.)
     &     elonl  = elon  - 360.
         if((elon - elonv).lt.(-180.))
     &     elonl = elon + 360.
!
         elonvr = elonv *radpd
!
!        radius to lower left hand (ll) corner
!
         ala1 =  alat1 * radpd
         rmll = rebydx * (((cosltn)**(1.-an))*(1.+an)**an) *
     &           (((cos(ala1))/(1.+h*sin(ala1)))**an)/an
!
!        use ll point info to locate pole point
!
         elo1 = elon1l * radpd
         arg = an * (elo1-elonvr)
         polei = 1. - h * rmll * sin(arg)
         polej = 1. + rmll * cos(arg)
!
!        radius to desired point and the i j too
!
         ala =  alat * radpd
         rm = rebydx * ((cosltn**(1.-an))*(1.+an)**an) *
     &           (((cos(ala))/(1.+h*sin(ala)))**an)/an
!
         elo = elonl * radpd
         arg = an*(elo-elonvr)
         xi = polei + h * rm * sin(arg)
         xj = polej - rm * cos(arg)
!
!        if coordinate less than 1
!        compensate for origin at (1,1)
!
         if(xi.lt.1.)  xi = xi - 1.
         if(xj.lt.1.)  xj = xj - 1.
!
      return
      end
