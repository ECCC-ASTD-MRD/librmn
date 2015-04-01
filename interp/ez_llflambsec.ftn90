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
      subroutine ez_llflambsec(alat,elon,xi,xj,alat1,elon1,dx,elonv,alatan,      ierr)
!$$$  subprogram  documentation  block
!
! subprogram:  llflamb        lambert(i,j) to lat/lon for grib
!   prgmmr: stackpole        org: nmc42       date:88-11-28
!
! abstract: converts the coordinates of a location on earth given in a
!   grid coordinate system overlaid on a lambert conformal tangent
!   cone projection true at a given n or s latitude to the
!   natural coordinate system of latitude/longitude
!   llflamb is the reverse of w3fb11.
!   uses grib specification of the location of the grid
!
! program history log:
!   88-11-25  original author:  stackpole, w/nmc42
!
! usage:  call llflamb(alat,elon,xi,xj,alat1,elon1,dx,elonv,alatan,ierr,
!                                   ierr)
!   input argument list:
!     xi       - i coordinate of the point  real*4
!     xj       - j coordinate of the point  real*4
!     alat1    - latitude  of lower left point of grid (point 1,1)
!                latitude <0 for southern hemisphere; real*4
!     elon1    - longitude of lower left point of grid (point 1,1)
!                  east longitude used throughout; real*4
!     dx       - mesh length of grid in meters at tangent latitude
!     elonv    - the orientation of the grid.  i.e.,
!                the east longitude value of the vertical meridian
!                which is parallel to the y-axis (or columns of
!                the grid) along which latitude increases as
!                the y-coordinate increases.  real*4
!                this is also the meridian (on the other side of the
!                tangent cone) along which the cut is made to lay
!                the cone flat.
!     alatan   - the latitude at which the lambert cone is tangent to
!                (touches or osculates) the spherical earth.
!                 set negative to indicate a
!                 southern hemisphere projection; real*4
!
!   output argument list:
!     alat     - latitude in degrees (negative in southern hemi.)
!     elon     - east longitude in degrees, real*4
!     ierr     - .eq. 0   if no problem
!                .ge. 1   if the requested xi,xj point is in the
!                         forbidden zone, i.e. off the lambert map
!                         in the open space where the cone is cut.
!                  if ierr.ge.1 then alat=999. and elon=999.
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
         logical newmap
         data  rerth /6.3712e+6/, pi/3.14159/, oldrml/99999./
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
         piby2 = pi/2.
         radpd = pi/180.0
         degprd = 1./radpd
         rebydx = rerth/dx
         alatn1 = alatan * radpd
         an = h * sin(alatn1)
         cosltn = cos(alatn1)
!
!        make sure that input longitude does not pass through
!        the cut zone (forbidden territory) of the flat map
!        as measured from the vertical (reference) longitude
!
         elon1l = elon1
         if((elon1-elonv).gt.180.)
     &     elon1l = elon1 - 360.
         if((elon1-elonv).lt.(-180.))
     &     elon1l = elon1 + 360.
!
         elonvr = elonv * radpd
!
!        radius to lower left hand (ll) corner
!
         ala1 =  alat1 * radpd
         rmll = rebydx * ((cosltn**(1.-an))*(1.+an)**an) *
     &           (((cos(ala1))/(1.+h*sin(ala1)))**an)/an
!
!        use rmll to test if map and grid unchanged from previous
!        call to this code.  thus avoid unneeded recomputations.
!
         if(rmll.eq.oldrml) then
           newmap = .false.
         else
           newmap = .true.
           oldrml = rmll
!
!          use ll point info to locate pole point
!
           elo1 = elon1l * radpd
           arg = an * (elo1-elonvr)
           polei = 1. - h * rmll * sin(arg)
           polej = 1. + rmll * cos(arg)
         endif
!
!        radius to the i,j point (in grid units)
!              yy reversed so positive is down
!
         xx = xi - polei
         yy = polej - xj
         r2 = xx**2 + yy**2
!
!        check that the requested i,j is not in the forbidden zone
!           yy must be positive up for this test
!
         theta = pi*(1.-an)
         beta = abs(atan2(xx,-yy))
         ierr = 0
         if(beta.le.theta) then
           ierr = 1
           alat = 999.
           elon = 999.
           if(.not.newmap)  return
         endif
!
!        now the magic formulae
!
         if(r2.eq.0) then
           alat = h * 90.
           elon = elonv
         else
!
!          first the longitude
!
           elon = elonv + degprd * atan2(h*xx,yy)/an
           elon = amod(elon+360., 360.)
!
!          now the latitude
!          recalculate the thing only if map is new since last time
!
           if(newmap) then
             aninv = 1./an
             aninv2 = aninv/2.
             thing = ((an/rebydx) ** aninv)/
     &         ((cosltn**((1.-an)*aninv))*(1.+ an))
           endif
           alat = h*(piby2 - 2.*atan(thing*(r2**aninv2)))*degprd
         endif
!
!        following to assure error values if first time thru
!         is off the map
!
         if(ierr.ne.0) then
           alat = 999.
           elon = 999.
           ierr = 2
         endif
         return
         end
