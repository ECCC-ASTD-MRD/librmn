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


!> Interpolation bi-cubique de points a partir d'une grille gaussienne.
subroutine ez_gggdint_nw(zo,px,py,npts,ay,z,i1,i2,j1,j2)
    use rmn_common
    implicit none


    integer npts, i1, i2, j1, j2
    real zo(npts),px(npts),py(npts)
    real ay(j1:j2),cy(6)
    real z(i1:i2,j1:j2)

    !  npts   : nombre de points a interpoler
    !  i1:i2  : dimension de la grille source selon x
    !  j1:j2  : dimension de la grille source selon y
    !  zo     : vecteur de sortie contenant les valeurs interpolees
    !  px     : vecteur contenant la position x des points que l'on
    !           veut interpoler
    !  py     : vecteur contenant la position y des points que l'on
    !           veut interpoler
    !  ay     : vecteur contenant la pos. des points sur l'axe des Y.
    !  cy     : vecteur contenant une table de differences selon Y.
    !  z      : valeurs de la grille source.


    !  *   *   *   *
    !
    !  *   *   *   *
    !        #        .eq.>   pt (x,y)
    !  *  (=)  *   *  .eq.> = pt (i, j)
    !
    !  *   *   *   *

    integer        :: i, j, n
    real(kind = real64) :: y,y1,y2,y3,y4
    real(kind = real64) :: y11, y12, y13, y14
    real(kind = real64) :: ay1, ay2, ay3, ay4
    real(kind = real64) :: dx

    do n=1,npts
        i = min(i2-2,max(i1+1,ifix(px(n))))
        j = min(j2-2,max(j1+1,ifix(py(n))))

        dx = px(n) - i

        y1=cubic(dble(z(i-1,j-1)),dble(z(i  ,j-1)),dble(z(i+1,j-1)),dble(z(i+2,j-1)),dx)
        y2=cubic(dble(z(i-1,j  )),dble(z(i  ,j  )),dble(z(i+1,j  )),dble(z(i+2,j  )),dx)
        y3=cubic(dble(z(i-1,j+1)),dble(z(i  ,j+1)),dble(z(i+1,j+1)),dble(z(i+2,j+1)),dx)
        y4=cubic(dble(z(i-1,j+2)),dble(z(i  ,j+2)),dble(z(i+1,j+2)),dble(z(i+2,j+2)),dx)

        y = ay(j) + (ay(j+1)-ay(j))*(py(n)-j)

        ! interpolation finale selon y

        ay1=ay(j-1)
        ay2=ay(j)
        ay3=ay(j+1)
        ay4=ay(j+2)

        cy(1) = 1.0 / (ay2 - ay1)
        cy(2) = 1.0 / (ay3 - ay1)
        cy(3) = 1.0 / (ay3 - ay2)
        cy(4) = 1.0 / (ay4 - ay1)
        cy(5) = 1.0 / (ay4 - ay2)
        cy(6) = 1.0 / (ay4 - ay3)

        y11 = y1
        y12 = fa2(dble(cy(1)),y1,y2)
        y13 = fa3(dble(cy(1)),dble(cy(2)),dble(cy(3)),y1,y2,y3)
        y14 = fa4(dble(cy(1)),dble(cy(2)),dble(cy(3)),dble(cy(4)), dble(cy(5)),dble(cy(6)),y1,y2,y3,y4)
        zo(n) = fa(y11,y12,y13,y14,y,ay1,ay2,ay3)
    enddo

    return
contains
#include "cubic8.cdk"
#include "fa8.cdk"
end subroutine ez_gggdint_nw
