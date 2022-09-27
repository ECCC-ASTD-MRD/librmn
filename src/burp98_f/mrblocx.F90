! RMNLIB - Library of useful routines for C and FORTRAN programming
! Copyright (C) 1975-2001  Division de Recherche en Prevision Numerique
!                          Environnement Canada
!
! This library is free software; you can redistribute it and/or
! modify it under the terms of the GNU Lesser General Public
! License as published by the Free Software Foundation,
! version 2.1 of the License.
!
! This library is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
! Lesser General Public License for more details.
!
! You should have received a copy of the GNU Lesser General Public
! License along with this library; if not, write to the
! Free Software Foundation, Inc., 59 Temple Place - Suite 330,
! Boston, MA 02111-1307, USA.


!> Create search key for mrbloc from bknat, bktyp and bkstp
integer function mrblocx(buf, bfam, bdesc, bknat, bktyp, bkstp, blkno)
    implicit none

    integer :: buf(*)
    !> Familly of the sreached block
    integer :: bfam
    !> Description of the searched block
    integer :: bdesc
    !> Natural part of the btyp of the searched block
    integer :: bknat
    !> Type part of the searched block
    integer :: bktyp
    !> Sub-type part of the search block
    integer :: bkstp
    !> Block from where the search starts
    integer :: blkno

    ! POUR CHAQUE CLEF D'ENTREE, ON TRANSPOSE LA VALEUR DU BIT
    ! 28, 29 OU 30 RESPECTIVEMENT DANS INBTYP (CES BITS NE SONT ALLUMES
    ! QUE SI LES CLEFS D'ENTREE SON MISE A -1)

#include "masques.cdk"
#include "defi.cdk"
#include "bpl.cdk"
#include "burpopt.cdk"
#include "codes.cdk"
#include <ftnmacros.hf>

    external mrbloc
    integer :: mrbloc
    integer :: inbtyp

    ! Build key
    inbtyp = 0
    inbtyp = iand(bkstp, ior(sgbkstp, bkstpmsk))
    inbtyp = ior(inbtyp, ior(iand(sgbktyp, bktyp), LSHIFT(iand(bktyp, bktypmsk), bpbktyp)))
    inbtyp = ior(inbtyp, ior(iand(sgbknat, bknat), LSHIFT(iand(bknat, bknatmsk), bpbknat)))

    mrblocx = mrbloc(buf, bfam, bdesc, inbtyp, blkno)
end
