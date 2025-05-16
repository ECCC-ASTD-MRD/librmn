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


!> \file


!> Convert bknat, bktyp, bkstp to/from btyp
integer function mrbtyp(bknat, bktyp, bkstp, btyp)
    use app
    use rmn_burp, only: erbtyp, bkstpmsk, bktypmsk, bpbktyp, bpbknat, bknatmsk
    implicit none

    !> Portion nature du btyp de bloc recherche
    integer, intent(inout) :: bknat
    !> Portion type du btyp de bloc recherche
    integer, intent(inout) :: bktyp
    !> Portion sous-type du btyp de bloc recherche
    integer, intent(inout) :: bkstp
    !> Composite key indicating the block type
    !> btyp = -1 - Convert bknat, bktyp, bkstp -> btyp (return value)
    !> btyp >= 0 - Convert btyp -> bknat, bktyp, bkstp
    integer, intent(in) :: btyp

    !> \return 0 on success, erbtyp if btyp < -1

    !     fonction servant a batir une clef de recherche btyp a
    !     partir de bknat, bktyp et bkstp ou a extraire
    !     bknat, bktyp et bkstp de btyp

    if (btyp < -1) then
        write(app_msg, *) 'mrbtyp: valeur de btyp invalide'
        call lib_log(app_libfst, app_warning, app_msg)
        mrbtyp = erbtyp
        return
    endif

    ! construire la clef btyp
    mrbtyp = 0
    if (btyp == -1) then
        mrbtyp = iand(bkstp, bkstpmsk)
        mrbtyp = ior(mrbtyp, lshift(iand(bktyp, bktypmsk), bpbktyp))
        mrbtyp = ior(mrbtyp, lshift(iand(bknat, bknatmsk), bpbknat))
    else
        bkstp  = iand(btyp, bkstpmsk)
        bktyp  = iand(rshift(btyp, bpbktyp), bktypmsk)
        bknat  = iand(rshift(btyp, bpbknat), bknatmsk)
    endif
end
