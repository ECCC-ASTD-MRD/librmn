!/* RMNLIB - Library of useful routines for C and FORTRAN programming
! * Copyright (C) 1975-2005  Environnement Canada
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

module bmf_mod
    save
    type bmf_entree
        sequence
        character*4 :: nom
        integer :: padscrap, ni, istart, iend, nj, jstart, jend, nk, kstart, kend
        integer :: time1, time2
        integer :: hgrid, vgrid, dtyp, scat
        integer :: ndata
        integer, dimension(:,:,:), pointer :: tableau
    end type bmf_entree


    type bmf_liste
        sequence
        type(bmf_entree) :: bmf_champ
        type(bmf_liste), pointer :: champ_suivant
    end type bmf_liste

    integer :: bmf_intsize
    integer :: bmf_realsize

    integer, parameter :: bmf_real4 = 41
    integer, parameter :: bmf_real8 = 81
    integer, parameter :: bmf_integer4 = 40
    integer, parameter :: bmf_integer8 = 80
    integer, parameter :: bmf_complex8 = 82
    integer, parameter :: bmf_complex16 = 162
    integer, parameter :: bmf_character = 10

    integer bmf_length
    logical bmf_started
    logical bmf_liste_started
    integer bmf_errorlvl

    type(bmf_liste), pointer :: liste
end module bmf_mod
