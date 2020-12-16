!/* RMNLIB - Library of useful routines for C and FORTRAN programming
! * Copyright (C) 1975-2007  Environnement Canada
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
!object
!  A few fortran helpers to deal with logical fortran type in C
!author
!  Stephane Chamberland, aug 2007
!** end of rpn-doc sections


!> \brief Handler function call in C to store/retreive fortran logical
!> \deprecated { This function should no longer be used.  f_logical2int should be used instead, or even, better C compatible types from iso_c_binding }
subroutine ftn2c_logical2int(dest, src, n)
  implicit none

  integer :: n
  integer, dimension(n) :: dest
  logical, dimension(n) :: src

  dest = 0
  where(src) dest = 1
end subroutine ftn2c_logical2int


!> \brief Handler function call in C to store/retreive fortran logical
!> \deprecated { This function should no longer be used.  f_int2logical should be used instead, or even, better C compatible types from iso_c_binding }
subroutine ftn2c_int2logical(dest, src, n)
  implicit none

  integer :: n
  integer, dimension(n) :: src
  logical, dimension(n) :: dest

  dest = src .ne. 0
  return
end subroutine ftn2c_int2logical


!> \brief andler function call in C to store/retreive fortran logical
!> \deprecated { This function should no longer be used.  f_logical_move should be used instead, or even, better C compatible types from iso_c_binding }
subroutine ftn2c_logical_move(dest, src, n)
  implicit none

  integer :: n
  logical, dimension(n) :: dest
  logical, dimension(n) :: src

  dest = src
  return
end subroutine ftn2c_logical_move
