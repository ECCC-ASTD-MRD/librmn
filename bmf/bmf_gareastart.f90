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
subroutine bmf_gareastart(lonsize,latsize,path,prefix,date,hh,mm,ss)
  use bmf_garea
  implicit none
  integer lonsize,latsize,date,hh,mm,ss
  character(len=2) prefix
  character* (*) path

  character(len=2) unit
  integer blocx, blocy, num,numlen
  integer ierr
  

  integer prog_filename, fnom


  ierr=prog_filename(filename,prefix,date,hh,mm,ss, &
       blocx,blocy,num,numlen,unit)



end subroutine bmf_gareastart
