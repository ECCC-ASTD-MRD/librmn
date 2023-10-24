! Copyright (C) 2023  Environnement et Changement climatique Canada
!
! This is free software; you can redistribute it and/or
! modify it under the terms of the GNU Lesser General Public
! License as published by the Free Software Foundation,
! version 2.1 of the License.
!
! This software is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
! Lesser General Public License for more details.
!

!> \file rmn_fst_record.F90
!> Encapsulation of FST record information into a derived type 

module rmn_fst_record
    use rmn_common
    implicit none
    private

    type, public :: fst_record
        private
        type(C_PTR) :: p = C_NULL_PTR
    end type fst_record
end module rmn_fst_record
