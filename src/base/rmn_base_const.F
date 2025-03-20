!> \file

!> Base constants
      module rmn_base_const
!         Care must be taken with the indentation and line continuation
!         in this file since it must be compatible both with fixed form
!         and free form Fortran code

!         Constants are specified with an excessive number of digits
!         When considering the achievable precision, but are kept that
!         way to maintain the same bit pattern

!         rdtodg = 180 / pie, dgtord = pie / 180

!> PI Mathematical constant
          real, parameter :: pie = 3.1415926535898
!> Radian to degree conversion factor
          real, parameter :: rdtodg = 57.295779513082
!> Degree to radian conversion factor
          real, parameter :: dgtord = 1.7453292519943e-2

           integer, parameter :: global = 0
           integer, parameter :: north = 1
           integer, parameter :: nord = 1
           integer, parameter :: south = 2
           integer, parameter :: sud = 2
      end module rmn_base_const
