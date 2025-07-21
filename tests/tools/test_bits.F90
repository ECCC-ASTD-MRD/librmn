! Copyright (C) 2022  Environnement et Changement climatique Canada
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
! Author:
!     M. Valin,   Environnement et Changement climatique Canada, 2022
!
program test_bits
  use ISO_C_BINDING
  implicit none
  interface
    subroutine qqqexit(code) bind(C, name='exit')
      import :: C_INT32_T
      integer(C_INT32_T), intent(IN) :: code
    end subroutine
!     subroutine start_of_test(str) bind(C, name='StartOfTest')
!       import :: C_CHAR
!       character(len=C_CHAR), dimension(*) :: str
!     end subroutine
  end interface
! next define not necessary for GNU/aocc/LLVM/PGI Fortran compilers, necessary for Intel
#define IN_FORTRAN_CODE
#include <rmn/bits.h>
  integer(C_INT32_T) :: i32, p32, lz32, lnz32
  integer(C_INT64_T) :: i64, p64, lz64, lnz64
  real(C_FLOAT) :: r32
  real(C_DOUBLE) :: r64

  call start_of_test("Fortran extra bit operators"//achar(0))
  i32 = 31
  p32 = popcnt(i32) ; lz32 = lzcnt(i32) ; lnz32 = lnzcnt(i32)
  print 1,'i32 =', i32, ', popcnt(i32) =', popcnt(i32), ', lzcnt(i32) =', lzcnt(i32), ', lnzcnt(i32) =', lnzcnt(i32), ' expecting 5, 27, 0'
  if(p32 .ne. 5 .or. lz32 .ne. 27 .or. lnz32 .ne. 0) goto 9
  i64 = 63
  p64 = popcnt(i64) ; lz64 = lzcnt(i64) ; lnz64 = lnzcnt(i64)
  print 2,'i64 =', i64, ', popcnt(i64) =', popcnt(i64), ', lzcnt(i64) =', lzcnt(i64), ', lnzcnt(i64) =', lnzcnt(i64), ' expecting 6, 58, 0'
  if(p64 .ne. 6 .or. lz64 .ne. 58 .or. lnz64 .ne. 0) goto 9
  r32 = 1.5
  p32 = popcnt(transfer(r32,i32)) ; lz32 = lzcnt(transfer(r32,i32)) ; lnz32 = lnzcnt(transfer(r32,i32))
  print 1,'r32 =', r32, ', popcnt(r32) =', popcnt(transfer(r32,i32)), ', lzcnt(r32) =', lzcnt(transfer(r32,i32)), ', lnzcnt(r32) =', lnzcnt(transfer(r32,i32)), &
          ' expecting 8, 2, 0'
  if(p32 .ne. 8 .or. lz32 .ne. 2 .or. lnz32 .ne. 0) goto 9
  r64 = 1.5
  print 2,'r64 =', r64, ', popcnt(r64) =', popcnt(transfer(r64,i64)), ', lzcnt(r64) =', lzcnt(transfer(r64,i64)), ', lnzcnt(r64) =', lnzcnt(transfer(r64,i64)), &
          ' expecting 11, 2, 0'
  p64 = popcnt(transfer(r64,i64)) ; lz64 = lzcnt(transfer(r64,i64)) ; lnz64 = lnzcnt(transfer(r64,i64))
  if(p64 .ne. 11 .or. lz64 .ne. 2 .or. lnz64 .ne. 0) goto 9
  print 3,'i32/i64, r32/r64 =', i32, i64, r32, r64

  i32 = -i32
  p32 = popcnt(i32) ; lz32 = lzcnt(i32) ; lnz32 = lnzcnt(i32)
  print 1,'i32 =', i32, ', popcnt(i32) =', popcnt(i32), ', lzcnt(i32) =', lzcnt(i32), ', lnzcnt(i32) =', lnzcnt(i32), ' expecting 28, 0, 27'
  if(p32 .ne. 28 .or. lz32 .ne. 0 .or. lnz32 .ne. 27) goto 9
  i64 = -i64
  p64 = popcnt(i64) ; lz64 = lzcnt(i64) ; lnz64 = lnzcnt(i64)
  print 2,'i64 =', i64, ', popcnt(i64) =', popcnt(i64), ', lzcnt(i64) =', lzcnt(i64), ', lnzcnt(i64) =', lnzcnt(i64), ' expecting 59, 0, 58'
  if(p64 .ne. 59 .or. lz64 .ne. 0 .or. lnz64 .ne. 58) goto 9
  r32 = -r32
  p32 = popcnt(transfer(r32,i32)) ; lz32 = lzcnt(transfer(r32,i32)) ; lnz32 = lnzcnt(transfer(r32,i32))
  print 1,'r32 =', r32, ', popcnt(r32) =', popcnt(transfer(r32,i32)), ', lzcnt(r32) =', lzcnt(transfer(r32,i32)), ', lnzcnt(r32) =', lnzcnt(transfer(r32,i32)), &
          ' expecting 9, 0, 1'
  if(p32 .ne. 9 .or. lz32 .ne. 0 .or. lnz32 .ne. 1) goto 9
  r64 = -r64
  print 2,'r64 =', r64, ', popcnt(r64) =', popcnt(transfer(r64,i64)), ', lzcnt(r64) =', lzcnt(transfer(r64,i64)), ', lnzcnt(r64) =', lnzcnt(transfer(r64,i64)), &
          ' expecting 12, 0, 1'
  p64 = popcnt(transfer(r64,i64)) ; lz64 = lzcnt(transfer(r64,i64)) ; lnz64 = lnzcnt(transfer(r64,i64))
  if(p64 .ne. 12 .or. lz64 .ne. 0 .or. lnz64 .ne. 1) goto 9
  print 3,'i32/i64, r32/r64 =', i32, i64, r32, r64

  print *,'SUCCESS'
  stop

9 continue
  print *,'FAIL'
  call qqqexit(1)

1 format(A,Z16.8,A,i5,A,I5,A,I5,A)
2 format(A,Z16.16,A,i5,A,I5,A,I5,A)
3 format(A,I5,I5,F5.1,F5.1)
end program
