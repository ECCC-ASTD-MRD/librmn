! Test that the rdtsc()/rdtscp() cycle counters from src/primitives/cpu_type.c
! work when called from Fortran through the interface block in
! rmn/cpu_type.h (included with IN_FORTRAN_CODE defined).
!
! Only the raw cycle counts are exercised here. The seconds-returning
! functions (wall_clock_seconds, rdtsc_seconds, rdtscp_seconds) are
! deliberately NOT tested: they depend on the CPU frequency calibration in
! get_cpu_capabilities(), which mis-parses brand strings that do not end in
! "@ <freq>GHz" (e.g. "Intel(R) Xeon(R) 6767P"), and wall_clock_seconds has a
! separate Fortran argument-passing issue.
program test_rdtsc_fortran
    use, intrinsic :: iso_c_binding, only: c_int, c_int64_t, c_double
    implicit none

#define IN_FORTRAN_CODE
#include <rmn/cpu_type.h>

    integer(c_int64_t) :: t1, t2

    ! ------------------------------------------------------------------
    ! rdtsc(): monotonic counter
    ! ------------------------------------------------------------------
    t1 = rdtsc()
    t2 = rdtsc()
    if (t2 < t1) then
        print *, 'FAIL: rdtsc() is not monotonic: ', t1, ' -> ', t2
        error stop 1
    end if
    print *, 'rdtsc() overhead = ', t2 - t1, ' ticks'

    ! ------------------------------------------------------------------
    ! rdtscp(): monotonic counter
    ! ------------------------------------------------------------------
    t1 = rdtscp()
    t2 = rdtscp()
    if (t2 < t1) then
        print *, 'FAIL: rdtscp() is not monotonic: ', t1, ' -> ', t2
        error stop 1
    end if
    print *, 'rdtscp() overhead = ', t2 - t1, ' ticks'

    ! ------------------------------------------------------------------
    ! rdtsc(): a busy loop must advance the counter by a large amount
    ! ------------------------------------------------------------------
    t1 = rdtsc()
    call busy_loop()
    t2 = rdtsc()
    if (t2 <= t1 .or. (t2 - t1) <= 1000000) then
        print *, 'FAIL: rdtsc() did not advance enough over a busy loop: ', t2 - t1
        error stop 1
    end if
    print *, 'rdtsc() advanced ', t2 - t1, ' ticks over a busy loop'

    ! ------------------------------------------------------------------
    ! rdtscp(): a busy loop must advance the counter by a large amount
    ! ------------------------------------------------------------------
    t1 = rdtscp()
    call busy_loop()
    t2 = rdtscp()
    if (t2 <= t1 .or. (t2 - t1) <= 1000000) then
        print *, 'FAIL: rdtscp() did not advance enough over a busy loop: ', t2 - t1
        error stop 1
    end if
    print *, 'rdtscp() advanced ', t2 - t1, ' ticks over a busy loop'

    print *, 'PASS: rdtsc()/rdtscp() cycle counters work from Fortran'
end program test_rdtsc_fortran

! ~2e8 iterations of a serial floating-point recurrence. The serial
! dependency (each x depends on the previous x) plus -fp-model source
! (no reassociation) means the compiler cannot vectorize, collapse, or
! constant-fold the loop, so it does real work and the TSC advances.
! x is referenced through a never-taken branch so the loop is not
! eliminated as dead code.
subroutine busy_loop()
    use, intrinsic :: iso_c_binding, only: c_double
    implicit none
    integer :: i
    real(c_double) :: x
    x = 0.5d0
    do i = 1, 200000000
        x = x * 1.000000001d0 + 1.0d-12
    end do
    if (x < 0.0d0) print *, 'unreachable'
end subroutine busy_loop
