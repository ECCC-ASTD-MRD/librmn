!> \file rmn_date.inc
!> Public interface to functions provided by moduledate.F90

interface
    subroutine incdati(idate1, idate2, nhours)
        use, intrinsic :: iso_fortran_env, only: real64
        implicit none
        integer, intent(out) :: idate1
        integer, intent(in) :: idate2
        real(kind = real64), intent(in) :: nhours
    end subroutine incdati

    subroutine incdatr(idate1, idate2, nhours)
        use, intrinsic :: iso_fortran_env, only: real64
        implicit none
        integer, intent(out) :: idate1
        integer, intent(in) :: idate2
        real(kind = real64), intent(in) :: nhours
    end subroutine incdatr

    subroutine difdati(idate1, idate2, nhours)
        use, intrinsic :: iso_fortran_env, only: real64
        implicit none
        integer, intent(in) :: idate1
        integer, intent(in) :: idate2
        real(kind = real64), intent(out) :: nhours
    end subroutine difdati

    subroutine difdatr(idate1, idate2, nhours)
        use, intrinsic :: iso_fortran_env, only: real64
        implicit none
        integer, intent(in) :: idate1
        integer, intent(in) :: idate2
        real(kind = real64), intent(out) :: nhours
    end subroutine difdatr

    integer function newdate(dat1, dat2, dat3, mode)
        implicit none
        integer, intent(inout) :: dat1, dat2(*), dat3
        integer, intent(in) :: mode
    end function newdate

    integer function idatmg2(idate)
        implicit none
        integer, intent(inout) :: idate(14)
    end function idatmg2

    subroutine datmgp2(idate)
        implicit none
        integer, intent(inout) :: idate(14)
    end subroutine datmgp2

    subroutine newdate_options(value, command)
        implicit none
        character(len = *), intent(inout) :: value
        character(len = *), intent(in) :: command
    end subroutine newdate_options

    subroutine get_calendar_status(noleapyears, ccclxdays)
        implicit none
        logical, intent(out) :: noleapyears, ccclxdays
    end subroutine get_calendar_status

    integer function calendar_adjust(tdate1, tdate2, true_date_mode, adding)
        implicit none
        integer, intent(inout) :: tdate1, tdate2
        character(len = 1), intent(in) :: true_date_mode
        logical, intent(in) :: adding
    end function calendar_adjust

    integer function ccclxdays_adjust(tdate1, tdate2, true_date_mode, adding)
        implicit none
        integer, intent(inout) :: tdate1, tdate2 ! input truedates
        character(len = 1), intent(in) :: true_date_mode ! (b)asic or (e)xtended truedates
        logical, intent(in) :: adding
    end function ccclxdays_adjust

    integer function leapyear_adjust(tdate1, tdate2, true_date_mode, adding)
        implicit none
        integer, intent(inout) :: tdate1, tdate2
        character(len = 1), intent(in) :: true_date_mode ! (b)asic or (e)xtended true dates
        logical, intent(in) :: adding
    end function leapyear_adjust

    subroutine ignore_leapyear()
        implicit none
    end subroutine ignore_leapyear

    subroutine accept_leapyear()
        implicit none
    end subroutine accept_leapyear

    subroutine get_leapyear_status(no_leap_year_status)
        implicit none
        logical, intent(out) :: no_leap_year_status
    end subroutine get_leapyear_status
end interface
