!> \file rmn_date.inc
!> Public interface to functions provided by moduledate.F90

interface
    subroutine INCDATi(idate1, idate2, nhours)
        use rmn_common
        IMPLICIT NONE
        integer :: idate1, idate2
        real(kind = real64) :: nhours
    end subroutine INCDATi

    subroutine INCDATr(idate1, idate2, nhours)
        use rmn_common
        IMPLICIT NONE
        integer :: idate1, idate2
        real(kind = real64) :: nhours
    end subroutine INCDATr

    subroutine DIFDATi(idate1, idate2, nhours)
        use rmn_common
        IMPLICIT NONE
        integer :: idate1, idate2
        real(kind = real64) :: nhours
    end subroutine DIFDATi

    subroutine DIFDATr(idate1, idate2, nhours)
        use rmn_common
        IMPLICIT NONE
        integer :: idate1, idate2
        real(kind = real64) :: nhours
    end subroutine DIFDATr

    INTEGER FUNCTION newdate(DAT1, DAT2, DAT3, MODE)
        IMPLICIT NONE
        integer :: DAT1, DAT2(*), DAT3, MODE
        integer, external :: naetwed
    end function newdate

    INTEGER FUNCTION IDATMG2(IDATE)
        IMPLICIT NONE
        integer idate(14)
        integer, external :: itdmag2
    end function IDATMG2

    subroutine DATMGP2(IDATE)
        IMPLICIT NONE
        integer idate(14)
    end subroutine DATMGP2

    subroutine NewDate_Options( value, command )
        IMPLICIT NONE
        character(len = *) :: value, command
    end subroutine NewDate_Options

    subroutine Get_Calendar_Status( NoLeapYears, CcclxDays )
        IMPLICIT NONE
        logical :: NoLeapYears, CcclxDays
    end subroutine Get_Calendar_Status

    integer function Calendar_Adjust(tdate1, tdate2, true_date_mode, adding)
        IMPLICIT NONE
        integer, external :: Calendar_Adjust_int
        integer :: tdate1, tdate2
        character(len=1) :: true_date_mode
        logical :: adding
    end function Calendar_Adjust

    integer function CcclxDays_Adjust(tdate1, tdate2, true_date_mode, adding)
        IMPLICIT NONE
        integer, external :: CcclxDays_Adjust_int
        integer :: tdate1, tdate2 ! input TrueDates
        character(len=1) :: true_date_mode ! (B)asic or (E)xtended TrueDates
        logical :: adding ! operating mode (T=incadtr, F=difdatr)
    end function CcclxDays_Adjust

    integer function LeapYear_Adjust(tdate1, tdate2, true_date_mode, adding)
        IMPLICIT NONE
        integer, external :: LeapYear_Adjust_int
        logical :: adding
        character(len=1) :: true_date_mode ! (B)asic or (E)xtended true dates
        integer :: tdate1, tdate2
    end function LeapYear_Adjust

    subroutine Ignore_LeapYear()
        IMPLICIT NONE
    end subroutine Ignore_LeapYear

    subroutine Accept_LeapYear()
        IMPLICIT NONE
    end subroutine Accept_LeapYear

    subroutine Get_LeapYear_Status(no_leap_year_status)
        IMPLICIT NONE
        logical :: no_leap_year_status
    end subroutine Get_LeapYear_Status
end interface
