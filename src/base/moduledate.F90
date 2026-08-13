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
!> This file contains a collection of function for manipulating dates.
!>
!> A true date is an integer (possibly negative) that contains the number of 5 seconds intervals since 1980/01/01 00h00.
!> Negative values arise as this concept applies from 1900/01/01.
!> 
!> An extended true date is an integer that contains the number of 3 hour intervals since year 00/01/01.
!> 
!> There are three styles of CMC date-time stamps (all use integers):
!> - Old: an integer(< 123 200 000) of the following form: MMDDYYZZR
!>   + MM = Month of the year (1-12)
!>   + DD = Day of the month (1-31)
!>   + YY = Year(00-99)=>old style only good before 2000/1/1
!>   + ZZ = Hour(00-23)
!>   + R  = Run (0-9) kept for backward compatibility
!> - New: an integer(>= 123 200 000) that contains the true date(number of 5 seconds intervals since 1980/1/1 00h00)
!>   + It can be computed with the following algorithm:
!>   + `false_date = new_date_time_stamp - 123200000`
!>   + `true_date = (false_date / 10) * 8 + mod(false_date, 10)`
!> - Extended: an unsigned integer(>= 3 000 000 000) that contains the extended true date (number of hours since 0000/1/1 00h)
!>   + It can be computed with the following algorithm:
!>   + `ext_false_date = ext_date_time_stamp - 3000000000`
!>   + `ext_true_date = (ext_false_date / 10) * 8 + mod(ext_false_date, 10)`
!>   + As this extended date is stored in a signed integer, the stored value will be a large negative one.
!>
!> # Environment variable
!>
!> The NEWDATE_OPTIONS environment variable can be set to modify the behavior of the date manipulation functions.
!> It follows the syntax below:
!> `export NEWDATE_OPTIONS="[debug][,][year=360_day|365_day|gregorian][,][debug]"`
!>
!> Examples of usage:
!> - `export NEWDATE_OPTIONS="debug"`
!> - `export NEWDATE_OPTIONS="year=360_day"`
!> - `export NEWDATE_OPTIONS="debug, year=360_day"`
!> - `export NEWDATE_OPTIONS="year=365_day"`
!> - `export NEWDATE_OPTIONS="year=365_day, debug"`


!> This module is meant to be private to this file
!> Helper functions that were previously defined inside individual functions (sometimes
!> defined several times)
module rmn_md_helpers
    use, intrinsic :: iso_fortran_env, only: int64
    implicit none

    !> 3 000 000 000 (Integer*8, Z'B2D05E00')
    integer(kind = int64), parameter :: troisg = 3000000000_8
    !> Z'00000000FFFFFFFF'
    integer(kind = int64), parameter :: masque32 = ishft(-1_8, -32)
    !> Julian day for jan 1, 1900
    integer, parameter :: jd1900 = 2415021
    !> Julian day for jan 1, 1980
    integer, parameter :: jd1980 = 2444240
    !> Julian day for jan 1, 0
    integer, parameter :: jd0 = 1721060
    !> Julian day for jan 1, 10, 000
    integer, parameter :: jd10k = 5373485
    !> (((jd10k - jd0) * 24) / 8) * 10
    integer, parameter :: max_offset = 109572750
    !> Julian day for jan 1, 2236
    integer, parameter :: jd2236 = 2537742
    !> Extended truedate for jan 1, 1901, 01Z
    integer, parameter :: tdexcept = 16663825
    !> Base for newdates (jan 1, 1980, 00Z)
    integer, parameter :: tdstart = 123200000
    !> Truedate of jan 1, 1900
    integer, parameter :: td1900 = -504904320
    !> Truedate of dec 31, 2235, 23h59
    integer, parameter :: td2235 = 1615714548
    !> Truedate for jan 1, 2000 , 00Z
    integer, parameter :: td2000 = 126230400
    !> Number of 5 sec intervals in a day
    integer, parameter :: nb_5_sec_per_day = 17280
    !> Number of 5 sec intervals in an hour
    integer, parameter :: nb_5_sec_per_hour = 720

    !> Number of days for each month of the year
    integer , dimension(12), parameter :: mdays = [31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]

contains
    !> Calculates julian calendar day 
    !> \details(number of days since day 1 -> jan 1, year 1?)
    !> see CACM letter to editor by Fliegel and Flandern 1968 page 657
    pure function julian_day(year, month, day) result(jd)
        implicit none
        integer, intent(in) :: year, month, day
        integer :: jd
        jd = day - 32075 + 1461 * (year + 4800 + (month - 14) / 12) / 4 &
                + 367 * (month - 2 - (month - 14) / 12 * 12) / 12 &
                - 3 * ((year + 4900 + (month - 14) / 12) / 100) / 4
    end function julian_day

    pure function is_bissextile(year) result(res)
        implicit none
        integer, intent(in) :: year
        logical :: res
        res = ( ((MOD(year, 4) == 0) .and. (MOD(year, 100) /= 0) ) .or. (MOD(year, 400) == 0) )
    end function is_bissextile

    !> Check whether the given truedate is valid (date > jan 1, 1980 if 5 sec interval, else > jan 1, 1900)
    pure function is_validtd(tdate) result(is_valid)
        implicit none 
        integer, intent(in) :: tdate
        logical :: is_valid
        is_valid = ((tdate >= 0) .or. ((tdate < 0) .and. (tdate >= td1900) .and. (mod(tdate - td1900, nb_5_sec_per_hour) == 0)))
    end function is_validtd

    !> Check that year, month, day, zulu have valid values
    pure function is_validtm(year, month, day, zulu) result(is_valid)
        implicit none
        integer, intent(in) :: year, month, day, zulu
        logical :: is_valid
        is_valid = &
            (year >= 1900) .and. (year < 2236) .and.  &
            (month <= 12) .and. &
            (zulu <= 23) .and. &
            (month > 0) .and. (day > 0) .and. (zulu >= 0)
        if (is_valid) then
            is_valid = (day <= mdays(month)) 
        endif
    end function is_validtm

    !> Check that year, month, day, zulu have valid values
    !> \details The year must be in [0, 10000[.
    !> The month must be in ]0, 12].
    !> The day of month must not exceed the max for that month.
    !> Bissextile years aren't checked, 29 is used as the lastly of February regardless of the year.
    !> Zulu must be in [0, 23].
    pure function is_validtme(year, month, day, zulu) result(is_valid)
        implicit none
        integer, intent(in) :: year, month, day, zulu
        logical :: is_valid
        is_valid = ( &
            (year  >= 0) .and. (year  <  10000) .and. &
            (month >  0) .and. (month <= 12) .and. &
            (day   >  0) .and. &
            (zulu  >= 0) .and. (zulu  <= 23) )
        if (is_valid) then
            is_valid = day <= mdays(month)
        end if
    end function is_validtme
end module rmn_md_helpers

module rmn_date
    implicit none
    include 'rmn/rmn_date.inc'

    interface
        pure subroutine datec(julian_day, year, month, day)
            implicit none
            integer, intent(in) :: julian_day
            integer, intent(out) :: year
            integer, intent(out) :: month
            integer, intent(out) :: day
        end subroutine
    end interface

contains


    !> Convert from true_date and run_number to CMCstamp
    !> \return 0 on success, 1 otherwise
    !> \qualifier "Interface"
    function tdate_runnb_to_cmcstamp(tdate, runnb, cmcstamp) result (retval)
        use rmn_md_helpers
        implicit none

        !> True date
        integer, intent(in) :: tdate
        !> Run number
        integer, intent(in) :: runnb
        !> CMCstamp
        integer, intent(out) :: cmcstamp

        integer :: retval

        integer :: year, month, day, zulu, tmptd

        retval = 1
        cmcstamp = 0
        if ((runnb > 9) .or. (.not. is_validtd(tdate))) return
        ! use new stamp if > jan 1, 2000 or fractional hour
        if (tdate >= td2000 .or. mod(tdate, nb_5_sec_per_hour) /= 0) then
            ! encode it in a new date - time stamp, ignore run nb
            cmcstamp = tdstart + (tdate / 8) * 10 + mod(tdate, 8)
        else
            ! encode it in an old date-time stamp
            call datec(jd1900 + (tdate - td1900) / nb_5_sec_per_day, year, month, day)
            tmptd = (tdate - td1900) / nb_5_sec_per_hour * nb_5_sec_per_hour + td1900
            zulu = mod(tmptd - td1900, nb_5_sec_per_day) / nb_5_sec_per_hour
            cmcstamp = month * 10000000 + day * 100000 + (year - 1900) * 1000 + zulu * 10 + runnb
        endif
        retval = 0
    end function


    !> Convert from CMCstamp (old or new) to true_date and run_number
    !> \return 0 on success, 1 otherwise
    !> \qualifier "Interface"
    function cmcstamp_to_tdate_runnb(cmcstamp, tdate, runnb) result (retval)
        use rmn_md_helpers
        use app, only: lib_log, app_librmn, app_error
        implicit none

        !> CMCstamp
        integer, intent(in) :: cmcstamp
        !> True date
        integer, intent(out) :: tdate
        !> Run number
        integer, intent(out) :: runnb

        integer :: retval

        integer :: year, month, day, zulu

        retval = 1
        tdate = 0
        runnb = 0
        if (cmcstamp >= tdstart) then
            ! cmcstamp is a new date-time stamp
            tdate = (cmcstamp - tdstart) / 10 * 8 + mod(cmcstamp - tdstart, 10)
            runnb = 0
        else if (cmcstamp < -1) then
            call lib_log(APP_LIBRMN, APP_ERROR, 'naetwed: newdate error mode 1, negative stamp')
            return
        else
            ! cmcstamp is an old date-time stamp
            runnb = mod(cmcstamp, 10)
            zulu = mod(cmcstamp / 10, 100)
            year = mod(cmcstamp / 1000, 100) + 1900
            day = mod(cmcstamp / 100000, 100)
            month = mod(cmcstamp / 10000000, 100)
            tdate = (julian_day(year, month, day) - jd1980) * nb_5_sec_per_day + zulu * nb_5_sec_per_hour
        endif
        if (.not. is_validtd(tdate)) return
        retval = 0
    end function


    !> Convert from extended true date to CMCstamp
    !> \return 0 on success, 1 otherwise
    !> \qualifier "Interface"
    function exttdate_to_cmcstamp(exttdate, cmcstamp) result (retval)
        use rmn_md_helpers
        implicit none

        !> Extended true date
        integer, intent(in) :: exttdate
        !> CMCstamp
        integer, intent(out) :: cmcstamp

        integer :: retval

        integer :: stamp, tdate, zulu
        integer(kind = int64) :: date_unsigned

        retval = 1
        cmcstamp = 0
        if (exttdate == tdexcept .or. (exttdate / 24 + jd0) < jd1900 .or. (exttdate / 24 + jd0) >= jd2236) then
            ! extended stamp
            stamp = (exttdate / 8) * 10 + mod(exttdate, 8)
            date_unsigned = stamp + troisg
            cmcstamp = int(date_unsigned)
            retval = 0
        else
            ! (new or old) stamp
            zulu = mod(exttdate, 24)
            tdate = (exttdate / 24 + jd0 - jd1980) * nb_5_sec_per_day + zulu * nb_5_sec_per_hour
            retval = tdate_runnb_to_cmcstamp(tdate, 0, cmcstamp)
        endif
    end function


    !> Convert from printable to extended stamp
    !> \return 0 on success, 1 otherwise
    !> \qualifier "Interface"
    function printable_to_extstamp(pdate, ptime, extstamp) result (retval)
        use rmn_md_helpers
        use app, only: lib_log, app_librmn, app_error, app_msg
        implicit none

        !> Integer value representing the date in a printable form (YYYYMMDD)
        integer, intent(in) :: pdate
        !> Integer value representing the time in a printable form (HHMM0000)
        integer, intent(in) :: ptime
        !> Extended stamp
        integer, intent(out) :: extstamp

        integer :: retval

        integer :: year, month, day, minute, stamp, tdate, zulu

        retval = 1
        extstamp = 0
        year = mod(pdate / 10000, 10000)
        month = mod(pdate / 100, 100)
        day = mod(pdate, 100)
        zulu = mod(ptime / 1000000, 100)
        minute = mod(ptime / 10000, 100)
        if (.not. is_validtme(year, month, day, zulu)) return
        if ((month == 2) .and. (day == 29)) then
            if (.not. is_bissextile(year)) return
        endif
        tdate = julian_day(year, month, day)
        if (tdate < jd0 .or. tdate >= jd10k) then
            write(app_msg, *)'naetwed: newdate error, date outside of supported range, date =', pdate
            call lib_log(APP_LIBRMN, APP_ERROR, app_msg)
            return
        endif
        tdate = (tdate - jd0) * 24 + zulu + minute / 60
        ! encode it in a new date - time stamp
        stamp = (tdate / 8) * 10 + mod(tdate, 8)
        extstamp = int(stamp + troisg)
        retval = 0
    end function


    !> Convert from extended stamp to printable
    !> \return 0 on success, 1 otherwise
    !> \qualifier "Interface"
    function extstamp_to_printable(extstamp, pdate, ptime) result (retval)
        use rmn_md_helpers
        use app, only: lib_log, app_librmn, app_error, app_msg
        implicit none

        !> Extended stamp
        integer, intent(in) :: extstamp
        !> Integer value representing the date in a printable form (YYYYMMDD)
        integer, intent(out) :: pdate
        !> Integer value representing the time in a printable form (HHMM0000)
        integer, intent(out) :: ptime

        integer :: retval

        integer(kind = int64) :: date_unsigned
        integer :: year, month, day, minute, stamp, tdate, zulu

        retval = 1
        pdate = 0
        ptime = 0
        date_unsigned = iand(masque32, int(extstamp, int64))
        if (date_unsigned < troisg .or. date_unsigned >= troisg + max_offset) then
            write(app_msg, *) 'naetwed: newdate error, invalid stamp for mode -5, stamp=', stamp
            call lib_log(APP_LIBRMN, APP_ERROR, app_msg)
            return
        endif
        stamp = int(date_unsigned - troisg)
        tdate = stamp / 10 * 8 + mod(stamp, 10)
        call datec(jd0 + tdate / 24, year, month, day)
        zulu = mod(tdate, 24)
        minute = 0
        if (.not. is_validtme(year, month, day, zulu)) return
        if ((month == 2) .and. (day == 29)) then
            if (.not. is_bissextile(year)) return
        endif
        pdate = year * 10000 + month * 100 + day
        ptime = zulu * 1000000 + minute * 10000
        retval = 0
    end function


    !> Convert from printable to CMC stamp
    !> \return 0 on success, 1 otherwise
    !> \qualifier "Interface"
    function printable_to_cmcstamp(pdate, ptime, cmcstamp) result (retval)
        use rmn_md_helpers
        implicit none

        !> Integer value representing the date in a printable form (YYYYMMDD)
        integer, intent(in) :: pdate
        !> Integer value representing the time in a printable form (HHMM0000)
        integer, intent(in) :: ptime
        !> CMC stamp
        integer, intent(out) :: cmcstamp

        integer :: retval

        integer :: year, month, day, tdate, zulu, second

        retval = 1
        cmcstamp = 0
        year = mod(pdate / 10000, 10000)
        ! ptime, pdate = 19010101, 01000000 will be encoded extended stamp
        ! as the corresponding old date - time stamp is used as an
        ! error indicator by INCDATR / IDATMG2 / DATMGP2
        ! years not in [ 1900, 2235 ] will be encoded extended stamp
        if ((pdate == 19010101 .and. ptime == 01000000) .or. (year < 1900 .or. year > 2235)) then
            retval = printable_to_extstamp(pdate, ptime, cmcstamp)
        else
            month = mod(pdate / 100, 100)
            day = mod(pdate, 100)
            zulu = mod(ptime / 1000000, 100)
            second = mod(ptime / 10000, 100) * 60 + mod(ptime / 100, 100)
            if (.not. is_validtm(year, month, day, zulu)) return
            if ((month == 2) .and. (day == 29)) then
                if (.not. is_bissextile(year)) return
            endif
            tdate = (julian_day(year, month, day) - jd1980) * nb_5_sec_per_day + zulu * nb_5_sec_per_hour + second / 5
            if (year >= 2000 .or. (year >= 1980 .and. second /= 0)) then
                ! encode it in a new date-time stamp
                cmcstamp = tdstart + (tdate / 8) * 10 + mod(tdate, 8)
            else
                ! encode it in an old date-time stamp
                tdate = (tdate - td1900) / nb_5_sec_per_hour * nb_5_sec_per_hour + td1900
                call datec(jd1900 + (tdate - td1900) / nb_5_sec_per_day, year, month, day)
                zulu = mod(tdate - td1900, nb_5_sec_per_day) / nb_5_sec_per_hour
                cmcstamp = month * 10000000 + day * 100000 + (year - 1900) * 1000 + zulu * 10
            endif
            retval = 0
        end if
    end function


    !> Convert from CMC stamp (old or new) to printable
    !> \return 0 on success, 1 otherwise
    !> \qualifier "Interface"
    function cmcstamp_to_printable(cmcstamp, pdate, ptime) result (retval)
        use rmn_md_helpers
        implicit none

        !> CMC stamp
        integer, intent(in) :: cmcstamp
        !> Integer value representing the date in a printable form (YYYYMMDD)
        integer, intent(out) :: pdate
        !> Integer value representing the time in a printable form (HHMM0000)
        integer, intent(out) :: ptime

        integer :: retval

        integer :: year, month, day, zulu, tdate, second

        retval = 1
        pdate = 0
        ptime = 0

        ! cmcstamp < -1 means extended stamp
        if (cmcstamp < -1) then
            retval = extstamp_to_printable(cmcstamp, pdate, ptime)
        else
            if (cmcstamp >= tdstart) then
                ! cmcstamp is a new date - time stamp
                tdate = (cmcstamp - tdstart) / 10 * 8 + mod(cmcstamp - tdstart, 10)
                call datec(jd1900 + (tdate - td1900) / nb_5_sec_per_day, year, month, day)
                zulu = mod(tdate - td1900, nb_5_sec_per_day) / nb_5_sec_per_hour
                second = (mod(tdate - td1900, nb_5_sec_per_day) - zulu * nb_5_sec_per_hour) * 5
                pdate = year * 10000 + month * 100 + day
                ptime = zulu * 1000000 + (second / 60) * 10000 + mod(second, 60) * 100
            else
                ! cmcstamp is an old date - time stamp
                zulu = mod(cmcstamp / 10, 100)
                year = mod(cmcstamp / 1000, 100) + 1900
                day = mod(cmcstamp / 100000, 100)
                month = mod(cmcstamp / 10000000, 100)
                pdate = year * 10000 + month * 100 + day
                ptime = zulu * 1000000
            endif
            if (.not. is_validtm(year, month, day, zulu)) return
            if ((month == 2) .and. (day == 29)) then
                if (.not. is_bissextile(year)) return
            endif
            retval = 0
        end if
    end function


    !> Convert from CMC stamp to extended true date
    !> \return 0 on success, 1 otherwise
    !> \qualifier "Interface"
    function cmcstamp_to_exttdate_runnb(cmcstamp, exttdate, runnb) result (retval)
        use rmn_md_helpers
        use app, only: lib_log, app_librmn, app_error, app_msg
        implicit none

        !> CMCstamp
        integer, intent(in) :: cmcstamp
        !> Extended true date
        integer, intent(out) :: exttdate
        !> Run number
        integer, intent(out) :: runnb

        integer :: retval

        integer :: year, month, day, zulu, stamp, tdate, run
        integer(kind = int64) :: date_unsigned, stamp8

        retval = 1
        exttdate = 0
        runnb = 0
        if (cmcstamp <  -1) then
            stamp8 = cmcstamp
            stamp8 = iand(masque32, stamp8)
            date_unsigned = stamp8
            if (date_unsigned < troisg .or. date_unsigned > troisg + max_offset) then
                write(app_msg, *) 'naetwed: newdate error, invalid stamp for mode -6, stamp=', cmcstamp
                call lib_log(APP_LIBRMN, APP_ERROR, app_msg)
                return
            endif
            stamp = int(date_unsigned - troisg)
            exttdate = stamp / 10 * 8 + mod(stamp, 10)
        else
            if (cmcstamp >= tdstart) then
                ! cmcstamp is a new date-time stamp
                run = 0
                tdate = (cmcstamp - tdstart) / 10 * 8 + mod(cmcstamp - tdstart, 10)
                call datec(jd1900 + (tdate - td1900) / nb_5_sec_per_day, year, month, day)
                zulu = mod(tdate - td1900, nb_5_sec_per_day) / nb_5_sec_per_hour
                tdate = (julian_day(year, month, day) - jd0) * 24 + zulu
            else
                ! cmcstamp is an old date-time stamp
                run = mod(cmcstamp, 10)
                zulu = mod(cmcstamp / 10, 100)
                year = mod(cmcstamp / 1000, 100) + 1900
                day = mod(cmcstamp / 100000, 100)
                month = mod(cmcstamp / 10000000, 100)
                tdate = (julian_day(year, month, day) - jd0) * 24 + zulu
            endif
            if (.not. is_validtd(tdate)) return
            exttdate = tdate
            runnb = run
        endif
        retval = 0
    end function


    !> Convert from extended true date to printable
    !> \return 0 on success, 1 otherwise
    !> \qualifier "Interface"
    function exttdate_to_printable(exttdate, pdate, ptime) result (retval)
        use rmn_md_helpers
        implicit none

        !> Extended true date
        integer, intent(in) :: exttdate
        !> Integer value representing the date in a printable form (YYYYMMDD)
        integer, intent(out) :: pdate
        !> Integer value representing the time in a printable form (HHMM0000)
        integer, intent(out) :: ptime

        integer :: retval

        integer :: year, month, day, zulu

        retval = 1
        pdate = 0
        ptime = 0
        if (.not. is_validtd(exttdate)) return
        call datec(jd0 + exttdate / 24, year, month, day)
        zulu = mod(exttdate, 24)
        if (.not. is_validtme(year, month, day, zulu)) return
        if ((month == 2) .and. (day == 29)) then
            if (.not. is_bissextile(year)) return
        endif
        pdate = year * 10000 + month * 100 + day
        ptime = zulu * 1000000
        retval = 0
    end function


    !> Convert from printable to extended true date
    !> \return 0 on success, 1 otherwise
    !> \qualifier "Interface"
    function printable_to_exttdate(pdate, ptime, exttdate) result (retval)
        use rmn_md_helpers
        use app, only: lib_log, app_librmn, app_error, app_msg
        implicit none

        !> Integer value representing the date in a printable form (YYYYMMDD)
        integer, intent(in) :: pdate
        !> Integer value representing the time in a printable form (HHMM0000)
        integer, intent(in) :: ptime
        !> Extended true date
        integer, intent(out) :: exttdate

        integer :: retval

        integer :: year, month, day, second, zulu

        retval = 1
        exttdate = 0
        year = mod(pdate / 10000, 10000)
        if (year < 0 .or. year >= 10000) then
            write(app_msg, *)'naetwed: newdate error, date outside of supported range, date =', pdate
            call lib_log(APP_LIBRMN, APP_ERROR, app_msg)
            return
        endif
        month = mod(pdate / 100, 100)
        day = mod(pdate, 100)
        zulu = mod(ptime / 1000000, 100)
        second = mod(ptime / 10000, 100) * 60 + mod(ptime / 100, 100)
        if (.not. is_validtme(year, month, day, zulu)) return
        if ((month == 2) .and. (day == 29)) then
            if (.not. is_bissextile(year)) return
        endif
        exttdate = (julian_day(year, month, day) - jd0) * 24 + zulu
        retval = 0
    end function


    !> Convert from true date to printable
    !> \return 0 on success, 1 otherwise
    !> \qualifier "Interface"
    function tdate_to_printable(tdate, pdate, ptime) result (retval)
        use rmn_md_helpers
        implicit none

        !> True date
        integer, intent(in) :: tdate
        !> Integer value representing the date in a printable form (YYYYMMDD)
        integer, intent(out) :: pdate
        !> Integer value representing the time in a printable form (HHMM0000)
        integer, intent(out) :: ptime

        integer :: retval

        integer :: year, month, day, second, zulu

        retval = 1
        pdate = 0
        ptime = 0
        if (.not. is_validtd(tdate)) return
        call datec(jd1900 + (tdate - td1900) / nb_5_sec_per_day, year, month, day)
        zulu = mod(tdate - td1900, nb_5_sec_per_day) / nb_5_sec_per_hour
        second = (mod(tdate - td1900, nb_5_sec_per_day) - zulu * nb_5_sec_per_hour) * 5
        pdate = year * 10000 + month * 100 + day
        ptime = zulu * 1000000 + second / 60 * 10000 + mod(second, 60) * 100
        retval = 0
    end function


    !> Convert from printable to true date
    !> \return 0 on success, 1 otherwise
    !> \qualifier "Interface"
    function printable_to_tdate(pdate, ptime, tdate) result (retval)
        use rmn_md_helpers
        implicit none

        !> Integer value representing the date in a printable form (YYYYMMDD)
        integer, intent(in) :: pdate
        !> Integer value representing the time in a printable form (HHMM0000)
        integer, intent(in) :: ptime
        !> True date
        integer, intent(out) :: tdate

        integer :: retval

        integer :: year, month, day, second, zulu

        retval = 1
        tdate = 0

        ! pdate, ptime = 19010101, 01000000 will be encoded extended true_date
        ! as the corresponding old date-time stamp is used as an
        ! error indicator by INCDATR / IDATMG2 / DATMGP2
        if (pdate == 19010101 .and. ptime == 01000000) then
            retval = printable_to_exttdate(pdate, ptime, tdate)
        else
            year = mod(pdate / 10000, 10000)
            month = mod(pdate / 100, 100)
            day = mod(pdate, 100)
            zulu = mod(ptime / 1000000, 100)
            second = mod(ptime / 10000, 100) * 60 + mod(ptime / 100, 100)
            if (.not. is_validtm(year, month, day, zulu)) return
            if ((month == 2) .and. (day == 29)) then
                if (.not. is_bissextile(year)) return
            endif
            tdate = (julian_day(year, month, day) - jd1980) * nb_5_sec_per_day + zulu * nb_5_sec_per_hour + second / 5
        end if
        retval = 0
    end function
end module rmn_date

!============================================================================
!                       THREAD SAFE ROUTINES
!     (they call the original ones inside a OpenMP critical region)
!     the original routine names have been deliberately mangled
!============================================================================

subroutine date_thread_lock(lock)
    implicit none
    !> If .true. attempt to acquire lock, .false. release lock
    logical, intent(IN) :: lock
    integer, save :: owner_thread = 0
    external :: set_user_lock
    call set_user_lock(owner_thread, lock)
end subroutine date_thread_lock

!> \copydoc idnacti
!> \qualifier "Interface"
subroutine incdati(idate1, idate2, nhours)
    use, intrinsic :: iso_fortran_env, only: real64
    implicit none
    integer, intent(out) :: idate1
    integer, intent(in) :: idate2
    real(real64), intent(in) :: nhours
    external :: date_thread_lock, idnacti
    call date_thread_lock(.true.)
    call idnacti(idate1, idate2, nhours)
    call date_thread_lock(.false.)
end subroutine incdati

!> \copydoc idnactr
!> \qualifier "Interface"
subroutine incdatr(idate1, idate2, nhours)
    use, intrinsic :: iso_fortran_env, only: real64
    implicit none
    integer, intent(out) :: idate1
    integer, intent(in) :: idate2
    real(real64), intent(in) :: nhours
    external :: date_thread_lock, idnactr
    call date_thread_lock(.true.)
    call idnactr(idate1, idate2, nhours)
    call date_thread_lock(.false.)
end subroutine incdatr

!> \copydoc ddiafti
!> \qualifier "Interface"
subroutine difdati(idate1, idate2, nhours)
    use, intrinsic :: iso_fortran_env, only: real64
    implicit none
    integer, intent(in) :: idate1
    integer, intent(in) :: idate2
    real(kind = real64), intent(out) :: nhours
    external :: date_thread_lock, ddiafti
    call date_thread_lock(.true.)
    call ddiafti(idate1, idate2, nhours)
    call date_thread_lock(.false.)
end subroutine difdati

!> \copydoc ddiaftr
!> \qualifier "Interface"
subroutine difdatr(idate1, idate2, nhours)
    use, intrinsic :: iso_fortran_env, only: real64
    implicit none
    integer, intent(in) :: idate1
    integer, intent(in) :: idate2
    real(kind = real64), intent(out) :: nhours
    external :: date_thread_lock, ddiaftr
    call date_thread_lock(.true.)
    call ddiaftr(idate1, idate2, nhours)
    call date_thread_lock(.false.)
end subroutine difdatr

!> \copydoc naetwed
!> \qualifier "Interface"
integer function newdate(dat1, dat2, dat3, mode)
    implicit none
    integer, intent(inout) :: dat1, dat2(*), dat3
    integer, intent(in) :: mode
    integer, external :: naetwed
    external :: date_thread_lock
    call date_thread_lock(.true.)
    newdate = naetwed(dat1, dat2, dat3, mode)
    call date_thread_lock(.false.)
end function newdate

!> \copydoc itdmag2
!> \qualifier "Interface"
integer function idatmg2(idate)
    implicit none
    integer, intent(inout) :: idate(14)
    integer, external :: itdmag2
    external :: date_thread_lock
    call date_thread_lock(.true.)
    idatmg2 = itdmag2(idate)
    call date_thread_lock(.false.)
end function idatmg2

!> \copydoc dmagtp2
!> \qualifier "Interface"
subroutine datmgp2(idate)
    implicit none
    integer, intent(inout) :: idate(14)
    external :: date_thread_lock, dmagtp2
    call date_thread_lock(.true.)
    call dmagtp2(idate)
    call date_thread_lock(.false.)
end subroutine datmgp2

!> \qualifier "Interface"
subroutine newdate_options(value, command)
    implicit none
    character(len = *), intent(inout) :: value
    character(len = *), intent(in) :: command
    external :: date_thread_lock, newdate_options_int
    call date_thread_lock(.true.)
    call newdate_options_int(value, command)
    call date_thread_lock(.false.)
end subroutine newdate_options

!> \qualifier "Interface"
subroutine get_calendar_status(noleapyears, ccclxdays)
    implicit none
    logical, intent(out) :: noleapyears, ccclxdays
    external :: date_thread_lock, get_calendar_status_int
    call date_thread_lock(.true.)
    call get_calendar_status_int(noleapyears, ccclxdays)
    call date_thread_lock(.false.)
end subroutine get_calendar_status

integer function calendar_adjust(tdate1, tdate2, true_date_mode, adding)
    implicit none
    integer, intent(inout) :: tdate1, tdate2
    character(len = 1), intent(in) :: true_date_mode
    logical, intent(in) :: adding
    external :: date_thread_lock
    integer, external :: calendar_adjust_int
    call date_thread_lock(.true.)
    calendar_adjust = calendar_adjust_int(tdate1, tdate2, true_date_mode, adding)
    call date_thread_lock(.false.)
end function calendar_adjust

!> \copydoc ccclxdays_adjust_int
!> \qualifier "Interface"
integer function ccclxdays_adjust(tdate1, tdate2, true_date_mode, adding)
    implicit none
    integer, intent(inout) :: tdate1, tdate2 ! input truedates
    character(len = 1), intent(in) :: true_date_mode ! (b)asic or (e)xtended truedates
    logical, intent(in) :: adding
    external :: date_thread_lock
    integer, external :: ccclxdays_adjust_int
    call date_thread_lock(.true.)
    ccclxdays_adjust = ccclxdays_adjust_int(tdate1, tdate2, true_date_mode, adding)
    call date_thread_lock(.false.)
end function ccclxdays_adjust

integer function leapyear_adjust(tdate1, tdate2, true_date_mode, adding)
    implicit none
    integer, intent(inout) :: tdate1, tdate2
    character(len = 1), intent(in) :: true_date_mode ! (B)asic or (E)xtended true dates
    logical, intent(in) :: adding
    external :: date_thread_lock
    integer, external :: LeapYear_Adjust_int
    call date_thread_lock(.true.)
    LeapYear_Adjust = LeapYear_Adjust_int(tdate1, tdate2, true_date_mode, adding)
    call date_thread_lock(.false.)
end function LeapYear_Adjust

subroutine Ignore_LeapYear()
    implicit none
    external :: date_thread_lock, Ignore_LeapYear_int
    call date_thread_lock(.true.)
    call Ignore_LeapYear_int
    call date_thread_lock(.false.)
end subroutine Ignore_LeapYear

subroutine Accept_LeapYear()
    implicit none
    external :: date_thread_lock, Accept_LeapYear_int
    call date_thread_lock(.true.)
    call Accept_LeapYear_int
    call date_thread_lock(.false.)
end subroutine Accept_LeapYear

subroutine Get_LeapYear_Status(no_leap_year_status)
    implicit none
    logical, intent(out) :: no_leap_year_status
    external :: date_thread_lock, Get_LeapYear_Status_int
    call date_thread_lock(.true.)
    call Get_LeapYear_Status_int(no_leap_year_status)
    call date_thread_lock(.false.)
end subroutine Get_LeapYear_Status

!============================================================================
!     END OF THREAD SAFE ROUTINES
!============================================================================

!     C-callable functions/subroutines
subroutine difdatr_c(idate1, idate2, nhours) bind(c, name = 'difdatr_c')
    use, intrinsic :: iso_c_binding, only : c_int, c_double
    implicit none
    integer(kind = c_int), intent(inout) :: idate1, idate2
    real(kind = c_double) :: nhours
    external :: difdatr
    call difdatr(idate1, idate2, nhours)
end subroutine difdatr_c

subroutine incdatr_c(idate1, idate2, nhours) bind(c, name = 'incdatr_c')
    use, intrinsic :: iso_c_binding, only : c_int, c_double
    implicit none
    integer(kind = c_int), intent(inout) :: idate1, idate2
    real(kind = c_double) :: nhours
    external :: incdatr
    call incdatr(idate1, idate2, nhours)
end subroutine incdatr_c

!> \copydoc naetwed
integer(kind = c_int) function newdate_c(dat1, dat2, dat3, mode) bind(C, name = 'newdate_c')
    use rmn_date
    use, intrinsic :: iso_c_binding, only : c_int
    implicit none

    integer(kind = c_int), intent(inout) :: dat1, dat2(*), dat3
    integer(kind = c_int), intent(in) :: mode
    newdate_c = newdate(dat1, dat2, dat3, mode)
end function newdate_c

!> \copydoc tdate_runnb_to_cmcstamp
integer(kind = c_int) function tdate_runnb_to_cmcstamp_c(tdate, runnb, cmcstamp) bind(C, name = 'tdate_runnb_to_cmcstamp_c')
    use rmn_date
    use, intrinsic :: iso_c_binding, only : c_int
    implicit none

    integer(kind = c_int), value, intent(in) :: tdate, runnb
    integer(kind = c_int), intent(out) :: cmcstamp
    tdate_runnb_to_cmcstamp_c = tdate_runnb_to_cmcstamp(tdate, runnb, cmcstamp)
end function

!> \copydoc cmcstamp_to_tdate_runnb
integer(kind = c_int) function cmcstamp_to_tdate_runnb_c(cmcstamp, tdate, runnb) bind(C, name = 'cmcstamp_to_tdate_runnb_c')
    use rmn_date
    use, intrinsic :: iso_c_binding, only : c_int
    implicit none

    integer(kind = c_int), value, intent(in) :: cmcstamp
    integer(kind = c_int), intent(out) :: tdate, runnb
    cmcstamp_to_tdate_runnb_c = cmcstamp_to_tdate_runnb(cmcstamp, tdate, runnb)
end function

!> \copydoc tdate_to_printable
integer(kind = c_int) function tdate_to_printable_c(tdate, pdate, ptime) bind(C, name = 'tdate_to_printable_c')
    use rmn_date
    use, intrinsic :: iso_c_binding, only : c_int
    implicit none

    integer(kind = c_int), value, intent(in) :: tdate
    integer(kind = c_int), intent(out) :: pdate, ptime
    tdate_to_printable_c = tdate_to_printable(tdate, pdate, ptime)
end function

!> \copydoc printable_to_tdate
integer(kind = c_int) function printable_to_tdate_c(pdate, ptime, tdate) bind(C, name = 'printable_to_tdate_c')
    use rmn_date
    use, intrinsic :: iso_c_binding, only : c_int
    implicit none

    integer(kind = c_int), value, intent(in) :: pdate, ptime
    integer(kind = c_int), intent(out) :: tdate
    printable_to_tdate_c = printable_to_tdate(pdate, ptime, tdate)
end function

!> \copydoc cmcstamp_to_printable
integer(kind = c_int) function cmcstamp_to_printable_c(cmcstamp, pdate, ptime) bind(C, name = 'cmcstamp_to_printable_c')
    use rmn_date
    use, intrinsic :: iso_c_binding, only : c_int
    implicit none

    integer(kind = c_int), value, intent(in) :: cmcstamp
    integer(kind = c_int), intent(out) :: pdate, ptime
    cmcstamp_to_printable_c = cmcstamp_to_printable(cmcstamp, pdate, ptime)
end function

!> \copydoc printable_to_cmcstamp
integer(kind = c_int) function printable_to_cmcstamp_c(pdate, ptime, cmcstamp) bind(C, name = 'printable_to_cmcstamp_c')
    use rmn_date
    use, intrinsic :: iso_c_binding, only : c_int
    implicit none

    integer(kind = c_int), value, intent(in) :: pdate, ptime
    integer(kind = c_int), intent(out) :: cmcstamp
    printable_to_cmcstamp_c = printable_to_cmcstamp(pdate, ptime, cmcstamp)
end function

!> \copydoc extstamp_to_printable
integer(kind = c_int) function extstamp_to_printable_c(extstamp, pdate, ptime) bind(C, name = 'extstamp_to_printable_c')
    use rmn_date
    use, intrinsic :: iso_c_binding, only : c_int
    implicit none

    integer(kind = c_int), value, intent(in) :: extstamp
    integer(kind = c_int), intent(out) :: pdate, ptime
    extstamp_to_printable_c = extstamp_to_printable(extstamp, pdate, ptime)
end function

!> \copydoc printable_to_extstamp
integer(kind = c_int) function printable_to_extstamp_c(pdate, ptime, extstamp) bind(C, name = 'printable_to_extstamp_c')
    use rmn_date
    use, intrinsic :: iso_c_binding, only : c_int
    implicit none

    integer(kind = c_int), value, intent(in) :: pdate, ptime
    integer(kind = c_int), intent(out) :: extstamp
    printable_to_extstamp_c = printable_to_extstamp(pdate, ptime, extstamp)
end function

!> \copydoc exttdate_to_cmcstamp
integer(kind = c_int) function exttdate_to_cmcstamp_c(exttdate, cmcstamp) bind(C, name = 'exttdate_to_cmcstamp_c')
    use rmn_date
    use, intrinsic :: iso_c_binding, only : c_int
    implicit none

    integer(kind = c_int), value, intent(in) :: exttdate
    integer(kind = c_int), intent(out) :: cmcstamp
    exttdate_to_cmcstamp_c = exttdate_to_cmcstamp(exttdate, cmcstamp)
end function

!> \copydoc cmcstamp_to_exttdate_runnb
integer(kind = c_int) function cmcstamp_to_exttdate_runnb_c(cmcstamp, exttdate, runnb) bind(C, name = 'cmcstamp_to_exttdate_runnb_c')
    use rmn_date
    use, intrinsic :: iso_c_binding, only : c_int
    implicit none

    integer(kind = c_int), value, intent(in) :: cmcstamp
    integer(kind = c_int), intent(out) :: exttdate, runnb
    cmcstamp_to_exttdate_runnb_c = cmcstamp_to_exttdate_runnb(cmcstamp, exttdate, runnb)
end function

!> \copydoc exttdate_to_printable
integer(kind = c_int) function exttdate_to_printable_c(exttdate, pdate, ptime) bind(C, name = 'exttdate_to_printable_c')
    use rmn_date
    use, intrinsic :: iso_c_binding, only : c_int
    implicit none

    integer(kind = c_int), value, intent(in) :: exttdate
    integer(kind = c_int), intent(out) :: pdate, ptime
    exttdate_to_printable_c = exttdate_to_printable(exttdate, pdate, ptime)
end function

!> \copydoc printable_to_exttdate
integer(kind = c_int) function printable_to_exttdate_c(pdate, ptime, exttdate) bind(C, name = 'printable_to_exttdate_c')
    use rmn_date
    use, intrinsic :: iso_c_binding, only : c_int
    implicit none

    integer(kind = c_int), value, intent(in) :: pdate, ptime
    integer(kind = c_int), intent(out) :: exttdate
    printable_to_exttdate_c = printable_to_exttdate(pdate, ptime, exttdate)
end function

! The original names of the following routines have been altered because of
! the above mentioned thread safe routines. Internal calls use the mangled internal names


!> Computes idate1 = idate2 + nhours
subroutine idnactr(idate1, idate2, nhours)
    use, intrinsic :: iso_fortran_env, only: real64
    implicit none

    !> Resulting date of the addition of idate2 and nhours
    !> Will be set to 101010101 (1910/10/10 10z run 1) if at least one of the input arguments is invalid
    integer, intent(out) :: idate1
    !> Date to which to add nhours
    integer, intent(in) :: idate2
    !> Number of hours to add to idate2
    real(kind = real64), intent(in) :: nhours

    external :: date_add_sub

    integer :: date2
    real(kind = real64) :: hours

    ! Copy input parameters to local variables in order to provide a clean interface
    date2 = idate2
    hours = nhours
    call date_add_sub(idate1, date2, hours, .true., .false.)
end


!> Compute idate1 = idate2 + nhours (idate2 and nhours rounded to nearest hour)
subroutine idnacti(idate1, idate2, nhours)
    use, intrinsic :: iso_fortran_env, only: real64
    implicit none

    !> Resulting date of the addition of idate2 and nhours (both rounded to the hour)
    !> Will be set to 101010101 (1910/10/10 10z run 1) if at least one of the input arguments is invalid
    integer, intent(out) :: idate1
    !> Date to which to add nhours (rounded to nearest hour if fractional)
    integer, intent(in) :: idate2
    !> Number of hours to add to idate2 (rounded to nearest hour if fractional)
    real(kind = real64), intent(in) :: nhours

    external :: date_add_sub

    integer :: date2
    real(kind = real64) :: hours

    ! Copy input parameters to local variables in order to provide a clean interface
    date2 = idate2
    hours = nhours
    call date_add_sub(idate1, date2, hours, .true., .true.)
end


!> Compute nhours = idate1 - idate2
subroutine ddiaftr(idate1, idate2, nhours)
    use, intrinsic :: iso_fortran_env, only: real64
    implicit none

    !> Date from which to substract idate2
    integer, intent(in) :: idate1
    !> Date subtracted from idate1
    integer, intent(in) :: idate2
    !> Difference in hours (fractional) between idate1 and idate2
    !> Will be set to 2**30 if at least one of the input arguments is invalid
    real(kind = real64), intent(out) :: nhours

    external :: date_add_sub

    integer :: date1, date2

    ! Copy input parameters to local variables in order to provide a clean interface
    date1 = idate1
    date2 = idate2
    call date_add_sub(date1, date2, nhours, .false., .false.)
end


!> Compute nhours = idate1 - idate2 (idate1 and idate2 rounded to nearest hour)
subroutine ddiafti(idate1, idate2, nhours)
    use, intrinsic :: iso_fortran_env, only: real64
    implicit none

    !> Date from which to substract idate2 (rounded to the hour before the computation)
    integer, intent(in) :: idate1
    !> Date subtracted from idate1 (rounded to the hour before the computation)
    integer, intent(in) :: idate2
    !> Difference in hours between idate1 and idate2
    !> Will be set to 2**30 if at least one of the input arguments is invalid
    real(kind = real64), intent(out) :: nhours

    external :: date_add_sub

    integer :: date1, date2

    ! Copy input parameters to local variables in order to provide a clean interface
    date1 = idate1
    date2 = idate2
    call date_add_sub(date1, date2, nhours, .false., .true.)
end


!> Compute additions and differences between dates and durations
!>
!> In cases where want_adding is true and one or more of the input arguments (idate2, nhours) is invalid,
!> idate1 will be set to 101010101 (1910/10/10 10z run 1).
!>
!> In cases where want_adding is false and one or more of the input arguments (idate1, idate2) is invalid,
!> nhours will be set to 2**30.
subroutine date_add_sub(idate1, idate2, nhours, want_adding, want_rounding)
    use app
    use, intrinsic :: iso_fortran_env, only: real64, int64
    use rmn_md_helpers
    implicit none

    !> CMC date-time stamp (old or new style)
    integer, intent(inout) :: idate1
    !> CMC date-time stamp (old or new style)
    integer, intent(inout) :: idate2
    !> Number of hours (can be fractional)
    real(kind = real64), intent(inout) :: nhours
    !> If true, idate1 = idate2 + nhours, otherwise nhours = idate1 - idate2
    logical, intent(in) :: want_adding
    !> Round input dates and hours to nearest hour before performing operation
    logical, intent(in) :: want_rounding

    integer, external :: Calendar_Adjust_int, naetwed
    external :: Get_Calendar_Status_int

    integer :: result

    logical :: no_leap_years, ccclx_days, goextend
    logical :: rounding

    integer(kind = int64) :: addit
    integer :: tdate1, tdate2, runnum, ndays, pdate2
    integer :: idate(2), pdate1(2)

    rounding = .false.

    if (.not. want_adding) then
        if (idate2 < -1 .or. idate1 < -1) then
            if (idate1 > -1) then
                result = naetwed(idate1, pdate1, pdate2, -3)
                if (result /= 0) then
                    write(app_msg, *) 'ddiaft: label 1,idate1:', idate1
                    call lib_log(APP_LIBRMN, APP_DEBUG, app_msg)
                    goto 2
                endif
                result = naetwed(tdate1, pdate1, pdate2, +7)
                if (result /= 0) then
                    write(app_msg, *) 'ddiaft: label 2,pdate1,pdate2:', pdate1(1), pdate2
                    call lib_log(APP_LIBRMN, APP_DEBUG, app_msg)
                    goto 2
                endif
            else
                idate(1) = idate1
                result = naetwed(tdate1, idate, runnum, 6)
            endif
        else
            idate(1) = idate1
            result = naetwed(tdate1, idate, runnum, 1)
        endif
        if (result /= 0) then
            write(app_msg, *) 'ddiaft: label 3,idate1:', idate1
            call lib_log(APP_LIBRMN, APP_DEBUG, app_msg)
            goto 2
        endif
    end if

    call Get_calendar_Status_int(no_leap_years, ccclx_days)
    if (idate2 < -1 .or. (idate1 < -1 .and. .not. want_adding)) then
        if (idate2 > -1) then
            result = naetwed(idate2, pdate1, pdate2, -3)
            if (result /= 0) then
                write(app_msg, *) 'ddiaft: label 4,idate2:', idate2
                call lib_log(APP_LIBRMN, APP_DEBUG, app_msg)
                goto 2
            endif
            result = naetwed(tdate2, pdate1, pdate2, +7)
            if (result /= 0) then
                write(app_msg, *) 'ddiaft: label 5,pdate1,pdate2:', pdate1(1), pdate2
                call lib_log(APP_LIBRMN, APP_DEBUG, app_msg)
                goto 2
            endif
        else
            idate(1) = idate2
            result = naetwed(tdate2, idate, runnum, 6)
        endif
        if (result /= 0) then
            write(app_msg, *) 'ddiaft: label 6,idate2:', idate2
            call lib_log(APP_LIBRMN, APP_DEBUG, app_msg)
            goto 2
        endif
        if (want_adding) then
            tdate1 = tdate2 + nint(nhours)
            if (no_leap_years .or. ccclx_days) then
                ndays = Calendar_Adjust_int(tdate1, tdate2, 'E', want_adding)
                tdate1 = tdate1 + (ndays * 24)
            endif
            result = naetwed(tdate1, idate, runnum, -6)
            idate1 = idate(1)
            if (result /= 0) then
                write(app_msg, *) 'ddiaft: after if adding,if rounding', tdate1
                call lib_log(APP_LIBRMN, APP_DEBUG, app_msg)
                goto 2
            endif
        else
            nhours = (tdate1 - tdate2)
            if (no_leap_years .or. ccclx_days) then
                ndays = Calendar_Adjust_int(tdate1, tdate2, 'E', want_adding)
                nhours = nhours - (ndays * 24)
            endif
        endif
    else
        idate(1) = idate2
        result = naetwed(tdate2, idate, runnum, 1)
        if (result /= 0) then
            write(app_msg, *) 'ddiaft: label 1,idate2:', idate2
            call lib_log(APP_LIBRMN, APP_DEBUG, app_msg)
            goto 2
        endif
        if (want_adding) then
            goextend = .false.
            rounding = want_rounding .or. (tdate2 < 0)
            if (rounding) then
                tdate2 = (tdate2 + sign(360, tdate2)) / nb_5_sec_per_hour * nb_5_sec_per_hour
                addit = nb_5_sec_per_hour * nint(nhours, 8)
            else
                addit = nint(nb_5_sec_per_hour * nhours, 8)
            endif
            if ((td1900 - tdate2) * 1_8 <= addit .and. & ! tdate2 + addit >= td1900 and
                (td2235 - tdate2) * 1_8 >= addit) then   ! tdate2 + addit <= td2235, where
                tdate1 = int(tdate2 + addit)         ! addit can be a very large
                if (no_leap_years .or. ccclx_days) then ! integer * 8 number
                    ndays = Calendar_Adjust_int(tdate1, tdate2, 'B', want_adding)
                    tdate1 = tdate1 + (ndays * 24 * nb_5_sec_per_hour)
                endif
                if ((tdate1 > td2235) .or. (tdate1 < td1900)) goextend = .true.
            else
                goextend = .true.
            endif
            if (goextend) then
                ! exiting regular date range for extended range
                result = naetwed(idate2, pdate1, pdate2, -3)
                if (result /= 0) then
                    write(app_msg, *) 'ddiaft: label 7,idate2:', idate2
                    call lib_log(APP_LIBRMN, APP_DEBUG, app_msg)
                    goto 2
                endif
                result = naetwed(tdate2, pdate1, pdate2, +7)
                if (result /= 0) then
                    write(app_msg, *) 'ddiaft: label 8,pdate1,pdate2:', pdate1(1), pdate2
                    call lib_log(APP_LIBRMN, APP_DEBUG, app_msg)
                    goto 2
                endif
                tdate1 = tdate2 + nint(nhours)
                if (no_leap_years .or. ccclx_days) then
                ndays = Calendar_Adjust_int(tdate1, tdate2, 'E', want_adding)
                tdate1 = tdate1 + (ndays * 24)
                endif
                result = naetwed(tdate1, idate, runnum, -6)
                idate1 = idate(1)
            else
                result = naetwed(tdate1, idate, runnum, -1)
                idate1 = idate(1)
            endif
            if (result /= 0) then
                write(app_msg, *) 'ddiaft: after if adding,if rounding', tdate1
                call lib_log(APP_LIBRMN, APP_DEBUG, app_msg)
                goto 2
            endif
        else
            if (want_rounding) then
                tdate1 = (tdate1 + sign(360, tdate1)) / nb_5_sec_per_hour * nb_5_sec_per_hour
                tdate2 = (tdate2 + sign(360, tdate2)) / nb_5_sec_per_hour * nb_5_sec_per_hour
                nhours = nint((tdate1 - tdate2) / real(nb_5_sec_per_hour))
            else
                nhours = (tdate1 - tdate2)
                nhours = nhours / real(nb_5_sec_per_hour)
            endif
            if (no_leap_years .or. ccclx_days) then
                ndays = Calendar_Adjust_int(tdate1, tdate2, 'B', want_adding)
                nhours = nhours - (ndays * 24)
            endif
        endif
    endif
    return

 2  continue
    if (want_adding) then
        idate1 = 101010101
    else
        nhours = 2.0 ** 30
    endif
end subroutine date_add_sub


!> Constructs a canadian meteorological centre date-time stamp using the operational cmc date-time group
!> \return CMC date-time stamp (same as idate(14)), 101010101 in case of error (invalid inputs)
integer function itdmag2(idate)
    implicit none

    !> Input and output 14 member array
    !> | Index | Intent | Description                                     |
    !> | ----: | :----: | :---------------------------------------------- |
    !> |     1 |    in  | Day of week [1, 7] (Sunday = 1)                 |
    !> |     2 |    in  | Month [1, 12]                                   |
    !> |     3 |    in  | Day of month [1, 31]                            |
    !> |     4 |    in  | Year [0, 99], [100, 10000]                      |
    !> |     5 |    in  | Zulu [0, 23]                                    |
    !> |     6 |    in  | Hundredth of second since last hour [0, 359999] |
    !> |    14 |   out  | CMC date-time stamp (new, old, extended)        |
    integer, intent(inout) :: idate(14)

    integer, external :: naetwed
    integer :: dtpr(2), tmpr, year, result

    year = idate(4)
    if ((year >= 0) .and. (year <= 99)) then
        year = year + 1900
    endif
    dtpr(1) = year * 10000 + idate(2) * 100 + idate(3)
    tmpr = idate(5) * 1000000 + (idate(6) / 6000) * 10000 + mod(idate(6) / 100, 60) * 100
    result = naetwed(idate(14), dtpr, tmpr, 3)
    if (result /= 0) idate(14) = 101010101

    itdmag2 = idate(14)
end


!> Decode a CMC date stamp to various date and time components
!>
!> If idate(14) is invalid, the outputs will correspond to 1910-10-10 10z
!> \todo Test and verified the meaning of idate indexes 11 to 13
subroutine dmagtp2(idate)
    use rmn_md_helpers
    implicit none

    !> 14 member array used for input and output
    !> | Index | Intent | Description                                         |
    !> | ----: | :----: | :-------------------------------------------------- |
    !> |     1 |   out  | Day of the week [1, 7] (sunday=1)                   |
    !> |     2 |   out  | Month [1, 12]                                       |
    !> |     3 |   out  | Day of month [1, 31]                                |
    !> |     4 |   out  | Year [0, 10000]                                     |
    !> |     5 |   out  | Zulu hour [0, 23]                                   |
    !> |     6 |   out  | 100 * number_of_second_since_last_hour [0, 359 999] |
    !> |     7 |   out  | Day of week uppercase 3 letter abbreviation         |
    !> |     8 |   out  | Month uppercase 3 letter abbreviation               |
    !> |     9 |   out  | Day of month                                        |
    !> |    10 |   out  | Year                                                |
    !> |    11 |   out  | Minutes                                             |
    !> |    12 |   out  | Seconds                                             |
    !> |    13 |   out  | Minutes                                             |
    !> |    14 |   in   | CMC date-time stamp (old, new or extended)          |
    integer, intent(inout) :: idate(14)

    integer, external :: naetwed

    character(len = 3), parameter :: xmonth(12) = &
        ['JAN', 'FEB', 'MAR', 'APR', 'MAY', 'JUN', 'JUL', 'AUG', 'SEP', 'OCT', 'NOV', 'DEC']
    character(len = 3), parameter :: xday(7) = ['SUN', 'MON', 'TUE', 'WED', 'THU', 'FRI', 'SAT']

    integer :: dtpr, tmpr, result, tpr(2)
    integer i, iday, idt, mon
    character(len = 3) :: amonth, aday
    character(len = 128) :: wrk

    idt = idate(14)

    result = naetwed(idt, tpr, tmpr, -3)
    dtpr = tpr(1)
    if (result /= 0) then
        idt = 101010101
        dtpr = 19101010
        tmpr = 10000000
    endif

    idate(2) = mod(dtpr / 100, 100)
    idate(3) = mod(dtpr, 100)
    idate(4) = mod(dtpr / 10000, 10000)
    idate(5) = mod(tmpr / 1000000, 100)
    idate(6) = mod(tmpr / 10000, 100) * 6000 + mod(tmpr / 100, 100) * 100 + mod(tmpr, 100)

    mon = idate(2)
    amonth = xmonth(mon)
    idate(1) = julian_day(idate(4), idate(2), idate(3))
    idate(1) = 1 + mod(idate(1) + 1, 7)
    iday = idate(1)
    aday = xday(iday)

    write(wrk, "(1x, a, 1x, a, i3.2, 1x, i4.2, i3.2, 'Z', i2.2, ':', i2.2, '.', i2.2)") &
        aday, amonth, (idate(i), i = 3, 5), idate(6) / 6000, mod(idate(6) / 100, 60), mod(idate(6), 100)
    read (wrk, "(7a4)") (idate(i), i = 7, 13)
end subroutine dmagtp2


subroutine datmgp(idate)
    use rmn_date
    implicit none

    integer, intent(inout) :: idate(14)

    call datmgp2(idate)
    idate(4) = mod(idate(4), 100)
    idate(6) = mod(idate(6), 10)
end


integer function idatmg(idate)
    use rmn_date
    implicit none

    integer, intent(inout) :: idate(14)

    integer :: status

    status = idatmg2(idate)
    idatmg = idate(14)
end

subroutine Ignore_LeapYear_int()
    implicit none

    external :: NewDate_Options_int

    character(len = 512) :: str

    str = 'year=365_day'
    call NewDate_Options_int(str, 'set')
end


subroutine Accept_LeapYear_int()
    implicit none

    external :: NewDate_Options_int

    character(len = 512) :: str

    str = 'year=gregorian'
    call NewDate_Options_int(str, 'set')
end


subroutine Get_LeapYear_Status_int(no_leap_year_status)
    implicit none

    logical, intent(out) :: no_leap_year_status

    external :: NewDate_Options_int

    character(len = 512) :: value_str

    value_str = 'year'
    call NewDate_Options_int(value_str, 'get')

    if (value_str == '365_day' .or. value_str == '360_day') then
        no_leap_year_status = .true.
    else
        no_leap_year_status = .false.
    endif
end


module calendar_status_info
    logical, save :: called_newdate_options = .false.
    logical, save :: no_newdate_env_options = .true.
    logical, save :: no_leap_years = .false.
    logical, save :: ccclx_days = .false.
    logical, save :: debug = .false.
end


!> Get, set or unset calendar options
!>
!> Options are initialized with the NEWDATE_OPTIONS if defined
!> The known calendars options are currently: gregorian, 365_day (no leap years) and 360_day
subroutine NewDate_Options_int(value_str, command)
    ! A) Permits alternative calendar options, via either the NEWDATE_OPTIONS environment variable (which
    ! has precedence) or via appropriate "set" commands
    ! B) Also, returns calendar status via the "get" command
    ! C) The Get_Calendar_Status entry also return this

    use calendar_status_info
    implicit none

    !> Option value
    character(len = *), intent(inout) :: value_str
    !> Operation to perform (get|set|unset)
    character(len = *), intent(in) :: command

    external :: getenvc, up2low

    integer :: ii
    character(512) :: evalue, string

    ! check environment once
    if (.not. called_newdate_options) then
        call getenvc('NEWDATE_OPTIONS', evalue)
        called_newdate_options = .true.
        if (evalue /= ' ') then
            ! variable was set
            call up2low(evalue, evalue)
            ii = index(evalue, 'debug')
            if (ii > 0) debug = .true.
            ii = index(evalue, 'year=')
            if (ii > 0) then
                ! found known option. check its value
                if (evalue(ii+5:ii+11) == '365_day' .or. evalue(ii+5:ii+11) == '360_day') then
                    no_newdate_env_options = .false.
                    no_leap_years = .true.
                    if (evalue(ii+5:ii+11) == '360_day') ccclx_days = .true.
                else if (evalue(ii+5:ii+13) == 'gregorian') then
                    no_newdate_env_options = .false.
                    no_leap_years = .false.
                    ccclx_days = .false.
                endif
                if (debug) write(6, "(/' Debug no_leap_years,ccclx_days=', L1, 1x, L1/)") no_leap_years, ccclx_days
            endif
        endif
    endif

    evalue = value_str
    call up2low(evalue, evalue)
    string = command
    call up2low(string, string)

    if (string == 'get') then
        ! check for value of defined options
        if (evalue == 'year') then
            if (ccclx_days) then
                value_str = '360_day'
            else if (no_leap_years) then
                value_str = '365_day'
            else
                value_str = 'gregorian'
            endif
        endif
    else if (string == 'set' .and. no_newdate_env_options) then
        ! try to set known options, but environment has precedence
        ii = index(evalue, 'year=')
        if (ii > 0) then
            if (evalue(ii+5:ii+11) == '365_day' .or. evalue(ii+5:ii+11) == '360_day') then
                no_leap_years = .true.
                ccclx_days = .false.
                if (evalue(ii+5:ii+11) == '360_day') ccclx_days = .true.
            else if (evalue(ii+5:ii+13) == 'gregorian') then
                no_leap_years = .false.
                ccclx_days = .false.
            endif
        endif
    else if (string == 'unset' .and. no_newdate_env_options) then
        ! try to unset known options, but environment has precedence
        ii = index(evalue, 'year=')
        if (ii > 0) then
            if (evalue(ii+5:ii+11) == '365_day') no_leap_years = .false.
            if (evalue(ii+5:ii+11) == '360_day') ccclx_days = .false.
            if (evalue(ii+5:ii+13) == 'gregorian') no_leap_years = .true.
            if (no_leap_years) ccclx_days = .false.
        endif
    endif
end

subroutine Get_Calendar_Status_int(NoLeapYears, CcclxDays)
    use calendar_status_info
    implicit none

    logical, intent(out) :: NoLeapYears
    logical, intent(out) :: CcclxDays

    external :: getenvc, up2low

    character(len = 512) :: evalue
    integer :: ii

    if (.not. called_newdate_options) then ! check environment once
        call getenvc('NEWDATE_OPTIONS', evalue)
        called_newdate_options = .true.
        if (evalue /= ' ') then ! variable was set
            call up2low(evalue, evalue)
            ii = index(evalue, 'debug')
            if (ii > 0) debug = .true.
            ii = index(evalue, 'year=')
            if (ii > 0) then ! found known option. check its value
            if (evalue(ii+5:ii+11) == '365_day' .or. evalue(ii+5:ii+11) == '360_day') then
                no_newdate_env_options = .false.
                no_leap_years = .true.
                if (evalue(ii+5:ii+11) == '360_day') ccclx_days = .true.
            else if (evalue(ii+5:ii+13) == 'gregorian') then
                no_newdate_env_options = .false.
                no_leap_years = .false.
                ccclx_days = .false.
            endif
            if (debug) write(6, "(/' Debug no_leap_years,ccclx_days=', L1, 1x, L1/)") no_leap_years, ccclx_days
            endif
        endif
    endif

    NoLeapYears = no_leap_years
    CcclxDays = ccclx_days
end


!> Adjust dates based on the CcclxDays or NoLeapYears options
integer function Calendar_Adjust_int(tdate1, tdate2, true_date_mode, adding)
    implicit none

    integer, intent(inout) :: tdate1, tdate2
    character(len = 1), intent(in) :: true_date_mode
    logical, intent(in) :: adding

    external :: Get_Calendar_Status_int

    integer Adjust
    logical NoLeapYears, CcclxDays
    integer, external :: LeapYear_Adjust_int, CcclxDays_Adjust_int

    call Get_Calendar_Status_int(NoLeapYears, CcclxDays)

    Adjust = 0

    if (CcclxDays) then
        Adjust = CcclxDays_Adjust_int(tdate1, tdate2, true_date_mode, adding)
    else if (NoLeapYears) then
        Adjust = LeapYear_Adjust_int(tdate1, tdate2, true_date_mode, adding)
    endif

    Calendar_Adjust_int = Adjust
end


integer function LeapYear_Adjust_int(tdate1, tdate2, true_date_mode, adding)
    use app
    use rmn_md_helpers
    implicit none

    !> 
    integer, intent(inout) :: tdate1
    !> 
    integer, intent(inout) :: tdate2
    !> 'B' for basic TrueDates, 'E' for extended ones
    character(len = 1), intent(in) :: true_date_mode
    logical, intent(in) :: adding

    integer, parameter :: limite = 23595500 ! 23h 59m 55s

    integer :: true2print, print2true
    integer :: ier, inc, m1, m2, dat(2)
    integer :: annee, y1, y1L, y2, p1a(2), p1b, p2a(2), p2b
    integer :: ndays, tdate1L, tdate28f, tdate29f, addit
    integer :: date3_tmp
    integer :: dummy

    integer, external :: naetwed

    addit = 0 ! If adding, will hold a day in units of True Dates

    if (true_date_mode == 'B') then
        ! Basic true date mode
        true2print = -2
        print2true = +2
        if (adding) addit = nb_5_sec_per_day
    elseif (true_date_mode == 'E') then
        ! Extended true date mode
        true2print = -7
        print2true = +7
        if (adding) addit = 24
    endif

    ! Local value of tdat1; if adding, it will gradually evolve to its real value as leap days are found
    tdate1L = tdate1

    ier = naetwed(tdate1, p1a, p1b, true2print) ! true date to printable, but this
    y1 = p1a(1) / 10000 ! may still accounts for leap days
    m1 = mod(p1a(1) / 100, 100)
    ier = naetwed(tdate2, p2a, p2b, true2print)
    y2 = p2a(1) / 10000
    m2 = mod(p2a(1) / 100, 100)
    ndays = 0
    inc = 1
    if (y2 > y1 .or. (y1 == y2 .and. m2 > m1)) inc = -1
    do annee = y2, y1, inc
        if (is_bissextile(annee)) then
            dat(1) = annee * 10000 + 0228
            date3_tmp = limite
            ier = naetwed(tdate28f, dat, date3_tmp, print2true)
            dat(1) = annee * 10000 + 0229
            if (inc > 0) then
                date3_tmp = 0
                ier = naetwed(tdate29f, dat, date3_tmp, print2true)
                if (tdate29f <= tdate28f) call lib_log(APP_LIBRMN, APP_ERROR, 'LeapYear_Adjust_int: tdate29f < tdate28f')
                if ((tdate2 <= tdate28f) .and. (tdate1L >= tdate29f)) then
                    ndays = ndays + inc
                    tdate1L = tdate1L + addit * inc
                endif
            else
                date3_tmp = limite
                ier = naetwed(tdate29f, dat, date3_tmp, print2true)
                if (tdate29f <= tdate28f) call lib_log(APP_LIBRMN, APP_ERROR, 'LeapYear_Adjust_int: tdate29f < tdate28f')
                if ((tdate2 >= tdate28f) .and. (tdate1L <= tdate29f)) then
                    ndays = ndays + inc
                    tdate1L = tdate1L + addit * inc
                endif
            endif
        endif
    enddo
    ier = naetwed(tdate1L, p1a, p1b, true2print)
    y1L = p1a(1) / 10000
    do annee = y1 + inc, y1L, inc
        if (is_bissextile(annee)) then
            dat(1) = annee * 10000 + 0228
            ! Since the actual intent of naetwed's arguments changes based on the mode,
            ! they are all declared as "inout". Therefore, using a parameter as an actual argument causes a warning
            dummy = limite
            ier = naetwed(tdate28f, dat, dummy, print2true)
            dat(1) = annee * 10000 + 0229
            if (inc > 0) then
                dummy = 0
                ier = naetwed(tdate29f, dat, dummy, print2true)
                if (tdate29f <= tdate28f) call lib_log(APP_LIBRMN, APP_ERROR, 'LeapYear_Adjust_int: tdate29f < tdate28f')
                if ((tdate2 <= tdate28f) .and. (tdate1L >= tdate29f)) then
                    ndays = ndays + inc
                    tdate1L = tdate1L + addit * inc
                endif
            else
                dummy = limite
                ier = naetwed(tdate29f, dat, dummy, print2true)
                if (tdate29f <= tdate28f) call lib_log(APP_LIBRMN, APP_ERROR, 'LeapYear_Adjust_int: tdate29f < tdate28f')
                if ((tdate2 >= tdate28f) .and. (tdate1L <= tdate29f)) then
                    ndays = ndays + inc
                    tdate1L = tdate1L + addit * inc
                endif
            endif
        endif
    enddo

    LeapYear_Adjust_int = ndays
end function LeapYear_Adjust_int


!> Calculate correction (in days) to account for "360-day calendar"
!> \details difdatr and incdatr calculation errors, which are by default always done with the gregorian calendar
integer function CcclxDays_Adjust_int(tdate1, tdate2, true_date_mode, adding)
    use app
    use, intrinsic :: iso_fortran_env, only: real64
    use rmn_md_helpers, only: nb_5_sec_per_hour
    implicit none

    !> First date
    integer, intent(inout) :: tdate1
    !> Second date
    integer, intent(inout) :: tdate2
    !> 'B' for basic TrueDates, 'E' for extended ones
    character(len = 1), intent(in) :: true_date_mode
    !> True for incadtr, False for difdatr
    logical, intent(in) :: adding

    real(kind = real64) :: nhours, nhoursi, td2h
    integer :: true2print, print2true, ier
    integer :: ye1, mo1, da1, ho1, mi1, se1, p1a(2), p1b
    integer :: ye2, mo2, da2, ho2, mi2, se2, p2a(2), p2b
    integer :: addit, tdateL
    integer, external :: naetwed

    addit = 0 ! Holds a day in units of TrueDates
    td2h = 0 ! Holds the True Dates to hours conversion factor

    if (true_date_mode == 'B') then
        ! Basic TrueDates mode
        true2print = -2
        print2true = +2
        addit = 17280
        td2h = real(nb_5_sec_per_hour)
    elseif (true_date_mode == 'E') then
        ! Extended TrueDates mode
        true2print = -7
        print2true = +7
        addit = 24
        td2h = 1.
    endif

    ier = naetwed(tdate2, p2a, p2b, true2print)

    ! decode p2a and p2b
    ye2 = p2a(1) / 10000
    mo2 = mod(p2a(1) / 100, 100)
    da2 = mod(p2a(1), 100)

    ! sanity check: make sure that tdate2 conforms to a 360-day
    if ((da2 > 28 .and. mo2 == 2) .or. (da2 > 30 .and. mo2 > 4)) then
        write(app_msg, *) 'CcclxDays_Adjust_int: Illegal date for 360-day calendar ', p2a(1)
        call lib_log(APP_LIBRMN, APP_ERROR, app_msg)
        CcclxDays_Adjust_int = 89478485 ! * 24 = 2^31 - 8, a LARGE number
        if (.not. adding) CcclxDays_Adjust_int = -CcclxDays_Adjust_int
        ! and should cause a quick abort
        return
    endif

    if (mo2 == 1 .and. da2 == 31) then
        ! Convert to 30-day months
        da2 = 1
        mo2 = 2
    else if (mo2 == 2) then
        da2 = da2 + 1
    else if (mo2 == 3) then
        if (da2 == 1) then
            da2 = 30
            mo2 = 2
        else
            da2 = da2 -1
        endif
    endif

    da2 = (mo2 - 1) * 30 + da2 ! Work with 360 days in a year (12*30)

    ho2 = p2b / 1000000
    mi2 = mod(p2b / 10000 , 100)
    se2 = mod(p2b / 100 , 100)

    if (adding) then
        ! incdatr mode

        ! nhours is the interval (in hours) we
        ! are trying to add/substract to tdate2
        nhours = (tdate1 - tdate2) / td2h

        ho1 = int(abs(nhours))
        se1 = nint((abs(nhours) - ho1) * 3600)
        mi1 = se1 / 60
        se1 = mod(se1, 60)
        ye1 = ho1 / (360 * 24)
        ho1 = mod(ho1, (360 * 24))
        da1 = ho1 / 24
        ho1 = mod(ho1, 24)

        if (nhours < 0) then
            ! substracting ...
            se1 = se2 - se1
            if (se1 < 0) then
                se1 = se1 + 60
                mi2 = mi2 - 1
            endif

            mi1 = mi2 - mi1
            if (mi1 < 0) then
                mi1 = mi1 + 60
                ho2 = ho2 - 1
            endif

            ho1 = ho2 - ho1
            if (ho1 < 0) then
                ho1 = ho1 + 24
                da2 = da2 - 1
            endif

            da1 = da2 - da1
            if (da1 < 1) then
                da1 = da1 + 360
                ye2 = ye2 - 1
            endif

            ye1 = ye2 - ye1
        else
            ! ... adding
            se1 = se2 + se1
            if (se1 > 59) then
                se1 = se1 - 60
                mi2 = mi2 + 1
            endif

            mi1 = mi2 + mi1
            if (mi1 > 59) then
                mi1 = mi1 - 60
                ho2 = ho2 + 1
            endif

            ho1 = ho2 + ho1
            if (ho1 > 23) then
                ho1 = ho1 - 24
                da2 = da2 + 1
            endif

            da1 = da2 + da1
            if (da1 > 360) then
                da1 = da1 - 360
                ye2 = ye2 + 1
            endif

            ye1 = ye2 + ye1
        endif

        mo1 = (da1 - 1) / 30 + 1
        da1 = da1 - (mo1 - 1) * 30

        ! reverse the previous constant 30-day months conversion
        if (mo1 == 2 .and. da1 == 1) then
            da1 = 31
            mo1 = 1
        else if (mo1 == 2) then
            if (da1 == 30) then
                da1 = 1
                mo1 = 3
            else
                da1 = da1 - 1
            endif
        else if (mo1 == 3) then
            da1 = da1 + 1
        endif

        ! calculate the real TrueDate
        p1a(1) = (ye1 * 100 + mo1) * 100 + da1
        p1b = ((ho1 * 100 + mi1) * 100 + se1) * 100

        ier = naetwed(tdateL, p1a, p1b, print2true)

        ! ensure that tdate1 + CcclxDays_Adjust = tdateL
        CcclxDays_Adjust_int = (tdateL - tdate1) / addit

        ier = mod(tdateL - tdate1 , addit)
        if (ier /= 0) call lib_log(APP_LIBRMN, APP_ERROR, 'CcclxDays_Adjust_int: probleme 1 dans CcclxDays_Adjust')
    else
        ! difdatr mode
        ier = naetwed(tdate1, p1a, p1b, true2print)

        ! decode p1a and p1b
        ye1 = p1a(1) / 10000
        mo1 = mod(p1a(1) / 100, 100)
        da1 = mod(p1a(1), 100)

        ! sanity check: make sure that tdate1 conforms to a 360-day
        if ((da1 > 28 .and. mo1 == 2) .or. (da1 > 30 .and. mo1 > 4)) then
            write(app_msg, *) 'Illegal date for 360-day calendar ', p1a(1)
            call lib_log(APP_LIBRMN, APP_ERROR, 'CcclxDays_Adjust_int: probleme 1 dans CcclxDays_Adjust')
            CcclxDays_Adjust_int = 89478485 ! * 24 = 2^31 - 8, a LARGE number
            return ! and should cause a quick abort
        endif

        if (mo1 == 1 .and. da1 == 31) then ! Convert to 30-day months
            da1 = 1
            mo1 = 2
        else if (mo1 == 2) then
            da1 = da1+1
        else if (mo1 == 3) then
            if (da1 == 1) then
                da1 = 30
                mo1 = 2
            else
                da1 = da1 -1
            endif
        endif

        da1 = (mo1 - 1) * 30 + da1 ! Work with 360 days in a year (12*30)

        ho1 = p1b / 1000000
        mi1 = mod(p1b / 10000 , 100)
        se1 = mod(p1b / 100 , 100)

        ! calculate the difference between the decoded tdate1
        ! and tdate2 in hours in this 360-day calendar
        nhours = (se1 - se2) / 3600.0_8
        nhours = nhours + (mi1 - mi2) / 60.0_8
        nhours = nhours + (ho1 - ho2)
        nhours = nhours + (da1 - da2) * 24.0_8
        nhours = nhours + (ye1 - ye2) * 8640.0_8 ! 24*360

        ! ensure that nhours = nhours(I) - (correction * 24)
        nhoursi = (tdate1 - tdate2) / td2h
        CcclxDays_Adjust_int = nint((nhoursi - nhours) / 24.0)

        ier = int(mod(nint((nhoursi - nhours) * 10000.0, 8), 240000_8))
        if (ier /= 0) call lib_log(APP_LIBRMN, APP_ERROR, 'CcclxDays_Adjust_int: probleme 2 dans CcclxDays_Adjust')
    endif
end


!> Converts dates between various formats
!> \return 0 on success, 1 otherwise
!> \warning If this function returns one, the output values are unreliable.
!> \warning Please use \ref newdate instead of \ref naetwed since the latter is not reentrant.
integer function naetwed(dat1, dat2, dat3, mode)
    use rmn_md_helpers
    use rmn_date
    implicit none

    !> First parameter, see the table for a parameter "mode" for meaning
    integer, intent(inout) :: dat1
    !> Second parameter, see the table for a parameter "mode" for meaning
    integer, intent(inout) :: dat2(*)
    !> Third parameter, see the table for a parameter "mode" for meaning
    integer, intent(inout) :: dat3
    !> Operation mode: conversion to perform
    !> | Mode | dat1 intent | dat1 content                           | dat2 intent | dat2 content                           | dat3 intent | dat3 content                         | Replacement function            |
    !> | ---: | :---------: | :------------------------------------- | :---------: | :------------------------------------- | :---------: | :----------------------------------- | :------------------------------ |
    !> |   -1 |      in     | TrueDate                               |     out     | CMC Date-Time stamp (old or new style) |      in     | Run number                           | \ref tdate_runnb_to_cmcstamp    |
    !> |    1 |     out     | TrueDate                               |      in     | CMC Date-Time stamp (old or new style) |      in     | Run number                           | \ref cmcstamp_to_tdate_runnb    |
    !> |   -2 |      in     | TrueDate                               |     out     | Integer of printable date (YYYYMMDD)   |     out     | Integer of printable time (HHMMSShh) | \ref tdate_to_printable         |
    !> |    2 |     out     | TrueDate                               |      in     | Integer of printable date (YYYYMMDD)   |      in     | Integer of printable time (HHMMSShh) | \ref printable_to_tdate         |
    !> |   -3 |      in     | CMC Date-Time stamp (old or new style) |     out     | Integer of printable date (YYYYMMDD)   |     out     | Integer of printable time (HHMMSShh) | \ref cmcstamp_to_printable      |
    !> |    3 |     out     | CMC Date-Time stamp (old or new style) |      in     | Integer of printable date (YYYYMMDD)   |      in     | Integer of printable time (HHMMSShh) | \ref printable_to_cmcstamp      |
    !> |   -4 |      in     | CMC Date-Time stamp (old or new style) |     out     | 14 member old style date array         |     N/A     | Unused                               | \ref dmagtp2                    |
    !> |    4 |     out     | CMC Date-Time stamp (old or new style) |      in     | 14 member old style date array         |     N/A     | Unused                               | \ref itdmag2                    |
    !> |   -5 |      in     | Extended stamp                         |     out     | Integer of printable date (YYYYMMDD)   |     out     | Integer of printable time (HHMMSShh) | \ref extstamp_to_printable      |
    !> |    5 |     out     | Extended stamp                         |      in     | Integer of printable date (YYYYMMDD)   |      in     | Integer of printable time (HHMMSShh) | \ref printable_to_extstamp      |
    !> |   -6 |      in     | Extended TrueDate                      |     out     | CMC Date-Time stamp (old or new style) |     out     | Run number                           | \ref exttdate_to_cmcstamp       |
    !> |    6 |     out     | Extended TrueDate                      |      in     | CMC Date-Time stamp (old or new style) |     out     | Run number                           | \ref cmcstamp_to_exttdate_runnb |
    !> |   -7 |     out     | Extended TrueDate                      |      in     | Integer of printable date (YYYYMMDD)   |      in     | Integer of printable time (HHMMSShh) | \ref printable_to_exttdate      |
    !> |    7 |      in     | Extended TrueDate                      |     out     | Integer of printable date (YYYYMMDD)   |     out     | Integer of printable time (HHMMSShh) | \ref exttdate_to_printable      |
    integer, intent(in) :: mode

    integer :: tdate, runnb, stamp, pdate, ptime

    integer, external :: itdmag2
    external :: dmagtp2


    ! Signal failure by default
    naetwed = 1
    if (abs(mode) > 7 .or. mode == 0) return

    if (mode == -3) then
        ! From stamp(old or new) to printable

        ! The original implementation didn't change the actual arguments values in case of error
        ! We mimic this behavior here to preserve backward compatibility
        ptime = dat2(1)
        pdate = dat3
        naetwed = cmcstamp_to_printable(dat1, pdate, ptime)
        if (naetwed == 0) then
            dat2(1) = pdate
            dat3 = ptime
        end if
        return
    end if ! mode == -3

    if (mode == 3) then
        ! From printable to stamp

        ! The original implementation didn't change the actual arguments values in case of error
        ! We mimic this behavior here to preserve backward compatibility
        stamp = dat1
        naetwed = printable_to_cmcstamp(dat2(1), dat3, stamp)
        if (naetwed == 0) then
            dat1 = stamp
        end if
        return
    end if ! mode == 3

    if (mode == -2) then
        ! From true_date to printable
        ptime = dat2(1)
        pdate = dat3
        naetwed = tdate_to_printable(dat1, ptime, pdate)
        if (naetwed == 0) then
            dat2(1) = ptime
            dat3 = pdate
        end if
        return
    end if ! mode == -2

    if (mode == 2) then
        ! From printable to true_date

        ! The original implementation didn't change the actual arguments values in case of error
        ! We mimic this behavior here to preserve backward compatibility
        tdate = dat1
        naetwed = printable_to_tdate(dat2(1), dat3, tdate)
        if (naetwed == 0) then
            dat1 = tdate
        end if
    end if ! mode == 2

    if (mode == -1) then
        ! From (true_date and run_number) to stamp

        ! The original implementation didn't change the actual arguments values in case of error
        ! We mimic this behavior here to preserve backward compatibility
        stamp = dat2(1)
        naetwed = tdate_runnb_to_cmcstamp(dat1, dat3, stamp)
        if (naetwed == 0) then
            dat2(1) = stamp
        end if
        return
    end if ! mode == -1

    if (mode == 1) then
        ! From stamp(old or new) to (true_date and run_number)

        ! The original implementation didn't change the actual arguments values in case of error
        ! We mimic this behavior here to preserve backward compatibility
        tdate = dat1
        runnb = dat3
        naetwed = cmcstamp_to_tdate_runnb(dat2(1), tdate, runnb)
        if (naetwed == 0) then
            dat1 = tdate
            dat3 = runnb
        end if
        return
    end if ! mode == 1

    if (mode == 4) then
        ! mode = 4 : from 14 word old style DATE array TO STAMP and array(14)
        dat1 = itdmag2(dat2)
        naetwed = 0
        return
    end if ! mode == 4

    if (mode == -4) then
        ! From STAMP TO 14 word old style DATE array
        dat2(14) = dat1
        call dmagtp2(dat2)
        naetwed = 0
        return
    end if ! mode == -4

    if (mode == 5) then
        ! From printable to extended stamp

        ! Contrary to the other modes, this one sets its output (dat1) to 0 regardless of if it succeeds or not
        naetwed = printable_to_extstamp(dat2(1), dat3, dat1)
        return
    end if ! mode == 5

    if (mode == -5) then
        ! From extended stamp to printable

        ! The original implementation didn't change the actual arguments values in case of error
        ! We mimic this behavior here to preserve backward compatibility
        ptime = dat2(1)
        pdate = dat3
        naetwed = extstamp_to_printable(dat1, ptime, pdate)
        if (naetwed == 0) then
            dat2(1) = ptime
            dat3 = pdate
        end if
        return
    end if ! mode == -5

    if (mode == -6) then
        ! From extended true date to stamp

        ! The original implementation didn't change the actual arguments values in case of error
        ! We mimic this behavior here to preserve backward compatibility
        stamp = dat2(1)
        naetwed = exttdate_to_cmcstamp(dat1, stamp)
        if (naetwed == 0) then
            dat2(1) = stamp
        end if
        return
    end if ! mode == -6

    if (mode == 6) then
        ! From stamp to extended true date

        ! The original implementation didn't change the actual arguments values in case of error
        ! We mimic this behavior here to preserve backward compatibility
        tdate = dat1
        runnb = dat3
        naetwed = cmcstamp_to_exttdate_runnb(dat2(1), tdate, runnb)
        if (naetwed == 0) then
            dat1 = tdate
            dat3 = runnb
        end if
        return
    end if ! mode == 6

    if (mode == -7) then
        ! From extended true_date to printable

        ! The original implementation didn't change the actual arguments values in case of error
        ! We mimic this behavior here to preserve backward compatibility
        ptime = dat2(1)
        pdate = dat3
        naetwed = exttdate_to_printable(dat1, ptime, pdate)
        if (naetwed == 0) then
            dat2(1) = ptime
            dat3 = pdate
        end if
        return
    end if ! mode == -7

    if (mode == 7) then
        ! From printable to extended true_date

        ! The original implementation didn't change the actual arguments values in case of error
        ! We mimic this behavior here to preserve backward compatibility
        tdate = dat1
        naetwed = printable_to_exttdate(dat2(1), dat3, tdate)
        if (naetwed == 0) then
            dat1 = tdate
        end if
        return
    end if ! mode == 7
end function
