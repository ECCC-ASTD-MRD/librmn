program test_module_date
    use app
    use rmn_common
    use rmn_date
    implicit none

    integer, parameter :: day1(1) = 20231211
    integer, parameter :: day2(1) = 20231225
    integer, parameter :: time1 = 12383700
    integer, parameter :: time2 = 00000000

    real(kind = real64), parameter :: num_hours = 323.35

    integer, parameter :: expected_day3_r(1) = 20231224
    integer, parameter :: expected_time3_r = 23593500
    integer, parameter :: expected_day3_i(1) = 20231225
    integer, parameter :: expected_time3_i = 0

    real(kind = real64), parameter :: expected_diff_r = (((13 * 24 + 11) * 60 + 21) * 60 + 25) / 3600.0
    real(kind = real64), parameter :: expected_diff_i = 323.0

    integer :: date1, date2, date3_r, date3_i, tmpd(1), tmpt
    integer :: day3_i(1), time3_i, day3_r(1), time3_r
    integer :: extstamp, exttdate, cmcstamp, refcmcstamp, refexttdate, tdate, runnb
    real(kind = real64) :: diff_r, diff_i
    integer :: res

    res = printable_to_cmcstamp(day1(1), time1, cmcstamp)
    if (res /= 0) then
        call app_log(APP_ERROR, 'printable_to_cmcstamp did not return 0!')
        error stop 1
    end if
    write(app_msg, "('cmcstamp = ', i12)") cmcstamp
    call app_log(APP_VERBATIM, app_msg)
    res = cmcstamp_to_printable(cmcstamp, day3_i(1), time3_i)
    if (res /= 0) then
        call app_log(APP_ERROR, 'cmcstamp_to_printable did not return 0!')
        error stop 1
    end if
    ! The maximum resolution of a cmcstamp is 5 seconds.
    ! If the input seconds weren't a multiple of 5, the printable round trip can have
    ! a difference up to 500
    if (day1(1) /= day3_i(1) .or. abs(time1 - time3_i) >= 500) then
        write(app_msg, "('Roundtrip printable -> cmcstamp -> printable failed: Date ', i8.8, ' vs ', i8.8, ', Time: ', i8.8, ' vs ', i8.8)") &
            day1(1), day3_i(1), time1, time3_i
        call app_log(APP_ERROR, app_msg)
        error stop 1
    end if

    refcmcstamp = cmcstamp
    res = cmcstamp_to_tdate_runnb(cmcstamp, tdate, runnb)
    if (res /= 0) then
        call app_log(APP_ERROR, 'cmcstamp_to_tdate_runnb did not return 0!')
        error stop 1
    end if
    res = tdate_runnb_to_cmcstamp(tdate, runnb, cmcstamp)
    if (res /= 0) then
        call app_log(APP_ERROR, 'tdate_runnb_to_cmcstamp did not return 0!')
        error stop 1
    end if
    if (cmcstamp /= refcmcstamp) then
        write(app_msg, "('Roundtrip cmcstamp -> tdate, runnb -> cmcstamp failed: Date ', i12, ' vs ', i12)") &
            cmcstamp, refcmcstamp
        call app_log(APP_ERROR, app_msg)
        error stop 1
    end if

    res = printable_to_extstamp(day1(1), time1, extstamp)
    if (res /= 0) then
        call app_log(APP_ERROR, 'printable_to_extstamp did not return 0!')
        error stop 1
    end if
    write(app_msg, "('extstamp = ', i12)") extstamp
    call app_log(APP_VERBATIM, app_msg)
    ! extstamp = -1272790382
    res = extstamp_to_printable(extstamp, day3_i(1), time3_i)
    if (res /= 0) then
        call app_log(APP_ERROR, 'extstamp_to_printable did not return 0!')
        error stop 1
    end if
    ! The maximum resolution of a extstamp is 1 hour.
    if (day1(1) /= day3_i(1) .or. abs(time1 - time3_i) >= 1000000) then
        write(app_msg, "('Roundtrip printable -> extstamp -> printable failed: Date ', i8.8, ' vs ', i8.8, ', Time: ', i8.8, ' vs ', i8.8)") &
            day1(1), day3_i(1), time1, time3_i
        call app_log(APP_ERROR, app_msg)
        error stop 1
    end if

    res = printable_to_tdate(day1(1), time1, tdate)
    if (res /= 0) then
        call app_log(APP_ERROR, 'printable_to_tdate did not return 0!')
        error stop 1
    end if
    res = tdate_to_printable(tdate, day3_i(1), time3_i)
    if (res /= 0) then
        call app_log(APP_ERROR, 'tdate_to_printable did not return 0!')
        error stop 1
    end if
    write(app_msg, "('tdate = ', i12, ' date = ', i8.8, ' time = ', i8.8)") tdate, day3_i(1), time3_i
    call app_log(APP_VERBATIM, app_msg)
    if (day1(1) /= day3_i(1) .or. abs(time1 - time3_i) >= 500) then
        write(app_msg, "('Roundtrip printable -> tdate -> printable failed: Date ', i8.8, ' vs ', i8.8, ', Time: ', i8.8, ' vs ', i8.8)") &
            day1(1), day3_i(1), time1, time3_i
        call app_log(APP_ERROR, app_msg)
        error stop 1
    end if

    res = printable_to_exttdate(day1(1), time1, exttdate)
    if (res /= 0) then
        call app_log(APP_ERROR, 'printable_to_exttdate did not return 0!')
        error stop 1
    end if
    res = exttdate_to_printable(exttdate, day3_i(1), time3_i)
    if (res /= 0) then
        call app_log(APP_ERROR, 'exttdate_to_printable did not return 0!')
        error stop 1
    end if
    write(app_msg, "('exttdate = ', i12, ' date = ', i8.8, ' time = ', i8.8)") exttdate, day3_i(1), time3_i
    call app_log(APP_VERBATIM, app_msg)
    ! The maximum resolution of a extstamp is 1 hour.
    if (day1(1) /= day3_i(1) .or. abs(time1 - time3_i) >= 1000000 ) then
        write(app_msg, "('Roundtrip printable -> exttdate -> printable failed: Date ', i8.8, ' vs ', i8.8, ', Time: ', i8.8, ' vs ', i8.8)") &
            day1(1), day3_i(1), time1, time3_i
        call app_log(APP_ERROR, app_msg)
        error stop 1
    end if

    res = exttdate_to_cmcstamp(exttdate, cmcstamp)
    if (res /= 0) then
        call app_log(APP_ERROR, 'exttdate_to_cmcstamp did not return 0!')
        error stop 1
    end if
    res = cmcstamp_to_printable(cmcstamp, day3_i(1), time3_i)
    if (res /= 0) then
        call app_log(APP_ERROR, 'cmcstamp_to_printable did not return 0!')
        error stop 1
    end if
    write(app_msg, "('cmcstamp = ', i12, ' date = ', i8.8, ' time = ', i8.8)") cmcstamp, day3_i(1), time3_i
    call app_log(APP_VERBATIM, app_msg)
    refexttdate = exttdate
    res = cmcstamp_to_exttdate_runnb(cmcstamp, exttdate, runnb)
    if (res /= 0) then
        call app_log(APP_ERROR, 'cmcstamp_to_exttdate_runnb did not return 0!')
        error stop 1
    end if
    if (refexttdate /= exttdate) then
        write(app_msg, "('Roundtrip exttdate -> cmcstamp -> exttdate failed: refextdate = ', i12, ' vs exttdate = ', i12)") &
            refexttdate, exttdate
        call app_log(APP_ERROR, app_msg)
        error stop 1
    end if

    tmpd(1) = day1(1)
    tmpt = time1
    res = newdate(date1, tmpd, tmpt, 3)
    write(app_msg, "('date1 = ', i12)") date1
    call app_log(APP_VERBATIM, app_msg)
    tmpd(1) = day2(1)
    tmpt = time2
    res = newdate(date2, tmpd, tmpt, 3)

    ! print *, 'date 1 = ', date1
    ! print *, 'date 2 = ', date2

    ! call newdate(date1, day1, time1, -3)
    ! call newdate(date2, day2, time2, -3)

    ! print *, 'day1/time1: ', day1, time1
    ! print *, 'day2/time2: ', day2, time2

    call DIFDATr(date2, date1, diff_r)
    call DIFDATi(date2, date1, diff_i)
    write(app_msg, "('date1 = ', i12)") date1
    call app_log(APP_VERBATIM, app_msg)

    if (abs((diff_r - expected_diff_r) / expected_diff_r) > 1e-7) then
        write (app_msg, '(A, F11.6, A, F11.6)') 'Wrong time difference (real): ', diff_r, ', but expected ', expected_diff_r
        call app_log(APP_ERROR, app_msg)
        error stop 1
    end if

    if (diff_i /= expected_diff_i) then
        write (app_msg, '(A, F11.6, A, F11.6)') 'Wrong time difference (integer): ', diff_r, ', but expected ', expected_diff_r
        call app_log(APP_ERROR, app_msg)
        error stop 1
    end if

    write(app_msg, "('date1 = ', i12)") date1
    call app_log(APP_VERBATIM, app_msg)
    call INCDATr(date3_r, date1, num_hours)
    res = newdate(date3_r, day3_r, time3_r, -3)
    write (app_msg, "('date3_r = ', i12)") date3_r
    call app_log(APP_VERBATIM, app_msg)
    if ((day3_r(1) /= expected_day3_r(1)) .or. (time3_r /= expected_time3_r)) then
        write (app_msg, '(2(A, I9, "-", I8.8))') 'Wrong resulting date (rounded)! Got ', day3_r, time3_r, ', but expected ', expected_day3_r, expected_time3_r
        call app_log(APP_ERROR, app_msg)
        error stop 1
    end if

    call INCDATi(date3_i, date1, num_hours)
    res = newdate(date3_i, day3_i, time3_i, -3)
    if ((day3_i(1) /= expected_day3_i(1)) .or. (time3_i /= expected_time3_i)) then
        write (app_msg, '(2(A, I9, "-", I8.8))') 'Wrong resulting date (integer)! Got ', day3_i, time3_i, ', but expected ', expected_day3_i, expected_time3_i
        call app_log(APP_ERROR, app_msg)
        error stop 1
    end if
end program test_module_date
