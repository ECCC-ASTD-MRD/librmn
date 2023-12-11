program test_module_date
    use app
    use rmn_common
    implicit none


    integer, parameter :: day1 = 20231211
    integer, parameter :: day2 = 20231225
    integer, parameter :: time1 = 12383700
    integer, parameter :: time2 = 00000000

    real(kind = real64), parameter :: num_hours = 323.35

    integer, parameter :: expected_day3_r  = 20231224
    integer, parameter :: expected_time3_r = 23593500
    integer, parameter :: expected_day3_i  = 20231225
    integer, parameter :: expected_time3_i = 0

    real(kind = real64), parameter :: expected_diff_r = (((13 * 24 + 11) * 60 + 21) * 60 + 25) / 3600.0
    real(kind = real64), parameter :: expected_diff_i = 323.0

    integer :: date1, date2, date3_r, date3_i
    integer :: day3_i, time3_i, day3_r, time3_r
    real(kind = real64) :: diff_r, diff_i

    call newdate(date1, day1, time1, 3)
    call newdate(date2, day2, time2, 3)

    ! print *, 'date 1 = ', date1
    ! print *, 'date 2 = ', date2

    ! call newdate(date1, day1, time1, -3)
    ! call newdate(date2, day2, time2, -3)
    
    ! print *, 'day1/time1: ', day1, time1
    ! print *, 'day2/time2: ', day2, time2

    call DIFDATr(date2, date1, diff_r)
    call DIFDATi(date2, date1, diff_i)

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

    call INCDATr(date3_r, date1, num_hours)
    call INCDATi(date3_i, date1, num_hours)

    ! print *, 'date 3 r = ', date3_r
    ! print *, 'date 3 i = ', date3_i

    call newdate(date3_r, day3_r, time3_r, -3)
    call newdate(date3_i, day3_i, time3_i, -3)

    if ((day3_r /= expected_day3_r) .or. (time3_r /= expected_time3_r)) then
        write (app_msg, '(2(A, I9, "-", I8.8))') 'Wrong resulting date! Got ', day3_r, time3_r, ', but expected ', expected_day3_r, expected_time3_r
        call app_log(APP_ERROR, app_msg)
        error stop 1
    end if

    if ((day3_i /= expected_day3_i) .or. (time3_i /= expected_time3_i)) then
        write (app_msg, '(2(A, I9, "-", I8.8))') 'Wrong resulting date! Got ', day3_i, time3_i, ', but expected ', expected_day3_i, expected_time3_i
        call app_log(APP_ERROR, app_msg)
        error stop 1
    end if

    ! print *, 'Diff_r = ', diff_r
    ! print *, 'Diff_i = ', diff_i
    ! print *, 'date 3 r = ', date3_r, day3_r, time3_r
    ! print *, 'date 3 i = ', date3_i, day3_i, time3_i

    ! print *, 'date 1 = ', date1
    ! print *, 'date 2 = ', date2

end program test_module_date
