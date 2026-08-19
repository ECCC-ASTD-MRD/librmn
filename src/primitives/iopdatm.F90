!> \file

!> Compute a timestamp based on the input string. 
function iopdatm(date_string) result(timestamp)
    use app
    use rmn_date
    implicit none

    !> Date as a string of characters with format YYDDDHH
    character(len = *), intent(in) :: date_string
    !> The returned timestamp, in some format. 10101011 if there was any error.
    integer :: timestamp

    integer :: date_val
    integer :: date_year, date_month, date_day, yyyymmdd(1), time
    integer :: jd
    integer :: dummy
    integer :: ios

    read(date_string, '(I10)', iostat = ios) date_val
    if (ios == 0) then
        if (date_val < 9936624) then  ! it is in YYJJJHH format
            date_year = 1900 + date_val / 100000
            if (date_year < 1950) date_year = date_year + 100
            call jdatec(jd, date_year, 1, 1)
            call datec(jd + mod(date_val / 100, 1000) - 1, date_year, date_month, date_day)
            yyyymmdd(1) = date_year * 10000 + date_month * 100 + date_day
            time = mod(date_val, 100) * 1000000
            dummy = newdate(date_val, yyyymmdd, time, 3)
            timestamp = date_val
            return
        endif
    end if

    call lib_log(APP_LIBRMN, APP_ERROR, 'iopdatm: Unable to read date')
    timestamp = 010101011
end function iopdatm
