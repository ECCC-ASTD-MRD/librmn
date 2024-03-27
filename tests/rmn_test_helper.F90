
module rmn_test_helper
    use app
    implicit none
contains

    subroutine check_status(status, expected, expected_min, expected_max, fail_message)
        implicit none
        integer, intent(in) :: status
        integer, intent(in), optional :: expected, expected_min, expected_max
        character(len=*), intent(in), optional :: fail_message

        logical :: error
        integer, dimension(3) :: expected_range

        expected_range(:) = 0
        error = .false.

        if (present(expected)) then
            expected_range(1) = expected
            if (status /= expected) error = .true.
        else if (present(expected_min)) then
            expected_range(2) = expected_min
            if (status < expected_min) error = .true.
        else if (present(expected_max)) then
            expected_range(3) = expected_max
            if (status > expected_max) error = .true.
        end if

        if (error) then
            write(app_msg, '(A, 1X, A, I12, A, 3I12)') fail_message, 'Got status = ', status, ', but expected ', expected_range
            call App_Log(APP_ERROR, app_msg)
            error stop 1
        end if
    end subroutine check_status

end module rmn_test_helper
