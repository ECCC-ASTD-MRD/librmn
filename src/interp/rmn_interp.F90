module rmn_interp
    use app
    use iso_c_binding
    implicit none
    private

    public :: ezqkdef, ezdefset, ezgetopt, ezsetopt

    interface
        function c_ezqkdef(ni, nj, grtyp, ig1, ig2, ig3, ig4, iunit) result(status) bind(C, name = 'c_ezqkdef')
            import :: C_INT32_T, C_CHAR
            implicit none
            integer(C_INT32_T), intent(in), value :: ni, nj
            character(C_CHAR), dimension(*), intent(in) :: grtyp
            integer(C_INT32_T), intent(in), value :: ig1, ig2, ig3, ig4
            integer(C_INT32_T), intent(in), value :: iunit
            integer(C_INT32_T) :: status
        end function c_ezqkdef

        function ezdefset(gdout, gdin) result(status) bind(C, name = 'c_ezdefset')
            import C_INT32_T
            implicit none
            integer(C_INT32_T), intent(in), value :: gdout, gdin
            integer(C_INT32_T) :: status
        end function ezdefset

        function c_ezgetopt(option, opt_value) result(status) bind(C, name = 'c_ezgetopt')
            import :: C_CHAR, C_INT32_T
            implicit none
            character(C_CHAR), dimension(*), intent(in)  :: option
            character(C_CHAR), dimension(*), intent(out) :: opt_value
            integer(C_INT32_T) :: status
        end function c_ezgetopt

        function c_ezsetopt(option, opt_value) result(status) bind(C, name = 'c_ezsetopt')
            import :: C_CHAR, C_INT32_T
            implicit none
            character(C_CHAR), dimension(*), intent(in) :: option
            character(C_CHAR), dimension(*), intent(in) :: opt_value
            integer(C_INT32_T) :: status
        end function c_ezsetopt

    end interface
contains

    function ezqkdef(ni, nj, grtyp, ig1, ig2, ig3, ig4, iunit) result(status)
        implicit none
        integer(C_INT32_T), intent(in) :: ni, nj
        character(len=*),   intent(in) :: grtyp
        integer(C_INT32_T), intent(in) :: ig1, ig2, ig3, ig4
        integer(C_INT32_T), intent(in) :: iunit
        integer(C_INT32_T) :: status
        status = c_ezqkdef(ni, nj, grtyp(1:1) // c_null_char, ig1, ig2, ig3, ig4, iunit)
    end function ezqkdef

    function ezgetopt(option, opt_value) result(status)
        implicit none
        character(len=*), intent(in)  :: option
        character(len=*), intent(out) :: opt_value
        integer(C_INT32_T) :: status

        integer, parameter :: max_val_length = 32
        character(len=max_val_length) :: local_value
        character(len=:), allocatable :: trimmed_option

        trimmed_option = trim(option)
        if (len(trimmed_option) < max_val_length) then
            status = c_ezgetopt(trimmed_option//c_null_char, local_value)
        else
            status = c_ezgetopt(trimmed_option(1:max_val_length - 1)//c_null_char, local_value)
        end if

        if (len(opt_value) < max_val_length) then
            write(app_msg, '(A, I2, A, I2, A)')        &
                'Character string (len ', len(opt_value),           &
                ') passed to ezgetopt might be too short for the max value length (', max_val_length, ')'
            call lib_log(APP_LIBRMN, APP_WARNING, app_msg)
        end if

        opt_value = trim(local_value)
    end function ezgetopt

    function ezsetopt(option, opt_value) result(status)
        implicit none
        character(len=*), intent(in) :: option
        character(len=*), intent(in) :: opt_value
        integer(C_INT32_T) :: status
        status = c_ezsetopt(option//c_null_char, opt_value//c_null_char)
    end function ezsetopt
end module rmn_interp
