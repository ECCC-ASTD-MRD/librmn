
module rmn_fstd23
    use App
    use rmn_common
    use rmn_fst_record
    use rmn_fstd98
    implicit none

    include 'fst24_interface.inc'

    type, extends(fstd98) :: fstd23
        private
        type(C_PTR) :: file_struct = c_null_ptr ! Pointer to C file control structure
    contains
        procedure, pass :: open => fstd23_open
        procedure, pass :: is_open => fstd23_is_open
        procedure, pass :: close => fstd23_close
        procedure, pass :: frm => fstd23_frm            ! Override frm from fstd98 to avoid issues
        final :: fstd23_final
    end type fstd23

contains
    subroutine fstd23_open(this, filename, options)
        class(fstd23),    intent(inout)        :: this     !< fstd23 instance
        character(len=*), intent(in)           :: filename !< Name of the file we want to open
        character(len=*), intent(in), optional :: options  !< Additional options to pass

        character(len=:), allocatable :: c_options

        if (present(options)) then
            c_options = trim(options) // achar(0)
        else
            c_options = 'RND+RSF+R/O' // achar(0)    ! Open a read-only RSF file by default
        end if

        this % file_struct = fst24_open(filename//achar(0), c_options)

        if (c_associated(this % file_struct)) then
            ! Query iun, to be able to use the old interface
            this % iun = fst24_get_iun(this % file_struct)

            write(app_msg, '(A, A, A, I6)') 'Opened file ', filename, ', iun = ', this % iun
            call lib_log(APP_LIBFST, APP_DEBUG, app_msg)
        end if
    end subroutine

    pure function fstd23_is_open(this) result(is_open)
        implicit none
        class(fstd23), intent(in) :: this
        logical :: is_open

        integer(C_INT32_T) :: c_is_open

        is_open = .false.
        c_is_open = fst24_is_open(this % file_struct)

        if (c_is_open == 1) is_open = .true.
    end function fstd23_is_open

    subroutine fstd23_close(this)
        implicit none
        class(fstd23), intent(inout) :: this

        integer(C_INT32_T) :: status
        
        if (this % is_open()) then
            status = fst24_close(this % file_struct)
            this % iun = -1
        end if
    end subroutine fstd23_close

    function fstd23_frm(this) result(status)
        implicit none
        class(fstd23), intent(inout) :: this
        integer(C_INT32_T) :: status
        call this % close()
        status = 0
    end function fstd23_frm

    subroutine fstd23_final(this)
        implicit none
        type(fstd23), intent(inout) :: this
        call this % close()
    end subroutine fstd23_final
end module rmn_fstd23
