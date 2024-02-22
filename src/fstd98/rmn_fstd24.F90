
module rmn_fst24
    use App
    use rmn_common
    use rmn_fst_record
    implicit none

    include 'fst24_interface.inc'

    type :: fst24_file
        private
        type(C_PTR) :: file_struct = c_null_ptr ! Pointer to C file control structure
    contains
        procedure, nopass :: is_valid => fst24_file_is_valid
        procedure, pass   :: is_open => fst24_file_is_open
        procedure, pass   :: open => fst24_file_open
        procedure, pass   :: close => fst24_file_close
    end type fst24_file

contains

    !> \result Check whether the given path is a valid standard file
    function fst24_file_is_valid(filename) result(is_valid)
        implicit none
        character(len=*), intent(in) :: filename
        logical :: is_valid

        integer(C_INT32_T) :: c_is_valid
        
        is_valid = .false.
        c_is_valid = fst24_is_valid(trim(filename) // achar(0))
        if (c_is_valid == 1) is_valid = .true.
    end function fst24_file_is_valid

    !> \result Check whether this file is open
    function fst24_file_is_open(this) result(is_open)
        implicit none
        class(fst24_file), intent(in) :: this !< fst24_file instance
        logical :: is_open !< Whether this file is open
        
        integer(C_INT32_T) :: c_is_open

        is_open = .false.

        c_is_open = fst24_is_open(this % file_struct)
        if (c_is_open == 1) is_open = .true.
    end function fst24_file_is_open

    function fst24_file_open(this, filename, options) result(could_open)
        class(fst24_file),intent(inout)        :: this     !< fst24_file instance
        character(len=*), intent(in)           :: filename !< Name of the file we want to open
        character(len=*), intent(in), optional :: options  !< Additional options to pass

        logical :: could_open  !< Whether we were able to open the file
        character(len=:), allocatable :: c_options

        could_open = .false.
        if (this % is_open()) then
            call lib_log(APP_LIBFST, APP_ERROR, "You need to close file before opening a new one with the same object")
            return
        end if

        if (present(options)) then
            c_options = 'RND+RSF+' // trim(options) // achar(0)
        else
            c_options = 'RND+RSF+R/O' // achar(0)    ! Open a read-only RSF file by default
        end if

        this % file_struct = fst24_open(filename//achar(0), c_options)
        could_open = this % is_open()
    end function fst24_file_open

    function fst24_file_close(this) result(could_close)
        implicit none
        class(fst24_file), intent(inout) :: this
        logical :: could_close

        integer(C_INT32_T) :: c_could_close
        could_close = .false.
        c_could_close = fst24_close(this % file_struct)

        this % file_struct = c_null_ptr
        if (c_could_close == 1) could_close = .true.
    end function fst24_file_close

end module rmn_fst24
