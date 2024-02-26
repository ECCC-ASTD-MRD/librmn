
module rmn_fst24
    use App
    use f_c_strings_mod
    use rmn_common
    use rmn_fst24_record
    use rmn_fstd_types
    implicit none

    include 'fst24_interface.inc'


    type :: fst_file
        private
        type(C_PTR) :: file_ptr = c_null_ptr ! Pointer to C file control structure
    contains
        procedure, nopass :: is_valid => fst24_file_is_valid
        procedure, pass   :: is_open => fst24_file_is_open
        procedure, pass   :: open => fst24_file_open
        procedure, pass   :: close => fst24_file_close
        procedure, pass   :: get_num_records => fst24_file_get_num_records

        procedure, pass :: set_search_criteria => fst24_file_set_search_criteria
        procedure, pass :: find_next => fst24_file_find_next
        procedure, pass :: find_all  => fst24_file_find_all
        procedure, pass :: read_next => fst24_file_read_next

        procedure, pass :: write => fst24_file_write

        procedure, pass :: checkpoint => fst24_file_checkpoint
        procedure, pass :: print_summary => fst24_file_print_summary
        procedure, pass :: unlink => fst24_file_unlink
    end type fst_file

    interface
        subroutine libc_free(ptr) BIND(C, name='free')
            import :: C_PTR
            implicit none
            type(C_PTR), intent(IN), value :: ptr
        end subroutine libc_free
    end interface

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
        class(fst_file), intent(in) :: this !< fst24_file instance
        logical :: is_open !< Whether this file is open
        
        integer(C_INT32_T) :: c_is_open

        is_open = .false.

        c_is_open = fst24_is_open(this % file_ptr)
        if (c_is_open == 1) is_open = .true.
    end function fst24_file_is_open

    function fst24_file_open(this, filename, options) result(could_open)
        class(fst_file),intent(inout)        :: this     !< fst24_file instance
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

        this % file_ptr = fst24_open(filename//achar(0), c_options)
        could_open = this % is_open()
    end function fst24_file_open

    function fst24_file_close(this) result(could_close)
        implicit none
        class(fst_file), intent(inout) :: this
        logical :: could_close

        integer(C_INT32_T) :: c_could_close
        could_close = .false.
        c_could_close = fst24_close(this % file_ptr)
        call libc_free(this % file_ptr)

        this % file_ptr = c_null_ptr
        if (c_could_close == 1) could_close = .true.
    end function fst24_file_close

    !> \return Number of record in file (including linked files). 0 if file is invalid or not open.
    !> \sa fst24_get_num_records
    function fst24_file_get_num_records(this) result(num_records)
        implicit none
        class(fst_file), intent(in) :: this
        integer(C_INT64_T) :: num_records
        num_records = fst24_get_num_records(this % file_ptr)
    end function fst24_file_get_num_records

    function fst24_file_set_search_criteria(this,                                                                   &
            dateo, datev, datyp, dasiz, npak, ni, nj, nk,                                                           &
            deet, npas, ip1, ip2, ip3, ig1, ig2, ig3, ig4, typvar, grtyp, nomvar, etiket) result(success)
        implicit none
        class(fst_file), intent(inout) :: this
        integer(C_INT64_T), intent(in), optional :: dateo, datev
        integer(C_INT32_T), intent(in), optional :: datyp, dasiz, npak, ni, nj, nk
        integer(C_INT32_T), intent(in), optional :: deet, npas, ip1, ip2, ip3, ig1, ig2, ig3, ig4
        character(len=2),  intent(in), optional :: typvar
        character(len=1),  intent(in), optional :: grtyp
        character(len=4),  intent(in), optional :: nomvar
        character(len=12), intent(in), optional :: etiket
        logical :: success

        type(fst_record_c), target :: criteria
        integer(C_INT32_T) :: c_status

        success = .false.

        if (present(dateo)) criteria % dateo = dateo
        if (present(datev)) criteria % datev = datev
        if (present(datyp)) criteria % datyp = datyp
        if (present(dasiz)) criteria % dasiz = dasiz
        if (present(npak)) criteria % npak = npak
        if (present(ni)) criteria % ni = ni
        if (present(nj)) criteria % nj = nj
        if (present(nk)) criteria % nk = nk
        if (present(deet)) criteria % deet = deet
        if (present(npas)) criteria % npas = npas
        if (present(ip1)) criteria % ip1 = ip1
        if (present(ip2)) criteria % ip2 = ip2
        if (present(ip3)) criteria % ip3 = ip3
        if (present(ig1)) criteria % ig1 = ig1
        if (present(ig2)) criteria % ig2 = ig2
        if (present(ig3)) criteria % ig3 = ig3
        if (present(ig4)) criteria % ig4 = ig4
        if (present(typvar)) call strncpy_f2c(typvar, criteria % typvar, 2)
        if (present(grtyp)) call strncpy_f2c(grtyp, criteria % grtyp, 1)
        if (present(nomvar)) call strncpy_f2c(nomvar, criteria % nomvar, 4)
        if (present(etiket)) call strncpy_f2c(etiket, criteria % etiket, 12)

        c_status = fst24_set_search_criteria(this % file_ptr, c_loc(criteria))

        if (c_status > 0) success = .true.

    end function fst24_file_set_search_criteria

    function fst24_file_find_next(this, record) result(found)
        implicit none
        class(fst_file), intent(in) :: this
        type(fst_record), intent(inout) :: record
        logical :: found

        integer(C_INT32_T) :: c_result

        found = .false.
        c_result = fst24_find_next(this % file_ptr, record % get_c_ptr())

        if (c_result > 0) then
            call record % from_c_self()
            ! call record % print()
            found = .true.
        end if
    end function fst24_file_find_next

    function fst24_file_find_all(this, records) result(num_found)
        implicit none
        class(fst_file), intent(in) :: this
        type(fst_record), dimension(:), intent(inout) :: records
        integer(C_INT32_T) :: num_found

        integer(C_INT32_T) :: max_num_records, c_status
        integer :: i

        num_found = 0

        c_status = fst24_rewind_search(this % file_ptr)
        if (c_status /= 1) return

        max_num_records = size(records)
        do i = 1, max_num_records
            if (.not. this % find_next(records(i))) return
            num_found = num_found + 1
        end do
    end function fst24_file_find_all

    function fst24_file_read_next(this, record) result(found)
        implicit none
        class(fst_file), intent(in) :: this
        type(fst_record), intent(inout) :: record
        logical :: found

        integer(C_INT32_T) :: c_result

        found = .false.
        c_result = fst24_read_next(this % file_ptr, record % get_c_ptr())

        if (c_result > 0) then
            call record % from_c_self()
            found = .true.
        end if
    end function fst24_file_read_next

    function fst24_file_write(this, record, rewrite) result(success)
        implicit none
        class(fst_file),  intent(inout) :: this
        type(fst_record), intent(inout) :: record
        logical, intent(in), optional     :: rewrite
        logical :: success

        integer(C_INT32_T) :: c_rewrite, c_status

        success = .false.

        call record % make_c_self()
        c_rewrite = 0
        if (present(rewrite)) then
            if (rewrite) c_rewrite = 1
        end if

        c_status = fst24_write(this % file_ptr, record % get_c_ptr(), c_rewrite)

        if (c_status > 0) success = .true.
    end function fst24_file_write

    function fst24_file_checkpoint(this) result(success)
        implicit none
        class(fst_file), intent(inout) :: this
        logical :: success

        integer(C_INT32_T) :: c_status

        success = .false.
        c_status = fst24_checkpoint(this % file_ptr)
        if (c_status >= 0) success = .true.
    end function fst24_file_checkpoint

    subroutine fst24_file_print_summary(this,                                                                       &
            dateo, datev, datestamps, level, datyp, ni, nj, nk,                                                     &
            deet, npas, ip1, ip2, ip3, decoded_ip, grid_info, ig1234, typvar, nomvar, etiket)
        implicit none
        class(fst_file), intent(in) :: this
        logical, intent(in), optional :: dateo, datev, datestamps, level, datyp, ni, nj, nk
        logical, intent(in), optional :: deet, npas, ip1, ip2, ip3, decoded_ip, grid_info, ig1234
        logical, intent(in), optional :: typvar, nomvar, etiket

        type(fst_record_fields), target :: fields
        integer(C_INT32_T) :: c_status

        fields = fst24_make_fields(dateo=dateo, datev=datev, datestamps=datestamps, level=level, datyp=datyp,           &
                                   ni=ni, nj=nj, nk=nk, deet=deet, npas=npas, ip1=ip1, ip2=ip2, ip3=ip3,                &
                                   decoded_ip=decoded_ip, grid_info=grid_info, ig1234=ig1234, typvar=typvar,            &
                                   nomvar=nomvar, etiket=etiket)
        c_status = fst24_print_summary(this % file_ptr, c_loc(fields))
    end subroutine fst24_file_print_summary

    function fst24_link(files) result(success)
        implicit none
        type(fst_file), dimension(:), intent(inout) :: files
        logical :: success

        type(C_PTR), dimension(size(files)), target :: c_files
        integer(C_INT32_T) :: num_files
        integer(C_INT32_T) :: c_status
        integer :: i

        success = .false.

        num_files = size(files)
        do i = 1, num_files
            c_files(i) = files(i) % file_ptr
        end do

        c_status = fst24_link_c(c_loc(c_files(1)), num_files)
        if (c_status > 0) success = .true.
    end function fst24_link

    function fst24_file_unlink(this) result(success)
        implicit none
        class(fst_file), intent(inout) :: this
        logical :: success

        integer(C_INT32_T) :: c_status

        success = .false.
        c_status = fst24_unlink(this % file_ptr)
        if (c_status > 0) success = .true.
    end function fst24_file_unlink

end module rmn_fst24
