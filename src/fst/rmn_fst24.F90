
module rmn_fst24
    use App
    use f_c_strings_mod
    use rmn_common
    use rmn_meta
    use rmn_fst24_record
    implicit none

    include 'fst24_interface.inc'
#include "c_fst98_interface.hf"

    type :: fst_file
        private
        type(C_PTR) :: file_ptr = c_null_ptr ! Pointer to C file control structure
    contains
        procedure, nopass :: is_valid => fst24_file_is_valid    !< \copydoc fst24_file_is_valid
        procedure, pass   :: is_open  => fst24_file_is_open     !< \copydoc fst24_file_is_open
        procedure, pass   :: open     => fst24_file_open        !< \copydoc fst24_file_open
        procedure, pass   :: close    => fst24_file_close       !< \copydoc fst24_file_close
        procedure, pass   :: get_num_records => fst24_file_get_num_records !< fst24_file_get_num_records 
        procedure, pass   :: get_unit => fst24_file_get_unit    !< \copydoc fst24_file_get_unit

        procedure, pass :: new_query => fst24_file_new_query !< \copydoc fst24_file_new_query
        procedure, pass :: read => fst24_file_read !< \copydoc fst24_file_read

        procedure, pass :: write => fst24_file_write    !< \copydoc fst24_file_write

        procedure, pass :: flush => fst24_file_flush !< \copydoc fst24_file_flush
        procedure, pass :: print_summary => fst24_file_print_summary !< \copydoc fst24_file_print_summary
        procedure, pass :: unlink => fst24_file_unlink  !< \copydoc fst24_file_unlink  

        ! Sequential files
        procedure, pass :: eof    => fst24_file_eof !< \copydoc fst24_file_eof
        procedure, pass :: weo    => fst24_file_weo !< \copydoc fst24_file_weo
        procedure, pass :: rewind => fst24_file_rwd !< \copydoc fst24_file_rwd
    end type fst_file

    type :: fst_query
        private
        type(C_PTR) :: query_ptr = c_null_ptr ! Pointer to C fst_query structure
    contains
        procedure, pass :: is_valid   => fst_query_is_valid     !< \copydoc fst_query_is_valid
        procedure, pass :: find_next  => fst_query_find_next    !< \copydoc fst_query_find_next
        procedure, pass :: find_all   => fst_query_find_all     !< \copydoc fst_query_find_all
        procedure, pass :: find_count => fst_query_find_count   !< \copydoc fst_query_find_count
        procedure, pass :: read_next  => fst_query_read_next    !< \copydoc fst_query_read_next
        procedure, pass :: rewind     => fst_query_rewind       !< \copydoc fst_query_rewind
        procedure, pass :: free       => fst_query_free         !< \copydoc fst_query_free
    end type fst_query

    !> Must match exactly the fst_query_options struct from C code
    type, bind(c), private :: fst_query_options_c
        private
        integer(C_INT32_T) :: ip1_all = 0
        integer(C_INT32_T) :: ip2_all = 0
        integer(C_INT32_T) :: ip3_all = 0
    end type fst_query_options_c


    interface
        subroutine libc_free(ptr) BIND(C, name='free')
            import :: C_PTR
            implicit none
            type(C_PTR), intent(IN), value :: ptr
        end subroutine libc_free
    end interface

contains

    !> Check whether the file at the given path is a valid standard file
    !> \return .true. if the given path is a valid standard file, .false. otherwise
    function fst24_file_is_valid(filename) result(is_valid)
        implicit none
        character(len=*), intent(in) :: filename
        logical :: is_valid

        integer(C_INT32_T) :: c_is_valid
        
        is_valid = .false.
        c_is_valid = fst24_is_valid(trim(filename) // achar(0))
        if (c_is_valid == 1) is_valid = .true.
    end function fst24_file_is_valid

    !> Check whether this file is open
    function fst24_file_is_open(this) result(is_open)
        implicit none
        class(fst_file), intent(in) :: this !< fst24_file instance
        logical :: is_open !< Whether this file is open

        integer(C_INT32_T) :: c_is_open

        is_open = .false.

        c_is_open = fst24_is_open(this % file_ptr)
        if (c_is_open == 1) is_open = .true.
    end function fst24_file_is_open


    !> \copybrief fst24_open
    function fst24_file_open(this, filename, options) result(could_open)
        class(fst_file),intent(inout)        :: this     !< fst_file instance. Must not be an already-open file
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
            c_options = options // achar(0)
        else
            c_options = 'RND+RSF+R/O' // achar(0)    ! Open a read-only RSF file by default
        end if
        this % file_ptr = fst24_open(trim(filename)//achar(0), c_options)
        could_open = this % is_open()
    end function fst24_file_open

    !> \copybrief fst24_close
    function fst24_file_close(this) result(could_close)
        implicit none
        class(fst_file), intent(inout) :: this  !< fst_file instance we want to close
        logical :: could_close                  !< Whether we were actually able to close it

        integer(C_INT32_T) :: c_could_close
        could_close = .false.
        c_could_close = fst24_close(this % file_ptr)
        call libc_free(this % file_ptr)

        this % file_ptr = c_null_ptr
        if (c_could_close == 1) could_close = .true.
    end function fst24_file_close

    !> \copybrief fst24_get_num_records
    !> \return Number of record in file (including linked files). 0 if file is invalid or not open.
    function fst24_file_get_num_records(this) result(num_records)
        implicit none
        class(fst_file), intent(in) :: this
        integer(C_INT64_T) :: num_records
        num_records = fst24_get_num_records(this % file_ptr)
    end function fst24_file_get_num_records

    !> \copybrief fst24_get_unit
    !> \return Unit of the file if open, 0 otherwise
    function fst24_file_get_unit(this) result(status)
        implicit none
        class(fst_file), intent(inout) :: this

        integer(C_INT32_T) :: status

        status = fst24_get_unit(this % file_ptr)
    end function fst24_file_get_unit

    !> \copybrief fst24_read
    !> \return .true. if we found a record, .false. if not or if error
    function fst24_file_read(this, record,                                                                          &
            dateo, datev, datyp, dasiz, npak, ni, nj, nk,                                                           &
            deet, npas, ip1, ip2, ip3, ig1, ig2, ig3, ig4, typvar, grtyp, nomvar, etiket, metadata,                 &
            ip1_all, ip2_all, ip3_all) result(found)
        implicit none
        class(fst_file), intent(inout) :: this
        type(fst_record), intent(inout) :: record !< Information of the record found. Left unchanged if nothing found

        integer(C_INT32_T), intent(in), optional :: dateo, datev
        integer(C_INT32_T), intent(in), optional :: datyp, dasiz, npak, ni, nj, nk
        integer(C_INT32_T), intent(in), optional :: deet, npas, ip1, ip2, ip3, ig1, ig2, ig3, ig4
        character(len=2),  intent(in), optional :: typvar
        character(len=1),  intent(in), optional :: grtyp
        character(len=4),  intent(in), optional :: nomvar
        character(len=12), intent(in), optional :: etiket
        logical, intent(in), optional :: ip1_all, ip2_all, ip3_all !< Whether we want to match any IP encoding
        type(meta), intent(in), optional :: metadata
        logical :: found

        type(fst_query) :: query

        found = .false.

        query = this % new_query(dateo, datev, datyp, dasiz, npak, ni, nj, nk, deet, npas,                          &
                                 ip1, ip2, ip3, ig1, ig2, ig3, ig4, typvar, grtyp, nomvar, etiket,                  &
                                 metadata, ip1_all, ip2_all, ip3_all)
        found = query % find_next(record)
    end function fst24_file_read

    !> \copybrief fst24_new_query
    !> \return A valid fst_query if the inputs are valid (open file, OK criteria struct), an invalid query otherwise
    function fst24_file_new_query(this,                                                                             & 
            dateo, datev, datyp, dasiz, npak, ni, nj, nk,                                                           &
            deet, npas, ip1, ip2, ip3, ig1, ig2, ig3, ig4, typvar, grtyp, nomvar, etiket, metadata,                 &
            ip1_all, ip2_all, ip3_all) result(query)
        implicit none
        class(fst_file), intent(inout) :: this
        integer(C_INT32_T), intent(in), optional :: dateo, datev
        integer(C_INT32_T), intent(in), optional :: datyp, dasiz, npak, ni, nj, nk
        integer(C_INT32_T), intent(in), optional :: deet, npas, ip1, ip2, ip3, ig1, ig2, ig3, ig4
        character(len=2),  intent(in), optional :: typvar
        character(len=1),  intent(in), optional :: grtyp
        character(len=4),  intent(in), optional :: nomvar
        character(len=12), intent(in), optional :: etiket
        logical, intent(in), optional :: ip1_all, ip2_all, ip3_all !< Whether we want to match any IP encoding
        type(meta), intent(in), optional :: metadata
        type(fst_query) :: query

        type(fst_record_c), target :: criteria
        type(fst_query_options_c), target :: options

        ! Criteria
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
        if (present(typvar)) call strncpy_f2c(typvar, criteria % typvar, 3)
        if (present(grtyp)) call strncpy_f2c(grtyp, criteria % grtyp, 2)
        if (present(nomvar)) call strncpy_f2c(nomvar, criteria % nomvar, 5)
        if (present(etiket)) call strncpy_f2c(etiket, criteria % etiket, 13)
        if (present(metadata)) criteria % metadata = metadata % json_obj

        ! Options
        if (present(ip1_all)) then
            if (ip1_all) options % ip1_all = 1
        end if
        if (present(ip2_all)) then
            if (ip2_all) options % ip2_all = 1
        end if
        if (present(ip3_all)) then
            if (ip3_all) options % ip3_all = 1
        end if

        query % query_ptr = fst24_new_query(this % file_ptr, c_loc(criteria), c_loc(options))

    end function fst24_file_new_query

    !> \copybrief fst24_find_next
    !> \return .true. if we found a record, .false. if not or if error
    function fst_query_find_next(this, record) result(found)
        implicit none
        class(fst_query), intent(in) :: this                !< Query we are using for the search
        type(fst_record), intent(inout), optional :: record !< Information of the record found. Left unchanged if nothing found
        type(C_PTR) :: c_record
        logical :: found

        integer(C_INT32_T) :: c_result

        c_record= C_NULL_PTR;
        if (present(record)) then
            c_record = record % get_c_ptr()
        end if

        found = .false.
        c_result = fst24_find_next(this % query_ptr, c_record)

        if (c_result > 0) then
            if (present(record)) call record % from_c_self()
            ! call record % print()
            found = .true.
        end if
    end function fst_query_find_next

    !> \copybrief fst24_find_all
    !> \return Number of records found, up to size(records)
    function fst_query_find_all(this, records) result(num_found)
        implicit none
        class(fst_query), intent(inout) :: this     !< Query used for the search
        !> [in,out] Array where the records found will be put. On ly number of matches returned if not present.
        !> We stop searching after we found enough records to fill it.
        type(fst_record), dimension(:), intent(inout), optional :: records
        integer(C_INT32_T) :: num_found

        integer(C_INT32_T) :: max_num_records, c_status
        integer :: i

        num_found = 0

        call this % rewind()

        max_num_records = 2147483647
        if (present(records)) then
           max_num_records = size(records)
        endif

        do i = 1, max_num_records
            if (present(records)) then
               if (.not. this % find_next(records(i))) return
            else 
               if (.not. this % find_next()) return
            endif
            num_found = num_found + 1
        end do
    end function fst_query_find_all

    !> \copybrief fst24_find_count
    !> \return Number of records found
    function fst_query_find_count(this) result(num_found)
        implicit none
        class(fst_query), intent(inout) :: this     !< Query used for the search
        integer(C_INT32_T) :: num_found

        num_found = fst24_find_count(this % query_ptr)
    end function fst_query_find_count

    !> \copybrief fst24_read_next
    !> \return .true. if we read a record, .false. if none found or if error
    function fst_query_read_next(this, record) result(found)
        implicit none
        class(fst_query), intent(in)    :: this     !< Query used for the search
        type(fst_record), intent(inout) :: record   !< Record that was read (left unchanged if nothing was found)
        logical :: found

        integer(C_INT32_T) :: c_result

        found = .false.
        c_result = fst24_read_next(this % query_ptr, record % get_c_ptr())

        if (c_result > 0) then
            call record % from_c_self()
            found = .true.
        end if
    end function fst_query_read_next

    !> \copybrief fst24_write
    !> \return Whether the write was successful
    function fst24_file_write(this, record, rewrite) result(success)
        implicit none
        class(fst_file),  intent(inout) :: this     !< File where we want to write
        type(fst_record), intent(inout) :: record   !< Record we want to write
        logical, intent(in), optional     :: rewrite!< Whether we want to rewrite an existing record (default .false.)
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

    !> \copybrief fst24_flush
    !> \return Whether the underlying call was successful
    function fst24_file_flush(this) result(success)
        implicit none
        class(fst_file), intent(inout) :: this
        logical :: success

        integer(C_INT32_T) :: c_status

        success = .false.
        c_status = fst24_flush(this % file_ptr)
        if (c_status >= 0) success = .true.
    end function fst24_file_flush

    !> \copybrief fst24_print_summary
    !> All optional parameters are booleans determining whether we print the corresponding field.
    subroutine fst24_file_print_summary(this,                                                                       &
            dateo, datev, datestamps, level, datyp, nijk,                                                     &
            deet, npas, ip1, ip2, ip3, decoded_ip, grid_info, ig1234, typvar, nomvar, etiket, metadata, string)
        implicit none
        class(fst_file), intent(in) :: this
        logical, intent(in), optional :: dateo, datev, datestamps, level, datyp, nijk
        logical, intent(in), optional :: deet, npas, ip1, ip2, ip3, decoded_ip, grid_info, ig1234
        logical, intent(in), optional :: typvar, nomvar, etiket, metadata
        character(len=*), intent(in), optional :: string

        type(fst_record_fields), target :: fields
        integer(C_INT32_T) :: c_status

        if (present(string)) then
            fields = fst24_make_fields_from_string(string)
        else 
            fields = fst24_make_fields(dateo=dateo, datev=datev, datestamps=datestamps, level=level, datyp=datyp,           &
                                   nijk=nijk, deet=deet, npas=npas, ip1=ip1, ip2=ip2, ip3=ip3,                &
                                   decoded_ip=decoded_ip, grid_info=grid_info, ig1234=ig1234, typvar=typvar,            &
                                   nomvar=nomvar, etiket=etiket, metadata=metadata)
        end if
        c_status = fst24_print_summary(this % file_ptr, c_loc(fields))
    end subroutine fst24_file_print_summary

    !> Link the given files so that they can be searched and read as one.
    !> \return Whether the linking was successful
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

    !> Unlink files that are linked into the given file
    !> \return Whether the unlinking was successful
    function fst24_file_unlink(this) result(success)
        implicit none
        class(fst_file), intent(inout) :: this !< File to unlink. Must be the first in the list when linking occurred
        logical :: success

        integer(C_INT32_T) :: c_status

        success = .false.
        c_status = fst24_unlink(this % file_ptr)
        if (c_status > 0) success = .true.
    end function fst24_file_unlink

    !> \copydoc c_fsteof
    !> Only works with sequential files
    function fst24_file_eof(this) result(status)
        implicit none
        class(fst_file), intent(inout) :: this

        integer(C_INT32_T) :: status

        status = c_fsteof(fst24_get_unit(this % file_ptr))
    end function fst24_file_eof

    !> \copydoc c_fstweo
    !> Only works with sequential files
    function fst24_file_weo(this,level) result(status)
        implicit none
        class(fst_file), intent(inout) :: this
        integer, intent(in) :: level 

        integer(C_INT32_T) :: status

        status = c_fstweo(fst24_get_unit(this % file_ptr), level)
    end function fst24_file_weo

    !> \copydoc c_fstrwd
    !> Only works with sequential files
    function fst24_file_rwd(this) result(status)
        implicit none
        class(fst_file), intent(inout) :: this

        integer(C_INT32_T) :: status

        status = c_fstrwd(fst24_get_unit(this % file_ptr))
    end function fst24_file_rwd

    pure function fst_query_is_valid(this) result(is_valid)
        implicit none
        class(fst_query), intent(in) :: this
        logical :: is_valid
        integer(C_INT32_T) :: is_valid_c
        is_valid = .false.
        if (c_associated(this % query_ptr)) then
            is_valid_c = fst24_query_is_valid(this % query_ptr)
            if (is_valid_c == 1) is_valid = .true.
        end if
    end function fst_query_is_valid

    !> \copydoc fst24_rewind_search
    subroutine fst_query_rewind(this)
        implicit none
        class(fst_query), intent(inout) :: this
        integer(C_INT32_T) :: c_status 
        if (this % is_valid()) c_status = fst24_rewind_search(this % query_ptr)
    end subroutine fst_query_rewind

    !> \copydoc fst24_query_free
    subroutine fst_query_free(this)
        implicit none
        class(fst_query), intent(inout) :: this
        if (this % is_valid()) then
            call fst24_query_free(this % query_ptr)
            this % query_ptr = c_null_ptr
        end if
    end subroutine fst_query_free

    function fst24_is_default_query_options_valid() result(is_valid)
        implicit none
        logical :: is_valid

        integer(C_INT32_T) :: c_status
        type(fst_query_options_c), target :: to_compare

        is_valid = .false.

        c_status = fst24_validate_default_query_options(c_loc(to_compare), storage_size(to_compare, kind = int64) / 8_int64)
        if (c_status == 0) is_valid = .true.

        if (.not. is_valid) then
            call Lib_Log(APP_LIBFST, APP_ERROR, 'Default FST query options struct is not valid, check your library versions!')
        end if
    end function fst24_is_default_query_options_valid
end module rmn_fst24
