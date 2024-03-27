
interface
    ! FST record functions

    function fst24_record_new_c(data, type, nbits, ni, nj, nk) result(record) bind(C, name = 'fst24_record_new')
        import :: C_PTR, C_INT32_T
        implicit none
        type(C_PTR), intent(in), value :: data
        type(C_PTR) :: record
        integer(C_INT32_T), intent(in), value :: type, nbits, ni, nj, nk
        integer(C_INT32_T) :: status
    end function fst24_record_new_c

    function fst24_validate_default_record(fortran_record, fortran_size) result(status) bind(C, name = 'fst24_validate_default_record')
        import :: C_PTR, C_INT32_T, C_SIZE_T
        implicit none
        type(C_PTR), intent(in), value :: fortran_record
        integer(C_SIZE_T), intent(in), value :: fortran_size
        integer(C_INT32_T) :: status
    end function fst24_validate_default_record

    subroutine fst24_record_print_c(record) bind(c, name = 'fst24_record_print')
        import :: C_PTR
        implicit none
        type(C_PTR), intent(in), value :: record
    end subroutine fst24_record_print_c

    subroutine fst24_record_print_short_c(record, fields, print_header, prefix) bind(c, name = 'fst24_record_print_short')
        import :: C_PTR, C_CHAR, C_INT32_T
        implicit none
        type(C_PTR), intent(in), value :: record
        type(C_PTR), intent(in), value :: fields
        integer(C_INT32_T), intent(in), value :: print_header
        character(C_CHAR), dimension(*), intent(in) :: prefix
    end subroutine fst24_record_print_short_c

    function fst24_record_has_same_info_c(a, b) result(status) bind(C, name = 'fst24_record_has_same_info')
        import :: C_PTR, C_INT32_T
        implicit none
        type(C_PTR), intent(in), value :: a, b
        integer(C_INT32_T) :: status
    end function fst24_record_has_same_info_c

    function fst24_read(record) result(status) bind(C, name = 'fst24_read')
        import :: C_PTR, C_INT32_T
        implicit none
        type(C_PTR), intent(in), value :: record
        integer(C_INT32_T) :: status
    end function fst24_read

    function fst24_read_metadata(record) result(metadata) bind(C, name = 'fst24_read_metadata')
        import :: C_PTR
        implicit none
        type(C_PTR), intent(in), value :: record
        type(C_PTR) :: metadata
    end function fst24_read_metadata

    ! FST file functions

    function fst24_is_valid(file_name) result(is_valid) bind(C, name = 'fst24_is_valid')
        import :: C_CHAR, C_INT32_T
        implicit none
        character(C_CHAR), dimension(*), intent(in) :: file_name
        integer(C_INT32_T) :: is_valid
    end function fst24_is_valid

    function fst24_is_open(file) result(is_open) bind(C, name = 'fst24_is_open')
        import :: C_PTR, C_INT32_T
        implicit none
        type(C_PTR), intent(in), value :: file
        integer(C_INT32_T) :: is_open
    end function fst24_is_open

    function fst24_open(file_name, options) result(file_ptr) bind(C, name = 'fst24_open')
        import :: C_PTR, C_CHAR
        implicit none
        character(C_CHAR), dimension(*), intent(in) :: file_name
        character(C_CHAR), dimension(*), intent(in) :: options
        type(C_PTR) :: file_ptr
    end function fst24_open

    function fst24_close(file) result(could_close) bind(C, name = 'fst24_close')
        import :: C_PTR, C_INT32_T
        implicit none
        type(C_PTR), intent(in), value :: file
        integer(C_INT32_T) :: could_close
    end function fst24_close

    function fst24_get_num_records(file) result(num_records) bind(C, name = 'fst24_get_num_records')
        import :: C_INT64_T, C_PTR
        implicit none
        type(C_PTR), intent(in), value :: file
        integer(C_INT64_T) :: num_records
    end function fst24_get_num_records

    function fst24_new_query(file, criteria) result(query) bind(C, name = 'fst24_new_query')
        import :: C_PTR
        implicit none
        type(C_PTR), intent(in), value :: file
        type(C_PTR), intent(in), value :: criteria
        type(C_PTR) :: query
    end function fst24_new_query

    function fst24_rewind_search(query) result(status) bind(C, name = 'fst24_rewind_search')
        import :: C_PTR, C_INT32_T
        implicit none
        type(C_PTR), intent(in), value :: query
        integer(C_INT32_T) :: status
    end function fst24_rewind_search

    function fst24_find_next(query, record) result(status) bind(C, name = 'fst24_find_next')
        import :: C_PTR, C_INT32_T
        implicit none
        type(C_PTR), intent(in), value :: query
        type(C_PTR), intent(in), value :: record
        integer(C_INT32_T) :: status
    end function fst24_find_next

    function fst24_find_all(query, records, max_num_results) result(num_found) bind(C, name = 'fst24_find_all')
        import :: C_PTR, C_INT32_T
        implicit none
        type(C_PTR), intent(in), value :: query
        type(C_PTR), intent(in), value :: records
        integer(C_INT32_T), intent(in), value :: max_num_results
        integer(C_INT32_T) :: num_found
    end function fst24_find_all

    function fst24_read_next(query, record) result(status) bind(C, name = 'fst24_read_next')
        import :: C_PTR, C_INT32_T
        implicit none
        type(C_PTR), intent(in), value :: query
        type(C_PTR), intent(in), value :: record
        integer(C_INT32_T) :: status
    end function fst24_read_next

    function fst24_write(file, record, rewrite) result(status) bind(C, name = 'fst24_write')
        import :: C_PTR, C_INT32_T
        implicit none
        type(C_PTR), intent(in), value :: file
        type(C_PTR), intent(in), value :: record
        integer(C_INT32_T), intent(in), value :: rewrite
        integer(C_INT32_T) :: status
    end function fst24_write

    function fst24_flush(file) result(status) bind(C, name = 'fst24_flush')
        import :: C_PTR, C_INT32_T
        implicit none
        type(C_PTR), intent(in), value :: file
        integer(C_INT32_T) :: status
    end function fst24_flush

    function fst24_print_summary(file, fields) result(status) bind(C, name = 'fst24_print_summary')
        import :: C_PTR, C_INT32_T
        implicit none
        type(C_PTR), intent(in), value :: file
        type(C_PTR), intent(in), value :: fields
        integer(C_INT32_T) :: status
    end function fst24_print_summary

    function fst24_link_c(files, num_files) result(status) bind(C, name = 'fst24_link')
        import :: C_PTR, C_INT32_T
        implicit none
        type(C_PTR), intent(in), value :: files
        integer(C_INT32_T), intent(in), value :: num_files
        integer(C_INT32_T) :: status
    end function fst24_link_c

    function fst24_unlink(file) result(status) bind(C, name = 'fst24_unlink')
        import :: C_PTR, C_INT32_T
        implicit none
        type(C_PTR), intent(in), value :: file
        integer(C_INT32_T) :: status
    end function fst24_unlink

    function fst24_get_unit(file) result(iun) bind(C, name = 'fst24_get_unit')
        import :: C_PTR, C_INT32_T
        implicit none
        type(C_PTR), intent(in), value :: file
        integer(C_INT32_T) :: iun
    end function fst24_get_unit

    subroutine fst24_query_free(query) bind(C, name = 'fst24_query_free')
        import :: C_PTR
        implicit none
        type(C_PTR), intent(in), value :: query
    end subroutine fst24_query_free

    pure function fst24_query_is_valid(query) result(is_valid) bind(C, name = 'fst24_query_is_valid')
        import :: C_PTR, C_INT32_T
        implicit none
        type(C_PTR), intent(in), value :: query
        integer(C_INT32_T) :: is_valid
    end function fst24_query_is_valid
end interface
