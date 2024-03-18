
module test_fst24_interface_module
    use App
    use rmn_common
    use rmn_fst24
    use rmn_meta
    implicit none

    character(len=*), dimension(3), parameter :: test_file_names = [    &
                'fst24_interface1.fst',                                 &
                'fst24_interface2.fst',                                 &
                'fst24_interface3.fst'                                  &
    ]
    character(len=2000) :: cmd
    integer, parameter :: DATA_SIZE = 1024

    real(real32), dimension(:,:), pointer :: test_data
    type(fst_record) :: test_record

contains

subroutine make_test_data()
    implicit none
    integer :: i, j
    real(real32) :: powi, powj, num, denom
    if (.not. associated(test_data)) then
        allocate(test_data(DATA_SIZE, DATA_SIZE))
    end if
    do j = 1, DATA_SIZE
        powj    = ((1.0 * j) / DATA_SIZE) ** 2
        do i = 1, DATA_SIZE
            powi    = ((1.0 * i) / DATA_SIZE) ** 2
            num     = 1 + cos(12 * sqrt(powi + powj))
            denom   = 0.5 * (powi + powj) + 2
            test_data(i, j) = num / denom
        end do
    end do
end subroutine make_test_data

subroutine delete_test_data()
    implicit none
    if (associated(test_data)) then
        deallocate(test_data)
        nullify(test_data)
    end if
end subroutine delete_test_data

subroutine make_test_record()
    implicit none
    integer(C_INT32_T) :: status

    call make_test_data()

    test_record % data = c_loc(test_data)
    test_record % npak = -32
    test_record % ni   = DATA_SIZE
    test_record % nj   = DATA_SIZE
    test_record % nk   = 1
    test_record % dateo= 20220610
    test_record % deet = 300
    test_record % npas = 0
    test_record % ip1  = 1
    test_record % ip2  = 10
    test_record % ip3  = 100
    test_record % typvar = 'P'
    test_record % nomvar = 'WAVE'
    test_record % etiket = 'float'
    test_record % grtyp  = 'X'
    test_record % ig1   = 0
    test_record % ig2   = 0
    test_record % ig3   = 0
    test_record % ig4   = 0
    test_record % datyp = FST_TYPE_REAL
    test_record % dasiz = 32

    status = test_record % metadata % init(META_TYPE_RECORD,"")
    write(6,*) test_record % metadata % stringify()
end subroutine make_test_record

function check_content(content, expected) result(success)
    real(kind = real32), dimension(:, :), intent(in) :: content, expected
    logical :: success

    integer, dimension(2) :: shape_a, shape_b
    integer :: i, j

    success = .false.

    shape_a = shape(content)
    shape_b = shape(expected)
    if (any(shape_a /= shape_b)) then
        call app_log(APP_ERROR, 'Not even the same shape!')
        return 
    end if
    
    do j = 1, shape_a(2)
        do i = 1, shape_a(1)
            if (content(i, j) /= expected(i, j)) then
                call app_log(APP_ERROR, 'AAAhhhhhh content not same as expected!')
                return
            end if
        end do
    end do

    success = .true.
end function check_content

function create_file(name, is_rsf, ip2, ip3) result(success)
    implicit none
    character(len=*), intent(in) :: name
    logical, intent(in) :: is_rsf
    integer, intent(in) :: ip2, ip3
    logical :: success

    character(len=:), allocatable :: options
    type(fst_file) :: new_file
    type(fst_record) :: new_record

    success = .false.

    ! Remove file(s) so that we have a fresh start
    write(cmd, '(A, (1X, A))') 'rm -fv ', name
    call execute_command_line(trim(cmd))

    options = 'RND+R/W'
    if (is_rsf) options = options // '+RSF'

    success = new_file % open(trim(name), options)
    if (.not. success) then
        write(app_msg, '(A, A, A, A)') 'Unable to open new test file with name ', trim(name), ' and options ', trim(options)
        call App_Log(APP_ERROR, app_msg)
        return
    end if

    if (.not. new_file % is_open()) then
        write(app_msg, '(A, A)') 'File is not open! ', trim(name)
        call app_log(APP_ERROR, app_msg)
        return
    end if

    ! /////////////////////////////////////
    ! // Write a record
    new_record = test_record
    new_record % ip2 = ip2
    new_record % ip3 = ip3

    success = new_file % write(new_record)
    if (.not. success) then
        write(app_msg, '(A, A)') 'Unable to write record (1) to new file ', trim(name)
        call app_log(APP_ERROR, app_msg)
        return
    end if

    success = new_file % flush()
    if (.not. success) then
        call app_log(APP_ERROR, 'Flush failed')
        return
    end if

    new_record % ip1 = new_record % ip1 + 1
    success = new_file % write(new_record)
    if (.not. success) then
        write(app_msg, '(A, A)') 'Unable to write record (2) to new file ', trim(name)
        call app_log(APP_ERROR, app_msg)
        return
    end if

    new_record % ip1 = new_record % ip1 + 1
    success = new_file % write(new_record)
    if (.not. success) then
        write(app_msg, '(A, A)') 'Unable to write record (3) to new file ', trim(name)
        call app_log(APP_ERROR, app_msg)
        return
    end if

    ! ///////////////////////////
    ! // Close the new file
    success = new_file % close()
    if (.not. success) then
        write(app_msg, '(A, A)') 'Unable to close new file ', trim(name)
        call app_log(APP_ERROR, app_msg)
        return
    end if

    success = fst24_file_is_valid(trim(name))
    if (.not. success) then
        write(app_msg, '(A, A)') 'Newly-created file does not seem valid ', trim(name)
        call app_log(APP_ERROR, app_msg)
        return
    end if

    success = .true.
end function

function test_fst24_interface(is_rsf) result(success)
    implicit none
    logical, intent(in) :: is_rsf
    logical :: success

    type(fst_file) :: test_file
    type(fst_record_fields) :: fields
    type(fst_record) :: expected, record
    type(fst_query) :: query
    integer :: num_found
    real(kind = real32), dimension(:, :), pointer :: data_array
    integer(kind = int32), dimension(:, :), pointer :: bad_type
    real(kind = real64), dimension(:, :), pointer :: bad_size_64
    real(kind = real32), dimension(:), pointer :: bad_dim
    real(kind = real32), dimension(:, :, :), pointer :: ok_dim
    integer(C_INT32_T) :: status

    success = .false.

    success = create_file(test_file_names(1), is_rsf, test_record % ip2, test_record % ip3)
    if (.not. success) then
        call App_Log(APP_ERROR, 'file is not created')
        return
    end if

    success = test_file % open(test_file_names(1))
    if (.not. success) then
        call App_log(APP_ERROR, 'Unable to open test FST file')
        return
    end if

    block
        integer(C_INT64_T) :: num_rec
        num_rec = test_file % get_num_records()
        if (num_rec /= 3) then
            write(app_msg, '(A, I12)') 'There should be 3 records in the file, but there are ', num_rec
            call app_log(APP_ERROR, app_msg)
            return
        end if
    end block

    call test_file % print_summary(grid_info = .true.)

    expected = test_record

    ! ///////////////////////////////////////////////
    ! // Find next + read
    num_found = 0
    query = test_file % make_search_query() ! Default search criteria (wildcard everywhere)
    success = query % is_valid()
    do while (query % find_next(record))
        ! call record % print_short(print_header = (num_found == 0))
        num_found = num_found + 1

        expected % ip1 = num_found
        success = record % has_same_info(expected)
        if (.not. success) then
            write(app_msg, '(A, I3, A)') 'Record read from file is not identical to the one written! (num_found = ',    &
                num_found, ')'
            call app_log(APP_ERROR, app_msg)
            call record % print()
            call expected % print()
            return
        end if

        success = record % read_metadata()
        if ((.not. success) .and. is_rsf) then
            call app_log(APP_ERROR, 'Shoud have been able to read metadata')
            return
        end if

        success = record % read()
        if (.not. success) then
            call app_log(APP_ERROR, 'Could not read data from record')
            return
        end if

        success = .false.
        call record % get_data_array(bad_type)
        if (associated(bad_type)) then
            call app_log(APP_ERROR, 'Pointer should not be associated!')
            return
        end if
        call record % get_data_array(bad_size_64)
        if (associated(bad_size_64)) then
            call app_log(APP_ERROR, 'Pointer should not be associated!')
            return
        end if
        call record % get_data_array(bad_dim)
        if (associated(bad_dim)) then
            call app_log(APP_ERROR, 'Pointer should not be associated!')
            return
        end if
        call record % get_data_array(ok_dim)
        if (.not. associated(ok_dim)) then
            call app_log(APP_ERROR, 'Pointer should be associated!')
            return
        end if

        call record % get_data_array(data_array)
        success = check_content(data_array, test_data)
        if (.not. success) return

        success = test_file % is_open()
        if (.not. success) then
            call app_log(APP_ERROR, 'AAAaaaaahhhh file is no longer open!')
            return
        end if
    end do

    success = (num_found == 3)
    if (.not. success) then
        write(app_msg, '(A, I4, A)') 'Found only ', num_found, ' of the 3 records we wrote!'
        call app_log(APP_ERROR, app_msg)
        return
    end if

    ! ////////////////////////////////////////////////////////////
    ! // Read next
    call query % rewind()
    num_found = 0
    call app_log(APP_INFO, 'Reading again, with read_next')
    do while (query % read_next(record))
        num_found = num_found + 1
        expected % ip1 = num_found

        success = record % has_same_info(expected)
        if (.not. success) then
            write(app_msg, '(A, I3, A)') 'Record read from file is not identical to the one written! (num_found = ',    &
                num_found, ')'
            call app_log(APP_ERROR, app_msg)
            call record % print()
            call expected % print()
            return
        end if

        ! call c_f_pointer(record % data, data, [DATA_SIZE, DATA_SIZE])
        call record % get_data_array(data_array)
        success = check_content(data_array, test_data)
        if (.not. success) return
    end do
    
    success = (num_found == 3)
    if (.not. success) then
        write(app_msg, '(A, I4, A)') 'Found only ', num_found, ' of the 3 records we wrote!'
        call app_log(APP_ERROR, app_msg)
        return
    end if

    ! ///////////////////////////////////////////////
    ! // Find all
    call app_log(APP_INFO, 'Testing find_all')
    block
        type(fst_record), dimension(5) :: all_records
        integer :: i

        num_found = query % find_all(all_records(1:1))

        success = (num_found == 1)
        if (.not. success) then
            write(app_msg, '(A, I4, A)') 'Find all with a max of 1 actually found ', num_found, ' records'
            call app_log(APP_ERROR, app_msg)
            return
        end if

        expected % ip1 = 1
        success = all_records(1) % has_same_info(expected)
        if (.not. success) then
            write(app_msg, '(A, I3, A)') 'Record read from file is not identical to the one written! (num_found = ',    &
                num_found, ')'
            call app_log(APP_ERROR, app_msg)
            call record % print()
            call expected % print()
            return
        end if

        num_found = query % find_all(all_records)
        success = (num_found == 3)
        if (.not. success) then
            write(app_msg, '(A, I4, A)') 'Found only ', num_found, ' of the 3 records we wrote!'
            call app_log(APP_ERROR, app_msg)
            return
        end if

        do i = 1, 3
            expected % ip1 = i
            success = all_records(i) % has_same_info(expected)
            if (.not. success) then
                write(app_msg, '(A, I3, A)') 'Record read from file is not identical to the one written! (num_found = ',    &
                    num_found, ')'
                call app_log(APP_ERROR, app_msg)
                call record % print()
                call expected % print()
                return
            end if
        end do
    end block

    call query % free()

    ! /////////////////////////////////////////
    ! // Everything again, with linked files
    block
        type(fst_file), dimension(3) :: file_list
        type(fst_record), dimension(10) :: results
        integer(C_INT64_T) :: num_records

        file_list(1) = test_file

        success = create_file(test_file_names(2), .not. is_rsf, test_record % ip2 + 1, test_record % ip3 + 1)   &
            .and. create_file(test_file_names(3),       is_rsf, test_record % ip2 + 2, test_record % ip3 + 1)

        if (.not. success) then
            call app_log(APP_ERROR, 'Unable to create other files for link tests')
            return
        end if

        success = file_list(2) % open(test_file_names(2), options = 'R/O') .and.            &
                  file_list(3) % open(test_file_names(3), options = 'R/O')

        if (.not. success) then
            call app_log(APP_ERROR, 'Unable to open other files for link tests')
            return
        end if

        success = fst24_link(file_list(1:1)) ! Link only 1 (should work)
        if (.not. success) then 
            call app_log(APP_ERROR, 'Should succeed linking only 1 file')
            return
        end if

        success = fst24_link(file_list)
        if (.not. success) then
            call app_log(APP_ERROR, 'Error trying to link 3 files')
            return
        end if

        success = .not. fst24_link(file_list(1:2))
        if (.not. success) then
            call app_log(APP_ERROR, 'Should not succeed linking already-linked file')
            return
        end if
        
        call test_file % print_summary(ip2 = .true., ip3 = .true.)

        num_records = test_file % get_num_records()
        success = (num_records == 9)
        if (.not. success) then
            write(app_msg, '(A, I4)') 'Wrong number of records in test file: ', num_records
            call app_log(APP_ERROR, app_msg)
            return
        end if

        query = test_file % make_search_query(ip2 = test_record % ip2 + 1)
        success = query % is_valid()
        if (.not. success) then
            call app_log(APP_ERROR, 'Unable to set search criteria!')
            return
        end if

        num_found = 0
        call app_log(APP_INFO, 'Looking for 3 records (should be in second file)')

        ! Should find the 3 records in the second file only
        do while(query % find_next(record))
            num_found = num_found + 1
            expected % ip1 = num_found
            expected % ip2 = test_record % ip2 + 1
            expected % ip3 = test_record % ip3 + 1

            success = record % has_same_info(expected)
            if (.not. success) then
                write(app_msg, '(A, I3, A)') 'Record read from file is not identical to the one written! (num_found = ',    &
                    num_found, ')'
                call app_log(APP_ERROR, app_msg)
                call record % print()
                call expected % print()
                return
            end if
        end do

        success = (num_found == 3)
        if (.not. success) then
            write(app_msg, '(A, I4, A)') 'Found only ', num_found, ' of the 3 records we wrote!'
            call app_log(APP_ERROR, app_msg)
            return
        end if

        num_found = query % find_all(results)
        success = (num_found == 3)
        if (.not. success) then
            write(app_msg, '(A, I4)') 'Find all should have found 3 rather than ', num_found
            call app_log(APP_ERROR, app_msg)
            return
        end if

        ! Should find the 6 records in second + third file
        call query % free()
        query = test_file % make_search_query(ip3 = test_record % ip3 + 1)
        success = query % is_valid()
        num_found = 0
        call app_log(APP_INFO, 'Looking for 6 records (should be in second + third files)')
        do while (query % find_next(record))
            num_found = num_found + 1
            success = record % read()
            if (.not. success) then
                call app_log(APP_ERROR, 'Unable to read record from linke files')
                return
            end if
        end do

        success = (num_found == 6)
        if (.not. success) then
            write(app_msg, '(A, I4, A)') 'Found only ', num_found, ' rather than 6!'
            call app_log(APP_ERROR, app_msg)
            return
        end if

        call app_log(APP_INFO, 'Find all (should be 6)')
        num_found = query % find_all(results)
        success = (num_found == 6)
        if (.not. success) then
            write(app_msg, '(A, I4)') 'Find all should have found 6 rather than ', num_found
            call app_log(APP_ERROR, app_msg)
            return
        end if

        call app_log(APP_INFO, 'Testing rewind')
        call query % rewind()
        num_found = query % find_all(results)
        success = (num_found == 6)
        if (.not. success) then
            write(app_msg, '(A, I4)') 'Find all should have found 6 rather than ', num_found
            call app_log(APP_ERROR, app_msg)
            return
        end if

        call app_log(APP_INFO, 'Read all, one by one')
        call query % free()
        query = test_file % make_search_query()
        success = query % is_valid()
        num_found = 0
        do while (query % read_next(record))
            num_found = num_found + 1
        end do

        success = (num_found == 9)
        if (.not. success) then
            write(app_msg, '(A, I4, A)') 'Found only ', num_found, ' rather than 9!'
            call app_log(APP_ERROR, app_msg)
            return
        end if

        success = test_file % unlink()
        if (.not. success) then
            call app_log(APP_ERROR, 'Error while unlinking 3 files')
            return
        end if

        success = test_file % close() .and. file_list(2) % close() .and. file_list(3) % close()
        if (.not. success) then
            call app_log(APP_ERROR, 'Unable to close files')
            return
        end if 

        success = .not. fst24_link(file_list)
        if (.not. success) then
            call app_log(APP_ERROR, 'Should not be able to link closed files')
            return
        end if
    end block

    call app_log(APP_INFO, 'A few calls that should fail')

    success = .not. record % read()
    if (.not. success) then
        call App_Log(APP_ERROR, 'Should not be able to read record data after file is closed')
        return
    end if

    success = .not. test_file % unlink()
    if (.not. success) then
        call App_Log(APP_ERROR, 'Should not be able to unlink closed file')
        return
    end if
    
    success = .not. query % find_next(record)    
    if (.not. success) then
        call App_Log(APP_ERROR, 'Should not be able to search a closed file')
        return
    end if

    success = .not. query % read_next(record)    
    if (.not. success) then
        call App_Log(APP_ERROR, 'Should not be able to search a closed file')
        return
    end if

    call query % free()
    query = test_file % make_search_query()
    success = .not. query % is_valid()
    if (.not. success) then
        call App_Log(APP_ERROR, 'Should not be able to set search criteria on a closed file')
        return
    end if

    success = (test_file % get_num_records() == 0)
    if (.not. success) then
        call App_Log(APP_ERROR, 'Should not be able to get num records of closed file')
        return
    end if

    success = .not. test_file % close()
    if (.not. success) then
        call App_log(APP_ERROR, 'Should not be able to close closed file!')
        return
    end if

    success = .true.
end function test_fst24_interface

end module test_fst24_interface_module


program fst24_interface
    use test_fst24_interface_module

    if (.not. fst24_is_default_record_valid()) error stop 1
    call make_test_record()

    call App_Log(APP_INFO, 'Doing RSF tests')
    if (.not. test_fst24_interface(.true.)) error stop 1
    call App_Log(APP_INFO, 'Doing XDF tests')
    if (.not. test_fst24_interface(.false.)) error stop 1

    call delete_test_data()
    call App_Log(APP_INFO, 'Tests successful')
end program fst24_interface
