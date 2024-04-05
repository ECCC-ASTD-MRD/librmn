
module fst24_search_params_module
    use app
    use rmn_fst24
    implicit none

    character(len=*), parameter :: test_filename = 'search_params.fst'
    integer, dimension(2*2*2), target :: test_data = [ 0, 1, 2, 3, 4, 5, 6, 7 ]
    type(fst_record) :: record1
    type(fst_record) :: record2
    type(fst_record) :: dummy_record

    character(len=2048) :: cmd
contains

subroutine create_file(is_rsf)
    implicit none
    logical, intent(in) :: is_rsf

    type(fst_file) :: test_file
    logical :: success
    character(len=:), allocatable :: options

    write(cmd, '(A, (1X, A))') 'rm -fv ', test_filename
    call execute_command_line(trim(cmd))

    options = 'RSF'
    if (.not. is_rsf) options = 'XDF'

    success = test_file % open(test_filename, options = options)
    if (.not. success) then
        call App_Log(APP_ERROR, 'Unable to open file for creating')
        error stop 1
    end if

    dummy_record % datyp = FST_TYPE_BINARY
    dummy_record % dasiz = 8
    dummy_record % npak = -8
    dummy_record % ni = 1
    dummy_record % nj = 2
    dummy_record % nk = 1
    dummy_record % data = c_loc(test_data)

    dummy_record % dateo = 0
    dummy_record % datev = 0
    dummy_record % deet = 0
    dummy_record % npas = 0

    dummy_record % ip1 = 0
    dummy_record % ip2 = 0
    dummy_record % ip3 = 0
    dummy_record % ig1 = 0
    dummy_record % ig2 = 0
    dummy_record % ig3 = 0
    dummy_record % ig4 = 0

    dummy_record % typvar = '0'
    dummy_record % nomvar = '0'
    dummy_record % grtyp  = '0'
    dummy_record % etiket = '0'

    success = test_file % write(dummy_record)
    if (.not. success) then
        call App_Log(APP_ERROR, 'Unable to write dummy record in test file')
        error stop 1
    end if

    record1 % datyp = FST_TYPE_SIGNED
    record1 % dasiz = 32
    record1 % npak = -32
    record1 % ni = 1
    record1 % nj = 1
    record1 % nk = 1
    record1 % data = c_loc(test_data)

    record1 % dateo = 458021600
    record1 % datev = 1
    record1 % deet = 3600 * 1
    record1 % npas = 1

    record1 % ip1 = 1
    record1 % ip2 = 1
    record1 % ip3 = 1
    record1 % ig1 = 1
    record1 % ig2 = 1
    record1 % ig3 = 1
    record1 % ig4 = 1

    record1 % typvar = '1'
    record1 % nomvar = '1'
    record1 % grtyp  = '1'
    record1 % etiket = '1'

    success = test_file % write(record1)
    if (.not. success) then
        call App_Log(APP_ERROR, 'Unable to write 1st record in test file')
        error stop 1
    end if

    record2 % datyp = FST_TYPE_UNSIGNED
    record2 % dasiz = 16
    record2 % npak = -16
    record2 % ni = 2
    record2 % nj = 2
    record2 % nk = 2
    record2 % data = c_loc(test_data)

    record2 % dateo = 458021601
    record2 % datev = 458021601
    record2 % deet = 3600 * 2
    record2 % npas = 2

    record2 % ip1 = 2
    record2 % ip2 = 2
    record2 % ip3 = 2
    record2 % ig1 = 2
    record2 % ig2 = 2
    record2 % ig3 = 2
    record2 % ig4 = 2

    record2 % typvar = '2'
    record2 % nomvar = '2'
    record2 % grtyp  = '2'
    record2 % etiket = '2'

    success = test_file % write(record2)
    if (.not. success) then
        call App_Log(APP_ERROR, 'Unable to write 2nd record in test file')
        error stop 1
    end if

    call test_file % print_summary(datev = .true.)

    success = test_file % close()
end subroutine create_file

subroutine run_test(is_rsf)
    implicit none
    logical, intent(in) :: is_rsf

    type(fst_file)   :: test_file
    type(fst_record) :: rec
    logical :: success

    call create_file(is_rsf)

    success = test_file % open(test_filename)
    if (.not. success) then
        call App_Log(APP_ERROR, 'Unable to open file for searching')
        error stop 1
    end if

    ! TODO uncomment when search by origin date is implemented
    ! success = test_file % read(rec, dateo = record1 % dateo)
    ! if (.not. success .or. .not. rec % has_same_info(record1)) then
    !     call App_Log(APP_ERROR, 'dateo search failed')
    !     call rec % print_short(print_header = .true.)
    !     call record1 % print_short()
    !     error stop 1
    ! end if

    ! Search by valid date
    success = test_file % read(rec, datev = 458025201_int64)
    if (.not. success .or. .not. rec % has_same_info(record2)) then
        call App_Log(APP_ERROR, 'datev search failed')
        call rec % print_short(print_header = .true.)
        call record2 % print_short()
        error stop 1
    end if
end subroutine run_test

end module fst24_search_params_module

program fst24_search_params
    use fst24_search_params_module
    implicit none

    call run_test(.true.)
    ! call run_test(.false.)
end program fst24_search_params
