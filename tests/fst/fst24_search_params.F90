
module fst24_search_params_module
    use app
    use rmn_fst24
    implicit none

    character(len=*), parameter :: test_filename = 'search_params.fst'
    integer, dimension(3*3*3), target :: test_data
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

    dummy_record % data_type = FST_TYPE_BINARY
    dummy_record % data_bits = 8
    dummy_record % pack_bits = 8
    dummy_record % ni = 3
    dummy_record % nj = 3
    dummy_record % nk = 3
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

    record1 % data_type = FST_TYPE_SIGNED
    record1 % data_bits = 32
    record1 % pack_bits = 32
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

    record2 % data_type = FST_TYPE_UNSIGNED
    record2 % data_bits = 16
    record2 % pack_bits = 16
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

    ! Search by ni
    success = test_file % read(rec, ni = record1 % ni)
    if (.not. success .or. .not. rec % has_same_info(record1)) then
        call App_Log(APP_ERROR, 'ni search failed')
        call rec % print_short(print_header = .true.)
        call record1 % print_short()
        error stop 1
    end if

    ! Search by nj
    success = test_file % read(rec, nj = record2 % nj)
    if (.not. success .or. .not. rec % has_same_info(record2)) then
        call App_Log(APP_ERROR, 'nj search failed')
        call rec % print_short(print_header = .true.)
        call record2 % print_short()
        error stop 1
    end if

    ! Search by nk
    success = test_file % read(rec, nk = record1 % nk)
    if (.not. success .or. .not. rec % has_same_info(record1)) then
        call App_Log(APP_ERROR, 'nk search failed')
        call rec % print_short(print_header = .true.)
        call record1 % print_short()
        error stop 1
    end if

    ! Search by datatype
    success = test_file % read(rec, data_type = record2 % data_type)
    if (.not. success .or. .not. rec % has_same_info(record2)) then
        call App_Log(APP_ERROR, 'data_type search failed')
        call rec % print_short(print_header = .true.)
        call record2 % print_short()
        error stop 1
    end if

    ! Search by data size
    success = test_file % read(rec, data_bits = record1 % data_bits)
    if (.not. success .or. .not. rec % has_same_info(record1)) then
        call App_Log(APP_ERROR, 'data_bits search failed')
        call rec % print_short(print_header = .true.)
        call record1 % print_short()
        error stop 1
    end if

    ! Search by packed size
    success = test_file % read(rec, pack_bits = record2 % pack_bits)
    if (.not. success .or. .not. rec % has_same_info(record2)) then
        call App_Log(APP_ERROR, 'pack_bits search failed')
        call rec % print_short(print_header = .true.)
        call record2 % print_short()
        error stop 1
    end if

    ! Search by timestep size
    success = test_file % read(rec, deet = record1 % deet)
    if (.not. success .or. .not. rec % has_same_info(record1)) then
        call App_Log(APP_ERROR, 'deet search failed')
        call rec % print_short(print_header = .true., deet = .true.)
        call record1 % print_short(deet = .true.)
        error stop 1
    end if

    ! Search by timestep number
    success = test_file % read(rec, npas = record2 % npas)
    if (.not. success .or. .not. rec % has_same_info(record2)) then
        call App_Log(APP_ERROR, 'npas search failed')
        call rec % print_short(print_header = .true., npas = .true.)
        call record2 % print_short(npas = .true.)
        error stop 1
    end if

    ! Search by ip1
    success = test_file % read(rec, ip1 = record1 % ip1)
    if (.not. success .or. .not. rec % has_same_info(record1)) then
        call App_Log(APP_ERROR, 'ip1 search failed')
        call rec % print_short(print_header = .true., ip1 = .true.)
        call record1 % print_short(ip1 = .true.)
        error stop 1
    end if

    ! Search by ip2
    success = test_file % read(rec, ip2 = record2 % ip2)
    if (.not. success .or. .not. rec % has_same_info(record2)) then
        call App_Log(APP_ERROR, 'ip2 search failed')
        call rec % print_short(print_header = .true., ip2 = .true.)
        call record2 % print_short(ip2 = .true.)
        error stop 1
    end if

    ! Search by ip3
    success = test_file % read(rec, ip3 = record1 % ip3)
    if (.not. success .or. .not. rec % has_same_info(record1)) then
        call App_Log(APP_ERROR, 'ip3 search failed')
        call rec % print_short(print_header = .true., ip1 = .true.)
        call record1 % print_short(ip1 = .true.)
        error stop 1
    end if

    ! Search by ig1
    success = test_file % read(rec, ig1 = record2 % ig1)
    if (.not. success .or. .not. rec % has_same_info(record2)) then
        call App_Log(APP_ERROR, 'ig1 search failed')
        call rec % print_short(print_header = .true., ig1234 = .true.)
        call record2 % print_short(ig1234 = .true.)
        error stop 1
    end if
    ! Search by ig2
    success = test_file % read(rec, ig2 = record1 % ig2)
    if (.not. success .or. .not. rec % has_same_info(record1)) then
        call App_Log(APP_ERROR, 'ig2 search failed')
        call rec % print_short(print_header = .true., ig1234 = .true.)
        call record1 % print_short(ig1234 = .true.)
        error stop 1
    end if
    ! Search by ig3
    success = test_file % read(rec, ig3 = record2 % ig3)
    if (.not. success .or. .not. rec % has_same_info(record2)) then
        call App_Log(APP_ERROR, 'ig3 search failed')
        call rec % print_short(print_header = .true., ig1234 = .true.)
        call record2 % print_short(ig1234 = .true.)
        error stop 1
    end if
    ! Search by ig4
    success = test_file % read(rec, ig4 = record1 % ig4)
    if (.not. success .or. .not. rec % has_same_info(record1)) then
        call App_Log(APP_ERROR, 'ig4 search failed')
        call rec % print_short(print_header = .true., ig1234 = .true.)
        call record1 % print_short(ig1234 = .true.)
        error stop 1
    end if

    ! Search by nomvar
    success = test_file % read(rec, nomvar = record2 % nomvar)
    if (.not. success .or. .not. rec % has_same_info(record2)) then
        call App_Log(APP_ERROR, 'nomvar search failed')
        call rec % print_short(print_header = .true.)
        call record2 % print_short()
        error stop 1
    end if
    ! Search by typvar
    success = test_file % read(rec, typvar = record1 % typvar)
    if (.not. success .or. .not. rec % has_same_info(record1)) then
        call App_Log(APP_ERROR, 'typvar search failed')
        call rec % print_short(print_header = .true.)
        call record1 % print_short()
        error stop 1
    end if
    ! Search by grtyp
    success = test_file % read(rec, grtyp = record2 % grtyp)
    if (.not. success .or. .not. rec % has_same_info(record2)) then
        call App_Log(APP_ERROR, 'grtyp search failed')
        call rec % print_short(print_header = .true.)
        call record2 % print_short()
        error stop 1
    end if
    ! Search by etiket
    success = test_file % read(rec, etiket = record1 % etiket)
    if (.not. success .or. .not. rec % has_same_info(record1)) then
        call App_Log(APP_ERROR, 'etiket search failed')
        call rec % print_short(print_header = .true.)
        call record1 % print_short()
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
