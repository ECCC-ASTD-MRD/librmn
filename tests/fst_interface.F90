
#ifndef FST_TEST_IS_RSF
#define FST_TEST_IS_RSF .true.
#endif

program fst_interface
    use app
    use iso_c_binding
    use rmn_test_helper
    use rmn_fstd98
    implicit none

    integer, external :: fnom

    integer :: status, i, j
    logical :: is_rsf
    character(len=*), parameter :: test_file_name = 'fst_interface.fst'

    integer, parameter :: NUM_DATA = 8
    integer, dimension(NUM_DATA, 3) :: data_array
    integer, dimension(NUM_DATA)    :: work_array

    integer, parameter :: UNIT_NUMBER = 11
    integer, parameter :: DATE = 0, TIMESTEP_SIZE = 0, TIMESTEP_NUM = 0
    integer, parameter :: DATATYPE = 4 ! 4 = integer
    integer, parameter :: REWRITE = 0

    integer :: ni, nj, nk
    integer :: num_records, new_num_records, expected_num_records
    integer :: record_key
    integer :: expected
    integer :: ip1, ip2, ip3
    character(len=20) :: etiket

    type(fstd98) :: test_file

    is_rsf = FST_TEST_IS_RSF
    ! if (is_rsf) then
    !     test_file_name = 'fst_interface_rsf.fst'
    ! else
    !     test_file_name = 'fst_interface_xdf.fst'
    ! end if

    do j = 1, 3
        do i = 1, NUM_DATA
            data_array(i, j) = i + j * 100
        end do
    end do

    ! --- fstouv ---
    if (is_rsf) then
        status = test_file % ouv(test_file_name, 'STD+RND+RSF')
    else
        status = test_file % ouv(test_file_name, 'STD+RND')
    end if
    call check_status(status, expected_min = 0, fail_message = 'ouv')

    num_records = status

    ! --- fstecr ---
    do j = 1, 2
        do i = 1, 3
            status = test_file % ecr(data_array(:, i), work_array, -32, DATE, TIMESTEP_SIZE, TIMESTEP_NUM,     &
                                    NUM_DATA, 1, 1,                                                            &
                                    i, 0, 0,                                                                   &
                                    'XX', 'YYYY', 'ETIKET', 'X',                                               &
                                    0, 0, 0, 0,                                                                &
                                    DATATYPE,  REWRITE)
            call check_status(status, expected = 0, fail_message = 'ecr')
        end do
    end do

    ! ----- fstnbr -----
    new_num_records = test_file % nbr()
    call check_status(new_num_records, expected = num_records, fail_message = 'nbr')

    ! ----- fstfrm -----
    status = test_file % frm()
    call check_status(status,expected = 0, fail_message = 'frm')

    ! ----- fstouv -----
    status = test_file % ouv(test_file_name, 'STD+RND')
    call check_status(status, expected_min = 1, fail_message = 'ouv (second one)')

    ! ----- fstnbr -----
    new_num_records = test_file % nbr()
    call check_status(new_num_records, expected = num_records + 6, fail_message = 'nbr (second one)')

    ! ----- fstlir ----- (includes fstinf and fstluk)
    ! Not found
    record_key = test_file % lir(work_array, ni, nj, nk, -1, ' ', 1, -1, -1, 'nooooooo', ' ')
    call check_status(record_key, expected_max = -1, fail_message = 'lir (not found)')

    ! Found
    work_array(:) = 0
    record_key = test_file % lir(work_array, ni, nj, nk, -1, ' ', 2, -1, -1, ' ', ' ')
    call check_status(record_key, expected_min = 1, fail_message = 'lir (found)')
    if (.not. all(work_array == data_array(:, 2)) .or. ni /= NUM_DATA .or. nj /= 1 .or. nk /= 1) then
        write(app_msg, '(A, 2(5X, 8I4))') 'Got data', work_array, data_array(:, 2)
        call App_Log(APP_ERROR, app_msg)
        write(app_msg, '(A, 1X, 3I4, A, 1X, 3I4)')                                                                  &
            'Got dimensions', ni, nj, nk, ' Should have been', NUM_DATA, 1, 1
        call App_Log(APP_ERROR, app_msg)
        error stop 1
    end if

    ! ----- fstmsq -----
    status = test_file % msq(ip1, ip2, ip3, etiket, 1)
    call check_status(status, expected = 0, fail_message = 'msq (status)')
    ! write(app_msg, '(A, 3I11)') 'Got IPs ', ip1, ip2, ip3
    ! call App_Log(APP_WARNING, app_msg)
    call check_status(ip1, expected = 0, fail_message = 'msq (ip1)')
    call check_status(ip2, expected = int(z'fffffff', kind=4), fail_message = 'msq (ip2)')
    call check_status(ip3, expected = int(z'fffffff', kind=4), fail_message = 'msq (ip3)')
    if (etiket(1:12) /= '************') then
        write(app_msg, '(A, A, A, A)') 'Wrong etiket, got ', etiket(1:12)
        call App_Log(APP_ERROR, app_msg)
        write(app_msg, '(A, A)') 'but expected      ', '************' // C_NULL_CHAR
        call App_Log(APP_ERROR, app_msg)
        error stop 1
    end if

    ! ----- fstlis -----
    work_array(:) = 0
    record_key = test_file % lis(work_array, ni, nj, nk)
    call check_status(record_key, expected_min = 1, fail_message = 'lis')
    if (.not. all(work_array == data_array(:, 2)) .or. ni /= NUM_DATA .or. nj /= 1 .or. nk /= 1) then
        write(app_msg, '(A, 2(5X, 8I4))') 'Got data', work_array, data_array(:, 2)
        call App_Log(APP_ERROR, app_msg)
        write(app_msg, '(A, 1X, 3I4, A, 1X, 3I4)')                                                                  &
            'Got dimensions', ni, nj, nk, ' Should have been', NUM_DATA, 1, 1
        call App_Log(APP_ERROR, app_msg)
        error stop 1
    end if

    ! ----- fstinl ----- (includes fstsui)
    block
        integer, dimension(num_records + 6) :: record_keys
        integer :: num_record_found
        status = test_file % inl(ni, nj, nk, -1, ' ', -1, -1, -1, ' ', ' ',          &
                                 record_keys, num_record_found, num_records + 6)
        call check_status(status, expected = 0, fail_message = 'fstinl status')
        call check_status(num_record_found, expected = num_records + 6, fail_message = 'fstinl count')
    end block

    ! ----- fsteff -----
    ! Better put this test (second-to-)last, because after it the state will be different depending on type of
    ! standard file (RSF or XDF)
    status = test_file % eff(record_key)
    if (test_file % is_rsf()) then
        call check_status(status, expected_max = -1, fail_message = 'eff')
    else
        call check_status(status, expected = 0, fail_message = 'eff')
    end if

    ! ----- fstfrm -----
    status = test_file % frm()
    call check_status(status,expected = 0, fail_message = 'frm (second one)')

    call App_Log(APP_INFO, 'done')

end program fst_interface