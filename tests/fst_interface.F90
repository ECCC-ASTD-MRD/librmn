
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
    character(len=*), parameter :: test_file_name_2 = 'fst_interface_2.fst'
    character(len=2000) :: cmd

    integer, parameter :: NUM_DATA = 8
    integer, dimension(NUM_DATA, 3) :: data_array
    integer, dimension(NUM_DATA)    :: work_array

    integer, parameter :: UNIT_NUMBER = 11
    integer, parameter :: DATE = 0, TIMESTEP_SIZE = 0, TIMESTEP_NUM = 0
    integer, parameter :: DATATYPE = 4 ! 4 = integer
    integer, parameter :: REWRITE = 0

    integer :: old_num_records, new_num_records, expected_num_records, num_records
    integer :: record_key
    integer :: expected

    type(fstd98) :: test_file

    is_rsf = FST_TEST_IS_RSF

    ! Remove file(s) so that we have a fresh start
    write(cmd, '(A, 2(1X, A))') 'rm -fv ', test_file_name, test_file_name_2
    call execute_command_line(trim(cmd))

    ! Initialize data
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

    old_num_records = status
    new_num_records = old_num_records + 6

    ! --- fstecr ---
    do j = 1, 2
        do i = 1, 3
            status = test_file % ecr(data_array(:, i), work_array, -32, DATE, TIMESTEP_SIZE, TIMESTEP_NUM,     &
                                    NUM_DATA, 1, 1,                                                            &
                                    i, j, 0,                                                                   &
                                    'XX', 'YYYY', 'ETIKET', 'X',                                               &
                                    0, 0, 0, 0,                                                                &
                                    DATATYPE,  REWRITE)
            call check_status(status, expected = 0, fail_message = 'ecr')
        end do
    end do

    ! ----- fstnbr -----
    num_records = test_file % nbr()
    call check_status(num_records, expected = old_num_records, fail_message = 'nbr')

    ! ----- fstnbrv -----
    num_records = test_file % nbrv()
    call check_status(num_records, expected = new_num_records, fail_message = 'nbrv')

    ! ----- fstfrm -----
    status = test_file % frm()
    call check_status(status,expected = 0, fail_message = 'frm')

    ! Copy file to have multiple ones to link together
    write(cmd, '(A, 2(1X,A))') 'cp -v ', test_file_name, test_file_name_2
    call execute_command_line(trim(cmd))

    ! ----- fstouv -----
    status = test_file % ouv(test_file_name, 'STD+RND')
    call check_status(status, expected = new_num_records, fail_message = 'ouv (second one)')

    ! ----- fstnbr -----
    num_records = test_file % nbr()
    call check_status(num_records, expected = new_num_records, fail_message = 'nbr (second one)')

    ! ----- fstlir ----- (includes fstinf and fstluk)
    ! Not found
    block
        integer :: ni, nj, nk
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
    end block

    ! ----- fstprm -----
    block
        integer :: date, deet, npas, ni, nj, nk, nbits, datyp, ip1, ip2, ip3
        character(len=20) :: typvar, nomvar, etiket, grtyp
        integer :: ig1, ig2, ig3, ig4, swa, lng, dlft, ubc, extra1, extra2, extra3
        status = fstprm(record_key, date, deet, npas, ni, nj, nk, nbits, datyp,                     &
                        ip1, ip2, ip3, typvar, nomvar, etiket, grtyp,                               &
                        ig1, ig2, ig3, ig4, swa, lng, dlft, ubc, extra1, extra2, extra3)

        call check_status(status, expected = 0, fail_message = 'prm')

        ! print *, record_key
        ! print *, date, deet, npas
        ! print *, ni, nj, nk
        ! print *, nbits, datyp
        ! print *, ip1, ip2, ip3
        ! print '(4(A, ", "))', trim(typvar), trim(nomvar), trim(etiket), trim(grtyp)
        ! print *, ig1, ig2, ig3, ig4
        ! print *, swa, lng, dlft, ubc
        ! print *, extra1, extra2, extra3

        call check_status(ni, expected = NUM_DATA, fail_message = 'prm (ni)')
        call check_status(nj, expected = 1, fail_message = 'prm (nj)')
        call check_status(nk, expected = 1, fail_message = 'prm (nk)')
        call check_status(nbits, expected = 32, fail_message = 'prm (nbits)')
        call check_status(datyp, expected = DATATYPE, fail_message = 'prm (datyp)')
        call check_status(ip1, expected = 2, fail_message = 'prm (ip1)')
        call check_status(ig1, expected = 0, fail_message = 'prm (ig1)')
        call check_status(ig2, expected = 0, fail_message = 'prm (ig2)')
        call check_status(ig3, expected = 0, fail_message = 'prm (ig3)')
        call check_status(ig4, expected = 0, fail_message = 'prm (ig4)')

        call check_status(dlft, expected = 0, fail_message = 'prm (dlft)')
        call check_status(ubc, expected = 0, fail_message = 'prm (ubc)')
        call check_status(extra1, expected = 0, fail_message = 'prm (extra1)')
        call check_status(extra2, expected = 0, fail_message = 'prm (extra2)')
        call check_status(extra3, expected = 0, fail_message = 'prm (extra3)')

        if (typvar(1:2) /= 'XX' .or. nomvar(1:4) /= 'YYYY' .or. etiket(1:6) /= 'ETIKET' .or. grtyp(1:1) /= 'X') then
            write(app_msg, '(6A)') 'Unexpected names for typvar, nomvar, etiket, grtyp: ',          &
                               trim(typvar), trim(nomvar), trim(etiket), trim(grtyp),               &
                               '. Should be XX, YYYY, ETIKET and X'
            call App_Log(APP_ERROR, app_msg)
            error stop 1
        end if

    end block

    ! ----- fstmsq -----
    block
        integer :: ip1, ip2, ip3
        character(len=20) :: etiket
        etiket(1:20) = 'abcdefghijklmnopqrst'
        status = test_file % msq(ip1, ip2, ip3, etiket, 1)
        ! status = fstmsq(test_file % iun, ip1, ip2, ip3, etiket, 1)
        call check_status(status, expected = 0, fail_message = 'msq (status)')
        ! write(app_msg, '(A, 3I11)') 'Got IPs ', ip1, ip2, ip3
        ! call App_Log(APP_WARNING, app_msg)
        call check_status(ip1, expected = 0, fail_message = 'msq (ip1)')
        call check_status(ip2, expected = 0, fail_message = 'msq (ip2)')
        call check_status(ip3, expected = 0, fail_message = 'msq (ip3)')
        if (etiket(1:12) /= '            ') then
            write(app_msg, '(A, A, A, A)') 'Wrong etiket, got ', '"' // etiket(1:12) // '"'
            call App_Log(APP_ERROR, app_msg)
            write(app_msg, '(A, A)') 'but expected      ', '"' // '            ' // '"'
            call App_Log(APP_ERROR, app_msg)
            error stop 1
        end if
    end block

    ! ----- fstlis -----
    block
        integer :: ni, nj, nk
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
    end block

    ! ----- fstinl ----- (includes fstsui)
    block
        integer, dimension(new_num_records) :: record_keys
        integer :: num_record_found
        integer :: ni, nj, nk
        status = test_file % inl(ni, nj, nk, -1, ' ', -1, -1, -1, ' ', ' ',          &
                                 record_keys, num_record_found, new_num_records)
        call check_status(status, expected = 0, fail_message = 'fstinl status')
        call check_status(num_record_found, expected = new_num_records, fail_message = 'fstinl count')
    end block

    ! ----- fstlnk -----
    block
        type(fstd98) :: test_file_2
        integer(C_INT32_T), dimension(2) :: unit_list
        integer, dimension(new_num_records * 2) :: record_keys
        integer :: num_record_found
        integer :: ni, nj, nk

        status = test_file_2 % ouv(test_file_name_2, 'STD+RND')
        call check_status(status, expected = new_num_records, fail_message = 'ouv (second file)')

        unit_list(1) = test_file % iun
        unit_list(2) = test_file_2 % iun

        status = fstlnk(unit_list, 2)
        call check_status(status, expected = 0, fail_message = 'fstlnk')

        ! num_records = test_file % nbr()
        ! call check_status(num_records, expected = new_num_records * 2, fail_message = 'nbr (linked)')

        status = test_file % inl(ni, nj, nk, -1, ' ', -1, -1, -1, ' ', ' ',          &
                                 record_keys, num_record_found, new_num_records * 2)
        call check_status(status, expected = 0, fail_message = 'fstinl status (linked)')
        call check_status(num_record_found, expected = new_num_records * 2, fail_message = 'fstinl count (linked)')

        status = fstunl()
        call check_status(status, expected = 0, fail_message = 'fstunl')

        status = test_file % inl(ni, nj, nk, -1, ' ', -1, -1, -1, ' ', ' ',          &
                                 record_keys, num_record_found, new_num_records * 2)
        call check_status(status, expected = 0, fail_message = 'fstinl status (unlinked)')
        call check_status(num_record_found, expected = new_num_records, fail_message = 'fstinl count (unlinked)')

        status = test_file_2 % inl(ni, nj, nk, -1, ' ', -1, -1, -1, ' ', ' ',          &
                                   record_keys, num_record_found, new_num_records * 2)
        call check_status(status, expected = 0, fail_message = 'fstinl status (unlinked, file 2)')
        call check_status(num_record_found, expected = new_num_records, fail_message = 'fstinl count (unlinked, file 2)')

    end block

    ! ----- fstvoi -----
    status = test_file % voi(' ')
    call check_status(status, expected = 0, fail_message = 'fstvoi')

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