module fst_link_mod
    use rmn_fst24
    use rmn_common
    use rmn_test_helper
    use rmn_fst98
    implicit none

    character(len=*), parameter :: rsf_name_1 = 'rsf1.fst'
    character(len=*), parameter :: rsf_name_2 = 'rsf2.fst'
    character(len=*), parameter :: xdf_name_1 = 'xdf1.fst'
    character(len=*), parameter :: xdf_name_2 = 'xdf2.fst'

contains

    subroutine test_fst_link()
        implicit none
        ! integer :: iun_rsf1, iun_rsf2, iun_xdf1, iun_xdf2
        integer :: status
        integer :: single_num_records, current_num_records
        integer(C_INT32_T), dimension(4) :: unit_list, unit_list_tmp
        integer      :: record_key
        integer, dimension(2000) :: work_array

        unit_list(:) = 0

        ! status = fnom(unit_list(1), rsf_name_1, 'STD+RND', 0)
        ! call check_status(status, expected = 0, fail_message = 'fnom')
        status = (fstouv(rsf_name_1, unit_list(1),'STD+RND+R/O'))
        call check_status(status, expected_min = 0, fail_message = 'fstouv (1)')

        status = (fstouv(xdf_name_1, unit_list(2),'RND+R/O'))
        call check_status(status, expected_min = 0, fail_message = 'fstouv (2)')

        status = (fstouv(xdf_name_2, unit_list(3),'RND+R/O'))
        call check_status(status, expected_min = 0, fail_message = 'fstouv (3)')

        status = (fstouv(rsf_name_2, unit_list(4),'RND+R/O'))
        call check_status(status, expected_min = 0, fail_message = 'fstouv (4)')

        single_num_records = fstnbr(unit_list(1))

        ! --------------------------------------------
        ! The link operation itself
        unit_list_tmp(:) = unit_list
        unit_list_tmp(3) = unit_list_tmp(2) ! We want the wrong one, xdf1, to check for fstlnk failure

        write(app_msg, '(A, 1X, 4I4, 1X, A)') 'fstlnk with ', unit_list, '(should fail)'
        call app_log(APP_INFO, app_msg)
        status = fstlnk(unit_list_tmp, 4) ! Should fail
        call check_status(status, expected_max = -1, fail_message = 'fstlnk with twice the same file')

        write(app_msg, '(A, 1X, 4I4, 1X, A)') 'fstlnk with ', unit_list, '(should succeed)'
        call app_log(APP_INFO, app_msg)
        status = fstlnk(unit_list, 4)
        call check_status(status, expected = 0, fail_message = 'fstlnk with correct input (first)')

        status = fstunl()
        call check_status(status, expected = 0, fail_message = 'fstunl (first)')

        status = fstlnk(unit_list, 4)
        call check_status(status, expected = 0, fail_message = 'fstlnk with correct input (second)')

        ! ---------------------------------------------
        ! fstnbr
        current_num_records = fstnbr(unit_list(1))
        call check_status(current_num_records, expected = single_num_records * 4, fail_message = 'nbr from first file in lnk')

        current_num_records = fstnbr(unit_list(2))
        call check_status(current_num_records, expected = single_num_records * 3, fail_message = 'nbr from second file in lnk')

        current_num_records = fstnbr(unit_list(3))
        call check_status(current_num_records, expected = single_num_records * 2, fail_message = 'nbr from third file in lnk')

        current_num_records = fstnbr(unit_list(4))
        call check_status(current_num_records, expected = single_num_records * 1, fail_message = 'nbr from fourth file in lnk')

        status = fstunl()
        current_num_records = fstnbr(unit_list(1))
        call check_status(current_num_records, expected = single_num_records * 1, fail_message = 'nbr from first file in lnk after unlink')
        current_num_records = fstnbr(unit_list(2))
        call check_status(current_num_records, expected = single_num_records * 1, fail_message = 'nbr from second file in lnk after unlink')
        current_num_records = fstnbr(unit_list(3))
        call check_status(current_num_records, expected = single_num_records * 1, fail_message = 'nbr from third file in lnk after unlink')
        current_num_records = fstnbr(unit_list(4))
        call check_status(current_num_records, expected = single_num_records * 1, fail_message = 'nbr from fourth file in lnk after unlink')
        
        status = fstlnk(unit_list, 4)
        call check_status(status, expected = 0, fail_message = 'fstlnk with correct input (after nbr with unlink)')

        ! ---------------------------------------------
        ! fstlir
        block
            integer :: ni, nj, nk
            call App_Log(APP_INFO, 'Testing fstlir')

            record_key = fstlir(work_array, unit_list(1), ni, nj, nk, -1, ' ', 1, -1, -1, 'nooooooo', ' ')
            call check_status(record_key, expected_max = -1, fail_message = 'lir (not found)')

            work_array(:) = 0
            record_key = fstlir(work_array, unit_list(1), ni, nj, nk, -1, ' ', 1, -1, -1, ' ', ' ')
            call check_status(record_key, expected_min = 1, fail_message = 'lir (found)')

            work_array(:) = 0
            record_key = fstlir(work_array, unit_list(2), ni, nj, nk, -1, ' ', -1, -1, 1234, ' ', ' ')
            call check_status(record_key, expected_min = 1, fail_message = 'lir (second file in link, record in third file)')

            work_array(:) = 0
            record_key = fstlir(work_array, unit_list(2), ni, nj, nk, -1, ' ', -1, -1, 30, ' ', ' ')
            call check_status(record_key, expected_min = 1, fail_message = 'lir (second file in link, record in fourth file)')
        end block

        ! ---------------------------------------------
        ! fstinl
        block
            integer, dimension(single_num_records * 4) :: record_keys
            integer :: num_record_found
            integer :: ni, nj, nk
            
            call App_Log(APP_INFO, 'Testing fstinl')

            status = fstinl(unit_list(1), ni, nj, nk, -1, ' ', -1, -1, -1, ' ', 'C',          &
                                record_keys, num_record_found, single_num_records * 4)
            call check_status(status, expected = 0, fail_message = 'fstinl status')
            ! call check_status(num_record_found, expected = single_num_records * 4, fail_message = 'fstinl count (1st file)')
            call check_status(num_record_found, expected = 4, fail_message = 'fstinl count (1st file)')


            status = fstinl(unit_list(2), ni, nj, nk, -1, ' ', -1, -1, -1, ' ', 'C',          &
                                record_keys, num_record_found, single_num_records * 3)
            call check_status(status, expected = 0, fail_message = 'fstinl status')
            ! call check_status(num_record_found, expected = single_num_records * 3, fail_message = 'fstinl count (2nd file)')
            call check_status(num_record_found, expected = 3, fail_message = 'fstinl count (2nd file)')

            status = fstinl(unit_list(3), ni, nj, nk, -1, ' ', -1, -1, -1, ' ', 'C',          &
                                record_keys, num_record_found, single_num_records * 3)
            call check_status(status, expected = 0, fail_message = 'fstinl status')
            ! call check_status(num_record_found, expected = single_num_records * 2, fail_message = 'fstinl count (3rd file)')
            call check_status(num_record_found, expected = 2, fail_message = 'fstinl count (3rd file)')

            status = fstinl(unit_list(4), ni, nj, nk, -1, ' ', -1, -1, -1, ' ', 'C',          &
                                record_keys, num_record_found, single_num_records * 3)
            call check_status(status, expected = 0, fail_message = 'fstinl status')
            ! call check_status(num_record_found, expected = single_num_records * 1, fail_message = 'fstinl count (4th file)')
            call check_status(num_record_found, expected = 1, fail_message = 'fstinl count (4th file)')

            call App_Log(APP_INFO, 'Testing fstinl (second batch)')

            status = fstinl(unit_list(1), ni, nj, nk, -1, ' ', -1, -1, 2, ' ', ' ',          &
                                record_keys, num_record_found, single_num_records * 4)
            call check_status(num_record_found, expected = single_num_records, fail_message = 'fstinl count (in 1st file)')
            status = fstinl(unit_list(1), ni, nj, nk, -1, ' ', -1, -1, 30, ' ', ' ',          &
                                record_keys, num_record_found, single_num_records * 4)
            call check_status(num_record_found, expected = single_num_records, fail_message = 'fstinl count (in 2nd file)')
            status = fstinl(unit_list(1), ni, nj, nk, -1, ' ', -1, -1, 100, ' ', ' ',          &
                                record_keys, num_record_found, single_num_records * 4)
            call check_status(num_record_found, expected = single_num_records, fail_message = 'fstinl count (in 3rd file)')
            status = fstinl(unit_list(1), ni, nj, nk, -1, ' ', -1, -1, 1234, ' ', ' ',          &
                                record_keys, num_record_found, single_num_records * 4)
            call check_status(num_record_found, expected = single_num_records, fail_message = 'fstinl count (in 4th file)')
        end block

        ! ---------------------------------------------
        ! fstinf + fstluk
        block
            integer :: ni, nj, nk
            integer :: record_key2

            call App_Log(APP_INFO, 'Testing fstinf, fstluk, fstlis and fstsui')

            record_key = fstinf(unit_list(1), ni, nj, nk, -1, ' ', -1, -1, 1, ' ', ' ')
            call check_status(record_key, expected_max = 0, fail_message = 'fstinf (not found)')

            record_key = fstinf(unit_list(1), ni, nj, nk, -1, ' ', -1, -1, 2, ' ', ' ')
            call check_status(record_key, expected_min = 1, fail_message = 'fstinf (ip3 = 2)')
            work_array(:) = 0
            status = fstluk(work_array, record_key, ni, nj, nk)
            call check_status(status, expected = record_key, fail_message = 'fstluk (ip3 = 2)')
            work_array(:) = 0
            record_key2 = fstlis(work_array, unit_list(1), ni, nj, nk)
            call check_status(record_key2, expected_min = record_key + 1, fail_message = 'fstlis (ip3 = 2)')
            status = fstsui(unit_list(1), ni, nj, nk)
            call check_status(status, expected_min = record_key2 + 1, fail_message = 'fstsui (ip3 = 2)')

            record_key = fstinf(unit_list(1), ni, nj, nk, -1, ' ', -1, -1, 30, ' ', ' ')
            call check_status(record_key, expected_min = 1, fail_message = 'fstinf (ip3 = 30)')
            work_array(:) = 0
            status = fstluk(work_array, record_key, ni, nj, nk)
            call check_status(status, expected = record_key, fail_message = 'fstluk (ip3 = 30)')
            work_array(:) = 0
            record_key2 = fstlis(work_array, unit_list(1), ni, nj, nk)
            call check_status(record_key2, expected_min = record_key + 1, fail_message = 'fstlis (ip3 = 30)')
            status = fstsui(unit_list(1), ni, nj, nk)
            call check_status(status, expected_min = record_key2 + 1, fail_message = 'fstsui (ip3 = 30)')

            record_key = fstinf(unit_list(1), ni, nj, nk, -1, ' ', -1, -1, 100, ' ', ' ')
            call check_status(record_key, expected_min = 1, fail_message = 'fstinf (ip3 = 100)')
            work_array(:) = 0
            status = fstluk(work_array, record_key, ni, nj, nk)
            call check_status(status, expected = record_key, fail_message = 'fstluk (ip3 = 100)')
            work_array(:) = 0
            record_key2 = fstlis(work_array, unit_list(1), ni, nj, nk)
            call check_status(record_key2, expected_min = record_key + 1, fail_message = 'fstlis (ip3 = 100)')
            status = fstsui(unit_list(1), ni, nj, nk)
            call check_status(status, expected_min = record_key2 + 1, fail_message = 'fstsui (ip3 = 100)')

            record_key = fstinf(unit_list(1), ni, nj, nk, -1, ' ', -1, -1, 1234, ' ', ' ')
            call check_status(record_key, expected_min = 1, fail_message = 'fstinf (ip3 = 1234)')
            work_array(:) = 0
            status = fstluk(work_array, record_key, ni, nj, nk)
            call check_status(status, expected = record_key, fail_message = 'fstluk (ip3 = 1234)')
            work_array(:) = 0
            record_key2 = fstlis(work_array, unit_list(1), ni, nj, nk)
            call check_status(record_key2, expected_min = record_key + 1, fail_message = 'fstlis (ip3 = 1234)')
            status = fstsui(unit_list(1), ni, nj, nk)
            call check_status(status, expected_min = record_key2 + 1, fail_message = 'fstsui (ip3 = 1234)')

        end block

        ! ---------------------------------------------
        ! fstnbrv
        current_num_records = fstnbrv(unit_list(1))
        call check_status(current_num_records, expected = single_num_records * 4, fail_message = 'nbrv from first file in lnk')

        current_num_records = fstnbrv(unit_list(2))
        call check_status(current_num_records, expected = single_num_records * 3, fail_message = 'nbrv from second file in lnk')

        current_num_records = fstnbrv(unit_list(3))
        call check_status(current_num_records, expected = single_num_records * 2, fail_message = 'nbrv from third file in lnk')

        current_num_records = fstnbrv(unit_list(4))
        call check_status(current_num_records, expected = single_num_records * 1, fail_message = 'nbrv from fourth file in lnk')

        ! ---------------------------------------------
        ! fstvoi
        call app_log(APP_INFO, 'testing fstvoi')
        status = fstvoi(unit_list(1), ' ')
        call check_status(status, expected = 0, fail_message = 'fstvoi')

        status = fstfrm(unit_list(1))
        status = fstfrm(unit_list(2))
        status = fstfrm(unit_list(3))
        status = fstfrm(unit_list(4))

    end subroutine test_fst_link

    !> Create a test file with a handful of records of various types.
    !> Inlined here (previously in generate_fstd_module.F90) so that this test is self-contained.
    subroutine generate_file(filename, backend, ip3_offset)
        implicit none
        character(len = *), intent(in) :: filename
        character(len = *), intent(in) :: backend
        integer, optional,  intent(in) :: ip3_offset

        integer, parameter :: NUM_DATA = 50

        integer(kind = int8),   dimension(NUM_DATA), target :: array_int8, array_uint8
        integer(kind = int16),  dimension(NUM_DATA), target :: array_int16, array_uint16
        integer(kind = int32),  dimension(NUM_DATA), target :: array_int32, array_uint32
        integer(kind = int64),  dimension(NUM_DATA), target :: array_int64, array_uint64
        real(kind = real32),    dimension(NUM_DATA), target :: array_real32
        real(kind = real64),    dimension(NUM_DATA), target :: array_real64
        complex(kind = real32), dimension(NUM_DATA), target :: array_complex32
        complex(kind = real64), dimension(NUM_DATA), target :: array_complex64

        character(len=4000) :: cmd
        character(len=3) :: filetype
        integer :: i
        type(fst_file) :: the_file
        type(fst_record) :: record
        logical :: success

        integer :: ip3
        ip3 = 0
        if (present(ip3_offset)) ip3 = ip3_offset

        ! Remove file so that we have a fresh start
        write(cmd, '(A, 2(1X, A))') 'rm -fv ', filename
        call execute_command_line(trim(cmd))

        ! Initialize data arrays
        do i = 1, NUM_DATA
            array_real32(i)     = i * 1.0 + .5
            array_real64(i)     = array_real32(i) + 1.2345
            array_complex32(i)  = cmplx(i * 1.0 + .6, i * 2.0 - 0.4)
            array_complex64(i)  = array_complex32(i) + cmplx(-1.3456, 1.9876)
            array_int64(i)      = i - 1001
            array_int32(i)      = i - 13
            array_int16(i)      = i - 11
            array_int8(i)       = i - 10
            array_uint64(i)     = i + 1005
            array_uint32(i)     = i + 22
            array_uint16(i)     = i + 25
            array_uint8(i)      = i + 3
        end do

        if (backend == 'RSF') then
            filetype = 'RSF'
        else
            filetype = 'XDF'
        end if
        success = the_file % open(filename, 'RND+' // backend // '+R/W')

        if (.not. success) then
            write (app_msg, '(A, A)') 'Error when opening (creating) file ', filename
            call App_Log(APP_ERROR, app_msg)
            return
        end if

        record % dateo = 0
        record % datev = 0
        record % deet  = 0
        record % npas  = 0
        record % ni  = NUM_DATA
        record % nj  = 1
        record % nk  = 1
        record % ip1 = 1
        record % ip2 = 1
        record % ip3 = ip3
        record % typvar = 'XX'
        record % nomvar = 'YYYY'
        record % grtyp  = 'X'
        record % ig1 = 0
        record % ig2 = 0
        record % ig3 = 0
        record % ig4 = 0

        record % data = c_loc(array_int32)
        record % pack_bits = 32
        record % data_bits = 32
        record % data_type = FST_TYPE_SIGNED
        record % nomvar = 'A'
        record % etiket = 'INT32'
        success = the_file % write(record)

        record % data = c_loc(array_real32)
        record % ip1 = 2
        record % data_type = FST_TYPE_REAL_IEEE
        record % nomvar = 'B'
        record % etiket = 'REAL32'
        success = the_file % write(record)

        record % data = c_loc(array_uint32)
        record % ip1 = 1
        record % ip2 = 2
        record % data_type = FST_TYPE_UNSIGNED
        record % nomvar = 'C'
        record % etiket = 'UINT32'
        success = the_file % write(record)

        record % data = c_loc(array_real64)
        record % data_bits = 64
        record % ip1 = 3
        record % ip2 = 1
        record % nomvar = 'D'
        record % etiket = 'REAL64'
        record % data_type = FST_TYPE_REAL_IEEE
        success = the_file % write(record)

        record % data = c_loc(array_uint16)
        record % pack_bits = 16
        record % data_bits = 16
        record % ip1 = 1
        record % ip2 = 3
        record % nomvar = 'E'
        record % etiket = 'UINT16'
        record % data_type = FST_TYPE_UNSIGNED
        success = the_file % write(record)

        record % data = c_loc(array_int16)
        record % ip1 = 1
        record % ip2 = 4
        record % data_type = FST_TYPE_SIGNED
        record % nomvar = 'F'
        record % etiket = 'INT16'
        success = the_file % write(record)

        record % data = c_loc(array_int8)
        record % pack_bits = 8
        record % data_bits = 8
        record % data_type = FST_TYPE_SIGNED
        record % ip1 = 1
        record % ip2 = 5
        record % nomvar = 'G'
        record % etiket = 'INT8'
        success = the_file % write(record)

        record % data = c_loc(array_uint8)
        record % data_type = FST_TYPE_UNSIGNED
        record % ip1 = 1
        record % ip2 = 6
        record % nomvar = 'H'
        record % etiket = 'UINT8'
        success = the_file % write(record)

        success = the_file % close()

        write(app_msg, '(A)')                                                                  &
            'Created file ' // filename // ' with type ' // filetype // ''
        call App_Log(APP_INFO, app_msg)

    end subroutine generate_file

end module fst_link_mod

program test_fstlnk
    use fst_link_mod
    implicit none

    integer :: status
    integer :: iun

    call generate_file(rsf_name_1, 'RSF', ip3_offset = 2)
    call generate_file(rsf_name_2, 'RSF', ip3_offset = 30)
    call generate_file(xdf_name_1, 'XDF', ip3_offset = 100)
    call generate_file(xdf_name_2, 'XDF', ip3_offset = 1234)

    call test_fst_link()

    ! These files should diseapear at test end
    status = fstouv('rsf_volatile', iun, 'STD+RND+RSF+VOLATILE')
    call check_status(status, expected = 0, fail_message = 'fstouv rsf VOLATILE')
    status = fstouv('xdf_volatile', iun, 'STD+RND+VOLATILE')
    call check_status(status, expected = 0, fail_message = 'fstouv xdf VOLATILE')

end program test_fstlnk
