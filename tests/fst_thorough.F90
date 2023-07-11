module fst_thorough_mod
    use generate_fstd_mod
    use rmn_test_helper
    implicit none

    character(len=*), parameter :: rsf_name_1 = 'rsf1.fst'
    character(len=*), parameter :: rsf_name_2 = 'rsf2.fst'
    character(len=*), parameter :: xdf_name_1 = 'xdf1.fst'
    character(len=*), parameter :: xdf_name_2 = 'xdf2.fst'

contains

    subroutine test_fst_link()
        implicit none
        type(fstd98) :: rsf1, rsf2, xdf1, xdf2
        integer      :: status
        integer      :: single_num_records, current_num_records
        integer(C_INT32_T), dimension(4) :: unit_list
        integer      :: record_key
        integer, dimension(2000) :: work_array

        single_num_records = rsf1 % ouv(rsf_name_1, 'STD+RND+RSF')
        status = rsf2 % ouv(rsf_name_2, 'STD+RND+RSF')
        status = xdf1 % ouv(xdf_name_1, 'STD+RND')
        status = xdf2 % ouv(xdf_name_2, 'STD+RND')

        ! --------------------------------------------
        ! The link operation itself
        unit_list(1) = rsf1 % iun
        unit_list(2) = xdf1 % iun
        unit_list(3) = xdf1 % iun ! We want the wrong one, xdf1, to check for fstlnk failure
        unit_list(4) = rsf2 % iun

        write(app_msg, '(A, 1X, 4I4, 1X, A)') 'fstlnk with ', unit_list, '(should fail)'
        call app_log(APP_INFO, app_msg)
        status = fstlnk(unit_list, 4) ! Should fail
        call check_status(status, expected_max = -1, fail_message = 'fstlnk with twice the same file')

        unit_list(3) = xdf2 % iun
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
        current_num_records = rsf1 % nbr()
        call check_status(current_num_records, expected = single_num_records * 4, fail_message = 'nbr from first file in lnk')

        current_num_records = xdf1 % nbr()
        call check_status(current_num_records, expected = single_num_records * 3, fail_message = 'nbr from second file in lnk')

        current_num_records = xdf2 % nbr()
        call check_status(current_num_records, expected = single_num_records * 2, fail_message = 'nbr from third file in lnk')

        current_num_records = rsf2 % nbr()
        call check_status(current_num_records, expected = single_num_records * 1, fail_message = 'nbr from fourth file in lnk')

        status = fstunl()
        current_num_records = rsf1 % nbr()
        call check_status(current_num_records, expected = single_num_records * 1, fail_message = 'nbr from first file in lnk after unlink')
        current_num_records = xdf1 % nbr()
        call check_status(current_num_records, expected = single_num_records * 1, fail_message = 'nbr from second file in lnk after unlink')
        current_num_records = xdf2 % nbr()
        call check_status(current_num_records, expected = single_num_records * 1, fail_message = 'nbr from third file in lnk after unlink')
        current_num_records = rsf2 % nbr()
        call check_status(current_num_records, expected = single_num_records * 1, fail_message = 'nbr from fourth file in lnk after unlink')
        
        status = fstlnk(unit_list, 4)
        call check_status(status, expected = 0, fail_message = 'fstlnk with correct input (after nbr with unlink)')

        ! ---------------------------------------------
        ! fstlir
        block
            integer :: ni, nj, nk
            record_key = rsf1 % lir(work_array, ni, nj, nk, -1, ' ', 1, -1, -1, 'nooooooo', ' ')
            call check_status(record_key, expected_max = -1, fail_message = 'lir (not found)')
            work_array(:) = 0
            record_key = rsf1 % lir(work_array, ni, nj, nk, -1, ' ', 1, -1, -1, ' ', ' ')
            call check_status(record_key, expected_min = 1, fail_message = 'lir (found)')
        end block

        ! ---------------------------------------------
        ! fstinl (incl. fstsui)
        block
            integer, dimension(single_num_records * 4) :: record_keys
            integer :: num_record_found
            integer :: ni, nj, nk
            status = rsf1 % inl(ni, nj, nk, -1, ' ', -1, -1, -1, ' ', ' ',          &
                                record_keys, num_record_found, single_num_records * 4)
            call check_status(status, expected = 0, fail_message = 'fstinl status')
            call check_status(num_record_found, expected = single_num_records * 4, fail_message = 'fstinl count (1st file)')

            status = xdf1 % inl(ni, nj, nk, -1, ' ', -1, -1, -1, ' ', ' ',          &
                                record_keys, num_record_found, single_num_records * 3)
            call check_status(status, expected = 0, fail_message = 'fstinl status')
            call check_status(num_record_found, expected = single_num_records * 3, fail_message = 'fstinl count (2nd file)')

            status = xdf2 % inl(ni, nj, nk, -1, ' ', -1, -1, -1, ' ', ' ',          &
                                record_keys, num_record_found, single_num_records * 3)
            call check_status(status, expected = 0, fail_message = 'fstinl status')
            call check_status(num_record_found, expected = single_num_records * 2, fail_message = 'fstinl count (3rd file)')

            status = rsf2 % inl(ni, nj, nk, -1, ' ', -1, -1, -1, ' ', ' ',          &
                                record_keys, num_record_found, single_num_records * 3)
            call check_status(status, expected = 0, fail_message = 'fstinl status')
            call check_status(num_record_found, expected = single_num_records * 1, fail_message = 'fstinl count (4th file)')
        end block

        ! ---------------------------------------------
        ! fstnbrv
        current_num_records = rsf1 % nbrv()
        call check_status(current_num_records, expected = single_num_records * 4, fail_message = 'nbrv from first file in lnk')

        current_num_records = xdf1 % nbrv()
        call check_status(current_num_records, expected = single_num_records * 3, fail_message = 'nbrv from second file in lnk')

        current_num_records = xdf2 % nbrv()
        call check_status(current_num_records, expected = single_num_records * 2, fail_message = 'nbrv from third file in lnk')

        current_num_records = rsf2 % nbrv()
        call check_status(current_num_records, expected = single_num_records * 1, fail_message = 'nbrv from fourth file in lnk')

        ! ---------------------------------------------
        ! fstvoi
        status = rsf1 % voi(' ')
        call check_status(status, expected = 0, fail_message = 'fstvoi')

        status = rsf1 % frm()
        status = rsf2 % frm()
        status = xdf1 % frm()
        status = xdf2 % frm()

    end subroutine test_fst_link

end module fst_thorough_mod

program fst_thorough
    use fst_thorough_mod
    implicit none

    call generate_file(rsf_name_1, .true.)
    call generate_file(rsf_name_2, .true.)
    call generate_file(xdf_name_1, .false.)
    call generate_file(xdf_name_2, .false.)

    call test_fst_link()

end program fst_thorough
