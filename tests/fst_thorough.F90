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
        call app_log(APP_INFO, 'testing fstvoi')
        status = rsf1 % voi(' ')
        call check_status(status, expected = 0, fail_message = 'fstvoi')

        status = rsf1 % frm()
        status = rsf2 % frm()
        status = xdf1 % frm()
        status = xdf2 % frm()

    end subroutine test_fst_link

    subroutine test_fst_prm()
        implicit none
        type(fstd98) :: rsf_file
        integer      :: status, num_records, handle

        integer(C_INT) :: ni, nj, nk, ni_prm, nj_prm, nk_prm
        integer(C_INT) :: date, deet, npas, nbits, datyp, ip1, ip2, ip3
        integer(C_INT) :: ig1, ig2, ig3, ig4, swa, lng, dlft, ubc, extra1, extra2, extra3
        character(len=:), allocatable :: typvar, nomvar, etiket, grtyp

        call App_Log(APP_INFO, 'test_fst_prm')

        num_records = rsf_file % ouv(rsf_name_1, 'STD+RND+RSF+R/O')
        call check_status(num_records, expected_min = 1, fail_message = 'fstouv num records')

        handle = rsf_file % inf(ni, nj, nk, -1, ' ', 1, -1, -1, ' ', ' ')
        call check_status(handle, expected_min = 0, fail_message = 'fstinf handle')

        status = fstprm(handle, date, deet, npas, ni_prm, nj_prm, nk_prm, nbits, datyp,     &
                        ip1, ip2, ip3, typvar, nomvar, etiket, grtyp, ig1, ig2, ig3, ig4,   &
                        swa, lng, dlft, ubc, extra1, extra2, extra3)

        ! write(app_msg, '(A, 5I6)') 'ni, nj, nk, nbits, lng = ', ni, nj, nk, nbits, lng
        ! call app_log(APP_INFO, app_msg)

        if (ni * nj * nk * nbits / 32 < 50) then
            call app_log(APP_WARNING, 'Size of record is too small for test to work, it should be at least 50 32-bit words')
        end if

        call check_status(ni_prm, expected = ni, fail_message = 'fstprm ni')
        call check_status(nj_prm, expected = nj, fail_message = 'fstprm nj')
        call check_status(nk_prm, expected = nk, fail_message = 'fstprm nk')
        call check_status(lng, expected_min = ni * nj * nk * nbits / 32, fail_message = 'fstprm lng')

        status = rsf_file % frm()
    end subroutine test_fst_prm

end module fst_thorough_mod

program fst_thorough
    use fst_thorough_mod
    implicit none

    call generate_file(rsf_name_1, .true.)
    call generate_file(rsf_name_2, .true.)
    call generate_file(xdf_name_1, .false.)
    call generate_file(xdf_name_2, .false.)

    call test_fst_link()
    call test_fst_prm()

end program fst_thorough
