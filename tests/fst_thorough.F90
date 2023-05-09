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

        status = fstlnk(unit_list, 4) ! Should fail
        call check_status(status, expected_max = -1, fail_message = 'fstlnk with twice the same file')

        unit_list(3) = xdf2 % iun
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
