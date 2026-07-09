
module link_test_mod
    use app
    use rmn_fst24
    implicit none

    integer, dimension(1), target :: dummy_data = [ 123 ]

contains

    subroutine run_test()
        implicit none
        
        character(len=64), dimension(4) :: filenames = [ 'f1.fst', 'f2.fst', 'no.fst', 'f4.fst' ]
        integer :: i
        character(len=1024*4) :: cmd

        type(fst_file) :: all_the_files
        logical :: success
        integer(C_INT64_T) :: num_records

        do i = 1, size(filenames)
            write(cmd, '(A, (1X, A))') 'rm -fv ', filenames(i)
            call execute_command_line(trim(cmd))
        end do

        call create_file(filenames(1), 'f1', 'RSF')
        call create_file(filenames(2), 'f2', 'XDF')
        call create_file(filenames(4), 'f4', 'XDF')

        call app_log(APP_ALWAYS, 'Opening 4, but expecting one of them to fail')
        success = all_the_files % open_and_link(filenames)

        if (.not. success) then
            call app_log(APP_ERROR, 'Unable to open any file')
            error stop 1
        end if

        num_records = all_the_files % get_num_records()
        
        if (num_records /= 3) then
            write(app_msg, '("Found ", I3, " records, but expected 3")') num_records
            call app_log(APP_ERROR, app_msg)
            error stop 1
        end if

        call all_the_files % print_summary()

        success = all_the_files % close()
        if (.not. success) then
            call app_log(APP_ERROR, "Unable to close all files")
            error stop 1
        end if
        
    end subroutine run_test

    !> Create a file containing 1 record (we assume the file does not already exist)
    subroutine create_file(filename, nomvar, filetype)
        implicit none
        character(len=*), intent(in) :: filename    !< Name of the file
        character(len=*), intent(in) :: nomvar      !< nomvar parameter of the record
        character(len=*), intent(in) :: filetype    !< Must be either 'RSF' or 'XDF'

        type(fst_file) :: f
        type(fst_record) :: rec
        logical :: success

        success = f % open(filename, options='R/W+'//filetype)
        if (.not. success) then
            write(app_msg, '("Unable to open file ", A, " for creation")') filename
            call app_log(APP_ERROR, app_msg)
            error stop 1
        end if

        rec % data = c_loc(dummy_data)
        rec % pack_bits = 32
        rec % ni   = 1
        rec % nj   = 1
        rec % nk   = 1
        rec % dateo= 20220610
        rec % deet = 300
        rec % npas = 0
        rec % ip1  = 1
        rec % ip2  = 10
        rec % ip3  = 100
        rec % typvar = 'P'
        rec % nomvar = nomvar
        rec % etiket = 'int'
        rec % grtyp  = 'X'
        rec % ig1   = 0
        rec % ig2   = 0
        rec % ig3   = 0
        rec % ig4   = 0
        rec % data_type = FST_TYPE_SIGNED
        rec % data_bits = 32

        success = f % write(rec)
        if (.not. success) then
            write(app_msg, '("Unable to write record to file ", A)') filename
            call app_log(APP_ERROR, app_msg)
            error stop 1
        end if

        success = f % close()
        if (.not. success) then
            write(app_msg, '("Unable to close file ", A, " after creation")') filename
            call app_log(APP_ERROR, app_msg)
            error stop 1
        end if

        write(app_msg, '("Created ", A)') filename
        call app_log(APP_ALWAYS, app_msg)
    end subroutine create_file

end module link_test_mod

program open_link_test
    use link_test_mod
    implicit none
    call run_test()
end program open_link_test
