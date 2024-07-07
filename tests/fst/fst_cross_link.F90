
module fst_cross_link_module
    use App
    use rmn_fst24
    use rmn_fst98
    implicit none

    character(len=*), parameter :: filename1 = 'file1.fst'
    character(len=*), parameter :: filename2 = 'file2.fst'
    character(len=2048) :: cmd

    real, dimension(1), target :: dummy_data

contains
    subroutine create_files(is_rsf1, is_rsf2)
        implicit none
        logical, intent(in) :: is_rsf1, is_rsf2

        type(fst_file) :: f
        type(fst_record) :: r
        logical :: success

        ! Remove file(s) so that we have a fresh start
        write(cmd, '(A, (1X, A))') 'rm -fv ', filename1
        call execute_command_line(trim(cmd))
        write(cmd, '(A, (1X, A))') 'rm -fv ', filename2
        call execute_command_line(trim(cmd))

        ! Create empty first file
        if (is_rsf1) then
            success = f % open(filename1, options = 'RSF+R/W')
        else
            success = f % open(filename1, options = 'XDF+R/W')
        end if
        if (.not. success) error stop 1
        success = f % close()
        if (.not. success) error stop 1

        ! Create second file with 1 record
        if (is_rsf2) then
            success = f % open(filename2, options = 'RSF+R/W')
        else
            success = f % open(filename2, options = 'XDF+R/W')
        end if
        if (.not. success) error stop 1

        r % data = c_loc(dummy_data)
        r % ni = 1
        r % nj = 1
        r % nk = 1
        r % data_type = FST_TYPE_REAL
        r % data_bits = 32
        r % pack_bits = 32
        r % deet = 0
        r % npas = 0
        r % dateo = 0
        r % ip1 = 0
        r % ip2 = 0
        r % ip3 = 0
        r % ig1 = 0
        r % ig2 = 0
        r % ig3 = 0
        r % ig4 = 0

        success = f % write(r)
        if (.not. success) then
            call App_Log(APP_ERROR, 'Could not write record')
            error stop 1
        end if

        success = f % close()
        if (.not. success) error stop 1

    end subroutine create_files

    subroutine test_cross_link(is_rsf1, is_rsf2)
        implicit none
        logical, intent(in) :: is_rsf1, is_rsf2

        type(fst_file)   :: f1, f2
        type(fst_query)  :: q
        type(fst_record) :: r
        logical :: success
        integer :: handle
        integer :: ni, nj, nk
        integer :: iun, status

        call create_files(is_rsf1, is_rsf2)

        success = f1 % open(filename1) .and. f2 % open(filename2)
        if (.not. success) error stop 1

        q = f2 % new_query()
        success = q % find_next(r)
        if (.not. success) error stop 1
        call q % free()

        q = f1 % new_query()
        success = .not. q % find_next(r)
        if (.not. success) error stop 1

        success = fst24_link([f1, f2])
        if (.not. success) error stop 1

        success = q % find_next(r)
        if (.not. success) error stop 1

        handle = fstinf(f2 % get_unit(), ni, nj, nk, -1, ' ', -1, -1, -1, ' ', ' ')
        if (handle <= 0) then
            call App_Log(APP_ERROR, 'fstinf should have found a record in the second file!')
            error stop 1
        end if

        handle = fstinf(f1 % get_unit(), ni, nj, nk, -1, ' ', -1, -1, -1, ' ', ' ')
        if (handle <= 0) then
            call App_Log(APP_ERROR, 'fstinf should have found the record by looking through the first file')
            error stop 1
        end if

        call q % rewind()

        success = f1 % unlink()
        if (.not. success) error stop 1

        success = .not. q % find_next(r)
        if (.not. success) error stop 1

        handle = fstinf(f1 % get_unit(), ni, nj, nk, -1, ' ', -1, -1, -1, ' ', ' ')
        if (handle > 0) then
            call App_Log(APP_ERROR, 'fstinf should not have found the record after unlinking')
            error stop 1
        end if

        call q % free()

        success = f1 % close() .and. f2 % close()
        if (.not. success) error stop 1
    end subroutine test_cross_link

end module fst_cross_link_module

program fst_cross_link
    use fst_cross_link_module
    implicit none

    call test_cross_link(.true., .true.)
    call test_cross_link(.true., .false.)
    call test_cross_link(.false., .true.)
    call test_cross_link(.false., .false.)

    call App_Log(APP_INFO, 'Test successful')
end program fst_cross_link
