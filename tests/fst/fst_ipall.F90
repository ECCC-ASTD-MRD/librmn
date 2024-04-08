
module fst98_ipall_module
    use rmn_fst98
    use rmn_fst24
    implicit none

    character(len=*), dimension(2), parameter :: filenames = ['ip1.fst', 'ip2.fst']

    real, dimension(3), parameter :: p = [ 0.1, 0.2, 0.3 ]
    integer, parameter :: ip_kind = 2
    character(len=1) :: dummy

contains

subroutine create_files(is_rsf)
    implicit none
    logical, dimension(2), intent(in) :: is_rsf

    type(fst_file), dimension(2) :: files
    character(len=2000) :: cmd
    integer :: i, j
    real, dimension(1), target :: work
    logical :: success

    integer :: ip

    type(fst_record) :: record

    write(app_msg, '(A, 2L2)') 'Creating test files ', is_rsf
    call App_Log(APP_INFO, app_msg)

    ! Remove file so that we have a fresh start
    do i = 1, 2
        write(cmd, '(A, 2(1X, A))') 'rm -fv ', filenames(i)
        call execute_command_line(trim(cmd))

        if (is_rsf(i)) then
            success = files(i) % open(filenames(i), 'RSF')
        else
            success = files(i) % open(filenames(i), 'XDF')
        end if

        if (.not. success) then
            call App_Log(APP_ERROR, 'Could not open (create) file')
            error stop 1
        end if
    end do

    record % dateo = 0
    record % datev = 0
    record % deet  = 0
    record % npas  = 0
    record % ni  = 1
    record % nj  = 1
    record % nk  = 1
    record % ip1 = 1
    record % ip2 = 1
    record % ip3 = 1
    record % typvar = 'XX'
    record % nomvar = 'YYYY'
    record % grtyp  = 'X'
    record % ig1 = 0
    record % ig2 = 0
    record % ig3 = 0
    record % ig4 = 0

    record % data = c_loc(work)
    record % npak  = -32
    record % dasiz = 32
    record % datyp = FST_TYPE_SIGNED
    record % nomvar = 'A'
    record % etiket = 'INT32'

    record % ip2 = 1
    record % ip3 = 1
    do i = 1, 3
        call CONVIP_plus(record % ip1, p(i), ip_kind, 3, dummy, .false.)
        success = files(2) % write(record) .and. success
        record % ip2 = record % ip2 + 1
        record % nomvar(1:1) = 'B'
        success = files(2) % write(record) .and. success
        call CONVIP_plus(record % ip1, p(i), ip_kind, 2, dummy, .false.)
        record % ip2 = record % ip2 + 1
        record % nomvar(1:1) = 'C'
        success = files(2) % write(record)
        record % ip2 = record % ip2 + 1
        record % nomvar(1:1) = 'D'
        success = files(2) % write(record)
        success = files(2) % write(record)

        if (.not. success) then
            call App_Log(APP_ERROR, 'Unable to write record')
            error stop 1
        end if
    end do

    record % ip1 = 10
    record % ip3 = 10
    do i = 1, 3
        call CONVIP_plus(record % ip2, p(i), ip_kind, 2, dummy, .false.)
        success = files(2) % write(record)
        call CONVIP_plus(record % ip2, p(i), ip_kind, 3, dummy, .false.)
        success = files(2) % write(record) .and. success

        if (.not. success) then
            call App_Log(APP_ERROR, 'Unable to write record')
            error stop 1
        end if
    end do

    record % ip1 = 100
    record % ip2 = 100
    do i = 1, 3
        call CONVIP_plus(record % ip3, p(i), ip_kind, 2, dummy, .false.)
        success = files(2) % write(record)
        call CONVIP_plus(record % ip3, p(i), ip_kind, 3, dummy, .false.)
        success = files(2) % write(record) .and. success

        if (.not. success) then
            call App_Log(APP_ERROR, 'Unable to write record')
            error stop 1
        end if
    end do

    success = files(1) % close()
    if (.not. success) error stop 1

    success = files(2) % close()

    success = files(2) % open(filenames(2), '')
    success = files(1) % open(filenames(1), '') .and. success
    if (.not. success) error stop 1
end subroutine create_files

subroutine look_fst98()
    implicit none
    integer :: status, status2
    integer, dimension(2) :: units
    integer, dimension(10) :: records
    integer, dimension(1000) :: work

    integer :: i
    integer :: ni, nj, nk
    integer :: ip
    integer :: num_record_found

    call App_Log(APP_INFO, 'Testing fst98 interface')

    units(:) = 0
    status = fstouv(filenames(1), units(1), 'RND+R/O')
    if (status /= 0) then
        call App_Log(APP_ERROR, 'Could not open 1st file')
        error stop 1
    end if

    status = fstouv(filenames(2), units(2), 'RND')
    if (status /= 27) then
        write(app_msg, '(A, I12, A)') 'Wrong number of records in file (', status, ') expected 18'
        call App_Log(APP_ERROR, app_msg)
        error stop 1
    end if

    status = fstlnk(units, 2)
    if (status /= 0) then
        call App_Log(APP_ERROR, 'Failed to link the 2 test files')
        error stop 1
    end if

    ! IP1 not all
    call CONVIP_plus(ip, p(1), ip_kind, 2, dummy, .false.)
    status = fstinl(units(1), ni, nj, nk, -1, ' ', ip, -1, 1, ' ', ' ',          &
                        records, num_record_found, 10)
    if ((status /= 0) .or. (num_record_found /= 3)) then
        write(app_msg, '(A, I12)') 'Should have found exactly 3 records (ip1) rather than ', num_record_found
        call App_Log(APP_ERROR, app_msg)
        error stop 1
    end if

    do i = 1, num_record_found
        status = fstluk(work, records(i), ni, nj, nk)
    end do

    ! IP1 all
    ip = ip1_all(p(1), ip_kind) 
    status = fstinl(units(1), ni, nj, nk, -1, ' ', ip, -1, 1, ' ', ' ',          &
                        records, num_record_found, 10)

    if ((status /= 0) .or. (num_record_found /= 2)) then
        write(app_msg, '(A, I12)') 'Should have found exactly 2 records (ip1 all) rather than ', num_record_found
        call App_Log(APP_ERROR, app_msg)
        error stop 1
    end if

    do i = 1, num_record_found
        status = fstluk(work, records(i), ni, nj, nk)
    end do

    status  = fstfrm(units(1))
    status2 = fstfrm(units(2))
    if (status < 0 .or. status2 < 0) then
        call App_Log(APP_ERROR, 'Could not close test files')
        error stop 1
    end if
end subroutine look_fst98

subroutine look_fst24()
    implicit none

    call App_Log(APP_INFO, 'Testing fst24 interface')
end subroutine look_fst24

subroutine test_ip_all(is_rsf)
    implicit none
    logical, dimension(2), intent(in) :: is_rsf

    call create_files(is_rsf)
    call look_fst98()
    call look_fst24()

end subroutine test_ip_all

end module fst98_ipall_module

program fst98_ipall
    use fst98_ipall_module
    implicit none

    call test_ip_all([ .false., .false. ])
    ! call test_ip_all([ .false., .true.  ])
    ! call test_ip_all([ .true.,  .false. ])
    ! call test_ip_all([ .true.,  .true.  ])

end program fst98_ipall
