
module fst24_delete_module
    use App
    use rmn_fst24
    implicit none

    character(len=*), parameter :: filename_rsf = 'delete.rsf'
    character(len=*), parameter :: filename_xdf = 'delete.xdf'

    character(len=2048) :: cmd

contains
    subroutine create_file(is_rsf)
        implicit none
        logical, intent(in) :: is_rsf

        integer, parameter :: NUM_DATA = 150
        real, dimension(NUM_DATA) :: dummy_data

        character(len=:), allocatable :: name
        character(len=:), allocatable :: options
        logical :: success
        type(fst_file) :: file
        type(fst_record) :: record

        if (is_rsf) then
            name = filename_rsf
            options = 'RSF'
        else
            name = filename_xdf
            options = 'XDF'
        end if

        write(cmd, '(A, (1X, A))') 'rm -fv ', name
        call execute_command_line(trim(cmd))

        success = file % open(name, options)

        if (.not. success) error stop 1

        record % data = c_loc(dummy_data)
        record % datyp = FST_TYPE_REAL
        record % dasiz = 32
        record % npak = -32
        record % ni = NUM_DATA
        record % nj = 1
        record % nk = 1

        record % dateo = 0
        record % deet = 0
        record % npas = 0

        record % ip1 = 1
        record % ip2 = 1
        record % ip3 = 1

        record % ig1 = 1
        record % ig2 = 1
        record % ig3 = 1
        record % ig4 = 1

        success = file % write(record)
        if (.not. success) error stop 1

        record % ip1 = 2
        success = file % write(record)
        if (.not. success) error stop 1
        record % ip1 = 3
        success = file % write(record)
        if (.not. success) error stop 1

        record % ip2 = 2
        success = file % write(record)
        if (.not. success) error stop 1
        record % ip2 = 3
        success = file % write(record)
        if (.not. success) error stop 1

        record % ip1 = 2
        success = file % write(record)
        if (.not. success) error stop 1

        success = file % close()
        if (.not. success) error stop 1

    end subroutine create_file

    subroutine test_delete(is_rsf)
        implicit none
        logical, intent(in) :: is_rsf

        character(len=:), allocatable :: name
        logical :: success
        integer :: i
        type(fst_file) :: file
        type(fst_record) :: record
        type(fst_query) :: query

        if (is_rsf) then
            name = filename_rsf
        else
            name = filename_xdf
        end if
        
        call create_file(is_rsf)

        success = file % open(name, options = 'R/W')
        if (.not. success) error stop 1

        query = file % new_query(ip1 = 2)
        if (.not. query % is_valid()) error stop 1

        do i = 1, 2
            success = query % find_next(record)
            if (.not. record % delete()) then
                call App_Log(APP_ERROR, 'Unable to delete record')
                error stop 1
            end if
        end do

        call file % print_summary()

        success = file % close()
        if (.not. success) error stop 1

    end subroutine test_delete
end module fst24_delete_module


program fst24_delete
    use fst24_delete_module
    implicit none

    call App_Log(APP_INFO, 'Testing RSF')
    call test_delete(.true.)
    call App_Log(APP_INFO, 'Testing XDF')
    call test_delete(.false.)
    
    call App_Log(APP_INFO, 'Test successful');
    
end program fst24_delete
