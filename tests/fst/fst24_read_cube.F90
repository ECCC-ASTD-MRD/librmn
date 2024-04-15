
module read_cube_module
    use App
    use rmn_fst24
    implicit none

    integer, parameter :: NUM_X = 8
    integer, parameter :: NUM_Y = 8
    integer, parameter :: NUM_Z = 5

    integer, dimension(NUM_X, NUM_Y, NUM_Z), target :: initial_data
    character(len=*), parameter :: test_filename = 'cube.fst'

contains

subroutine create_file()
    implicit none
    integer :: i, j, k
    character(len=2000) :: cmd

    type(fst_file)   :: test_file
    type(fst_record) :: rec
    logical :: success

    ! Initialize data
    do k = 1, NUM_Z
        do j = 1, NUM_Y
            do i = 1, NUM_X
                initial_data(i, j, k) = (k * NUM_Y + j) * NUM_X + i;
            end do
        end do
    end do

    write(cmd, '(A, (1X, A))') 'rm -fv ', test_filename
    call execute_command_line(trim(cmd))

    success = test_file % open(test_filename, 'RSF')
    if (.not. success) then
        call App_Log(APP_ERROR, 'Unable to open test file for creation')
        error stop 1
    end if

    rec % ni = NUM_X
    rec % nj = NUM_Y
    rec % nk = 1
    rec % data_type = FST_TYPE_SIGNED
    rec % data_bits = 32
    rec % pack_bits = 32

    rec % dateo = 0
    rec % ip1 = 1
    rec % ip2 = 1
    rec % ip3 = 1
    rec % deet = 0
    rec % npas = 0
    rec % ig1 = 0
    rec % ig2 = 0
    rec % ig3 = 0
    rec % ig4 = 0

    do k = 1, NUM_Z
        rec % data = c_loc(initial_data(1, 1, k))
        rec % ig1 = k
        rec % ip1 = k
        success = test_file % write(rec)

        if (.not. success) then
            call App_Log(APP_ERROR, 'Unable to write slice')
            error stop 1
        end if
    end do

    success = test_file % close()
end subroutine create_file

subroutine check_result(read_data)
    implicit none
    integer, dimension(NUM_X, NUM_Y, NUM_Z), intent(in) :: read_data

    integer :: i, j, k

    if (.not. all(read_data == initial_data)) then
        call App_Log(APP_ERROR, 'Data is not the same!')
        error stop 1
    end if
end subroutine check_result

end module read_cube_module

program read_cube
    use read_cube_module
    implicit none

    integer, dimension(NUM_X, NUM_Y, NUM_Z), target :: read_data
    integer :: k

    type(fst_file) :: test_file
    type(fst_record) :: rec
    logical :: success

    call create_file()

    read_data(:, :, :) = 0

    success = test_file % open(test_filename)
    if (.not. success) then
        call App_Log(APP_ERROR, 'Unable to open test file for reading')
        error stop 1
    end if

    do k = 1, NUM_Z

        ! Alternative method:
        ! rec % data = c_loc(read_data(1, 1, k))
        ! success = test_file % read(rec, ig1 = k)
        success = test_file % read(rec, data = c_loc(read_data(1, 1, k)), ig1 = k)

        if (.not. success) then
            write(app_msg, '(A, I3)') 'Unable to read slice ', k
            call App_Log(APP_ERROR, app_msg)
            error stop 1
        end if
    end do

    call check_result(read_data)

    call App_Log(APP_INFO, 'Test successful')
end program read_cube
