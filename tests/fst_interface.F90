
#ifndef FST_TEST_IS_RSF
#define FST_TEST_IS_RSF .true.
#endif

program fst_interface
    use app
    use rmn_test_helper
    use rmn_fstd98
    implicit none

    integer, external :: fnom

    integer :: status, i
    logical :: is_rsf
    character(len=*), parameter :: test_file_name = 'fst_interface.fst'

    integer, parameter :: NUM_DATA = 8
    integer, dimension(NUM_DATA) :: data_array, work_array

    integer, parameter :: UNIT_NUMBER = 11
    integer, parameter :: DATE = 0, TIMESTEP_SIZE = 0, TIMESTEP_NUM = 0
    integer, parameter :: DATATYPE = 4 ! 4 = integer
    integer, parameter :: REWRITE = 0

    integer :: ni, nj, nk
    integer :: num_records, new_num_records

    type(fstd98) :: test_file

    is_rsf = FST_TEST_IS_RSF

    do i = 1, NUM_DATA
        data_array(i) = i
    end do

    ! --- fstouv ---
    if (is_rsf) then
        status = test_file % ouv(test_file_name, 'STD+RND+RSF')
    else
        status = test_file % ouv(test_file_name, 'STD+RND')
    end if
    call check_status(status, expected_min = 0, fail_message = 'ouv')

    num_records = status

    ! --- fstecr ---
    status = test_file % ecr(data_array, work_array, -8, DATE, TIMESTEP_SIZE, TIMESTEP_NUM,             &
                             NUM_DATA, 1, 1,                                                            &
                             1, 0, 0,                                                                   &
                             'XX', 'YYYY', 'ETIKET', 'X',                                               &
                             0, 0, 0, 0,                                                                &
                             DATATYPE,  REWRITE)
    call check_status(status, expected = 0, fail_message = 'ecr')

    ! --- fstnbr ---
    new_num_records = test_file % nbr()
    call check_status(new_num_records, expected = num_records + 1, fail_message = 'nbr')

    ! --- fstfrm ---
    status = test_file % frm()
    call check_status(status,expected = 0, fail_message = 'frm')

    ! --- fstouv ---
    status = test_file % ouv(test_file_name, 'STD+RND')
    call check_status(status, expected_min = 1, fail_message = 'ouv (second one)')

    ! --- fstnbr ---
    new_num_records = test_file % nbr()
    call check_status(new_num_records, expected = num_records + 1, fail_message = 'nbr')

    ! --- fstlir ---
    work_array(:) = 0
    status = test_file % lir(work_array, ni, nj, nk, -1, ' ', 1, -1, -1, ' ', ' ')
    call check_status(status, expected_min = 1, fail_message = 'lir')
    if (.not. all(work_array == data_array)) then
        write(app_msg, '(A, 2(5X, 8I3))') 'Got different data!!!', work_array, data_array
        call App_Log(APP_ERROR, app_msg)
        error stop 1
    end if

    ! --- fstfrm ---
    status = test_file % frm()
    call check_status(status,expected = 0, fail_message = 'frm (second one)')

    call App_Log(APP_INFO, 'done')

end program fst_interface