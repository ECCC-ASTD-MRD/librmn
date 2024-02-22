
module test_fst24_interface_module
    use App
    use rmn_fst24
    implicit none
contains

function create_file(name, is_rsf, ip2, ip3) result(success)
    implicit none
    character(len=*), intent(in) :: name
    logical, intent(in) :: is_rsf
    integer, intent(in) :: ip2, ip3
    logical :: success
end function

function test_fst24_interface(is_rsf) result(success)
    implicit none
    logical, intent(in) :: is_rsf
    logical :: success

    character(len=*), parameter :: test_file_name = 'fst24_interface.fst'
    character(len=2000) :: cmd

    type(fst24_file) :: test_file

    success = .false.

    ! Remove file(s) so that we have a fresh start
    write(cmd, '(A, (1X, A))') 'rm -fv ', test_file_name
    call execute_command_line(trim(cmd))

    if (is_rsf) then
        success = test_file % open(test_file_name, 'STD+RND+RSF')
    else
        success = test_file % open(test_file_name, 'STD+RND')
    end if

    if (.not. success) then
        call App_log(APP_ERROR, 'Unable to open test FST file')
        return
    end if

    success = test_file % close()

    if (.not. success) then
        call App_log(APP_ERROR, 'Unable to close the file')
        return
    end if

    success = .true.
end function test_fst24_interface

end module test_fst24_interface_module


program fst24_interface
    use test_fst24_interface_module
    if (.not. test_fst24_interface(.true.)) error stop 1
    if (.not. test_fst24_interface(.false.)) error stop 1
end program fst24_interface
