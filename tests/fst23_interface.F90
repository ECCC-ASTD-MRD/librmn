
module test_fst23_interface_module
    use App
    use rmn_fstd23
    implicit none
contains

function test_fst23_interface(is_rsf) result(success)
    implicit none
    logical, intent(in) :: is_rsf
    logical :: success

    character(len=*), parameter :: test_file_name = 'fst23_interface.fst'
    character(len=2000) :: cmd

    type(fstd23) :: test_file

    success = .false.

    ! Remove file(s) so that we have a fresh start
    write(cmd, '(A, (1X, A))') 'rm -fv ', test_file_name
    call execute_command_line(trim(cmd))

    if (is_rsf) then
        call test_file % open(test_file_name, 'STD+RND+RSF')
    else
        call test_file % open(test_file_name, 'STD+RND')
    end if

    if (.not. test_file % is_open()) then
        call App_log(APP_ERROR, 'Unable to open test FST file')
        return
    end if

    success = .true.
end function test_fst23_interface

end module test_fst23_interface_module


program fst23_interface
    use test_fst23_interface_module
    if (.not. test_fst23_interface(.true.)) error stop 1
    if (.not. test_fst23_interface(.false.)) error stop 1
end program fst23_interface
