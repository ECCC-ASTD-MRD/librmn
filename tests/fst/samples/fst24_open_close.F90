
subroutine remove_file(name)
    character(len=*), intent(in) :: name
    character(len=1024) :: cmd
    write(cmd, '("rm -fv ", A)') name
    call execute_command_line(trim(cmd))
end subroutine remove_file

program fst24_interface
    use rmn_fst24
    implicit none

    type(fst_file) :: my_file
    logical :: success
  
    call remove_file('my_file.fst')
  
    success = my_file % open('my_file.fst', options='R/W')
  
    if (.not. success) then
        ! Deal with error
        error stop 1
    end if
  
    success = my_file % close()

    if (.not. success) error stop 1

end program
