integer function f_iargc() bind(C, name = 'iargc')
    implicit none
    f_iargc = command_argument_count()
end


subroutine f_getarg(pos, val)
    implicit none

    integer, intent(in) :: pos
    character(len = *), intent(out)  :: val

    integer :: valLen
    integer :: stat

    call get_command_argument(pos, val, valLen, stat)
end

