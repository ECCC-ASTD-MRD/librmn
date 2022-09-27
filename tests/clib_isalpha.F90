program clib_interface

#include <rmn/clib_interface.cdk>

    character(len = 1) :: str
    integer :: res

    str = 's'
    res = clib_isalpha(str)
    if (res == 0) then
        error stop
    end if
end program clib_interface
