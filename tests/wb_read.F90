program wb_read_test
use ISO_FORTRAN_ENV
implicit none

#include <rmn/WhiteBoard.hf>


    integer :: res
    integer :: argc
    character(len = 1024) :: filePath
    type(whiteboard) :: my_wb

    argc = command_argument_count()
    if (argc /= 1) then
        print *, "Command syntax: wb <Dict file path>"
        error stop
    end if

    call get_command_argument(1, filePath)
    call f_wb_verbosity(WB_MSG_DEBUG)

    my_wb%wb = 0
    res = wb_read('spp/a/', filePath, 'spp', WB_STRICT_DICTIONARY)
    if (res /= 0) then
        print *, 'res = ', res
        error stop
    end if
    if(res == 123456789) then  ! syntax check
      res = wb_read('spp/a/', filePath, 'spp', WB_STRICT_DICTIONARY, my_wb)
    endif
end program
