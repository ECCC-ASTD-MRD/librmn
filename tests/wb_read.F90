program wb

#include <rmn/WhiteBoard.hf>

    integer :: res
    integer :: argc
    character(len = 1024) :: filePath

    argc = command_argument_count()
    if (argc /= 1) then
        print *, "Command syntax: wb <Dict file path>"
        error stop
    end if

    call get_command_argument(1, filePath)
!     character(len = 8) :: val
!     character(len = *), parameter :: key = "spp"
! 
!     val = "01234567"
!     print *, '"', val, '"'
!     res = wb_put(key, val)
!     print *, "res=", res
!     res = wb_get(key, val)
!     print *, "res=", res
!     print *, '"', val, '"'

    call f_wb_verbosity(WB_MSG_DEBUG)

    res = wb_read('spp/a/', filePath, 'spp', WB_STRICT_DICTIONARY)
    if (res /= 0) then
        print *, 'res = ', res
        error stop
    end if
end program wb
