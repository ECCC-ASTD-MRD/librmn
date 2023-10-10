program test_wb
use ISO_FORTRAN_ENV
use whiteboard_mod
implicit none
!#include <rmn/WhiteBoard.hf>

    character(len = *), parameter :: key = 'Something'
    character(len = WB_MAXSTRINGLENGTH) :: inbuff, outbuff
    integer :: stat, i

    do i = 1, WB_MAXSTRINGLENGTH 
        inbuff(i:i) = 'a'
    end do

    stat = wb_put(key, inbuff, WB_REWRITE_MANY)
    stat = wb_get(key, outbuff)

    print *, "trim(inbuff)  = '" // trim(inbuff) // "'"
    print *, "trim(outbuff) = '" // trim(outbuff) // "'"
    print *, "trim(inbuff) == trim(outbuff) =", trim(inbuff) == trim(outbuff)

    if (.not. (trim(inbuff) == trim(outbuff))) then
       stop 1
    endif
end program
