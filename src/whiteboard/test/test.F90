program test_wb
#include <WhiteBoard.hf>

    character(len = *), parameter :: key = 'Something'
    character(len = WB_MAXSTRINGLENGTH) :: inbuff, outbuff
    integer :: stat, i

    do i = 1, WB_MAXSTRINGLENGTH - 1
        inbuff(i:i) = 'a'
    end do
    inbuff(WB_MAXSTRINGLENGTH:WB_MAXSTRINGLENGTH) = char(0)

    stat = wb_put(key, inbuff, WB_REWRITE_MANY)
    stat = wb_get(key, outbuff)

    print *, "trim(inbuff) = '" // trim(inbuff) // "'"
    print *, "trim(inbuff) == trim(outbuff) =", trim(inbuff) == trim(outbuff)
    print *, "trim(outbuff) = '" // trim(outbuff) // "'"
end program