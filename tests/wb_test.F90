

program wb_test

#include <WhiteBoard.hf>
    
   character(len=32) :: list_S(4)

    list_S = (/'a123', 'b2  ', 'c34 ', 'd   '/)
    istat = wb_put('test/wb',list_S,WB_REWRITE_MANY)
    if (istat<0) then
        stop 1
      endif
      
    ier = wb_checkpoint()

    print *,'TEST: wb_put',ier,istat,list_S

    ! Need to reset becuase we reload in the same run
    ier= wb_reset()

    ier= wb_reload()
    list_S = ' '
    istat = wb_get('test/wb',list_S,nitems)
    print *,'TEST: wb_get',ier,istat,list_S

    if (istat<0 .or. ier<0) then
      stop 1
    endif
end program