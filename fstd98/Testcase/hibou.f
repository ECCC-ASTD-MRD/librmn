
        program hibou

        real lev,p2
        integer kind,mode,ip1_old,ip1_new,ip2
        character *50 blk_S

*        call convip(ip1, lev, kind,0, blk_S, .false.)
        ip1_old = 1620
        print *,'ip1_old=',ip1_old
        call convip(ip1_old, lev, kind,-1, blk_S, .true.)
        print *,'kind=',kind
        print *,'lev=',lev
        print *,'blk_S=',blk_S
        call convip(ip1_new, lev, kind,+2, blk_S, .false.)
        print *,'ip1_new=',ip1_new
        read(blk_S,*) lev
        call convip(ip1_new, lev, kind,+2, blk_S, .false.)
        print *,'ip1_new from string=',ip1_new
c        p2 = .0001/3600.
        p2 = 1.001E10
        call convip(ip2,p2,10,+2,blk_S,.false.)
        print *,'p2=',p2
        call convip(ip2,p2,kind,-1,blk_S,.false.)
        print *,'p2=',p2
        stop
        end


