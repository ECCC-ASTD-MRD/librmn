!> Manage user locks
subroutine set_user_lock(lockId, lock)
! call set_user_lock(lock_variable, .true. ) to acquire lock
! call set_user_lock(lock_variable, .false.) to release lock
    use app
    implicit none

    integer, intent(INOUT) :: lockId !< Id of the lock, MUST be negative
    logical, intent(IN) :: lock !< Try to acquire lock when .true., Otherwise, release

#if defined(_OPENMP)
    integer, external :: omp_get_thread_num
    integer :: current_pid, owner_pid, owner_count
    logical :: ok

    ! if (lockId < 0) lockId = 0
    current_pid = omp_get_thread_num()
    ! keep lower 24 bits (8-31)
    current_pid = iand(current_pid, Z"FFFFFF")

    ! print *,'thread=',current_pid,' lock=',lock,' owner_pid=',owner_pid,' count=',owner_count
    ok = .false.
    do while (.not. ok)
!$OMP CRITICAL
        owner_pid   = iand(lockId, Z"FFFFFF")  ! lower 24 bits (8-31)
        owner_count = ishft(lockId, -24)       ! bits 1-7
        owner_count = iand(owner_count, 127)
        if (lock) then
            ! print *,'attempt to lock by thread',current_pid
            if (owner_count == 0 .or. owner_pid == current_pid) then
            ! lock is not owned or already owned by this thread, acquire it
                ! add one to depth
                owner_count = owner_count + 1
                lockId = current_pid + ishft(owner_count, 24)
                ok = .true.
            endif
            ! if(.not. ok) print *,'unsuccessful attempt to lock by thread',current_pid
        else
            ! print *,'attempt to unlock by thread',current_pid
            if (current_pid .ne. owner_pid .and. lockId > 0) then
                ! lock is owned by another thread
                write(app_msg,*) 'set_user_lock: ',current_pid, ' attempting to release a lock owned by', owner_pid
                call lib_log(APP_LIBRMN,APP_ERROR,app_msg)
                call qqexit(1)
            endif
            ! subtract one from depth
            owner_count = owner_count - 1
            lockId = current_pid + ishft(owner_count, 24)
            if (owner_count <= 0) then
                ! this thread's depth count for the lock is zero, release it totally
                lockId = 0
            endif
            ok = .true.
        endif
!$OMP END CRITICAL
    enddo
    ! if(lock) print *,'locked by thread',current_pid,' count=',owner_count
    ! if(.not. lock) print *,'unlocked by thread',current_pid,' count=',owner_count
#endif

end subroutine set_user_lock