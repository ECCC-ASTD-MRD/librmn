!> Checkpoint read or write for all known types
integer function gmm_checkpoint_all(read_or_write)
    use gmm_internals
    implicit none
    logical read_or_write
    integer code, istat, fnom
    external fnom

    ! Read checkpoint file one record at a time
    if (read_or_write) then
        if (restart_mode) then
            if (gmm_verbose_level <= GMM_MSG_WARN) then
                print *,'(GMM_CHECKPOINT_ALL) Warning: restart file already read'
            endif
            gmm_checkpoint_all = GMM_OK
            return
        endif
        if (file_unit .eq. 0) then
            ! Open checkpoint file
            istat = fnom(file_unit, 'gmm_restart', 'SEQ+UNF+FTN+OLD', 0)
            if (gmm_verbose_level == GMM_MSG_DEBUG) then
                print *, 'open restart file, status=', istat
            endif
            if (istat .lt. 0) then
                file_unit = 0
                gmm_checkpoint_all = GMM_ERROR
                return
            endif
        endif
        do while(.true.)
            read(file_unit, end = 999)code
            ! We are in restart mode if a single record is read from restart file
            restart_mode = .true.
            if (-1 .eq. code) then
                print *,'ERROR: gmm_checkpoint_all this cannot happen'
            else if (code .eq. 184) then
                call gmm_checkpoint_184(.true.)
            else if (code .eq. 144) then
                call gmm_checkpoint_144(.true.)
            else if (code .eq. 284) then
                call gmm_checkpoint_284(.true.)
            else if (code .eq. 244) then
                call gmm_checkpoint_244(.true.)
            else if (code .eq. 384) then
                call gmm_checkpoint_384(.true.)
            else if (code .eq. 183) then
                call gmm_checkpoint_183(.true.)
            else if (code .eq. 143) then
                call gmm_checkpoint_143(.true.)
            else if (code .eq. 283) then
                call gmm_checkpoint_283(.true.)
            else if (code .eq. 243) then
                call gmm_checkpoint_243(.true.)
            else if (code .eq. 383) then
                call gmm_checkpoint_383(.true.)
            else if (code .eq. 182) then
                call gmm_checkpoint_182(.true.)
            else if (code .eq. 142) then
                call gmm_checkpoint_142(.true.)
            else if (code .eq. 282) then
                call gmm_checkpoint_282(.true.)
            else if (code .eq. 242) then
                call gmm_checkpoint_242(.true.)
            else if (code .eq. 382) then
                call gmm_checkpoint_382(.true.)
            else if (code .eq. 181) then
                call gmm_checkpoint_181(.true.)
            else if (code .eq. 141) then
                call gmm_checkpoint_141(.true.)
            else if (code .eq. 281) then
                call gmm_checkpoint_281(.true.)
            else if (code .eq. 241) then
                call gmm_checkpoint_241(.true.)
            else if (code .eq. 381) then
                call gmm_checkpoint_381(.true.)
            else
                print *, 'ERROR: gmm_checkpoint_all unrecognized type=', code, ' in restart file'
                call qqexit(1)
            endif
        end do
    else
        ! Write all tables to checkpoint file
        if (file_unit .eq. 0) then
            ! Open checkpoint file
            istat = fnom(file_unit, 'gmm_restart', 'SEQ+UNF+FTN', 0)
            if (gmm_verbose_level == GMM_MSG_DEBUG) then
                print *, 'open restart file, status=', istat
            endif
            if (istat .lt. 0) then
                file_unit = 0
                gmm_checkpoint_all = GMM_ERROR
                return
            endif
        endif
        call gmm_checkpoint_184(.false.)
        call gmm_checkpoint_144(.false.)
        call gmm_checkpoint_284(.false.)
        call gmm_checkpoint_244(.false.)
        call gmm_checkpoint_384(.false.)
        call gmm_checkpoint_183(.false.)
        call gmm_checkpoint_143(.false.)
        call gmm_checkpoint_283(.false.)
        call gmm_checkpoint_243(.false.)
        call gmm_checkpoint_383(.false.)
        call gmm_checkpoint_182(.false.)
        call gmm_checkpoint_142(.false.)
        call gmm_checkpoint_282(.false.)
        call gmm_checkpoint_242(.false.)
        call gmm_checkpoint_382(.false.)
        call gmm_checkpoint_181(.false.)
        call gmm_checkpoint_141(.false.)
        call gmm_checkpoint_281(.false.)
        call gmm_checkpoint_241(.false.)
        call gmm_checkpoint_381(.false.)
    endif
999 call fclos(file_unit)
    file_unit = 0
    gmm_checkpoint_all = GMM_OK
end function gmm_checkpoint_all
