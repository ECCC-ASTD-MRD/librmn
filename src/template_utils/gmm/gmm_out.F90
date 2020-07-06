# 1 "gmm.F90"
# 1 "<built-in>"
# 1 "<command-line>"
# 1 "gmm.F90"
! Sadly, the concatenations of other macros can not be automated since in traditional mode,
! preprocessors introduce spaces when concatenating

! The GNU Fortran documentation, see URL below, states that the GNU C
! Preprocessor is invoqued in the traditional mode when compiling Fotran.
! This explains why macro expension does not work the same way as when called
! from gcc instead of gfortran.
! https://gcc.gnu.org/onlinedocs/gfortran/Preprocessing-and-conditional-compilation.html

! Names are built the following way:
!   DATACODE : A code for the data type (integer = 1, real = 2, complex = 3)
!   DATALENGTH : The size, in bytes, of each data element
!   DIM : Dimension of the array
!
! Together <DATACODE><DATALENGTH><DIM> form the EXTENSION added to function
! names after an underscore.
!
! FNCNAME is a macro that appends an underscore and the EXTENSION to its
! argument to return the actual function name.

! Integer functions:



! 141




# 1 "gmm_checkpoint_tmpl.F90" 1
subroutine gmm_checkpoint_141(read_or_write)
    use gmm_internals
    use pointer_table_data_141
    implicit none
    logical read_or_write
    integer istat, fnom, i, j, ier, lcl_pti
    type(gmm_layout), dimension(1:1) :: siz
    type(gmm_attributes) :: attrib
    integer *8 :: key
    external fnom
    integer *8 get_address_from
    external get_address_from

    ! Read one  record from checkpoint file
    if (read_or_write) then
        ! Into next directory entry
        call add_directory_entry
        read(file_unit)directory(cur_page)%entry(cur_entry)%name
        ! Read layout
        read(file_unit)siz(1:1)
        ! Set layout in table
        directory(cur_page)%entry(cur_entry)%l(1:1) = siz(1:1)
        ! Read attributes
        read(file_unit)attrib
        attrib%flags = ior(attrib%flags,GMM_FLAG_READ)
        ! Set attributes in table
        directory(cur_page)%entry(cur_entry)%a = attrib
        read(file_unit)directory(cur_page)%entry(cur_entry)%data_type
        lcl_pti = lgmm_get_nxt_avail_ptr()
        directory(cur_page)%entry(cur_entry)%pointer_table_index = lcl_pti
        ordinal = ordinal + 1
        key = ishft((cur_page-1),PAGE_NB_SHFT) + ishft((cur_entry-1),NTRY_NB_SHFT)
        key = key + ishft(141,EXTN_NB_SHFT) + ishft(ordinal,MAGC_NB_SHFT)
        ! Set creation ordinal
        directory(cur_page)%entry(cur_entry)%a%key = key
        ! CODE POSSIBLY MISSING HERE FOR FLAGS SETTINGS
        allocate(gmm_ptrs_141(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p(&
                                                            &siz(1)%low:siz(1)%high&
# 48 "gmm_checkpoint_tmpl.F90"
                                                        ))
        ! Read array
        read(file_unit)gmm_ptrs_141(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p
        directory(cur_page)%entry(cur_entry)%array_addr = get_address_from(&
            gmm_ptrs_141(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p)

        if (gmm_verbose_level == GMM_MSG_DEBUG) then
            write(6,'(a,a8,a,i4,a,i4,a,i4,a,i10)') 'name=',directory(cur_page)%entry(cur_entry)%name,' cur_page=',cur_page,' cur_entry=',cur_entry,' index=',directory(cur_page)%entry(cur_entry)%pointer_table_index,' addr=',get_address_from(gmm_ptrs_141(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p)
        endif
        ier = add_table_entry(gmm_ptrs_141(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p,key)
    else
        ! Write to checkpoint file (records with FLAG_RSTR property)
        if (gmm_verbose_level == GMM_MSG_DEBUG) then
            print *, 'checkpointing type ', 141
        endif
        do i = 1, table_size
            do j = 1, PAGE_SIZE
                if (iand(GMM_FLAG_RSTR,directory(i)%entry(j)%a%flags) .ne. 0 .and. directory(i)%entry(j)%data_type == 141) then
                    if (gmm_verbose_level == GMM_MSG_DEBUG) then
                        print *, 'writing field ', directory(i)%entry(j)%name
                    endif
                    write(file_unit)141
                    write(file_unit)directory(i)%entry(j)%name
                    write(file_unit)directory(i)%entry(j)%l(1:1)
                    attrib = directory(i)%entry(j)%a
                    attrib%flags = iand(attrib%flags,FLAGS_KEPT_IN_RESTART)
                    write(file_unit)attrib
                    write(file_unit)directory(i)%entry(j)%data_type
                    write(file_unit)gmm_ptrs_141(directory(i)%entry(j)%pointer_table_index)%p
                endif
            enddo
        enddo
    endif
end subroutine gmm_checkpoint_141
# 30 "gmm.F90" 2

# 1 "gmm_get_tmpl.F90" 1
integer function gmm_get_141(iname, p, m)
    use gmm_internals
    use pointer_table_data__141
    implicit none
    integer :: i, array_rank
    ! name (partially redundant with attributes)
    character(len=*), intent(in) :: iname

    integer*4, pointer  :: p(:)







    ! attributes (name in attributes is not used)
    type(gmm_metadata), optional, intent(out) :: m
    !  integer,intent(inout) :: reqid
    include 'gmm_directory_interface.inc'
    type(gmm_metadata) :: m2
    integer*8 :: key
    integer *8 get_address_from
    external get_address_from

    key = 0
    call check_directory_entry(iname, key)
    if (cur_page .eq. 0 .or. cur_entry .eq. 0) then
        ! quick check using key was not successful
        call find_directory_entry(iname,key)
    endif
    if(cur_page .eq. 0 .or. cur_entry .eq. 0) then
        ! return null entry
        if (present(m)) then
            m%a = GMM_NULL_ATTRIB
            m%l = GMM_NULL_LAYOUT
        endif
        nullify(p)
        key = GMM_KEY_NOT_FOUND
        gmm_get_141 = GMM_VAR_NOT_FOUND
    else
        m2%l = directory(cur_page)%entry(cur_entry)%l
        m2%a = directory(cur_page)%entry(cur_entry)%a
        if (present(m)) then
            ! return a copy of the proper entry
            m = m2
        endif
        p => gmm_ptrs_141(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p
        do i = 1, 4
            if (m2%l(i)%n /= 0) then
                array_rank = i
            endif
        enddo
        if (array_rank /= 1) then
            nullify(p)
            if (present(m)) then
                m = GMM_NULL_METADATA
            endif
            gmm_get_141 = GMM_INCONSISTENT_DIMS
        else
            gmm_get_141 = GMM_OK
        endif
    endif
end function gmm_get_141


subroutine gmm_dealloc_ptr_141()
    use gmm_internals
    use pointer_table_data__141
    implicit none

    deallocate (gmm_ptrs_141(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p)
    nullify    (gmm_ptrs_141(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p)
end subroutine gmm_dealloc_ptr_141
# 31 "gmm.F90" 2

# 1 "undefiner.hf" 1
# 32 "gmm.F90" 2

! 142




# 1 "gmm_checkpoint_tmpl.F90" 1
subroutine gmm_checkpoint_142(read_or_write)
    use gmm_internals
    use pointer_table_data_142
    implicit none
    logical read_or_write
    integer istat, fnom, i, j, ier, lcl_pti
    type(gmm_layout), dimension(1:2) :: siz
    type(gmm_attributes) :: attrib
    integer *8 :: key
    external fnom
    integer *8 get_address_from
    external get_address_from

    ! Read one  record from checkpoint file
    if (read_or_write) then
        ! Into next directory entry
        call add_directory_entry
        read(file_unit)directory(cur_page)%entry(cur_entry)%name
        ! Read layout
        read(file_unit)siz(1:2)
        ! Set layout in table
        directory(cur_page)%entry(cur_entry)%l(1:2) = siz(1:2)
        ! Read attributes
        read(file_unit)attrib
        attrib%flags = ior(attrib%flags,GMM_FLAG_READ)
        ! Set attributes in table
        directory(cur_page)%entry(cur_entry)%a = attrib
        read(file_unit)directory(cur_page)%entry(cur_entry)%data_type
        lcl_pti = lgmm_get_nxt_avail_ptr()
        directory(cur_page)%entry(cur_entry)%pointer_table_index = lcl_pti
        ordinal = ordinal + 1
        key = ishft((cur_page-1),PAGE_NB_SHFT) + ishft((cur_entry-1),NTRY_NB_SHFT)
        key = key + ishft(142,EXTN_NB_SHFT) + ishft(ordinal,MAGC_NB_SHFT)
        ! Set creation ordinal
        directory(cur_page)%entry(cur_entry)%a%key = key
        ! CODE POSSIBLY MISSING HERE FOR FLAGS SETTINGS
        allocate(gmm_ptrs_142(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p(&
                                                            &siz(1)%low:siz(1)%high&

                                                            &,siz(2)%low:siz(2)%high,&







                                                        ))
        ! Read array
        read(file_unit)gmm_ptrs_142(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p
        directory(cur_page)%entry(cur_entry)%array_addr = get_address_from(&
            gmm_ptrs_142(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p)

        if (gmm_verbose_level == GMM_MSG_DEBUG) then
            write(6,'(a,a8,a,i4,a,i4,a,i4,a,i10)') 'name=',directory(cur_page)%entry(cur_entry)%name,' cur_page=',cur_page,' cur_entry=',cur_entry,' index=',directory(cur_page)%entry(cur_entry)%pointer_table_index,' addr=',get_address_from(gmm_ptrs_142(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p)
        endif
        ier = add_table_entry(gmm_ptrs_142(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p,key)
    else
        ! Write to checkpoint file (records with FLAG_RSTR property)
        if (gmm_verbose_level == GMM_MSG_DEBUG) then
            print *, 'checkpointing type ', 142
        endif
        do i = 1, table_size
            do j = 1, PAGE_SIZE
                if (iand(GMM_FLAG_RSTR,directory(i)%entry(j)%a%flags) .ne. 0 .and. directory(i)%entry(j)%data_type == 142) then
                    if (gmm_verbose_level == GMM_MSG_DEBUG) then
                        print *, 'writing field ', directory(i)%entry(j)%name
                    endif
                    write(file_unit)142
                    write(file_unit)directory(i)%entry(j)%name
                    write(file_unit)directory(i)%entry(j)%l(1:2)
                    attrib = directory(i)%entry(j)%a
                    attrib%flags = iand(attrib%flags,FLAGS_KEPT_IN_RESTART)
                    write(file_unit)attrib
                    write(file_unit)directory(i)%entry(j)%data_type
                    write(file_unit)gmm_ptrs_142(directory(i)%entry(j)%pointer_table_index)%p
                endif
            enddo
        enddo
    endif
end subroutine gmm_checkpoint_142
# 38 "gmm.F90" 2

# 1 "gmm_get_tmpl.F90" 1
integer function gmm_get_142(iname, p, m)
    use gmm_internals
    use pointer_table_data__142
    implicit none
    integer :: i, array_rank
    ! name (partially redundant with attributes)
    character(len=*), intent(in) :: iname



    integer*4, pointer  :: p(:,:)





    ! attributes (name in attributes is not used)
    type(gmm_metadata), optional, intent(out) :: m
    !  integer,intent(inout) :: reqid
    include 'gmm_directory_interface.inc'
    type(gmm_metadata) :: m2
    integer*8 :: key
    integer *8 get_address_from
    external get_address_from

    key = 0
    call check_directory_entry(iname, key)
    if (cur_page .eq. 0 .or. cur_entry .eq. 0) then
        ! quick check using key was not successful
        call find_directory_entry(iname,key)
    endif
    if(cur_page .eq. 0 .or. cur_entry .eq. 0) then
        ! return null entry
        if (present(m)) then
            m%a = GMM_NULL_ATTRIB
            m%l = GMM_NULL_LAYOUT
        endif
        nullify(p)
        key = GMM_KEY_NOT_FOUND
        gmm_get_142 = GMM_VAR_NOT_FOUND
    else
        m2%l = directory(cur_page)%entry(cur_entry)%l
        m2%a = directory(cur_page)%entry(cur_entry)%a
        if (present(m)) then
            ! return a copy of the proper entry
            m = m2
        endif
        p => gmm_ptrs_142(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p
        do i = 1, 4
            if (m2%l(i)%n /= 0) then
                array_rank = i
            endif
        enddo
        if (array_rank /= 2) then
            nullify(p)
            if (present(m)) then
                m = GMM_NULL_METADATA
            endif
            gmm_get_142 = GMM_INCONSISTENT_DIMS
        else
            gmm_get_142 = GMM_OK
        endif
    endif
end function gmm_get_142


subroutine gmm_dealloc_ptr_142()
    use gmm_internals
    use pointer_table_data__142
    implicit none

    deallocate (gmm_ptrs_142(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p)
    nullify    (gmm_ptrs_142(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p)
end subroutine gmm_dealloc_ptr_142
# 39 "gmm.F90" 2

# 1 "undefiner.hf" 1
# 40 "gmm.F90" 2

! 143




# 1 "gmm_checkpoint_tmpl.F90" 1
subroutine gmm_checkpoint_143(read_or_write)
    use gmm_internals
    use pointer_table_data_143
    implicit none
    logical read_or_write
    integer istat, fnom, i, j, ier, lcl_pti
    type(gmm_layout), dimension(1:3) :: siz
    type(gmm_attributes) :: attrib
    integer *8 :: key
    external fnom
    integer *8 get_address_from
    external get_address_from

    ! Read one  record from checkpoint file
    if (read_or_write) then
        ! Into next directory entry
        call add_directory_entry
        read(file_unit)directory(cur_page)%entry(cur_entry)%name
        ! Read layout
        read(file_unit)siz(1:3)
        ! Set layout in table
        directory(cur_page)%entry(cur_entry)%l(1:3) = siz(1:3)
        ! Read attributes
        read(file_unit)attrib
        attrib%flags = ior(attrib%flags,GMM_FLAG_READ)
        ! Set attributes in table
        directory(cur_page)%entry(cur_entry)%a = attrib
        read(file_unit)directory(cur_page)%entry(cur_entry)%data_type
        lcl_pti = lgmm_get_nxt_avail_ptr()
        directory(cur_page)%entry(cur_entry)%pointer_table_index = lcl_pti
        ordinal = ordinal + 1
        key = ishft((cur_page-1),PAGE_NB_SHFT) + ishft((cur_entry-1),NTRY_NB_SHFT)
        key = key + ishft(143,EXTN_NB_SHFT) + ishft(ordinal,MAGC_NB_SHFT)
        ! Set creation ordinal
        directory(cur_page)%entry(cur_entry)%a%key = key
        ! CODE POSSIBLY MISSING HERE FOR FLAGS SETTINGS
        allocate(gmm_ptrs_143(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p(&
                                                            &siz(1)%low:siz(1)%high&

                                                            &,siz(2)%low:siz(2)%high,&


                                                            &,siz(3)%low:siz(3)%high,&




                                                        ))
        ! Read array
        read(file_unit)gmm_ptrs_143(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p
        directory(cur_page)%entry(cur_entry)%array_addr = get_address_from(&
            gmm_ptrs_143(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p)

        if (gmm_verbose_level == GMM_MSG_DEBUG) then
            write(6,'(a,a8,a,i4,a,i4,a,i4,a,i10)') 'name=',directory(cur_page)%entry(cur_entry)%name,' cur_page=',cur_page,' cur_entry=',cur_entry,' index=',directory(cur_page)%entry(cur_entry)%pointer_table_index,' addr=',get_address_from(gmm_ptrs_143(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p)
        endif
        ier = add_table_entry(gmm_ptrs_143(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p,key)
    else
        ! Write to checkpoint file (records with FLAG_RSTR property)
        if (gmm_verbose_level == GMM_MSG_DEBUG) then
            print *, 'checkpointing type ', 143
        endif
        do i = 1, table_size
            do j = 1, PAGE_SIZE
                if (iand(GMM_FLAG_RSTR,directory(i)%entry(j)%a%flags) .ne. 0 .and. directory(i)%entry(j)%data_type == 143) then
                    if (gmm_verbose_level == GMM_MSG_DEBUG) then
                        print *, 'writing field ', directory(i)%entry(j)%name
                    endif
                    write(file_unit)143
                    write(file_unit)directory(i)%entry(j)%name
                    write(file_unit)directory(i)%entry(j)%l(1:3)
                    attrib = directory(i)%entry(j)%a
                    attrib%flags = iand(attrib%flags,FLAGS_KEPT_IN_RESTART)
                    write(file_unit)attrib
                    write(file_unit)directory(i)%entry(j)%data_type
                    write(file_unit)gmm_ptrs_143(directory(i)%entry(j)%pointer_table_index)%p
                endif
            enddo
        enddo
    endif
end subroutine gmm_checkpoint_143
# 46 "gmm.F90" 2

# 1 "gmm_get_tmpl.F90" 1
integer function gmm_get_143(iname, p, m)
    use gmm_internals
    use pointer_table_data__143
    implicit none
    integer :: i, array_rank
    ! name (partially redundant with attributes)
    character(len=*), intent(in) :: iname





    integer*4, pointer  :: p(:,:,:)



    ! attributes (name in attributes is not used)
    type(gmm_metadata), optional, intent(out) :: m
    !  integer,intent(inout) :: reqid
    include 'gmm_directory_interface.inc'
    type(gmm_metadata) :: m2
    integer*8 :: key
    integer *8 get_address_from
    external get_address_from

    key = 0
    call check_directory_entry(iname, key)
    if (cur_page .eq. 0 .or. cur_entry .eq. 0) then
        ! quick check using key was not successful
        call find_directory_entry(iname,key)
    endif
    if(cur_page .eq. 0 .or. cur_entry .eq. 0) then
        ! return null entry
        if (present(m)) then
            m%a = GMM_NULL_ATTRIB
            m%l = GMM_NULL_LAYOUT
        endif
        nullify(p)
        key = GMM_KEY_NOT_FOUND
        gmm_get_143 = GMM_VAR_NOT_FOUND
    else
        m2%l = directory(cur_page)%entry(cur_entry)%l
        m2%a = directory(cur_page)%entry(cur_entry)%a
        if (present(m)) then
            ! return a copy of the proper entry
            m = m2
        endif
        p => gmm_ptrs_143(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p
        do i = 1, 4
            if (m2%l(i)%n /= 0) then
                array_rank = i
            endif
        enddo
        if (array_rank /= 3) then
            nullify(p)
            if (present(m)) then
                m = GMM_NULL_METADATA
            endif
            gmm_get_143 = GMM_INCONSISTENT_DIMS
        else
            gmm_get_143 = GMM_OK
        endif
    endif
end function gmm_get_143


subroutine gmm_dealloc_ptr_143()
    use gmm_internals
    use pointer_table_data__143
    implicit none

    deallocate (gmm_ptrs_143(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p)
    nullify    (gmm_ptrs_143(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p)
end subroutine gmm_dealloc_ptr_143
# 47 "gmm.F90" 2

# 1 "undefiner.hf" 1
# 48 "gmm.F90" 2

! 144




# 1 "gmm_checkpoint_tmpl.F90" 1
subroutine gmm_checkpoint_144(read_or_write)
    use gmm_internals
    use pointer_table_data_144
    implicit none
    logical read_or_write
    integer istat, fnom, i, j, ier, lcl_pti
    type(gmm_layout), dimension(1:4) :: siz
    type(gmm_attributes) :: attrib
    integer *8 :: key
    external fnom
    integer *8 get_address_from
    external get_address_from

    ! Read one  record from checkpoint file
    if (read_or_write) then
        ! Into next directory entry
        call add_directory_entry
        read(file_unit)directory(cur_page)%entry(cur_entry)%name
        ! Read layout
        read(file_unit)siz(1:4)
        ! Set layout in table
        directory(cur_page)%entry(cur_entry)%l(1:4) = siz(1:4)
        ! Read attributes
        read(file_unit)attrib
        attrib%flags = ior(attrib%flags,GMM_FLAG_READ)
        ! Set attributes in table
        directory(cur_page)%entry(cur_entry)%a = attrib
        read(file_unit)directory(cur_page)%entry(cur_entry)%data_type
        lcl_pti = lgmm_get_nxt_avail_ptr()
        directory(cur_page)%entry(cur_entry)%pointer_table_index = lcl_pti
        ordinal = ordinal + 1
        key = ishft((cur_page-1),PAGE_NB_SHFT) + ishft((cur_entry-1),NTRY_NB_SHFT)
        key = key + ishft(144,EXTN_NB_SHFT) + ishft(ordinal,MAGC_NB_SHFT)
        ! Set creation ordinal
        directory(cur_page)%entry(cur_entry)%a%key = key
        ! CODE POSSIBLY MISSING HERE FOR FLAGS SETTINGS
        allocate(gmm_ptrs_144(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p(&
                                                            &siz(1)%low:siz(1)%high&

                                                            &,siz(2)%low:siz(2)%high,&


                                                            &,siz(3)%low:siz(3)%high,&


                                                            &,siz(4)%low:siz(4)%high&

                                                        ))
        ! Read array
        read(file_unit)gmm_ptrs_144(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p
        directory(cur_page)%entry(cur_entry)%array_addr = get_address_from(&
            gmm_ptrs_144(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p)

        if (gmm_verbose_level == GMM_MSG_DEBUG) then
            write(6,'(a,a8,a,i4,a,i4,a,i4,a,i10)') 'name=',directory(cur_page)%entry(cur_entry)%name,' cur_page=',cur_page,' cur_entry=',cur_entry,' index=',directory(cur_page)%entry(cur_entry)%pointer_table_index,' addr=',get_address_from(gmm_ptrs_144(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p)
        endif
        ier = add_table_entry(gmm_ptrs_144(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p,key)
    else
        ! Write to checkpoint file (records with FLAG_RSTR property)
        if (gmm_verbose_level == GMM_MSG_DEBUG) then
            print *, 'checkpointing type ', 144
        endif
        do i = 1, table_size
            do j = 1, PAGE_SIZE
                if (iand(GMM_FLAG_RSTR,directory(i)%entry(j)%a%flags) .ne. 0 .and. directory(i)%entry(j)%data_type == 144) then
                    if (gmm_verbose_level == GMM_MSG_DEBUG) then
                        print *, 'writing field ', directory(i)%entry(j)%name
                    endif
                    write(file_unit)144
                    write(file_unit)directory(i)%entry(j)%name
                    write(file_unit)directory(i)%entry(j)%l(1:4)
                    attrib = directory(i)%entry(j)%a
                    attrib%flags = iand(attrib%flags,FLAGS_KEPT_IN_RESTART)
                    write(file_unit)attrib
                    write(file_unit)directory(i)%entry(j)%data_type
                    write(file_unit)gmm_ptrs_144(directory(i)%entry(j)%pointer_table_index)%p
                endif
            enddo
        enddo
    endif
end subroutine gmm_checkpoint_144
# 54 "gmm.F90" 2

# 1 "gmm_get_tmpl.F90" 1
integer function gmm_get_144(iname, p, m)
    use gmm_internals
    use pointer_table_data__144
    implicit none
    integer :: i, array_rank
    ! name (partially redundant with attributes)
    character(len=*), intent(in) :: iname







    integer*4, pointer  :: p(:,:,:,:)

    ! attributes (name in attributes is not used)
    type(gmm_metadata), optional, intent(out) :: m
    !  integer,intent(inout) :: reqid
    include 'gmm_directory_interface.inc'
    type(gmm_metadata) :: m2
    integer*8 :: key
    integer *8 get_address_from
    external get_address_from

    key = 0
    call check_directory_entry(iname, key)
    if (cur_page .eq. 0 .or. cur_entry .eq. 0) then
        ! quick check using key was not successful
        call find_directory_entry(iname,key)
    endif
    if(cur_page .eq. 0 .or. cur_entry .eq. 0) then
        ! return null entry
        if (present(m)) then
            m%a = GMM_NULL_ATTRIB
            m%l = GMM_NULL_LAYOUT
        endif
        nullify(p)
        key = GMM_KEY_NOT_FOUND
        gmm_get_144 = GMM_VAR_NOT_FOUND
    else
        m2%l = directory(cur_page)%entry(cur_entry)%l
        m2%a = directory(cur_page)%entry(cur_entry)%a
        if (present(m)) then
            ! return a copy of the proper entry
            m = m2
        endif
        p => gmm_ptrs_144(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p
        do i = 1, 4
            if (m2%l(i)%n /= 0) then
                array_rank = i
            endif
        enddo
        if (array_rank /= 4) then
            nullify(p)
            if (present(m)) then
                m = GMM_NULL_METADATA
            endif
            gmm_get_144 = GMM_INCONSISTENT_DIMS
        else
            gmm_get_144 = GMM_OK
        endif
    endif
end function gmm_get_144


subroutine gmm_dealloc_ptr_144()
    use gmm_internals
    use pointer_table_data__144
    implicit none

    deallocate (gmm_ptrs_144(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p)
    nullify    (gmm_ptrs_144(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p)
end subroutine gmm_dealloc_ptr_144
# 55 "gmm.F90" 2

# 1 "undefiner.hf" 1
# 56 "gmm.F90" 2




! 181




# 1 "gmm_checkpoint_tmpl.F90" 1
subroutine gmm_checkpoint_181(read_or_write)
    use gmm_internals
    use pointer_table_data_181
    implicit none
    logical read_or_write
    integer istat, fnom, i, j, ier, lcl_pti
    type(gmm_layout), dimension(1:1) :: siz
    type(gmm_attributes) :: attrib
    integer *8 :: key
    external fnom
    integer *8 get_address_from
    external get_address_from

    ! Read one  record from checkpoint file
    if (read_or_write) then
        ! Into next directory entry
        call add_directory_entry
        read(file_unit)directory(cur_page)%entry(cur_entry)%name
        ! Read layout
        read(file_unit)siz(1:1)
        ! Set layout in table
        directory(cur_page)%entry(cur_entry)%l(1:1) = siz(1:1)
        ! Read attributes
        read(file_unit)attrib
        attrib%flags = ior(attrib%flags,GMM_FLAG_READ)
        ! Set attributes in table
        directory(cur_page)%entry(cur_entry)%a = attrib
        read(file_unit)directory(cur_page)%entry(cur_entry)%data_type
        lcl_pti = lgmm_get_nxt_avail_ptr()
        directory(cur_page)%entry(cur_entry)%pointer_table_index = lcl_pti
        ordinal = ordinal + 1
        key = ishft((cur_page-1),PAGE_NB_SHFT) + ishft((cur_entry-1),NTRY_NB_SHFT)
        key = key + ishft(181,EXTN_NB_SHFT) + ishft(ordinal,MAGC_NB_SHFT)
        ! Set creation ordinal
        directory(cur_page)%entry(cur_entry)%a%key = key
        ! CODE POSSIBLY MISSING HERE FOR FLAGS SETTINGS
        allocate(gmm_ptrs_181(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p(&
                                                            &siz(1)%low:siz(1)%high&
# 48 "gmm_checkpoint_tmpl.F90"
                                                        ))
        ! Read array
        read(file_unit)gmm_ptrs_181(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p
        directory(cur_page)%entry(cur_entry)%array_addr = get_address_from(&
            gmm_ptrs_181(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p)

        if (gmm_verbose_level == GMM_MSG_DEBUG) then
            write(6,'(a,a8,a,i4,a,i4,a,i4,a,i10)') 'name=',directory(cur_page)%entry(cur_entry)%name,' cur_page=',cur_page,' cur_entry=',cur_entry,' index=',directory(cur_page)%entry(cur_entry)%pointer_table_index,' addr=',get_address_from(gmm_ptrs_181(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p)
        endif
        ier = add_table_entry(gmm_ptrs_181(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p,key)
    else
        ! Write to checkpoint file (records with FLAG_RSTR property)
        if (gmm_verbose_level == GMM_MSG_DEBUG) then
            print *, 'checkpointing type ', 181
        endif
        do i = 1, table_size
            do j = 1, PAGE_SIZE
                if (iand(GMM_FLAG_RSTR,directory(i)%entry(j)%a%flags) .ne. 0 .and. directory(i)%entry(j)%data_type == 181) then
                    if (gmm_verbose_level == GMM_MSG_DEBUG) then
                        print *, 'writing field ', directory(i)%entry(j)%name
                    endif
                    write(file_unit)181
                    write(file_unit)directory(i)%entry(j)%name
                    write(file_unit)directory(i)%entry(j)%l(1:1)
                    attrib = directory(i)%entry(j)%a
                    attrib%flags = iand(attrib%flags,FLAGS_KEPT_IN_RESTART)
                    write(file_unit)attrib
                    write(file_unit)directory(i)%entry(j)%data_type
                    write(file_unit)gmm_ptrs_181(directory(i)%entry(j)%pointer_table_index)%p
                endif
            enddo
        enddo
    endif
end subroutine gmm_checkpoint_181
# 65 "gmm.F90" 2

# 1 "gmm_get_tmpl.F90" 1
integer function gmm_get_181(iname, p, m)
    use gmm_internals
    use pointer_table_data__181
    implicit none
    integer :: i, array_rank
    ! name (partially redundant with attributes)
    character(len=*), intent(in) :: iname

    integer*8, pointer  :: p(:)







    ! attributes (name in attributes is not used)
    type(gmm_metadata), optional, intent(out) :: m
    !  integer,intent(inout) :: reqid
    include 'gmm_directory_interface.inc'
    type(gmm_metadata) :: m2
    integer*8 :: key
    integer *8 get_address_from
    external get_address_from

    key = 0
    call check_directory_entry(iname, key)
    if (cur_page .eq. 0 .or. cur_entry .eq. 0) then
        ! quick check using key was not successful
        call find_directory_entry(iname,key)
    endif
    if(cur_page .eq. 0 .or. cur_entry .eq. 0) then
        ! return null entry
        if (present(m)) then
            m%a = GMM_NULL_ATTRIB
            m%l = GMM_NULL_LAYOUT
        endif
        nullify(p)
        key = GMM_KEY_NOT_FOUND
        gmm_get_181 = GMM_VAR_NOT_FOUND
    else
        m2%l = directory(cur_page)%entry(cur_entry)%l
        m2%a = directory(cur_page)%entry(cur_entry)%a
        if (present(m)) then
            ! return a copy of the proper entry
            m = m2
        endif
        p => gmm_ptrs_181(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p
        do i = 1, 4
            if (m2%l(i)%n /= 0) then
                array_rank = i
            endif
        enddo
        if (array_rank /= 1) then
            nullify(p)
            if (present(m)) then
                m = GMM_NULL_METADATA
            endif
            gmm_get_181 = GMM_INCONSISTENT_DIMS
        else
            gmm_get_181 = GMM_OK
        endif
    endif
end function gmm_get_181


subroutine gmm_dealloc_ptr_181()
    use gmm_internals
    use pointer_table_data__181
    implicit none

    deallocate (gmm_ptrs_181(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p)
    nullify    (gmm_ptrs_181(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p)
end subroutine gmm_dealloc_ptr_181
# 66 "gmm.F90" 2

# 1 "undefiner.hf" 1
# 67 "gmm.F90" 2

! 182




# 1 "gmm_checkpoint_tmpl.F90" 1
subroutine gmm_checkpoint_182(read_or_write)
    use gmm_internals
    use pointer_table_data_182
    implicit none
    logical read_or_write
    integer istat, fnom, i, j, ier, lcl_pti
    type(gmm_layout), dimension(1:2) :: siz
    type(gmm_attributes) :: attrib
    integer *8 :: key
    external fnom
    integer *8 get_address_from
    external get_address_from

    ! Read one  record from checkpoint file
    if (read_or_write) then
        ! Into next directory entry
        call add_directory_entry
        read(file_unit)directory(cur_page)%entry(cur_entry)%name
        ! Read layout
        read(file_unit)siz(1:2)
        ! Set layout in table
        directory(cur_page)%entry(cur_entry)%l(1:2) = siz(1:2)
        ! Read attributes
        read(file_unit)attrib
        attrib%flags = ior(attrib%flags,GMM_FLAG_READ)
        ! Set attributes in table
        directory(cur_page)%entry(cur_entry)%a = attrib
        read(file_unit)directory(cur_page)%entry(cur_entry)%data_type
        lcl_pti = lgmm_get_nxt_avail_ptr()
        directory(cur_page)%entry(cur_entry)%pointer_table_index = lcl_pti
        ordinal = ordinal + 1
        key = ishft((cur_page-1),PAGE_NB_SHFT) + ishft((cur_entry-1),NTRY_NB_SHFT)
        key = key + ishft(182,EXTN_NB_SHFT) + ishft(ordinal,MAGC_NB_SHFT)
        ! Set creation ordinal
        directory(cur_page)%entry(cur_entry)%a%key = key
        ! CODE POSSIBLY MISSING HERE FOR FLAGS SETTINGS
        allocate(gmm_ptrs_182(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p(&
                                                            &siz(1)%low:siz(1)%high&

                                                            &,siz(2)%low:siz(2)%high,&







                                                        ))
        ! Read array
        read(file_unit)gmm_ptrs_182(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p
        directory(cur_page)%entry(cur_entry)%array_addr = get_address_from(&
            gmm_ptrs_182(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p)

        if (gmm_verbose_level == GMM_MSG_DEBUG) then
            write(6,'(a,a8,a,i4,a,i4,a,i4,a,i10)') 'name=',directory(cur_page)%entry(cur_entry)%name,' cur_page=',cur_page,' cur_entry=',cur_entry,' index=',directory(cur_page)%entry(cur_entry)%pointer_table_index,' addr=',get_address_from(gmm_ptrs_182(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p)
        endif
        ier = add_table_entry(gmm_ptrs_182(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p,key)
    else
        ! Write to checkpoint file (records with FLAG_RSTR property)
        if (gmm_verbose_level == GMM_MSG_DEBUG) then
            print *, 'checkpointing type ', 182
        endif
        do i = 1, table_size
            do j = 1, PAGE_SIZE
                if (iand(GMM_FLAG_RSTR,directory(i)%entry(j)%a%flags) .ne. 0 .and. directory(i)%entry(j)%data_type == 182) then
                    if (gmm_verbose_level == GMM_MSG_DEBUG) then
                        print *, 'writing field ', directory(i)%entry(j)%name
                    endif
                    write(file_unit)182
                    write(file_unit)directory(i)%entry(j)%name
                    write(file_unit)directory(i)%entry(j)%l(1:2)
                    attrib = directory(i)%entry(j)%a
                    attrib%flags = iand(attrib%flags,FLAGS_KEPT_IN_RESTART)
                    write(file_unit)attrib
                    write(file_unit)directory(i)%entry(j)%data_type
                    write(file_unit)gmm_ptrs_182(directory(i)%entry(j)%pointer_table_index)%p
                endif
            enddo
        enddo
    endif
end subroutine gmm_checkpoint_182
# 73 "gmm.F90" 2

# 1 "gmm_get_tmpl.F90" 1
integer function gmm_get_182(iname, p, m)
    use gmm_internals
    use pointer_table_data__182
    implicit none
    integer :: i, array_rank
    ! name (partially redundant with attributes)
    character(len=*), intent(in) :: iname



    integer*8, pointer  :: p(:,:)





    ! attributes (name in attributes is not used)
    type(gmm_metadata), optional, intent(out) :: m
    !  integer,intent(inout) :: reqid
    include 'gmm_directory_interface.inc'
    type(gmm_metadata) :: m2
    integer*8 :: key
    integer *8 get_address_from
    external get_address_from

    key = 0
    call check_directory_entry(iname, key)
    if (cur_page .eq. 0 .or. cur_entry .eq. 0) then
        ! quick check using key was not successful
        call find_directory_entry(iname,key)
    endif
    if(cur_page .eq. 0 .or. cur_entry .eq. 0) then
        ! return null entry
        if (present(m)) then
            m%a = GMM_NULL_ATTRIB
            m%l = GMM_NULL_LAYOUT
        endif
        nullify(p)
        key = GMM_KEY_NOT_FOUND
        gmm_get_182 = GMM_VAR_NOT_FOUND
    else
        m2%l = directory(cur_page)%entry(cur_entry)%l
        m2%a = directory(cur_page)%entry(cur_entry)%a
        if (present(m)) then
            ! return a copy of the proper entry
            m = m2
        endif
        p => gmm_ptrs_182(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p
        do i = 1, 4
            if (m2%l(i)%n /= 0) then
                array_rank = i
            endif
        enddo
        if (array_rank /= 2) then
            nullify(p)
            if (present(m)) then
                m = GMM_NULL_METADATA
            endif
            gmm_get_182 = GMM_INCONSISTENT_DIMS
        else
            gmm_get_182 = GMM_OK
        endif
    endif
end function gmm_get_182


subroutine gmm_dealloc_ptr_182()
    use gmm_internals
    use pointer_table_data__182
    implicit none

    deallocate (gmm_ptrs_182(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p)
    nullify    (gmm_ptrs_182(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p)
end subroutine gmm_dealloc_ptr_182
# 74 "gmm.F90" 2

# 1 "undefiner.hf" 1
# 75 "gmm.F90" 2

! 183




# 1 "gmm_checkpoint_tmpl.F90" 1
subroutine gmm_checkpoint_183(read_or_write)
    use gmm_internals
    use pointer_table_data_183
    implicit none
    logical read_or_write
    integer istat, fnom, i, j, ier, lcl_pti
    type(gmm_layout), dimension(1:3) :: siz
    type(gmm_attributes) :: attrib
    integer *8 :: key
    external fnom
    integer *8 get_address_from
    external get_address_from

    ! Read one  record from checkpoint file
    if (read_or_write) then
        ! Into next directory entry
        call add_directory_entry
        read(file_unit)directory(cur_page)%entry(cur_entry)%name
        ! Read layout
        read(file_unit)siz(1:3)
        ! Set layout in table
        directory(cur_page)%entry(cur_entry)%l(1:3) = siz(1:3)
        ! Read attributes
        read(file_unit)attrib
        attrib%flags = ior(attrib%flags,GMM_FLAG_READ)
        ! Set attributes in table
        directory(cur_page)%entry(cur_entry)%a = attrib
        read(file_unit)directory(cur_page)%entry(cur_entry)%data_type
        lcl_pti = lgmm_get_nxt_avail_ptr()
        directory(cur_page)%entry(cur_entry)%pointer_table_index = lcl_pti
        ordinal = ordinal + 1
        key = ishft((cur_page-1),PAGE_NB_SHFT) + ishft((cur_entry-1),NTRY_NB_SHFT)
        key = key + ishft(183,EXTN_NB_SHFT) + ishft(ordinal,MAGC_NB_SHFT)
        ! Set creation ordinal
        directory(cur_page)%entry(cur_entry)%a%key = key
        ! CODE POSSIBLY MISSING HERE FOR FLAGS SETTINGS
        allocate(gmm_ptrs_183(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p(&
                                                            &siz(1)%low:siz(1)%high&

                                                            &,siz(2)%low:siz(2)%high,&


                                                            &,siz(3)%low:siz(3)%high,&




                                                        ))
        ! Read array
        read(file_unit)gmm_ptrs_183(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p
        directory(cur_page)%entry(cur_entry)%array_addr = get_address_from(&
            gmm_ptrs_183(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p)

        if (gmm_verbose_level == GMM_MSG_DEBUG) then
            write(6,'(a,a8,a,i4,a,i4,a,i4,a,i10)') 'name=',directory(cur_page)%entry(cur_entry)%name,' cur_page=',cur_page,' cur_entry=',cur_entry,' index=',directory(cur_page)%entry(cur_entry)%pointer_table_index,' addr=',get_address_from(gmm_ptrs_183(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p)
        endif
        ier = add_table_entry(gmm_ptrs_183(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p,key)
    else
        ! Write to checkpoint file (records with FLAG_RSTR property)
        if (gmm_verbose_level == GMM_MSG_DEBUG) then
            print *, 'checkpointing type ', 183
        endif
        do i = 1, table_size
            do j = 1, PAGE_SIZE
                if (iand(GMM_FLAG_RSTR,directory(i)%entry(j)%a%flags) .ne. 0 .and. directory(i)%entry(j)%data_type == 183) then
                    if (gmm_verbose_level == GMM_MSG_DEBUG) then
                        print *, 'writing field ', directory(i)%entry(j)%name
                    endif
                    write(file_unit)183
                    write(file_unit)directory(i)%entry(j)%name
                    write(file_unit)directory(i)%entry(j)%l(1:3)
                    attrib = directory(i)%entry(j)%a
                    attrib%flags = iand(attrib%flags,FLAGS_KEPT_IN_RESTART)
                    write(file_unit)attrib
                    write(file_unit)directory(i)%entry(j)%data_type
                    write(file_unit)gmm_ptrs_183(directory(i)%entry(j)%pointer_table_index)%p
                endif
            enddo
        enddo
    endif
end subroutine gmm_checkpoint_183
# 81 "gmm.F90" 2

# 1 "gmm_get_tmpl.F90" 1
integer function gmm_get_183(iname, p, m)
    use gmm_internals
    use pointer_table_data__183
    implicit none
    integer :: i, array_rank
    ! name (partially redundant with attributes)
    character(len=*), intent(in) :: iname





    integer*8, pointer  :: p(:,:,:)



    ! attributes (name in attributes is not used)
    type(gmm_metadata), optional, intent(out) :: m
    !  integer,intent(inout) :: reqid
    include 'gmm_directory_interface.inc'
    type(gmm_metadata) :: m2
    integer*8 :: key
    integer *8 get_address_from
    external get_address_from

    key = 0
    call check_directory_entry(iname, key)
    if (cur_page .eq. 0 .or. cur_entry .eq. 0) then
        ! quick check using key was not successful
        call find_directory_entry(iname,key)
    endif
    if(cur_page .eq. 0 .or. cur_entry .eq. 0) then
        ! return null entry
        if (present(m)) then
            m%a = GMM_NULL_ATTRIB
            m%l = GMM_NULL_LAYOUT
        endif
        nullify(p)
        key = GMM_KEY_NOT_FOUND
        gmm_get_183 = GMM_VAR_NOT_FOUND
    else
        m2%l = directory(cur_page)%entry(cur_entry)%l
        m2%a = directory(cur_page)%entry(cur_entry)%a
        if (present(m)) then
            ! return a copy of the proper entry
            m = m2
        endif
        p => gmm_ptrs_183(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p
        do i = 1, 4
            if (m2%l(i)%n /= 0) then
                array_rank = i
            endif
        enddo
        if (array_rank /= 3) then
            nullify(p)
            if (present(m)) then
                m = GMM_NULL_METADATA
            endif
            gmm_get_183 = GMM_INCONSISTENT_DIMS
        else
            gmm_get_183 = GMM_OK
        endif
    endif
end function gmm_get_183


subroutine gmm_dealloc_ptr_183()
    use gmm_internals
    use pointer_table_data__183
    implicit none

    deallocate (gmm_ptrs_183(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p)
    nullify    (gmm_ptrs_183(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p)
end subroutine gmm_dealloc_ptr_183
# 82 "gmm.F90" 2

# 1 "undefiner.hf" 1
# 83 "gmm.F90" 2

! 184




# 1 "gmm_checkpoint_tmpl.F90" 1
subroutine gmm_checkpoint_184(read_or_write)
    use gmm_internals
    use pointer_table_data_184
    implicit none
    logical read_or_write
    integer istat, fnom, i, j, ier, lcl_pti
    type(gmm_layout), dimension(1:4) :: siz
    type(gmm_attributes) :: attrib
    integer *8 :: key
    external fnom
    integer *8 get_address_from
    external get_address_from

    ! Read one  record from checkpoint file
    if (read_or_write) then
        ! Into next directory entry
        call add_directory_entry
        read(file_unit)directory(cur_page)%entry(cur_entry)%name
        ! Read layout
        read(file_unit)siz(1:4)
        ! Set layout in table
        directory(cur_page)%entry(cur_entry)%l(1:4) = siz(1:4)
        ! Read attributes
        read(file_unit)attrib
        attrib%flags = ior(attrib%flags,GMM_FLAG_READ)
        ! Set attributes in table
        directory(cur_page)%entry(cur_entry)%a = attrib
        read(file_unit)directory(cur_page)%entry(cur_entry)%data_type
        lcl_pti = lgmm_get_nxt_avail_ptr()
        directory(cur_page)%entry(cur_entry)%pointer_table_index = lcl_pti
        ordinal = ordinal + 1
        key = ishft((cur_page-1),PAGE_NB_SHFT) + ishft((cur_entry-1),NTRY_NB_SHFT)
        key = key + ishft(184,EXTN_NB_SHFT) + ishft(ordinal,MAGC_NB_SHFT)
        ! Set creation ordinal
        directory(cur_page)%entry(cur_entry)%a%key = key
        ! CODE POSSIBLY MISSING HERE FOR FLAGS SETTINGS
        allocate(gmm_ptrs_184(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p(&
                                                            &siz(1)%low:siz(1)%high&

                                                            &,siz(2)%low:siz(2)%high,&


                                                            &,siz(3)%low:siz(3)%high,&


                                                            &,siz(4)%low:siz(4)%high&

                                                        ))
        ! Read array
        read(file_unit)gmm_ptrs_184(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p
        directory(cur_page)%entry(cur_entry)%array_addr = get_address_from(&
            gmm_ptrs_184(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p)

        if (gmm_verbose_level == GMM_MSG_DEBUG) then
            write(6,'(a,a8,a,i4,a,i4,a,i4,a,i10)') 'name=',directory(cur_page)%entry(cur_entry)%name,' cur_page=',cur_page,' cur_entry=',cur_entry,' index=',directory(cur_page)%entry(cur_entry)%pointer_table_index,' addr=',get_address_from(gmm_ptrs_184(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p)
        endif
        ier = add_table_entry(gmm_ptrs_184(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p,key)
    else
        ! Write to checkpoint file (records with FLAG_RSTR property)
        if (gmm_verbose_level == GMM_MSG_DEBUG) then
            print *, 'checkpointing type ', 184
        endif
        do i = 1, table_size
            do j = 1, PAGE_SIZE
                if (iand(GMM_FLAG_RSTR,directory(i)%entry(j)%a%flags) .ne. 0 .and. directory(i)%entry(j)%data_type == 184) then
                    if (gmm_verbose_level == GMM_MSG_DEBUG) then
                        print *, 'writing field ', directory(i)%entry(j)%name
                    endif
                    write(file_unit)184
                    write(file_unit)directory(i)%entry(j)%name
                    write(file_unit)directory(i)%entry(j)%l(1:4)
                    attrib = directory(i)%entry(j)%a
                    attrib%flags = iand(attrib%flags,FLAGS_KEPT_IN_RESTART)
                    write(file_unit)attrib
                    write(file_unit)directory(i)%entry(j)%data_type
                    write(file_unit)gmm_ptrs_184(directory(i)%entry(j)%pointer_table_index)%p
                endif
            enddo
        enddo
    endif
end subroutine gmm_checkpoint_184
# 89 "gmm.F90" 2

# 1 "gmm_get_tmpl.F90" 1
integer function gmm_get_184(iname, p, m)
    use gmm_internals
    use pointer_table_data__184
    implicit none
    integer :: i, array_rank
    ! name (partially redundant with attributes)
    character(len=*), intent(in) :: iname







    integer*8, pointer  :: p(:,:,:,:)

    ! attributes (name in attributes is not used)
    type(gmm_metadata), optional, intent(out) :: m
    !  integer,intent(inout) :: reqid
    include 'gmm_directory_interface.inc'
    type(gmm_metadata) :: m2
    integer*8 :: key
    integer *8 get_address_from
    external get_address_from

    key = 0
    call check_directory_entry(iname, key)
    if (cur_page .eq. 0 .or. cur_entry .eq. 0) then
        ! quick check using key was not successful
        call find_directory_entry(iname,key)
    endif
    if(cur_page .eq. 0 .or. cur_entry .eq. 0) then
        ! return null entry
        if (present(m)) then
            m%a = GMM_NULL_ATTRIB
            m%l = GMM_NULL_LAYOUT
        endif
        nullify(p)
        key = GMM_KEY_NOT_FOUND
        gmm_get_184 = GMM_VAR_NOT_FOUND
    else
        m2%l = directory(cur_page)%entry(cur_entry)%l
        m2%a = directory(cur_page)%entry(cur_entry)%a
        if (present(m)) then
            ! return a copy of the proper entry
            m = m2
        endif
        p => gmm_ptrs_184(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p
        do i = 1, 4
            if (m2%l(i)%n /= 0) then
                array_rank = i
            endif
        enddo
        if (array_rank /= 4) then
            nullify(p)
            if (present(m)) then
                m = GMM_NULL_METADATA
            endif
            gmm_get_184 = GMM_INCONSISTENT_DIMS
        else
            gmm_get_184 = GMM_OK
        endif
    endif
end function gmm_get_184


subroutine gmm_dealloc_ptr_184()
    use gmm_internals
    use pointer_table_data__184
    implicit none

    deallocate (gmm_ptrs_184(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p)
    nullify    (gmm_ptrs_184(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p)
end subroutine gmm_dealloc_ptr_184
# 90 "gmm.F90" 2

# 1 "undefiner.hf" 1
# 91 "gmm.F90" 2





! Real Functions:



! 241




# 1 "gmm_checkpoint_tmpl.F90" 1
subroutine gmm_checkpoint_241(read_or_write)
    use gmm_internals
    use pointer_table_data_241
    implicit none
    logical read_or_write
    integer istat, fnom, i, j, ier, lcl_pti
    type(gmm_layout), dimension(1:1) :: siz
    type(gmm_attributes) :: attrib
    integer *8 :: key
    external fnom
    integer *8 get_address_from
    external get_address_from

    ! Read one  record from checkpoint file
    if (read_or_write) then
        ! Into next directory entry
        call add_directory_entry
        read(file_unit)directory(cur_page)%entry(cur_entry)%name
        ! Read layout
        read(file_unit)siz(1:1)
        ! Set layout in table
        directory(cur_page)%entry(cur_entry)%l(1:1) = siz(1:1)
        ! Read attributes
        read(file_unit)attrib
        attrib%flags = ior(attrib%flags,GMM_FLAG_READ)
        ! Set attributes in table
        directory(cur_page)%entry(cur_entry)%a = attrib
        read(file_unit)directory(cur_page)%entry(cur_entry)%data_type
        lcl_pti = lgmm_get_nxt_avail_ptr()
        directory(cur_page)%entry(cur_entry)%pointer_table_index = lcl_pti
        ordinal = ordinal + 1
        key = ishft((cur_page-1),PAGE_NB_SHFT) + ishft((cur_entry-1),NTRY_NB_SHFT)
        key = key + ishft(241,EXTN_NB_SHFT) + ishft(ordinal,MAGC_NB_SHFT)
        ! Set creation ordinal
        directory(cur_page)%entry(cur_entry)%a%key = key
        ! CODE POSSIBLY MISSING HERE FOR FLAGS SETTINGS
        allocate(gmm_ptrs_241(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p(&
                                                            &siz(1)%low:siz(1)%high&
# 48 "gmm_checkpoint_tmpl.F90"
                                                        ))
        ! Read array
        read(file_unit)gmm_ptrs_241(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p
        directory(cur_page)%entry(cur_entry)%array_addr = get_address_from(&
            gmm_ptrs_241(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p)

        if (gmm_verbose_level == GMM_MSG_DEBUG) then
            write(6,'(a,a8,a,i4,a,i4,a,i4,a,i10)') 'name=',directory(cur_page)%entry(cur_entry)%name,' cur_page=',cur_page,' cur_entry=',cur_entry,' index=',directory(cur_page)%entry(cur_entry)%pointer_table_index,' addr=',get_address_from(gmm_ptrs_241(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p)
        endif
        ier = add_table_entry(gmm_ptrs_241(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p,key)
    else
        ! Write to checkpoint file (records with FLAG_RSTR property)
        if (gmm_verbose_level == GMM_MSG_DEBUG) then
            print *, 'checkpointing type ', 241
        endif
        do i = 1, table_size
            do j = 1, PAGE_SIZE
                if (iand(GMM_FLAG_RSTR,directory(i)%entry(j)%a%flags) .ne. 0 .and. directory(i)%entry(j)%data_type == 241) then
                    if (gmm_verbose_level == GMM_MSG_DEBUG) then
                        print *, 'writing field ', directory(i)%entry(j)%name
                    endif
                    write(file_unit)241
                    write(file_unit)directory(i)%entry(j)%name
                    write(file_unit)directory(i)%entry(j)%l(1:1)
                    attrib = directory(i)%entry(j)%a
                    attrib%flags = iand(attrib%flags,FLAGS_KEPT_IN_RESTART)
                    write(file_unit)attrib
                    write(file_unit)directory(i)%entry(j)%data_type
                    write(file_unit)gmm_ptrs_241(directory(i)%entry(j)%pointer_table_index)%p
                endif
            enddo
        enddo
    endif
end subroutine gmm_checkpoint_241
# 105 "gmm.F90" 2

# 1 "gmm_get_tmpl.F90" 1
integer function gmm_get_241(iname, p, m)
    use gmm_internals
    use pointer_table_data__241
    implicit none
    integer :: i, array_rank
    ! name (partially redundant with attributes)
    character(len=*), intent(in) :: iname

    real*4, pointer  :: p(:)







    ! attributes (name in attributes is not used)
    type(gmm_metadata), optional, intent(out) :: m
    !  integer,intent(inout) :: reqid
    include 'gmm_directory_interface.inc'
    type(gmm_metadata) :: m2
    integer*8 :: key
    integer *8 get_address_from
    external get_address_from

    key = 0
    call check_directory_entry(iname, key)
    if (cur_page .eq. 0 .or. cur_entry .eq. 0) then
        ! quick check using key was not successful
        call find_directory_entry(iname,key)
    endif
    if(cur_page .eq. 0 .or. cur_entry .eq. 0) then
        ! return null entry
        if (present(m)) then
            m%a = GMM_NULL_ATTRIB
            m%l = GMM_NULL_LAYOUT
        endif
        nullify(p)
        key = GMM_KEY_NOT_FOUND
        gmm_get_241 = GMM_VAR_NOT_FOUND
    else
        m2%l = directory(cur_page)%entry(cur_entry)%l
        m2%a = directory(cur_page)%entry(cur_entry)%a
        if (present(m)) then
            ! return a copy of the proper entry
            m = m2
        endif
        p => gmm_ptrs_241(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p
        do i = 1, 4
            if (m2%l(i)%n /= 0) then
                array_rank = i
            endif
        enddo
        if (array_rank /= 1) then
            nullify(p)
            if (present(m)) then
                m = GMM_NULL_METADATA
            endif
            gmm_get_241 = GMM_INCONSISTENT_DIMS
        else
            gmm_get_241 = GMM_OK
        endif
    endif
end function gmm_get_241


subroutine gmm_dealloc_ptr_241()
    use gmm_internals
    use pointer_table_data__241
    implicit none

    deallocate (gmm_ptrs_241(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p)
    nullify    (gmm_ptrs_241(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p)
end subroutine gmm_dealloc_ptr_241
# 106 "gmm.F90" 2

# 1 "undefiner.hf" 1
# 107 "gmm.F90" 2

! 242




# 1 "gmm_checkpoint_tmpl.F90" 1
subroutine gmm_checkpoint_242(read_or_write)
    use gmm_internals
    use pointer_table_data_242
    implicit none
    logical read_or_write
    integer istat, fnom, i, j, ier, lcl_pti
    type(gmm_layout), dimension(1:2) :: siz
    type(gmm_attributes) :: attrib
    integer *8 :: key
    external fnom
    integer *8 get_address_from
    external get_address_from

    ! Read one  record from checkpoint file
    if (read_or_write) then
        ! Into next directory entry
        call add_directory_entry
        read(file_unit)directory(cur_page)%entry(cur_entry)%name
        ! Read layout
        read(file_unit)siz(1:2)
        ! Set layout in table
        directory(cur_page)%entry(cur_entry)%l(1:2) = siz(1:2)
        ! Read attributes
        read(file_unit)attrib
        attrib%flags = ior(attrib%flags,GMM_FLAG_READ)
        ! Set attributes in table
        directory(cur_page)%entry(cur_entry)%a = attrib
        read(file_unit)directory(cur_page)%entry(cur_entry)%data_type
        lcl_pti = lgmm_get_nxt_avail_ptr()
        directory(cur_page)%entry(cur_entry)%pointer_table_index = lcl_pti
        ordinal = ordinal + 1
        key = ishft((cur_page-1),PAGE_NB_SHFT) + ishft((cur_entry-1),NTRY_NB_SHFT)
        key = key + ishft(242,EXTN_NB_SHFT) + ishft(ordinal,MAGC_NB_SHFT)
        ! Set creation ordinal
        directory(cur_page)%entry(cur_entry)%a%key = key
        ! CODE POSSIBLY MISSING HERE FOR FLAGS SETTINGS
        allocate(gmm_ptrs_242(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p(&
                                                            &siz(1)%low:siz(1)%high&

                                                            &,siz(2)%low:siz(2)%high,&







                                                        ))
        ! Read array
        read(file_unit)gmm_ptrs_242(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p
        directory(cur_page)%entry(cur_entry)%array_addr = get_address_from(&
            gmm_ptrs_242(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p)

        if (gmm_verbose_level == GMM_MSG_DEBUG) then
            write(6,'(a,a8,a,i4,a,i4,a,i4,a,i10)') 'name=',directory(cur_page)%entry(cur_entry)%name,' cur_page=',cur_page,' cur_entry=',cur_entry,' index=',directory(cur_page)%entry(cur_entry)%pointer_table_index,' addr=',get_address_from(gmm_ptrs_242(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p)
        endif
        ier = add_table_entry(gmm_ptrs_242(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p,key)
    else
        ! Write to checkpoint file (records with FLAG_RSTR property)
        if (gmm_verbose_level == GMM_MSG_DEBUG) then
            print *, 'checkpointing type ', 242
        endif
        do i = 1, table_size
            do j = 1, PAGE_SIZE
                if (iand(GMM_FLAG_RSTR,directory(i)%entry(j)%a%flags) .ne. 0 .and. directory(i)%entry(j)%data_type == 242) then
                    if (gmm_verbose_level == GMM_MSG_DEBUG) then
                        print *, 'writing field ', directory(i)%entry(j)%name
                    endif
                    write(file_unit)242
                    write(file_unit)directory(i)%entry(j)%name
                    write(file_unit)directory(i)%entry(j)%l(1:2)
                    attrib = directory(i)%entry(j)%a
                    attrib%flags = iand(attrib%flags,FLAGS_KEPT_IN_RESTART)
                    write(file_unit)attrib
                    write(file_unit)directory(i)%entry(j)%data_type
                    write(file_unit)gmm_ptrs_242(directory(i)%entry(j)%pointer_table_index)%p
                endif
            enddo
        enddo
    endif
end subroutine gmm_checkpoint_242
# 113 "gmm.F90" 2

# 1 "gmm_get_tmpl.F90" 1
integer function gmm_get_242(iname, p, m)
    use gmm_internals
    use pointer_table_data__242
    implicit none
    integer :: i, array_rank
    ! name (partially redundant with attributes)
    character(len=*), intent(in) :: iname



    real*4, pointer  :: p(:,:)





    ! attributes (name in attributes is not used)
    type(gmm_metadata), optional, intent(out) :: m
    !  integer,intent(inout) :: reqid
    include 'gmm_directory_interface.inc'
    type(gmm_metadata) :: m2
    integer*8 :: key
    integer *8 get_address_from
    external get_address_from

    key = 0
    call check_directory_entry(iname, key)
    if (cur_page .eq. 0 .or. cur_entry .eq. 0) then
        ! quick check using key was not successful
        call find_directory_entry(iname,key)
    endif
    if(cur_page .eq. 0 .or. cur_entry .eq. 0) then
        ! return null entry
        if (present(m)) then
            m%a = GMM_NULL_ATTRIB
            m%l = GMM_NULL_LAYOUT
        endif
        nullify(p)
        key = GMM_KEY_NOT_FOUND
        gmm_get_242 = GMM_VAR_NOT_FOUND
    else
        m2%l = directory(cur_page)%entry(cur_entry)%l
        m2%a = directory(cur_page)%entry(cur_entry)%a
        if (present(m)) then
            ! return a copy of the proper entry
            m = m2
        endif
        p => gmm_ptrs_242(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p
        do i = 1, 4
            if (m2%l(i)%n /= 0) then
                array_rank = i
            endif
        enddo
        if (array_rank /= 2) then
            nullify(p)
            if (present(m)) then
                m = GMM_NULL_METADATA
            endif
            gmm_get_242 = GMM_INCONSISTENT_DIMS
        else
            gmm_get_242 = GMM_OK
        endif
    endif
end function gmm_get_242


subroutine gmm_dealloc_ptr_242()
    use gmm_internals
    use pointer_table_data__242
    implicit none

    deallocate (gmm_ptrs_242(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p)
    nullify    (gmm_ptrs_242(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p)
end subroutine gmm_dealloc_ptr_242
# 114 "gmm.F90" 2

# 1 "undefiner.hf" 1
# 115 "gmm.F90" 2

! 243




# 1 "gmm_checkpoint_tmpl.F90" 1
subroutine gmm_checkpoint_243(read_or_write)
    use gmm_internals
    use pointer_table_data_243
    implicit none
    logical read_or_write
    integer istat, fnom, i, j, ier, lcl_pti
    type(gmm_layout), dimension(1:3) :: siz
    type(gmm_attributes) :: attrib
    integer *8 :: key
    external fnom
    integer *8 get_address_from
    external get_address_from

    ! Read one  record from checkpoint file
    if (read_or_write) then
        ! Into next directory entry
        call add_directory_entry
        read(file_unit)directory(cur_page)%entry(cur_entry)%name
        ! Read layout
        read(file_unit)siz(1:3)
        ! Set layout in table
        directory(cur_page)%entry(cur_entry)%l(1:3) = siz(1:3)
        ! Read attributes
        read(file_unit)attrib
        attrib%flags = ior(attrib%flags,GMM_FLAG_READ)
        ! Set attributes in table
        directory(cur_page)%entry(cur_entry)%a = attrib
        read(file_unit)directory(cur_page)%entry(cur_entry)%data_type
        lcl_pti = lgmm_get_nxt_avail_ptr()
        directory(cur_page)%entry(cur_entry)%pointer_table_index = lcl_pti
        ordinal = ordinal + 1
        key = ishft((cur_page-1),PAGE_NB_SHFT) + ishft((cur_entry-1),NTRY_NB_SHFT)
        key = key + ishft(243,EXTN_NB_SHFT) + ishft(ordinal,MAGC_NB_SHFT)
        ! Set creation ordinal
        directory(cur_page)%entry(cur_entry)%a%key = key
        ! CODE POSSIBLY MISSING HERE FOR FLAGS SETTINGS
        allocate(gmm_ptrs_243(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p(&
                                                            &siz(1)%low:siz(1)%high&

                                                            &,siz(2)%low:siz(2)%high,&


                                                            &,siz(3)%low:siz(3)%high,&




                                                        ))
        ! Read array
        read(file_unit)gmm_ptrs_243(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p
        directory(cur_page)%entry(cur_entry)%array_addr = get_address_from(&
            gmm_ptrs_243(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p)

        if (gmm_verbose_level == GMM_MSG_DEBUG) then
            write(6,'(a,a8,a,i4,a,i4,a,i4,a,i10)') 'name=',directory(cur_page)%entry(cur_entry)%name,' cur_page=',cur_page,' cur_entry=',cur_entry,' index=',directory(cur_page)%entry(cur_entry)%pointer_table_index,' addr=',get_address_from(gmm_ptrs_243(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p)
        endif
        ier = add_table_entry(gmm_ptrs_243(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p,key)
    else
        ! Write to checkpoint file (records with FLAG_RSTR property)
        if (gmm_verbose_level == GMM_MSG_DEBUG) then
            print *, 'checkpointing type ', 243
        endif
        do i = 1, table_size
            do j = 1, PAGE_SIZE
                if (iand(GMM_FLAG_RSTR,directory(i)%entry(j)%a%flags) .ne. 0 .and. directory(i)%entry(j)%data_type == 243) then
                    if (gmm_verbose_level == GMM_MSG_DEBUG) then
                        print *, 'writing field ', directory(i)%entry(j)%name
                    endif
                    write(file_unit)243
                    write(file_unit)directory(i)%entry(j)%name
                    write(file_unit)directory(i)%entry(j)%l(1:3)
                    attrib = directory(i)%entry(j)%a
                    attrib%flags = iand(attrib%flags,FLAGS_KEPT_IN_RESTART)
                    write(file_unit)attrib
                    write(file_unit)directory(i)%entry(j)%data_type
                    write(file_unit)gmm_ptrs_243(directory(i)%entry(j)%pointer_table_index)%p
                endif
            enddo
        enddo
    endif
end subroutine gmm_checkpoint_243
# 121 "gmm.F90" 2

# 1 "gmm_get_tmpl.F90" 1
integer function gmm_get_243(iname, p, m)
    use gmm_internals
    use pointer_table_data__243
    implicit none
    integer :: i, array_rank
    ! name (partially redundant with attributes)
    character(len=*), intent(in) :: iname





    real*4, pointer  :: p(:,:,:)



    ! attributes (name in attributes is not used)
    type(gmm_metadata), optional, intent(out) :: m
    !  integer,intent(inout) :: reqid
    include 'gmm_directory_interface.inc'
    type(gmm_metadata) :: m2
    integer*8 :: key
    integer *8 get_address_from
    external get_address_from

    key = 0
    call check_directory_entry(iname, key)
    if (cur_page .eq. 0 .or. cur_entry .eq. 0) then
        ! quick check using key was not successful
        call find_directory_entry(iname,key)
    endif
    if(cur_page .eq. 0 .or. cur_entry .eq. 0) then
        ! return null entry
        if (present(m)) then
            m%a = GMM_NULL_ATTRIB
            m%l = GMM_NULL_LAYOUT
        endif
        nullify(p)
        key = GMM_KEY_NOT_FOUND
        gmm_get_243 = GMM_VAR_NOT_FOUND
    else
        m2%l = directory(cur_page)%entry(cur_entry)%l
        m2%a = directory(cur_page)%entry(cur_entry)%a
        if (present(m)) then
            ! return a copy of the proper entry
            m = m2
        endif
        p => gmm_ptrs_243(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p
        do i = 1, 4
            if (m2%l(i)%n /= 0) then
                array_rank = i
            endif
        enddo
        if (array_rank /= 3) then
            nullify(p)
            if (present(m)) then
                m = GMM_NULL_METADATA
            endif
            gmm_get_243 = GMM_INCONSISTENT_DIMS
        else
            gmm_get_243 = GMM_OK
        endif
    endif
end function gmm_get_243


subroutine gmm_dealloc_ptr_243()
    use gmm_internals
    use pointer_table_data__243
    implicit none

    deallocate (gmm_ptrs_243(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p)
    nullify    (gmm_ptrs_243(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p)
end subroutine gmm_dealloc_ptr_243
# 122 "gmm.F90" 2

# 1 "undefiner.hf" 1
# 123 "gmm.F90" 2

! 244




# 1 "gmm_checkpoint_tmpl.F90" 1
subroutine gmm_checkpoint_244(read_or_write)
    use gmm_internals
    use pointer_table_data_244
    implicit none
    logical read_or_write
    integer istat, fnom, i, j, ier, lcl_pti
    type(gmm_layout), dimension(1:4) :: siz
    type(gmm_attributes) :: attrib
    integer *8 :: key
    external fnom
    integer *8 get_address_from
    external get_address_from

    ! Read one  record from checkpoint file
    if (read_or_write) then
        ! Into next directory entry
        call add_directory_entry
        read(file_unit)directory(cur_page)%entry(cur_entry)%name
        ! Read layout
        read(file_unit)siz(1:4)
        ! Set layout in table
        directory(cur_page)%entry(cur_entry)%l(1:4) = siz(1:4)
        ! Read attributes
        read(file_unit)attrib
        attrib%flags = ior(attrib%flags,GMM_FLAG_READ)
        ! Set attributes in table
        directory(cur_page)%entry(cur_entry)%a = attrib
        read(file_unit)directory(cur_page)%entry(cur_entry)%data_type
        lcl_pti = lgmm_get_nxt_avail_ptr()
        directory(cur_page)%entry(cur_entry)%pointer_table_index = lcl_pti
        ordinal = ordinal + 1
        key = ishft((cur_page-1),PAGE_NB_SHFT) + ishft((cur_entry-1),NTRY_NB_SHFT)
        key = key + ishft(244,EXTN_NB_SHFT) + ishft(ordinal,MAGC_NB_SHFT)
        ! Set creation ordinal
        directory(cur_page)%entry(cur_entry)%a%key = key
        ! CODE POSSIBLY MISSING HERE FOR FLAGS SETTINGS
        allocate(gmm_ptrs_244(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p(&
                                                            &siz(1)%low:siz(1)%high&

                                                            &,siz(2)%low:siz(2)%high,&


                                                            &,siz(3)%low:siz(3)%high,&


                                                            &,siz(4)%low:siz(4)%high&

                                                        ))
        ! Read array
        read(file_unit)gmm_ptrs_244(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p
        directory(cur_page)%entry(cur_entry)%array_addr = get_address_from(&
            gmm_ptrs_244(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p)

        if (gmm_verbose_level == GMM_MSG_DEBUG) then
            write(6,'(a,a8,a,i4,a,i4,a,i4,a,i10)') 'name=',directory(cur_page)%entry(cur_entry)%name,' cur_page=',cur_page,' cur_entry=',cur_entry,' index=',directory(cur_page)%entry(cur_entry)%pointer_table_index,' addr=',get_address_from(gmm_ptrs_244(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p)
        endif
        ier = add_table_entry(gmm_ptrs_244(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p,key)
    else
        ! Write to checkpoint file (records with FLAG_RSTR property)
        if (gmm_verbose_level == GMM_MSG_DEBUG) then
            print *, 'checkpointing type ', 244
        endif
        do i = 1, table_size
            do j = 1, PAGE_SIZE
                if (iand(GMM_FLAG_RSTR,directory(i)%entry(j)%a%flags) .ne. 0 .and. directory(i)%entry(j)%data_type == 244) then
                    if (gmm_verbose_level == GMM_MSG_DEBUG) then
                        print *, 'writing field ', directory(i)%entry(j)%name
                    endif
                    write(file_unit)244
                    write(file_unit)directory(i)%entry(j)%name
                    write(file_unit)directory(i)%entry(j)%l(1:4)
                    attrib = directory(i)%entry(j)%a
                    attrib%flags = iand(attrib%flags,FLAGS_KEPT_IN_RESTART)
                    write(file_unit)attrib
                    write(file_unit)directory(i)%entry(j)%data_type
                    write(file_unit)gmm_ptrs_244(directory(i)%entry(j)%pointer_table_index)%p
                endif
            enddo
        enddo
    endif
end subroutine gmm_checkpoint_244
# 129 "gmm.F90" 2

# 1 "gmm_get_tmpl.F90" 1
integer function gmm_get_244(iname, p, m)
    use gmm_internals
    use pointer_table_data__244
    implicit none
    integer :: i, array_rank
    ! name (partially redundant with attributes)
    character(len=*), intent(in) :: iname







    real*4, pointer  :: p(:,:,:,:)

    ! attributes (name in attributes is not used)
    type(gmm_metadata), optional, intent(out) :: m
    !  integer,intent(inout) :: reqid
    include 'gmm_directory_interface.inc'
    type(gmm_metadata) :: m2
    integer*8 :: key
    integer *8 get_address_from
    external get_address_from

    key = 0
    call check_directory_entry(iname, key)
    if (cur_page .eq. 0 .or. cur_entry .eq. 0) then
        ! quick check using key was not successful
        call find_directory_entry(iname,key)
    endif
    if(cur_page .eq. 0 .or. cur_entry .eq. 0) then
        ! return null entry
        if (present(m)) then
            m%a = GMM_NULL_ATTRIB
            m%l = GMM_NULL_LAYOUT
        endif
        nullify(p)
        key = GMM_KEY_NOT_FOUND
        gmm_get_244 = GMM_VAR_NOT_FOUND
    else
        m2%l = directory(cur_page)%entry(cur_entry)%l
        m2%a = directory(cur_page)%entry(cur_entry)%a
        if (present(m)) then
            ! return a copy of the proper entry
            m = m2
        endif
        p => gmm_ptrs_244(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p
        do i = 1, 4
            if (m2%l(i)%n /= 0) then
                array_rank = i
            endif
        enddo
        if (array_rank /= 4) then
            nullify(p)
            if (present(m)) then
                m = GMM_NULL_METADATA
            endif
            gmm_get_244 = GMM_INCONSISTENT_DIMS
        else
            gmm_get_244 = GMM_OK
        endif
    endif
end function gmm_get_244


subroutine gmm_dealloc_ptr_244()
    use gmm_internals
    use pointer_table_data__244
    implicit none

    deallocate (gmm_ptrs_244(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p)
    nullify    (gmm_ptrs_244(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p)
end subroutine gmm_dealloc_ptr_244
# 130 "gmm.F90" 2

# 1 "undefiner.hf" 1
# 131 "gmm.F90" 2




! 281




# 1 "gmm_checkpoint_tmpl.F90" 1
subroutine gmm_checkpoint_281(read_or_write)
    use gmm_internals
    use pointer_table_data_281
    implicit none
    logical read_or_write
    integer istat, fnom, i, j, ier, lcl_pti
    type(gmm_layout), dimension(1:1) :: siz
    type(gmm_attributes) :: attrib
    integer *8 :: key
    external fnom
    integer *8 get_address_from
    external get_address_from

    ! Read one  record from checkpoint file
    if (read_or_write) then
        ! Into next directory entry
        call add_directory_entry
        read(file_unit)directory(cur_page)%entry(cur_entry)%name
        ! Read layout
        read(file_unit)siz(1:1)
        ! Set layout in table
        directory(cur_page)%entry(cur_entry)%l(1:1) = siz(1:1)
        ! Read attributes
        read(file_unit)attrib
        attrib%flags = ior(attrib%flags,GMM_FLAG_READ)
        ! Set attributes in table
        directory(cur_page)%entry(cur_entry)%a = attrib
        read(file_unit)directory(cur_page)%entry(cur_entry)%data_type
        lcl_pti = lgmm_get_nxt_avail_ptr()
        directory(cur_page)%entry(cur_entry)%pointer_table_index = lcl_pti
        ordinal = ordinal + 1
        key = ishft((cur_page-1),PAGE_NB_SHFT) + ishft((cur_entry-1),NTRY_NB_SHFT)
        key = key + ishft(281,EXTN_NB_SHFT) + ishft(ordinal,MAGC_NB_SHFT)
        ! Set creation ordinal
        directory(cur_page)%entry(cur_entry)%a%key = key
        ! CODE POSSIBLY MISSING HERE FOR FLAGS SETTINGS
        allocate(gmm_ptrs_281(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p(&
                                                            &siz(1)%low:siz(1)%high&
# 48 "gmm_checkpoint_tmpl.F90"
                                                        ))
        ! Read array
        read(file_unit)gmm_ptrs_281(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p
        directory(cur_page)%entry(cur_entry)%array_addr = get_address_from(&
            gmm_ptrs_281(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p)

        if (gmm_verbose_level == GMM_MSG_DEBUG) then
            write(6,'(a,a8,a,i4,a,i4,a,i4,a,i10)') 'name=',directory(cur_page)%entry(cur_entry)%name,' cur_page=',cur_page,' cur_entry=',cur_entry,' index=',directory(cur_page)%entry(cur_entry)%pointer_table_index,' addr=',get_address_from(gmm_ptrs_281(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p)
        endif
        ier = add_table_entry(gmm_ptrs_281(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p,key)
    else
        ! Write to checkpoint file (records with FLAG_RSTR property)
        if (gmm_verbose_level == GMM_MSG_DEBUG) then
            print *, 'checkpointing type ', 281
        endif
        do i = 1, table_size
            do j = 1, PAGE_SIZE
                if (iand(GMM_FLAG_RSTR,directory(i)%entry(j)%a%flags) .ne. 0 .and. directory(i)%entry(j)%data_type == 281) then
                    if (gmm_verbose_level == GMM_MSG_DEBUG) then
                        print *, 'writing field ', directory(i)%entry(j)%name
                    endif
                    write(file_unit)281
                    write(file_unit)directory(i)%entry(j)%name
                    write(file_unit)directory(i)%entry(j)%l(1:1)
                    attrib = directory(i)%entry(j)%a
                    attrib%flags = iand(attrib%flags,FLAGS_KEPT_IN_RESTART)
                    write(file_unit)attrib
                    write(file_unit)directory(i)%entry(j)%data_type
                    write(file_unit)gmm_ptrs_281(directory(i)%entry(j)%pointer_table_index)%p
                endif
            enddo
        enddo
    endif
end subroutine gmm_checkpoint_281
# 140 "gmm.F90" 2

# 1 "gmm_get_tmpl.F90" 1
integer function gmm_get_281(iname, p, m)
    use gmm_internals
    use pointer_table_data__281
    implicit none
    integer :: i, array_rank
    ! name (partially redundant with attributes)
    character(len=*), intent(in) :: iname

    real*8, pointer  :: p(:)







    ! attributes (name in attributes is not used)
    type(gmm_metadata), optional, intent(out) :: m
    !  integer,intent(inout) :: reqid
    include 'gmm_directory_interface.inc'
    type(gmm_metadata) :: m2
    integer*8 :: key
    integer *8 get_address_from
    external get_address_from

    key = 0
    call check_directory_entry(iname, key)
    if (cur_page .eq. 0 .or. cur_entry .eq. 0) then
        ! quick check using key was not successful
        call find_directory_entry(iname,key)
    endif
    if(cur_page .eq. 0 .or. cur_entry .eq. 0) then
        ! return null entry
        if (present(m)) then
            m%a = GMM_NULL_ATTRIB
            m%l = GMM_NULL_LAYOUT
        endif
        nullify(p)
        key = GMM_KEY_NOT_FOUND
        gmm_get_281 = GMM_VAR_NOT_FOUND
    else
        m2%l = directory(cur_page)%entry(cur_entry)%l
        m2%a = directory(cur_page)%entry(cur_entry)%a
        if (present(m)) then
            ! return a copy of the proper entry
            m = m2
        endif
        p => gmm_ptrs_281(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p
        do i = 1, 4
            if (m2%l(i)%n /= 0) then
                array_rank = i
            endif
        enddo
        if (array_rank /= 1) then
            nullify(p)
            if (present(m)) then
                m = GMM_NULL_METADATA
            endif
            gmm_get_281 = GMM_INCONSISTENT_DIMS
        else
            gmm_get_281 = GMM_OK
        endif
    endif
end function gmm_get_281


subroutine gmm_dealloc_ptr_281()
    use gmm_internals
    use pointer_table_data__281
    implicit none

    deallocate (gmm_ptrs_281(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p)
    nullify    (gmm_ptrs_281(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p)
end subroutine gmm_dealloc_ptr_281
# 141 "gmm.F90" 2

# 1 "undefiner.hf" 1
# 142 "gmm.F90" 2

! 282




# 1 "gmm_checkpoint_tmpl.F90" 1
subroutine gmm_checkpoint_282(read_or_write)
    use gmm_internals
    use pointer_table_data_282
    implicit none
    logical read_or_write
    integer istat, fnom, i, j, ier, lcl_pti
    type(gmm_layout), dimension(1:2) :: siz
    type(gmm_attributes) :: attrib
    integer *8 :: key
    external fnom
    integer *8 get_address_from
    external get_address_from

    ! Read one  record from checkpoint file
    if (read_or_write) then
        ! Into next directory entry
        call add_directory_entry
        read(file_unit)directory(cur_page)%entry(cur_entry)%name
        ! Read layout
        read(file_unit)siz(1:2)
        ! Set layout in table
        directory(cur_page)%entry(cur_entry)%l(1:2) = siz(1:2)
        ! Read attributes
        read(file_unit)attrib
        attrib%flags = ior(attrib%flags,GMM_FLAG_READ)
        ! Set attributes in table
        directory(cur_page)%entry(cur_entry)%a = attrib
        read(file_unit)directory(cur_page)%entry(cur_entry)%data_type
        lcl_pti = lgmm_get_nxt_avail_ptr()
        directory(cur_page)%entry(cur_entry)%pointer_table_index = lcl_pti
        ordinal = ordinal + 1
        key = ishft((cur_page-1),PAGE_NB_SHFT) + ishft((cur_entry-1),NTRY_NB_SHFT)
        key = key + ishft(282,EXTN_NB_SHFT) + ishft(ordinal,MAGC_NB_SHFT)
        ! Set creation ordinal
        directory(cur_page)%entry(cur_entry)%a%key = key
        ! CODE POSSIBLY MISSING HERE FOR FLAGS SETTINGS
        allocate(gmm_ptrs_282(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p(&
                                                            &siz(1)%low:siz(1)%high&

                                                            &,siz(2)%low:siz(2)%high,&







                                                        ))
        ! Read array
        read(file_unit)gmm_ptrs_282(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p
        directory(cur_page)%entry(cur_entry)%array_addr = get_address_from(&
            gmm_ptrs_282(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p)

        if (gmm_verbose_level == GMM_MSG_DEBUG) then
            write(6,'(a,a8,a,i4,a,i4,a,i4,a,i10)') 'name=',directory(cur_page)%entry(cur_entry)%name,' cur_page=',cur_page,' cur_entry=',cur_entry,' index=',directory(cur_page)%entry(cur_entry)%pointer_table_index,' addr=',get_address_from(gmm_ptrs_282(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p)
        endif
        ier = add_table_entry(gmm_ptrs_282(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p,key)
    else
        ! Write to checkpoint file (records with FLAG_RSTR property)
        if (gmm_verbose_level == GMM_MSG_DEBUG) then
            print *, 'checkpointing type ', 282
        endif
        do i = 1, table_size
            do j = 1, PAGE_SIZE
                if (iand(GMM_FLAG_RSTR,directory(i)%entry(j)%a%flags) .ne. 0 .and. directory(i)%entry(j)%data_type == 282) then
                    if (gmm_verbose_level == GMM_MSG_DEBUG) then
                        print *, 'writing field ', directory(i)%entry(j)%name
                    endif
                    write(file_unit)282
                    write(file_unit)directory(i)%entry(j)%name
                    write(file_unit)directory(i)%entry(j)%l(1:2)
                    attrib = directory(i)%entry(j)%a
                    attrib%flags = iand(attrib%flags,FLAGS_KEPT_IN_RESTART)
                    write(file_unit)attrib
                    write(file_unit)directory(i)%entry(j)%data_type
                    write(file_unit)gmm_ptrs_282(directory(i)%entry(j)%pointer_table_index)%p
                endif
            enddo
        enddo
    endif
end subroutine gmm_checkpoint_282
# 148 "gmm.F90" 2

# 1 "gmm_get_tmpl.F90" 1
integer function gmm_get_282(iname, p, m)
    use gmm_internals
    use pointer_table_data__282
    implicit none
    integer :: i, array_rank
    ! name (partially redundant with attributes)
    character(len=*), intent(in) :: iname



    real*8, pointer  :: p(:,:)





    ! attributes (name in attributes is not used)
    type(gmm_metadata), optional, intent(out) :: m
    !  integer,intent(inout) :: reqid
    include 'gmm_directory_interface.inc'
    type(gmm_metadata) :: m2
    integer*8 :: key
    integer *8 get_address_from
    external get_address_from

    key = 0
    call check_directory_entry(iname, key)
    if (cur_page .eq. 0 .or. cur_entry .eq. 0) then
        ! quick check using key was not successful
        call find_directory_entry(iname,key)
    endif
    if(cur_page .eq. 0 .or. cur_entry .eq. 0) then
        ! return null entry
        if (present(m)) then
            m%a = GMM_NULL_ATTRIB
            m%l = GMM_NULL_LAYOUT
        endif
        nullify(p)
        key = GMM_KEY_NOT_FOUND
        gmm_get_282 = GMM_VAR_NOT_FOUND
    else
        m2%l = directory(cur_page)%entry(cur_entry)%l
        m2%a = directory(cur_page)%entry(cur_entry)%a
        if (present(m)) then
            ! return a copy of the proper entry
            m = m2
        endif
        p => gmm_ptrs_282(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p
        do i = 1, 4
            if (m2%l(i)%n /= 0) then
                array_rank = i
            endif
        enddo
        if (array_rank /= 2) then
            nullify(p)
            if (present(m)) then
                m = GMM_NULL_METADATA
            endif
            gmm_get_282 = GMM_INCONSISTENT_DIMS
        else
            gmm_get_282 = GMM_OK
        endif
    endif
end function gmm_get_282


subroutine gmm_dealloc_ptr_282()
    use gmm_internals
    use pointer_table_data__282
    implicit none

    deallocate (gmm_ptrs_282(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p)
    nullify    (gmm_ptrs_282(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p)
end subroutine gmm_dealloc_ptr_282
# 149 "gmm.F90" 2

# 1 "undefiner.hf" 1
# 150 "gmm.F90" 2

! 283




# 1 "gmm_checkpoint_tmpl.F90" 1
subroutine gmm_checkpoint_283(read_or_write)
    use gmm_internals
    use pointer_table_data_283
    implicit none
    logical read_or_write
    integer istat, fnom, i, j, ier, lcl_pti
    type(gmm_layout), dimension(1:3) :: siz
    type(gmm_attributes) :: attrib
    integer *8 :: key
    external fnom
    integer *8 get_address_from
    external get_address_from

    ! Read one  record from checkpoint file
    if (read_or_write) then
        ! Into next directory entry
        call add_directory_entry
        read(file_unit)directory(cur_page)%entry(cur_entry)%name
        ! Read layout
        read(file_unit)siz(1:3)
        ! Set layout in table
        directory(cur_page)%entry(cur_entry)%l(1:3) = siz(1:3)
        ! Read attributes
        read(file_unit)attrib
        attrib%flags = ior(attrib%flags,GMM_FLAG_READ)
        ! Set attributes in table
        directory(cur_page)%entry(cur_entry)%a = attrib
        read(file_unit)directory(cur_page)%entry(cur_entry)%data_type
        lcl_pti = lgmm_get_nxt_avail_ptr()
        directory(cur_page)%entry(cur_entry)%pointer_table_index = lcl_pti
        ordinal = ordinal + 1
        key = ishft((cur_page-1),PAGE_NB_SHFT) + ishft((cur_entry-1),NTRY_NB_SHFT)
        key = key + ishft(283,EXTN_NB_SHFT) + ishft(ordinal,MAGC_NB_SHFT)
        ! Set creation ordinal
        directory(cur_page)%entry(cur_entry)%a%key = key
        ! CODE POSSIBLY MISSING HERE FOR FLAGS SETTINGS
        allocate(gmm_ptrs_283(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p(&
                                                            &siz(1)%low:siz(1)%high&

                                                            &,siz(2)%low:siz(2)%high,&


                                                            &,siz(3)%low:siz(3)%high,&




                                                        ))
        ! Read array
        read(file_unit)gmm_ptrs_283(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p
        directory(cur_page)%entry(cur_entry)%array_addr = get_address_from(&
            gmm_ptrs_283(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p)

        if (gmm_verbose_level == GMM_MSG_DEBUG) then
            write(6,'(a,a8,a,i4,a,i4,a,i4,a,i10)') 'name=',directory(cur_page)%entry(cur_entry)%name,' cur_page=',cur_page,' cur_entry=',cur_entry,' index=',directory(cur_page)%entry(cur_entry)%pointer_table_index,' addr=',get_address_from(gmm_ptrs_283(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p)
        endif
        ier = add_table_entry(gmm_ptrs_283(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p,key)
    else
        ! Write to checkpoint file (records with FLAG_RSTR property)
        if (gmm_verbose_level == GMM_MSG_DEBUG) then
            print *, 'checkpointing type ', 283
        endif
        do i = 1, table_size
            do j = 1, PAGE_SIZE
                if (iand(GMM_FLAG_RSTR,directory(i)%entry(j)%a%flags) .ne. 0 .and. directory(i)%entry(j)%data_type == 283) then
                    if (gmm_verbose_level == GMM_MSG_DEBUG) then
                        print *, 'writing field ', directory(i)%entry(j)%name
                    endif
                    write(file_unit)283
                    write(file_unit)directory(i)%entry(j)%name
                    write(file_unit)directory(i)%entry(j)%l(1:3)
                    attrib = directory(i)%entry(j)%a
                    attrib%flags = iand(attrib%flags,FLAGS_KEPT_IN_RESTART)
                    write(file_unit)attrib
                    write(file_unit)directory(i)%entry(j)%data_type
                    write(file_unit)gmm_ptrs_283(directory(i)%entry(j)%pointer_table_index)%p
                endif
            enddo
        enddo
    endif
end subroutine gmm_checkpoint_283
# 156 "gmm.F90" 2

# 1 "gmm_get_tmpl.F90" 1
integer function gmm_get_283(iname, p, m)
    use gmm_internals
    use pointer_table_data__283
    implicit none
    integer :: i, array_rank
    ! name (partially redundant with attributes)
    character(len=*), intent(in) :: iname





    real*8, pointer  :: p(:,:,:)



    ! attributes (name in attributes is not used)
    type(gmm_metadata), optional, intent(out) :: m
    !  integer,intent(inout) :: reqid
    include 'gmm_directory_interface.inc'
    type(gmm_metadata) :: m2
    integer*8 :: key
    integer *8 get_address_from
    external get_address_from

    key = 0
    call check_directory_entry(iname, key)
    if (cur_page .eq. 0 .or. cur_entry .eq. 0) then
        ! quick check using key was not successful
        call find_directory_entry(iname,key)
    endif
    if(cur_page .eq. 0 .or. cur_entry .eq. 0) then
        ! return null entry
        if (present(m)) then
            m%a = GMM_NULL_ATTRIB
            m%l = GMM_NULL_LAYOUT
        endif
        nullify(p)
        key = GMM_KEY_NOT_FOUND
        gmm_get_283 = GMM_VAR_NOT_FOUND
    else
        m2%l = directory(cur_page)%entry(cur_entry)%l
        m2%a = directory(cur_page)%entry(cur_entry)%a
        if (present(m)) then
            ! return a copy of the proper entry
            m = m2
        endif
        p => gmm_ptrs_283(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p
        do i = 1, 4
            if (m2%l(i)%n /= 0) then
                array_rank = i
            endif
        enddo
        if (array_rank /= 3) then
            nullify(p)
            if (present(m)) then
                m = GMM_NULL_METADATA
            endif
            gmm_get_283 = GMM_INCONSISTENT_DIMS
        else
            gmm_get_283 = GMM_OK
        endif
    endif
end function gmm_get_283


subroutine gmm_dealloc_ptr_283()
    use gmm_internals
    use pointer_table_data__283
    implicit none

    deallocate (gmm_ptrs_283(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p)
    nullify    (gmm_ptrs_283(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p)
end subroutine gmm_dealloc_ptr_283
# 157 "gmm.F90" 2

# 1 "undefiner.hf" 1
# 158 "gmm.F90" 2

! 284




# 1 "gmm_checkpoint_tmpl.F90" 1
subroutine gmm_checkpoint_284(read_or_write)
    use gmm_internals
    use pointer_table_data_284
    implicit none
    logical read_or_write
    integer istat, fnom, i, j, ier, lcl_pti
    type(gmm_layout), dimension(1:4) :: siz
    type(gmm_attributes) :: attrib
    integer *8 :: key
    external fnom
    integer *8 get_address_from
    external get_address_from

    ! Read one  record from checkpoint file
    if (read_or_write) then
        ! Into next directory entry
        call add_directory_entry
        read(file_unit)directory(cur_page)%entry(cur_entry)%name
        ! Read layout
        read(file_unit)siz(1:4)
        ! Set layout in table
        directory(cur_page)%entry(cur_entry)%l(1:4) = siz(1:4)
        ! Read attributes
        read(file_unit)attrib
        attrib%flags = ior(attrib%flags,GMM_FLAG_READ)
        ! Set attributes in table
        directory(cur_page)%entry(cur_entry)%a = attrib
        read(file_unit)directory(cur_page)%entry(cur_entry)%data_type
        lcl_pti = lgmm_get_nxt_avail_ptr()
        directory(cur_page)%entry(cur_entry)%pointer_table_index = lcl_pti
        ordinal = ordinal + 1
        key = ishft((cur_page-1),PAGE_NB_SHFT) + ishft((cur_entry-1),NTRY_NB_SHFT)
        key = key + ishft(284,EXTN_NB_SHFT) + ishft(ordinal,MAGC_NB_SHFT)
        ! Set creation ordinal
        directory(cur_page)%entry(cur_entry)%a%key = key
        ! CODE POSSIBLY MISSING HERE FOR FLAGS SETTINGS
        allocate(gmm_ptrs_284(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p(&
                                                            &siz(1)%low:siz(1)%high&

                                                            &,siz(2)%low:siz(2)%high,&


                                                            &,siz(3)%low:siz(3)%high,&


                                                            &,siz(4)%low:siz(4)%high&

                                                        ))
        ! Read array
        read(file_unit)gmm_ptrs_284(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p
        directory(cur_page)%entry(cur_entry)%array_addr = get_address_from(&
            gmm_ptrs_284(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p)

        if (gmm_verbose_level == GMM_MSG_DEBUG) then
            write(6,'(a,a8,a,i4,a,i4,a,i4,a,i10)') 'name=',directory(cur_page)%entry(cur_entry)%name,' cur_page=',cur_page,' cur_entry=',cur_entry,' index=',directory(cur_page)%entry(cur_entry)%pointer_table_index,' addr=',get_address_from(gmm_ptrs_284(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p)
        endif
        ier = add_table_entry(gmm_ptrs_284(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p,key)
    else
        ! Write to checkpoint file (records with FLAG_RSTR property)
        if (gmm_verbose_level == GMM_MSG_DEBUG) then
            print *, 'checkpointing type ', 284
        endif
        do i = 1, table_size
            do j = 1, PAGE_SIZE
                if (iand(GMM_FLAG_RSTR,directory(i)%entry(j)%a%flags) .ne. 0 .and. directory(i)%entry(j)%data_type == 284) then
                    if (gmm_verbose_level == GMM_MSG_DEBUG) then
                        print *, 'writing field ', directory(i)%entry(j)%name
                    endif
                    write(file_unit)284
                    write(file_unit)directory(i)%entry(j)%name
                    write(file_unit)directory(i)%entry(j)%l(1:4)
                    attrib = directory(i)%entry(j)%a
                    attrib%flags = iand(attrib%flags,FLAGS_KEPT_IN_RESTART)
                    write(file_unit)attrib
                    write(file_unit)directory(i)%entry(j)%data_type
                    write(file_unit)gmm_ptrs_284(directory(i)%entry(j)%pointer_table_index)%p
                endif
            enddo
        enddo
    endif
end subroutine gmm_checkpoint_284
# 164 "gmm.F90" 2

# 1 "gmm_get_tmpl.F90" 1
integer function gmm_get_284(iname, p, m)
    use gmm_internals
    use pointer_table_data__284
    implicit none
    integer :: i, array_rank
    ! name (partially redundant with attributes)
    character(len=*), intent(in) :: iname







    real*8, pointer  :: p(:,:,:,:)

    ! attributes (name in attributes is not used)
    type(gmm_metadata), optional, intent(out) :: m
    !  integer,intent(inout) :: reqid
    include 'gmm_directory_interface.inc'
    type(gmm_metadata) :: m2
    integer*8 :: key
    integer *8 get_address_from
    external get_address_from

    key = 0
    call check_directory_entry(iname, key)
    if (cur_page .eq. 0 .or. cur_entry .eq. 0) then
        ! quick check using key was not successful
        call find_directory_entry(iname,key)
    endif
    if(cur_page .eq. 0 .or. cur_entry .eq. 0) then
        ! return null entry
        if (present(m)) then
            m%a = GMM_NULL_ATTRIB
            m%l = GMM_NULL_LAYOUT
        endif
        nullify(p)
        key = GMM_KEY_NOT_FOUND
        gmm_get_284 = GMM_VAR_NOT_FOUND
    else
        m2%l = directory(cur_page)%entry(cur_entry)%l
        m2%a = directory(cur_page)%entry(cur_entry)%a
        if (present(m)) then
            ! return a copy of the proper entry
            m = m2
        endif
        p => gmm_ptrs_284(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p
        do i = 1, 4
            if (m2%l(i)%n /= 0) then
                array_rank = i
            endif
        enddo
        if (array_rank /= 4) then
            nullify(p)
            if (present(m)) then
                m = GMM_NULL_METADATA
            endif
            gmm_get_284 = GMM_INCONSISTENT_DIMS
        else
            gmm_get_284 = GMM_OK
        endif
    endif
end function gmm_get_284


subroutine gmm_dealloc_ptr_284()
    use gmm_internals
    use pointer_table_data__284
    implicit none

    deallocate (gmm_ptrs_284(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p)
    nullify    (gmm_ptrs_284(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p)
end subroutine gmm_dealloc_ptr_284
# 165 "gmm.F90" 2

# 1 "undefiner.hf" 1
# 166 "gmm.F90" 2




! Complex functions:




! 381




# 1 "gmm_checkpoint_tmpl.F90" 1
subroutine gmm_checkpoint_381(read_or_write)
    use gmm_internals
    use pointer_table_data_381
    implicit none
    logical read_or_write
    integer istat, fnom, i, j, ier, lcl_pti
    type(gmm_layout), dimension(1:1) :: siz
    type(gmm_attributes) :: attrib
    integer *8 :: key
    external fnom
    integer *8 get_address_from
    external get_address_from

    ! Read one  record from checkpoint file
    if (read_or_write) then
        ! Into next directory entry
        call add_directory_entry
        read(file_unit)directory(cur_page)%entry(cur_entry)%name
        ! Read layout
        read(file_unit)siz(1:1)
        ! Set layout in table
        directory(cur_page)%entry(cur_entry)%l(1:1) = siz(1:1)
        ! Read attributes
        read(file_unit)attrib
        attrib%flags = ior(attrib%flags,GMM_FLAG_READ)
        ! Set attributes in table
        directory(cur_page)%entry(cur_entry)%a = attrib
        read(file_unit)directory(cur_page)%entry(cur_entry)%data_type
        lcl_pti = lgmm_get_nxt_avail_ptr()
        directory(cur_page)%entry(cur_entry)%pointer_table_index = lcl_pti
        ordinal = ordinal + 1
        key = ishft((cur_page-1),PAGE_NB_SHFT) + ishft((cur_entry-1),NTRY_NB_SHFT)
        key = key + ishft(381,EXTN_NB_SHFT) + ishft(ordinal,MAGC_NB_SHFT)
        ! Set creation ordinal
        directory(cur_page)%entry(cur_entry)%a%key = key
        ! CODE POSSIBLY MISSING HERE FOR FLAGS SETTINGS
        allocate(gmm_ptrs_381(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p(&
                                                            &siz(1)%low:siz(1)%high&
# 48 "gmm_checkpoint_tmpl.F90"
                                                        ))
        ! Read array
        read(file_unit)gmm_ptrs_381(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p
        directory(cur_page)%entry(cur_entry)%array_addr = get_address_from(&
            gmm_ptrs_381(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p)

        if (gmm_verbose_level == GMM_MSG_DEBUG) then
            write(6,'(a,a8,a,i4,a,i4,a,i4,a,i10)') 'name=',directory(cur_page)%entry(cur_entry)%name,' cur_page=',cur_page,' cur_entry=',cur_entry,' index=',directory(cur_page)%entry(cur_entry)%pointer_table_index,' addr=',get_address_from(gmm_ptrs_381(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p)
        endif
        ier = add_table_entry(gmm_ptrs_381(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p,key)
    else
        ! Write to checkpoint file (records with FLAG_RSTR property)
        if (gmm_verbose_level == GMM_MSG_DEBUG) then
            print *, 'checkpointing type ', 381
        endif
        do i = 1, table_size
            do j = 1, PAGE_SIZE
                if (iand(GMM_FLAG_RSTR,directory(i)%entry(j)%a%flags) .ne. 0 .and. directory(i)%entry(j)%data_type == 381) then
                    if (gmm_verbose_level == GMM_MSG_DEBUG) then
                        print *, 'writing field ', directory(i)%entry(j)%name
                    endif
                    write(file_unit)381
                    write(file_unit)directory(i)%entry(j)%name
                    write(file_unit)directory(i)%entry(j)%l(1:1)
                    attrib = directory(i)%entry(j)%a
                    attrib%flags = iand(attrib%flags,FLAGS_KEPT_IN_RESTART)
                    write(file_unit)attrib
                    write(file_unit)directory(i)%entry(j)%data_type
                    write(file_unit)gmm_ptrs_381(directory(i)%entry(j)%pointer_table_index)%p
                endif
            enddo
        enddo
    endif
end subroutine gmm_checkpoint_381
# 180 "gmm.F90" 2

# 1 "gmm_get_tmpl.F90" 1
integer function gmm_get_381(iname, p, m)
    use gmm_internals
    use pointer_table_data__381
    implicit none
    integer :: i, array_rank
    ! name (partially redundant with attributes)
    character(len=*), intent(in) :: iname

    complex*8, pointer  :: p(:)







    ! attributes (name in attributes is not used)
    type(gmm_metadata), optional, intent(out) :: m
    !  integer,intent(inout) :: reqid
    include 'gmm_directory_interface.inc'
    type(gmm_metadata) :: m2
    integer*8 :: key
    integer *8 get_address_from
    external get_address_from

    key = 0
    call check_directory_entry(iname, key)
    if (cur_page .eq. 0 .or. cur_entry .eq. 0) then
        ! quick check using key was not successful
        call find_directory_entry(iname,key)
    endif
    if(cur_page .eq. 0 .or. cur_entry .eq. 0) then
        ! return null entry
        if (present(m)) then
            m%a = GMM_NULL_ATTRIB
            m%l = GMM_NULL_LAYOUT
        endif
        nullify(p)
        key = GMM_KEY_NOT_FOUND
        gmm_get_381 = GMM_VAR_NOT_FOUND
    else
        m2%l = directory(cur_page)%entry(cur_entry)%l
        m2%a = directory(cur_page)%entry(cur_entry)%a
        if (present(m)) then
            ! return a copy of the proper entry
            m = m2
        endif
        p => gmm_ptrs_381(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p
        do i = 1, 4
            if (m2%l(i)%n /= 0) then
                array_rank = i
            endif
        enddo
        if (array_rank /= 1) then
            nullify(p)
            if (present(m)) then
                m = GMM_NULL_METADATA
            endif
            gmm_get_381 = GMM_INCONSISTENT_DIMS
        else
            gmm_get_381 = GMM_OK
        endif
    endif
end function gmm_get_381


subroutine gmm_dealloc_ptr_381()
    use gmm_internals
    use pointer_table_data__381
    implicit none

    deallocate (gmm_ptrs_381(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p)
    nullify    (gmm_ptrs_381(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p)
end subroutine gmm_dealloc_ptr_381
# 181 "gmm.F90" 2

# 1 "undefiner.hf" 1
# 182 "gmm.F90" 2

! 382




# 1 "gmm_checkpoint_tmpl.F90" 1
subroutine gmm_checkpoint_382(read_or_write)
    use gmm_internals
    use pointer_table_data_382
    implicit none
    logical read_or_write
    integer istat, fnom, i, j, ier, lcl_pti
    type(gmm_layout), dimension(1:2) :: siz
    type(gmm_attributes) :: attrib
    integer *8 :: key
    external fnom
    integer *8 get_address_from
    external get_address_from

    ! Read one  record from checkpoint file
    if (read_or_write) then
        ! Into next directory entry
        call add_directory_entry
        read(file_unit)directory(cur_page)%entry(cur_entry)%name
        ! Read layout
        read(file_unit)siz(1:2)
        ! Set layout in table
        directory(cur_page)%entry(cur_entry)%l(1:2) = siz(1:2)
        ! Read attributes
        read(file_unit)attrib
        attrib%flags = ior(attrib%flags,GMM_FLAG_READ)
        ! Set attributes in table
        directory(cur_page)%entry(cur_entry)%a = attrib
        read(file_unit)directory(cur_page)%entry(cur_entry)%data_type
        lcl_pti = lgmm_get_nxt_avail_ptr()
        directory(cur_page)%entry(cur_entry)%pointer_table_index = lcl_pti
        ordinal = ordinal + 1
        key = ishft((cur_page-1),PAGE_NB_SHFT) + ishft((cur_entry-1),NTRY_NB_SHFT)
        key = key + ishft(382,EXTN_NB_SHFT) + ishft(ordinal,MAGC_NB_SHFT)
        ! Set creation ordinal
        directory(cur_page)%entry(cur_entry)%a%key = key
        ! CODE POSSIBLY MISSING HERE FOR FLAGS SETTINGS
        allocate(gmm_ptrs_382(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p(&
                                                            &siz(1)%low:siz(1)%high&

                                                            &,siz(2)%low:siz(2)%high,&







                                                        ))
        ! Read array
        read(file_unit)gmm_ptrs_382(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p
        directory(cur_page)%entry(cur_entry)%array_addr = get_address_from(&
            gmm_ptrs_382(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p)

        if (gmm_verbose_level == GMM_MSG_DEBUG) then
            write(6,'(a,a8,a,i4,a,i4,a,i4,a,i10)') 'name=',directory(cur_page)%entry(cur_entry)%name,' cur_page=',cur_page,' cur_entry=',cur_entry,' index=',directory(cur_page)%entry(cur_entry)%pointer_table_index,' addr=',get_address_from(gmm_ptrs_382(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p)
        endif
        ier = add_table_entry(gmm_ptrs_382(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p,key)
    else
        ! Write to checkpoint file (records with FLAG_RSTR property)
        if (gmm_verbose_level == GMM_MSG_DEBUG) then
            print *, 'checkpointing type ', 382
        endif
        do i = 1, table_size
            do j = 1, PAGE_SIZE
                if (iand(GMM_FLAG_RSTR,directory(i)%entry(j)%a%flags) .ne. 0 .and. directory(i)%entry(j)%data_type == 382) then
                    if (gmm_verbose_level == GMM_MSG_DEBUG) then
                        print *, 'writing field ', directory(i)%entry(j)%name
                    endif
                    write(file_unit)382
                    write(file_unit)directory(i)%entry(j)%name
                    write(file_unit)directory(i)%entry(j)%l(1:2)
                    attrib = directory(i)%entry(j)%a
                    attrib%flags = iand(attrib%flags,FLAGS_KEPT_IN_RESTART)
                    write(file_unit)attrib
                    write(file_unit)directory(i)%entry(j)%data_type
                    write(file_unit)gmm_ptrs_382(directory(i)%entry(j)%pointer_table_index)%p
                endif
            enddo
        enddo
    endif
end subroutine gmm_checkpoint_382
# 188 "gmm.F90" 2

# 1 "gmm_get_tmpl.F90" 1
integer function gmm_get_382(iname, p, m)
    use gmm_internals
    use pointer_table_data__382
    implicit none
    integer :: i, array_rank
    ! name (partially redundant with attributes)
    character(len=*), intent(in) :: iname



    complex*8, pointer  :: p(:,:)





    ! attributes (name in attributes is not used)
    type(gmm_metadata), optional, intent(out) :: m
    !  integer,intent(inout) :: reqid
    include 'gmm_directory_interface.inc'
    type(gmm_metadata) :: m2
    integer*8 :: key
    integer *8 get_address_from
    external get_address_from

    key = 0
    call check_directory_entry(iname, key)
    if (cur_page .eq. 0 .or. cur_entry .eq. 0) then
        ! quick check using key was not successful
        call find_directory_entry(iname,key)
    endif
    if(cur_page .eq. 0 .or. cur_entry .eq. 0) then
        ! return null entry
        if (present(m)) then
            m%a = GMM_NULL_ATTRIB
            m%l = GMM_NULL_LAYOUT
        endif
        nullify(p)
        key = GMM_KEY_NOT_FOUND
        gmm_get_382 = GMM_VAR_NOT_FOUND
    else
        m2%l = directory(cur_page)%entry(cur_entry)%l
        m2%a = directory(cur_page)%entry(cur_entry)%a
        if (present(m)) then
            ! return a copy of the proper entry
            m = m2
        endif
        p => gmm_ptrs_382(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p
        do i = 1, 4
            if (m2%l(i)%n /= 0) then
                array_rank = i
            endif
        enddo
        if (array_rank /= 2) then
            nullify(p)
            if (present(m)) then
                m = GMM_NULL_METADATA
            endif
            gmm_get_382 = GMM_INCONSISTENT_DIMS
        else
            gmm_get_382 = GMM_OK
        endif
    endif
end function gmm_get_382


subroutine gmm_dealloc_ptr_382()
    use gmm_internals
    use pointer_table_data__382
    implicit none

    deallocate (gmm_ptrs_382(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p)
    nullify    (gmm_ptrs_382(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p)
end subroutine gmm_dealloc_ptr_382
# 189 "gmm.F90" 2

# 1 "undefiner.hf" 1
# 190 "gmm.F90" 2

! 383




# 1 "gmm_checkpoint_tmpl.F90" 1
subroutine gmm_checkpoint_383(read_or_write)
    use gmm_internals
    use pointer_table_data_383
    implicit none
    logical read_or_write
    integer istat, fnom, i, j, ier, lcl_pti
    type(gmm_layout), dimension(1:3) :: siz
    type(gmm_attributes) :: attrib
    integer *8 :: key
    external fnom
    integer *8 get_address_from
    external get_address_from

    ! Read one  record from checkpoint file
    if (read_or_write) then
        ! Into next directory entry
        call add_directory_entry
        read(file_unit)directory(cur_page)%entry(cur_entry)%name
        ! Read layout
        read(file_unit)siz(1:3)
        ! Set layout in table
        directory(cur_page)%entry(cur_entry)%l(1:3) = siz(1:3)
        ! Read attributes
        read(file_unit)attrib
        attrib%flags = ior(attrib%flags,GMM_FLAG_READ)
        ! Set attributes in table
        directory(cur_page)%entry(cur_entry)%a = attrib
        read(file_unit)directory(cur_page)%entry(cur_entry)%data_type
        lcl_pti = lgmm_get_nxt_avail_ptr()
        directory(cur_page)%entry(cur_entry)%pointer_table_index = lcl_pti
        ordinal = ordinal + 1
        key = ishft((cur_page-1),PAGE_NB_SHFT) + ishft((cur_entry-1),NTRY_NB_SHFT)
        key = key + ishft(383,EXTN_NB_SHFT) + ishft(ordinal,MAGC_NB_SHFT)
        ! Set creation ordinal
        directory(cur_page)%entry(cur_entry)%a%key = key
        ! CODE POSSIBLY MISSING HERE FOR FLAGS SETTINGS
        allocate(gmm_ptrs_383(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p(&
                                                            &siz(1)%low:siz(1)%high&

                                                            &,siz(2)%low:siz(2)%high,&


                                                            &,siz(3)%low:siz(3)%high,&




                                                        ))
        ! Read array
        read(file_unit)gmm_ptrs_383(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p
        directory(cur_page)%entry(cur_entry)%array_addr = get_address_from(&
            gmm_ptrs_383(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p)

        if (gmm_verbose_level == GMM_MSG_DEBUG) then
            write(6,'(a,a8,a,i4,a,i4,a,i4,a,i10)') 'name=',directory(cur_page)%entry(cur_entry)%name,' cur_page=',cur_page,' cur_entry=',cur_entry,' index=',directory(cur_page)%entry(cur_entry)%pointer_table_index,' addr=',get_address_from(gmm_ptrs_383(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p)
        endif
        ier = add_table_entry(gmm_ptrs_383(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p,key)
    else
        ! Write to checkpoint file (records with FLAG_RSTR property)
        if (gmm_verbose_level == GMM_MSG_DEBUG) then
            print *, 'checkpointing type ', 383
        endif
        do i = 1, table_size
            do j = 1, PAGE_SIZE
                if (iand(GMM_FLAG_RSTR,directory(i)%entry(j)%a%flags) .ne. 0 .and. directory(i)%entry(j)%data_type == 383) then
                    if (gmm_verbose_level == GMM_MSG_DEBUG) then
                        print *, 'writing field ', directory(i)%entry(j)%name
                    endif
                    write(file_unit)383
                    write(file_unit)directory(i)%entry(j)%name
                    write(file_unit)directory(i)%entry(j)%l(1:3)
                    attrib = directory(i)%entry(j)%a
                    attrib%flags = iand(attrib%flags,FLAGS_KEPT_IN_RESTART)
                    write(file_unit)attrib
                    write(file_unit)directory(i)%entry(j)%data_type
                    write(file_unit)gmm_ptrs_383(directory(i)%entry(j)%pointer_table_index)%p
                endif
            enddo
        enddo
    endif
end subroutine gmm_checkpoint_383
# 196 "gmm.F90" 2

# 1 "gmm_get_tmpl.F90" 1
integer function gmm_get_383(iname, p, m)
    use gmm_internals
    use pointer_table_data__383
    implicit none
    integer :: i, array_rank
    ! name (partially redundant with attributes)
    character(len=*), intent(in) :: iname





    complex*8, pointer  :: p(:,:,:)



    ! attributes (name in attributes is not used)
    type(gmm_metadata), optional, intent(out) :: m
    !  integer,intent(inout) :: reqid
    include 'gmm_directory_interface.inc'
    type(gmm_metadata) :: m2
    integer*8 :: key
    integer *8 get_address_from
    external get_address_from

    key = 0
    call check_directory_entry(iname, key)
    if (cur_page .eq. 0 .or. cur_entry .eq. 0) then
        ! quick check using key was not successful
        call find_directory_entry(iname,key)
    endif
    if(cur_page .eq. 0 .or. cur_entry .eq. 0) then
        ! return null entry
        if (present(m)) then
            m%a = GMM_NULL_ATTRIB
            m%l = GMM_NULL_LAYOUT
        endif
        nullify(p)
        key = GMM_KEY_NOT_FOUND
        gmm_get_383 = GMM_VAR_NOT_FOUND
    else
        m2%l = directory(cur_page)%entry(cur_entry)%l
        m2%a = directory(cur_page)%entry(cur_entry)%a
        if (present(m)) then
            ! return a copy of the proper entry
            m = m2
        endif
        p => gmm_ptrs_383(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p
        do i = 1, 4
            if (m2%l(i)%n /= 0) then
                array_rank = i
            endif
        enddo
        if (array_rank /= 3) then
            nullify(p)
            if (present(m)) then
                m = GMM_NULL_METADATA
            endif
            gmm_get_383 = GMM_INCONSISTENT_DIMS
        else
            gmm_get_383 = GMM_OK
        endif
    endif
end function gmm_get_383


subroutine gmm_dealloc_ptr_383()
    use gmm_internals
    use pointer_table_data__383
    implicit none

    deallocate (gmm_ptrs_383(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p)
    nullify    (gmm_ptrs_383(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p)
end subroutine gmm_dealloc_ptr_383
# 197 "gmm.F90" 2

# 1 "undefiner.hf" 1
# 198 "gmm.F90" 2

! 384




# 1 "gmm_checkpoint_tmpl.F90" 1
subroutine gmm_checkpoint_384(read_or_write)
    use gmm_internals
    use pointer_table_data_384
    implicit none
    logical read_or_write
    integer istat, fnom, i, j, ier, lcl_pti
    type(gmm_layout), dimension(1:4) :: siz
    type(gmm_attributes) :: attrib
    integer *8 :: key
    external fnom
    integer *8 get_address_from
    external get_address_from

    ! Read one  record from checkpoint file
    if (read_or_write) then
        ! Into next directory entry
        call add_directory_entry
        read(file_unit)directory(cur_page)%entry(cur_entry)%name
        ! Read layout
        read(file_unit)siz(1:4)
        ! Set layout in table
        directory(cur_page)%entry(cur_entry)%l(1:4) = siz(1:4)
        ! Read attributes
        read(file_unit)attrib
        attrib%flags = ior(attrib%flags,GMM_FLAG_READ)
        ! Set attributes in table
        directory(cur_page)%entry(cur_entry)%a = attrib
        read(file_unit)directory(cur_page)%entry(cur_entry)%data_type
        lcl_pti = lgmm_get_nxt_avail_ptr()
        directory(cur_page)%entry(cur_entry)%pointer_table_index = lcl_pti
        ordinal = ordinal + 1
        key = ishft((cur_page-1),PAGE_NB_SHFT) + ishft((cur_entry-1),NTRY_NB_SHFT)
        key = key + ishft(384,EXTN_NB_SHFT) + ishft(ordinal,MAGC_NB_SHFT)
        ! Set creation ordinal
        directory(cur_page)%entry(cur_entry)%a%key = key
        ! CODE POSSIBLY MISSING HERE FOR FLAGS SETTINGS
        allocate(gmm_ptrs_384(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p(&
                                                            &siz(1)%low:siz(1)%high&

                                                            &,siz(2)%low:siz(2)%high,&


                                                            &,siz(3)%low:siz(3)%high,&


                                                            &,siz(4)%low:siz(4)%high&

                                                        ))
        ! Read array
        read(file_unit)gmm_ptrs_384(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p
        directory(cur_page)%entry(cur_entry)%array_addr = get_address_from(&
            gmm_ptrs_384(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p)

        if (gmm_verbose_level == GMM_MSG_DEBUG) then
            write(6,'(a,a8,a,i4,a,i4,a,i4,a,i10)') 'name=',directory(cur_page)%entry(cur_entry)%name,' cur_page=',cur_page,' cur_entry=',cur_entry,' index=',directory(cur_page)%entry(cur_entry)%pointer_table_index,' addr=',get_address_from(gmm_ptrs_384(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p)
        endif
        ier = add_table_entry(gmm_ptrs_384(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p,key)
    else
        ! Write to checkpoint file (records with FLAG_RSTR property)
        if (gmm_verbose_level == GMM_MSG_DEBUG) then
            print *, 'checkpointing type ', 384
        endif
        do i = 1, table_size
            do j = 1, PAGE_SIZE
                if (iand(GMM_FLAG_RSTR,directory(i)%entry(j)%a%flags) .ne. 0 .and. directory(i)%entry(j)%data_type == 384) then
                    if (gmm_verbose_level == GMM_MSG_DEBUG) then
                        print *, 'writing field ', directory(i)%entry(j)%name
                    endif
                    write(file_unit)384
                    write(file_unit)directory(i)%entry(j)%name
                    write(file_unit)directory(i)%entry(j)%l(1:4)
                    attrib = directory(i)%entry(j)%a
                    attrib%flags = iand(attrib%flags,FLAGS_KEPT_IN_RESTART)
                    write(file_unit)attrib
                    write(file_unit)directory(i)%entry(j)%data_type
                    write(file_unit)gmm_ptrs_384(directory(i)%entry(j)%pointer_table_index)%p
                endif
            enddo
        enddo
    endif
end subroutine gmm_checkpoint_384
# 204 "gmm.F90" 2

# 1 "gmm_get_tmpl.F90" 1
integer function gmm_get_384(iname, p, m)
    use gmm_internals
    use pointer_table_data__384
    implicit none
    integer :: i, array_rank
    ! name (partially redundant with attributes)
    character(len=*), intent(in) :: iname







    complex*8, pointer  :: p(:,:,:,:)

    ! attributes (name in attributes is not used)
    type(gmm_metadata), optional, intent(out) :: m
    !  integer,intent(inout) :: reqid
    include 'gmm_directory_interface.inc'
    type(gmm_metadata) :: m2
    integer*8 :: key
    integer *8 get_address_from
    external get_address_from

    key = 0
    call check_directory_entry(iname, key)
    if (cur_page .eq. 0 .or. cur_entry .eq. 0) then
        ! quick check using key was not successful
        call find_directory_entry(iname,key)
    endif
    if(cur_page .eq. 0 .or. cur_entry .eq. 0) then
        ! return null entry
        if (present(m)) then
            m%a = GMM_NULL_ATTRIB
            m%l = GMM_NULL_LAYOUT
        endif
        nullify(p)
        key = GMM_KEY_NOT_FOUND
        gmm_get_384 = GMM_VAR_NOT_FOUND
    else
        m2%l = directory(cur_page)%entry(cur_entry)%l
        m2%a = directory(cur_page)%entry(cur_entry)%a
        if (present(m)) then
            ! return a copy of the proper entry
            m = m2
        endif
        p => gmm_ptrs_384(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p
        do i = 1, 4
            if (m2%l(i)%n /= 0) then
                array_rank = i
            endif
        enddo
        if (array_rank /= 4) then
            nullify(p)
            if (present(m)) then
                m = GMM_NULL_METADATA
            endif
            gmm_get_384 = GMM_INCONSISTENT_DIMS
        else
            gmm_get_384 = GMM_OK
        endif
    endif
end function gmm_get_384


subroutine gmm_dealloc_ptr_384()
    use gmm_internals
    use pointer_table_data__384
    implicit none

    deallocate (gmm_ptrs_384(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p)
    nullify    (gmm_ptrs_384(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p)
end subroutine gmm_dealloc_ptr_384
# 205 "gmm.F90" 2

# 1 "undefiner.hf" 1
# 206 "gmm.F90" 2

