integer function FNCNAME(gmm_create)(iname, p, field_meta, flags_arg)
    use gmm_internals
    use FNCNAME(pointer_table_data_)
    implicit none
    ! name (partially redundant with attributes)
    character(len=*), intent(in) :: iname
#if DIM == 1
    DATATYPE*DATALENGTH, pointer  :: p(:)
#elif DIM == 2
    DATATYPE*DATALENGTH, pointer  :: p(:,:)
#elif DIM == 3
    DATATYPE*DATALENGTH, pointer  :: p(:,:,:)
#elif DIM == 4
    DATATYPE*DATALENGTH, pointer  :: p(:,:,:,:)
#endif
    ! attributes (name in attributes is not used)
    type(gmm_metadata), intent(inout) :: field_meta
    integer, intent(in), optional :: flags_arg
    integer *8 get_address_from
    external get_address_from
    external fool_optimizer


    include "gmm_directory_interface.inc"

    type(gmm_attributes) :: localattr, attrs
    type (gmm_attributes) lcl_attr
    type(gmm_layout), dimension(4) :: lcl_layout, dims
    integer lcl_datatype
    ! fast lookup key
    integer*8 :: key
    logical consistent
    integer i, ier
    integer lcl_pti
    ! name (partially redundant with attributes)
    character(len=GMM_MAXNAMELENGTH) :: lcl_name
    integer u_bound, l_bound

#if DIM == 1
    DATATYPE*DATALENGTH, pointer  :: pp(:)
#elif DIM == 2
    DATATYPE*DATALENGTH, pointer  :: pp(:,:)
#elif DIM == 3
    DATATYPE*DATALENGTH, pointer  :: pp(:,:,:)
#elif DIM == 4
    DATATYPE*DATALENGTH, pointer  :: pp(:,:,:,:)
#endif
    real      :: NaN
    integer   :: inan
    real*8    :: NaN8
    integer*8 :: inan8

    equivalence (inan, NaN)
    equivalence (inan8, NaN8)

    data inan  /Z'7F800001'/
    data inan8 /Z'7FF0000000000001'/

    if (present(flags_arg)) then
        field_meta%a%flags = flags_arg
    endif
    lcl_layout = field_meta%l
    dims = lcl_layout
    lcl_attr   = field_meta%a
    attrs = lcl_attr
    lcl_datatype = EXTENSION


   ! 1- Verify that the pointer and the dims are consistent
   if (associated(p)) then
        consistent = .true.
        ! check that all dimensions are identical
        do i = 1, DIM
            consistent = consistent .and. size(p, i) .eq. (dims(i)%high - dims(i)%low + 1)
        enddo
        if (.not. consistent ) then
            if (gmm_verbose_level == GMM_MSG_DEBUG) then
                print *,'ERROR: gmm_create, p has dimensions that are not consistent with dims'
            endif
            key = 0
            FNCNAME(gmm_create) = GMM_INCONSISTENT_DIMS
            ! HOW SERIOUS AN ERROR IS THIS ?
            return
        endif
   endif
   ! p is null or is consistent with dims

    ! copy of user specified attributes
    localattr = attrs
    lcl_name = trim(iname)
    ! keep only a subset of user specified flags
    localattr%flags = iand(localattr%flags, FLAGS_KEPT_ON_CREATE)
#ifdef DEBUG_MODE
    print *, 'lcl_name="', lcl_name, '" has ', DIM, ' dimensions'
    do i = 1, DIM
        print *, i, dims(i)
    enddo
#endif
    ! is there a field with this name that exists ?
    call find_directory_entry(lcl_name, key)     
#ifdef DEBUG_MODE
    print *, 'after find_directory: cur_page, cur_entry=', cur_page,cur_entry
#endif

    ! 2- Verify if the field has already been created
    ! create may have already been called for this array
    if (cur_page .ne. 0 .and. cur_entry .ne. 0) then
        if (associated(p)) then
            print *,'ERROR: gmm_create called with existing p and array has already been created'
            key = 0
            FNCNAME(gmm_create) = GMM_ARRAY_ALREADY_EXISTS
            ! HOW SERIOUS AN ERROR IS THIS ?
            return
            endif
        pp => FNCNAME(gmm_ptrs)(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p
        consistent = .true.
        ! check that all dimensions are identical
        do i = 1, DIM
            ! work around to prevent the corruption of size(pp) by the pgi9xx optimizer (bug)
            call fool_optimizer(size(pp,i))
            consistent = consistent .and. (size(pp, i) .eq. (dims(i)%high - dims(i)%low + 1))
            if (.not. consistent ) then
                print *, 'size(pp,', i, ')=', size(pp, i), ' high=', dims(i)%high, ' low=', dims(i)%low
            endif
        enddo
        if (.not. consistent ) then
            print *, 'ERROR: gmm_create, requested dimensions differ from previous specification (restart/create)'
            print *, 'ERROR: gmm_create, variable name ="', lcl_name,'"'
            key = 0
            nullify(p)
            FNCNAME(gmm_create) = GMM_INCONSISTENT_DIMS
            ! HOW SERIOUS AN ERROR IS THIS ?
            return
        else
            if (gmm_verbose_level == GMM_MSG_DEBUG) then
                print *, 'INFO: gmm_create, variable name =', lcl_name, ' exists and is consistent'
            endif
        endif

        if (iand(GMM_FLAG_CRTD, directory(cur_page)%entry(cur_entry)%a%flags) .ne. 0) then
            ! OOPS really a double create
            print *, 'ERROR: gmm_create, field ', lcl_name, ' has already been created'
            key = 0
            nullify(p)
            FNCNAME(gmm_create) = GMM_VARIABLE_ALREADY_CREATED
            return
        else
            ! no, this array must then have been read from a restart file
            ! keep flags from restart file
            localattr%flags = ior(localattr%flags, directory(cur_page)%entry(cur_entry)%a%flags)
            ! get proper key value from directory
            key = directory(cur_page)%entry(cur_entry)%a%key
            ! turn on created flag
            directory(cur_page)%entry(cur_entry)%a%flags = ior(localattr%flags, GMM_FLAG_CRTD)
            ! point to array
            p => FNCNAME(gmm_ptrs)(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p
            directory(cur_page)%entry(cur_entry)%array_addr = get_address_from(p)
            FNCNAME(gmm_create) = 0
            ! no need to go any further, the array is created
            return
        endif
    else
        ! array not found in table, we may need to create an entry in directory
        if (iand(GMM_FLAG_RSTR, localattr%flags) .ne. 0 .and. restart_mode) then
            ! we are in restart mode and array should exist
            print *, 'ERROR: gmm_create field ', lcl_name, 'should have been read from restart file but was not'
            ! HOW SERIOUS AN ERROR IS THIS ?
        endif
        call add_directory_entry
    endif


    ! 3 - Variable not found in the table - creating it

    ! bump creation ordinal
    ordinal = ordinal + 1
#ifdef DEBUG_MODE
    print *, 'creation ordinal=', ordinal, ' cur_page, cur_entry=', cur_page,cur_entry
#endif
    key = ishft((cur_page - 1), PAGE_NB_SHFT) + ishft((cur_entry - 1), NTRY_NB_SHFT)
    key = key + ishft(EXTENSION, EXTN_NB_SHFT) + ishft(ordinal, MAGC_NB_SHFT)
    ! name, units, initmode, some flags
    directory(cur_page)%entry(cur_entry)%a = localattr
    ! CODE POSSIBLY MISSING HERE FOR FLAGS SETTINGS
    directory(cur_page)%entry(cur_entry)%name = lcl_name
    directory(cur_page)%entry(cur_entry)%a%key = key
    ! turn on created flag
    directory(cur_page)%entry(cur_entry)%a%flags = ior(localattr%flags, GMM_FLAG_CRTD)
    ! establish dimensions and allocate array
    directory(cur_page)%entry(cur_entry)%l(1:DIM) = dims(1:DIM)
    ! establish dimensions and allocate array
    directory(cur_page)%entry(cur_entry)%data_type = EXTENSION

    if (associated(p)) then
        ! the user gave a non null pointer
        if (gmm_verbose_level == GMM_MSG_DEBUG) then
            print *,'GMM_CREATE: using user supplied array'
        endif
        lcl_pti = lgmm_get_nxt_avail_ptr()
        directory(cur_page)%entry(cur_entry)%pointer_table_index = lcl_pti
        ier = add_table_entry(p, key)
        ! must check that certain attributes are not requested (e.g. FLAG_RSTR) and that size is consistent
    else
#ifdef DEBUG_MODE
        print *,'allocating array with number of dimensions=',DIM
#endif
        lcl_pti = lgmm_get_nxt_avail_ptr()
        directory(cur_page)%entry(cur_entry)%pointer_table_index = lcl_pti
        allocate(p(dims(1)%low:dims(1)%high &
#if DIM > 1
                  ,dims(2)%low:dims(2)%high &
#endif
#if DIM > 2
                  ,dims(3)%low:dims(3)%high &
#endif
#if DIM > 3
                  ,dims(4)%low:dims(4)%high &
#endif
                  ), stat = ier)
        if (ier /= 0) then
            FNCNAME(gmm_create) = GMM_ERROR
            return
        endif
        directory(cur_page)%entry(cur_entry)%array_addr = get_address_from(p)
        ier = add_table_entry(p, key)
    endif
    field_meta%l = directory(cur_page)%entry(cur_entry)%l
    field_meta%a = directory(cur_page)%entry(cur_entry)%a

    if (iand(field_meta%a%flags, GMM_FLAG_IZER) /= 0) then
        p = 0
    endif

    if (iand(field_meta%a%flags, GMM_FLAG_INAN) /= 0) then
#if ((DATATYPE == real) & (DATALENGTH == 4))
        if (gmm_verbose_level == GMM_MSG_DEBUG) then
            print *,iname,' Debug DATATYPE=real init to NaN'
        endif
        p = NaN
#else
#if ((DATATYPE == real) & (DATALENGTH == 8))
        if (gmm_verbose_level == GMM_MSG_DEBUG) then
            print *,iname,' Debug DATATYPE=real*8 init to NaN8'
        p = NaN8
#else
        print *,'GMM_CREATE ERROR, name=',iname,' : init to NaN is not available for this data type'
        FNCNAME(gmm_create) = GMM_ERROR
        return
#endif
#endif
    endif

    FNCNAME(gmm_create) = 0
end function FNCNAME(gmm_create)
