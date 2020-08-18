integer function gmm_delete(iname)
    use gmm_internals
    implicit none

    character(len = *), intent(in) :: iname
    include 'gmm_directory_interface.inc'
    integer*8 :: key
    integer :: datatype

    key = 0
    call check_directory_entry(iname, key)
    if (cur_page .eq. 0 .or. cur_entry .eq. 0) then
        ! Quick check using key was not successful
        call find_directory_entry(iname, key)
    endif
    if (cur_page .eq. 0 .or. cur_entry .eq. 0) then
        ! Variable not found
        key= GMM_KEY_NOT_FOUND
        gmm_delete = GMM_VAR_NOT_FOUND
    else
        datatype = directory(cur_page)%entry(cur_entry)%data_type
        directory(cur_page)%entry(cur_entry)%name = 'Variable deleted upon request'
        dtype: select case (datatype)
            case (184)
                call gmm_dealloc_ptr184()
            case (144)
                call gmm_dealloc_ptr144()
            case (284)
                call gmm_dealloc_ptr284()
            case (244)
                call gmm_dealloc_ptr244()
            case (384)
                call gmm_dealloc_ptr384()
            case (183)
                call gmm_dealloc_ptr183()
            case (143)
                call gmm_dealloc_ptr143()
            case (283)
                call gmm_dealloc_ptr283()
            case (243)
                call gmm_dealloc_ptr243()
            case (383)
                call gmm_dealloc_ptr383()
            case (182)
                call gmm_dealloc_ptr182()
            case (142)
                call gmm_dealloc_ptr142()
            case (282)
                call gmm_dealloc_ptr282()
            case (242)
                call gmm_dealloc_ptr242()
            case (382)
                call gmm_dealloc_ptr382()
            case (181)
                call gmm_dealloc_ptr181()
            case (141)
                call gmm_dealloc_ptr141()
            case (281)
                call gmm_dealloc_ptr281()
            case (241)
                call gmm_dealloc_ptr241()
            case (381)
                call gmm_dealloc_ptr381()
        end select dtype
        directory(cur_page)%entry(cur_entry)%l = GMM_NULL_LAYOUT
        directory(cur_page)%entry(cur_entry)%a = GMM_NULL_ATTRIB
        gmm_delete = GMM_OK
    endif
end function gmm_delete
