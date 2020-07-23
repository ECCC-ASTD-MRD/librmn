integer function gmm_getmeta(varname, meta)
    use gmm_internals
    implicit none

    character(len = *), intent(in) :: varname
    type(gmm_metadata), intent(out) :: meta

    include 'gmm_directory_interface.inc'

    integer*8 :: key

    key = 0
    call find_directory_entry(varname,key)

    if (key == GMM_KEY_NOT_FOUND) then
        if (gmm_verbose_level <= GMM_MSG_WARN) then
            print *, '(GMM_GETMETA) Variable ', varname, ' not found'
        endif
        gmm_getmeta = GMM_ERROR
        return
    endif

    meta%a = directory(cur_page)%entry(cur_entry)%a
    meta%l = directory(cur_page)%entry(cur_entry)%l
    gmm_getmeta = 0
end function gmm_getmeta


integer function gmm_getmeta2(iname, m)
    implicit none

    include "gmm_definitions.inc"

    ! name (partially redundant with attributes)
    character(len=*), intent(in) :: iname
    ! attributes (name in attributes is not used)
    type(gmm_metadata), intent(out) :: m

    gmm_getmeta2 = gmm_getmeta(iname, m)
end function gmm_getmeta2
