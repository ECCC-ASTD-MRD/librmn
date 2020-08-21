interface
    integer function gmm_getmeta2(iname, meta)
        include "gmm_definitions.inc"

        ! name (partially redundant with attributes)
        character(len=*), intent(in) :: iname
        ! attributes (name in attributes is not used)
        type(gmm_metadata), intent(out) :: meta
    end function gmm_getmeta2
end interface
