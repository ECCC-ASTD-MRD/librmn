interface
    integer function gmm_getmeta(varname, meta)
        include "gmm_definitions.inc"
        character(len = *), intent(in) :: varname
        type(gmm_metadata), intent(out) :: meta
    end function gmm_getmeta


    integer function gmm_getmeta2(iname, m)
        include "gmm_definitions.inc"

        ! name (partially redundant with attributes)
        character(len=*), intent(in) :: iname
        ! attributes (name in attributes is not used)
        type(gmm_metadata), intent(out) :: m
    end function gmm_getmeta2
end interface
