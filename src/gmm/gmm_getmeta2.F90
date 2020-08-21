integer function gmm_getmeta2(iname, meta)
    implicit none

    include "gmm_definitions.inc"
    include "gmm_getmeta_interface.inc"

    ! name (partially redundant with attributes)
    character(len = *), intent(in) :: iname
    ! attributes (name in attributes is not used)
    type(gmm_metadata), intent(out) :: meta

    gmm_getmeta2 = gmm_getmeta(iname, meta)
end function gmm_getmeta2