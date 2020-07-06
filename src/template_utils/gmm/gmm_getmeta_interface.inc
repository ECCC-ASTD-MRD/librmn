interface
    integer function gmm_getmeta(varname, meta)
#include "gmm_definitions.inc"
        character(len=*), intent(in) :: varname
        type(gmm_metadata), intent(out) :: meta
    end function gmm_getmeta
end interface
