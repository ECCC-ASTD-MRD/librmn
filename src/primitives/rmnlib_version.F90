!> Return the version identification string
#include <rmn_build_info.h>

subroutine rmnlib_version(version_string, prnt)
    implicit none

    character(len = *), intent(out) :: version_string
    logical, intent(in) :: prnt

    version_string = "  RMNLIB  -  Release: " // VERSION // " " // EC_ARCH

    if (prnt) then 
        print *, version_string
    end if
end subroutine