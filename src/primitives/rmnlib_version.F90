!> Return the version identification string
subroutine rmnlib_version(version, prnt)
    implicit none

    character(len = *), intent(out) :: version
    logical, intent(in) :: prnt

#ifndef EC_ARCH
#define EC_ARCH ""
#endif

    version = "  RMNLIB  -  Release: " // VERSION // " " // EC_ARCH

    if (prnt) then 
        print *, version
    end if
end subroutine