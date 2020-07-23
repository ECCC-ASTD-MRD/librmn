interface
    function gmm_updatemeta(iname, F_meta) result(F_istat)
        include "gmm_definitions.inc"
        character(len=*), intent(in) :: iname
        type(gmm_metadata), intent(in) :: F_meta
        ! Status
        integer ::  F_istat
    end function gmm_updatemeta
end interface
