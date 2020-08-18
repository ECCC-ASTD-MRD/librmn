interface
    function gmm_encodemeta(F_meta, F_code) result(F_istat)
        include "gmm_definitions.inc"
        type(gmm_metadata), intent(in) :: F_meta
        integer, intent(out):: F_code(:)
        integer ::  F_istat
    end function gmm_encodemeta

    function gmm_decodemeta(F_meta, F_code) result(F_istat)
        include "gmm_definitions.inc"
        type(gmm_metadata), intent(out):: F_meta
        integer, intent(in) :: F_code(:)
        integer ::  F_istat
    end function gmm_decodemeta
end interface
