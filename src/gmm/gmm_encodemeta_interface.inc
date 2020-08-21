interface
    integer function gmm_encodemeta(meta, output)
        include "gmm_definitions.inc"
        type(gmm_metadata), intent(in) :: meta
        integer, dimension(:), intent(out) :: output
    end function gmm_encodemeta

    integer function gmm_decodemeta(meta, input)
        include "gmm_definitions.inc"
        type(gmm_metadata), intent(out):: meta
        integer, dimension(:), intent(in) :: input
    end function gmm_decodemeta
end interface
