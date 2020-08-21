!> Encode/pack type(gmm_metadata) in a basic Fortran type
integer function gmm_encodemeta(meta, output)
    use gmm_internals
    implicit none

    type(gmm_metadata), intent(in) :: meta
    integer, dimension(:), intent(out) :: output

    integer :: i

    if (size(output) < GMM_META_SIZE) then
        gmm_encodemeta = GMM_ERROR
        return
    endif

    output(1:GMM_META_SIZE) = transfer(meta, output)

    gmm_encodemeta = GMM_OK
end function gmm_encodemeta


!> Decode/unpack meta into type(gmm_metadata)
integer function gmm_decodemeta(meta, input)
    use gmm_internals
    implicit none

    type(gmm_metadata), intent(out):: meta
    integer, dimension(:), intent(in) :: input

    integer :: i

    if (size(input) < GMM_META_SIZE) then
       gmm_decodemeta = GMM_ERROR
       return
    endif

    meta = transfer(input(1:GMM_META_SIZE), meta)

    gmm_decodemeta = GMM_OK
end function gmm_decodemeta
