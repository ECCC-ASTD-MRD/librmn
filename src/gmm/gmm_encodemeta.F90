!> Encode/pack type(gmm_metadata) in a basic Fortran type
integer function gmm_encodemeta(F_meta, F_code)
    use gmm_internals
    implicit none

    type(gmm_metadata), intent(in) :: F_meta
    integer, intent(out):: F_code(:)

    integer :: i

    if (size(F_code) < GMM_META_SIZE) then
        F_istat = GMM_ERROR
        return
    endif

    do i = 1, GMM_META_SIZE
        F_code(i) = F_meta(i)
    end do

    gmm_encodemeta = GMM_OK
end function gmm_encodemeta


!> Decode/unpack F_code into type(gmm_metadata)
integer function gmm_decodemeta(F_meta, F_code)
    use gmm_internals
    implicit none

    type(gmm_metadata), intent(out):: F_meta
    integer, intent(in) :: F_code(:)

    integer :: i

    if (size(F_code) < GMM_META_SIZE) then
       F_istat = GMM_ERROR
       return
    endif

    do i = 1, GMM_META_SIZE
        F_meta(i) = F_code(i)
    end do

    gmm_decodemeta = GMM_OK
end function gmm_decodemeta
