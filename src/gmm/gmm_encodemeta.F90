!> Encode/pack type(gmm_metadata) in a basic Fortran type
!> @author  Stephane Chamberland, 2008-03
function gmm_encodemeta(F_meta, F_code) result(F_istat)
    use gmm_internals
    implicit none

    type(gmm_metadata), intent(in) :: F_meta
    integer, intent(out):: F_code(:)

    integer :: F_istat  !status

    if (size(F_code) < GMM_META_SIZE) then
       F_istat = GMM_ERROR
       return
    endif
    F_istat = GMM_OK
    ! FIXME movlev is an undefined external!
    call movlev(F_meta, F_code, GMM_META_SIZE)
end function gmm_encodemeta


!> Decode/unpack F_code into type(gmm_metadata)
!> @author  Stephane Chamberland, 2008-03
function gmm_decodemeta(F_meta, F_code) result(F_istat)
    use gmm_internals
    implicit none

    type(gmm_metadata), intent(out):: F_meta
    integer, intent(in) :: F_code(:)

    integer :: F_istat

    if (size(F_code) < GMM_META_SIZE) then
       F_istat = GMM_ERROR
       return
    endif
    F_istat = GMM_OK
    ! FIXME movlev is an undefined external!
    call movlev(F_code, F_meta, GMM_META_SIZE)
end function gmm_decodemeta
