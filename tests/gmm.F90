program test_gmm
    use rmn_gmm

    implicit none

    integer :: istat
    real, pointer, contiguous, dimension (:,:) :: fis0 => null()
    type(gmm_metadata) :: meta2d ! (l_minx:l_maxx, l_miny:l_maxy)

    meta2d = GMM_NULL_METADATA
    istat = gmm_create('FISO', fis0, meta2d, GMM_FLAG_RSTR + GMM_FLAG_IZER)

end program
