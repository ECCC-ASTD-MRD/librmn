interface gmm_status
    logical function GMM_IS_OK(errcode)
        integer, intent(in) :: errcode
    end function

    logical function GMM_IS_ERROR(errcode)
        integer, intent(in) :: errcode
    end function
end interface gmm_status
