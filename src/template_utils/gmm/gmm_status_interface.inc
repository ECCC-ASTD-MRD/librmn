interface gmm_status
    logical function gmm_is_ok(errcode)
        integer, intent(in) :: errcode
    end function

    logical function gmm_is_error(errcode)
        integer, intent(in) :: errcode
    end function
end interface gmm_status
