logical function GMM_IS_OK(errcode)
    implicit none
    integer, intent(in) :: errcode
    GMM_IS_OK = (errcode >= 0)
end function

logical function GMM_IS_ERROR(errcode)
    implicit none
    integer, intent(in) :: errcode
    GMM_IS_ERROR = (errcode < 0)
end function
