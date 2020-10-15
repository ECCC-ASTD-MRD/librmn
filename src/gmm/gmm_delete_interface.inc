interface gmm_delete
    integer function gmm_delete(iname)
        character(len=*), intent(in) :: iname
    end function gmm_delete
end interface