interface
    integer function gmm_nkeys()
    end function gmm_nkeys

    integer function gmm_keys(taglist,pattern)
        character(len=*), intent(out) :: taglist(:)
        character(len=*), intent(in), optional :: pattern
    end function gmm_keys
end interface
