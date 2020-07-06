interface
    integer function gmm_shuffle(taglist)
        character(len=*), intent(in) :: taglist(:)
    end function gmm_shuffle
end interface
