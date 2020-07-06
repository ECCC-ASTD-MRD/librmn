interface
    integer function gmm_rename(old_varname, new_varname)
        character(len=*), intent(in) :: old_varname, new_varname
    end function gmm_rename
end interface
