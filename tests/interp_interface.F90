program test_interp_interface
    ! use rmn_interp
    implicit none
    integer, external :: ezqkdef, ezdefset, ezgetopt, ezsetopt

    integer :: status
    character(len=32) :: opt_value

    status = ezqkdef(1, 1, 'Q', 1, 1, 1, 1, 1)
    status = ezdefset(1, 2)

    status = ezgetopt('verbose', opt_value)
    print *, 'verbose = ', opt_value
    status = ezsetopt('verbose', 'oui')
    status = ezgetopt('verbose', opt_value)
    print *, 'verbose = ', opt_value
    status = ezsetopt('verbose', 'ouiouioui')
    status = ezgetopt('verbose', opt_value)
    print *, 'verbose = ', opt_value

end program test_interp_interface
