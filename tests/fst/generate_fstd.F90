program generate_fstd
    use generate_fstd_mod
    implicit none

    call generate_file('rsf_1.fst', .true.)
    call generate_file('xdf_1.fst', .false.)

end program generate_fstd
