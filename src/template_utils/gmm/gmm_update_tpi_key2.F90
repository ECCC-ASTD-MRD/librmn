integer function gmm_update_tpi_key2(indx, datatype, key)
    implicit none
    integer, intent(in) :: indx, datatype
    integer*8, intent(in) :: key
    integer ier
    integer gmm_update_tpi_key184
    integer gmm_update_tpi_key144
    integer gmm_update_tpi_key284
    integer gmm_update_tpi_key244
    integer gmm_update_tpi_key384
    integer gmm_update_tpi_key183
    integer gmm_update_tpi_key143
    integer gmm_update_tpi_key283
    integer gmm_update_tpi_key243
    integer gmm_update_tpi_key383
    integer gmm_update_tpi_key182
    integer gmm_update_tpi_key142
    integer gmm_update_tpi_key282
    integer gmm_update_tpi_key242
    integer gmm_update_tpi_key382
    integer gmm_update_tpi_key181
    integer gmm_update_tpi_key141
    integer gmm_update_tpi_key281
    integer gmm_update_tpi_key241
    integer gmm_update_tpi_key381
    dtype: select case (datatype)
        case (184)
            gmm_update_tpi_key2 = gmm_update_tpi_key184(indx, key)
        case (144)
            gmm_update_tpi_key2 = gmm_update_tpi_key144(indx, key)
        case (284)
            gmm_update_tpi_key2 = gmm_update_tpi_key284(indx, key)
        case (244)
            gmm_update_tpi_key2 = gmm_update_tpi_key244(indx, key)
        case (384)
            gmm_update_tpi_key2 = gmm_update_tpi_key384(indx, key)
        case (183)
            gmm_update_tpi_key2 = gmm_update_tpi_key183(indx, key)
        case (143)
            gmm_update_tpi_key2 = gmm_update_tpi_key143(indx, key)
        case (283)
            gmm_update_tpi_key2 = gmm_update_tpi_key283(indx, key)
        case (243)
            gmm_update_tpi_key2 = gmm_update_tpi_key243(indx, key)
        case (383)
            gmm_update_tpi_key2 = gmm_update_tpi_key383(indx, key)
        case (182)
            gmm_update_tpi_key2 = gmm_update_tpi_key182(indx, key)
        case (142)
            gmm_update_tpi_key2 = gmm_update_tpi_key142(indx, key)
        case (282)
            gmm_update_tpi_key2 = gmm_update_tpi_key282(indx, key)
        case (242)
            gmm_update_tpi_key2 = gmm_update_tpi_key242(indx, key)
        case (382)
            gmm_update_tpi_key2 = gmm_update_tpi_key382(indx, key)
        case (181)
            gmm_update_tpi_key2 = gmm_update_tpi_key181(indx, key)
        case (141)
            gmm_update_tpi_key2 = gmm_update_tpi_key141(indx, key)
        case (281)
            gmm_update_tpi_key2 = gmm_update_tpi_key281(indx, key)
        case (241)
            gmm_update_tpi_key2 = gmm_update_tpi_key241(indx, key)
        case (381)
            gmm_update_tpi_key2 = gmm_update_tpi_key381(indx, key)
    end select dtype
end function gmm_update_tpi_key2
