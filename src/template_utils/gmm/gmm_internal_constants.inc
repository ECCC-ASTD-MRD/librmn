        !> Max number of directory pages
        integer, parameter :: MAX_PAGES = 16
        !> Max number of entries in directory page
        integer, parameter :: PAGE_SIZE = 128
        !> Shift for entry number in page
        integer, parameter :: NTRY_NB_SHFT = 0
        !> Mask for entry number in page
        integer, parameter :: NTRY_NB_MASK = 127
        !> Shift for page number in directory
        integer, parameter :: PAGE_NB_SHFT = 7
        !> Mask for page number in directory
        integer, parameter :: PAGE_NB_MASK = 15
        !> Shift for extension code
        integer, parameter :: EXTN_NB_SHFT = 11
        !> Mask for extension code
        integer, parameter :: EXTN_NB_MASK = 511
        !> Shift for magic number
        integer, parameter :: MAGC_NB_SHFT = 32
        !> Mask for magic number (all ones)
        integer, parameter :: MAGC_NB_MASK = -1

        !> Flags kept from user specified flags upon creation
        integer, parameter :: FLAGS_KEPT_ON_CREATE = GMM_FLAG_IZER + GMM_FLAG_INAN + GMM_FLAG_RSTR
        !> Flags propagated to restart file
        integer, parameter :: FLAGS_KEPT_IN_RESTART = GMM_FLAG_IZER + GMM_FLAG_INAN + GMM_FLAG_RSTR + GMM_FLAG_IINV
