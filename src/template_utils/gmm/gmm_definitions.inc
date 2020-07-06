
        ! dimensioning elements
        type gmm_layout
            SEQUENCE
            ! dimension is low:high, useful contents 1..n
            integer :: low, high, halo, halomax, n
        end type

        type gmm_attributes
            SEQUENCE
            ! unique key, used mainly for vmmget interface
            integer*8 :: key
            ! extra keys used for lookup in external tables or additional flags
            integer*8 :: uuid1, uuid2
            ! how the field must be initialized (default=-1, no init)
            integer   :: initmode
            ! flags (valid, restart, .... )
            integer   :: flags
        end type

        type gmm_metadata
            SEQUENCE
            type(gmm_layout), dimension(4) :: l
            type(gmm_attributes) :: a
        end type
