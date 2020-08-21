        !> Dimensioning elements
        type gmm_layout
            SEQUENCE
            !> Dimension is low:high, useful contents 1..n
            integer :: low
            integer :: high
            integer :: halo
            integer :: halomax
            integer :: n
        end type

        type gmm_attributes
            SEQUENCE
            !> Unique key, used mainly for vmmget interface
            integer(kind = 8) :: key
            !> Extra keys used for lookup in external tables or additional flags
            integer(kind = 8) :: uuid1, uuid2
            !> How the field must be initialized (default=-1, no init)
            integer :: initmode
            !> Flags (valid, restart, .... )
            integer :: flags
        end type

        type gmm_metadata
            SEQUENCE
            type(gmm_layout), dimension(4) :: l
            type(gmm_attributes) :: a
        end type
