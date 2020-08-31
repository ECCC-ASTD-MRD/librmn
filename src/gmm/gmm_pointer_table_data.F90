module gmm_pointer_table_data
    use gmm_internals

    type gmm_p_141
        integer(kind = 4), pointer :: p(:)
        integer(kind = 8) :: key
    end type
    type (gmm_p_141), dimension(MAX_PAGES * PAGE_SIZE) :: gmm_ptrs141
    !> Number of pages in directory
    integer :: gmm_p_table_size_141 = 0

    type gmm_p_142
        integer(kind = 4), pointer :: p(:,:)
        integer(kind = 8) :: key
    end type
    type (gmm_p_142), dimension(MAX_PAGES * PAGE_SIZE) :: gmm_ptrs142
    !> Number of pages in directory
    integer :: gmm_p_table_size_142 = 0

    type gmm_p_143
        integer(kind = 4), pointer :: p(:,:,:)
        integer(kind = 8) :: key
    end type
    type (gmm_p_143), dimension(MAX_PAGES * PAGE_SIZE) :: gmm_ptrs143
    !> Number of pages in directory
    integer :: gmm_p_table_size_143 = 0

    type gmm_p_144
        integer(kind = 4), pointer :: p(:,:,:,:)
        integer(kind = 8) :: key
    end type
    type (gmm_p_144), dimension(MAX_PAGES * PAGE_SIZE) :: gmm_ptrs144
    !> Number of pages in directory
    integer :: gmm_p_table_size_144 = 0

    type gmm_p_181
        integer(kind = 8), pointer :: p(:)
        integer(kind = 8) :: key
    end type
    type (gmm_p_181), dimension(MAX_PAGES * PAGE_SIZE) :: gmm_ptrs181
    !> Number of pages in directory
    integer :: gmm_p_table_size_181 = 0

    type gmm_p_182
        integer(kind = 8), pointer :: p(:,:)
        integer(kind = 8) :: key
    end type
    type (gmm_p_182), dimension(MAX_PAGES * PAGE_SIZE) :: gmm_ptrs182
    !> Number of pages in directory
    integer :: gmm_p_table_size_182 = 0

    type gmm_p_183
        integer(kind = 8), pointer :: p(:,:,:)
        integer(kind = 8) :: key
    end type
    type (gmm_p_183), dimension(MAX_PAGES * PAGE_SIZE) :: gmm_ptrs183
    !> Number of pages in directory
    integer :: gmm_p_table_size_183 = 0

    type gmm_p_184
        integer(kind = 8), pointer :: p(:,:,:,:)
        integer(kind = 8) :: key
    end type
    type (gmm_p_184), dimension(MAX_PAGES * PAGE_SIZE) :: gmm_ptrs184
    !> Number of pages in directory
    integer :: gmm_p_table_size_184 = 0

    type gmm_p_241
        real*4, pointer :: p(:)
        integer(kind = 8) :: key
    end type
    type (gmm_p_241), dimension(MAX_PAGES * PAGE_SIZE) :: gmm_ptrs241
    !> Number of pages in directory
    integer :: gmm_p_table_size_241 = 0

    type gmm_p_242
        real*4, pointer :: p(:,:)
        integer(kind = 8) :: key
    end type
    type (gmm_p_242), dimension(MAX_PAGES * PAGE_SIZE) :: gmm_ptrs242
    !> Number of pages in directory
    integer :: gmm_p_table_size_242 = 0

    type gmm_p_243
        real*4, pointer :: p(:,:,:)
        integer(kind = 8) :: key
    end type
    type (gmm_p_243), dimension(MAX_PAGES * PAGE_SIZE) :: gmm_ptrs243
    !> Number of pages in directory
    integer :: gmm_p_table_size_243 = 0

    type gmm_p_244
        real*4, pointer :: p(:,:,:,:)
        integer(kind = 8) :: key
    end type
    type (gmm_p_244), dimension(MAX_PAGES * PAGE_SIZE) :: gmm_ptrs244
    !> Number of pages in directory
    integer :: gmm_p_table_size_244 = 0

    type gmm_p_281
        real*8, pointer :: p(:)
        integer(kind = 8) :: key
    end type
    type (gmm_p_281), dimension(MAX_PAGES * PAGE_SIZE) :: gmm_ptrs281
    !> Number of pages in directory
    integer :: gmm_p_table_size_281 = 0

    type gmm_p_282
        real*8, pointer :: p(:,:)
        integer(kind = 8) :: key
    end type
    type (gmm_p_282), dimension(MAX_PAGES * PAGE_SIZE) :: gmm_ptrs282
    !> Number of pages in directory
    integer :: gmm_p_table_size_282 = 0

    type gmm_p_283
        real*8, pointer :: p(:,:,:)
        integer(kind = 8) :: key
    end type
    type (gmm_p_283), dimension(MAX_PAGES * PAGE_SIZE) :: gmm_ptrs283
    !> Number of pages in directory
    integer :: gmm_p_table_size_283 = 0

    type gmm_p_284
        real*8, pointer :: p(:,:,:,:)
        integer(kind = 8) :: key
    end type
    type (gmm_p_284), dimension(MAX_PAGES * PAGE_SIZE) :: gmm_ptrs284
    !> Number of pages in directory
    integer :: gmm_p_table_size_284 = 0

    type gmm_p_381
        complex*8, pointer :: p(:)
        integer(kind = 8) :: key
    end type
    type (gmm_p_381), dimension(MAX_PAGES * PAGE_SIZE) :: gmm_ptrs381
    !> Number of pages in directory
    integer :: gmm_p_table_size_381 = 0

    type gmm_p_382
        complex*8, pointer :: p(:,:)
        integer(kind = 8) :: key
    end type
    type (gmm_p_382), dimension(MAX_PAGES * PAGE_SIZE) :: gmm_ptrs382
    !> Number of pages in directory
    integer :: gmm_p_table_size_382 = 0

    type gmm_p_383
        complex*8, pointer :: p(:,:,:)
        integer(kind = 8) :: key
    end type
    type (gmm_p_383), dimension(MAX_PAGES * PAGE_SIZE) :: gmm_ptrs383
    !> Number of pages in directory
    integer :: gmm_p_table_size_383 = 0

    type gmm_p_384
        complex*8, pointer :: p(:,:,:,:)
        integer(kind = 8) :: key
    end type
    type (gmm_p_384), dimension(MAX_PAGES * PAGE_SIZE) :: gmm_ptrs384
    !> Number of pages in directory
    integer :: gmm_p_table_size_384 = 0
end module gmm_pointer_table_data
