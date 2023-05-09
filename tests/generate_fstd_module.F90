module generate_fstd_mod
    use app
    use rmn_common
    use rmn_fstd98

    integer, parameter :: NUM_DATA = 20

    integer(kind = int8),   dimension(NUM_DATA) :: array_int8, array_uint8
    integer(kind = int16),  dimension(NUM_DATA) :: array_int16, array_uint16
    integer(kind = int32),  dimension(NUM_DATA) :: array_int32, array_uint32
    integer(kind = int64),  dimension(NUM_DATA) :: array_int64, array_uint64
    real(kind = real32),    dimension(NUM_DATA) :: array_real32
    real(kind = real64),    dimension(NUM_DATA) :: array_real64
    complex(kind = real32), dimension(NUM_DATA) :: array_complex32
    complex(kind = real64), dimension(NUM_DATA) :: array_complex64
    integer(kind = int32),  dimension(NUM_DATA*2) :: work_array

contains

subroutine generate_file(filename, is_rsf)
    implicit none
    character(len = *), intent(in) :: filename
    logical,            intent(in) :: is_rsf

    character(len=4000) :: cmd
    character(len=3) :: filetype
    integer :: status
    integer :: i
    type(fstd98) :: the_file

    ! Remove file so that we have a fresh start
    write(cmd, '(A, 2(1X, A))') 'rm -fv ', filename
    call execute_command_line(trim(cmd))

    ! Initialize data arrays
    do i = 1, NUM_DATA
        array_real32(i)     = i * 1.0 + .5
        array_real64(i)     = array_real32(i) + 1.2345
        array_complex32(i)  = cmplx(i * 1.0 + .6, i * 2.0 - 0.4)
        array_complex64(i)  = array_complex32(i) + cmplx(-1.3456, 1.9876)
        array_int64(i)      = i - 1001
        array_int32(i)      = i - 13
        array_int16(i)      = i - 11
        array_int8(i)       = i - 10
        array_uint64(i)     = i + 1005
        array_uint32(i)     = i + 22
        array_uint16(i)     = i + 25
        array_uint8(i)      = i + 3
    end do

    if (is_rsf) then
        filetype = 'RSF'
        status = the_file % ouv(filename, 'STD+RND+RSF')
    else
        filetype = 'XDF'
        status = the_file % ouv(filename, 'STD+RND')
    end if


    status = fst_data_length(4)
    status = the_file % ecr(array_int32, work_array, -32, 0, 0, 0, NUM_DATA,  1, 1, 1, 1, 0, 'XX', 'YYYY', 'INT32', 'X', 0, 0, 0, 0, 4, 0)
    ! status = the_file % ecr(ia, work_array, -8,  11, 0, 0, 0, ASIZE,   1, 1, 2,   0, 0, 'XX', 'YYYY', 'ETIKET', 'X', 0, 0, 0, 0, 68, .false.)  ! signed integer with missing

    status = the_file % ecr(array_real32, work_array, -32, 0, 0, 0, NUM_DATA,  1, 1, 2, 1, 0, 'XX', 'YYYY', 'REAL32', 'X', 0, 0, 0, 0, 1, 0)
    ! status = the_file % ecr(fa, work_array, -32, 11, 0, 0, 0, ASIZE,   1, 1, 19,  0, 0, 'XX', 'YYYY', 'ETIKET', 'X', 0, 0, 0, 0, 5,  .false.)  ! IEEE 32

    ! status = the_file % ecr(ca, work_array, -32, 11, 0, 0, 0, ASIZE/2, 1, 1, 119, 0, 0, 'XX', 'YYYY', 'ETIKET', 'X', 0,0,0,0,8+64+128,.false.) ! complex IEEE 32 with missing and compression
    ! status = the_file % ecr(fa, work_array, -16, 11, 0, 0, 0, ASIZE,   1, 1, 4,   0, 0, 'XX', 'YYYY', 'ETIKET', 'X', 0, 0, 0, 0, 65, .false.)  ! float with missing
    ! status = the_file % ecr(fa, work_array, -32, 11, 0, 0, 0, ASIZE,   1, 1, 20,  0, 0, 'XX', 'YYYY', 'ETIKET', 'X', 0, 0, 0, 0, 69, .false.)  ! IEEE 32 with missing

    status = the_file % ecr(array_uint32, work_array, -32, 0, 0, 0, NUM_DATA,   1, 1, 1,   2, 0, 'XX', 'YYYY', 'UINT32', 'X', 0, 0, 0, 0, 2,  0)  ! unsigned integer
    ! status = the_file % ecr(uia, work_array, -8, 11, 0, 0, 0, ASIZE,   1, 1, 6,   0, 0, 'XX', 'YYYY', 'ETIKET', 'X', 0, 0, 0, 0, 66, .false.)  ! unsigned integer with missing

    status = fst_data_length(8)
    status = the_file % ecr(array_real64, work_array, -32, 0, 0, 0, NUM_DATA,   1, 1, 3, 1, 0, 'XX', 'YYYY', 'REAL64', 'X', 0, 0, 0, 0, 1,  0)  ! double
    ! status = the_file % ecr(da, work_array, -64, 11, 0, 0, 0, ASIZE,   1, 1, 17,  0, 0, 'XX', 'YYYY', 'ETIKET', 'X', 0, 0, 0, 0, 5,  .false.)  ! IEEE64
    ! status = the_file % ecr(za, work_array, -64, 11, 0, 0, 0, ASIZE/2, 1, 1, 117, 0, 0, 'XX', 'YYYY', 'ETIKET', 'X',0,0,0,0,8+64+128,.false.)  ! complex IEEE64 with missing and compression
    status = fst_data_length(8)
    ! status = the_file % ecr(da, work_array, -16, 11, 0, 0, 0, ASIZE,   1, 1, 8,   0, 0, 'XX', 'YYYY', 'ETIKET', 'X', 0, 0, 0, 0, 65, .false.) ! double with missing
    ! status = the_file % ecr(da, work_array, -64, 11, 0, 0, 0, ASIZE,   1, 1, 18,  0, 0, 'XX', 'YYYY', 'ETIKET', 'X', 0, 0, 0, 0, 69, .false.) ! IEEE64 with missing

    status = fst_data_length(2)
    status = the_file % ecr(array_uint16, work_array, -16, 0, 0, 0, NUM_DATA, 1, 1, 1,  3, 0, 'XX', 'YYYY', 'UINT16', 'X', 0, 0, 0, 0, 2,  0)  ! unsigned short
    status = fst_data_length(2)
    ! status = the_file % ecr(usa, work_array, -8, 11, 0, 0, 0, ASIZE, 1, 1, 10, 0, 0, 'XX', 'YYYY', 'ETIKET', 'X', 0, 0, 0, 0, 66, .false.)  ! unsigned short with missing

    status = fst_data_length(2)
    status = the_file % ecr(array_int16,  work_array, -16, 0, 0, 0, NUM_DATA, 1, 1, 1, 4, 0, 'XX', 'YYYY', 'INT16', 'X', 0, 0, 0, 0, 4,  0)  ! signed short
    status = fst_data_length(2)
    ! status = the_file % ecr(sa,  work_array, -8, 11, 0, 0, 0, ASIZE, 1, 1, 12, 0, 0, 'XX', 'YYYY', 'ETIKET', 'X', 0, 0, 0, 0, 68, .false.)  ! signed short with missing

    status = fst_data_length(1)
    status = the_file % ecr(array_int8,  work_array, -8, 0, 0, 0, NUM_DATA, 1, 1, 1, 5, 0, 'XX', 'YYYY', 'INT8', 'X', 0, 0, 0, 0, 4,  0)  ! signed byte
    status = fst_data_length(1)
    ! status = the_file % ecr(ba,  work_array, -8, 11, 0, 0, 0, ASIZE, 1, 1, 14, 0, 0, 'XX', 'YYYY', 'ETIKET', 'X', 0, 0, 0, 0, 68, .false.)  ! signed byte with missing

    status = fst_data_length(1)
    status = the_file % ecr(array_uint8, work_array, -8, 0, 0, 0, NUM_DATA, 1, 1, 1, 6, 0, 'XX', 'YYYY', 'UINT8', 'X', 0, 0, 0, 0, 2,  0)  ! unsigned byte
    status = fst_data_length(1)
    ! status = the_file % ecr(uba, work_array, -8, 11, 0, 0, 0, ASIZE, 1, 1, 16, 0, 0, 'XX', 'YYYY', 'ETIKET', 'X', 0, 0, 0, 0, 66, .false.)  ! unsigned byte with missing

    status = the_file % frm()

    write(app_msg, '(A)')                                                                  &
        'Created file ' // filename // ' with type ' // filetype // ''
    call App_Log(APP_INFO, app_msg)

end subroutine generate_file

end module generate_fstd_mod
