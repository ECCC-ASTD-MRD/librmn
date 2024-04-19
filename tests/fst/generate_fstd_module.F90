module generate_fstd_mod
    use app
    use rmn_common
    use rmn_fst24

    integer, parameter :: NUM_DATA = 50

    integer(kind = int8),   dimension(NUM_DATA), target :: array_int8, array_uint8
    integer(kind = int16),  dimension(NUM_DATA), target :: array_int16, array_uint16
    integer(kind = int32),  dimension(NUM_DATA), target :: array_int32, array_uint32
    integer(kind = int64),  dimension(NUM_DATA), target :: array_int64, array_uint64
    real(kind = real32),    dimension(NUM_DATA), target :: array_real32
    real(kind = real64),    dimension(NUM_DATA), target :: array_real64
    complex(kind = real32), dimension(NUM_DATA), target :: array_complex32
    complex(kind = real64), dimension(NUM_DATA), target :: array_complex64

contains

subroutine generate_file(filename, is_rsf, ip3_offset)
    implicit none
    character(len = *), intent(in) :: filename
    logical,            intent(in) :: is_rsf
    integer, optional,  intent(in) :: ip3_offset

    character(len=4000) :: cmd
    character(len=3) :: filetype
    integer :: i
    type(fst_file) :: the_file
    type(fst_record) :: record
    logical :: success

    integer :: ip3
    ip3 = 0
    if (present(ip3_offset)) ip3 = ip3_offset

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
        success = the_file % open(filename, 'RND+RSF+R/W')
    else
        filetype = 'XDF'
        success = the_file % open(filename, 'RND+XDF+R/W')
    end if

    if (.not. success) then
        write (app_msg, '(A, A)') 'Error when opening (creating) file ', filename
        call App_Log(APP_ERROR, app_msg)
        return
    end if

    record % dateo = 0
    record % datev = 0
    record % deet  = 0
    record % npas  = 0
    record % ni  = NUM_DATA
    record % nj  = 1
    record % nk  = 1
    record % ip1 = 1
    record % ip2 = 1
    record % ip3 = ip3
    record % typvar = 'XX'
    record % nomvar = 'YYYY'
    record % grtyp  = 'X'
    record % ig1 = 0
    record % ig2 = 0
    record % ig3 = 0
    record % ig4 = 0

    record % data = c_loc(array_int32)
    record % pack_bits = 32
    record % data_bits = 32
    record % data_type = FST_TYPE_SIGNED
    record % nomvar = 'A'
    record % etiket = 'INT32'
    success = the_file % write(record)

    record % data = c_loc(array_real32)
    record % ip1 = 2
    record % data_type = FST_TYPE_REAL_IEEE
    record % nomvar = 'B'
    record % etiket = 'REAL32'
    success = the_file % write(record)

    record % data = c_loc(array_uint32)
    record % ip1 = 1
    record % ip2 = 2
    record % data_type = FST_TYPE_UNSIGNED
    record % nomvar = 'C'
    record % etiket = 'UINT32'
    success = the_file % write(record)

    record % data = c_loc(array_real64)
    record % data_bits = 64
    record % ip1 = 3
    record % ip2 = 1
    record % nomvar = 'D'
    record % etiket = 'REAL64'
    record % data_type = FST_TYPE_REAL_IEEE
    success = the_file % write(record)

    record % data = c_loc(array_uint16)
    record % pack_bits = 16
    record % data_bits = 16
    record % ip1 = 1
    record % ip2 = 3
    record % nomvar = 'E'
    record % etiket = 'UINT16'
    record % data_type = FST_TYPE_UNSIGNED
    success = the_file % write(record)

    record % data = c_loc(array_int16)
    record % ip1 = 1
    record % ip2 = 4
    record % data_type = FST_TYPE_SIGNED
    record % nomvar = 'F'
    record % etiket = 'INT16'
    success = the_file % write(record)

    record % data = c_loc(array_int8)
    record % pack_bits = 8
    record % data_bits = 8
    record % data_type = FST_TYPE_SIGNED
    record % ip1 = 1
    record % ip2 = 5
    record % nomvar = 'G'
    record % etiket = 'INT8'
    success = the_file % write(record)

    record % data = c_loc(array_uint8)
    record % data_type = FST_TYPE_UNSIGNED
    record % ip1 = 1
    record % ip2 = 6
    record % nomvar = 'H'
    record % etiket = 'UINT8'
    success = the_file % write(record)

    success = the_file % close()

    write(app_msg, '(A)')                                                                  &
        'Created file ' // filename // ' with type ' // filetype // ''
    call App_Log(APP_INFO, app_msg)

end subroutine generate_file

end module generate_fstd_mod
