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

subroutine generate_file(filename, is_rsf)
    implicit none
    character(len = *), intent(in) :: filename
    logical,            intent(in) :: is_rsf

    character(len=4000) :: cmd
    character(len=3) :: filetype
    integer :: i
    type(fst_file) :: the_file
    type(fst_record) :: record
    logical :: success

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
        success = the_file % open(filename, 'STD+RND+RSF')
    else
        filetype = 'XDF'
        success = the_file % open(filename, 'STD+RND+XDF')
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
    record % ip3 = 0
    record % typvar = 'XX'
    record % nomvar = 'YYYY'
    record % grtyp  = 'X'
    record % ig1 = 0
    record % ig2 = 0
    record % ig3 = 0
    record % ig4 = 0

    record % data = c_loc(array_int32)
    record % npak  = -32
    record % dasiz = 32
    record % datyp = FST_TYPE_SIGNED
    record % etiket = 'INT32'
    success = the_file % write(record)

    record % data = c_loc(array_real32)
    record % ip1 = 2
    record % datyp = FST_TYPE_REAL
    record % etiket = 'REAL32'
    success = the_file % write(record)

    record % data = c_loc(array_uint32)
    record % ip1 = 1
    record % ip2 = 2
    record % datyp = FST_TYPE_UNSIGNED
    record % etiket = 'UINT32'
    success = the_file % write(record)

    record % data = c_loc(array_real64)
    record % dasiz = 64
    record % ip1 = 3
    record % ip2 = 1
    record % etiket = 'REAL64'
    record % datyp = FST_TYPE_REAL
    success = the_file % write(record)

    record % data = c_loc(array_uint16)
    record % npak = -16
    record % dasiz = 16
    record % ip1 = 1
    record % ip2 = 3
    record % etiket = 'UINT16'
    record % datyp = FST_TYPE_UNSIGNED
    success = the_file % write(record)

    record % data = c_loc(array_int16)
    record % ip1 = 1
    record % ip2 = 4
    record % datyp = FST_TYPE_SIGNED
    record % etiket = 'INT16'
    success = the_file % write(record)

    record % data = c_loc(array_int8)
    record % npak = -8
    record % dasiz = 8
    record % datyp = FST_TYPE_SIGNED
    record % ip1 = 1
    record % ip2 = 5
    record % etiket = 'INT8'
    success = the_file % write(record)

    record % data = c_loc(array_uint8)
    record % datyp = FST_TYPE_UNSIGNED
    record % ip1 = 1
    record % ip2 = 6
    record % etiket = 'UINT8'
    success = the_file % write(record)

    success = the_file % close()

    write(app_msg, '(A)')                                                                  &
        'Created file ' // filename // ' with type ' // filetype // ''
    call App_Log(APP_INFO, app_msg)

end subroutine generate_file

end module generate_fstd_mod
