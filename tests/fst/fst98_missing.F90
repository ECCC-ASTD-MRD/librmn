#define COMPARE_VALS(a, b, threshold) \
    if (sum((a-b)**2) / sum(a**2) > threshold) then ;\
      print '(2(A, " = ", /, 2(8E14.7, /)))', #a, a, #b, b; \
      error stop 1          ;\
    end if

#define COMPARE_EXACT(a, b) call compare_fields(a, b, #a, #b)

module helpers
  use app
  use rmn_common
  use rmn_test_helper
  implicit none

  integer :: missing_value_status = 0

  interface compare_fields
    module procedure compare_fields_i32
    module procedure compare_fields_i16
    module procedure compare_fields_i8
  end interface compare_fields

contains

  subroutine compare_fields_i32(a, b, a_name, b_name)
    implicit none
    integer(int32), intent(in), dimension(:) :: a, b
    character(len=*), intent(in) :: a_name, b_name
    if (any(a /= b)) then
      print '(2(A, " = ", /, 2(8I12, /)))', a_name, a, b_name, b
      error stop 1
    end if
  end subroutine compare_fields_i32

  subroutine compare_fields_i16(a, b, a_name, b_name)
    implicit none
    integer(int16), intent(in), dimension(:) :: a, b
    character(len=*), intent(in) :: a_name, b_name
    if (any(a /= b)) then
      print '(2(A, " = ", /, 2(8I12, /)))', a_name, a, b_name, b
      error stop 1
    end if
  end subroutine compare_fields_i16

  subroutine compare_fields_i8(a, b, a_name, b_name)
    implicit none
    integer(int8), intent(in), dimension(:) :: a, b
    character(len=*), intent(in) :: a_name, b_name
    if (any(a /= b)) then
      print '(2(A, " = ", /, 2(8I12, /)))', a_name, a, b_name, b
      error stop 1
    end if
  end subroutine compare_fields_i8

  subroutine validate_status(status, line)
    implicit none
    integer, intent(in) :: status
    integer, intent(in) :: line
    if (status < 0) then
      print '(A, I5, A, I12)', 'Line ', line, ': Command failed with status ', status
      error stop 1
    end if
  end subroutine validate_status

  function vs(status, line) result(s)
    implicit none
    integer, intent(in) :: status
    integer, intent(in) :: line
    integer :: s
    call validate_status(status, line)
    s = status
  end function vs

  subroutine print_values(header, status, fm, dm, im, sm, bm, uim, usm, ubm, ca, za, fa, da)
    use app
    implicit none
    character(len=*), intent(in) :: header
    integer, intent(in) :: status
    real(kind = 4), intent(in) :: fm
    real(kind = 8), intent(in) :: dm
    integer(kind = 4), intent(in) :: im, uim
    integer(kind = 2), intent(in) :: sm, usm
    integer(kind = 1), intent(in) :: bm, ubm
    real(kind = 4), intent(in), optional :: ca, fa
    real(kind = 8), intent(in), optional :: za, da

201 format(A, 1X, I2, ', ', 2(G14.5, ', '), I11, ', ', 5I7, ', ', 4G14.5)

    if (present(ca) .and. present(za) .and. present(fa) .and. present(da)) then
      write(app_msg, 201) trim(header), status, fm, dm, im, sm, bm, uim, usm, ubm, ca, za, fa, da
    else if (present(ca) .and. present(za)) then
      write(app_msg, 201) trim(header), status, fm, dm, im, sm, bm, uim, usm, ubm, ca, za
    else
      write(app_msg, 201) trim(header), status, fm, dm, im, sm, bm, uim, usm, ubm
    end if
    call App_Log(APP_DEBUG, app_msg)

  end subroutine print_values

  subroutine compare_real32(a, b, threshold, a_name, b_name)
    implicit none
    real(real32), intent(in), dimension(:) :: a, b
    real(real32), intent(in) :: threshold
    character(len=*), intent(in) :: a_name, b_name

    real(real32), dimension(:), allocatable :: diff

    diff = abs((b - a) / a)
    if (any(diff > threshold)) then
      print '(3(A, " = ", /, 2(8E14.6, /)))', a_name, a, b_name, b, 'diff', diff
      error stop 1
    end if
  end subroutine compare_real32
  subroutine compare_real64(a, b, threshold, a_name, b_name)
    implicit none
    real(real64), intent(in), dimension(:) :: a, b
    real(real64), intent(in) :: threshold
    character(len=*), intent(in) :: a_name, b_name

    real(real64), dimension(:), allocatable :: diff

    diff = abs((b - a) / a)
    if (any(diff > threshold)) then
      print '(3(A, " = ", /, 2(8E14.6, /)))', a_name, a, b_name, b, 'diff', diff
      error stop 1
    end if
  end subroutine compare_real64

  subroutine check_record_has_missing(iun, ip1)
    use app
    use rmn_fst98
    implicit none
    integer, intent(in) :: iun
    integer, intent(in) :: ip1

    integer :: i

    integer :: ni, nj, nk
    integer, parameter :: MAX_REC = 100
    integer, dimension(MAX_REC) :: handles
    integer :: num_records
    integer :: status

    status = fstinl(iun, ni, nj, nk, -1, '', ip1, -1, -1, '', '', handles, num_records, MAX_REC)
    if (num_records <= 0 .or. status < 0) then
      write(app_msg, '(A, I5, A)') 'No record found with ip1 ', ip1, ' (or just an error...)'
      call app_log(APP_ERROR, app_msg)
      error stop 1
    end if

    block
      integer :: date, deet, npas
      integer :: dummy, ip2, ip3, ig1, ig2, ig3, ig4
      integer :: nbits, datyp
      character(len=:), allocatable :: nomvar, typvar, etiket, grtyp
      integer :: swa, lng, dlft, ubc
      do i = 1, num_records
        status = fstprm(handles(i), date, deet, npas, ni, nj, nk, nbits, datyp, &
                        dummy, ip2, ip3, typvar, nomvar, etiket, grtyp, ig1, ig2, ig3, ig4, &
                        swa, lng, dlft, ubc, dummy, dummy, dummy)
        if (iand(datyp, 64) /= 64) then
          write(app_msg, *) 'Record ', handles(i), '(ip1 = ', ip1, ') does not seem to have missing values!'
          call app_log(APP_ERROR, app_msg)
          error stop 1
        end if
      end do
    end block
  end subroutine

  subroutine test_missing_values(is_rsf)
    use rmn_fst98
    implicit none
    logical, intent(in) :: is_rsf

#define CHECK_STATUS(cmd) s=cmd;s=vs(s,__LINE__)
#define CHK(cmd) st=cmd;s=vs(st,__LINE__)

    integer, external :: fnom

    integer :: status, st, i, s
    integer, parameter :: ASIZE=16             ! ASIZE should be kept even because of complex tests
    integer, dimension(ASIZE) :: work
    integer, dimension(ASIZE) :: in_i32, out_i32, expected_i32
    integer, dimension(ASIZE) :: in_u32, out_u32, expected_u32
    integer (2), dimension(ASIZE) :: in_i16, out_i16, expected_i16
    integer (1), dimension(ASIZE) :: in_i8, out_i8, expected_i8
    integer (2), dimension(ASIZE) :: in_u16, out_u16, expected_u16
    integer (1), dimension(ASIZE) :: in_u8, out_u8, expected_u8
    real (4), dimension(ASIZE) :: in_r32, out_r32, expected_r32, fa3
    real (4), dimension(ASIZE) :: in_c32, out_c32, expected_c32   ! used for 32 bit complex data
    real (8), dimension(ASIZE) :: in_r64, out_r64, expected_r64, da3
    real (8), dimension(ASIZE) :: in_c64, out_c64, expected_c64   ! used for 64 bit complex data
    integer (c_int32_t) :: missing_i32, missing_u32
    integer (c_int16_t) :: missing_i16, missing_u16
    integer (c_int8_t)  :: missing_i8, missing_u8
    real (c_float)  :: missing_r32
    real (c_double) :: missing_r64
    integer (c_int32_t) :: old_missing_i32, old_missing_u32
    integer (c_int16_t) :: old_missing_i16, old_missing_u16
    integer (c_int8_t)  :: old_missing_i8, old_missing_u8
    real (c_float)  :: old_missing_r32
    real (c_double) :: old_missing_r64
    integer :: ni, nj, nk
    integer :: iun

    character(len=4) :: nomvar
    character(len=*), parameter :: test_file_name = 'missing.fst'
    character(len=2000) :: cmd

    nomvar = 'XDF '
    if (is_rsf) nomvar = 'RSF '


    if (is_rsf) call App_Log(APP_INFO, 'Testing RSF')
    if (.not. is_rsf) call App_Log(APP_INFO, 'Testing XDF')

    status = -1
    st = -1
    missing_r32 = -1
    missing_i32 = -1
    missing_u32 = -1
    missing_r64 = -1
    missing_i16 = -1
    missing_u16 = -1
    missing_i8 = -1
    missing_u8 = -1

    status = get_missing_value_flags(missing_r32, missing_i32, missing_u32, missing_r64, missing_i16, missing_u16, &
                                     missing_i8, missing_u8)
    call check_status(status, expected = missing_value_status, fail_message = 'get_missing_value_flags')
    call print_values('Missing values: status', s, missing_r32, missing_r64, missing_i32, missing_i16, &
                      missing_i8, missing_u32, missing_u16, missing_u8)
    missing_value_status = 1

    ! Preserve old missing values, to be able to restore them at the end
    old_missing_i32 = missing_i32
    old_missing_u32 = missing_u32
    old_missing_i16 = missing_i16
    old_missing_u16 = missing_u16
    old_missing_i8 = missing_i8
    old_missing_u8 = missing_u8
    old_missing_r32 = missing_r32
    old_missing_r64 = missing_r64

    ! Set values explicitly (validate the call)
    call set_missing_value_flags(missing_r32, missing_i32, missing_u32, missing_r64, missing_i16, missing_u16, &
                                 missing_i8, missing_u8)
    status = get_missing_value_flags(missing_r32, missing_i32, missing_u32, missing_r64, missing_i16, missing_u16, &
                                     missing_i8, missing_u8)
    call check_status(status, expected = 1, fail_message = 'get_missing_value_flags')
    call print_values('Missing values: status', s, missing_r32, missing_r64, missing_i32, missing_i16, missing_i8, &
                      missing_u32, missing_u16, missing_u8)

    ! ---------------
    ! Initialize data
    do i = 1, ASIZE
      in_r32(i)  = i*1.0+.5
      in_c32(i)  = i*1.0+.6
      in_i32(i)  = i-13
      in_u32(i) = i+22
      in_r64(i)  = in_r32(i)+1.2345
      in_c64(i)  = in_r32(i)+1.3456
      in_i16(i)  = i-13
      in_i8(i)  = i-13
      in_u16(i) = i+22
      in_u8(i) = i+22
    enddo

    in_i32(ASIZE/2)  = 125      ! set to 127 to force error message
    in_i16(ASIZE/2)  = 124      ! set to 127 to force error message
    in_i8(ASIZE/2)  = 123      ! set to 127 to force error message
    in_u32(ASIZE/2) = 113      ! set to 255 to force error message
    in_u16(ASIZE/2) = 112      ! set to 255 to force error message
    in_u8(ASIZE/2) = 111      ! set to 255 to force error message

    ! Missing values get truncated to 0 when storing with npak = -8
    expected_i32 = in_i32
    in_i32(2) = missing_i32; in_i32(ASIZE-1) = missing_i32; in_i32(3) = missing_i32; in_i32(ASIZE-2) = missing_i32
    expected_i32(2) = 0;     expected_i32(ASIZE-1) = 0;     expected_i32(3) = 0;     expected_i32(ASIZE-2) = 0
    ! in_i32(1) = missing_i32
    ! expected_i32(1) = 0

    expected_i16 = in_i16
    in_i16(2) = missing_i16; in_i16(ASIZE-1) = missing_i16; in_i16(4) = missing_i16; in_i16(ASIZE-3) = missing_i16
    expected_i16(2) = 0; expected_i16(ASIZE-1) = 0; expected_i16(4) = 0; expected_i16(ASIZE-3) = 0
    ! in_i16(1) = missing_i16
    ! expected_i16(1) = 0

    ! Missing values get truncated to 255 when storing with npak = -8
    expected_u32 = in_u32
    in_u32(3) = missing_u32; in_u32(ASIZE-2) = missing_u32
    expected_u32(3) = 255; expected_u32(ASIZE-2) = 255
    ! in_u32(1) = missing_u32
    ! expected_u32(1) = 255

    expected_u16 = in_u16
    in_u16(3) = missing_u16  ; in_u16(ASIZE-2) = missing_u16 ; in_u16(ASIZE)=missing_u16
    expected_u16(3) = 255  ; expected_u16(ASIZE-2) = 255 ; expected_u16(ASIZE)=255

    in_i8(2)  = missing_i8; in_i8(ASIZE-1) = missing_i8; in_i8(1) = missing_i8
    in_u8(3) = missing_u8  ; in_u8(ASIZE-2) = missing_u8
    expected_i8 = in_i8
    expected_u8 = in_u8

    in_r32(1)  = missing_r32; in_r32(ASIZE) = missing_r32; in_r32(2) = missing_r32
    in_c32(1)  = missing_r32; in_c32(ASIZE) = missing_r32; in_c32(2) = missing_r32
    in_r64(1)  = missing_r64; in_r64(ASIZE) = missing_r64; in_r64(ASIZE-1) = missing_r64
    in_c64(1)  = missing_r64; in_c64(ASIZE) = missing_r64; in_c64(ASIZE-1) = missing_r64
    expected_r32 = in_r32
    expected_c32 = in_c32
    expected_r64 = in_r64
    expected_c64 = in_c64

    call App_Log(APP_DEBUG, 'Test data:')
    do i = 1, ASIZE
      call print_values('', i, in_r32(i), in_r64(i), in_i32(i), in_i16(i), in_i8(i), in_u32(i), in_u16(i), in_u8(i), in_c32(i), in_c64(i))
    enddo

    ! Reset status
    status = -1

    write(cmd, '(A, (1X, A))') 'rm -fv ', test_file_name
    call execute_command_line(trim(cmd))

    !
    ! -----------------------------
    ! Write data into standard file
    !------------------------------
    !

    ! open(unit=1234, iostat=status, file=test_file_name, status='old')
    ! if (status == 0) close(1234, status='delete')
    call App_Log(APP_INFO, '============= writing into standard file with and without missing values ============')
    iun = 0
    status = (fnom(iun, test_file_name, 'STD+RND', 0))
    call check_status(status, expected = 0, fail_message = 'fnom')
    if (is_rsf) then
      status = (fstouv(iun, 'RND+RSF'))
      call check_status(status, expected_min = 0, fail_message = 'fstouv (RSF)')
    else
      status = (fstouv(iun,'RND'))
      call check_status(status, expected = 0, fail_message = 'fstouv (XDF)')
    end if
    
    ! Line limit is 127 columns (for gfortran)
    CHECK_STATUS(fstecr(in_i32, work, -8,  iun, 0, 0, 0, ASIZE, 1, 1, 1,   0, 0, 'XX', nomvar,   'i32',   'X', 0, 0, 0, 0, 4,  0))  ! signed integer
    CHECK_STATUS(fstecr(in_i32, work, -8,  iun, 0, 0, 0, ASIZE,   1, 1, 2,   0, 0, 'XX', nomvar, 'i32_M', 'X', 0, 0, 0, 0, 68, 0))  ! signed integer with missing
    call check_record_has_missing(iun, 2)

    CHECK_STATUS(fstecr(in_r32, work, -16, iun, 0, 0, 0, ASIZE,   1, 1, 3,   0, 0, 'XX', nomvar, 'r32',   'X', 0, 0, 0, 0, 1,  0))  ! float
    CHECK_STATUS(fstecr(in_r32, work, -32, iun, 0, 0, 0, ASIZE,   1, 1, 19,  0, 0, 'XX', nomvar, 'r32',   'X', 0, 0, 0, 0, 5,  0))  ! IEEE 32
    CHECK_STATUS(fstecr(in_c32, work, -32, iun, 0, 0, 0, ASIZE/2, 1, 1, 119, 0, 0, 'XX', nomvar, 'c32_M', 'X', 0,0,0,0,8+64+128,0)) ! complex IEEE 32 with missing and compression
    CHECK_STATUS(fstecr(in_r32, work, -16, iun, 0, 0, 0, ASIZE,   1, 1, 4,   0, 0, 'XX', nomvar, 'r32_M', 'X', 0, 0, 0, 0, 65, 0))  ! float with missing
    CHECK_STATUS(fstecr(in_r32, work, -32, iun, 0, 0, 0, ASIZE,   1, 1, 20,  0, 0, 'XX', nomvar, 'r32_M', 'X', 0, 0, 0, 0, 69, 0))  ! IEEE 32 with missing
    call check_record_has_missing(iun, 4)
    call check_record_has_missing(iun, 20)

    CHECK_STATUS(fstecr(in_u32, work, -8, iun, 0, 0, 0, ASIZE,   1, 1, 5,   0, 0, 'XX', nomvar, 'u32',   'X', 0, 0, 0, 0, 2,  0))  ! unsigned integer
    CHECK_STATUS(fstecr(in_u32, work, -8, iun, 0, 0, 0, ASIZE,   1, 1, 6,   0, 0, 'XX', nomvar, 'u32_M', 'X', 0, 0, 0, 0, 66, 0))  ! unsigned integer with missing
    call check_record_has_missing(iun, 6)

    CHECK_STATUS(fst_data_length(8))
    CHECK_STATUS(fstecr(in_r64, work, -16, iun, 0, 0, 0, ASIZE,   1, 1, 7,   0, 0, 'XX', nomvar, 'r64-',  'X', 0, 0, 0, 0, 1,  0))  ! double
    CHECK_STATUS(fst_data_length(8))
    CHECK_STATUS(fstecr(in_r64, work, -64, iun, 0, 0, 0, ASIZE,   1, 1, 17,  0, 0, 'XX', nomvar, 'r64',   'X', 0, 0, 0, 0, 5,  0))  ! IEEE64
    CHECK_STATUS(fst_data_length(8))
    CHECK_STATUS(fstecr(in_c64, work, -64, iun, 0, 0, 0, ASIZE/2, 1, 1, 117, 0, 0, 'XX', nomvar, 'c64_M', 'X',0,0,0,0,8+64+128,0))  ! complex IEEE64 with missing and compression
    CHECK_STATUS(fst_data_length(8))
    CHECK_STATUS(fstecr(in_r64, work, -16, iun, 0, 0, 0, ASIZE,   1, 1, 8,   0, 0, 'XX', nomvar, 'r64-M', 'X', 0, 0, 0, 0, 65, 0)) ! double with missing
    call check_record_has_missing(iun, 8)
    CHECK_STATUS(fst_data_length(8))
    CHECK_STATUS(fstecr(in_r64, work, -16, iun, 0, 0, 0, ASIZE,   1, 1, 28,   0, 0, 'XX', nomvar, 'r64-M', 'X', 0, 0, 0, 0, 69, 0)) ! IEEE64 with missing
    call check_record_has_missing(iun, 28)
    CHECK_STATUS(fst_data_length(8))
    CHECK_STATUS(fstecr(in_r64, work, -64, iun, 0, 0, 0, ASIZE,   1, 1, 18,  0, 0, 'XX', nomvar, 'r64_M', 'X', 0, 0, 0, 0, 69, 0)) ! IEEE64 with missing
    call check_record_has_missing(iun, 18)

    CHECK_STATUS(fst_data_length(2))
    CHECK_STATUS(fstecr(in_u16, work, -8, iun, 0, 0, 0, ASIZE, 1, 1, 9,  0, 0, 'XX', nomvar, 'u16',   'X', 0, 0, 0, 0, 2,  0))  ! unsigned short
    CHECK_STATUS(fst_data_length(2))
    CHECK_STATUS(fstecr(in_u16, work, -8, iun, 0, 0, 0, ASIZE, 1, 1, 10, 0, 0, 'XX', nomvar, 'u16_M', 'X', 0, 0, 0, 0, 66, 0))  ! unsigned short with missing
    call check_record_has_missing(iun, 10)

    CHECK_STATUS(fst_data_length(2))
    CHECK_STATUS(fstecr(in_i16,  work, -8, iun, 0, 0, 0, ASIZE, 1, 1, 11, 0, 0, 'XX', nomvar, 'i16',   'X', 0, 0, 0, 0, 4,  0))  ! signed short
    CHECK_STATUS(fst_data_length(2))
    CHECK_STATUS(fstecr(in_i16,  work, -8, iun, 0, 0, 0, ASIZE, 1, 1, 12, 0, 0, 'XX', nomvar, 'i16_M', 'X', 0, 0, 0, 0, 68, 0))  ! signed short with missing
    call check_record_has_missing(iun, 12)

    CHECK_STATUS(fst_data_length(1))
    CHECK_STATUS(fstecr(in_i8,  work, -8, iun, 0, 0, 0, ASIZE, 1, 1, 13, 0, 0, 'XX', nomvar, 'i08', 'X', 0, 0, 0, 0, 4,  0))  ! signed byte
    CHECK_STATUS(fst_data_length(1))
    CHECK_STATUS(fstecr(in_i8,  work, -8, iun, 0, 0, 0, ASIZE, 1, 1, 14, 0, 0, 'XX', nomvar, 'i08_M', 'X', 0, 0, 0, 0, 68, 0))  ! signed byte with missing
    call check_record_has_missing(iun, 14)

    CHECK_STATUS(fst_data_length(1))
    CHECK_STATUS(fstecr(in_u8, work, -8, iun, 0, 0, 0, ASIZE, 1, 1, 15, 0, 0, 'XX', nomvar, 'u08', 'X', 0, 0, 0, 0, 2,  0))  ! unsigned byte
    CHECK_STATUS(fst_data_length(1))
    CHECK_STATUS(fstecr(in_u8, work, -8, iun, 0, 0, 0, ASIZE, 1, 1, 16, 0, 0, 'XX', nomvar, 'u08_M', 'X', 0, 0, 0, 0, 66, 0))  ! unsigned byte with missing
    call check_record_has_missing(iun, 16)

    ! Close the file
    CHECK_STATUS(fstfrm(iun))

    !
    ! ----------------------------
    ! Read data from standard file
    ! ----------------------------
    !

    !  set new flag values before reading so we can see the difference 'type n' vs 'type n+64'
    ! missing_r32=-999.99; missing_r64=-888.88; missing_i32=-999999; missing_i16=-32768 ; missing_i8=-128; missing_u32=999999 ; missing_u16=32767; missing_u8=127
    ! call App_Log(APP_DEBUG, '============================== missing values =============================')
    ! call App_Log(APP_DEBUG, '#     float        double            int  short   byte   uint ushort  ubyte')
    ! call set_missing_value_flags(missing_r32,missing_i32,missing_u32,missing_r64,missing_i16,missing_u16,missing_i8,missing_u8)
    ! call print_values('',status,missing_r32,missing_r64,missing_i32,missing_i16,missing_i8,missing_u32,missing_u16,missing_u8)

    call App_Log(APP_INFO, '============= reading from standard file with and without missing values ============')
    iun = 0
    CHECK_STATUS(fnom(iun, test_file_name, 'STD+RND+OLD+R/O', 0))
    CHECK_STATUS(fstouv(iun,'RND'))

    call App_Log(APP_INFO, '=========== Read back: WITHOUT missing value ==========')
    CHECK_STATUS(fstlir(out_i32,iun,ni,nj,nk,-1,' ',1,-1,-1,' ',' '))     ! integer
    CHECK_STATUS(fstlir(out_u32,iun,ni,nj,nk,-1,' ',5,-1,-1,' ',' '))    ! unsigned integer
    CHECK_STATUS(fst_data_length(2))
    CHECK_STATUS(fstlir(out_u16,iun,ni,nj,nk,-1,' ',9,-1,-1,' ',' '))    ! unsigned short
    CHECK_STATUS(fst_data_length(2))
    CHECK_STATUS(fstlir(out_i16,iun,ni,nj,nk,-1,' ',11,-1,-1,' ',' '))    ! signed short
    CHECK_STATUS(fst_data_length(1))
    CHECK_STATUS(fstlir(out_i8,iun,ni,nj,nk,-1,' ',13,-1,-1,' ',' '))    ! signed byte
    CHECK_STATUS(fst_data_length(1))
    CHECK_STATUS(fstlir(out_u8,iun,ni,nj,nk,-1,' ',15,-1,-1,' ',' '))   ! unsigned byte

    ! Check values read
    call compare_fields(out_i32, expected_i32, 'out_i32', 'expected i32')
    call compare_fields(out_u32, expected_u32, 'out u32', 'expected u32')
    call compare_fields(out_i16, expected_i16, 'out i16', 'expected i16')
    call compare_fields(out_u16, expected_u16, 'out u16', 'expected u16')
    call compare_fields(out_i8, expected_i8, 'out i8', 'expected i8')
    call compare_fields(out_u8, expected_u8, 'out u8', 'expected u8')

    CHECK_STATUS(fstlir(out_r32,iun,ni,nj,nk,-1,' ',3,-1,-1,' ',' '))     ! float
    CHECK_STATUS(fstlir(fa3,iun,ni,nj,nk,-1,' ',19,-1,-1,' ',' '))     ! IEEE 32
    out_c32 = -1.0
    CHECK_STATUS(fstlir(out_c32,iun,ni,nj,nk,-1,' ',119,-1,-1,' ',' '))     ! complex IEEE 32
    if (ni /= ASIZE/2) then
      write(app_msg, '(A, I8, 1X, A, I8)') 'complex32 expected ni=', ASIZE / 2, 'got: ', ni
      call App_Log(APP_ERROR, app_msg)
      error stop 1
    end if

    CHECK_STATUS(fst_data_length(8))
    CHECK_STATUS(fstlir(out_r64,iun,ni,nj,nk,-1,' ',7,-1,-1,' ',' '))     ! double
    CHECK_STATUS(fstlir(da3,iun,ni,nj,nk,-1,' ',17,-1,-1,' ',' '))     ! IEEE 64

    out_c64 = -1.0
    CHECK_STATUS(fstlir(out_c64,iun,ni,nj,nk,-1,' ',117,-1,-1,' ',' '))     ! complex IEEE 64
    if (ni /= ASIZE/2) then
      write(app_msg, '(A, I8, 1X, A, I8)') 'complex64 expected ni=', ASIZE / 2, 'got: ', ni
      call App_Log(APP_ERROR, app_msg)
      error stop 1
    end if

    ! Check values read
    ! call compare_real32(out_r32, expected_r32, 1e-8, 'out r32', 'expected r32') ! With compression (destroys the data)
    call compare_real32(fa3, expected_r32, 0.0, 'fa3', 'expected r32') ! No compression
    call compare_real32(out_c32, expected_c32, 0.0, 'out_c32', 'expected c32') ! No compression
    ! call compare_real64(out_r64, expected_r64, 0.0_real64, 'out r64', 'expected r64') ! With compression (destroys data)
    call compare_real64(da3, expected_r64, 0.0_real64, 'da3', 'expected r64') ! No compression
    call compare_real64(out_c64, expected_c64, 0.0_real64, 'out c64', 'expected c64') ! No compression

    call App_Log(APP_DEBUG, '---------------------------------------------------------------------------------------------------')
    call App_Log(APP_DEBUG, '#       float  <16> double          int  short   byte   uint ushort  ubyte    float  <IEEE> double&
                      &     complex 32    complex 64')
    call App_Log(APP_DEBUG, '---------------------------------------------------------------------------------------------------')
    do i=1,ASIZE
      call print_values('',i,out_r32(i),out_r64(i),out_i32(i),out_i16(i),out_i8(i),out_u32(i),out_u16(i),out_u8(i),fa3(i),da3(i),out_c32(i),out_c64(i))
    enddo
    call App_Log(APP_INFO, '============= Read back: WITH possible missing value(s) ======')

    CHECK_STATUS(fstlir(out_i32,iun,ni,nj,nk,-1,' ',2,-1,-1,' ',' '))     ! integer with missing
    CHECK_STATUS(fstlir(out_u32,iun,ni,nj,nk,-1,' ',6,-1,-1,' ',' '))    ! unsigned integer with missing
    CHECK_STATUS(fst_data_length(2))
    CHECK_STATUS(fstlir(out_i16,iun,ni,nj,nk,-1,' ',12,-1,-1,' ',' '))    ! signed short with missing
    CHECK_STATUS(fst_data_length(2))
    CHECK_STATUS(fstlir(out_u16,iun,ni,nj,nk,-1,' ',10,-1,-1,' ',' '))   ! unsigned short with missing
    CHECK_STATUS(fst_data_length(1))
    CHECK_STATUS(fstlir(out_i8,iun,ni,nj,nk,-1,' ',14,-1,-1,' ',' '))   ! signed byte with missing
    CHECK_STATUS(fst_data_length(1))
    CHECK_STATUS(fstlir(out_u8,iun,ni,nj,nk,-1,' ',16,-1,-1,' ',' '))  ! unsigned byte with missing

    ! Check values read
    call compare_fields(out_i32, in_i32, 'out i32', 'in i32')
    call compare_fields(out_u32, in_u32, 'out u32', 'in u32')
    call compare_fields(out_i16, in_i16, 'out i16', 'in i16')
    call compare_fields(out_u16, in_u16, 'out u16', 'in u16')
    call compare_fields(out_i8, in_i8, 'out i8', 'in i8')
    call compare_fields(out_u8, in_u8, 'out u8', 'in u8')

    fa3(:) = 0.0

    CHECK_STATUS(fstlir(out_r32,iun,ni,nj,nk,-1,' ',4,-1,-1,' ',' '))     ! float with missing
    CHECK_STATUS(fstlir(fa3,iun,ni,nj,nk,-1,' ',20,-1,-1,' ',' '))     ! IEE 32 with missing
    ! Check values read
    call compare_real32(out_r32, in_r32, 0.0, 'out r32', 'in r32')
    call compare_real32(fa3, in_r32, 0.0, 'fa3', 'in r32')

    CHECK_STATUS(fst_data_length(8))
    CHECK_STATUS(fstlir(out_r64,iun,ni,nj,nk,-1,' ',8,-1,-1,' ',' '))     ! double with missing
    CHECK_STATUS(fst_data_length(8))
    CHECK_STATUS(fstlir(da3,iun,ni,nj,nk,-1,' ',18,-1,-1,' ',' '))     ! double with missing
    ! Check values read
    call compare_real64(out_r64, in_r64, 1.0e-7_real64, 'out_r64', 'in_r64')
    call compare_real64(da3, in_r64, 1.0e-7_real64, 'da3', 'in_r64')

    call App_Log(APP_DEBUG, '---------------------------------------------------------------------------------------------------')
    call App_Log(APP_DEBUG, '#    float  <f16> double             int  short   byte   uint ushort  ubyte     float  <IEEE> double&
                      &     complex 32    complex 64')
    call App_Log(APP_DEBUG, '---------------------------------------------------------------------------------------------------')
    do i=1,ASIZE
      call print_values('', i, out_r32(i), out_r64(i), out_i32(i), out_i16(i), out_i8(i), out_u32(i), out_u16(i), &
                        out_u8(i), fa3(i), da3(i), out_c32(i), out_c64(i))
    enddo

    CHECK_STATUS(fstfrm(iun))

    ! Restore original missing values
    call set_missing_value_flags(old_missing_r32, old_missing_i32, old_missing_u32, old_missing_r64, old_missing_i16, &
                                 old_missing_u16, old_missing_i8, old_missing_u8)

    call App_Log(APP_INFO, 'Test done')

  end subroutine test_missing_values
end module helpers

program fst_missing
  use helpers

  implicit none

  call test_missing_values(.false.)
  call test_missing_values(.true.)

  call App_Log(APP_INFO, 'Test is a success')
  stop

end program fst_missing
