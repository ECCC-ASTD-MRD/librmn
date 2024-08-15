module helpers
  use app
  use rmn_test_helper
  implicit none

  integer :: missing_value_status = 0

contains

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

  subroutine test_missing_values(is_rsf)
    use rmn_fst98
    implicit none
    logical, intent(in) :: is_rsf

#define CHECK_STATUS(cmd) s=cmd;s=vs(s,__LINE__)
#define CHK(cmd) st=cmd;s=vs(st,__LINE__)

    integer, external :: fnom, get_missing_value_flags, encode_missing_value
    external :: set_missing_value_flags

    integer :: junk, status, st, i, s
    integer, parameter :: ASIZE=16             ! ASIZE should be kept even because of complex tests
    integer, dimension(ASIZE) :: ia, ia2, work
    integer, dimension(ASIZE) :: uia, uia2
    integer (2), dimension(ASIZE) :: sa, sa2
    integer (1), dimension(ASIZE) :: ba, ba2
    integer (2), dimension(ASIZE) :: usa, usa2
    integer (1), dimension(ASIZE) :: uba, uba2
    real (4), dimension(ASIZE) :: fa, fa2, fa3
    real (4), dimension(ASIZE) :: ca, ca2, ca3   ! used for 32 bit complex data
    real (8), dimension(ASIZE) :: da, da2, da3
    real (8), dimension(ASIZE) :: za, za2, za3   ! used for 64 bit complex data
    integer :: im, uim
    integer (2) :: sm, usm
    integer (1) :: bm, ubm
    real (4) :: fm
    real (8) :: dm
    integer :: ni, nj, nk
    integer :: iun

    character(len=*), parameter :: test_file_name = 'missing.fst'
    character(len=2000) :: cmd
    logical, parameter :: verbose = .false.

    if (is_rsf) call App_Log(APP_INFO, 'Testing RSF')
    if (.not. is_rsf) call App_Log(APP_INFO, 'Testing XDF')

    status = -1
    st = -1
    fm = -1
    im = -1
    uim = -1
    dm = -1
    sm = -1
    usm = -1
    bm = -1
    ubm = -1

    status = get_missing_value_flags(fm, im, uim, dm, sm, usm, bm, ubm)
    call check_status(status, expected = missing_value_status, fail_message = 'get_missing_value_flags')
    call print_values('Missing values: status', s, fm, dm, im, sm, bm, uim, usm, ubm)
    missing_value_status = 1

    call set_missing_value_flags(fm, im, uim, dm, sm, usm, bm, ubm)
    status = get_missing_value_flags(fm, im, uim, dm, sm, usm, bm, ubm)
    call check_status(status, expected = 1, fail_message = 'get_missing_value_flags')
    call print_values('Missing values: status', s, fm, dm, im, sm, bm, uim, usm, ubm)

    ! ---------------
    ! Initialize data
    do i = 1, ASIZE
      fa(i)  = i*1.0+.5
      ca(i)  = i*1.0+.6
      ia(i)  = i-13
      uia(i) = i+22
      da(i)  = fa(i)+1.2345
      za(i)  = fa(i)+1.3456
      sa(i)  = i-13
      ba(i)  = i-13
      usa(i) = i+22
      uba(i) = i+22
    enddo
    fa(1)  = fm   ; fa(ASIZE)    = fm  ; fa(2) = fm
    ca(1)  = fm   ; ca(ASIZE)    = fm  ; ca(2) = fm
    da(1)  = dm   ; da(ASIZE)    = dm  ; da(ASIZE-1) = dm
    za(1)  = dm   ; za(ASIZE)    = dm  ; za(ASIZE-1) = dm
    ia(2)  = im   ; ia(ASIZE-1)  = im  ; ia(3) = im   ; ia(ASIZE-2) = im
    sa(2)  = sm   ; sa(ASIZE-1)  = sm  ; sa(4) = sm   ; sa(ASIZE-3) = sm
    ba(2)  = bm   ; ba(ASIZE-1)  = bm  ; ba(1) = bm
    uia(3) = uim  ; uia(ASIZE-2) = uim
    usa(3) = usm  ; usa(ASIZE-2) = usm ; usa(ASIZE)=usm
    uba(3) = ubm  ; uba(ASIZE-2) = ubm
    ia(ASIZE/2)  = 125      ! set to 127 to force error message
    sa(ASIZE/2)  = 124      ! set to 127 to force error message
    ba(ASIZE/2)  = 123      ! set to 127 to force error message
    uia(ASIZE/2) = 113      ! set to 255 to force error message
    usa(ASIZE/2) = 112      ! set to 255 to force error message
    uba(ASIZE/2) = 111      ! set to 255 to force error message

    call App_Log(APP_DEBUG, 'Test data:')
    do i = 1, ASIZE
      call print_values('', i, fa(i), da(i), ia(i), sa(i), ba(i), uia(i), usa(i), uba(i), ca(i), za(i))
    enddo

    ! Reset status
    status = -1

    write(cmd, '(A, (1X, A))') 'rm -fv ', test_file_name
    call execute_command_line(trim(cmd))
    ! -----------------------------
    ! Write data into standard file
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
    CHECK_STATUS(fstecr(ia, work, -8,  iun, 0, 0, 0, ASIZE,   1, 1, 1,   0, 0, 'XX', 'YYYY', 'ETIKET', 'X', 0, 0, 0, 0, 4,  0))  ! signed integer
    CHECK_STATUS(fstecr(ia, work, -8,  iun, 0, 0, 0, ASIZE,   1, 1, 2,   0, 0, 'XX', 'YYYY', 'ETIKET', 'X', 0, 0, 0, 0, 68, 0))  ! signed integer with missing

    CHECK_STATUS(fstecr(fa, work, -16, iun, 0, 0, 0, ASIZE,   1, 1, 3,   0, 0, 'XX', 'YYYY', 'ETIKET', 'X', 0, 0, 0, 0, 1,  0))  ! float
    CHECK_STATUS(fstecr(fa, work, -32, iun, 0, 0, 0, ASIZE,   1, 1, 19,  0, 0, 'XX', 'YYYY', 'ETIKET', 'X', 0, 0, 0, 0, 5,  0))  ! IEEE 32
    CHECK_STATUS(fstecr(ca, work, -32, iun, 0, 0, 0, ASIZE/2, 1, 1, 119, 0, 0, 'XX', 'YYYY', 'ETIKET', 'X', 0,0,0,0,8+64+128,0)) ! complex IEEE 32 with missing and compression
    CHECK_STATUS(fstecr(fa, work, -16, iun, 0, 0, 0, ASIZE,   1, 1, 4,   0, 0, 'XX', 'YYYY', 'ETIKET', 'X', 0, 0, 0, 0, 65, 0))  ! float with missing
    CHECK_STATUS(fstecr(fa, work, -32, iun, 0, 0, 0, ASIZE,   1, 1, 20,  0, 0, 'XX', 'YYYY', 'ETIKET', 'X', 0, 0, 0, 0, 69, 0))  ! IEEE 32 with missing

    CHECK_STATUS(fstecr(uia, work, -8, iun, 0, 0, 0, ASIZE,   1, 1, 5,   0, 0, 'XX', 'YYYY', 'ETIKET', 'X', 0, 0, 0, 0, 2,  0))  ! unsigned integer
    CHECK_STATUS(fstecr(uia, work, -8, iun, 0, 0, 0, ASIZE,   1, 1, 6,   0, 0, 'XX', 'YYYY', 'ETIKET', 'X', 0, 0, 0, 0, 66, 0))  ! unsigned integer with missing

    CHECK_STATUS(fst_data_length(8))
    CHECK_STATUS(fstecr(da, work, -16, iun, 0, 0, 0, ASIZE,   1, 1, 7,   0, 0, 'XX', 'YYYY', 'ETIKET', 'X', 0, 0, 0, 0, 1,  0))  ! double
    CHECK_STATUS(fstecr(da, work, -64, iun, 0, 0, 0, ASIZE,   1, 1, 17,  0, 0, 'XX', 'YYYY', 'ETIKET', 'X', 0, 0, 0, 0, 5,  0))  ! IEEE64
    CHECK_STATUS(fstecr(za, work, -64, iun, 0, 0, 0, ASIZE/2, 1, 1, 117, 0, 0, 'XX', 'YYYY', 'ETIKET', 'X',0,0,0,0,8+64+128,0))  ! complex IEEE64 with missing and compression
    CHECK_STATUS(fst_data_length(8))
    CHECK_STATUS(fstecr(da, work, -16, iun, 0, 0, 0, ASIZE,   1, 1, 8,   0, 0, 'XX', 'YYYY', 'ETIKET', 'X', 0, 0, 0, 0, 65, 0)) ! double with missing
    CHECK_STATUS(fstecr(da, work, -64, iun, 0, 0, 0, ASIZE,   1, 1, 18,  0, 0, 'XX', 'YYYY', 'ETIKET', 'X', 0, 0, 0, 0, 69, 0)) ! IEEE64 with missing

    ! if (is_rsf) then
    !   CHECK_STATUS(fstapp(iun, ''))
    !   CHECK_STATUS(fstckp(iun))
    ! end if

    CHECK_STATUS(fst_data_length(2))
    CHECK_STATUS(fstecr(usa, work, -8, iun, 0, 0, 0, ASIZE, 1, 1, 9,  0, 0, 'XX', 'YYYY', 'ETIKET', 'X', 0, 0, 0, 0, 2,  0))  ! unsigned short
    CHECK_STATUS(fst_data_length(2))
    CHECK_STATUS(fstecr(usa, work, -8, iun, 0, 0, 0, ASIZE, 1, 1, 10, 0, 0, 'XX', 'YYYY', 'ETIKET', 'X', 0, 0, 0, 0, 66, 0))  ! unsigned short with missing

    CHECK_STATUS(fst_data_length(2))
    CHECK_STATUS(fstecr(sa,  work, -8, iun, 0, 0, 0, ASIZE, 1, 1, 11, 0, 0, 'XX', 'YYYY', 'ETIKET', 'X', 0, 0, 0, 0, 4,  0))  ! signed short
    CHECK_STATUS(fst_data_length(2))
    CHECK_STATUS(fstecr(sa,  work, -8, iun, 0, 0, 0, ASIZE, 1, 1, 12, 0, 0, 'XX', 'YYYY', 'ETIKET', 'X', 0, 0, 0, 0, 68, 0))  ! signed short with missing

    CHECK_STATUS(fst_data_length(1))
    CHECK_STATUS(fstecr(ba,  work, -8, iun, 0, 0, 0, ASIZE, 1, 1, 13, 0, 0, 'XX', 'YYYY', 'ETIKET', 'X', 0, 0, 0, 0, 4,  0))  ! signed byte
    CHECK_STATUS(fst_data_length(1))
    CHECK_STATUS(fstecr(ba,  work, -8, iun, 0, 0, 0, ASIZE, 1, 1, 14, 0, 0, 'XX', 'YYYY', 'ETIKET', 'X', 0, 0, 0, 0, 68, 0))  ! signed byte with missing

    CHECK_STATUS(fst_data_length(1))
    CHECK_STATUS(fstecr(uba, work, -8, iun, 0, 0, 0, ASIZE, 1, 1, 15, 0, 0, 'XX', 'YYYY', 'ETIKET', 'X', 0, 0, 0, 0, 2,  0))  ! unsigned byte
    CHECK_STATUS(fst_data_length(1))
    CHECK_STATUS(fstecr(uba, work, -8, iun, 0, 0, 0, ASIZE, 1, 1, 16, 0, 0, 'XX', 'YYYY', 'ETIKET', 'X', 0, 0, 0, 0, 66, 0))  ! unsigned byte with missing

    ! Close the file
    CHECK_STATUS(fstfrm(iun))


    ! ----------------------------
    ! Read data from standard file

    !  set new flag values before reading so we can see the difference 'type n' vs 'type n+64'
    fm=-999.99; dm=-888.88; im=-999999; sm=-32768 ; bm=-128; uim=999999 ; usm=32767; ubm=127
    call App_Log(APP_DEBUG, '============================== missing values =============================')
    call App_Log(APP_DEBUG, '#     float        double            int  short   byte   uint ushort  ubyte')
    call set_missing_value_flags(fm,im,uim,dm,sm,usm,bm,ubm)
    call print_values('',status,fm,dm,im,sm,bm,uim,usm,ubm)

    call App_Log(APP_INFO, '============= reading from standard file with and without missing values ============')
    iun = 0
    CHECK_STATUS(fnom(iun, test_file_name, 'STD+RND+OLD+R/O', 0))
    CHECK_STATUS(fstouv(iun,'RND'))

    call App_Log(APP_INFO, '=========== WITHOUT missing value ==========')
    CHECK_STATUS(fstlir(ia2,iun,ni,nj,nk,-1,' ',1,-1,-1,' ',' '))     ! integer

    ! if (.not. all(ia2 == ia)) then
    !   call App_Log(APP_ERROR, 'Integer values are not the same!')
    !   do i = 1, ASIZE, 8
    !     write(app_msg, '(8I12)') ia(i:i+7)
    !     call App_Log(APP_ALWAYS, app_msg)
    !     write(app_msg, '(8I12)') ia2(i : i+7)
    !     call App_Log(APP_ALWAYS, app_msg)
    !   end do
    !   error stop 1
    ! end if

    CHECK_STATUS(fstlir(uia2,iun,ni,nj,nk,-1,' ',5,-1,-1,' ',' '))    ! unsigned integer

    CHECK_STATUS(fstlir(fa2,iun,ni,nj,nk,-1,' ',3,-1,-1,' ',' '))     ! float
    CHECK_STATUS(fstlir(fa3,iun,ni,nj,nk,-1,' ',19,-1,-1,' ',' '))     ! IEEE 32

    ca3 = -1.0
    CHECK_STATUS(fstlir(ca3,iun,ni,nj,nk,-1,' ',119,-1,-1,' ',' '))     ! complex IEEE 32
    if (ni /= ASIZE/2) then
      write(app_msg, '(A, I8, 1X, A, I8)') 'complex32 expected ni=', ASIZE / 2, 'got: ', ni
      call App_Log(APP_ERROR, app_msg)
      error stop 1
    end if

    CHECK_STATUS(fst_data_length(8))
    CHECK_STATUS(fstlir(da2,iun,ni,nj,nk,-1,' ',7,-1,-1,' ',' '))     ! double
    CHECK_STATUS(fstlir(da3,iun,ni,nj,nk,-1,' ',17,-1,-1,' ',' '))     ! IEEE 64
    za3 = -1.0
    CHECK_STATUS(fstlir(za3,iun,ni,nj,nk,-1,' ',117,-1,-1,' ',' '))     ! complex IEEE 64
    if (ni /= ASIZE/2) then
      write(app_msg, '(A, I8, 1X, A, I8)') 'complex64 expected ni=', ASIZE / 2, 'got: ', ni
      call App_Log(APP_ERROR, app_msg)
      error stop 1
    end if

    CHECK_STATUS(fst_data_length(2))
    CHECK_STATUS(fstlir(usa2,iun,ni,nj,nk,-1,' ',9,-1,-1,' ',' '))    ! unsigned short
    CHECK_STATUS(fst_data_length(2))
    CHECK_STATUS(fstlir(sa2,iun,ni,nj,nk,-1,' ',11,-1,-1,' ',' '))    ! signed short

    CHECK_STATUS(fst_data_length(1))
    CHECK_STATUS(fstlir(ba2,iun,ni,nj,nk,-1,' ',13,-1,-1,' ',' '))    ! signed byte
    CHECK_STATUS(fst_data_length(1))
    CHECK_STATUS(fstlir(uba2,iun,ni,nj,nk,-1,' ',15,-1,-1,' ',' '))   ! unsigned byte

    call App_Log(APP_DEBUG, '---------------------------------------------------------------------------------------------------')
    call App_Log(APP_DEBUG, '#       float  <16> double          int  short   byte   uint ushort  ubyte    float  <IEEE> double&
                      &     complex 32    complex 64')
    call App_Log(APP_DEBUG, '---------------------------------------------------------------------------------------------------')
    do i=1,ASIZE
      call print_values('',i,fa2(i),da2(i),ia2(i),sa2(i),ba2(i),uia2(i),usa2(i),uba2(i),fa3(i),da3(i),ca3(i),za3(i))
    enddo
    call App_Log(APP_INFO, '============= WITH possible missing value(s) ======')

    CHECK_STATUS(fstlir(ia2,iun,ni,nj,nk,-1,' ',2,-1,-1,' ',' '))     ! integer with missing
    CHECK_STATUS(fstlir(uia2,iun,ni,nj,nk,-1,' ',6,-1,-1,' ',' '))    ! unsigned integer with missing

    CHECK_STATUS(fstlir(fa2,iun,ni,nj,nk,-1,' ',4,-1,-1,' ',' '))     ! float with missing
    CHECK_STATUS(fstlir(fa3,iun,ni,nj,nk,-1,' ',20,-1,-1,' ',' '))     ! IEE 32 with missing
    CHECK_STATUS(fst_data_length(8))
    CHECK_STATUS(fstlir(da2,iun,ni,nj,nk,-1,' ',8,-1,-1,' ',' '))     ! double with missing
    CHECK_STATUS(fstlir(da3,iun,ni,nj,nk,-1,' ',18,-1,-1,' ',' '))     ! double with missing

    CHECK_STATUS(fst_data_length(2))
    CHECK_STATUS(fstlir(sa2,iun,ni,nj,nk,-1,' ',12,-1,-1,' ',' '))    ! signed short with missing
    CHECK_STATUS(fst_data_length(2))
    CHECK_STATUS(fstlir(usa2,iun,ni,nj,nk,-1,' ',10,-1,-1,' ',' '))   ! unsigned short with missing

    CHECK_STATUS(fst_data_length(1))
    CHECK_STATUS(fstlir(ba2,iun,ni,nj,nk,-1,' ',14,-1,-1,' ',' '))   ! signed byte with missing
    CHECK_STATUS(fst_data_length(1))
    CHECK_STATUS(fstlir(uba2,iun,ni,nj,nk,-1,' ',16,-1,-1,' ',' '))  ! unsigned byte with missing

    call App_Log(APP_DEBUG, '---------------------------------------------------------------------------------------------------')
    call App_Log(APP_DEBUG, '#    float  <f16> double             int  short   byte   uint ushort  ubyte     float  <IEEE> double&
                      &     complex 32    complex 64')
    call App_Log(APP_DEBUG, '---------------------------------------------------------------------------------------------------')
    do i=1,ASIZE
      call print_values('',i,fa2(i),da2(i),ia2(i),sa2(i),ba2(i),uia2(i),usa2(i),uba2(i),fa3(i),da3(i),ca3(i),za3(i))
    enddo

    CHECK_STATUS(fstfrm(iun))

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
