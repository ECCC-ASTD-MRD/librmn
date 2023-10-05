module rsf_mod
  use ISO_C_BINDING
  use ISO_FORTRAN_ENV, only : ERROR_UNIT
  implicit none

#include "fstd98/rsf.hf"
  integer, parameter :: RSF_rw      = RSF_RW
  integer, parameter :: RSF_ro      = RSF_RO
  integer, parameter :: RSF_ap      = RSF_AP
!   integer, parameter :: RSF_nseg    = RSF_NSEG   ! deferred implementation
!   integer, parameter :: RSF_pseg    = RSF_PSEG   ! deferred implementation
  integer, parameter :: RSF_fuse    = RSF_FUSE
  integer, parameter :: RSF_version = RSF_VERSION
  character(len=*), parameter :: RSF_version_string = RSF_VERSION_STRING

  integer, parameter :: RSF_diag_none = RSF_DIAG_NONE 
  integer, parameter :: RSF_diag_error = RSF_DIAG_ERROR
  integer, parameter :: RSF_diag_warn = RSF_DIAG_WARN
  integer, parameter :: RSF_diag_info = RSF_DIAG_INFO
  integer, parameter :: RSF_diag_note = RSF_DIAG_NOTE
  integer, parameter :: RSF_diag_debug0 = RSF_DIAG_DEBUG0
  integer, parameter :: RSF_diag_debug1 = RSF_DIAG_DEBUG1
  integer, parameter :: RSF_diag_debug2 = RSF_DIAG_DEBUG2

  integer, parameter :: RSF_meta_reserved = RSF_META_RESERVED

  type :: RSF_record_info
    type(RSF_record_info_c) :: info
    contains
    procedure, pass :: meta => RSF_record_info_meta
    procedure, pass :: fname => RSF_record_info_fname
  end type RSF_record_info

! the generic interfaces that follow accept RSF_record or RSF_record_handle arguments
! 1 suffixed procedures accept RSF_record (a pointer to RSF_record may be obtained from RSF_New_record)
! 2 suffixed procedures accept RSF_record_handle (from RSF_New_record_handle)

! TODO : add accessor procedures for type(RSF_record_info)

  interface RSF_Valid_record
    module procedure RSF_Valid_record1
    module procedure RSF_Valid_record2
  end interface

  interface RSF_New_record
    module procedure RSF_New_record1        ! short call, allocate space internally
    module procedure RSF_New_record2        ! long call, use caller supplied space
  end interface

  interface RSF_Record_metadata             ! pointer to metadata
    module procedure RSF_Record_metadata1
    module procedure RSF_Record_metadata2
  end interface

  interface RSF_Record_allocsize            ! allocated size in bytes
    module procedure RSF_Record_allocsize1
    module procedure RSF_Record_allocsize2
  end interface

  interface RSF_Record_max_payload          ! max payload size in 32 bit units
    module procedure RSF_Record_max_payload1
    module procedure RSF_Record_max_payload2
  end interface

  interface RSF_Record_payload              ! pointer to payload
    module procedure RSF_Record_payload1
    module procedure RSF_Record_payload2
  end interface

  interface RSF_Free_record                 ! free a record allocated with RSF_New_record_handle
    module procedure RSF_Free_record1
    module procedure RSF_Free_record2
  end interface

 contains

  function RSF_record_info_meta(this) result(dir_meta)  ! get a copy of the record metadata
    implicit none
    class(RSF_record_info), intent(in) :: this
    integer(C_INT32_T), dimension(:), allocatable :: dir_meta

    integer(C_INT32_T), dimension(:), pointer :: temp
    call C_F_POINTER(this%info%meta, temp, [this%info%dir_meta])
    dir_meta(1:this%info%dir_meta) = temp     ! auto allocate dir_meta
  end function RSF_record_info_meta

  function RSF_record_info_fname(this) result(fname)  ! get a copy of the file name associated
    implicit none
    class(RSF_record_info), intent(in) :: this
    character(len=:), allocatable :: fname
    integer :: i, nc
    character(C_CHAR), dimension(:), pointer :: str

    if(C_ASSOCIATED(this%info%fname)) then           ! is there an associated file name ?
      call C_F_POINTER(this%info%fname, str, [4096]) ! Fortran character(len=1) array
      nc = 0
      do while (str(nc+1) .ne. C_NULL_CHAR .and. nc < 4096)  ! find length of C file name string
        nc = nc + 1
      enddo
      allocate(character(len=nc) :: fname)           ! allocate function result with the proper length
      do i = 1, nc
        fname(i:i) = str(i)                          ! copy C string into Fortran string
      enddo
    else
      fname = ""
    endif
  end function RSF_record_info_fname

  function RSF_Record_metadata1(r) result (meta)   ! get pointer to metadata array from record
    implicit none
    type(RSF_record), intent(IN) :: r
    integer(C_INT32_T), dimension(:), pointer :: meta

    ! pointer to metadata array
    call C_F_POINTER(r%meta, meta, [r%rec_meta])      ! rec_meta is in 32 bit units
  end function RSF_Record_metadata1

  function RSF_Record_metadata2(rh) result (meta)   ! get pointer to metadata array from record handle
    implicit none
    type(RSF_record_handle), intent(IN), value :: rh
    integer(C_INT32_T), dimension(:), pointer :: meta
    type(RSF_record), pointer :: r

    call C_F_POINTER(rh%record, r)                          ! handle -> record pointer
    ! pointer to metadata array
    call C_F_POINTER(r%meta, meta, [r%rec_meta])      ! rec_meta is in 32 bit units
  end function RSF_Record_metadata2

  function RSF_Record_allocsize1(r) result(s)
    implicit none
    type(RSF_record), intent(IN) :: r
    integer(C_INT64_T) :: s

    ! allocated size of record
    s = r%rsz                                      ! rsz is in bytes
  end function RSF_Record_allocsize1

  function RSF_Record_allocsize2(rh) result(s)
    implicit none
    type(RSF_record_handle), intent(IN), value :: rh
    integer(C_INT64_T) :: s
    type(RSF_record), pointer :: r

    call C_F_POINTER(rh%record, r)             ! handle -> record pointer
    ! allocated size of record
    s = r%rsz                                  ! rsz is in bytes
  end function RSF_Record_allocsize2

  function RSF_Record_max_payload1(r) result (s)    ! get max size of payload array from record
    implicit none
    type(RSF_record), intent(IN) :: r
    integer(C_INT64_T) :: s

    s = r%max_data / 4                              ! max_data is in bytes
  end function RSF_Record_max_payload1

  function RSF_Record_max_payload2(rh) result (s)   ! get pointer to payload array from record handle
    implicit none
    type(RSF_record_handle), intent(IN), value :: rh
    integer(C_INT64_T) :: s
    type(RSF_record), pointer :: r

    call C_F_POINTER(rh%record, r)             ! handle -> record pointer
    ! pointer to data payload array
    s = r%max_data / 4                         ! max_data is in bytes
  end function RSF_Record_max_payload2

  function RSF_Record_payload1(r) result (data)     ! get pointer to payload array from record
    implicit none
    type(RSF_record), intent(IN) :: r
    integer(C_INT32_T), dimension(:), pointer :: data

    ! pointer to data payload array
    call C_F_POINTER(r%data, data, [r%data_size / 4])  ! data_size is in bytes
  end function RSF_Record_payload1

  function RSF_Record_payload2(rh) result (data)   ! get pointer to payload array from record handle
    implicit none
    type(RSF_record_handle), intent(IN), value :: rh
    integer(C_INT32_T), dimension(:), pointer :: data
    type(RSF_record), pointer :: r

    call C_F_POINTER(rh%record, r)                          ! handle -> record pointer
    ! pointer to data payload array
    call C_F_POINTER(r%data, data, [r%data_size / 4])  ! data_size is in bytes
  end function RSF_Record_payload2

  !> \copydoc RSF_New_record
  function RSF_New_record1(fh, max_data) result(r)      ! return a pointer to RSF_record type
    implicit none
    type(RSF_handle), intent(IN), value :: fh
    integer(C_INT64_T), intent(IN), value :: max_data
    type(RSF_record) ,pointer :: r
    type(RSF_record_handle) :: rh

    rh = RSF_New_record_handle(fh, 0, 0, max_data, C_NULL_PTR, max_data)
    call C_F_POINTER(rh%record, r)                          ! handle -> record pointer
  end function RSF_New_record1

  !> \copydoc RSF_New_record
  function RSF_New_record2(fh, max_data, t, szt) result(r)  ! return a pointer to RSF_record type
    implicit none
    type(RSF_handle), intent(IN), value :: fh
    integer(C_INT64_T), intent(IN), value :: max_data
    type(C_PTR) :: t                                        ! user supplied space
    integer(C_INT64_T), intent(IN), value :: szt            ! size in bytes of user supplied space
    type(RSF_record) ,pointer :: r
    type(RSF_record_handle) :: rh

    rh = RSF_New_record_handle(fh, 0, 0, max_data, t, szt)
    call C_F_POINTER(rh%record, r)                          ! handle -> record pointer
  end function RSF_New_record2

  subroutine RSF_Free_record1(rh)
    implicit none
    type(RSF_record_handle), intent(IN), value :: rh
    call RSF_Free_record_handle(rh)
  end subroutine RSF_Free_record1

  subroutine RSF_Free_record2(r)
    implicit none
    type(RSF_record), intent(IN), target :: r
    type(RSF_record_handle) :: rh
    rh%record = C_LOC(r)
    call RSF_Free_record_handle(rh)
  end subroutine RSF_Free_record2

  function RSF_Valid_record1(rh) result(s)
    implicit none
    type(RSF_record_handle), intent(IN), value :: rh
    integer(C_INT32_T) :: s
    s = RSF_Valid_record_handle(rh)
  end function RSF_Valid_record1

  function RSF_Valid_record2(r) result(s)
    implicit none
    type(RSF_record), intent(IN), target :: r
    integer(C_INT32_T) :: s
    type(RSF_record_handle) :: rh
    rh%record = C_LOC(r)
    s = RSF_Valid_record_handle(rh)
  end function RSF_Valid_record2

end module
