! to use in Fortran code
!  - the fstdxxx and c_fstdxxx interfaces
!  - the fstd98 user defined type and associated type bound procedures
!
! use rmn_fstd98
!
! WARNING : this is MUTUALLY EXCLUSIVE with
! #include <fstd98_interface.hf>
! (will only get the direct fstdxxx and c_fstdxxx interfaces to librmn functions/subprograms)
!
! use rmn_fstd98 will cause the direct fstxxx entry points from librmn to be ignored
! and use instead the equivalent procedures from the module and associated sub modules
!
module rmn_fstd98
  use rmn_common
  implicit none
#define C_INTERFACE_ONLY
#include <fstd98/fstd98_interface.hf>

  interface
    function memset(s, byte, n) result(p) bind(C, name='memset')
      import :: C_PTR, C_SIZE_T, C_INT
      implicit none
      type(C_PTR), intent(IN), VALUE :: s
      integer(C_INT), intent(IN), VALUE :: byte
      integer(C_SIZE_T), intent(IN), VALUE :: n
      type(C_PTR) :: p
    end function memset
  end interface

  interface fstouv
! /*****************************************************************************
!  *                              F S T O U V                                  *
!  *                                                                           *
!  *Object                                                                     *
!  *   Opens a RPN standard file.                                              *
!  *                                                                           *
!  *Arguments                                                                  *
!  *                                                                           *
!  *  IN  iun     unit number associated to the file                           *
!  *  IN  options random or sequential access                                  *
!  *                                                                           *
!  *****************************************************************************/
  module function fstouv_iun(iun, options) result (status)
    implicit none
    integer(C_INT), intent(IN) :: iun
    character(len=*), intent(IN) :: options
    integer(C_INT) :: status
  end function fstouv_iun
  module function fstouv_auto(name, iun, options) result (status) ! calls fnom and fstouv
    implicit none
    integer(C_INT), intent(OUT) :: iun
    character(len=*), intent(IN) :: name
    character(len=*), intent(IN), optional :: options
    integer(C_INT) :: status
  end function fstouv_auto
  end interface

  interface
  module function fstfrm(iun) result (status)
!     implicit none
    integer(C_INT), intent(IN) :: iun
    integer(C_INT) :: status
  end function fstfrm
  end interface

  interface
  ! /***************************************************************************** 
  !  *                              F S T L N K                                  *
  !  *                                                                           * 
  !  *Object                                                                     * 
  !  *   Links a list of files together for search purpose.                      *
  !  *                                                                           * 
  !  *Arguments                                                                  * 
  !  *                                                                           * 
  !  *  IN  liste   list of unit numbers associated to the files                 * 
  !  *  IN  n       number of files to link                                      * 
  !  *                                                                           * 
  !  *****************************************************************************/
  module function fstlnk(link_list,n) result(status) !bind(C,name='c_fstlnk')
    implicit none
    integer(C_INT32_T), intent(IN) :: n
    integer(C_INT32_T), dimension(n) :: link_list
    integer(C_INT32_T) :: status
  end function fstlnk

  !  /***************************************************************************** 
  !  *                              F S T U N L                                  *
  !  *                                                                           * 
  !  *Object                                                                     * 
  !  *   Unlinks a list of files previously linked by fstlnk.                    *
  !  *                                                                           * 
  !  *Arguments                                                                  * 
  !  *                                                                           * 
  !  *****************************************************************************/
  module function fstunl() result(status) !bind(C,name='c_fstunl')
    implicit none
    integer(C_INT32_T) :: status
  end function fstunl
  end interface

  interface
! /***************************************************************************** 
!  *                              F S T I N F                                  *
!  *                                                                           * 
!  *Object                                                                     * 
!  *   Locate the next record that matches the research keys.                  *
!  *                                                                           * 
!  *Arguments                                                                  * 
!  *                                                                           * 
!  *  IN  iun     unit number associated to the file                           * 
!  *  OUT ni      dimension 1 of the data field                                * 
!  *  OUT nj      dimension 2 of the data field                                * 
!  *  OUT nk      dimension 3 of the data field                                * 
!  *  IN  datev   valid date                                                   * 
!  *  IN  etiket  label                                                        * 
!  *  IN  ip1     vertical level                                               * 
!  *  IN  ip2     forecast hour                                                * 
!  *  IN  ip3     user defined identifier                                      * 
!  *  IN  typvar  type of field                                                * 
!  *  IN  nomvar  variable name                                                * 
!  *                                                                           * 
!  *****************************************************************************/
  module function fstinf(iun, ni, nj, nk, datev, etiket, ip1, ip2, ip3, typvar, nomvar) result(handle)
    implicit none
    integer(C_INT), intent(IN) :: iun
    integer(C_INT), intent(OUT) :: ni, nj, nk
    integer(C_INT), intent(IN)  :: datev, ip1, ip2, ip3
    character(len=*), intent(IN) :: typvar, nomvar, etiket
    integer(C_INT) :: handle
  end function fstinf

!  /***************************************************************************** 
!  *                            F S T S U I                                    *
!  *                                                                           * 
!  *Object                                                                     * 
!  *   Finds the next record that matches the last search criterias            *
!  *                                                                           * 
!  *Arguments                                                                  * 
!  *                                                                           * 
!  *  IN  iun     unit number associated to the file                           * 
!  *  OUT ni      dimension 1 of the data field                                * 
!  *  OUT nj      dimension 2 of the data field                                * 
!  *  OUT nk      dimension 3 of the data field                                * 
!  *                                                                           * 
!  *****************************************************************************/
  module function fstsui(iun, ni, nj, nk) result(handle)
    implicit none
    integer(C_INT), intent(IN) :: iun
    integer(C_INT), intent(OUT) :: ni, nj, nk
    integer(C_INT) :: handle
  end function fstsui

! /***************************************************************************** 
!  *                              F S T I N F X                                *
!  *                                                                           * 
!  *Object                                                                     * 
!  *   Locate the next record that matches the research keys.                  *
!  *   The search begins at the position given by the start handle.            * 
!  *                                                                           * 
!  *Arguments                                                                  * 
!  *                                                                           * 
!  *  IN  start   handle from which the search begins                          *
!  *  IN  iun     unit number associated to the file                           * 
!  *  OUT ni      dimension 1 of the data field                                * 
!  *  OUT nj      dimension 2 of the data field                                * 
!  *  OUT nk      dimension 3 of the data field                                * 
!  *  IN  datev   valid date                                                   * 
!  *  IN  etiket  label                                                        * 
!  *  IN  ip1     vertical level                                               * 
!  *  IN  ip2     forecast hour                                                * 
!  *  IN  ip3     user defined identifier                                      * 
!  *  IN  typvar  type of field                                                * 
!  *  IN  nomvar  variable name                                                * 
!  *                                                                           * 
!  *****************************************************************************/
  module function fstinfx(start, iun, ni, nj, nk, datev, etiket, ip1, ip2, ip3, typvar, nomvar) result(handle)
    implicit none
    integer(C_INT), intent(IN) :: iun
    integer(C_INT), intent(IN) :: start
    integer(C_INT), intent(OUT) :: ni, nj, nk
    integer(C_INT), intent(IN)  :: datev, ip1, ip2, ip3
    character(len=*), intent(IN) :: typvar, nomvar, etiket
    integer(C_INT) :: handle
  end function fstinfx

! /***************************************************************************** 
!  *                              F S T I N L                                  *
!  *                                                                           * 
!  *Object                                                                     * 
!  *   Locates all the records that match the research keys.                   *
!  *                                                                           * 
!  *Arguments                                                                  * 
!  *                                                                           * 
!  *  IN  iun     unit number associated to the file                           * 
!  *  OUT ni      dimension 1 of the data field                                * 
!  *  OUT nj      dimension 2 of the data field                                * 
!  *  OUT nk      dimension 3 of the data field                                * 
!  *  IN  datev   valid date                                                   * 
!  *  IN  etiket  label                                                        * 
!  *  IN  ip1     vertical level                                               * 
!  *  IN  ip2     forecast hour                                                * 
!  *  IN  ip3     user defined identifier                                      * 
!  *  IN  typvar  type of field                                                * 
!  *  IN  nomvar  variable name                                                *
!  *  OUT liste   list of handles to the records                               *
!  *  OUT infon   number of elements for the list (number of records found)    *
!  *  OUT nmax    dimension of list as given by caller                         *
!  *                                                                           * 
!  *****************************************************************************/
  module function fstinl(iun, ni, nj, nk, datev, etiket, ip1, ip2, ip3, typvar, nomvar, liste, infon, nmax) result(status)
    implicit none
    integer(C_INT), intent(IN) :: iun
    integer(C_INT), intent(OUT) :: ni, nj, nk
    integer(C_INT), intent(IN)  :: datev, ip1, ip2, ip3, nmax
    character(len=*), intent(IN) :: typvar, nomvar, etiket
    integer(C_INT), intent(OUT) :: infon
    integer(C_INT), dimension(nmax), intent(OUT) :: liste
    integer(C_INT) :: status
  end function fstinl

  end interface

  interface
! /*****************************************************************************
!  *                              F S T P R M                                  *
!  *                                                                           *
!  *Object                                                                     *
!  *   Get all the description informations of the record.                     *
!  *                                                                           *
!  *Arguments                                                                  *
!  *                                                                           *
!  *  IN  handle  positioning information to the record                        *
!  *  OUT date    date time stamp                                              *
!  *  OUT deet    length of a time step in seconds                             *
!  *  OUT npas    time step number                                             *
!  *  OUT ni      first dimension of the data field                            *
!  *  OUT nj      second dimension of the data field                           *
!  *  OUT nk      third dimension of the data field                            * 
!  *  OUT nbits   number of bits kept for the elements of the field            * 
!  *  OUT datyp   data type of the elements                                    * 
!  *  OUT ip1     vertical level                                               * 
!  *  OUT ip2     forecast hour                                                * 
!  *  OUT ip3     user defined identifier                                      * 
!  *  OUT typvar  type of field (forecast, analysis, climatology)              * 
!  *  OUT nomvar  variable name                                                * 
!  *  OUT etiket  label                                                        * 
!  *  OUT grtyp   type of geographical projection                              * 
!  *  OUT ig1     first grid descriptor                                        * 
!  *  OUT ig2     second grid descriptor                                       * 
!  *  OUT ig3     third grid descriptor                                        * 
!  *  OUT ig4     fourth grid descriptor                                       * 
!  *  OUT swa     starting word address                                        * 
!  *  OUT lng     record length                                                * 
!  *  OUT dltf    delete flag                                                  * 
!  *  OUT ubc     unused bit count                                             * 
!  *  OUT extra1  extra parameter                                              * 
!  *  OUT extra2  extra parameter                                              * 
!  *  OUT extra3  extra parameter                                              * 
!  *                                                                           * 
!  *****************************************************************************/
  module function fstprm(handle, date, deet, npas, ni, nj, nk, nbits, datyp, ip1, ip2, ip3, &
                    typvar, nomvar, etiket, grtyp, &
                    ig1, ig2, ig3, ig4, swa, lng, dlft, ubc, extra1, extra2, extra3)        &
                  result(status)
    implicit none
    integer, intent(IN) :: handle
    integer, intent(OUT) :: date, deet, npas, ni, nj, nk, nbits, datyp, ip1, ip2, ip3
    integer, intent(OUT) :: ig1, ig2, ig3, ig4, swa, lng, dlft, ubc, extra1, extra2, extra3
    character(len=*), intent(OUT) :: typvar, nomvar, etiket, grtyp
    integer(C_INT32_T) :: status

  end function fstprm

! /***************************************************************************** 
!  *                              F S T V O I                                  *
!  *                                                                           * 
!  *Object                                                                     * 
!  *   Opens a RPN standard file.                                              *
!  *                                                                           * 
!  *Arguments                                                                  * 
!  *                                                                           * 
!  *  IN  iun     unit number associated to the file                           * 
!  *  IN  options random or sequential access                                  * 
!  *                                                                           * 
!  *****************************************************************************/

  module function fstvoi(iun, options) result(status)
    implicit none
    integer(C_INT), intent(IN) :: iun
    character(len=*), intent(IN) :: options
    integer(C_INT) :: status
  end function fstvoi

! /***************************************************************************** 
!  *                           F S T  _ V E R S I O N                          *
!  *                                                                           * 
!  *Object                                                                     * 
!  *   Returns package version number.                                          *
!  *                                                                           * 
!  *****************************************************************************/

  module function fst_version(dummy) result(vers)
    implicit none
    integer(C_INT), intent(IN), optional :: dummy
    integer(C_INT) :: vers
  end function fst_version
!   module function fst_version() result(vers)
!     implicit none
!     integer(C_INT) :: vers
!   end function fst_version

! /***************************************************************************** 
!  *                              F S T N B R                                  *
!  *                                                                           * 
!  *Object                                                                     * 
!  *   Returns the number of records of the file associated with unit number.  *
!  *                                                                           * 
!  *Arguments                                                                  * 
!  *                                                                           * 
!  *  IN  iun     unit number associated to the file                           * 
!  *                                                                           * 
!  *****************************************************************************/
  module function fstnbr(iun) result (nrec)
    implicit none
    integer(C_INT), intent(IN) :: iun
    integer(C_INT) :: nrec
  end function fstnbr

! /***************************************************************************** 
!  *                              F S T N B R V                                *
!  *                                                                           * 
!  *Object                                                                     * 
!  *   Returns the number of valid records (excluding deleted records) of the  *
!  *   file associated with unit number.                                       *
!  *                                                                           * 
!  *Arguments                                                                  * 
!  *                                                                           * 
!  *  IN  iun     unit number associated to the file                           * 
!  *                                                                           * 
!  *****************************************************************************/
  module function fstnbrv(iun) result (status)
    implicit none
    integer(C_INT), intent(IN) :: iun
    integer(C_INT) :: status
  end function fstnbrv

!  /*****************************************************************************
!  *                              F S T C H E C K                              *
!  *                                                                           *
!  *Object                                                                     *
!  *   Checks if an RPN standard file is valid.                                *
!  *                                                                           *
!  *Arguments                                                                  *
!  *                                                                           *
!  *  IN  filename Path of the file to be checked                              *
!  *                                                                           *
!  *****************************************************************************/
  module function fstcheck(path) result(status)
    implicit none
    character(len=*), intent(IN) :: path
    integer(C_INT) :: status
  end function fstcheck
  end interface

  interface

! /***************************************************************************** 
!  *                              F S T L U K                                  *
!  *                                                                           * 
!  *Object                                                                     * 
!  *   Read the record at position given by handle.                            *
!  *                                                                           * 
!  *Arguments                                                                  * 
!  *                                                                           * 
!  *  OUT field   data field to be read                                        * 
!  *  IN  handle  positioning information to the record                        * 
!  *  OUT ni      dimension 1 of the data field                                * 
!  *  OUT nj      dimension 2 of the data field                                * 
!  *  OUT nk      dimension 3 of the data field                                * 
!  *                                                                           * 
!  *****************************************************************************/
  module function fstluk(field, handle, ni, nj, nk) result(handle_out)
    implicit none
#define IgnoreTypeKindRank field
#define ExtraAttributes 
#include <rmn/IgnoreTypeKindRank.hf>
    integer(C_INT), intent(IN) :: handle
    integer(C_INT), intent(OUT) :: ni, nj, nk
    integer(C_INT) :: handle_out
  end function fstluk
! /***************************************************************************** 
!  *                              F S T L I R X                                *
!  *                                                                           * 
!  *Object                                                                     * 
!  *   Reads the next record that matches the research keys.                   *
!  *   The search begins at the position given by the start handle.            * 
!  *                                                                           * 
!  *Arguments                                                                  * 
!  *                                                                           * 
!  *  OUT field   data field to be read                                        * 
!  *  IN  start   handle from which the search begins                          *
!  *  IN  iun     unit number associated to the file                           * 
!  *  OUT ni      dimension 1 of the data field                                * 
!  *  OUT nj      dimension 2 of the data field                                * 
!  *  OUT nk      dimension 3 of the data field                                * 
!  *  IN  datev   valid date                                                   * 
!  *  IN  etiket  label                                                        * 
!  *  IN  ip1     vertical level                                               * 
!  *  IN  ip2     forecast hour                                                * 
!  *  IN  ip3     user defined identifier                                      * 
!  *  IN  typvar  type of field                                                * 
!  *  IN  nomvar  variable name                                                * 
!  *                                                                           * 
!  *****************************************************************************/
  module function fstlirx(field, start, iun, ni, nj, nk, datev, etiket, ip1, ip2, ip3, typvar, nomvar) result(handle)
    implicit none
#define IgnoreTypeKindRank field
#define ExtraAttributes 
#include <rmn/IgnoreTypeKindRank.hf>
    integer(C_INT), intent(IN) :: iun, start
    integer(C_INT), intent(OUT) :: ni, nj, nk
    integer(C_INT), intent(IN)  :: datev, ip1, ip2, ip3
    character(len=*), intent(IN) :: typvar, nomvar, etiket
    integer(C_INT) :: handle
  end function fstlirx

! /***************************************************************************** 
!  *                              F S T L I R                                  *
!  *                                                                           * 
!  *Object                                                                     * 
!  *   Reads the next record that matches the research keys.                   *
!  *                                                                           * 
!  *Arguments                                                                  * 
!  *                                                                           * 
!  *  IN  iun     unit number associated to the file                           * 
!  *  OUT field   data field to be read                                        * 
!  *  OUT ni      dimension 1 of the data field                                * 
!  *  OUT nj      dimension 2 of the data field                                * 
!  *  OUT nk      dimension 3 of the data field                                * 
!  *  IN  datev   valid date                                                   * 
!  *  IN  etiket  label                                                        * 
!  *  IN  ip1     vertical level                                               * 
!  *  IN  ip2     forecast hour                                                * 
!  *  IN  ip3     user defined identifier                                      * 
!  *  IN  typvar  type of field                                                * 
!  *  IN  nomvar  variable name                                                * 
!  *                                                                           * 
!  *****************************************************************************/
  module function fstlir(field, iun, ni, nj, nk, datev, etiket, ip1, ip2, ip3, typvar, nomvar) result(handle)
    implicit none
#define IgnoreTypeKindRank field
#define ExtraAttributes 
#include <rmn/IgnoreTypeKindRank.hf>
    integer(C_INT), intent(IN) :: iun
    integer(C_INT), intent(OUT) :: ni, nj, nk
    integer(C_INT), intent(IN)  :: datev, ip1, ip2, ip3
    character(len=*), intent(IN) :: typvar, nomvar, etiket
    integer(C_INT) :: handle
  end function fstlir

  module function fstlir_d(dblewords, iun, ni, nj, nk, datev, etiket, ip1, ip2, ip3, typvar, nomvar) result(handle)
    implicit none
#define IgnoreTypeKindRank dblewords
#define ExtraAttributes 
#include <rmn/IgnoreTypeKindRank.hf>
    integer(C_INT), intent(IN) :: iun
    integer(C_INT), intent(OUT) :: ni, nj, nk
    integer(C_INT), intent(IN)  :: datev, ip1, ip2, ip3
    character(len=*), intent(IN) :: typvar, nomvar, etiket
    integer(C_INT) :: handle
  end function fstlir_d

  module function fstlir_h(halfwords, iun, ni, nj, nk, datev, etiket, ip1, ip2, ip3, typvar, nomvar) result(handle)
    implicit none
#define IgnoreTypeKindRank halfwords
#define ExtraAttributes 
#include <rmn/IgnoreTypeKindRank.hf>
    integer(C_INT), intent(IN) :: iun
    integer(C_INT), intent(OUT) :: ni, nj, nk
    integer(C_INT), intent(IN)  :: datev, ip1, ip2, ip3
    character(len=*), intent(IN) :: typvar, nomvar, etiket
    integer(C_INT) :: handle
  end function fstlir_h

  module function fstlir_b(bytes, iun, ni, nj, nk, datev, etiket, ip1, ip2, ip3, typvar, nomvar) result(handle)
    implicit none
#define IgnoreTypeKindRank bytes
#define ExtraAttributes 
#include <rmn/IgnoreTypeKindRank.hf>
    integer(C_INT), intent(IN) :: iun
    integer(C_INT), intent(OUT) :: ni, nj, nk
    integer(C_INT), intent(IN)  :: datev, ip1, ip2, ip3
    character(len=*), intent(IN) :: typvar, nomvar, etiket
    integer(C_INT) :: handle
  end function fstlir_b

! /***************************************************************************** 
!  *                              F S T L I R _ S                              *
!  *                                                                           * 
!  *Object                                                                     * 
!  *   Reads the next record that matches the research keys.                   *
!  *                                                                           * 
!  *Arguments                                                                  * 
!  *                                                                           * 
!  *  IN  iun     unit number associated to the file                           * 
!  *  OUT string  character string to be read                                  * 
!  *  OUT ni      dimension 1 of the data field                                * 
!  *  OUT nj      dimension 2 of the data field                                * 
!  *  OUT nk      dimension 3 of the data field                                * 
!  *  IN  datev   valid date                                                   * 
!  *  IN  etiket  label                                                        * 
!  *  IN  ip1     vertical level                                               * 
!  *  IN  ip2     forecast hour                                                * 
!  *  IN  ip3     user defined identifier                                      * 
!  *  IN  typvar  type of field                                                * 
!  *  IN  nomvar  variable name                                                * 
!  *                                                                           * 
!  *****************************************************************************/
! ftnword f77name(fstlir_s)(void *string, ftnword *f_iun,
!                         ftnword *f_ni, ftnword *f_nj,
!                         ftnword *f_nk, ftnword *f_datev, char *f_etiket,
!                         ftnword *f_ip1, ftnword *f_ip2, ftnword *f_ip3,
!                         char *f_typvar, char *f_nomvar,
!                         int lng_string, F2Cl ll1, F2Cl ll2, F2Cl ll3)
  module function fstlir_s(field, iun, ni, nj, nk, datev, etiket, ip1, ip2, ip3, typvar, nomvar, lngstr) result(handle)
    implicit none
#define IgnoreTypeKindRank field
#define ExtraAttributes ,target
#include <rmn/IgnoreTypeKindRank.hf>
    integer(C_INT), intent(IN) :: iun
    integer(C_INT), intent(OUT) :: ni, nj, nk
    integer(C_INT), intent(IN)  :: datev, ip1, ip2, ip3, lngstr
    character(len=*), intent(IN) :: typvar, nomvar, etiket
    integer(C_INT) :: handle
  end function fstlir_s

! /***************************************************************************** 
!  *                            F S T L I S                                    *
!  *                                                                           * 
!  *Object                                                                     * 
!  *   Reads the next record that matches the last search criterias            *
!  *                                                                           * 
!  *Arguments                                                                  * 
!  *                                                                           * 
!  *  OUT field   data field to be read                                        * 
!  *  IN  iun     unit number associated to the file                           * 
!  *  OUT ni      dimension 1 of the data field                                * 
!  *  OUT nj      dimension 2 of the data field                                * 
!  *  OUT nk      dimension 3 of the data field                                * 
!  *                                                                           * 
!  *****************************************************************************/
! int c_fstlis(word *field, int iun, int *ni, int *nj, int *nk)
! ftnword f77name(fstlis)(word *field, ftnword *f_iun,
!                         ftnword *f_ni, ftnword *f_nj, ftnword *f_nk)
  module function fstlis(field, iun, ni, nj, nk) result(handle)
    implicit none
#define IgnoreTypeKindRank field
#define ExtraAttributes 
#include <rmn/IgnoreTypeKindRank.hf>
    integer(C_INT), intent(IN) :: iun
    integer(C_INT), intent(OUT) :: ni, nj, nk
    integer(C_INT) :: handle
  end function fstlis

! /***************************************************************************** 
!  *                             F S T L I C                                   *
!  *                                                                           * 
!  *Object                                                                     * 
!  *   Search for a record that matches the research keys and check that the   *
!  *   remaining parmeters match the record descriptors                        *
!  *                                                                           *
!  *Arguments                                                                  * 
!  *                                                                           * 
!  *  OUT field    data field to be read                                       * 
!  *  IN  iun      unit number associated to the file                          * 
!  *  IN  niin     dimension 1 of the data field                               * 
!  *  IN  njin     dimension 2 of the data field                               * 
!  *  IN  nkin     dimension 3 of the data field                               * 
!  *  IN  datev    valid date                                                  * 
!  *  IN  etiket   label                                                       * 
!  *  IN  ip1      vertical level                                              * 
!  *  IN  ip2      forecast hour                                               * 
!  *  IN  ip3      user defined identifier                                     * 
!  *  IN  typvar   type of field                                               * 
!  *  IN  nomvar   variable name                                               * 
!  *  IN  ig1      first grid descriptor                                       * 
!  *  IN  ig2      second grid descriptor                                      * 
!  *  IN  ig3      third grid descriptor                                       * 
!  *  IN  ig4      fourth grid descriptor                                      * 
!  *  IN  grtyp    type of geographical projection                             * 
!  *                                                                           * 
!  *****************************************************************************/
  module function fstlic(field, iun, ni, nj, nk, datev, etiket, ip1, ip2, ip3, typvar, nomvar, &
                  ig1, ig2, ig3, ig4, grtyp) result(handle)
    implicit none
#define IgnoreTypeKindRank field
#define ExtraAttributes 
#include <rmn/IgnoreTypeKindRank.hf>
    integer(C_INT), intent(IN) :: iun
    integer(C_INT), intent(OUT) :: ni, nj, nk
    integer(C_INT), intent(IN)  :: datev, ip1, ip2, ip3
    character(len=*), intent(IN) :: typvar, nomvar, etiket, grtyp
    integer(C_INT), intent(IN)  :: ig1, ig2, ig3, ig4
    integer(C_INT) :: handle
  end function fstlic
  end interface

  interface

! /***************************************************************************** 
!  *                              F S T E C R                                  *
!  *                                                                           * 
!  *Object                                                                     * 
!  *   Writes a record into a file.                                                  *
!  *                                                                           * 
!  *Arguments                                                                  * 
!  *                                                                           * 
!  *  IN  field   field to write to the file                                   * 
!  *  IN  work    work field (kept for backward compatibility)                 * 
!  *  IN  npak    number of bits kept for the elements of the field (-npak)    * 
!  *  IN  iun     unit number associated to the file                           * 
!  *  IN  date    date time stamp                                              * 
!  *  IN  deet    length of a time step in seconds                             * 
!  *  IN  npas    time step number                                             * 
!  *  IN  ni      first dimension of the data field                            * 
!  *  IN  nj      second dimension of the data field                           * 
!  *  IN  nk      third dimension of the data field                            * 
!  *  IN  ip1     vertical level                                               * 
!  *  IN  ip2     forecast hour                                                * 
!  *  IN  ip3     user defined identifier                                      * 
!  *  IN  typvar  type of field (forecast, analysis, climatology)              * 
!  *  IN  nomvar  variable name                                                * 
!  *  IN  etiket  label                                                        * 
!  *  IN  grtyp   type of geographical projection                              * 
!  *  IN  ig1     first grid descriptor                                        * 
!  *  IN  ig2     second grid descriptor                                       * 
!  *  IN  ig3     third grid descriptor                                        * 
!  *  IN  ig4     fourth grid descriptor                                       * 
!  *  IN  datyp   data type of the elements                                    * 
!  *  IN  rewrit  rewrite flag (true=rewrite existing record, false=append)    *
!  *                                                                           * 
!  *****************************************************************************/
  module subroutine fstecr(field, work, npak, iun, date, deet, npas, ni, nj, nk, &
                    ip1, ip2, ip3, typvar, nomvar, etiket, &
                    grtyp, ig1, ig2, ig3, ig4, datyp, rewrite)
    implicit none
#define IgnoreTypeKindRank field, work
#define ExtraAttributes 
#include <rmn/IgnoreTypeKindRank.hf>
    integer(C_INT), intent(IN) :: iun
    integer(C_INT), intent(IN) :: npak, date, deet, npas, ni, nj, nk, datyp, rewrite
    integer(C_INT), intent(IN) :: ip1, ip2, ip3, ig1, ig2, ig3, ig4
    character(len=*), intent(IN) :: typvar, nomvar, etiket, grtyp
  end subroutine fstecr

  module function fstecr_fn(field, work, npak, iun, date, deet, npas, ni, nj, nk, &
                            ip1, ip2, ip3, typvar, nomvar, etiket, &
                            grtyp, ig1, ig2, ig3, ig4, datyp, rewrite) result(status)
    implicit none
#define IgnoreTypeKindRank field, work
#define ExtraAttributes 
#include <rmn/IgnoreTypeKindRank.hf>
    integer(C_INT), intent(IN) :: iun
    integer(C_INT), intent(IN) :: npak, date, deet, npas, ni, nj, nk, datyp, rewrite
    integer(C_INT), intent(IN) :: ip1, ip2, ip3, ig1, ig2, ig3, ig4
    character(len=*), intent(IN) :: typvar, nomvar, etiket, grtyp
    integer(C_INT) :: status
  end function fstecr_fn

  module subroutine fstecr_d(dblewords, work, npak, iun, date, deet, npas, ni, nj, nk, &
                      ip1, ip2, ip3, typvar, nomvar, etiket, &
                      grtyp, ig1, ig2, ig3, ig4, datyp, rewrite)
    implicit none
#define IgnoreTypeKindRank dblewords, work
#define ExtraAttributes 
#include <rmn/IgnoreTypeKindRank.hf>
    integer(C_INT), intent(IN) :: iun
    integer(C_INT), intent(IN) :: npak, date, deet, npas, ni, nj, nk, datyp, rewrite
    integer(C_INT), intent(IN) :: ip1, ip2, ip3, ig1, ig2, ig3, ig4
    character(len=*), intent(IN) :: typvar, nomvar, etiket, grtyp
  end subroutine fstecr_d

  module subroutine fstecr_h(halfwords, work, npak, iun, date, deet, npas, ni, nj, nk, &
                      ip1, ip2, ip3, typvar, nomvar, etiket, &
                      grtyp, ig1, ig2, ig3, ig4, datyp, rewrite)
    implicit none
#define IgnoreTypeKindRank halfwords, work
#define ExtraAttributes 
#include <rmn/IgnoreTypeKindRank.hf>
    integer(C_INT), intent(IN) :: iun
    integer(C_INT), intent(IN) :: npak, date, deet, npas, ni, nj, nk, datyp, rewrite
    integer(C_INT), intent(IN) :: ip1, ip2, ip3, ig1, ig2, ig3, ig4
    character(len=*), intent(IN) :: typvar, nomvar, etiket, grtyp
  end subroutine fstecr_h

  module subroutine fstecr_b(bytes, work, npak, iun, date, deet, npas, ni, nj, nk, &
                      ip1, ip2, ip3, typvar, nomvar, etiket, &
                      grtyp, ig1, ig2, ig3, ig4, datyp, rewrite)
    implicit none
#define IgnoreTypeKindRank bytes, work
#define ExtraAttributes 
#include <rmn/IgnoreTypeKindRank.hf>
    integer(C_INT), intent(IN) :: iun
    integer(C_INT), intent(IN) :: npak, date, deet, npas, ni, nj, nk, datyp, rewrite
    integer(C_INT), intent(IN) :: ip1, ip2, ip3, ig1, ig2, ig3, ig4
    character(len=*), intent(IN) :: typvar, nomvar, etiket, grtyp
  end subroutine fstecr_b

! /***************************************************************************** 
!  *                              F S T E C R _ S                              *
!  *                                                                           * 
!  *Object                                                                     * 
!  *   Writes record to file.                                                  *
!  *                                                                           * 
!  *Arguments                                                                  * 
!  *                                                                           * 
!  *  IN  string  character string to write to the file                        * 
!  *  IN  work    work field (kept for backward compatibility)                 * 
!  *  IN  npak    number of bits kept for the elements of the field (-npak)    * 
!  *  IN  iun     unit number associated to the file                           * 
!  *  IN  date    date time stamp                                              * 
!  *  IN  deet    length of a time step in seconds                             * 
!  *  IN  npas    time step number                                             * 
!  *  IN  ni      first dimension of the data field                            * 
!  *  IN  nj      second dimension of the data field                           * 
!  *  IN  nk      third dimension of the data field                            * 
!  *  IN  ip1     vertical level                                               * 
!  *  IN  ip2     forecast hour                                                * 
!  *  IN  ip3     user defined identifier                                      * 
!  *  IN  typvar  type of field (forecast, analysis, climatology)              * 
!  *  IN  nomvar  variable name                                                * 
!  *  IN  etiket  label                                                        * 
!  *  IN  grtyp   type of geographical projection                              * 
!  *  IN  ig1     first grid descriptor                                        * 
!  *  IN  ig2     second grid descriptor                                       * 
!  *  IN  ig3     third grid descriptor                                        * 
!  *  IN  ig4     fourth grid descriptor                                       * 
!  *  IN  datyp   data type of the elements                                    * 
!  *  IN  rewrit  rewrite flag (true=rewrite existing record, false=append)    *
!  *                                                                           * 
!  *****************************************************************************/
  module function fstecr_s(field, work, npak, iun, date, deet, npas, ni, nj, nk, &
                    ip1, ip2, ip3, typvar, nomvar, etiket, &
                    grtyp, ig1, ig2, ig3, ig4, datyp, rewrite, lngstr) result(status)
    implicit none
#define IgnoreTypeKindRank field, work
#define ExtraAttributes 
#include <rmn/IgnoreTypeKindRank.hf>
    integer(C_INT), intent(IN) :: iun
    integer(C_INT), intent(IN) :: npak, date, deet, npas, ni, nj, nk, datyp, rewrite
    integer(C_INT), intent(IN) :: ip1, ip2, ip3, ig1, ig2, ig3, ig4, lngstr
    character(len=*), intent(IN) :: typvar, nomvar, etiket, grtyp
    integer(C_INT) :: status
  end function fstecr_s

  module function fstecr_str(string, work, npak, iun, date, deet, npas, ni, nj, nk, &
                      ip1, ip2, ip3, typvar, nomvar, etiket, &
                      grtyp, ig1, ig2, ig3, ig4, datyp, rewrite) result(status)
    implicit none
    character(len=*), intent(IN) :: string
#define IgnoreTypeKindRank work
#define ExtraAttributes 
#include <rmn/IgnoreTypeKindRank.hf>
    integer(C_INT), intent(IN) :: iun
    integer(C_INT), intent(IN) :: npak, date, deet, npas, ni, nj, nk, datyp, rewrite
    integer(C_INT), intent(IN) :: ip1, ip2, ip3, ig1, ig2, ig3, ig4
    character(len=*), intent(IN) :: typvar, nomvar, etiket, grtyp
    integer(C_INT) :: status
  end function fstecr_str

! /***************************************************************************** 
!  *                             F S T E F F                                   *
!  *                                                                           * 
!  *Object                                                                     * 
!  *   Deletes the record associated to handle.                                *
!  *                                                                           * 
!  *Arguments                                                                  * 
!  *                                                                           * 
!  *  IN  handle  handle to the record to delete                               * 
!  *                                                                           * 
!  *****************************************************************************/
  module function fsteff(handle) result (status)
    implicit none
    integer(C_INT), intent(IN) :: handle
    integer(C_INT) :: status
  end function fsteff
  end interface

  interface
! /***************************************************************************** 
!  *                      F S T _ D A T A _ L E N G T H                        *
!  *                                                                           * 
!  *Object                                                                     * 
!  *   Gives information on data lenght of the elements passed to fstecr       *
!  *   and fstlir (double, short integer, byte ...)                            *
!  *                                                                           * 
!  *Arguments                                                                  * 
!  *                                                                           * 
!  *  IN  length_type     data length kind                                     * 
!  *                      1: byte                                              *
!  *                      2: short (16 bits)                                   *
!  *                      4: regular 32 bits                                   *
!  *                      8: double (64 bits)                                  *
!  *                                                                           * 
!  *****************************************************************************/
  module function fst_data_length(l) result(status)

    implicit none
    integer, intent(IN) :: l
    integer :: status
  end function fst_data_length

! /***************************************************************************** 
!  *                             I P n _ A L L                                 *
!  *                                                                           * 
!  *Object                                                                     * 
!  *   Generates all possible coded ip1 values for a given level               *
!  *                                                                           * 
!  *Arguments                                                                  * 
!  *                                                                           * 
!  *  IN  level          ip1 level (float value)                               * 
!  *  IN  kind           level kind as defined in convip_plus                  * 
!  *                                                                           * 
!  *****************************************************************************/

  module function ip1_all(level, vkind) result(ip_new)
    implicit none
    real(C_FLOAT), intent(IN) :: level
    integer(C_INT), intent(IN) :: vkind
    integer(C_INT) :: ip_new
  end function ip1_all
  module function ip2_all(level, vkind) result(ip_new)
    implicit none
    real(C_FLOAT), intent(IN) :: level
    integer(C_INT), intent(IN) :: vkind
    integer(C_INT) :: ip_new
  end function ip2_all
  module function ip3_all(level, vkind) result(ip_new)
    implicit none
    real(C_FLOAT), intent(IN) :: level
    integer(C_INT), intent(IN) :: vkind
    integer(C_INT) :: ip_new
  end function ip3_all

! /***************************************************************************** 
!  *                             I P n _ V A L                                 *
!  *                                                                           * 
!  *Object                                                                     * 
!  *   Generates all possible coded ip1 values for a given level               *
!  *                                                                           * 
!  *Arguments                                                                  * 
!  *                                                                           * 
!  *  IN  level          ip1 level (float value)                               * 
!  *  IN  kind           level kind as defined in convip_plus                  * 
!  *                                                                           * 
!  *****************************************************************************/

  module function ip1_val(level, vkind) result(ip_new)
    implicit none
    real(C_FLOAT), intent(IN) :: level
    integer(C_INT), intent(IN) :: vkind
    integer(C_INT) :: ip_new
  end function ip1_val
  module function ip2_val(level, vkind) result(ip_new)
    implicit none
    real(C_FLOAT), intent(IN) :: level
    integer(C_INT), intent(IN) :: vkind
    integer(C_INT) :: ip_new
  end function ip2_val
  module function ip3_val(level, vkind) result(ip_new)
    implicit none
    real(C_FLOAT), intent(IN) :: level
    integer(C_INT), intent(IN) :: vkind
    integer(C_INT) :: ip_new
  end function ip3_val

!  /*****************************************************************************
!  *                              F S T O P I                                  *
!  *                                                                           * 
!  *Object                                                                     * 
!  *   Print out or set a fstd or xdf global variable option.                  *
!  *                                                                           * 
!  *Arguments                                                                  * 
!  *                                                                           * 
!  *   IN     option   option name to be set/printed                           * 
!  *   IN     value    option value                                            * 
!  *   IN     getmode  logical (1: get option, 0: set option)                  * 
!  *                                                                           * 
!  *****************************************************************************/

  module function fstopi(option, val, getmode) result(status)
    implicit none
    character(len=*), intent(IN) :: option
    integer(C_INT), intent(IN) :: val, getmode
    integer(C_INT) :: status
  end function fstopi

!  /***************************************************************************** 
!  *                              F S T O P L                                  *
!  *                                                                           * 
!  *Object                                                                     * 
!  *   Print out or set a fstd or xdf global variable option.                  *
!  *                                                                           * 
!  *Arguments                                                                  * 
!  *                                                                           * 
!  *   IN     option   option name to be set/printed                           * 
!  *   IN     value    option value                                            * 
!  *   IN     getmode  logical (1: get option, 0: set option)                  * 
!  *                                                                           * 
!  *****************************************************************************/

  module function fstopl(option, val, getmode) result(status)
    implicit none
    character(len=*), intent(IN) :: option
    logical, intent(IN) :: val
    integer(C_INT), intent(IN) :: getmode
    integer(C_INT) :: status
  end function fstopl

!  /***************************************************************************** 
!  *                              F S T O P R                                  *
!  *                                                                           * 
!  *Object                                                                     * 
!  *   Print out or set a fstd or xdf global variable option.                  *
!  *                                                                           * 
!  *Arguments                                                                  * 
!  *                                                                           * 
!  *   IN     option   option name to be set/printed                           * 
!  *   IN     value    option value                                            * 
!  *   IN     getmode  logical (1: get option, 0: set option)                  * 
!  *                                                                           * 
!  *****************************************************************************/

  module function fstopr(option, val, getmode) result(status)
    implicit none
    character(len=*), intent(IN) :: option
    real(C_FLOAT), intent(IN) :: val
    integer(C_INT), intent(IN) :: getmode
    integer(C_INT) :: status
  end function fstopr

! /*****************************************************************************
!  *                              F S T O P C                                  *
!  *                                                                           *
!  *Object                                                                     *
!  *   Print out or set a fstd or xdf global variable option.                  *
!  *                                                                           *
!  *Arguments                                                                  *
!  *                                                                           *
!  *   IN     option   option name to be set/printed                           *
!  *   IN     value    option value                                            *
!  *   IN     getmode  logical (1: get option, 0: set option)                  *
!  *                                                                           *
!  *****************************************************************************/

  module function fstopc(option, val, getmode) result(status)
    implicit none
    character(len=*), intent(IN) :: option
    character(len=*), intent(IN) :: val
    integer(C_INT), intent(IN) :: getmode
    integer(C_INT) :: status
  end function fstopc

!  /***************************************************************************** 
!  *                   F S T R E S E T _ I P _ F L A G S                       *
!  *                                                                           * 
!  *Object                                                                     * 
!  *   Reset all the flags previously set by ip(1-3)_val                       *
!  *                                                                           * 
!  *****************************************************************************/
  module subroutine fstreset_ip_flags(dummy) ! aocc
    implicit none
    integer, optional :: dummy
  end subroutine fstreset_ip_flags
!   module subroutine fstreset_ip_flags()
!     implicit none
!   end subroutine fstreset_ip_flags
  end interface

  interface

! /***************************************************************************** 
!  *                                F S T C K P                                *
!  *                                                                           * 
!  *Object                                                                     * 
!  *   Checkpoint. Clear buffers, rewrite headers.                             *
!  *                                                                           * 
!  *Arguments                                                                  * 
!  *                                                                           * 
!  *  IN  iun     unit number associated to the file                           * 
!  *                                                                           * 
!  *****************************************************************************/
  module function fstckp(iun) result (status)
    implicit none
    integer(C_INT), intent(IN) :: iun
    integer(C_INT) :: status
  end function fstckp

! /*****************************************************************************
!  *                            F S T M S Q                                    *
!  *                                                                           *
!  *Object                                                                     *
!  *   Mask a portion of the research keys.                                    *
!  *                                                                           *
!  *Arguments                                                                  *
!  *                                                                           *
!  *   IN    iun     unit number associated to the file                        *
!  * IN/OUT  mip1    mask for vertical level                                   *
!  * IN/OUT  mip2    mask for forecast hour                                    *
!  * IN/OUT  mip3    mask for ip3 (user defined identifier)                    *
!  * IN/OUT  metiket mask for label                                            *
!  *   IN    getmode logical (1: getmode 0:set mode)                           *
!  *                                                                           *
!  *****************************************************************************/
  module function fstmsq(iun, ip1, ip2, ip3, etiket, getmode) result(status)
    implicit none
    integer(C_INT), intent(IN) :: iun
    integer(C_INT), intent(INOUT) :: ip1, ip2, ip3
    character(len=*), intent(INOUT) :: etiket
    integer(C_INT), intent(IN) :: getmode
    integer(C_INT) :: status
  end function fstmsq

  end interface

contains
end module rmn_fstd98
