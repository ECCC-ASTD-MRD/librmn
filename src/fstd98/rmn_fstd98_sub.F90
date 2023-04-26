! Copyright (C) 2022  Environnement et Changement climatique Canada
!
! This is free software; you can redistribute it and/or
! modify it under the terms of the GNU Lesser General Public
! License as published by the Free Software Foundation,
! version 2.1 of the License.
!
! This software is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
! Lesser General Public License for more details.
!
! Author:
!     M. Valin,   Recherche en Prevision Numerique, 2022
!
! this file contains several submodules and could possibly be split into several files
! the F2C_CHAR and F2C_TRIM macros might be needed in more thatn 1 file
!
submodule(rmn_fstd98) fstd98_file
  implicit none
! #define F2C_CHAR(fstr)  (fstr//achar(0))
! #define F2C_TRIM(fstr) (trim(fstr)//achar(0))
contains

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
    ! c_fstouv will need to process option 'RSF'
    status = c_fstouv(iun, trim(options)//achar(0))
  end function

  module function fstouv_auto(name, iun, options) result (status) ! calls fnom and fstouv
    implicit none
    integer(C_INT), intent(OUT) :: iun
    character(len=*), intent(IN) :: name
    character(len=*), intent(IN), optional :: options
    integer(C_INT) :: status
    integer, external :: fnom

    iun = 0
    if(present(options)) then
      status = fnom(iun, trim(name), trim(options), 0)
      status = fstouv_iun(iun, trim(options))
    else
      status = fnom(iun, trim(name),'STD+RND',0)
      status = fstouv_iun(iun, 'RND')
    endif
  end function

  module function ouviun(this, iun, options) result (status) ! calls fstouv
    implicit none
    class(fstd98), intent(INOUT) :: this
    integer(C_INT), intent(IN) :: iun
    character(len=*), intent(IN) :: options
    integer(C_INT) :: status
    this%iun = iun
    status = fstouv(iun, options)
  end function

  module function ouvauto(this, name, options) result (status) ! calls fnom and fstouv
    implicit none
    class(fstd98), intent(INOUT) :: this
    character(len=*), intent(IN) :: name
    character(len=*), intent(IN), optional :: options
    integer(C_INT) :: status
    integer(C_INT) :: iun
    integer, external :: fnom

    iun = 0
    if(present(options)) then
      status = fnom(iun, trim(name), trim(options), 0)
      status = fstouv(iun, trim(options))
    else
      status = fnom(iun, trim(name),'STD+RND',0)
      status = fstouv(iun, 'RND')
    endif
    this%iun = iun
  end function

! /***************************************************************************** 
!  *                              F S T F R M                                  *
!  *                                                                           * 
!  *Object                                                                     * 
!  *   Closes a RPN standard file.                                             *
!  *                                                                           * 
!  *Arguments                                                                  * 
!  *                                                                           * 
!  *  IN  iun     unit number associated to the file                           * 
!  *                                                                           * 
!  *****************************************************************************/
  module procedure fstfrm
    implicit none
    status = c_fstfrm(iun)
  end procedure
  module procedure frm
    implicit none
    status = fstfrm(this%iun)
    this%iun = -1
  end procedure

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
  module procedure fstlnk
    implicit none
    integer(C_INT), dimension(:), allocatable :: links_list
    integer(C_INT) :: link_n
    allocate(links_list(n))
    link_n = n
    links_list(1:n) = link_list(1:n)
    status = c_xdflnk(links_list, link_n)
  end procedure

!  /***************************************************************************** 
!  *                              F S T U N L                                  *
!  *                                                                           * 
!  *Object                                                                     * 
!  *   Unlinks a list of files previously linked by fstlnk.                    *
!  *                                                                           * 
!  *Arguments                                                                  * 
!  *                                                                           * 
!  *  IN  liste   list of unit numbers associated to the files                 * 
!  *  IN  n       number of files to link                                      * 
!  *                                                                           * 
!  *****************************************************************************/
  module procedure fstunl
    implicit none
    status = c_xdfunl(link_list,n)
  end procedure
end
! ==============================================================================
submodule(rmn_fstd98) fstd_98_optn
  use f_c_strings_mod
  implicit none
contains
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
  module procedure fst_data_length
    implicit none
    status = c_fst_data_length(l)
  end procedure

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

    module procedure ip1_all
      implicit none
      ip_new = c_ip1_all(level, vkind)
    end procedure
    module procedure ip2_all
      implicit none
      ip_new = c_ip2_all(level, vkind)
    end procedure
    module procedure ip3_all
      implicit none
      ip_new = c_ip3_all(level, vkind)
    end procedure

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

    module procedure ip1_val
      implicit none
      ip_new = c_ip1_val(level, vkind)
    end procedure
    module procedure ip2_val
      implicit none
      ip_new = c_ip2_val(level, vkind)
    end procedure
    module procedure ip3_val
      implicit none
      ip_new = c_ip3_val(level, vkind)
    end procedure

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

  module procedure fstopi
    implicit none
    status = c_fstopi(f_c_string(option), val, getmode)
  end procedure

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

  module procedure fstopl
    implicit none
    integer(C_INT) :: val_int
    val_int = 0
    if(val) val_int = 1
    status = c_fstopl(f_c_string(option), val_int, getmode)
  end procedure

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

  module procedure fstopr
    implicit none
    status = c_fstopr(f_c_string(option), val, getmode)
  end procedure

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

  module procedure fstopc
    implicit none
    status = c_fstopc(f_c_string(option), f_c_string(val), getmode)
  end procedure

!  /***************************************************************************** 
!  *                   F S T R E S E T _ I P _ F L A G S                       *
!  *                                                                           * 
!  *Object                                                                     * 
!  *   Reset all the flags previously set by ip(1-3)_val                       *
!  *                                                                           * 
!  *****************************************************************************/
    module procedure fstreset_ip_flags
      call c_fstreset_ip_flags()
    end procedure
end
! ==============================================================================
submodule(rmn_fstd98) fstd_98_inf
  use f_c_strings_mod
  implicit none
contains
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
  module procedure fstinf
    implicit none
    character(C_CHAR), dimension(5)  :: nom
    character(C_CHAR), dimension(3)  :: typ
    character(C_CHAR), dimension(13) :: eti
    call strncpy_f2c(typvar, typ, 3)
    call strncpy_f2c(nomvar, nom, 5)
    call strncpy_f2c(etiket, eti, 13)
    handle = c_fstinf(iun, ni, nj, nk, datev, eti, ip1, ip2, ip3, typ, nom)
  end procedure

  module procedure inf
    implicit none
    handle = fstinf(this%iun, ni, nj, nk, datev, etiket, ip1, ip2, ip3, typvar, nomvar)
  end procedure

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
  module procedure fstsui
    implicit none
    handle = c_fstsui(iun, ni, nj, nk)
  end procedure

  module procedure sui
    implicit none
    handle = c_fstsui(this%iun, ni, nj, nk)
  end procedure

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
  module procedure fstinfx
    implicit none
    character(C_CHAR), dimension(5)  :: nom
    character(C_CHAR), dimension(3)  :: typ
    character(C_CHAR), dimension(13) :: eti
    call strncpy_f2c(typvar, typ, 3)
    call strncpy_f2c(nomvar, nom, 5)
    call strncpy_f2c(etiket, eti, 13)
    handle = c_fstinfx(start, iun, ni, nj, nk, datev, eti, ip1, ip2, ip3, typ, nom)
  end procedure

  module procedure infx
    implicit none
    handle = fstinfx(start, this%iun, ni, nj, nk, datev, etiket, ip1, ip2, ip3, typvar, nomvar)
  end procedure

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
  module procedure fstinl
    implicit none
    character(C_CHAR), dimension(5)  :: nom
    character(C_CHAR), dimension(3)  :: typ
    character(C_CHAR), dimension(13) :: eti
    call strncpy_f2c(typvar, typ, 3)
    call strncpy_f2c(nomvar, nom, 5)
    call strncpy_f2c(etiket, eti, 13)
    status = c_fstinl(iun, ni, nj, nk, datev, eti, ip1, ip2, ip3, typ, nom, liste, infon, nmax)
  end procedure

  module procedure inl
    implicit none
    status = fstinl(this%iun, ni, nj, nk, datev, etiket, ip1, ip2, ip3, typvar, nomvar, liste, infon, nmax)
  end procedure

end
! ==============================================================================
submodule(rmn_fstd98) fstd_98_lir
  use f_c_strings_mod
  implicit none
contains

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
  module procedure fstluk
    implicit none
    handle_out = c_fstluk(field, handle, ni, nj, nk)
  end procedure

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
  module procedure fstlirx
    implicit none
    character(len=5)  :: nom
    character(len=3)  :: typ
    character(len=13) :: eti
    call strncpy_f2c(typvar, typ, 3)
    call strncpy_f2c(nomvar, nom, 5)
    call strncpy_f2c(etiket, eti, 13)
    handle = c_fstlirx(field, start, iun, ni, nj, nk, datev, eti, ip1, ip2, ip3, typ, nom)
  end procedure

  module procedure lirx
    implicit none
    character(len=5)  :: nom
    character(len=3)  :: typ
    character(len=13) :: eti
    call strncpy_f2c(typvar, typ, 3)
    call strncpy_f2c(nomvar, nom, 5)
    call strncpy_f2c(etiket, eti, 13)
    handle = c_fstlirx(field, start, this%iun, ni, nj, nk, datev, eti, ip1, ip2, ip3, typ, nom)
  end procedure

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
  module procedure fstlir
    implicit none
    character(len=5)  :: nom
    character(len=3)  :: typ
    character(len=13) :: eti
    call strncpy_f2c(typvar, typ, 3)
    call strncpy_f2c(nomvar, nom, 5)
    call strncpy_f2c(etiket, eti, 13)
    handle = c_fstlir(field, iun, ni, nj, nk, datev, eti, ip1, ip2, ip3, typ, nom)
  end procedure

  module procedure lir
    implicit none
    handle = fstlir(field, this%iun, ni, nj, nk, datev, etiket, ip1, ip2, ip3, typvar, nomvar)
  end procedure

  module procedure fstlir_d
    implicit none
    integer :: status
    status = fst_data_length(8)
    handle = fstlir(dblewords, iun, ni, nj, nk, datev, etiket, ip1, ip2, ip3, typvar, nomvar)
    status = fst_data_length(4)
  end procedure

  module procedure lir_d
    implicit none
    handle = fstlir_d(dblewords, this%iun, ni, nj, nk, datev, etiket, ip1, ip2, ip3, typvar, nomvar)
  end procedure

  module procedure fstlir_h
    implicit none
    integer :: status
    status = fst_data_length(2)
    handle = fstlir(halfwords, iun, ni, nj, nk, datev, etiket, ip1, ip2, ip3, typvar, nomvar)
    status = fst_data_length(4)
  end procedure

  module procedure lir_h
    implicit none
    handle = fstlir_h(halfwords, this%iun, ni, nj, nk, datev, etiket, ip1, ip2, ip3, typvar, nomvar)
  end procedure

  module procedure fstlir_b
    implicit none
    integer :: status
    status = fst_data_length(1)
    handle = fstlir(bytes, iun, ni, nj, nk, datev, etiket, ip1, ip2, ip3, typvar, nomvar)
    status = fst_data_length(4)
  end procedure

  module procedure lir_b
    implicit none
    handle = fstlir_b(bytes, this%iun, ni, nj, nk, datev, etiket, ip1, ip2, ip3, typvar, nomvar)
  end procedure

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
  module procedure fstlir_s
    implicit none
    integer(C_SIZE_T) :: nset
    type(C_PTR) :: p
    nset = lngstr
    p = memset(C_LOC(field), ichar(' '), nset)
    handle = fstlir(field, iun, ni, nj, nk, datev, etiket, ip1, ip2, ip3, typvar, nomvar)
  end procedure

  module procedure lir_s
    implicit none
    handle = fstlir_s(field, this%iun, ni, nj, nk, datev, etiket, ip1, ip2, ip3, typvar, nomvar, lngstr)
  end procedure

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
  module procedure fstlis
    implicit none
    handle = c_fstlis(field, iun, ni, nj, nk)
  end procedure

  module procedure lis
    implicit none
    handle = c_fstlis(field, this%iun, ni, nj, nk)
  end procedure

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
  module procedure fstlic
    implicit none
    character(len=5)  :: nom
    character(len=3)  :: typ
    character(len=13) :: eti
    character(len=2)  :: gty
    call strncpy_f2c(typvar, typ, 3)
    call strncpy_f2c(nomvar, nom, 5)
    call strncpy_f2c(etiket, eti, 13)
    call strncpy_f2c(grtyp,  gty, 2)
    handle = c_fstlic(field, iun, ni, nj, nk, datev, eti, ip1, ip2, ip3, typ, nom, &
                      ig1, ig2, ig3, ig4, gty)
  end procedure

end
! ==============================================================================
submodule(rmn_fstd98) fstd_98_ecr
  use f_c_strings_mod
  implicit none
contains

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
  module procedure fstecr
    implicit none
    integer(C_INT) :: status
    status = fstecr_fn(field, work, npak, iun, date, deet, npas, ni, nj, nk, &
                ip1, ip2, ip3, typvar, nomvar, etiket, grtyp,  &
                ig1, ig2, ig3, ig4, datyp, rewrite)
  end procedure

  module procedure fstecr_fn
    implicit none
    character(len=4)  :: nom
    character(len=2)  :: typ
    character(len=1)  :: gty
    character(len=12) :: eti
    nom = nomvar
    typ = typvar
    gty = grtyp
    eti = etiket
    status = c_fstecr(field, work, npak, iun, date, deet, npas, ni, nj, nk, &
                      ip1, ip2, ip3, f_c_string(typ), f_c_string(nom), f_c_string(eti), f_c_string(gty),  &
                      ig1, ig2, ig3, ig4, datyp, rewrite)
  end procedure fstecr_fn

  module procedure ecr
    implicit none
    status = fstecr_fn(field, work, npak, this%iun, date, deet, npas, ni, nj, nk, &
                ip1, ip2, ip3, typvar, nomvar, etiket, grtyp,  &
                ig1, ig2, ig3, ig4, datyp, rewrite)
  end procedure

  module procedure fstecr_d
    implicit none
    integer :: status
    status = fst_data_length(8)
    call fstecr(dblewords, work, npak, iun, date, deet, npas, ni, nj, nk, &
                ip1, ip2, ip3, typvar, nomvar, etiket, grtyp,  &
                ig1, ig2, ig3, ig4, datyp, rewrite)
    status = fst_data_length(4)
  end procedure

  module procedure ecr_d
    implicit none
    call fstecr_d(dblewords, work, npak, this%iun, date, deet, npas, ni, nj, nk, &
                      ip1, ip2, ip3, typvar, nomvar, etiket, grtyp,  &
                      ig1, ig2, ig3, ig4, datyp, rewrite)
  end procedure

  module procedure fstecr_h
    implicit none
    integer :: status
    status = fst_data_length(2)
    call fstecr(halfwords, work, npak, iun, date, deet, npas, ni, nj, nk, &
                ip1, ip2, ip3, typvar, nomvar, etiket, grtyp,  &
                ig1, ig2, ig3, ig4, datyp, rewrite)
    status = fst_data_length(4)
  end procedure

  module procedure ecr_h
    implicit none
    call fstecr_h(halfwords, work, npak, this%iun, date, deet, npas, ni, nj, nk, &
                      ip1, ip2, ip3, typvar, nomvar, etiket, grtyp,  &
                      ig1, ig2, ig3, ig4, datyp, rewrite)
  end procedure

  module procedure fstecr_b
    implicit none
    integer :: status
    status = fst_data_length(1)
    call fstecr(bytes, work, npak, iun, date, deet, npas, ni, nj, nk, &
                ip1, ip2, ip3, typvar, nomvar, etiket, grtyp,  &
                ig1, ig2, ig3, ig4, datyp, rewrite)
    status = fst_data_length(4)
  end procedure

  module procedure ecr_b
    implicit none
    call fstecr_b(bytes, work, npak, this%iun, date, deet, npas, ni, nj, nk, &
                      ip1, ip2, ip3, typvar, nomvar, etiket, grtyp,  &
                      ig1, ig2, ig3, ig4, datyp, rewrite)
  end procedure

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
  module procedure fstecr_s
    implicit none
    integer ninjnk
    character(len=4)  :: nom
    character(len=2)  :: typ
    character(len=1)  :: gty
    character(len=12) :: eti
    status = 1
    ninjnk = max(1,ni) * max(1,nj) * max(1,nk)
!     if (ninjnk > lngstr * nj) return
    if (ninjnk > lngstr) return
    nom = nomvar
    typ = typvar
    gty = grtyp
    eti = etiket
    call fstecr(field, work, npak, iun, date, deet, npas, ni, nj, nk, &
                ip1, ip2, ip3, f_c_string(typ), f_c_string(nom), f_c_string(eti), f_c_string(gty),  &
                ig1, ig2, ig3, ig4, datyp, rewrite)
    status = 0
  end procedure

  module procedure ecr_s
    implicit none
    status = fstecr_s(field, work, npak, this%iun, date, deet, npas, ni, nj, nk, &
                      ip1, ip2, ip3, typvar, nomvar, etiket, grtyp,  &
                      ig1, ig2, ig3, ig4, datyp, rewrite, lngstr)
    status = 0
  end procedure

  module procedure fstecr_str
    implicit none
    integer ninjnk
    ninjnk = max(1,ni) * max(1,nj) * max(1,nk)
    status = 1
    if (ninjnk > len(string)) return
    status = fstecr_s(string, work, npak, iun, date, deet, npas, ni, nj, nk, &
                      ip1, ip2, ip3, typvar, nomvar, etiket, grtyp,  &
                      ig1, ig2, ig3, ig4, datyp, rewrite, len(string))
  end procedure

  module procedure ecr_str
    implicit none
    status = fstecr_str(string, work, npak, this%iun, date, deet, npas, ni, nj, nk, &
                      ip1, ip2, ip3, typvar, nomvar, etiket, grtyp,  &
                      ig1, ig2, ig3, ig4, datyp, rewrite)
  end procedure

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
  module procedure fsteff
    implicit none
    status = c_fsteff(handle)
  end procedure

end
! ==============================================================================
submodule(rmn_fstd98) fstd_98_util
  use f_c_strings_mod
  implicit none
contains
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
  module procedure fstprm
    implicit none
    character(len=5)  :: nom
    character(len=3)  :: typ
    character(len=2)  :: gty
    character(len=13) :: eti
    nom = ' ' ; nom(5:5) = achar(0)
    typ = ' ' ; typ(3:3) = achar(0)
    gty = ' ' ; gty(2:2) = achar(0)
    eti = ' ' ; eti(13:13) = achar(0)
    status = c_fstprm(handle, date, deet, npas, ni, nj, nk, nbits, datyp, ip1, ip2, ip3,  &
                  typ, nom, eti, gty,                                                     &
                  ig1, ig2, ig3, ig4, swa, lng, dlft, ubc, extra1, extra2, extra3)
    typvar = typ(1:2)
    nomvar = nom(1:4)
    grtyp = gty(1:1)
    etiket = eti(1:12)
  end procedure

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
  module procedure fstvoi
    implicit none
    status = c_fstvoi(iun, f_c_string(options))
  end procedure

  module procedure voi
    implicit none
    status = fstvoi(this%iun, f_c_string(options))
  end procedure

  module procedure fstd98_is_rsf
    implicit none
    integer :: is_rsf_status
    integer :: dummy_arg

    is_rsf = .false.
    is_rsf_status = c_is_rsf(this % iun, dummy_arg)
    if (is_rsf_status == 1) is_rsf = .true.
  end procedure

  ! /***************************************************************************** 
  !  *                           F S T  _ V E R S I O N                          *
  !  *                                                                           * 
  !  *Object                                                                     * 
  !  *   Returns package version number.                                          *
  !  *                                                                           * 
  !  *****************************************************************************/
  module procedure fst_version
    implicit none
    vers = c_fst_version()
  end procedure

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
  module procedure fstnbr
    implicit none
    nrec = c_fstnbr(iun)
  end procedure

  module procedure nbr
    implicit none
    nrec = c_fstnbr(this%iun)
  end procedure

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
  module procedure fstnbrv
    implicit none
    status = c_fstnbrv(iun)
  end procedure

  module procedure nbrv
    implicit none
    nrecv = c_fstnbrv(this%iun)
  end procedure

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
  module procedure fstckp
    implicit none
    status = c_fstckp(iun)
  end procedure
  module procedure ckp
    implicit none
    status = fstckp(this%iun)
  end procedure

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
  module procedure fstcheck
    implicit none
    status = c_fstcheck(f_c_string(path))
  end procedure

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
  module procedure fstmsq
    implicit none
    status = c_fstmsq(iun, ip1, ip2, ip3, etiket, getmode)
  end procedure

  module procedure msq
    implicit none
    status = c_fstmsq(this % iun, ip1, ip2, ip3, etiket, getmode)
  end procedure

end submodule
