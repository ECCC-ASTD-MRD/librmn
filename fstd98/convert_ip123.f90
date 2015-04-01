!/* RMNLIB - Library of useful routines for C and FORTRAN programming
! * Copyright (C) 1975-2001  Division de Recherche en Prevision Numerique
! *                          Environnement Canada
! *
! * This library is free software; you can redistribute it and/or
! * modify it under the terms of the GNU Lesser General Public
! * License as published by the Free Software Foundation,
! * version 2.1 of the License.
! *
! * This library is distributed in the hope that it will be useful,
! * but WITHOUT ANY WARRANTY; without even the implied warranty of
! * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
! * Lesser General Public License for more details.
! *
! * You should have received a copy of the GNU Lesser General Public
! * License along with this library; if not, write to the
! * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
! * Boston, MA 02111-1307, USA.
! */
module convert_ip123
use ISO_C_BINDING

public  :: encode_ip_0, encode_ip_1, decode_ip_0, decode_ip_1
public  :: encode_ip_2, encode_ip_3, decode_ip_2, decode_ip_3
public  :: convip_plus, test_convip_plus, test_value_to_string
public  :: value_to_string
private :: conv_kind_15

type, BIND(C) :: float_ip
real(C_FLOAT) :: lo, hi
integer(C_INT) :: kind
end type

integer, public, parameter :: TO_IP=1
integer, public, parameter :: TO_RP=-1
integer, public, parameter :: CONVERT_OK=0
integer, public, parameter :: CONVERT_GUESS=14
integer, public, parameter :: CONVERT_GOOD_GUESS=2
integer, public, parameter :: CONVERT_BAD_GUESS=4
integer, public, parameter :: CONVERT_TERRIBLE_GUESS=8
integer, public, parameter :: CONVERT_WARNING=32
integer, public, parameter :: CONVERT_ERROR=64

integer, public, parameter :: KIND_ABOVE_SEA=0
integer, public, parameter :: KIND_SIGMA=1
integer, public, parameter :: KIND_PRESSURE=2
integer, public, parameter :: KIND_ARBITRARY=3
integer, public, parameter :: KIND_ABOVE_GND=4
integer, public, parameter :: KIND_HYBRID=5
integer, public, parameter :: KIND_THETA=6
integer, public, parameter :: KIND_HOURS=10
integer, public, parameter :: KIND_SAMPLES=15
integer, public, parameter :: KIND_MTX_IND=17
integer, public, parameter :: KIND_M_PRES=21

interface encode_ip
module procedure encode_ip_0
module procedure encode_ip_1
module procedure encode_ip_2
module procedure encode_ip_3
end interface

interface decode_ip
module procedure decode_ip_0
module procedure decode_ip_1
module procedure decode_ip_2
module procedure decode_ip_3
end interface

integer, private, parameter :: Max_Kind=31
!  1 means coordinate of type kind is ascending ( larger value = higher in the atmosphere )
! -1 means coordinate of type kind is descending ( larger value = lower in the atmosphere )
!  0 means coordinate of type kind cannot be deemed ascending nor descending
! kind = 0, 4, 21 ascending ( meters above ground, meters above msl, galchen meters )
! kind = 1, 2     descending (pressure, sigma)
! kind = 3, 5, 6  neither (arbitrary, hybrid, theta)
! non level coordinates are considered as neutral
integer, private, save, dimension(0:Max_Kind) :: order = &
  (/  1, -1, -1,  0,  1,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0, &
      0,  0,  0,  0,  0,  1,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0  /)

integer, private, save, dimension(0:Max_Kind) :: islevel = &
  (/  1,  1,  1,  1,  1,  1,  1,  0,  0,  0,  0,  0,  0,  0,  0,  0, &
      0,  0,  0,  0,  0,  1,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0  /)

private :: swap, swapi, is_invalid_kind, is_level, ascending, descending

contains
!===============================================================================================
function is_level(kind) result(status)  ! is this kind a level ?
  logical :: status
  integer, intent(IN) :: kind
  status = .false.
  if(kind < 31 .or. kind < 0) status = islevel(kind)==1
end function is_level
!===============================================================================================
function ascending(kind) result(status) ! is this kind "ascending" (larger value higher in atmosphere) ?
  logical :: status
  integer, intent(IN) :: kind
  status = .false.
  if(kind < 31 .or. kind < 0) status = order(kind)==1
end function ascending
!===============================================================================================
function descending(kind) result(status) ! is this kind "descending" (larger value lower in atmosphere) ?
  logical :: status
  integer, intent(IN) :: kind
  status = .false.
  if(kind < 31 .or. kind < 0) status = order(kind)==-1
end function descending
!===============================================================================================
function is_invalid_kind(kind) result(status) ! is this kind invalid ?
  logical :: status
  integer, intent(IN) :: kind
  status=.false.
  if(kind<0) status=.true.
  if(kind>Max_Kind .and. iand(kind,15)/=15) status=.true.
end function is_invalid_kind
!===============================================================================================
subroutine swapi(a,b)  ! swap a pair of integer values
  integer(C_INT), intent(INOUT) :: a,b
  integer(C_INT) :: t
  t = a ; a = b ; b = t
  return
end subroutine swapi
!===============================================================================================
subroutine swap(a,b)  ! swap a pair of real values
  real(C_FLOAT), intent(INOUT) :: a,b
  real(C_FLOAT) :: t
  t = a ; a = b ; b = t
  return
end subroutine swap
!===============================================================================================
!
! produce a valid (ip1,ip2,ip3) triplet from (real value,kind) pairs
! RP1 must contain a level (or a pair of levels) in the atmosphere
! RP2 must contain  a time (or a pair of times)
! RP3 may contain anything, RP3%hi will be ignored (if RP1 or RP2 contains a pair, RP3 is ignored)
! this routine is C interoperable
!
! the function returns CONVERT_ERROR in case of error, CONVERT_OK otherwise
!
!notes: some reordering may take place when RP! or RP@ contains a pair
!       levels: ip1 will be lower in atmosphere than ip2
!       times:  ip2 will be the end of the time range, ip3 will be the start of the time range
!
!       RP1 not a level or RP2 not a time will be flagged as an error
!       RP1 and RP2 both containing a range will be flagged as an error
!       in case of error, ip1,2,3 will be returned as -1
!
function encode_ip_0(IP1,IP2,IP3,RP1,RP2,RP3) result(status) BIND (C,name='EncodeIp')
  implicit none  ! coupled (rp1,rp2,rp3) to (ip1,ip2,ip3) conversion with type enforcement

  integer(C_INT) :: status
  integer(C_INT), intent(OUT) :: IP1,IP2,IP3
  type(float_ip), intent(IN)  :: RP1,RP2,RP3

  real*4, dimension(3) :: P
  integer, dimension(3) ::kind
  character(len=1) :: dummy
  integer :: i

  status=CONVERT_ERROR
  i=0
  IP1=-1 ; IP2=-1 ; IP3=-1
  P = 0
  kind = -1
  if(is_invalid_kind(RP1%kind) .or. is_invalid_kind(RP2%kind)) return   ! OOPS, invalid kind for RP1 or RP2

  if( is_level(RP1%kind) ) then  !  RP1 is a valid level kind
    P(1)=RP1%lo ; kind(1)=RP1%kind ; i=i+1
    if (RP1%hi /= RP1%lo) then       ! RP1 is a range
      P(3)=RP1%hi ; kind(3)=RP1%kind ; i=i+1
      if(RP1%hi < RP1%lo .and. ascending(RP1%kind)) call swap(P(1),p(3))  ! keep lo, hi in atmospheric ascending order
      if(RP1%hi > RP1%lo .and. descending(RP1%kind)) call swap(P(1),p(3))  ! i.e. level lo lower in atmosphere than level hi
    endif
  else
    return  ! ERROR, RP1 must be a level
  endif
  
  if(RP2%kind == 10) then             !  RP2 is a valid time kind
    P(2)=RP2%lo ; kind(2)=RP2%kind ; i=i+1
    if (RP2%hi /= RP2%lo) then  ! time range
      P(3)=RP2%hi ; kind(3)=RP2%kind ; i=i+1
      if(RP2%hi > RP2%lo) call swap(P(2),P(3)) ! keep times in descending order p(2) > p(3) => ip2 > ip3
    endif
  else
    return  ! ERROR, RP2 must be a time
  endif
  
  if(i>3) return  ! OOPS, we have 2 ranges
  
  if(i /= 3) then   !  no range was found, RP3 comes into play
    if(is_invalid_kind(RP3%kind)) return ! OOPS, invalid kind for RP3
    P(3)=RP3%lo ; kind(3)=RP3%kind ; i=i+1
  endif
  
  call convip_plus(IP1,P(1),kind(1),+2,dummy,.false.)  ! NEW style encoding not negotiable
  call convip_plus(IP2,P(2),kind(2),+2,dummy,.false.)
  call convip_plus(IP3,P(3),kind(3),+2,dummy,.false.)
  status=CONVERT_OK

  return
end function encode_ip_0
!===============================================================================================
!
! produce valid (real value,kind) pairs from (ip1,ip2,ip3) triplet
! ip1/2/3 should be encoded "new style" but old style encoding is accepted
! RP1 will contain a level (or a pair of levels in atmospheric ascending order) in the atmosphere
! RP2 will contain a time (or a pair of times in ascending order)
! RP3%hi will be the same as RP3%lo (if RP1 or RP2 contains a pair, RP3 is ignoref)
! this routine is C interoperable
!
! function returns:  CONVERT_ERROR        error, (ip1 not level, ip2 not time, etc...)
!                    CONVERT_OK           everything is OK
!                    CONVERT_GOOD_GUESS   old style ip1 and/or ip2 are present
!                    CONVERT_BAD_GUESS    old style ip3, interpreted as time
!                    CONVERT_TERRIBLE_GUESS old style ip3, interpreted as arbitrary code
!
! in case of error, RP1/2/3 are undefined (may contain anything)
!
!notes: some reordering may take place when RP1 or RP2 contains a pair
!       levels: ip1 will be lower in atmosphere than ip2
!       times:  ip2 will be the end of the time range, ip3 will be the start of the time range
!
!       ip1 not a level or ip2 not a time will be flagged as an error
!       RP1 and RP2 both containing a range will be flagged as an error
!       in case of error, ip1,2,3 will be returned as -1
!
function decode_ip_0(RP1,RP2,RP3,IP1V,IP2V,IP3V) result(status) BIND (C,name='DecodeIp')
  implicit none ! coupled (ip1,ip2,ip3) to (rp1,rp2,rp3) conversion with type enforcement

  integer(C_INT) :: status
  integer(C_INT), value, intent(IN)  :: IP1V,IP2V,IP3V
  type(float_ip), intent(OUT) :: RP1,RP2,RP3

  real*4, dimension(3) :: P
  integer, dimension(3) ::kind
  character(len=1) :: dummy
  integer :: IP1, IP2, IP3

  IP1=IP1V ; IP2=IP2V ; IP3 = IP3V
  status=CONVERT_OK
  if(ip1 < 0 .or. ip2 < 0 .or. ip3 < 0 ) goto 777

  call convip_plus(IP1,P(1),kind(1),-1,dummy,.false.)  ! kind of ip1 should be a level
  if(.not. is_level(kind(1))) goto 777                   ! ip1 is not a level
  if(IP1 < 32768) status = ior(status , CONVERT_GOOD_GUESS)  ! reasonable guess if old style level
  RP1%lo=P(1) ; RP1%hi=P(1) ; RP1%kind=kind(1)
  
  if(IP2 < 32768) then                          ! IP2 is old style, probably a time value
    RP2%lo = IP2 ; RP2%hi = IP2 ; 
    RP2%kind = 10                               ! time in hours ?
    status = ior(status , CONVERT_GOOD_GUESS)    ! reasonable guess
  else
    call convip_plus(IP2,P(2),kind(2),-1,dummy,.false.)  ! kind of ip2 should be new style time
    if(kind(2) /= KIND_HOURS) goto 777            ! ip2 not a time
    RP2%lo=P(2) ; RP2%hi=P(2) ; RP2%kind=kind(2)
  endif

  if(IP3 < 32768) then                          ! IP3 is old style,
    RP3%lo = IP3 ; RP3%hi = IP3
    if(IP3 <= 240) then                         ! time in hours ?
      RP3%kind = 10 
      status = ior(status,CONVERT_BAD_GUESS)      ! unreliable guess
    else                                        ! arbitraty value ?
      RP3%kind = 3
      status = ior(status,CONVERT_TERRIBLE_GUESS)  ! highly unreliable guess
    endif
  else
    call convip_plus(IP3,P(3),kind(3),-1,dummy,.false.)  ! kind of ip3 may be anything new style
    if(kind(3)==-1) goto 777
    RP3%lo=P(3) ; RP3%hi=P(3) ; RP3%kind=kind(3)
  endif
  
  if(kind(3)==10 .and. kind(2)==kind(3)) then   ! time, same kind as ip2
    RP2%hi=P(3)
    RP3%lo=0.0 ; RP3%hi=0.0 ; RP3%kind=-1
    if(RP2%hi < RP2%lo) call swap(RP2%lo,RP2%hi)
  elseif(kind(3)<=6 .and. kind(1)==kind(3)) then ! same level type as ip1
    RP1%hi=P(3)
    RP3%lo=0.0 ; RP3%hi=0.0 ; RP3%kind=-1
    if(RP1%hi < RP1%lo .and. ascending(kind(3))) call swap(RP1%lo,RP1%hi)
    if(RP1%hi > RP1%lo .and. descending(kind(3))) call swap(RP1%lo,RP1%hi)
  endif

  if(kind(1) >6 .or. kind(2)/=10) then  ! ip1 must be a level, ip2 must be a time
    status=ior(status,CONVERT_ERROR)       ! add bad coding flag
  endif

return

777 status=ior(status,CONVERT_ERROR)
  return
end function decode_ip_0
!===============================================================================================
! vector version of encode_ip_0 (EncodeIp)
function encode_ip_1(IP,RP) result(status) BIND (C,name='EncodeIp_v')
  implicit none  ! coupled (rp1,rp2,rp3) to (ip1,ip2,ip3) conversion with type enforcement

  integer(C_INT) :: status
  integer(C_INT), dimension(3), intent(OUT) :: IP
  type(float_ip), dimension(3), intent(IN)  :: RP

  status=encode_ip_0(IP(1),IP(2),IP(3),RP(1),RP(2),RP(3))

  return
end function encode_ip_1
!===============================================================================================
! vector version of decode_ip_0 (DecodeIp)
function decode_ip_1(RP,IP) result(status) BIND (C,name='DecodeIp_v')
  implicit none ! coupled (ip1,ip2,ip3) to (rp1,rp2,rp3) conversion with type enforcement

  integer(C_INT) :: status
  integer(C_INT), dimension(3), intent(IN)  :: IP
  type(float_ip), dimension(3), intent(OUT) :: RP

  status=decode_ip_0(RP(1),RP(2),RP(3),IP(1),IP(2),IP(3))

return
end function decode_ip_1
!===============================================================================================
! encode three (value,kind) pairs into three ip values
! pair 1 must be a level
! pair 2 should be a time but a level is accepted (and flagged)
! pair 3 may be kind
! function returns: CONVERT_OK           everything is OK
!                   CONVERT_ERROR        error (kind1 not a level, kind2 not level or time)
!                   CONVERT_WARNING      coding convention error, corrected
!
!notes: tolerated coding deviations: kind2 a level instead of a time (will be pushed to position 3 and flagged as warning)
!       ip1/ip3 forced to the proper atmospheric ascending order coding (not flagged as warning)
!       ip2/ip3 forced to proper descending order (not flagged as warning)
!       in case of error, the contents of ip1/2/3 is undefined (probably -1)
function encode_ip_2(IP1,IP2,IP3,P1,kkind1,P2,kkind2,P3,kkind3) result(status) BIND(C,name='ConvertPKtoIP')
implicit none  ! explicit, almost independent (rp,kind) to (ip) conversion

  integer(C_INT) :: status
  integer(C_INT),        intent(OUT) :: IP1,IP2,IP3
  real(C_FLOAT), value, intent(IN)   :: P1,P2,P3
  integer(C_INT), value, intent(IN)  :: kkind1,kkind2,kkind3

  character(len=1) :: dummy
  integer(C_INT) :: kind1,kind2,kind3
  real(C_FLOAT)  :: RP1,RP2,RP3

  ip1 = -1       ; ip2 = -1       ; ip3 = -1
  RP1 = P1       ; RP2 = P2       ; RP3 = P3
  kind1 = kkind1 ; kind2 = kkind2 ; kind3 = kkind3

  status=CONVERT_OK
  if(is_invalid_kind(kind1) .or. is_invalid_kind(kind2) .or. is_invalid_kind(kind3)) goto 777

  if(.not.is_level(kind1)) goto 777 ! ip1 must be a level

  if(is_level(kind2)) then          ! ip2 should be a time, but a level is tolerated
    status=ior(status,CONVERT_WARNING)               ! warning if level
  else
    if(kind2/=10) goto 777          ! ip2 must be a time if not a level
  endif

  call convip_plus(IP1,RP1,kind1,+2,dummy,.false.)  ! NEW style encoding not negotiable
  call convip_plus(IP2,RP2,kind2,+2,dummy,.false.)
  call convip_plus(IP3,RP3,kind3,+2,dummy,.false.)

  if(kind1==kind2 .and. is_level(kind1) .and. kind3==10) then  ! level/level/time
    call swapi(ip2,ip3)  ! second level into ip3, time flag into ip2
    call swap(rp2,rp3)
    if(rp1>rp3 .and.  ascending(kind1)) call swapi(ip1,ip3)  ! ip1, ip3 in atmospheric ascending order
    if(rp1<rp3 .and. descending(kind1)) call swapi(ip1,ip3)
  else
    if(kind2==10 .and. kind3==10 .and. rp2<rp3) call swapi(ip2,ip3)  ! level/time/time, put times in descending order
  endif
  return

777 status=ior(status,CONVERT_ERROR)
  return
end function encode_ip_2
!===============================================================================================
!decode ip1/2/3 into three (value,kind) pairs
! function returns: CONVERT_OK           everything is OK
!                   CONVERT_ERROR        error (bad kind, ip1 not a level, etc ....)
!                   CONVERT_WARNING      coding convention violations
!                   CONVERT_GOOD_GUESS   old style ip1 and/or ip2 are present
!                   CONVERT_BAD_GUESS    old style ip3, interpreted as time
!                   CONVERT_TERRIBLE_GUESS old style ip3, interpreted as arbitrary code
!
! in case of error, (value,kind) pairs are undefined (may contain anything)
!notes:
function decode_ip_2(RP1,kind1,RP2,kind2,RP3,kind3,IP1V,IP2V,IP3V) result(status) BIND(C,name='ConvertIPtoPK')
implicit none ! explicit, independent (ip) to (rp,kind) conversion

  integer(C_INT) :: status
  real(C_FLOAT),        intent(OUT)  :: RP1,RP2,RP3
  integer(C_INT),        intent(OUT) :: kind1,kind2,kind3
  integer(C_INT), value, intent(IN)  :: IP1V,IP2V,IP3V

  character(len=1) :: dummy
  integer :: IP1, IP2, IP3

  IP1=IP1V ; IP2=IP2V ; IP3 = IP3V
  status=CONVERT_OK
  if(ip1 < 0 .or. ip2 < 0 .or. ip3 < 0 ) goto 777

  call convip_plus(IP1,RP1,kind1,-1,dummy,.false.)   ! IP1 old style translation should be a safe bet
  if(kind1 == KIND_HOURS) then ! try to swap with ip2 is ip1 is time
    call swapi(ip1,ip2)
    call convip_plus(IP1,RP1,kind1,-1,dummy,.false.)
    status = ior(status,CONVERT_WARNING)
  endif
  if(is_invalid_kind(kind1)) goto 777  ! bad kind
  if(IP1 < 32768) status = ior(status,CONVERT_GOOD_GUESS)
  if( .not. is_level(kind1)) goto 777           ! ip1 must be a level

  if(IP2 < 32768) then                          ! IP2 is old style, probably a time value
    RP2 = IP2
    kind2 = 10                                  ! time in hours ?
    status = ior(status,CONVERT_GOOD_GUESS)     ! reasonable guess
  else
    call convip_plus(IP2,RP2,kind2,-1,dummy,.false.)
    if(is_invalid_kind(kind2)) goto 777  ! bad kind
    if(kind2 /= 10) then
      if(is_level(kind2)) then
        status = ior(status,CONVERT_WARNING)  ! ip2 is supposed to be a TIME, a level is tolerated
      else
        goto 777  ! neither time nor level
      endif
    endif
  endif
  if(IP3 < 32768) then                          ! IP3 is old style,
    RP3 = IP3
    if(IP3 <= 240) then                         ! time in hours ?
      kind3 = 10 
      status = ior(status,CONVERT_BAD_GUESS)    ! unreliable guess
    else                                        ! arbitraty value ?
      kind3 = 3
      status = ior(status,CONVERT_TERRIBLE_GUESS) ! highly unreliable guess
    endif
  else
    call convip_plus(IP3,RP3,kind3,-1,dummy,.false.)
    if(is_invalid_kind(kind3)) goto 777  ! bad kind
  endif
  if(kind1 == kind2 .and. kind3==KIND_HOURS) then   ! level/level/time range in ip1/ip2/ip3
    call swap(RP2,RP3)       ! level/time/level
    call swapi(kind2,kind3)
  endif
  if(kind1 == kind3) then   ! level range
    if(ascending(kind1)  .and. RP1>RP3) call swap(RP1,RP3)   ! force increasing values
    if(descending(kind1) .and. RP1<RP3) call swap(RP1,RP3)   ! force decreasing values
  endif
  if(kind2 == kind3 .and. kind2==KIND_HOURS) then   ! time range
    if(RP2 < RP3) call swap(RP2,RP3)    ! force decreasing time values
  endif

  return

777 status=ior(status,CONVERT_ERROR)
  return
end function decode_ip_2
!===============================================================================================
! vector version of encode_ip_2 (ConvertPKtoIP)
function encode_ip_3(IP,RP,kind) result(status) BIND(C,name='ConvertPKtoIP_v')
implicit none  ! explicit, independent (rp,kind) to (ip) conversion

  integer(C_INT) :: status
  integer(C_INT), dimension(3), intent(OUT) :: IP
  real(C_FLOAT), dimension(3), intent(IN)   :: RP
  integer(C_INT), dimension(3), intent(IN)  :: kind

  status=encode_ip_2(IP(1),IP(2),IP(3),RP(1),kind(1),RP(2),kind(2),RP(3),kind(3))

return
end function encode_ip_3
!===============================================================================================
!vector version of decode_ip_2 (ConvertIPtoPK)
function decode_ip_3(RP,kind,IP) result(status) BIND(C,name='ConvertIPtoPK_v')
implicit none ! explicit, independent (ip) to (rp,kind) conversion

  integer(C_INT) :: status
  real(C_FLOAT),  dimension(3), intent(OUT) :: RP
  integer(C_INT), dimension(3), intent(OUT) :: kind
  integer(C_INT), dimension(3), intent(IN)  :: IP

  status=decode_ip_2(RP(1),kind(1),RP(2),kind(2),RP(3),kind(3),IP(1),IP(2),IP(3))

return
end function decode_ip_3
!===============================================================================================
subroutine C_CONV_IP( ip, p, kind, mode ) BIND(C,name='ConvertIp') ! C language inteerface with no string option
!     void ConvIp(int *ip, float *p, int *kind, int mode)
  use ISO_C_BINDING
  implicit none
    integer(C_INT), intent(INOUT) :: ip, kind
    integer(C_INT), intent(IN), value :: mode
    real(C_FLOAT), intent(INOUT) :: p
    character (len=1) :: string
    integer :: mode2
    mode2 = mode
    call CONVIP_plus( ip, p, kind, mode2,string,.false.)
end subroutine C_CONV_IP
!===============================================================================================
! successeur de convip
SUBROUTINE CONVIP_plus( ip, p, kind, mode, string, flagv )
  implicit none
  integer, intent(INOUT) :: ip, kind
  integer, intent(IN) :: mode
  real, intent(INOUT) :: p
  character *(*), intent(OUT) :: string 
  logical, intent(IN) :: flagv

!*********************************************************************
!     Codage/Decodage P de/a IP pour IP1, IP2, IP3
!     necessaire avant de lire/ecrire un enregistrement
!     sur un fichier standard.
!
!     Etendue des valeurs encodes: 10e-5 -> 10e10
!     1024x1024-1 = 1048575    1048001 -> 1048575 non utilise
!                              1000000 -> 1048000 utilise pour valeurs negatives
!
!     Auteurs: N. Ek et B. Dugas - Mars 1996
!     Revision 001  M. Lepine - juin 1997 convpr devient convip
!     Revision 002  M. Valin  - mai  1998 fichiers std 98
!     Revision 003  B. Dugas  - juillet 2000 code arbitraire 
!     Revision 004  M. Lepine - fevrier 2002 kind = 4, hauteur au sol +
!                               possibilite de forcer newstyle ou non avec mode=2 et mode=3
!     Revision 005  M. Lepine - avril 2002 kind = 5 (hybride), kind = 21 (GalChen)
!                               valeur min, max, zero et facteur multiplicatif
!     Revision 006  M. Lepine - Juin 2002 kind = 6 (Theta)
!     Revision 007  M. Lepine - Oct 2003 kind = 10 (temps en heure)
!     Revision 008  M. Lepine - Dec 2005 kind = 17 (indice de matrice de niveaux)
!     Revision 009  M. Valin  - Mars 2008 kind = 21 (metres pression remplacant GalChen)
!                               introduction de zero_val2 pour la conversion ip->p
!     Revision 010  M. Lepine - Mai 2010 traitement des valeurs en dehors des intervals connus
!                               comme valeurs arbitraires
!     Revision 011  M. Valin  - Mai/Juin 2013 activation du code 15, ajout de la conversion groupee,
!                               menage dans le code, changement de nom, refactoring
!     Revision 012  M. Valin  - Oct/Nov 2013 bug de conversion corrige pour certains cas limites
!                               enleve une amelioration qui entrainait une non compatibilite avec convip
!
!     Input:    MODE = -1, de IP -->  P
!               MODE =  0, forcer conversion pour ip a 31 bits
!                          (default = ip a 15 bits)
!                          (appel d'initialisation)
!               MODE = +1, de P  --> IP
!               MODE = +2, de P  --> IP en mode NEWSTYLE force a true
!               MODE = +3, de P  --> IP en mode NEWSTYLE force a false
!               FLAG = .true. , ecriture de P avec format dans string
!
!     Input/
!     Ouput:    IP  =   Valeur codee 
!               P    =   Valeur reelle
!               KIND =0, p est en hauteur (m) par rapport au niveau de la mer (-20,000 -> 100,000)
!               KIND =1, p est en sigma                                       (0.0 -> 1.0)
!               KIND =2, p est en pression (mb)                               (0 -> 1100)
!               KIND =3, p est un code arbitraire                             (-4.8e8 -> 1.0e10)
!               KIND =4, p est en hauteur (M) par rapport au niveau du sol    (-20,000 -> 100,000)
!               KIND =5, p est en coordonnee hybride                          (0.0 -> 1.0)
!               KIND =6, p est en coordonnee theta                            (1 -> 200,000)
!               KIND =10, p represente le temps en heure                      (0.0 -> 1.0e10)
!               KIND =15, reserve (entiers)                                   
!               KIND =17, p represente l'indice x de la matrice de conversion (1.0 -> 1.0e10)
!                                                   (partage avec kind=1 a cause du range exclusif
!               KIND =21, p est en metres-pression  (partage avec kind=5 a cause du range exclusif)
!                                                                             (0 -> 1,000,000) fact=1e4
!               STRING = valeur de P formattee
!*********************************************************************
  real *8 TEN
  parameter (TEN=10.0)
  real *8 limit1, limit2, temp
  real abs_p
  integer iexp,  offset, itemp, lstring
  character *128 var_fmt

  INTEGER, PARAMETER :: Max_Kind = 31
  integer maxkind
  logical NEWSTYLE, NEWENCODING
  real *8 exptab(0:15)
  character *2 kinds(0:Max_Kind)
  character (len=12) :: string2
  integer :: status

  INTEGER :: i
  logical :: flag

  LOGICAL, PARAMETER, DIMENSION(0:Max_Kind) :: validkind =                    &
  & (/ (.true.,i=0,6), (.false.,i=7,9), .true., (.false.,i=11,14),            &
  &    .true., .false.,.true.,                                                &
  &    (.false., i=18,20), .true., (.false., i=22,30),.true. /)   ! kind 31 valide

  REAL, PARAMETER, DIMENSION(0:Max_Kind) :: low_val =                         &
  &  (/ -20000., 0., 0.,    -4.8e+8, -20000., 0.,                             &
  &    1.0, (-4.8e+8,i=7,9), 0.0, (-4.8e+8,i=11,16),                          &
  &    1.0, (-4.8e+8,i=18,20), 0., (-4.8e+8,i=22,31) /)
  REAL, PARAMETER, DIMENSION(0:Max_Kind) :: hi_val =                          &
  &  (/  100000., 1., 1100., 1.0e+10, 100000., 1.,                            &
  &     200000., (1.0e+10,i=7,9), 1.0e+10, (1.0e+10,i=11,16),                 &
  &     1.0e+10, (1.0e+10,i=18,20), 1000000., (1.0e+10,i=22,31) /)
  REAL, PARAMETER, DIMENSION(0:Max_Kind) :: zero_val =                        &
  &  (/ 0., 0., 0., 0., 0., 0., 1., (0.0,i=7,16),                             &
  &    1.0, (0.0,i=18,20), 1.001e-4, (0.0,i=22,31) /)
  REAL, PARAMETER, DIMENSION(0:Max_Kind) :: zero_val2 =                       &
  &  (/ 0., 0., 0., 0., 0., 0., 1., (0.0,i=7,16),                             &
  &    1.0, (0.0,i=18,20), 0.0, (0.0,i=22,31) /)
  REAL, PARAMETER, DIMENSION(0:Max_Kind) :: fact_val =                        &
  &  (/ 1., 1., 1., 1., 1., 1., 1., (1.0,i=7,16),                             &
  &    -1.0, (1.0,i=18,20), 1.0e+4, (1.0,i=22,31) /)

  save NEWSTYLE, exptab, kinds, maxkind

  data NEWSTYLE /.false./

  data exptab /0.0001D0, 0.001D0, 0.01D0, 0.1D0, 1.0, 10.0, 100.0,            &
  &  1000.0, 10000.0, 100000.0, 1000000.0, 10000000.0,                        &
  &  100000000.0, 1000000000.0, 10000000000.0, 100000000000.0 /

  data kinds                                                                  &
  &   / 'm ', 'sg', 'mb', '##', 'M ', 'hy', 'th', '??',                       &
  &     '??', '??', 'H ', '??', '??', '??', '??', '  ',                       &
  &     '??', '[]', '??', '??', '??', 'mp', '??', '??',                       &
  &     '??', '??', '??', '??', '??', '??', '??', '  '/

  if (mode .eq.0) then
      NEWSTYLE = .true.
      return
  endif
  NEWENCODING = NEWSTYLE
  if (mode .eq. 2) NEWENCODING = .true.
  if (mode .eq. 3) NEWENCODING = .false.
  if ((NEWENCODING) .or. (mode .eq. -1)) then
      maxkind = Max_Kind
  else
      maxkind = 3
  endif
!
  if (mode.gt.0) then  ! .... Conversion P,KIND a IP  ....

      if ( is_invalid_kind(kind) ) then
          write(6,6004) kind
!           call qqexit(1)    !  force excessive ?
          ip = -1 
          return  ! erreur si kind pas valide
      endif
      if (kind .eq. 2 .and. p .eq. 0.) then  ! ou ajouter .and. .not. NEWENCODING
         ip = 0
!          if(NEWENCODING) ip =  ishft(2,24) +  ishft(1,20) ! si  newstyle (kind=2, mantissa=0, exponent=1)
         return
      endif
!
      if(NEWENCODING)then    ! new style excoding
          if(iand(kind,15) == 15) then  ! kind 15 and subkinds done elsewhere (pure integers)
            status = conv_kind_15(p,kind,ip,mode)
            return
          endif
          if (p .lt. low_val(kind) .or. p .gt. hi_val(kind)) then
              write(6,6006) p,low_val(kind),hi_val(kind)
              ip = -999999
              return
          endif
          iexp = 4
          temp = p
          if (abs(temp) .lt. zero_val(kind)) temp = zero_val(kind)
          temp = temp * fact_val(kind)  ! apply scaling factor before conversion to mantissa and pseudo exponent
          if ( temp .ge. 0) then
              limit1 = 1000000
              limit2 = 100000
              offset = 0
          else
              temp = -temp
              limit1 = 48000
              limit2 = 4800
              offset = 1000000
          endif
!          temp=temp*1.00000005D0
          do while ( iexp .gt. 0 .and. iexp .lt. 15 )  ! must keep pseudo exponent in range
              if (temp .ge. limit1 ) then        ! too big, divide by 10 and adjust pseudo exponent
                temp = temp / TEN
                iexp = iexp -1
              else if ( temp .lt. limit2 ) then  ! too small multiply by 10 and adjust pseudo exponent
                temp = temp * TEN
                iexp = iexp + 1
              else   ! >=limit2 and <limit1
                EXIT
              endif
          enddo
          if ( temp .gt. limit1 ) then          ! number is too big, cannot code
              ip = -1
          else
              ip = offset + nint(temp)
          endif
          ip = ior (ip,ishft(iexp,20))          ! add pseudo exponent
          ip = ior (ip,ishft(iand(15,kind),24)) ! add primary kind flag

      else ! OLD style encoding

          if (kind.eq.0) then   ! ...  hauteur ...
              ip = max( 12001,min( 32000,nint( p/5.0 + 12001 ) ) )
          elseif (kind.eq.1) then  ! ...  sigma ...
              if ( .not. (  0.0 .le. p .and. p .le. 1.0 ) ) then
                write(6,6001) p
                ip = -999999
                return
              endif
              ip = nint( p * 10000. ) + 2000
          elseif (kind.eq.2) then  ! ...  pression ...
              if (  .not. (0.0 .le. p .and. p .lt. 1100. ) ) then
                write(6,6002) p
                ip = -999999
                return
              endif
              if (0.999999e+1 .le. p .and. p .lt. 1100. ) then
                ip = nint ( p )
              elseif ( p .lt. 0.999999e+1 ) then
                if( p .ge. 0.999999e0 ) then
                    ip = 1800 + nint(20.*p)
                elseif ( p .ge. 0.999999e-1 ) then
                    ip = 1600 + nint(200.*p)
                elseif ( p .ge. 0.999999e-2 ) then
                    ip = 1400 + nint(2000.*p)
                elseif ( p .ge. 0.999999e-3 ) then
                    ip = 1200 + nint(20000.*p)
                else
                    ip = 0
                endif
              endif
          elseif (kind.eq.3) then  ! ...  code arbitraire
              ip = nint( p )
              if ( 0 .le. ip .and. ip .le. 100 ) then
                ip = 1200 - ip
              else
                write(6,6003) p
                ip = -999999
                return
              endif
          else  !  OLD encoding not valid for this kind
              write(6,6004) kind
              ip = -999999
              return
          endif
      endif  ! .not NEWENCODING, OLD style encoding

  elseif (mode.lt.0) then  ! ....  Conversion de ip a p,kind .....
      flag = flagv
      lstring=0
      if(flag) then
        lstring=len(string)
        if(lstring<9) flag=.false.   ! if less than 9 characters are available, no attempt to format value will be made
      endif
      if ( ip .gt. 32767 ) then  !   tous types, nouveau codage
          p = 0.0
          kind = iand(15,ishft(ip,-24))
          if(kind == 15) then  ! type 15 et associes traite a part
            if(conv_kind_15(p,kind,ip,mode) /= 0) goto 777  ! il y a une erreur de decodage pour ip
            if (flag) goto 666  ! impression dans string
          endif
          if ( .not. validkind(kind) ) goto 777
!
          iexp = iand (15,ishft(ip,-20))
          itemp = iand (1048575, ip)
          if (itemp > 1000000) itemp = -(itemp - 1000000)
 555        continue
          p = itemp / exptab(iexp)             ! apply pseudo exponent
          p = p / fact_val(kind)               ! apply scaling factor
 !
          if (p < low_val(kind) .or. p>hi_val(kind)) then ! hors limite, essayer le type associe si valide
            if(kind+16 <= Max_Kind) then
              if(validkind(kind) .and. validkind(kind+16)) then
                kind = kind+16
                goto 555         ! try new kind
              else
                goto 777  ! invalid kind
              endif
            else
              goto 777  ! invalid kind
            endif
          endif
          p = max(p,low_val(kind))     ! clipping a la valeur minimale
          p = min(p,hi_val(kind))      ! clipping a la valeur maximale
          if (abs(p) .lt. 1.001*zero_val(kind)) p = zero_val2(kind)   ! mise a "zero" si valeur absolue trop faible
666       abs_p = abs(p)
          if (flag) then  ! convert P into a formatted string with appropriate units for kind
             string2=""
             status=value_to_string(p , string2 , min(len(string2),len(string)-3) )
             string=trim(string2)//' '//kinds(kind)
          endif
      elseif (  12000 .lt. ip .and. ip .le. 32000) then  !  ...  hauteur old style ...
          kind = 0
          p = 5 * ( ip -12001)
          if (flag) write(string,'(i6,1x,a1)') nint(p),'m'
      elseif (  2000 .le. ip .and. ip .le. 12000 ) then  !  ...  sigma old style ...
          kind = 1
          p = float (ip - 2000) / 10000.
          if (flag) write(string,'(f6.4,1x,a2)') p,'sg'
      elseif (( 0    .le. ip .and. ip .lt. 1100 )  .or. ( 1200 .lt. ip .and. ip .lt. 2000 )) then  !  ... pression old style ...
          kind = 2
          if ( 0 .le. ip .and. ip .lt. 1100 ) then
             p = float(ip)
             if (flag) write(string,'(i6,1x,a2)') ip,'mb'
          elseif ( ip .lt. 1400 ) then
                p = float(ip-1200) / 20000.D0
                if (flag) write(string,'(f6.5,1x,a2)') p,'mb'
          elseif ( ip .lt. 1600) then
                p = float(ip-1400) / 2000.D0
                if (flag) write(string,'(f6.4,1x,a2)') p,'mb'
          elseif ( ip .lt. 1800) then
                p = float(ip-1600) / 200.D0
                if (flag) write(string,'(f6.3,1x,a2)') p,'mb'
          elseif  ( ip .lt. 2000) then
                p = float(ip-1800) / 20.D0
                if (flag) write(string,'(f6.2,1x,a2)') p,'mb'
          endif
      elseif ( 1100 .le. ip .and. ip .le. 1200) then  ! ...  code arbitraire old style ...
          kind = 3
          p = float( ip )
          p = 1200. - p
          if (flag) write(string,'(i6,3x)') nint(p)
      else  !  Valeur inderminee de ip  old style
          kind = 3
          p = float( ip )
      endif   !    ip .gt. 32767 elseif, elseif, 
  endif  ! ....  Conversion de xx a yy .....
      
  return

777  continue  ! invalid ip, return kind = -1
  kind = -1
  return

  6001 format(' Error in convip: sigma value =',e10.5,' returned ip is -999999')
  6002 format(' Error in convip: pressure value =',e10.5,' returned ip is -999999')
  6003 format(' Error in convip: arbitrary value=',e10.5,' returned ip is -999999')
  6004 format(' Error in convip: invalid kind =',I10)
  6005 format(' Error in convip: kind=10 (oldstyle) value out of range=',e10.5,' returned ip is -999999')
  6006 format(' Error in convip: p is out of bounds =',e10.5,' min=',e10.5,' max=',e10.5,' returned ip is -999999')
! 6007 format(' Warning in convip: undetermined kind used =',I10)

end SUBROUTINE CONVIP_plus
!===============================================================================================
function conv_kind_15(p,mykind,ip,mode) result(status) ! convert kind = 15 and subkinds
  implicit none
  integer :: status
  integer, intent(INOUT) :: mykind,ip
  integer, intent(IN) :: mode ! -1, 1, 2, 3 (see convip code for meaning of mode)
  real, intent(INOUT) :: p
!
  type e15
    integer :: lo      ! lowest value of ipv for this sub kind
    integer :: hi      ! lhighest value of ipv for this sub kind
    integer :: base    ! offset for this sub kind
  end type
  type(e15), dimension(2), save :: t15 = & 
         (/ &
         e15(       0, 20000000,      0), &     ! values between 0 and 1 999 999    (kind 15)
         e15(16000000, 15000001,  -1000)  &     ! values between -1000 and 998 999 (kind 31) ! entries swapped to deactivate
         /)
  integer :: i, subt, ipv
  integer, parameter :: FFFFFF=Z'FFFFFF'
!
  status = -1               ! precondition for failure
!
  if(ip > 0 .and. ishft(ip,-24) == 15 .and. mode == -1) then  ! kind 15 and sub kinds ip to p conversion
    mykind = -1
    ipv = iand(ip,FFFFFF)  ! get rid of kind 15 indicator
    subt = -1
    do i=1,size(t15)   ! lookup in bounds table
      if(ipv >= t15(i)%lo .and. ipv <= t15(i)%hi ) then ! is ipv in the range of this sub kind ?
        subt = i    ! yes
        exit
      endif
    enddo
    if(subt == -1) return  ! invalid ip value for kind = 15 and associated sub kinds
    p = ipv - t15(subt)%lo + t15(subt)%base   ! convert ipv to actual value
    mykind = 15 + 16*(subt-1)    ! return proper kind type
    status = 0
  endif
!
  if(15 == iand(mykind,15) .and. mode > 0 .and. mode <3) then  ! newstyle p to ip conversion
    ip = -1                      ! precondition for fail
    subt = 1 + ishft(mykind,-4)
    if(subt <= 0 .or. subt > size(t15)) return   ! sub kind out of range
    ipv = nint(p) - t15(subt)%base + t15(subt)%lo
    if(ipv < t15(subt)%lo .or. ipv > t15(subt)%hi) return  ! p is out of range
    ip = ior(ipv,ishft(15,24))  ! add type 15 flag
    status = 0
  endif
! total failure if we get here
  return
end function conv_kind_15
!===============================================================================================
integer function value_to_string(val,string,maxlen)  ! write value val into string using at most maxlen characters
! Version originale M.Valin 2013
! Revision 001 :    M.Valin  Oct 2013 alignement a droite corrige pour valeurs entieres > 0
  integer :: maxlen
  character (len=*) :: string
  character (len=32) :: fstring
  real *4 :: val, value
  integer :: after, before
  integer :: grosint, maxc, intdig

  string=" "
  maxc=min(maxlen,len(string))
  value=abs(val)
  after=min(7,maxc-6)
  before=maxc
  value_to_string=-(100*before+after*10)
  write(fstring,11)maxc,after    ! default G format

  if(value >= 1000000000000.0 .or. value < .0001) goto 666   ! use G format

  if(nint(value)==value) then ! exact integral value
    grosint=1
    intdig=2
    do i=1,min(9,maxc-1)
      if(nint(value) > grosint) intdig=intdig+1
      grosint=grosint*10  ! largest integer value that will fit in maxc characters
    enddo
    if(value >= grosint) goto 444   ! try something else
    if(val>0) intdig = intdig - 1   ! one less character if number is positive
    write(fstring,12)'(I',min(maxc,intdig),')'    ! use I format
    value_to_string=min(maxc,intdig)
    goto 777
  endif

  444 continue  ! real values within "civilized" range
  if (value >= 1.0) then
    before = 0
    after = 0
    do while(value>=1.0)
      before = before +1
      value = value * .1
    enddo
    if(before<6) after=min(6-before,maxc-before-2) ! we have at best 6 significant digits
!    if(before<8) after=max(0,maxc-before-2)
  else   ! value < 1.0
    after = 5
    before = 0
    do while(value<1.0)
      value = value * 10.0
      after = after  +1
    enddo
    after=min(after,maxc-2)
  endif

  after=min(9,after)  ! never more than 9 digits after the decimal point

  if(before+after+2 > maxc) goto 666  ! use G format

  before=before+after+2
  value_to_string=100*before+after*10

  write(fstring,10)before,after       ! use F format
  !print *,'=',trim(fstring)
  write(string,fstring)val            ! F format
  i=len(trim(string))
  do while(i>4 .and. string(i-1:i-1)=='0')
    string(i:i)=' '
    i=i-1
  enddo
  return

  666 continue
  if(maxc-6<=0) goto 888
  !print *,'=',trim(fstring)
  write(string,fstring)val            ! G format
  return

  777 continue
  !print *,'=',trim(fstring)
  write(string,fstring)nint(val)      ! I format
  return

  888 continue
  value_to_string=0
  return

10 format(2H(F,I2,1H.,I1,1H))
11 format(2H(G,I2,1H.,I1,1H))
12 format(A,I2,A)
end function value_to_string
!===============================================================================================
subroutine test_value_to_string
  implicit none
  character (len=8) :: stringa
  character (len=12) :: stringb
  character (len=15) :: stringc
  integer :: i
  integer :: status
  real *4 :: value

  value=1.000001
  do i=1,9
    status=value_to_string(real(nint(value)),stringa,15)
    stringc = stringa
    print 101,stringc,trim(stringa),'',status*.01
    value=value*10.0
  enddo

  value=1.000001
  do i=1,9
    status=value_to_string(real(nint(-value)),stringa,15)
    stringc = stringa
    print 101,stringc,trim(stringa),'mb',status*.01
    value=value*10.0
  enddo

  value=1.234567
  do i=1,12
    status=value_to_string(-value,stringb,15)
    stringc = stringb
    print 101,stringc,trim(stringb),'mb',status*.01
    value=value*10.0
  enddo

  value=1.23456789
  do i=1,12
    status=value_to_string(-value,stringc,12)
    print 101,stringc,trim(stringc),'mb',status*.01
    value=value*0.1
  enddo

101 format(1H|,A15,1H|,A15,1H|,1X,A2,3X,f6.2)
return
end subroutine test_value_to_string
!===============================================================================================
subroutine test_convip_plus() ! test routine for convip_plus
  implicit none
  integer :: ip1, ip2, i, j, nip, nip2, k2, nip3
  real :: p,p2
  character(len=15) :: string
  integer :: kind
!     test #1, ip1 -> p,kind -> ip2 (ip2 must be = ip1)
  nip = 0
  nip2 = 0
  nip3 = 0
  do j=0,15
  do i=0,1047999
    ip1 = i
    ip1=ior(ip1, ishft(j,20))  ! add exponent
    ip1=ior(ip1, ishft(3,24))  ! kind =3 to test full range
    call CONVIP_plus( ip1, p, kind, -1, string, .false. )  ! ip1 -> p,kind 
    call CONVIP_plus( ip2, p, kind, +2, string, .false. )  ! p,kind -> ip2
    call CONVIP_plus( ip2, p2, k2, -1, string, .false. )
    if(ip1/=ip2) nip=nip+1
    if(p/=p2 .and. abs(p2/p-1.0) < .0000002) nip2=nip2+1
    if(ip1/=ip2 .and. p /= p2) then
      if(abs(p2/p-1.0) >= .0000002) then ! not within tolerance
        print 111, j,i,ip1,ip2,iand(ip1,1048575),iand(ip2,1048575),p,p2,abs(p2/p-1.0)
        nip3 = nip3+1
!           stop
      endif
    endif
  enddo
  enddo
  print 112,'ip1<>ip2 (normalization aliases)=',nip,' p1 ~= p2 (within 2.0E-7 relative error)',nip2,' errors=',nip3
  do i=5,1005,100
     ip1 = i
     call CONVIP_plus( ip1, p, kind, -1, string, .false.)
     p=p+.751
     call CONVIP_plus( ip1, p, kind, +2, string, .false. )
     call CONVIP_plus( ip1, p, kind, -1, string, .true.)
     print 113,i,p,kind,':'//trim(string)//':'
  enddo
111 format(2I9,2Z8,2I9,3G16.8)
112 format(A,I9,A,I9,A,I9)
113 format(I9,F11.5,I3,A)
  return
end subroutine test_convip_plus

end module convert_ip123
