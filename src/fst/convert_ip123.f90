!> \file


!> Set of generic IP123 conversion functions (IP123 from RPN standard files)
module convert_ip123_int
    use iso_c_binding
    implicit none

    !> Converts between (ip1, ip2, ip3) triplet and 3 (real value, kind) pairs

    ! Fortran users will call the generic functions
    !  - encode_ip (real value, kind) pairs -> (ip1, ip2, ip3) triplet
    !  - decode_ip (ip1, ip2, ip3) triplet   -> (real value, kind) pairs
    !  the kind of arguments will determine which specific conversion function will be used
    !  (see description of specific functions)
    !  encode_ip_0, encode_ip_1, encode_ip_2, encode_ip_3
    !  decode_ip_0, decode_ip_1, decode_ip_2, decode_ip_3

    ! EXAMPLES
    !   use ISO_C_BINDING
    !   implicit none
    !   include 'convert_ip123.inc'
    !   integer :: ip1, ip2, ip3, stat
    !   integer, dimension(3) :: vip123
    !   integer :: k1, k2, k3
    !   integer, dimension(3) :: vk123
    !   real    :: v1, v2, v3
    !   real, dimension(3)    :: v123
    !   type(FLOAT_IP) :: RP1, RP2, RP3
    !   type(FLOAT_IP), dimension(3) :: VRP123
    !
    !   stat = encode_ip(ip1, ip2, ip3, v1, k1, v2, k2, v3, k3)  ! everything explicit (encode_ip_2)
    !   stat = encode_ip(vip123, v123, vk123)              ! vector version of above (encode_ip_3)
    !   stat = encode_ip(ip1, ip2, ip3, RP1, RP2, RP3)        ! ip <- multiple FLOAT_IP  (encode_ip_0)
    !   stat = encode_ip(vip123, VRP123)                  ! vector version of above (encode_ip_1)
    !
    !   stat = decode_ip(v1, k1, v2, k2, v3, k3, ip1, ip2, ip3)  ! everything explicit (decode_ip_2)
    !   stat = decode_ip(v123, vk123, vip123)              ! vector version of above (decode_ip_3)
    !   stat = decode_ip(RP1, RP2, RP3, ip1, ip2, ip3)        ! ip -> multiple FLOAT_IP  (decode_ip_0)
    !   stat = decode_ip(VRP123, vip123)                  ! vector version of above (decode_ip_1)
    !
    ! program testip   ! very simple example of time range coding
    !   use ISO_C_BINDING
    !   implicit none
    !   include 'convert_ip123.inc'
    !   integer err
    !   type(FLOAT_IP) :: RP1, RP2, RP3
    !   integer :: ip1, ip2, ip3
    !   RP1%lo = 10.   ! 10 mb
    !   RP1%hi = 10.   ! 10 mb  hi == lo , no range
    !   RP1%kind = KIND_PRESSURE
    !   RP2%lo = 6.    ! 6 hours
    !   RP2%hi = 16.   ! 16 hours
    !   RP2%kind = KIND_HOURS
    !   RP3%lo = 0.    ! there is a range so an invalid RP3 is OK
    !   RP3%hi = 0.
    !   RP3%kind=-1
    !   err= encode_ip(IP1, IP2, IP3, RP1, RP2, RP3)
    !   print*, err, IP1, IP2, IP3
    !   stop
    ! end

    ! NOTES
    !  the FORTRAN user must include
    !    use ISO_C_BINDING
    !    include 'convert_ip123.inc'
    !  in order to access this package
    !
    !  individual IP to real value + kind conversions are performed by function convip_plus
    !  real value to formatted string encoding is performed by function value_to_string

    public  :: encode_ip_0, encode_ip_1, decode_ip_0, decode_ip_1
    public  :: encode_ip_2, encode_ip_3, decode_ip_2, decode_ip_3

    type, BIND(C)  :: FLOAT_IP
        real(C_FLOAT)  :: lo    ! Lower bound
        real(C_FLOAT)  :: hi    ! Upper bound
        integer(C_INT) :: kind  ! type code (see table below)
    end type


    ! TABLES
    ! kind     name              description                  range of values
    !
    !   0  KIND_ABOVE_SEA   height (m) above mean sea level (-20, 000 -> 100, 000)
    !   1  KIND_SIGMA       sigma coordinates               (0.0 -> 1.0)
    !   2  KIND_PRESSURE    pressure (mb)                   (0 -> 1100)
    !   3  KIND_ARBITRARY   arbitrary number, no units      (-4.8e8 -> 1.0e10)
    !   4  KIND_ABOVE_GND   height (m) above ground         (-20, 000 -> 100, 000)
    !   5  KIND_HYBRID      hybrid coordinates              (0.0 -> 1.0)
    !   6  KIND_THETA       theta coordinates               (1 -> 200, 000)
    !   7  KIND_BELOW_SEA   depth (m) below mean sea level  (0 -> 20, 000)
    !  10  KIND_HOURS       time (hours)                    (0.0 -> 1.0e10)
    !  15  KIND_SAMPLES     reserved (integer value)        (0 -> 1 999 999)
    !  17  KIND_MTX_IND     conversion matrix x subscript)  (1.0 -> 1.0e10)
    !                       (shared with kind = 1
    !  21  KIND_M_PRES      pressure-meters                 (0 -> 1, 000, 000) fact = 1E+4
    !                       (shared with kind = 5)
    !
    ! return FLAGS for encode/decode functions (more than one flag may be set)
    !  CONVERT_OK     ( 0 = no FLAG set)
    !  CONVERT_GUESS  (= CONVERT_GOOD_GUESS || CONVERT_BAD_GUESS || CONVERT_TERRIBLE_GUESS)
    !  CONVERT_GOOD_GUESS
    !  CONVERT_BAD_GUESS
    !  CONVERT_TERRIBLE_GUESS
    !  CONVERT_WARNING
    !  CONVERT_ERROR

    ! NOTES
    !  the FORTRAN user must include
    !    use ISO_C_BINDING
    !    include 'convert_ip123.inc'
    !  in order to use this derived type and the symbolic names for kind

    integer, public, parameter :: TO_IP = 1
    integer, public, parameter :: TO_RP = -1
    integer, public, parameter :: CONVERT_OK = 0
    integer, public, parameter :: CONVERT_GUESS = 14
    integer, public, parameter :: CONVERT_GOOD_GUESS = 2
    integer, public, parameter :: CONVERT_BAD_GUESS = 4
    integer, public, parameter :: CONVERT_TERRIBLE_GUESS = 8
    integer, public, parameter :: CONVERT_WARNING = 32
    integer, public, parameter :: CONVERT_ERROR = 64

    integer, public, parameter :: KIND_ABOVE_SEA = 0
    integer, public, parameter :: KIND_SIGMA = 1
    integer, public, parameter :: KIND_PRESSURE = 2
    integer, public, parameter :: KIND_ARBITRARY = 3
    integer, public, parameter :: KIND_ABOVE_GND = 4
    integer, public, parameter :: KIND_HYBRID = 5
    integer, public, parameter :: KIND_THETA = 6
    integer, public, parameter :: KIND_BELOW_SEA = 7
    integer, public, parameter :: KIND_HOURS = 10
    integer, public, parameter :: KIND_SAMPLES = 15
    integer, public, parameter :: KIND_MTX_IND = 17
    integer, public, parameter :: KIND_M_PRES = 21

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

    integer, private, parameter :: Max_Kind = 31

    !  1 means coordinate of type kind is ascending ( larger value = higher in the atmosphere )
    ! -1 means coordinate of type kind is descending ( larger value = lower in the atmosphere )
    !  0 means coordinate of type kind cannot be deemed ascending nor descending
    ! kind = 0, 4, 21 ascending ( meters above ground, meters above msl, galchen meters )
    ! kind = 1, 2     descending (pressure, sigma)
    ! kind = 3, 5, 6  neither (arbitrary, hybrid, theta)
    ! non level coordinates are considered as neutral
    integer, private, save, dimension(0:Max_Kind) :: order = &
    (/  1, -1, -1,  0,  1,  0,  0, -1,  0,  0,  0,  0,  0,  0,  0,  0, &
        0,  0,  0,  0,  0,  1,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0  /)

    integer, private, save, dimension(0:Max_Kind) :: islevel = &
    (/  1,  1,  1,  1,  1,  1,  1,  1,  0,  0,  0,  0,  0,  0,  0,  0, &
        0,  0,  0,  0,  0,  1,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0  /)

    private :: swap, swapi, ascending, descending
    public :: is_invalid_kind, is_level

contains
    ! Private functions

    !> Test if kind is a level
    function is_level(kind) result(status)
        implicit none

        logical :: status
        integer, intent(IN) :: kind

        status = .false.
        if (kind <= Max_Kind .and. kind >= 0) status = islevel(kind) == 1
    end function is_level


    ! Test if kind "ascending" (larger value higher in atmosphere)
    function ascending(kind) result(status)
        implicit none

        logical :: status
        integer, intent(IN) :: kind

        status = .false.
        if (kind <= Max_Kind .and. kind >= 0) status = order(kind) == 1
    end function ascending


    ! Test if kind "descending" (larger value lower in atmosphere) ?
    function descending(kind) result(status)
        implicit none

        logical :: status
        integer, intent(IN) :: kind

        status = .false.
        if (kind <= Max_Kind .and. kind >= 0) status = order(kind) == -1
    end function descending


    ! Test if kind is valid
    function is_invalid_kind(kind) result(status)
        implicit none

        logical :: status
        integer, intent(IN) :: kind

        status = .false.
        if (kind < 0) status = .true.
        if (kind > Max_Kind .and. iand(kind, 15) /= 15) status = .true.
    end function is_invalid_kind


    ! Swap a pair of integer values
    subroutine swapi(a, b)
        implicit none

        integer(C_INT), intent(INOUT) :: a, b
        integer(C_INT) :: t

        t = a ; a = b ; b = t
    end subroutine swapi


    ! Swap a pair of real values
    subroutine swap(a, b)
        implicit none

        real(C_FLOAT), intent(INOUT) :: a, b
        real(C_FLOAT) :: t

        t = a ; a = b ; b = t
    end subroutine swap


    ! Public functions

    !> Encode ip1, ip2, ip3 triplet from (real value, kind) pairs
    function encode_ip_0(ip1, ip2, ip3, rp1, rp2, rp3) result(status) bind (c, name='EncodeIp')
        use iso_fortran_env, only: real32
        implicit none

        !> Encoded IP1 value
        integer(c_int), intent(out) :: ip1
        !> Encoded IP2 value
        integer(c_int), intent(out) :: ip2
        !> Encoded IP3 value
        integer(c_int), intent(out) :: ip3
        !> Level (or a pair of levels) in the atmosphere
        type(float_ip), intent(in)  :: rp1
        !> Time (or a pair of times)
        type(float_ip), intent(in)  :: rp2
        !> May contain anything, RP3%hi will be ignored (if RP1 or RP2 contains a pair, RP3 is ignored)
        type(float_ip), intent(in)  :: rp3

        !> \return CONVERT_OK on success, CONVERT_ERROR otherwise
        integer(c_int) :: status

        !> \warning When the status is CONVERT_ERROR, the values of IP1, IP2, IP3 are undefined (may contain anything)

        !> - This function is C interoperable
        !> - Reordering may happen if RP1 or RP2 contain a pair of values
        !> - Levels: ip1 will be lower in atmosphere than ip2
        !> - Times:  ip2 will be the end of the time range, ip3 will be the start of the time range
        !> - RP1 not a level or RP2 not a time will be flagged as an error
        !> - RP1 and RP2 both containing a range will be flagged as an error

        !> FORTRAN users must add the following code in order to access this function:
        !> \code
        !>     use ISO_C_BINDING
        !>     include 'convert_ip123.inc'
        !> \endcode

        external :: convip_plus

        real(kind = real32), dimension(3) :: P
        integer, dimension(3) ::kind
        character(len = 1) :: dummy
        integer :: i

        status = CONVERT_ERROR
        i = 0
        IP1 = -1 ; IP2 = -1 ; IP3 =-1
        P = 0
        kind = -1
        if (is_invalid_kind(RP1%kind) .or. is_invalid_kind(RP2%kind)) return   ! OOPS, invalid kind for RP1 or RP2

        if ( is_level(RP1%kind) ) then  !  RP1 is a valid level kind
            P(1) = RP1%lo ; kind(1) = RP1%kind ; i = i + 1
            if (RP1%hi /= RP1%lo) then       ! RP1 is a range
            P(3) = RP1%hi ; kind(3) = RP1%kind ; i = i + 1
            if (RP1%hi < RP1%lo .and. ascending(RP1%kind)) call swap(P(1), p(3))  ! keep lo, hi in atmospheric ascending order
            if (RP1%hi > RP1%lo .and. descending(RP1%kind)) call swap(P(1), p(3))  ! i.e. level lo lower in atmosphere than level hi
            endif
        else
            return  ! ERROR, RP1 must be a level
        endif

        if (RP2%kind == 10) then             !  RP2 is a valid time kind
            P(2) = RP2%lo ; kind(2) = RP2%kind ; i = i + 1
            if (RP2%hi /= RP2%lo) then  ! time range
            P(3) = RP2%hi ; kind(3) = RP2%kind ; i = i + 1
            if (RP2%hi > RP2%lo) call swap(P(2), P(3)) ! keep times in descending order p(2) > p(3) => ip2 > ip3
            endif
        else
            return  ! ERROR, RP2 must be a time
        endif

        if (i > 3) return  ! OOPS, we have 2 ranges

        if (i /= 3) then   !  no range was found, RP3 comes into play
            if (is_invalid_kind(RP3%kind)) return ! OOPS, invalid kind for RP3
            P(3) = RP3%lo ; kind(3) = RP3%kind ; i = i + 1
        endif

        call convip_plus(IP1, P(1), kind(1), +2, dummy, .false.)  ! NEW style encoding not negotiable
        call convip_plus(IP2, P(2), kind(2), +2, dummy, .false.)
        call convip_plus(IP3, P(3), kind(3), +2, dummy, .false.)
        status = CONVERT_OK
    end function encode_ip_0


    !> Decode (real value, kind) pairs from (ip1, ip2, ip3) triplet
    function decode_ip_0(rp1, rp2, rp3, ip1v, ip2v, ip3v) result(status) bind (c, name = 'DecodeIp')
        implicit none

        !> Level (or a pair of levels in atmospheric ascending order) in the atmosphere
        type(float_ip), intent(out) :: rp1
        !> Time (or a pair of times in ascending order)
        type(float_ip), intent(out) :: rp2
        !> Same as RP3%lo (if RP1 or RP2 contains a pair, RP3 is ignored)
        type(float_ip), intent(out) :: rp3
        !> IP1 encoded "new style", but old style can still be processed
        integer(c_int), value, intent(in) :: ip1v
        !> IP2 encoded "new style", but old style can still be processed
        integer(c_int), value, intent(in) :: ip2v
        !> IP3 encoded "new style", but old style can still be processed
        integer(c_int), value, intent(in) :: ip3v
        !> \return Status. Can be one of the following:
        !> | Code                   | Description                                  |
        !> | :--------------------- | :------------------------------------------- |
        !> | CONVERT_OK             | Conversion successful                        |
        !> | CONVERT_ERROR          | Error, (ip1 not level, ip2 not time, etc...) |
        !> | CONVERT_GOOD_GUESS     | Old style ip1 and/or ip2 are present         |
        !> | CONVERT_BAD_GUESS      | Old style ip3, interpreted as time           |
        !> | CONVERT_TERRIBLE_GUESS | Old style ip3, interpreted as arbitrary code |
        integer(C_INT) :: status

        !> \warning When the status is CONVERT_ERROR, the values of rp1, rp2, rp3 are undefined (may contain anything)

        ! - This function is C interoperable
        ! - Some reordering may take place when RP1 or RP2 contains a pair
        ! - Levels: ip1 will be lower in atmosphere than ip2
        ! - Limes:  ip2 will be the end of the time range, ip3 will be the start of the time range
        ! - IP1 not a level or ip2 not a time will be flagged as an error
        ! - RP1 and RP2 both containing a range will be flagged as an error

        !> FORTRAN users must add the following code in order to access this function:
        !> \code
        !>     use ISO_C_BINDING
        !>     include 'convert_ip123.inc'
        !> \endcode

        external :: convip_plus

        real(C_FLOAT), dimension(3) :: P
        integer, dimension(3) ::kind
        character(len = 1) :: dummy
        integer :: IP1, IP2, IP3

        IP1 = IP1V ; IP2 = IP2V ; IP3 = IP3V
        status = CONVERT_OK
        if (ip1 < 0 .or. ip2 < 0 .or. ip3 < 0 ) then
            status = ior(status, CONVERT_ERROR)
            return
        end if

        call convip_plus(IP1, P(1), kind(1), -1, dummy, .false.)  ! kind of ip1 should be a level
        if (.not. is_level(kind(1))) then
            ! ip1 is not a level
            status = ior(status, CONVERT_ERROR)
            return
        end if
        if (IP1 < 32768) status = ior(status, CONVERT_GOOD_GUESS)  ! reasonable guess if old style level
        RP1%lo = P(1) ; RP1%hi = P(1) ; RP1%kind = kind(1)

        if (IP2 < 32768) then                          ! IP2 is old style, probably a time value
            RP2%lo = IP2 ; RP2%hi = IP2 ;
            RP2%kind = 10                               ! time in hours ?
            status = ior(status , CONVERT_GOOD_GUESS)    ! reasonable guess
        else
            call convip_plus(IP2, P(2), kind(2), -1, dummy, .false.)  ! kind of ip2 should be new style time
            if (kind(2) /= KIND_HOURS) then
                ! ip2 not a time
                status = ior(status, CONVERT_ERROR)
                return
            end if
            RP2%lo = P(2) ; RP2%hi = P(2) ; RP2%kind = kind(2)
        endif

        if (IP3 < 32768) then                          ! IP3 is old style,
            RP3%lo = IP3 ; RP3%hi = IP3
            RP3%kind = 3
            status = ior(status, CONVERT_TERRIBLE_GUESS)  ! highly unreliable guess
        else
            call convip_plus(IP3, P(3), kind(3), -1, dummy, .false.)  ! kind of ip3 may be anything new style
            if (kind(3) == -1) then
                status = ior(status, CONVERT_ERROR)
                return
            end if
            RP3%lo = P(3) ; RP3%hi = P(3) ; RP3%kind = kind(3)
        endif

        if (kind(3) == 10 .and. kind(2) == kind(3)) then   ! time, same kind as ip2
            RP2%hi = P(3)
            RP3%lo = 0.0 ; RP3%hi = 0.0 ; RP3%kind = -1
            if (RP2%hi < RP2%lo) call swap(RP2%lo, RP2%hi)
        elseif (kind(3) <= 6 .and. kind(1) == kind(3)) then ! same level type as ip1
            RP1%hi = P(3)
            RP3%lo = 0.0 ; RP3%hi = 0.0 ; RP3%kind = -1
            if (RP1%hi < RP1%lo .and. ascending(kind(3))) call swap(RP1%lo, RP1%hi)
            if (RP1%hi > RP1%lo .and. descending(kind(3))) call swap(RP1%lo, RP1%hi)
        endif

        if (kind(1) > 6 .or. kind(2) /= 10) then  ! ip1 must be a level, ip2 must be a time
            status = ior(status, CONVERT_ERROR)       ! add bad coding flag
        endif
    end function decode_ip_0


    !> Vector version of encode_ip_0
    function encode_ip_1(IP, RP) result(status) BIND (C, name='EncodeIp_v')
        implicit none

        integer(C_INT) :: status
        integer(C_INT), dimension(3), intent(OUT) :: IP
        type(FLOAT_IP), dimension(3), intent(IN)  :: RP
        !> \see encode_ip_0

        status = encode_ip_0(IP(1), IP(2), IP(3), RP(1), RP(2), RP(3))
    end function encode_ip_1


    !> Vector version of decode_ip_0
    function decode_ip_1(RP, IP) result(status) BIND (C, name='DecodeIp_v')
        implicit none

        integer(C_INT) :: status
        integer(C_INT), dimension(3), intent(IN)  :: IP
        type(FLOAT_IP), dimension(3), intent(OUT) :: RP

        !> \see decode_ip_0

        status = decode_ip_0(RP(1), RP(2), RP(3), IP(1), IP(2), IP(3))
    end function decode_ip_1


    !> Encode three (value, kind) pairs into three ip values
    function encode_ip_2(IP1, IP2, IP3, P1, kkind1, P2, kkind2, P3, kkind3) result(status) bind(c, name='ConvertPKtoIP')
        implicit none

        !> Encoded IP1
        integer(c_int), intent(out) :: ip1
        !> Encoded IP2
        integer(c_int), intent(out) :: ip2
        !> Encoded IP3
        integer(c_int), intent(out) :: ip3
        !> Value to be encoded into IP1
        real(c_float), value, intent(in) :: p1
        !> Level kind
        integer(c_int), value, intent(in) :: kkind1
        !> Value to be encoded into IP2, should be a time but a level is accepted (and flagged as a WARNING)
        real(c_float), value, intent(in) :: p2
        !> Time or level kind
        integer(c_int), value, intent(in) :: kkind2
        !> Value to be encoded into IP3
        real(c_float), value, intent(in) :: p3
        !> Kind of the value to be encoded into IP3
        integer(c_int), value, intent(in) :: kkind3
        !> \return Status. Can be one of the following:
        !> | Code                   | Description                                  |
        !> | :--------------------- | :------------------------------------------- |
        !> | CONVERT_OK             | Conversion successful                        |
        !> | CONVERT_ERROR          | Error, (ip1 not level, ip2 not time, etc...) |
        !> | CONVERT_WARNING        | Coding convention error was corrected        |
        integer(c_int) :: status

        !> \warning When the status is CONVERT_ERROR, the values of IP1, IP2, IP3 are undefined (may contain anything)

        !> - This function is C interoperable
        !> - Tolerated coding deviations: kind2 a level instead of a time (will be pushed to position 3 and flagged as warning)
        !> - IP1/IP3 forced to the proper atmospheric ascending order coding (not flagged as warning)
        !> - IP2/IP3 forced to proper descending order (not flagged as warning)

        !> FORTRAN users must add the following code in order to access this function:
        !> \code
        !>     use ISO_C_BINDING
        !>     include 'convert_ip123.inc'
        !> \endcode

        external :: convip_plus

        character(len = 1) :: dummy
        integer(C_INT) :: kind1, kind2, kind3
        real(C_FLOAT) :: RP1, RP2, RP3

        ip1 = -1       ; ip2 = -1       ; ip3 = -1
        RP1 = P1       ; RP2 = P2       ; RP3 = P3
        kind1 = kkind1 ; kind2 = kkind2 ; kind3 = kkind3

        status = CONVERT_OK
        if (is_invalid_kind(kind1) .or. is_invalid_kind(kind2) .or. is_invalid_kind(kind3)) then
            status = ior(status, CONVERT_ERROR)
            return
        end if

        if (.not.is_level(kind1)) then
            ! ip1 must be a level
            status = ior(status, CONVERT_ERROR)
            return
        end if

        if (is_level(kind2)) then          ! ip2 should be a time, but a level is tolerated
            status = ior(status, CONVERT_WARNING)               ! warning if level
        else
            if (kind2 /= 10) then
                ! ip2 must be a time if not a level
                status = ior(status, CONVERT_ERROR)
                return
            end if
        endif

        call convip_plus(IP1, RP1, kind1, +2, dummy, .false.)  ! NEW style encoding not negotiable
        call convip_plus(IP2, RP2, kind2, +2, dummy, .false.)
        call convip_plus(IP3, RP3, kind3, +2, dummy, .false.)

        if (kind1 == kind2 .and. is_level(kind1) .and. kind3 == 10) then  ! level/level/time
            call swapi(ip2, ip3)  ! second level into ip3, time flag into ip2
            call swap(rp2, rp3)
            if (rp1 > rp3 .and.  ascending(kind1)) call swapi(ip1, ip3)  ! ip1, ip3 in atmospheric ascending order
            if (rp1 < rp3 .and. descending(kind1)) call swapi(ip1, ip3)
        else
            if (kind2 == 10 .and. kind3 == 10 .and. rp2 < rp3) call swapi(ip2, ip3)  ! level/time/time, put times in descending order
        endif
    end function encode_ip_2


    !> Decode ip1/2/3 into three (value, kind) pairs. Explicit, independent (ip) to (rp, kind) conversion
    function decode_ip_2(rp1, kind1, rp2, kind2, rp3, kind3, ip1v, ip2v, ip3v) result(status) bind(c, name='ConvertIPtoPK')
        implicit none

        !> Decoded ip1 value
        real(c_float), intent(out) :: rp1
        !> Value kind
        integer(c_int), intent(out) :: kind1
        !> Decoded ip2 value
        real(c_float), intent(out) :: rp2
        !> Value kind
        integer(c_int), intent(out) :: kind2
        !> Decoded ip3 value
        real(c_float), intent(out) :: rp3
        !> Value kind
        integer(c_int), intent(out) :: kind3
        !> IP1 to be decoded
        integer(c_int), value, intent(in) :: ip1v
        !> IP2 to be decoded
        integer(c_int), value, intent(in) :: ip2v
        !> IP3 to be decoded
        integer(c_int), value, intent(in) :: ip3v
        !> \return Status. Can be one of the following:
        !> | Code                   | Description                                  |
        !> | :--------------------- | :------------------------------------------- |
        !> | CONVERT_OK             | Conversion successful                        |
        !> | CONVERT_ERROR          | Error, (ip1 not level, ip2 not time, etc...) |
        !> | CONVERT_WARNING        | Coding convention violation                  |
        !> | CONVERT_GOOD_GUESS     | Old style ip1 and/or ip2 are present         |
        !> | CONVERT_BAD_GUESS      | Old style ip3, interpreted as time           |
        !> | CONVERT_TERRIBLE_GUESS | Old style ip3, interpreted as arbitrary code |
        integer(c_int) :: status

        !> \warning When the status is CONVERT_ERROR, the values of rp1, kind1, rp2, kind2, rp3, kind3 are undefined (may contain anything)

        !> - This function is C interoperable

        !> FORTRAN users must add the following code in order to access this function:
        !> \code
        !>     use ISO_C_BINDING
        !>     include 'convert_ip123.inc'
        !> \endcode

        external :: convip_plus

        character(len = 1) :: dummy
        integer :: IP1, IP2, IP3

        IP1 = IP1V ; IP2 = IP2V ; IP3 = IP3V
        status = CONVERT_OK
        if (ip1 < 0 .or. ip2 < 0 .or. ip3 < 0 ) then
            status = ior(status, CONVERT_ERROR)
            return
        end if

        call convip_plus(IP1, RP1, kind1, -1, dummy, .false.)   ! IP1 old style translation should be a safe bet
        if (kind1 == KIND_HOURS) then ! try to swap with ip2 is ip1 is time
            call swapi(ip1, ip2)
            call convip_plus(IP1, RP1, kind1, -1, dummy, .false.)
            status = ior(status, CONVERT_WARNING)
        endif
        if (is_invalid_kind(kind1)) then
            ! bad kind
            status = ior(status, CONVERT_ERROR)
            return
        end if
        if (IP1 < 32768) status = ior(status, CONVERT_GOOD_GUESS)
        if ( .not. is_level(kind1)) then
            ! ip1 must be a level
            status = ior(status, CONVERT_ERROR)
            return
        end if

        if (IP2 < 32768) then                          ! IP2 is old style, probably a time value
            RP2 = IP2
            kind2 = 10                                  ! time in hours ?
            status = ior(status, CONVERT_GOOD_GUESS)     ! reasonable guess
        else
            call convip_plus(IP2, RP2, kind2, -1, dummy, .false.)
            if (is_invalid_kind(kind2)) then
                ! bad kind
                status = ior(status, CONVERT_ERROR)
                return
            end if
            if (kind2 /= 10) then
                if (is_level(kind2)) then
                    status = ior(status, CONVERT_WARNING)  ! ip2 is supposed to be a TIME, a level is tolerated
                else
                    ! neither time nor level
                    status = ior(status, CONVERT_ERROR)
                    return
                endif
            endif
        endif
        if (IP3 < 32768) then                          ! IP3 is old style,
            kind3 = 3
            status = ior(status, CONVERT_TERRIBLE_GUESS) ! highly unreliable guess
        else
            call convip_plus(IP3, RP3, kind3, -1, dummy, .false.)
            if (is_invalid_kind(kind3)) then
                ! bad kind
                status = ior(status, CONVERT_ERROR)
                return
            end if
        endif
        if (kind1 == kind2 .and. kind3 == KIND_HOURS) then   ! level/level/time range in ip1/ip2/ip3
            call swap(RP2, RP3)       ! level/time/level
            call swapi(kind2, kind3)
        endif
        if (kind1 == kind3) then   ! level range
            if (ascending(kind1)  .and. RP1>RP3) call swap(RP1, RP3)   ! force increasing values
            if (descending(kind1) .and. RP1<RP3) call swap(RP1, RP3)   ! force decreasing values
        endif
        if (kind2 == kind3 .and. kind2 == KIND_HOURS) then   ! time range
            if (RP2 < RP3) call swap(RP2, RP3)    ! force decreasing time values
        endif
    end function decode_ip_2


    !> Vector version of encode_ip_2 (ConvertPKtoIP)
    function encode_ip_3(ip, rp, kind) result(status) bind(c, name='ConvertPKtoIP_v')
        implicit none
        !> \see encode_ip_2

        integer(c_int) :: status
        integer(c_int), dimension(3), intent(out) :: ip
        real(c_float), dimension(3), intent(in)   :: rp
        integer(c_int), dimension(3), intent(in)  :: kind

        status = encode_ip_2(ip(1), ip(2), ip(3), rp(1), kind(1), rp(2), kind(2), rp(3), kind(3))
    end function encode_ip_3


    !> Vector version of decode_ip_2
    function decode_ip_3(rp, kind, ip) result(status) bind(c, name='ConvertIPtoPK_v')
        implicit none
        !> \see decode_ip_2

        integer(c_int) :: status
        real(c_float),  dimension(3), intent(out) :: rp
        integer(c_int), dimension(3), intent(out) :: kind
        integer(c_int), dimension(3), intent(in)  :: ip

        status = decode_ip_2(rp(1), kind(1), rp(2), kind(2), rp(3), kind(3), ip(1), ip(2), ip(3))
    end function decode_ip_3
end module convert_ip123_int
