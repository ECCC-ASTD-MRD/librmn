!> Return pressure given a unit number and a list of fstkeys of the RPN standard file
integer function hyb2pres(iun, fstkeys, NK, NI, NJ, PX, logPX_L)
    use app
    use rmn_date
    use rmn_fst98, only: fstinf, fstprm, fstluk
    implicit none

    !> Unit number of of the input RPN standard file
    integer, intent(in) :: iun
    !> First field dimension
    integer, intent(inout) :: NI
    !> Second field dimension
    integer, intent(inout) :: NJ
    !> Number of keys
    integer, intent(inout) :: NK
    !> Keys tagged to records in the RPN standard file
    integer, intent(in) :: fstkeys(NK)
    !> Output field
    real, intent(out) :: PX(NI, NJ, NK)
    !> Output PX in ln(Pascals) if true, output PX in mb otherwise
    logical, intent(in) ::  logPX_L

    !> \return 0 on success, -1 otherwise
    integer, external :: read_decode_hyb, hyb_to_pres, eta_to_pres, sigma_to_pres, etasef_to_pres

    include 'rmn/convert_ip123.inc'

    integer :: ip1(NK)
    real :: hyb(NK), p0(NI, NJ), work(NI, NJ)
    integer :: nia, nja, nka, ni1, nj1, nk1, i, j, k
    integer :: e1_key, hy_key, pt_key, p0_key, xx_key
    integer :: datev, dateo, deet, ipas, ip1a, ip2a, ip3a
    integer :: ig1a, ig2a, ig3a, ig4a, bit, datyp
    integer :: swa, lng, dlf, ubc, ex1, ex2, ex3, kind
    real :: lev, ptop, pref, rcoef, etatop
    character(len = 1) :: tva, grda, blk_S
    character(len = 4) :: var
    character(len = 12) :: etik_S

    hyb2pres = 0

    hyb2pres = fstprm(fstkeys(1), dateo, deet, ipas, nia, nja, nka, &
        bit, datyp, ip1a, ip2a, ip3a, tva, var, etik_S, grda, &
        ig1a, ig2a, ig3a, ig4a, swa, lng, dlf, ubc, ex1, ex2, ex3)
    if (hyb2pres < 0) then
        write(app_msg, *) 'hyb2pres: fstprm failed on key', fstkeys(1)
        call lib_log(APP_LIBRMN, APP_ERROR, app_msg)
        return
    else
        call convip_plus(ip1a, lev, kind, -1, blk_S, .false.)
        ip1(1) = ip1a
        hyb(1) = lev
    endif
    do k = 2, NK
        hyb2pres = fstprm(fstkeys(k), dateo, deet, ipas, ni1, nj1, nk1, &
            bit, datyp, ip1a, ip2a, ip3a, tva, var, etik_S, grda, &
            ig1a, ig2a, ig3a, ig4a, swa, lng, dlf, ubc, ex1, ex2, ex3)
        if (ni1 /= nia .and. nj1 /= nja .and. nk1 /= nka.or.hyb2pres < 0) then
            write(app_msg, *) 'hyb2pres: fstprm on key', fstkeys(k), 'dim mismatch'
            call lib_log(APP_LIBRMN, APP_ERROR, app_msg)
            return
        endif
        call convip_plus(ip1a, lev, kind, -1, blk_S, .false.)
        ip1(k) = ip1a
        hyb(k) = lev
    enddo
    if (kind /= 1 .and. kind /= 2 .and. kind /= 5) then
        write(app_msg, *) 'hyb2pres: kind = ', kind, ' has to be 1, 2 or 5'
        call lib_log(APP_LIBRMN, APP_ERROR, app_msg)
        hyb2pres = -1
        return
    endif
    !> \todo Check what the actual intent of ni, nj, nk should actually be.
    !> The old documentation (prior to doxygen) defined those parameters as intent(in), but
    !> fstinf has intent(inout) for them. To preserve the old behaviour, their intent in this
    !> was changed to inout.
    hy_key = fstinf(iun, ni, nj, nk, -1, ' ', -1,  -1,  -1, ' ', 'HY')
    pt_key = fstinf(iun, ni, nj, nk, -1, ' ', -1,  -1,  -1, ' ', 'PT')
    e1_key = fstinf(iun, ni, nj, nk, -1, ' ', -1,  -1,  -1, ' ', 'E1')

    call incdatr(datev, dateo, ipas * deet / 3600.0d0)

    if (kind == 1) then
        p0_key = fstinf(iun, ni, nj, nk, datev, etik_S, -1, ip2a, ip3a, ' ', 'P0')
        if (p0_key < 0) then
            call lib_log(APP_LIBRMN, APP_ERROR, 'hyb2pres: No p0 found, kind = 1')
            hyb2pres = -1
            return
        else
            hyb2pres = fstluk(p0, p0_key, ni, nj, nk)
        endif
        if (pt_key >= 0) then
            hyb2pres = fstluk(work, pt_key, ni, nj, nk)
            if (hyb2pres < 0) then
                call lib_log(APP_LIBRMN, APP_ERROR, 'hyb2pres: fstluk failed on PT')
                return
            endif
            ptop = work(1, 1)
            if (e1_key >= 0) then
                ! etasef coordinate found
                hyb2pres = fstluk(work, e1_key, ni, nj, nk)
                if (hyb2pres < 0) then
                    call lib_log(APP_LIBRMN, APP_ERROR, 'hyb2pres: fstluk failed on E1')
                    return
                endif
                etatop = work(1, 1)
                hyb2pres = etasef_to_pres(PX, hyb, ptop, etatop, p0, NI, NJ, NK)
                if (logPX_L) PX(:, :, :) = log(100.0 * PX(:, :, :))
                return
            else
                ! eta coordinate found
                hyb2pres = eta_to_pres(PX, hyb, ptop, p0, NI, NJ, NK)
                if (logPX_L) PX(:, :, :) = log(100.0 * PX(:, :, :))
                return
            endif
        else if (hy_key >= 0) then
            ! hybrid (normalized) coordinate found
            hyb2pres = read_decode_hyb(iun, 'HY',  -1,  -1, ' ', -1, ptop, pref, rcoef)
            if (hyb2pres < 0) then
                call lib_log(APP_LIBRMN, APP_ERROR, 'hyb2pres: read_decode_hyb error')
                return
            endif
            hyb2pres = hyb_to_pres(PX, hyb, ptop, rcoef, pref, kind, p0, NI, NJ, NK)
            if (logPX_L) PX(:, :, :) = log(100.0 * PX(:, :, :))
            return
        else
            ! sigma coordinate found
            hyb2pres = sigma_to_pres(PX, hyb, p0, NI, NJ, NK)
            if (logPX_L) PX(:, :, :) = log(100.0 * PX(:, :, :))
        endif
    endif

    if (kind == 2) then
        ! pressure coordinate found
        do k = 1, NK
            do j = 1, nJ
                do i = 1, nI
                    PX(i, j, k) = hyb(k) * 100.0
                enddo
            enddo
        enddo
        return
    endif

    if (kind == 5) then
        ! vstag coordinate found
        xx_key = fstinf(iun, ni, nj, nk, -1, etik_S, -1, -1, -1 , ' ', '!!  ')
        if (xx_key >= 0) then
            call lib_log(APP_LIBRMN, APP_ERROR, 'hyb2pres: coordinate is not ready')
            return
        else if (hy_key >= 0) then
            ! hybrid (un-normalized) coordinate found
            hyb2pres = read_decode_hyb(iun, 'HY',  -1,  -1, ' ', -1, ptop, pref, rcoef)
            if (hyb2pres < 0) then
                call lib_log(APP_LIBRMN, APP_ERROR, 'hyb2pres: read_decode_hyb error')
                return
            endif
            p0_key = fstinf(iun, ni, nj, nk, datev, etik_S, -1, ip2a, ip3a, ' ', 'P0')
            if (p0_key < 0) then
                call lib_log(APP_LIBRMN, APP_ERROR, 'hyb2pres: HY found, No p0 found, kind=5')
                hyb2pres = -1
                return
            else
                hyb2pres = fstluk(p0, p0_key, ni, nj, nk)
            endif
            hyb2pres = hyb_to_pres(PX, hyb, ptop, rcoef, pref, kind, p0, NI, NJ, NK)
            return
        else
            call lib_log(APP_LIBRMN, APP_ERROR, 'hyb2pres: kind=5 but !! nor  HY NOT FOUND')
            hyb2pres = -1
            return
        endif
    endif
end


!> Convert from hybrid to pressure
integer function hybrid_to_pres(pressure, hybm, ptop, ps, NI, NJ, rcoef, pref, hyb, NK)
    use app
    use rmn_common
    implicit none

    !> First dimension of the field
    integer, intent(in) :: NI
    !> Second dimension of the field
    integer, intent(in) :: NJ
    !> Number of levels in hybm
    integer, intent(in) :: NK
    !> Array of pressure levels (same units as ps)
    real, intent(out) :: pressure(NI * NJ, NK)
    !> Array of model hybrid levels (0.0 to 1.0) - calculated using ptop, rcoef and pref
    real, intent(out) ::  hybm(NK)
    !> Average pressure at the top (mb)
    real, intent(in) :: ptop
    ! Greater done Space Pressure at the surface (mb or pascals)
    real, intent(in) :: ps(NI * NJ)
    !> Coefficient (1.0 to 2.0)
    real, intent(inout) :: rcoef
    !> Reference pressure (mb), normally 800 mb
    real, intent(in) :: pref
    !> Array of user-defined hybrid levels (0.0 to 1.0)
    real, intent(in) :: hyb(NK)

    !> \return 0 on success, -1 otherwise
    !> \warning Only good for normalized hybrid!

    integer i, k
    real(kind = real64) :: hybm_8(nk), prpref, pr1, pibb(nk), pia(nk)
    real(kind = real64) :: conv, fact

    hybrid_to_pres = -1
    call lib_log(APP_LIBRMN, APP_WARNING, 'hybrid_to_pres: function hybrid_to_pres will calculate only a NORMALIZED (kind=1) hybrid coordinate')
    call lib_log(APP_LIBRMN, APP_WARNING, 'hybrid_to_pres: RECOMMEND using hyb_to_pres function')

    if (rcoef < 1.0 .or. rcoef > 2.0) then
        call lib_log(APP_LIBRMN, APP_ERROR, 'hybrid_to_pres: rcoef must be between 1.0 and 2.0')
        return
    endif
    if (pref < 400 .or. pref > 1050) then
        call lib_log(APP_LIBRMN, APP_ERROR, 'hybrid_to_pres: pref must be a value between 400 and 1050')
        return
    endif
    if (ptop < 0 .or. ptop > 1200) then
        call lib_log(APP_LIBRMN, APP_ERROR, 'hybrid_to_pres:  ptop  must be a value between 0 and 1200')
        return
    endif
    if (abs(rcoef - 1.0) < 1.0e-5) then
        !> \todo Figure out if it makes sense that this parameter is modified by the function.
        !> The old documentation (prior to doxygen) to find a para meter as being input only, but
        !> it's modified here.
        rcoef = 1.0
    end if

    fact = 1.0
    ! detect if ps is in millibars or pascals
    if (ps(1) < 40000.0) then
        conv = 100.0
        fact = fact / 100.0
    else
        conv = 1.0
    endif


    do k = 1, nk
        if (hyb(k) < 0 .or. hyb(k) > 1.0) then
        call lib_log(APP_LIBRMN, APP_ERROR, 'hybrid_to_pres: invalid value(s) in hybrid coordinate array')
        return
        endif
        hybm_8(k) = hyb(k) + (1 - hyb(k)) * ptop / pref
    enddo

    prpref = 100.0 * ptop / hybm_8(1)

    pr1 = 1.0 / (1.0 - hybm_8(1))
    do k = 1, nk
        pibb(k) = ((hybm_8(k) - hybm_8(1)) * pr1) ** rcoef
        pia(k) = prpref * ( hybm_8(k) - pibb(k) )
    enddo

    do k = 1, nk
        pibb(k) = pibb(k) * conv
        do i = 1, ni * nj
            pressure(i, k) = real( (pia(k) + pibb(k) * ps(i)) * fact )
        enddo
        hybm(k) = real( hybm_8(k) )
    enddo
    call lib_log(APP_LIBRMN, APP_WARNING, 'hybrid_to_pres: Recommend to use hyb_to_pres')
    hybrid_to_pres = 0
end


!> Derive IG values given hybrid reference values
integer function hybref_to_ig(ig1, ig2, ig3, ig4, rcoef, pref, x1, x2)
    use app
    implicit none

    !> IG1
    integer, intent(out) :: ig1
    !> IG2
    integer, intent(out) :: ig2
    !> IG3
    integer, intent(out) :: ig3
    !> IG4
    integer, intent(out) :: ig4
    !> Coefficient (1.0 to 2.0)
    real, intent(in) :: rcoef
    !> Reference pressure (normally 800 mb)
    real, intent(in) :: pref
    !> Unused
    real, intent(in) :: x1
    !> Unused
    real, intent(in) :: x2

    !> \return 0 on success, -1 otherwise

    ! Hack to prevent unused dummy argument warning without actually changing the interface
    if (x1 > 0.0) continue
    if (x2 > 0.0) continue

    hybref_to_ig = -1
    if (pref < 400.0 .or. pref > 1050.0) then
        call lib_log(APP_LIBRMN, APP_ERROR, 'hybref_to_ig: pref must be between 400.0 and 1050.0')
        return
    endif
    if (rcoef < 1.0 .or. rcoef > 2.0) then
        call lib_log(APP_LIBRMN, APP_ERROR, 'hybref_to_ig: rcoef must be between 1.0 and 2.0')
        return
    endif
    ig1 = int(pref)
    ig2 = int(rcoef * 1000.0)
    ig3 = 0
    ig4 = 0
    hybref_to_ig = 0
end


!> Derive hybrid reference values given ig? values
integer function ig_to_hybref(ig1, ig2, ig3, ig4, rcoef, pref, x1, x2)
    use app
    implicit none

    !> IG1
    integer, intent(in) :: ig1
    !> IG2
    integer, intent(in) :: ig2
    !> IG3
    integer, intent(in) :: ig3
    !> IG4
    integer, intent(in) :: ig4
    !> Coefficient (1.0 to 2.0)
    real, intent(out) :: rcoef
    !> Reference pressure (normally 800 mb)
    real, intent(out) :: pref
    !> Unused
    real, intent(in) :: x1
    !> Unused
    real, intent(in) :: x2

    !> \return 0 on success, -1 otherwise

    ! Hack to prevent unused dummy argument warning without actually changing the interface
    if (x1 > 0.0) continue
    if (x2 > 0.0) continue

    ig_to_hybref = -1
    pref = ig1
    rcoef = ig2 / 1000.0
    if (pref < 400.0 .or. pref > 1050.0) then
        call lib_log(APP_LIBRMN, APP_ERROR, 'ig1 must be between 400 and 1050')
        return
    endif
    if (rcoef < 1.0 .or. rcoef > 2.0) then
        call lib_log(APP_LIBRMN, APP_ERROR, 'ig_to_hybref: rcoef(ig2/1000) must be between 1.0 and 2.0')
        return
    endif
    if (ig3 /= 0) then
        call lib_log(APP_LIBRMN, APP_ERROR, 'ig_to_hybref: ig3 must be 0')
        return
    endif
    if (ig4 /= 0) then
        call lib_log(APP_LIBRMN, APP_ERROR, 'ig_to_hybref: ig4 must be 0')
        return
    endif
    ig_to_hybref = 0
end


!> Derive hybrid reference values given a selected FSTD record
integer function read_decode_hyb(iun, nom, ip2, ip3, etik, date, ptop, pref, rcoef)
    use app
    implicit none

    !> Unit number of the file in which to search
    integer, intent(in) :: iun
    !> Variable name for to search
    character(len = *), intent(in) :: nom
    !> IP2 for to search
    integer, intent(in) :: ip2
    !> IP3 for to search
    integer, intent(in) :: ip3
    !> Label for the search
    character(len = *), intent(in) :: etik
    !> Validity date for the search
    integer, intent(in) :: date
    !> Pressure at the top
    real, intent(out) :: ptop
    !> Reference pressure (mb), normally 800 mb
    real, intent(out) :: pref
    !> Coefficient (1.0 to 2.0)
    real, intent(out) :: rcoef

    !> \return KEY of FSTD record upon success, a negative error code otherwise

    integer, external :: fstinf, fstprm, ig_to_hybref

    include 'rmn/convert_ip123.inc'

    integer  l, deet, ip1a, ip2a, ip3a, ig1a, ig2a, ig3a, ig4a, bit
    integer  idayo, dty,  swa,  lng,  dlf,  ubc,  ex1,  ex2, ex3
    integer  npas, nia, nja, i, j, k, ierr, kind
    real     x1, x2
    character(len=1) typ, grda, blk_S
    character(len=4) var
    character(len=12) labanl

    ! typvar of HY must be X
    l = fstinf(iun, i, j, k, date, etik, -1, ip2, ip3, 'X', nom)
    read_decode_hyb = l   !!! BUG FIX read_decode_hyb now properly set
    if (l >= 0) then
        ierr= fstprm ( l, idayo, deet, npas, nia, nja, k, bit, dty, &
                    ip1a, ip2a, ip3a, typ, var, labanl, grda,       &
                    ig1a, ig2a, ig3a, ig4a, swa, lng, dlf, ubc, ex1, ex2, ex3 )
        call convip_plus(ip1a, ptop, kind, -1, blk_S, .false.)
        if (ptop < 0.0.or.ptop > 1200..or. kind /= 2) then
            write(app_msg, *) 'read_decode_hyb: Decoding of ip1 in ', nom
            call lib_log(APP_LIBRMN, APP_ERROR, app_msg)
            read_decode_hyb = -1
        endif
        ierr=ig_to_hybref(ig1a, ig2a, ig3a, ig4a, rcoef, pref, x1, x2)
        if (ierr < 0) then
            write(app_msg, *) 'read_decode_hyb: Decoding of ig?? in ', nom
            call lib_log(APP_LIBRMN, APP_ERROR, app_msg)
            read_decode_hyb = -1
        endif
    else
        write(app_msg, *) 'read_decode_hyb: Record ', nom, ' of typvar X is not found'
        call lib_log(APP_LIBRMN, APP_ERROR, app_msg)
        read_decode_hyb = -2 ! SPECIFIC error code if record not found
    endif
end

!> Encode the given hybrid reference values into IG parameters and write out the FSTD record with given IP!, etik, datev
integer function write_encode_hyb(iun, nom, ip2, ip3, etik, date, ptop, pref, rcoef)
    use app
    use rmn_fst98, only: fstecr
    implicit none

    !> Unit number of the file into which to write
    integer, intent(in) :: iun
    !> Variable name or the field written
    character(len = *), intent(in) :: nom
    !> IP2 value for the field written
    integer, intent(in) :: ip2
    !> IP3 value for the field written
    integer, intent(in) :: ip3
    !> Label for the field written
    character(len = *), intent(in) :: etik
    !> Validity date for the field written
    integer, intent(in) :: date
    !> Surface pressure at the top (mb)
    real, intent(in) :: ptop
    !> Reference pressure (normally 800 mb)
    real, intent(in) :: pref
    !> Coefficient (1.0 to 2.0)
    real, intent(in) :: rcoef

    !> \returns 0 on success, -1 otherwise

    integer, external :: hybref_to_ig

    include 'rmn/convert_ip123.inc'

    integer :: ip1, ig1, ig2, ig3, ig4
    ! We have to copy to value of ptop to a local variable since convip_plus sets the intent of many parameters to inout
    real :: lptop
    integer :: kind
    integer :: ierr
    real :: x1, x2
    character(len = 1) blk_S

    if (ptop < 0.0.or.ptop > 1200.) then
        write(app_msg, *) 'write_encode_hyb: Encoding of ip1 in ', nom
        call lib_log(APP_LIBRMN, APP_ERROR, app_msg)
        write_encode_hyb = -1
        return
    endif
    kind = 2
    lptop = ptop
    call convip_plus(ip1, lptop, kind, +1, blk_S, .false.)
    ierr = hybref_to_ig(ig1, ig2, ig3, ig4, rcoef, pref, x1, x2)
    if (ierr < 0) then
        write(app_msg, *) 'write_encode_hyb: Encoding of ig?? in ', nom
        call lib_log(APP_LIBRMN, APP_ERROR, app_msg)
        write_encode_hyb = -1
        return
    endif
    x1 = ptop
    write_encode_hyb = fstecr([x1], [x2], -32, iun, date, 0, 0, 1, 1, 1, ip1, ip2, ip3, 'X', nom, etik, 'X', ig1, ig2, ig3, ig4, 5, .true.)
end

!> Write the given hybrid reference values into a binary file
integer function write_bin_hyb(iun, nom, ip2, ip3, etik, datev, ptop, pref, rcoef)
    use app
    implicit none

    !> Unit number of the file into which to write
    integer, intent(in) :: iun
    !> Variable name or the field written
    character(len = 4), intent(in) :: nom
    !> IP2 value for the field written
    integer, intent(in) :: ip2
    !> IP3 value for the field written
    integer, intent(in) :: ip3
    !> Label for the field written
    character(len = 12), intent(in) :: etik
    !> Validity date for the field written
    integer, intent(in) :: datev
    !> Surface pressure at the top (mb)
    real, intent(in) :: ptop
    !> Reference pressure (normally 800 mb)
    real, intent(in) :: pref
    !> Coefficient (1.0 to 2.0)
    real, intent(in) :: rcoef

    !> \returns 0 on success, -1 otherwise

    write_bin_hyb = 0
    if (ptop < 0.0 .or. ptop > 1200.) then
        call lib_log(APP_LIBRMN, APP_ERROR, 'write_bin_hyb: ptop out of range')
        write_bin_hyb = -1
        return
    endif
    if (pref < 400.0 .or. pref > 1050.0) then
        call lib_log(APP_LIBRMN, APP_ERROR, 'write_bin_hyb: pref must be between 400.0 and 1050.0')
        write_bin_hyb = -1
        return
    endif
    if (rcoef < 1.0 .or. rcoef > 2.0) then
        call lib_log(APP_LIBRMN, APP_ERROR, 'write_bin_hyb: rcoef must be between 1.0 and 2.0')
        write_bin_hyb = -1
        return
    endif
    write(iun)nom, etik, ip2, ip3, datev, ptop, rcoef, pref
end


!> Read the hybrid reference values from a binary file
integer function read_bin_hyb(iun, nom, ip2, ip3, etik, datev, ptop, pref, rcoef)
    use app
    implicit none

    !> Unit number of the file in which to search
    integer, intent(in) :: iun
    !> Variable name for to search
    character(len = 4), intent(out) :: nom
    !> IP2 for to search
    integer, intent(out) :: ip2
    !> IP3 for to search
    integer, intent(out) :: ip3
    !> Label for the search
    character(len = 12), intent(out) :: etik
    !> Validity date for the search
    integer, intent(out) :: datev
    !> Pressure at the top
    real, intent(out) :: ptop
    !> Reference pressure (mb), normally 800 mb
    real, intent(out) :: pref
    !> Coefficient (1.0 to 2.0)
    real, intent(out) :: rcoef

    !> \returns 0 on success, -1 otherwise

    read_bin_hyb = 0
    read(iun) nom, etik, ip2, ip3, datev, ptop, rcoef, pref
    if (ptop < 0.0 .or. ptop > 1200.) then
        read_bin_hyb = -1
        call lib_log(APP_LIBRMN, APP_ERROR, 'read_bin_hyb: ptop out of range')
        return
    endif
    if (pref < 400.0 .or. pref > 1050.0) then
        call lib_log(APP_LIBRMN, APP_ERROR, 'read_bin_hyb: pref must be between 400.0 and 1050.0')
        read_bin_hyb = -1
        return
    endif
    if (rcoef < 1.0 .or. rcoef > 2.0) then
        call lib_log(APP_LIBRMN, APP_ERROR, 'read_bin_hyb: rcoef must be between 1.0 and 2.0')
        read_bin_hyb = -1
        return
    endif
end


!> Convert from hybrid to pressure(mb) which includes the kind value
integer function hyb_to_pres(pressure, hyb, ptop, rcoef, pref, kind, ps, NI, NJ, NK)
    use app
    use rmn_common
    implicit none

    !> First field dimension
    integer, intent(in) :: NI
    !> Second field dimension
    integer, intent(in) :: NJ
    !> Number of levels in hyb
    integer, intent(in) :: NK
    !> Array of pressure levels (mb)
    real, intent(out) :: pressure(NI * NJ, NK)
    !> Array of user-defined hybrid levels (0.0 to 1.0)
    real, intent(in) :: hyb(NK)
    !> Average pressure at the top (mb)
    real, intent(in) :: ptop
    !> Coefficient (1.0 to 2.0)
    real, intent(in) :: rcoef
    !> Reference pressure (mb), normally 800 mb
    real, intent(in) :: pref
    !> 1 = normalized, 5 = unnormalized
    integer, intent(in) :: kind
    !> 2D pressure at the surface (mb)
    real, intent(in) :: ps(NI * NJ)

    !> \return 0 on success, -1 otherwise

    !> Derive pressure fields from levels derived by "convip_plus",
    !> the kind value from the ip1 codes and the hybrid reference
    !> parameters(ptop, rcoef, pref)

    integer i, k
    real(kind = real64) :: hybm_8(nk), pr1, pibb(nk), pia(nk)

    hyb_to_pres = -1

    if (kind == 1) then
        do k=1, nk
            hybm_8(k)= hyb(k) + (1.-hyb(k)) * ptop/pref
        enddo
    else if (kind == 5) then
        do k = 1, nk
            hybm_8(k) = hyb(k)
        enddo
    else
        call lib_log(APP_LIBRMN, APP_ERROR, 'hyb_to_pres: kind is not 1 nor 5')
        return
    endif

    pr1 = 1.0 / (1.0 - ptop / pref)
    do k = 1, nk
        pibb(k) = (dmax1(hybm_8(k) - ptop / pref, 0.0d0) * pr1 ) ** rcoef
        pia(k) = pref * ( hybm_8(k) - pibb(k) )
    enddo

    do k = 1, nk
        do i = 1, ni * nj
            pressure(i, k) = real( pia(k) + pibb(k) * ps(i) )
        enddo
    enddo
    hyb_to_pres = 0
end


!> Derive pressure fields from model eta levels (levels used by the model) and parameter(ptop) and p0
integer function eta_to_pres(pressure, hybm, ptop, ps, NI, NJ, NK)
    !> \return 0 upon success, -1 if there is an error

    use rmn_common
    implicit none

    !> Ni dimension of field
    integer, intent(in) :: NI
    !> Nj dimension of field
    integer, intent(in) :: NJ
    !> Number of level in hybm
    integer, intent(in) :: NK
    !> Average pressure at the top (mb)
    real, intent(in) :: ptop
    !> Array of model eta levels (0.0 to 1.0)
    real, intent(in) :: hybm(NK)
    !> 2D pressure at the surface (mb)
    real, intent(in) :: ps(NI * NJ)
    !> Array of pressure levels (mb)
    real, intent(out) :: pressure(NI * NJ, NK)

    integer :: i, k
    real(kind = real64) :: pibb(nk), pia(nk)

    eta_to_pres = -1

    do k = 1, nk
        pibb(k) = hybm(k)
        pia(k) = ptop * (1.0d0 - hybm(k))
    enddo

    do k = 1, nk
        do i = 1, ni * nj
        pressure(i, k) = real( pia(k) + pibb(k) * ps(i) )
        enddo
    enddo
    eta_to_pres = 0
end


!> Convert from eta in model SEF to pressure(mb)
integer function etasef_to_pres(pressure, hybm, ptop, etatop, ps, NI, NJ, NK)
    use rmn_common
    implicit none

    !> Ni dimension of field
    integer, intent(in) :: NI
    !> Nj dimension of field
    integer, intent(in) :: NJ
    !> Number of level in hybm
    integer, intent(in) :: NK
    !> Average pressure at the top (mb)
    real, intent(in) :: ptop
    !> Eta at top of model (0.0 to 1.0)
    real, intent(in) :: etatop
    !> Array of model eta levels (0.0 to 1.0)
    real, intent(in) :: hybm(NK)
    !> 2D pressure at the surface (mb)
    real, intent(in) :: ps(NI * NJ)
    !> Array of pressure levels (mb)
    real, intent(out) :: pressure(NI * NJ, NK)

    !> Derive pressure fields from SEF model levels (value from IP1 using convip_plus), the ptop (PT), the etatop (E1) and p0
    !> \return 0 on success, -1 otherwise

    integer :: i, k
    real(kind = real64) :: pibb(nk), pia(nk), eta1

    etasef_to_pres = -1

    eta1 = 1.0 / (1.0 - etatop)
    do k = 1, nk
        pibb(k) = (hybm(k) - etatop) * eta1
        pia(k) = ptop * ( 1.0d0 - pibb(k))
    enddo

    do k = 1, nk
        do i = 1, ni * nj
            pressure(i, k) = real( pia(k) + pibb(k) * ps(i) )
        enddo
    enddo
    etasef_to_pres = 0
end

!> Convert from sigma to pressure(mb)
integer function sigma_to_pres(pressure, hybm, ps, NI, NJ, NK)
    !> Derive pressure fields from model sigma levels
    !> \return 0 upon success, -1 if there is an error

    implicit none

    !> Ni dimension of field
    integer, intent(in) :: NI
    !> Nj dimension of field
    integer, intent(in) :: NJ
    !> Number of level in hybm
    integer, intent(in) :: NK
    !> Array of model sigma levels (0.0 to 1.0)
    real, intent(in) :: hybm(NK)
    !> 2D pressure at the surface (mb)
    real, intent(in) :: ps(NI * NJ)
    !> Array of pressure levels (mb)
    real, intent(out) :: pressure(NI * NJ, NK)

    integer :: i, k

    sigma_to_pres = -1
    do k = 1, nk
        do i = 1, ni * nj
            pressure(i, k) = hybm(k) * ps(i)
        enddo
    enddo
    sigma_to_pres = 0
end
