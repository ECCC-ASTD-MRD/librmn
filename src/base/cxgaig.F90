!> \file

!> Convert real grid descriptors to integer parameters
subroutine cxgaig(cgtyp, ig1, ig2, ig3, ig4, xg1, xg2, xg3, xg4)
    use iso_fortran_env, only : real64
    use app
    implicit none

    !> Grid type
    character(len = 1), intent(in) :: cgtyp
    !> First integer grid descriptor
    integer, intent(out) :: ig1
    !> Second integer grid descriptor
    integer, intent(out) :: ig2
    !> Third integer grid descriptor
    integer, intent(out) :: ig3
    !> Forth integer grid descriptor
    integer, intent(out) :: ig4
    !> First real grid descriptor
    real, intent(in) :: xg1
    !> Second real grid descriptor
    real, intent(in) :: xg2
    !> Third real grid descriptor
    real, intent(in) :: xg3
    !> Forth real grid descriptor
    real, intent(in) :: xg4

    !> Input descriptors meaning based on the grid type:
    !> | cgtyp |  xg1  |  xg2  |  xg3  |  xg4  |
    !> | ----: | ----: | ----: | ----: | ----: |
    !> |     N |    pi |    pj |   d60 |  dgrw |
    !> |     L |  lat0 |  lon0 |  dlat |  dlon |
    !> |     A |     a |     b |       |       |
    !> |     B |     a |     b |       |       |
    !> |     G |     a |     b |       |       |
    !> |     E |  lat1 |  lon1 |  lat2 |  lon2 |
    !> |     + |   lat |   lon |       |       |

    !> For A, B and G grid types, xg1 means the following:
    !> | xg1 | Meaning |
    !> | --: | ------: |
    !> |   0 |  Global |
    !> |   1 |   North |
    !> |   2 |   South |

    logical, external :: valide
    external :: llfxy

    real :: dlat, dlon
    real :: xxg2, xxg4
    real(kind = real64) :: xlat8, xlon8
    integer :: ihem, i2b
    logical :: status

    if (cgtyp == 'N' .or. cgtyp == 'S') then
        ig1 = nint(xg2 * 10.0)
        ig2 = nint(xg1 * 10.0)
        ig3 = nint(xg4 * 100.0)
        ig4 = nint(xg3 * 0.01)
        do while (ig3 < 0)
            ig3 = ig3 + 36000
        end do
        if (ig1 < 0 .or. ig2 < 0 .or. ig1 > 2047 .or. ig2 > 2047 .or. ig4 > 32000) then
            ig1 = 0
            ig2 = 0
            ig3 = 0
            ig4 = 32768
            if (xg3 > 204700) then ! ca ne passe pas en hectometres
                ig3 = 32768 ! on le met en kilometres
                ig1 = nint(xg3 * 0.001)
            else
                ig3 = 0
                ig1 = nint(xg3 * 0.01)
            endif
            ig2 = nint(xg4 * 10) ! dgrw en decidegres
            if (ig2 < 0) then
                ig2 = abs(ig2)
                ig4 = ig4 + 16384
            endif
            if (ig2 > 1800) then
                ig2 = abs(ig2 - 3600)
                ig4 = ig4 + 16384
            endif
            ihem = 1
            if ('S' == cgtyp) ihem = 2
            call llfxy(dlat, dlon, 1.0 -xg1, 1.0 - xg2, xg3, xg4, ihem)
            dlat = 90.0 - dlat   ! angle par rapport au pole nord
            if (dlon < 0) dlon = dlon + 360.0
            ig3 = ig3 + nint(dlon * 32767.0 / 360.0)
            ig4 = ig4 + nint(dlat * 16383.0 / 180.0)
        endif
    else if (cgtyp == 'A' .or. cgtyp == 'B' .or. cgtyp == 'G')  then
        ig1 = nint(xg1)
        ig2 = nint(xg2)
        ig3 = 0
        ig4 = 0
        status = valide("IG1", ig1, 0, 2) ! verifier si ig1=0, 1, ou 2
        status = valide("IG2", ig2, 0, 1) ! verifier si ig2=0 ou 1
    else if (cgtyp == 'C') then
        ! c type lat lon grid
        ig1 = nint(180.0 / xg3)
        ig2 = nint(360.0 / xg4)
        ig3 = nint((90.0 + xg1) * 100.0)
        ig4 = nint(xg2 * 100.0)
        do while (ig4 < 0)
            ig4 = ig4 + 36000
        end do
        if (ig3 < 0) call lib_log(APP_LIBRMN, APP_ERROR, 'cxgaig: Bad specification (LAT0)')
    else if (cgtyp == 'H') then
        ! lambert conforme centree
        ig1 = nint(5.0 * xg4) ! lambda0 en 5eme de degre
        do while (ig1 < 0)
            ig1 = ig1 + 1800
        end do
        ig2 = nint(0.002 * xg3) ! delta s en demi kilometres
        ig3 = nint(xg1) ! 200*phi1+phi2 (demi degres)
        ig4 = nint(100.0 * (90.0 + xg2)) ! phi0+90  en centidegres
    else if (cgtyp == 'L') then
        ig1 = nint(xg3 * 100.0)
        ig2 = nint(xg4 * 100.0)
        ig3 = nint((90.0 + xg1) * 100.0)
        ig4 = nint(xg2 * 100.0)
        do while (ig4 < 0)
            ig4 = ig4 + 36000
        end do
        if (ig3 < 0) call lib_log(APP_LIBRMN, APP_ERROR, 'cxgaig: Bad specification (LAT0)')
    else if (cgtyp == 'E') then
        ! grille lat, lon (gef)
        status = valide("XG1", nint(xg1), -90, 90)
        status = valide("XG3", nint(xg3), -90, 90)
        xxg2 = xg2
        xxg4 = xg4
        do while (xxg2 < 0)
            xxg2 = xxg2 + 360.0
        end do
        do while (xxg4 < 0)
            xxg4 = xxg4 + 360.0
        end do
        ig1 = nint((xg1 + 90.0) * 40.0)
        ig2 = nint(xg3 * 40.0)
        ig3 = nint((xxg2 + 90.0) * 40.0)

        ! bug de code, le +90 est de trop, ce qui peut causer un debordement pour ig3
        if (ig3 >= 16384) ig3 = ig3 - 16384
        ig4 = nint(xxg4 * 40.0)
        i2b = iand(ig1, 3)
        ig1 = ishft(ig1, -2)
        ig3 = ior(ishft(ig3, 2), i2b)
        if (ig2 < 0) ig2 = ig2 + 7201
        i2b = iand(ig2, 3)
        ig2 = ishft(ig2, -2)
        ig4 = ior(ishft(ig4, 2), i2b)
    else if (cgtyp == '+') then
        ! point lat, lon
        xlat8 = xg1
        status = valide("XG1", nint(xlat8), -90, 90) ! -90, +90
        xlon8 = xg2
        if (xlon8 < 0) xlon8 = xlon8 + 360.0 ! -180, +180 -> 0, 360
        status = valide("XG2", nint(xlon8), 0, 360)    ! 0, 360
        ! compatibilite arriere, centidegres (10 -> 19000)
        ig3  = nint( (xlat8 + 100.0 ) * 100.0 )
        ! compatibilite arriere, centidegres (0 -> 36000)
        ig4  = nint( xlon8 * 100.0 )
        ! en 1/100000 de degre
        ig1  = nint( (xlat8 + 100.0) * 100000.0 ) - ig3 * 1000
        ! correction, ig1 pourrait etre < 0  (500 -> 1500)
        ig1  = ig1 + 1000
        ig2  = nint( xlon8 * 100000.0 ) - ig4 * 1000 ! en 1/100000 de degre
        ! correction, ig2 pourrait etre < 0  (500 -> 1500)
        ig2  = ig2 + 1000
    else
        call lib_log(APP_LIBRMN, APP_ERROR, 'cxgaig: Unknown grid (TYPE)')
    end if
end subroutine
