!> \file

!> Passe des parametres (entiers) descripteurs de grille aux parametres reels
subroutine cigaxg(cgtyp, xg1, xg2, xg3, xg4, ig1, ig2, ig3, ig4)
    use iso_fortran_env, only: real64
    use app
    implicit none

    !> Type de grille
    character(len = 1), intent(in) :: cgtyp
    !> Descripteur de grille réels
    real, intent(out) :: xg1, xg2, xg3, xg4
    !> Descripteurs de grille entiers
    integer, intent(in) :: ig1, ig2, ig3, ig4

    !   OUT   - XG1   - !! DESCRIPTEUR DE GRILLE (REEL),
    !   OUT   - XG2   -    CGTYP = 'N', PI, PJ, D60, DGRW
    !   OUT   - XG3   -    CGTYP = 'L', LAT0, LON0, DLAT, DLON,
    !   OUT   - XG4   -    CGTYP = 'A', 'B', 'G', XG1 = 0. GLOBAL,
    !                                                 = 1. NORD
    !                                                 = 2. SUD !!
    !     CGTYP = 'E', LAT1, LON1, LAT2, LON2
    !     CGTYP = '+', LAT, LON, dummy, dummy

    external :: xyfll

    real(kind = real64) :: XG18, XG28
    real :: DLAT, DLON
    integer :: I2B, IHEM, JG3, JG4, LG1, LG2, LG3, LG4

    IF ((CGTYP .EQ. 'N') .OR. (CGTYP .EQ.'S')) THEN
        IF(IG4 .LT. 32768) THEN
            ! ANCIEN STYLE DE CODAGE
            XG1 = IG2 * 0.1
            XG2 = IG1 * 0.1       ! PJ
            XG3 = IG4 * 100.      ! D60
            XG4 = IG3 * 0.01      ! DGRW
        ELSE
            ! NOUVEAU STYLE PLUS GENERAL
            JG3 = IG3
            JG4 = IG4
            JG4 = JG4 - 32768
            XG3 = IG1 * 100.      ! D60 NORMALEMENT EN HECTOMETRES
            IF (IG3 .GT. 32767) THEN
                ! C'EST EN KILOMETRES
                XG3 = XG3 * 10.
                JG3 = JG3 - 32768
            ENDIF
            XG4 = IG2 * .1        ! ABS(DGRW)
            IF (JG4 .GT. 16383) THEN
                ! DGRW NEGATIF
                XG4 = 360. - XG4
                JG4 = JG4 - 16384
            ENDIF
            DLAT = 90. -(JG4*180./16383.)
            DLON = (JG3*360./32767.)
            IHEM = 1
            IF ('S'.EQ.CGTYP) IHEM = 2
            CALL XYFLL(XG1, XG2, DLAT, DLON, XG3, XG4, IHEM)
            XG1 = 1.0 - XG1
            XG2 = 1.0 - XG2
        ENDIF

    ELSE IF(CGTYP .EQ. 'C') THEN
        ! C TYPE LAT LON GRID
        XG1 = IG3 * 0.01 - 90.
        XG2 = IG4 * 0.01
        XG3 = 180. / IG1
        XG4 = 360. / IG2

    ELSE IF ((CGTYP .EQ. 'A') .OR. (CGTYP .EQ. 'B') .OR. (CGTYP .EQ. 'G')) THEN
        XG1 = IG1
        XG2 = IG2
        XG3 = 0.
        XG4 = 0.

    ELSE IF(CGTYP .EQ. 'L') THEN
        XG1 = IG3 * 0.01 - 90.
        XG2 = IG4 * 0.01
        XG3 = IG1 * 0.01
        XG4 = IG2 * 0.01

    ELSE IF(CGTYP .EQ. 'H') THEN
        XG1 = IG3                !  PHI1 / PHI2
        XG2 = .01*IG4 - 90.      !  PHI0
        XG3 = 500*IG2            !  DELTA S  (DEMI KILOMETRES)
        XG4 = IG1*.2             !  LAMBDA0  GRILLE LAMBERT CONFORME CENTREE NORD

    ELSE IF(CGTYP .EQ. 'E') THEN
        I2B = IAND(IG3, 3)
        LG3 = ISHFT(IG3, -2)
        LG1 = IOR(ISHFT(IG1, 2), I2B)
        I2B = IAND(IG4, 3)
        LG4 = ISHFT(IG4, -2)
        LG2 = IOR(ISHFT(IG2, 2), I2B)
        if (lg2.gt.3600)  lg2=lg2-7201
        XG1 = (LG1 -3600.0D0) / 40.0
        ! bug de code, le +90 est de trop, ce qui peut causer un debordement pour ig3
        if (lg3 .lt. 3559) lg3=lg3+16384
        XG2 = (LG3 -3600.0D0) / 40.0
        XG3 = LG2 / 40.0D0
        XG4 = LG4 / 40.0D0
        !  GRILLE LAT, LON (GEF)
    ELSE IF(CGTYP .EQ. '+') THEN
        XG18 = (IG3*.01_8) - 100.0   ! compatibilite arriere
        ! correction au 1/100000 de degre
        if(IG1.ne.0) XG18 = XG18 + (IG1-1000)*.00001_8
        XG28 = (IG4*.01_8)           ! compatibilite arriere
        ! correction au 1/100000 de degre
        if(IG2.ne.0) XG28 = XG28 + (IG2-1000)*.00001_8
        XG1 = XG18
        XG2 = XG28
        XG3 = 0.0    ! dummy
        XG4 = 0.0    ! dummy
    else
        call lib_log(APP_LIBRMN, APP_ERROR, 'cigaxg: Bad grid specification (TYPE)')
    endif
end
