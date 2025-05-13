! RMNLIB - Library of useful routines for C and FORTRAN programming
! Copyright (C) 1975-2001  Division de Recherche en Prevision Numerique
!                          Environnement Canada
!
! This library is free software; you can redistribute it and/or
! modify it under the terms of the GNU Lesser General Public
! License as published by the Free Software Foundation,
! version 2.1 of the License.
!
! This library is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
! Lesser General Public License for more details.
!
! You should have received a copy of the GNU Lesser General Public
! License along with this library; if not, write to the
! Free Software Foundation, Inc., 59 Temple Place - Suite 330,
! Boston, MA 02111-1307, USA.


module igaxg95_helper
    implicit none
    save

    integer, parameter :: ncodemax = 32
    character(*), parameter :: star_line = "(' ', 79('*'))"

    character(len = 512) :: cgbinfo(ncodemax)
    character(len = 256) :: gdsinfo(ncodemax)

    integer :: qqqngb = 0
end module igaxg95_helper


integer function igagds95(gds, gtyin, ig1, ig2, ig3, ig4)
    use igaxg95_helper
    implicit none

    character(len = 1)  :: gtyin, gtyout
    character(len =256) :: gds
    integer ig1, ig2, ig3, ig4

    integer, external :: qqqgbld, qgblukig

    real xglst(20)
    integer nxg

    integer ier

    nxg = 20
    if (gtyin /= '!') then
        print star_line
        print *, ' <igagds95> Error: bad grid type ', gtyin
        igagds95 = -1
        return
    else
        ier = qqqgbld()
        if (ier < 0) then
            igagds95 = -1
            return
        endif

        ier = qgblukig(gtyout, xglst, nxg, gtyin, ig1, ig2, ig3, ig4)
        if (ier < 0) then
            igagds95 = -1
            return
        else
            gds = gdsinfo(ier)
            igagds95 = nint(xglst(14))
        endif
    endif
end


subroutine igaxg95(grtypout, xglst, nxg, grtypin, ig1, ig2, ig3, ig4)
    use igaxg95_helper, only: star_line
    implicit none

    character(len = 1), intent(out) :: grtypout
    character(len = 1), intent(in) :: grtypin
    integer, intent(in) :: nxg
    real, intent(out) :: xglst(nxg)
    integer, intent(inout) :: ig1, ig2, ig3, ig4

    integer, external :: qqqgbld, qgblukig

    integer ier

    grtypout = '*'
    if (grtypin /= '!') then
        if (nxg < 4) then
            print star_line
            print *, ' <igaxg95> Error: xglist not long enough'
            print *, ' <igaxg95> need 4 elements, list given has only ', nxg
            return
        endif
        call cigaxg(grtypin, xglst(1), xglst(2), xglst(3), xglst(4), ig1, ig2, ig3, ig4)
        grtypout = grtypin
        return
    else
        ier = qqqgbld()
        if (ier < 0) then
            return
        endif

        ier = qgblukig(grtypout, xglst, nxg, grtypin, ig1, ig2, ig3, ig4)
        ig3 = 256 * nint(xglst(13)) + nint(xglst(14))
        ig4 = 0
    endif
end


integer function qgblukig(gtyout, xglst, nxg, gtyin, ig1, ig2, ig3, ig4)
    use igaxg95_helper
    implicit none

    character(len = 1), intent(out) :: gtyout
    integer, intent(in) :: nxg
    real, intent(out) :: xglst(nxg)
    character(len = 1), intent(in) :: gtyin
    integer, intent(in) :: ig1, ig2, ig3, ig4


    integer i, gribcode, ni, nj, centercode, subcentercode, projcode
    real xlat00, xlon00, xlatninj, xlonninj, dx, dy, yaxislon, latin1, latin2
    logical found

    found = .false.

    gtyout = '*'
    i = 1
 10   if (i > qqqngb .and. .not. found) goto 999
      read (cgbinfo(i), *, err=110) gribcode
      if (gribcode == ig1) then
         if (nxg < 14) then
            print star_line
            print *, ' <qgblukig> Error: xglist not long enough'
            print *, ' <qgblukig> need 14 elements, list given has only', nxg
            qgblukig = -1
            return
         endif
         read (cgbinfo(i), *, err=110) gribcode, &
             centercode, subcentercode, projcode, gtyout, &
             xlat00, xlon00, dx, dy, &
             yaxislon, latin1, latin2, ni, nj, xlatninj, xlonninj, gdsinfo(i)

         xglst( 1)  = xlat00
         xglst( 2)  = xlon00
         xglst( 3)  = dx
         xglst( 4)  = dy
         xglst( 5)  = yaxislon
         xglst( 6)  = latin1
         xglst( 7)  = latin2
         xglst( 8)  = ni * 1.0
         xglst( 9)  = nj * 1.0
         xglst(10)  = xlatninj
         xglst(11)  = xlonninj
         xglst(12)  = centercode * 1.0
         xglst(13)  = subcentercode * 1.0
         xglst(14)  = projcode * 1.0

         if (ig2 == nint(xglst(12)) .or. ig2 == 0) then
            found = .true.
         endif
      else
         i = i + 1
         goto 10
      endif

      qgblukig = i
      return

 110  print star_line
      print *, ' <qgblukig> read error encountered in file "gribtable"'
      print *, ' <qgblukig> contents of file don''t match field description'
      goto 999

 999  qgblukig = -1
      print *, ' <qgblukig> GRIB code ', ig1, 'not found...'
end


subroutine qqqcltab(str, strlen)
    implicit none

    integer, intent(in) :: strlen
    character, intent(inout) :: str(strlen)

    integer i

    do i=1, strlen
        if (str(i) < ' ') str(i) = ' '
    enddo
end


integer function qqqgbld()
    use igaxg95_helper
    implicit none

#include <rmn/fnom.hf>
    character(len = 256) :: filename, armnlib
    character(len = 512) :: buffer
    integer ier, strlen, iun

    logical, save :: once = .false.

    if (once) then
        qqqgbld = 1
        return
    endif

    filename = 'gribtable'
    iun = 0
    ier = fnom(iun, filename, 'FTN+FMT+R/O+OLD', 0)
    if (ier /= 0) then
        print *, ' <qqqgbld> no local "gribtable" file found'
        ier = fclos(iun)
        call getenvc('CMCCONST', armnlib)
        strlen = len_trim(armnlib)
        print *, len(armnlib)
        filename = armnlib(1:strlen) // '/' // filename
        ier = fnom(iun, filename, 'FTN+FMT+R/O+OLD', 0)
        if (ier /= 0) then
            call getenvc('ARMNLIB', armnlib)
            strlen = len_trim(armnlib)
            print *, len(armnlib)
            filename = armnlib(1:strlen) // '/data/' // filename
            ier = fnom(iun, filename, 'FTN+FMT+R/O+OLD', 0)
            if (ier /= 0) then
                print star_line
                print *, ' <qqqgbld> no "gribtable" file found'
                print *, ' <qqqgbld> no "./gribtable" and  no "', filename(1:len_trim(filename)), '"'
                ier = fclos(iun)
                print *, ' <qqqgbld> using grib table in ', filename(1:len_trim(filename))
            endif
        endif
    endif

      rewind(iun)
 20   read(iun, "(a512)", err=110, end=120) buffer
      if (buffer(1:1) /= '#') then
         call qqqcltab(buffer, len(buffer))
         qqqngb = qqqngb + 1
         if (qqqngb > ncodemax) goto 111
         cgbinfo(qqqngb) = buffer
      endif
      goto 20

 120  print *, ' <qqqgbld> end-of-file reached'
      print *, ' <qqqgbld> found ', qqqngb, ' grids'
      once = .true.

      ier = fclos(iun)
      qqqgbld = 0
      return

 110  print star_line
      print *, ' <qqqgbld> read error encountered in file "gribtable"'
      ier = fclos(iun)
      qqqgbld = -1

 111  print star_line
      print *, ' <qqqgbld> internal string table too small'
      print *, ' <qqqgbld> modify code to allocate more space'
      ier = fclos(iun)
      qqqgbld = -1
end
