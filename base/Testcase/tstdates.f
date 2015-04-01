      program tstdates
      
      INTEGER Debut, cmcd , A1900, A1901, A1980, A1979
*     Date de depart pour references et calcul de difference (1 jan 1989)
      DATA Debut /010189000/          ! 1989 01 01 00 00 00
      DATA A1900 /010100000/          ! 1900 01 01 00 00 00
      DATA A1901 /010101000/          ! 1901 01 01 00 00 00
      DATA A1980 /010180000/          ! 1980 01 01 00 00 00
      DATA A1979 /010179000/          ! 1979 01 01 00 00 00
      DATA cmcd /289742750/           ! 2001 02 09 07 30 00

      EXTERNAL difdatr
      real *8 nhours

      print *
      print *,'Debug debut = 01 Jan 1989 ',Debut
      call difdatr(cmcd,Debut,nhours)
      jds8 = nhours*3600. + 0.5             ! nint en integer*8
      print *,'Debug nhours=',nhours

      print *
      print *,'Debug debut = 01 Jan 1900 ',A1900
      call difdatr(cmcd,A1900,nhours)
      print *,'Debug nhours=',nhours

      print *
      print *,'Debug debut = 01 Jan 1901 ',A1901
      call difdatr(cmcd,A1901,nhours)
      print *,'Debug nhours=',nhours

      print *
      print *,'Debug debut = 01 Jan 1980 ',A1980
      call difdatr(cmcd,A1980,nhours)
      print *,'Debug nhours=',nhours

      print *
      print *,'Debug debut = 01 Jan 1979 ',A1979
      call difdatr(cmcd,A1979,nhours)
      print *,'Debug nhours=',nhours

      stop
      END 
