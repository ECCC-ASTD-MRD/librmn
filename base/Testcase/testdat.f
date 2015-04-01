      program tstdat

      integer stamp,dat1,dat2,dat3

*INTEGER FUNCTION NEWDATE (dat1, dat2, dat3, mode)

*      integer newdate
      external newdate

      stamp = 294563600
      call newdate(stamp,dat2,dat3,-3)

      print *,'Debug dat2=',dat2,' dat3=',dat3

*      ier = newdate(294563600,dat2,dat3,-3)
*      print *,'Debug #2 dat2=',dat2,' dat3=',dat3

      stop
      end
