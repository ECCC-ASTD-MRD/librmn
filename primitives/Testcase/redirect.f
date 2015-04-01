      program redirect
      integer ier,fnon
      external fnom

      ier = fnom(6,'outputfile','seq',0)
      write(6,*) 'Debug debut du programme'
      print *,'Debug ligne print *'
      stop
      end
