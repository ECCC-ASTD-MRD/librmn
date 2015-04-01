      program tstcc
      character * 256 ccard_args
      integer atyp, qqqoenv
      character *256 argum
      character * 6 argtyp(0:5)
      data argtyp / 'Fini','Nom','Valeur','Posit','Indef','Help' /

      call getenv('CCARD_ARGS',ccard_args)
      l = longueur(ccard_args)
      print *,'Debug l = ',l
      print *,'Debug ccard_args = ',ccard_args

      DO
         atyp = qqqoenv(argum,ccard_args,l)
         print *,'Argument de type ',argtyp(atyp),'  (',atyp,')',
     %  '    argum-->',argum
         if (atyp == 0) EXIT
      END DO
      stop
      end
