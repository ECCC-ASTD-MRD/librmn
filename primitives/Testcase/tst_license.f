      program tstlic
      integer check_host_id
      external check_host_id
      id = check_host_id()
      print *,'RMNLIB license OK'
      stop
      end
