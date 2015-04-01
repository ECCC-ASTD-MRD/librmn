      program tstip
      integer :: ip, kind
      real :: p,p2
      character(len=30) :: string

      external convip
!
!      SUBROUTINE CONVIP( ip, p, kind, mode, string, flag )
!
      call convip(ip,5.0,17,2,string,.false.)
      write(6,*) 'pour indice = 5. ip=',ip
      call convip(ip,p,kind,-1,string,.false.)
      write(6,*) 'au retour pour indice = 5.0 p=',p,' kind=',kind
      write(6,*)
      
      call convip(ip,76.,17,2,string,.false.)
      write(6,*) 'pour indice = 76. ip=',ip
      call convip(ip,p,kind,-1,string,.false.)
      write(6,*) 'au retour pour indice = 76. p=',p,' kind=',kind
      write(6,*)
       
      stop
      end
