      program tstip
      integer :: ip, kind
      real :: p,p2
      character(len=30) :: string

      external convip
!      goto 111
!
!      SUBROUTINE CONVIP( ip, p, kind, mode, string, flag )
!
      call convip(95345840,p,kind,-1,string,.true.)
      write(6,*) 'pour ip=',95345840,' p=',p
      write(6,*) 'string=',string
      read(string,*) p2
      write(6,*) 'p2=',p2
      write(6,*)

      call convip(ip,1.0,1,2,string,.false.)
      write(6,*) 'pour sigma = 1.0 ip=',ip
      call convip(ip,p,kind,-1,string,.false.)
      write(6,*) 'au retour pour sigma = 1.0 p=',p,' kind=',kind
      write(6,*)
!
 111  continue
      call convip(ip,0.25,1,2,string,.false.)
      write(6,*) 'pour sigma = 0.25 ip=',ip
      call convip(ip,p,kind,-1,string,.false.)
      write(6,*) 'au retour pour sigma = 0.25 p=',p,' kind=',kind
      write(6,*)
!      goto 222
!
      call convip(ip,0,2,2,string,.false.)
      write(6,*) 'pour pression = 0 ip=',ip
      call convip(ip,p,kind,-1,string,.false.)
      write(6,*) 'au retour pour pression = 0 p=',p,' kind=',kind
      write(6,*)
!
      call convip(ip,850.,2,2,string,.false.)
      write(6,*) 'pour pression = 850 ip=',ip
      call convip(ip,p,kind,-1,string,.false.)
      write(6,*) 'au retour pour pression = 850 p=',p,' kind=',kind
      write(6,*)
!
      call convip(ip,0.8,5,2,string,.false.)
      write(6,*) 'pour hybride = 0.8 ip=',ip
      call convip(ip,p,kind,-1,string,.true.)
      write(6,*) 'au retour pour hybride = 0.8 p=',p,' kind=',kind
      write(6,*) 'string=',string
      write(6,*)
!
      call convip(ip,0.0,5,2,string,.false.)
      write(6,*) 'pour hybride = 0.0 ip=',ip
      call convip(ip,p,kind,-1,string,.true.)
      write(6,*) 'au retour pour hybride = 0.0 p=',p,' kind=',kind
      write(6,*) 'string=',string
      write(6,*)
!
      call convip(ip,1.0,5,2,string,.false.)
      write(6,*) 'pour hybride = 1.0 ip=',ip
      call convip(ip,p,kind,-1,string,.true.)
      write(6,*) 'au retour pour hybride = 1.0 p=',p,' kind=',kind
      write(6,*) 'string=',string
      write(6,*)
!
      call convip(ip,50 000.0,21,2,string,.false.)
      write(6,*) 'pour GalChen = 50 000 ip=',ip
      call convip(ip,p,kind,-1,string,.false.)
      write(6,*) 'au retour pour GalChen = 50 000 p=',p,
     %           ' kind=',kind
      write(6,*)

!
      call convip(ip,0.0,21,2,string,.false.)
      write(6,*) 'pour GalChen = 0 ip=',ip
      call convip(ip,p,kind,-1,string,.false.)
      write(6,*) 'au retour pour GalChen = 0 p=',p,
     %           ' kind=',kind
      write(6,*)

!
      call convip(ip,250.0,6,2,string,.false.)
      write(6,*) 'pour Theta = 250 ip=',ip
      call convip(ip,p,kind,-1,string,.false.)
      write(6,*) 'au retour pour Theta = 250 p=',p,
     %           ' kind=',kind
      write(6,*)

 222  continue
!
      call convip(ip,0.25,10,2,string,.false.)
      write(6,*) 'pour Temps = 0.25 ip=',ip
      call convip(ip,p,kind,-1,string,.false.)
      write(6,*) 'au retour pour Temps = 0.25 p=',p,
     %           ' kind=',kind
      write(6,*)

!
      call convip(ip,0.25,10,2,string,.false.)
      write(6,*) 'pour Temps = 0.25 ip=',ip
      call convip(ip,p,kind,-1,string,.false.)
      write(6,*) 'au retour pour Temps = 0.25 p=',p,
     %           ' kind=',kind
      write(6,*)

!
      call convip(ip,0.0,2,2,string,.false.)
      write(6,*) 'pour p = 0.0 Mbar ip=',ip
      write(6,*)
      stop 'ici pour le moment'

!
      write(6,*) 'pour sigma hors limite = 1.5 ip=',ip
      call convip(ip,1.5,1,2,string,.false.)
      call convip(ip,p,kind,-1,string,.false.)
      write(6,*) 'au retour pour sigma = 1.5 p=',p,' kind=',kind
      write(6,*)
      stop
      end
