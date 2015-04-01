      program tpow
      real *4 x(5),y(5),r(5)
      real *4 c

      do i = 1,5
         x(i) = 2.0
         y(i) = float(i)
      enddo
      call vspownn(r,x,y,5)
      print *,'r (nn) =',r
      print *,'r -x**y =',(r-x**y)/r
      print *,'r -exp  =',(r-exp(y*log(x)))/r
      c = 3.0
      call vspow1n(r,c,y,5)
      print *,'r (1n) =',r
      call vspown1(r,x,c,5)
      print *,'r (n1) =',r
      print *,'2**3=',2.0d0 ** 3.0d0
      stop
      end
