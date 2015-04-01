   subroutine ez_rgdint_1_1m(zout,zin,x,y,masque)
   implicit none
   real zout
   real zin(2,2),x,y,z1,z2
   integer masque(2,2)
   real a11,a21,a31,a12,a22,a32,a13,a23,a33

   a11 = zin(1,1)
   a21 = 0.5*(zin(1,1)+zin(2,1))
   a31 = zin(2,1)
   a12 = a11
   a22 = 0.3333*(zin(1,1)+zin(2,1)+zin(2,2))
   a32 = 0.5*(zin(2,1)+zin(2,2))
   a23 = zin(2,2)
   a33 = zin(2,2)
   if (masque(1,2) == 0) then
      if (x < 0.5) then
         if (y < 0.5) then
            z1   = a11 + 2.0 * x * (a21-a11)
            z2   = a12 + 2.0 * x * (a22-a12)
            zout = z1  + 2.0 * y * (z2 - z1)
         else
            print *, 'incorrect value'
         endif
      else
         if (y < 0.5) then
            z1   = a21 + 2.0 * (x - 0.5) * (a31-a21)
            z2   = a22 + 2.0 * (x - 0.5) * (a32-a22)
            zout = z1  + 2.0 * y * (z2 - z1)
         else
            z1   = a22 + 2.0 * (x-0.5) * (a32-a22)
            z2   = a23 + 2.0 * (x-0.5) * (a33-a23)
            zout = z1  + 2.0 * (y-0.5) * (z2 - z1)
         endif
      endif
   endif

   end subroutine ez_rgdint_1_1m