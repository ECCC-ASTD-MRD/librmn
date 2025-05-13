module interp_mod
    implicit none
contains
    pure function cubic(z1, z2, z3, z4, dx) result(z_out)
        use iso_fortran_env, only: real64
        implicit none

        real(kind = real64), parameter :: sixth = 1.0_real64 / 6.0_real64
        real(kind = real64), parameter :: third = 1.0_real64 / 3.0_real64
        real(kind = real64), intent(in) :: z1, z2, z3, z4, dx
        real(kind = real64) :: z_out
        z_out = ((((z4 - z1) * sixth + 0.5 * (z2 - z3)) * dx + 0.5 * (z1 + z3) - z2) * dx + z3 - sixth * z4 - 0.5 * z2 - third*z1) * dx + z2
    end function cubic


    pure function fa32(a1, a2, a3, a4, x, x1, x2, x3) result(f_out)
        use iso_fortran_env, only: real32
        implicit none

        real(kind = real32), intent(in) :: a1, a2, a3, a4
        real(kind = real32), intent(in) :: x, x1, x2, x3
        real(kind = real32) :: f_out
        f_out = a1 + (x-x1) * (a2 + (x-x2) * (a3 + a4 * (x-x3)))
    end function


    pure function fa32_2(c1, a1, a2) result(f_out)
        use iso_fortran_env, only: real32
        implicit none

        real(kind = real32), intent(in) :: c1, a1, a2
        real(kind = real32) :: f_out
        f_out = c1 * (a2-a1)
    end function


    pure function fa32_3(c1, c2, c3, a1, a2, a3) result(f_out)
        use iso_fortran_env, only: real32
        implicit none

        real(kind = real32), intent(in) :: c1, c2, c3, a1, a2, a3
        real(kind = real32) :: f_out
        f_out = c2 * (c3 * (a3-a2) - c1 * (a2-a1))
    end function


    pure function fa32_4(c1, c2, c3, c4, c5, c6, a1, a2, a3, a4) result(f_out)
        use iso_fortran_env, only: real32
        implicit none

        real(kind = real32), intent(in) :: c1, c2, c3, c4, c5, c6
        real(kind = real32), intent(in) :: a1, a2, a3, a4
        real(kind = real32) :: f_out
        f_out = c4 * (c5 * (c6 * (a4-a3) - c3 * (a3-a2)) - c2 * (c3 * (a3-a2) - c1 * (a2-a1)))
    end function


    pure function fa64(a1, a2, a3, a4, x, x1, x2, x3) result(f_out)
        use iso_fortran_env, only: real64
        implicit none

        real(kind = real64), intent(in) :: a1, a2, a3, a4
        real(kind = real64), intent(in) :: x, x1, x2, x3
        real(kind = real64) :: f_out
        f_out = a1 + (x-x1) * (a2 + (x-x2) * (a3 + a4 * (x-x3)))
    end function


    pure function fa64_2(c1, a1, a2) result(f_out)
        use iso_fortran_env, only: real64
        implicit none

        real(kind = real64), intent(in) :: c1, a1, a2
        real(kind = real64) :: f_out
        f_out = c1 * (a2-a1)
    end function

    pure function fa64_3(c1, c2, c3, a1, a2, a3) result(f_out)
        use iso_fortran_env, only: real64
        implicit none

        real(kind = real64), intent(in) :: c1, c2, c3, a1, a2, a3
        real(kind = real64) :: f_out
        f_out = c2 * (c3 * (a3-a2) - c1 * (a2-a1))
    end function

    pure function fa64_4(c1, c2, c3, c4, c5, c6, a1, a2, a3, a4) result(f_out)
        use iso_fortran_env, only: real64
        implicit none

        real(kind = real64), intent(in) :: c1, c2, c3, c4, c5, c6
        real(kind = real64), intent(in) :: a1, a2, a3, a4
        real(kind = real64) :: f_out
        f_out = c4 * (c5 * (c6 * (a4-a3) - c3 * (a3-a2)) - c2 * (c3 * (a3-a2) - c1 * (a2-a1)))
    end function


    pure function zlin32(zz1, zz2, zdx) result(z_out)
        implicit none

        real, intent(in) :: zz1, zz2, zdx
        real :: z_out

        z_out = zz1 + (zz2 - zz1) * zdx
    end function


    pure function zlin64(zz1, zz2, zdx) result(z_out)
        use iso_fortran_env, only: real64
        implicit none

        real(kind = real64), intent(in) :: zz1, zz2, zdx
        real(kind = real64) :: z_out

        z_out = zz1 + (zz2 - zz1) * zdx
    end function
end module interp_mod