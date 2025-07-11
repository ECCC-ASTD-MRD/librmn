      interface
         !> Translate ip kind into a 2 character string code
         function kind_to_string(code) result(string)
            integer, intent(in) :: code
            character(len = 2) :: string
         end function kind_to_string

         integer function value_to_string(val, string, maxlen)
            use iso_fortran_env, only: real32
            real(kind = real32), intent(in) :: val
            character (len = *), intent(out) :: string
            integer, intent(in) :: maxlen
         end function value_to_string
      end interface
