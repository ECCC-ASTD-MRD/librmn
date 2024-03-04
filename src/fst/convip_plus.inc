      interface

      FUNCTION kind_to_string(code) RESULT(string)  ! translate ip kind into a 2 character string code
        integer, intent(IN) :: code
        character(len=2) :: string
      end FUNCTION kind_to_string

      integer function value_to_string(val,string,maxlen)  
        import :: real32
        integer, intent(IN) :: maxlen
        character (len=*), intent(OUT) :: string
        real(kind = real32), intent(IN) :: val
      end function value_to_string

      end interface