FUNCTION C_F_STRING_CONVERT(CPTR) RESULT(FPTR)
    USE ISO_C_BINDING
    IMPLICIT NONE
    ! Convert a null-terminated C string into a Fortran character array pointer
    TYPE(C_PTR), INTENT(IN) :: CPTR ! The C address
    CHARACTER(KIND=C_CHAR), DIMENSION(:), POINTER :: FPTR
    CHARACTER(C_CHAR), DIMENSION(1), SAVE, TARGET :: dummy_string="?"

    INTERFACE ! strlen is a standard C function from <string.h>
        ! int strlen(char *string)
        FUNCTION strlen(string) RESULT(len) BIND(C,NAME="strlen")
            USE ISO_C_BINDING
            INTEGER(C_INT) :: len
            TYPE(C_PTR), VALUE :: string ! A C pointer
        END FUNCTION
    END INTERFACE

    IF(C_ASSOCIATED(CPTR)) THEN
        CALL C_F_POINTER(FPTR=FPTR, CPTR=CPTR, SHAPE=[strlen(CPTR)])
    ELSE
        ! To avoid segfaults, associate FPTR with a dummy target:
        FPTR=>dummy_string
    END IF
END FUNCTION C_F_STRING_CONVERT


