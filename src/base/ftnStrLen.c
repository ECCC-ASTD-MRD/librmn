#include <rmn/ftnStrLen.h>

//! Find the actual length of a Fortran string without the trailling blank padding
//! \return Length of the Fotran string without the trailling blank padding
unsigned int ftnStrLen(
    //! [in] String for which we want the length
    const char * const str,
    //! [in] Maximum length as defined for the Fortran string
    const uint32_t maxLen
) {
    uint32_t len = maxLen - 1;
    while (len > 0 && (str[len] == ' ' || str[len] == '\0')) {
        len--;
    }
    if (str[len] != ' ' && str[len] != '\0') {
        len++;
    }
    return len;
}
