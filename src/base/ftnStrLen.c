#include <ftnStrLen.h>

//! Find the actual length of a Fortran string without the trailling blank padding
//! \return Length of the Fotran string without the trailling blank padding
unsigned int ftnStrLen(
    //! [in] String for which we want the length
    const char const * str,
    //! [in] Maximum length as defined for the Fortran string
    const unsigned int maxLen
) {
    unsigned int len = maxLen - 1;
    while(len > 1 && str[len] == ' ') {
        len--;
    }
    return len;
}
