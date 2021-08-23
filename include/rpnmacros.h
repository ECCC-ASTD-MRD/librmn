#if !defined (_RPN_MACROS_)
#define _RPN_MACROS_ _rpn_macros_

#include <stdint.h>

#include <rpn_macros_arch.h>
#include <rpnmacros_global.h>

// Fail if propper arch specific definitions are not found
#ifndef F2Cl
#error "Architecture/Compiler specific macros not found!  Was the propper rpn_macros_arch.h included?"
#endif

#endif
