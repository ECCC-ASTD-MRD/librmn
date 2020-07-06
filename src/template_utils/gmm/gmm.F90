! Sadly, the concatenations of other macros can not be automated since in traditional mode,
! preprocessors introduce spaces when concatenating

! The GNU Fortran documentation, see URL below, states that the GNU C
! Preprocessor is invoqued in the traditional mode when compiling Fotran.
! This explains why macro expension does not work the same way as when called
! from gcc instead of gfortran.
! https://gcc.gnu.org/onlinedocs/gfortran/Preprocessing-and-conditional-compilation.html

! Names are built the following way:
!   DATACODE : A code for the data type (integer = 1, real = 2, complex = 3)
!   DATALENGTH : The size, in bytes, of each data element
!   DIM : Dimension of the array
!
! Together <DATACODE><DATALENGTH><DIM> form the EXTENSION added to function
! names after an underscore.
!
! FNCNAME is a macro that appends an underscore and the EXTENSION to its
! argument to return the actual function name.

! Integer functions:
#define DATATYPE integer
#define DATALENGTH 4

! 141
#define DIM 1
#define EXTENSION 141
#define FNCNAME(name) name/**/_/**/141
#include "gmm_checkpoint_tmpl.F90"
#include "gmm_get_tmpl.F90"
#include "undefiner.hf"

! 142
#define DIM 2
#define EXTENSION 142
#define FNCNAME(name) name/**/_/**/142
#include "gmm_checkpoint_tmpl.F90"
#include "gmm_get_tmpl.F90"
#include "undefiner.hf"

! 143
#define DIM 3
#define EXTENSION 143
#define FNCNAME(name) name/**/_/**/143
#include "gmm_checkpoint_tmpl.F90"
#include "gmm_get_tmpl.F90"
#include "undefiner.hf"

! 144
#define DIM 4
#define EXTENSION 144
#define FNCNAME(name) name/**/_/**/144
#include "gmm_checkpoint_tmpl.F90"
#include "gmm_get_tmpl.F90"
#include "undefiner.hf"

#undef DATALENGTH
#define DATALENGTH 8

! 181
#define DIM 1
#define EXTENSION 181
#define FNCNAME(name) name/**/_/**/181
#include "gmm_checkpoint_tmpl.F90"
#include "gmm_get_tmpl.F90"
#include "undefiner.hf"

! 182
#define DIM 2
#define EXTENSION 182
#define FNCNAME(name) name/**/_/**/182
#include "gmm_checkpoint_tmpl.F90"
#include "gmm_get_tmpl.F90"
#include "undefiner.hf"

! 183
#define DIM 3
#define EXTENSION 183
#define FNCNAME(name) name/**/_/**/183
#include "gmm_checkpoint_tmpl.F90"
#include "gmm_get_tmpl.F90"
#include "undefiner.hf"

! 184
#define DIM 4
#define EXTENSION 184
#define FNCNAME(name) name/**/_/**/184
#include "gmm_checkpoint_tmpl.F90"
#include "gmm_get_tmpl.F90"
#include "undefiner.hf"

#undef DATALENGTH
#undef DATATYPE


! Real Functions:
#define DATATYPE real
#define DATALENGTH 4

! 241
#define DIM 1
#define EXTENSION 241
#define FNCNAME(name) name/**/_/**/241
#include "gmm_checkpoint_tmpl.F90"
#include "gmm_get_tmpl.F90"
#include "undefiner.hf"

! 242
#define DIM 2
#define EXTENSION 242
#define FNCNAME(name) name/**/_/**/242
#include "gmm_checkpoint_tmpl.F90"
#include "gmm_get_tmpl.F90"
#include "undefiner.hf"

! 243
#define DIM 3
#define EXTENSION 243
#define FNCNAME(name) name/**/_/**/243
#include "gmm_checkpoint_tmpl.F90"
#include "gmm_get_tmpl.F90"
#include "undefiner.hf"

! 244
#define DIM 4
#define EXTENSION 244
#define FNCNAME(name) name/**/_/**/244
#include "gmm_checkpoint_tmpl.F90"
#include "gmm_get_tmpl.F90"
#include "undefiner.hf"

#undef DATALENGTH
#define DATALENGTH 8

! 281
#define DIM 1
#define EXTENSION 281
#define FNCNAME(name) name/**/_/**/281
#include "gmm_checkpoint_tmpl.F90"
#include "gmm_get_tmpl.F90"
#include "undefiner.hf"

! 282
#define DIM 2
#define EXTENSION 282
#define FNCNAME(name) name/**/_/**/282
#include "gmm_checkpoint_tmpl.F90"
#include "gmm_get_tmpl.F90"
#include "undefiner.hf"

! 283
#define DIM 3
#define EXTENSION 283
#define FNCNAME(name) name/**/_/**/283
#include "gmm_checkpoint_tmpl.F90"
#include "gmm_get_tmpl.F90"
#include "undefiner.hf"

! 284
#define DIM 4
#define EXTENSION 284
#define FNCNAME(name) name/**/_/**/284
#include "gmm_checkpoint_tmpl.F90"
#include "gmm_get_tmpl.F90"
#include "undefiner.hf"

#undef DATALENGTH
#undef DATATYPE

! Complex functions:

#define DATATYPE complex
#define DATALENGTH 8

! 381
#define DIM 1
#define EXTENSION 381
#define FNCNAME(name) name/**/_/**/381
#include "gmm_checkpoint_tmpl.F90"
#include "gmm_get_tmpl.F90"
#include "undefiner.hf"

! 382
#define DIM 2
#define EXTENSION 382
#define FNCNAME(name) name/**/_/**/382
#include "gmm_checkpoint_tmpl.F90"
#include "gmm_get_tmpl.F90"
#include "undefiner.hf"

! 383
#define DIM 3
#define EXTENSION 383
#define FNCNAME(name) name/**/_/**/383
#include "gmm_checkpoint_tmpl.F90"
#include "gmm_get_tmpl.F90"
#include "undefiner.hf"

! 384
#define DIM 4
#define EXTENSION 384
#define FNCNAME(name) name/**/_/**/384
#include "gmm_checkpoint_tmpl.F90"
#include "gmm_get_tmpl.F90"
#include "undefiner.hf"

#undef DATALENGTH
#undef DATATYPE
