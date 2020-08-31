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


#include <template.hf>
#define FNCNAME(fun) CAT(fun,EXTENSION)


! Integer functions:
#define DATATYPE integer
#define DATALENGTH 4

! 141
#define DIM 1
#define EXTENSION 141
#include "gmm_pointer_table.tmpl90"
#include "gmm_checkpoint.tmpl90"
#include "gmm_get.tmpl90"
#include "gmm_create.tmpl90"
#include "gmm_update_tpi_key.tmpl90"
#include "undefiner.tmpl90"

! 142
#define DIM 2
#define EXTENSION 142
#include "gmm_pointer_table.tmpl90"
#include "gmm_checkpoint.tmpl90"
#include "gmm_get.tmpl90"
#include "gmm_create.tmpl90"
#include "gmm_update_tpi_key.tmpl90"
#include "undefiner.tmpl90"

! 143
#define DIM 3
#define EXTENSION 143
#include "gmm_pointer_table.tmpl90"
#include "gmm_checkpoint.tmpl90"
#include "gmm_get.tmpl90"
#include "gmm_create.tmpl90"
#include "gmm_update_tpi_key.tmpl90"
#include "undefiner.tmpl90"

! 144
#define DIM 4
#define EXTENSION 144
#include "gmm_pointer_table.tmpl90"
#include "gmm_checkpoint.tmpl90"
#include "gmm_get.tmpl90"
#include "gmm_create.tmpl90"
#include "gmm_update_tpi_key.tmpl90"
#include "undefiner.tmpl90"

#undef DATALENGTH
#define DATALENGTH 8

! 181
#define DIM 1
#define EXTENSION 181
#include "gmm_pointer_table.tmpl90"
#include "gmm_checkpoint.tmpl90"
#include "gmm_get.tmpl90"
#include "gmm_create.tmpl90"
#include "gmm_update_tpi_key.tmpl90"
#include "undefiner.tmpl90"

! 182
#define DIM 2
#define EXTENSION 182
#include "gmm_pointer_table.tmpl90"
#include "gmm_checkpoint.tmpl90"
#include "gmm_get.tmpl90"
#include "gmm_create.tmpl90"
#include "gmm_update_tpi_key.tmpl90"
#include "undefiner.tmpl90"

! 183
#define DIM 3
#define EXTENSION 183
#include "gmm_pointer_table.tmpl90"
#include "gmm_checkpoint.tmpl90"
#include "gmm_get.tmpl90"
#include "gmm_create.tmpl90"
#include "gmm_update_tpi_key.tmpl90"
#include "undefiner.tmpl90"

! 184
#define DIM 4
#define EXTENSION 184
#include "gmm_pointer_table.tmpl90"
#include "gmm_checkpoint.tmpl90"
#include "gmm_get.tmpl90"
#include "gmm_create.tmpl90"
#include "gmm_update_tpi_key.tmpl90"
#include "undefiner.tmpl90"

#undef DATALENGTH
#undef DATATYPE


! Real Functions:
#define DATATYPE real
#define DATALENGTH 4

! 241
#define DIM 1
#define EXTENSION 241
#include "gmm_pointer_table.tmpl90"
#include "gmm_checkpoint.tmpl90"
#include "gmm_get.tmpl90"
#include "gmm_create.tmpl90"
#include "gmm_update_tpi_key.tmpl90"
#include "undefiner.tmpl90"

! 242
#define DIM 2
#define EXTENSION 242
#include "gmm_pointer_table.tmpl90"
#include "gmm_checkpoint.tmpl90"
#include "gmm_get.tmpl90"
#include "gmm_create.tmpl90"
#include "gmm_update_tpi_key.tmpl90"
#include "undefiner.tmpl90"

! 243
#define DIM 3
#define EXTENSION 243
#include "gmm_pointer_table.tmpl90"
#include "gmm_checkpoint.tmpl90"
#include "gmm_get.tmpl90"
#include "gmm_create.tmpl90"
#include "gmm_update_tpi_key.tmpl90"
#include "undefiner.tmpl90"

! 244
#define DIM 4
#define EXTENSION 244
#include "gmm_pointer_table.tmpl90"
#include "gmm_checkpoint.tmpl90"
#include "gmm_get.tmpl90"
#include "gmm_create.tmpl90"
#include "gmm_update_tpi_key.tmpl90"
#include "undefiner.tmpl90"

#undef DATALENGTH
#define DATALENGTH 8

! 281
#define DIM 1
#define EXTENSION 281
#include "gmm_pointer_table.tmpl90"
#include "gmm_checkpoint.tmpl90"
#include "gmm_get.tmpl90"
#include "gmm_create.tmpl90"
#include "gmm_update_tpi_key.tmpl90"
#include "undefiner.tmpl90"

! 282
#define DIM 2
#define EXTENSION 282
#include "gmm_pointer_table.tmpl90"
#include "gmm_checkpoint.tmpl90"
#include "gmm_get.tmpl90"
#include "gmm_create.tmpl90"
#include "gmm_update_tpi_key.tmpl90"
#include "undefiner.tmpl90"

! 283
#define DIM 3
#define EXTENSION 283
#include "gmm_pointer_table.tmpl90"
#include "gmm_checkpoint.tmpl90"
#include "gmm_get.tmpl90"
#include "gmm_create.tmpl90"
#include "gmm_update_tpi_key.tmpl90"
#include "undefiner.tmpl90"

! 284
#define DIM 4
#define EXTENSION 284
#include "gmm_pointer_table.tmpl90"
#include "gmm_checkpoint.tmpl90"
#include "gmm_get.tmpl90"
#include "gmm_create.tmpl90"
#include "gmm_update_tpi_key.tmpl90"
#include "undefiner.tmpl90"

#undef DATALENGTH
#undef DATATYPE

! Complex functions:

#define DATATYPE complex
#define DATALENGTH 8

! 381
#define DIM 1
#define EXTENSION 381
#include "gmm_pointer_table.tmpl90"
#include "gmm_checkpoint.tmpl90"
#include "gmm_get.tmpl90"
#include "gmm_create.tmpl90"
#include "gmm_update_tpi_key.tmpl90"
#include "undefiner.tmpl90"

! 382
#define DIM 2
#define EXTENSION 382
#include "gmm_pointer_table.tmpl90"
#include "gmm_checkpoint.tmpl90"
#include "gmm_get.tmpl90"
#include "gmm_create.tmpl90"
#include "gmm_update_tpi_key.tmpl90"
#include "undefiner.tmpl90"

! 383
#define DIM 3
#define EXTENSION 383
#include "gmm_pointer_table.tmpl90"
#include "gmm_checkpoint.tmpl90"
#include "gmm_get.tmpl90"
#include "gmm_create.tmpl90"
#include "gmm_update_tpi_key.tmpl90"
#include "undefiner.tmpl90"

! 384
#define DIM 4
#define EXTENSION 384
#include "gmm_pointer_table.tmpl90"
#include "gmm_checkpoint.tmpl90"
#include "gmm_get.tmpl90"
#include "gmm_create.tmpl90"
#include "gmm_update_tpi_key.tmpl90"
#include "undefiner.tmpl90"

#undef DATALENGTH
#undef DATATYPE


#undef FNCNAME