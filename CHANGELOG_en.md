# What's new

- IgnoreTypeKindRank.hf  It allows Fortran compilers to ignore the rank
type and kind of functions arguments in formal interfaces.  This is
especially useful when calling C functions that expect `void *`

- The code has been made more portable; home-brewed tools are no longer
required to build the library.  Building the library is now done with
[CMake](https://cmake.org/documentation/)

- Added an explicit interface for fnom and fclos.  Add the following 
`#include <fnom.hf>` to use it.

- Added an explicit interface for MGI functions.  It can be used with
`#include <mgi.hf>`

- Added a new vertical coordinate type for IP1 (KIND=7) for depth
in meters below sea level

- Verbosity control is now managed through the [App](https://gitlab.science.gc.ca/RPN-SI/App) package and can be 
controled for the different components with the following methods:

  - C & Fortran: 
     - previous=Lib_LogLevel(APP_LIB`[lib]`,"`[niveau]`")  
     - previous=Lib_LogLevelNo(APP_LIB`[lib]`,`[niveau]`)
  
  - Environment: 
     - export APP_VERBOSE_LIB`[lib]`=`[niveau]`
    
  - Where: 
     - `[lib]` = RMN,FST,WB,GMM,VGRID or INTERPV
     - `[niveau]` = ERROR,WARNING,INFO,TRIVIAL,DEBUG,EXTRA or QUIET
# Breaks in backward compatibility

Version 20 is a major one and there has been a considerable code
cleanup done.  Obsolete functions and constructs have been removed.

- The case of file names will no longer be modified; capitalization in
file names will be kept as-is.

- The following data types will no longer be available.  Please use the
corresponding data types accessible via `#include <stdint.h>`

| Old      | New      |
| ---------| -------- |
| INT_32   |  int32_t |
| INT_64   |  int64_t |
| word     | uint32_t |
| wordint  |  int32_t |
| ftnfloat |    float |

- The `Amd64` macro was removed.  Where necessary, code should use
explicitly sized types such as `int32_t` or `int64_t`.

- The `string_copy` was deleted.  The standard C function `strncpy`
should be used instead.

- The Fortran function `longueur(str)` was deleted.  Fortran 90,
introduced the `len_trim(str)` built-in function that should be used
instead.  For C code handling Fortran strings, `ftnStrLen(str, maxLen)`
can be used.

- The `tell` was deleted.  Please use the corresponding POSIX function:
`ftell`.

- The `CLIB_SUFFIX` was deleted.

- arch_specific.hf was deleted since it is now empty

- Support for WIN32 has been removed.  The code has not been tested on
that platform for many years and was likely broken.

- Removed `IOPDATM`.  The function has been broken since U2 and
Operations will provide an alternate method to get run dates.

- The environment variable `AFSISIO` does not exist anymore and is 
replaced by `CMCCONST`. This variable is used to access the CMC constant 
directory and the BURF tables

- The `second()`, `_second()` et `__second()` functions which allowed to
query the CPU time used have been deleted.  External tools allow
retrieving that information or profiling.

- All header files except the main one (`rmn.h`) have been moved to the
`rmn` sub-directory.  Therefore, you will have to fix the paths in the
`#include` directives.  For example, `#include <rpnmacros.h>` becomes
`#include <rmn/rpnmacros.h>`.

- The program librmn_version is not available anymore.Information about
version, architecture and more can be retreived through the `rmn-config`
script. ex: `rmn-config --version --arch`

- GMM is now available as a module (`rmn_gmm`) and is no longer made
available as multiple *.hf files

- All the functions in DMMS are obsolete and should no longer be used:
    - memoirc
    - dmmsdbg
    - dmmsnabt
    - hpalloc
    - hpdeallc
    - ca_alloc
    - ca_deallc
    - memoirh
    - memoir

- All the following functions have already been removed from the library
since we expect that they are already no longer used.
    - VMM
    - BMF
        - bmf_catalog
        - bmf_char2i
        - bmf_i2char
        - bmf_clear
        - bmf_connect
        - bmf_error
        - bmf_get
        - bmf_get2
        - bmf_geth
        - bmf_gobe
        - bmf_init
        - bmf_list
        - bmf_perturb
        - bmf_splitend
        - bmf_splithalo
        - bmf_splithole
        - bmf_splitinit
        - bmf_splitname
        - bmf_splitoptions
        - bmf_splitstart
        - bmf_splitwrall
        - bmf_splitwrite
        - bmf_splitwrite2
        - bmf_splitwriteh
        - bmf_splitwriteh2
        - bmf_splitwritex
        - bmf_splitwritex2
        - bmf_splitwritexy
        - bmf_splitwritexy2
        - bmf_splitwritey
        - bmf_splitwritey2
        - bmf_statfld
        - bmf_write
        - bmf_write2
    - lamineur
        - slabopt
        - slabini
        - slabig34
        - slabdsc
        - slabxtr
        - slabxtrf
        - slabxtr
        - slabend
    - Spectral
        - ALPAS2
        - ALPAS8
        - ALPDL2
        - ALPDL8
        - ALPDR2
        - ALPDR8
        - ALPNM2
        - ALPNM8
        - DIMCAL
        - EPSIL2
        - EPSIL8
        - EWFDR2
        - FASP2
        - FASP8
        - GGASP0
        - GGASP8
        - GWAQD0
        - GWAQD8
        - PERM
        - SCOF2
        - SPAF2
        - SPAF8
        - SPAGG0
        - SPAGG8
    - twinbuffer
        - open_db_file
        - close_db_file
        - read_db_file
        - write_db_file
        - rewind_db_file
    - Base
        - AFIX
        - AFIX8
        - AMAX
        - AMEAN
        - AMIN
        - GDADCN
        - GDADGD
        - GDMPGD
        - GDSQRT
        - SLL
        - VLL
        - VPOLAGR
        - VPS
        - grid_to_grid
        - grid_to_grid_interp
        - grid_to_grid_coef
        - DEFVEC
        - FCONW
        - mxma
    - primitives
        - unstakw
        - stkmemw

## Finding deprecated functions in your code

The command below can help identify if your code uses deprecated
functions:

```
for fnctName in AFIX AFIX8 ALPAS2 ALPAS8 ALPDL2 ALPDL8 ALPDR2 ALPDR8 \
    ALPNM2 ALPNM8 AMAX AMEAN AMIN DEFVEC \
    DIMCAL EPSIL2 EPSIL8 EWFDR2 FASP2 FASP8 FCONW GDADCN \
    GDADGD GDMPGD GDSQRT GGASP0 GGASP8 GWAQD0 GWAQD8 IOPDATM PERM \
    SCOF2 SLL SPAF2 SPAF8 SPAGG0 SPAGG8 SPS VLL \
    VPOLAGR VPS bmf_catalog bmf_char2i bmf_clear bmf_connect bmf_error \
    bmf_get bmf_get2 bmf_geth bmf_gobe bmf_i2char bmf_init bmf_list \
    bmf_perturb bmf_splitend bmf_splithalo bmf_splithole bmf_splitinit \
    bmf_splitname bmf_splitoptions bmf_splitstart bmf_splitwrall \
    bmf_splitwrite bmf_splitwrite2 bmf_splitwriteh bmf_splitwriteh2 \
    bmf_splitwritex bmf_splitwritex2 bmf_splitwritexy \
    bmf_splitwritexy2 bmf_splitwritey bmf_splitwritey2 bmf_statfld \
    bmf_write bmf_write2 ca_alloc ca_deallc close_db_file dmmsdbg \
    dmmsnabt grid_to_grid grid_to_grid_coef grid_to_grid_interp \
    hpalloc hpdeallc memoir memoirc memoirh mxma open_db_file \
    read_db_file rewind_db_file slabdsc slabend slabig34 slabini \
    slabopt slabxtr slabxtr slabxtrf stkmemw unstakw write_db_file; do
    grepOut=$(grep --color=always -RinP "(?>\W)${fnctName}(?>\W)" | grep -v tests)
    if [[ -n "${grepOut}" ]]; then
        echo ${fnctName};
        echo "${grepOut}";
        echo "";
    fi
done
```
