# What's new

- IgnoreTypeKindRank.hf  It allows Fortran compilers to ignore the rank
type and kind of functions arguments in formal interfaces.  This is
especially useful when calling C functions that expect `void *`

- The code has been made more portable; home-brewed tools are no longer
required to build the library.  Building the library is now done with
[CMake](https://cmake.org/documentation/)



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

- The Fortran function `longueur(str)` was deleted.  Fortran 90,
introduced the `len_trim(str)` built-in function that should be used
instead.  For C code handling Fortran strings, `ftnStrLen(str, maxLen)`
can be used.

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
        - SGLO
        - SGLOGG
        - VPOLAGR
        - SPS
        - VPS
        - grid_to_grid
        - grid_to_grid_interp
        - grid_to_grid_coef
        - FD1
        - D1INT2
        - INT1D1
        - D1
        - DEFVEC
        - FDM
        - SPD
        - D1INTR
        - D1INT1
        - FCONW
        - mxma
        - mxma8
        - ROSSR3
    - primitives
        - unstakw
        - stkmemw

## Finding deprecated functions in your code

The command below can help identify if your code uses deprecated
functions:

```
for fnctName in AFIX AFIX8 ALPAS2 ALPAS8 ALPDL2 ALPDL8 ALPDR2 ALPDR8 \
    ALPNM2 ALPNM8 AMAX AMEAN AMIN D1 D1INT1 D1INT2 D1INTR DEFVEC \
    DIMCAL EPSIL2 EPSIL8 EWFDR2 FASP2 FASP8 FCONW FD1 FDM GDADCN \
    GDADGD GDMPGD GDSQRT GGASP0 GGASP8 GWAQD0 GWAQD8 INT1D1 PERM \
    ROSSR3 SCOF2 SGLO SGLOGG SLL SPAF2 SPAF8 SPAGG0 SPAGG8 SPD SPS VLL \
    VPOLAGR VPS bmf_catalog bmf_char2i bmf_clear bmf_connect bmf_error \
    bmf_get bmf_get2 bmf_geth bmf_gobe bmf_i2char bmf_init bmf_list \
    bmf_perturb bmf_splitend bmf_splithalo bmf_splithole bmf_splitinit \
    bmf_splitname bmf_splitoptions bmf_splitstart bmf_splitwrall \
    bmf_splitwrite bmf_splitwrite2 bmf_splitwriteh bmf_splitwriteh2 \
    bmf_splitwritex bmf_splitwritex2 bmf_splitwritexy \
    bmf_splitwritexy2 bmf_splitwritey bmf_splitwritey2 bmf_statfld \
    bmf_write bmf_write2 ca_alloc ca_deallc close_db_file dmmsdbg \
    dmmsnabt grid_to_grid grid_to_grid_coef grid_to_grid_interp \
    hpalloc hpdeallc memoir memoirc memoirh mxma mxma8 open_db_file \
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
