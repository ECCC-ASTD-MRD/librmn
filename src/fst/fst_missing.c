/*
 * This is a set of routines used to manage the "missing data" feature of the standard file package.
 * These routines can also be used by other packages, provided that the datatype indicator
 * uses values that are the same as the standard file package, namely:
 *
 *   data type codes
 *   0: binary, transparent
 *   1: floating point
 *   2: unsigned integer
 *   3: character (R4A in an integer)
 *   4: signed integer
 *   5: IEEE floating point
 *   6: floating point
 *   7: character string
 *   8: complex IEEE
 * +128 = with second stage packer
 * +64  = missing data flagging is used
 *
 * to activate the package:
 * - set environment variable MISSING_VALUE_FLAGS
 *   (the order of the values is important)
 *   (real, integer, unsigned integer, 8 byte real, 2 byte integer, unsigned 2 byte integer)
 *   example:
 *   export MISSING_VALUE_FLAGS="1.234E37 123456 9999 4.567E+300 -5432 65432"
 *   NOTE: if less than 6 values are given, the missing ones are set to internal default values
 *            export MISSING_VALUE_FLAGS="   "
 *         would activate the package with the internal default values
 *
 * - call subroutine set_missing_value_flags
 *
 * There are C and FORTRAN callable routines/functions
 *
 * =================================================================================
 *   missing_value_used, ForceMissingValueUsage
 * =================================================================================
 *
 *   get the state of the package
 *   this integer function will return 0 if feature is not active, 1 if it is
 *
 * C interface:
 *   int missing_value_used()
 * FORTRAN interface:
 *   integer, external :: missing_value_used
 *
 *   forcibly set the state of the package
 *
 * C interface:
 *   oldmode = ForceMissingValueUsage(flag)
 *
 * FORTRAN interface:
 *   oldmode = Force_Missing_ValueUsage(flag)
 * =================================================================================
 *   get_missing_value_flags
 * =================================================================================
 *
 *   this integer function is used to get the special values used to flag "missing" data for
 *   real, integer, unsigned integer, 8 byte real, 2 byte integer, unsigned 2 byte integer
 *   this integer function will return 0 if feature is not active, 1 if it is
 *
 * C interface:
 *   int get_missing_value_flags(float *f, int *i, unsigned int *ui, double *d, short *s, unsigned short *us,
 *                               signed char *b, unsigned char *ub)
 * FORTRAN interface:
 *   integer function get_missing_value_flags(f, i, ui, d, s, us, b, ub)
 *   real, intent(OUT) :: f
 *   integer, intent(OUT) :: i, ui
 *   real *8, intent(OUT) :: d
 *   integer *2, intent(OUT) :: s, us
 *   integer *1, intent(OUT) :: b, ub
 *   NOTE: us, ui, us are unsigned numbers, which can be confusing in FORTRAN
 *
 * =================================================================================
 *   set_missing_value_flags
 * =================================================================================
 *
 *   this subroutine is used to set the special values used to flag "missing" data for
 *   real, integer, unsigned integer, 8 byte real, 2 byte integer, unsigned 2 byte integer
 *   calling this subroutine will also activate the package
 *
 * C interface:
 *   void set_missing_value_flags(float *f, int *i, unsigned int *ui, double *d, short *s, unsigned short *us,
 *                               signed char *b, unsigned char *ub)
 * FORTRAN interface:
 *   subroutine set_missing_value_flags(f, i, ui, d, s, us, b, ub)
 *   real, intent(IN) :: f
 *   integer, intent(IN) :: i, ui
 *   real *8, intent(IN) :: d
 *   integer *2, intent(IN) :: s, us
 *   integer *1, intent(IN) :: b, ub
 *   NOTE: us, ui, us are unsigned numbers, which can be confusing in FORTRAN
 *
 * =================================================================================
 * EncodeMissingValue / encode_missing_value
 * =================================================================================
 *
 * this function copies data from field2 into field replacing values that have the "missing data" value
 * with values that are non detrimental to further packing by the "standard file" packers. field 2 is left
 * as is, field will contain the output.
 * if proper encoding is not possible, the function returns zero immediately, leaving field untouched.
 *
 * the return value of the function is the number of missing values found ( 0 if unsuccessful for any reason)
 *
 * C interface:
 *   int EncodeMissingValue(void *field, void *field2, int nElems, int datatype, int datasize, int npak);
 * FORTRAN interface:
 *   integer function encode_missing_value(field, field2, nElems, datatype, datasize, npak)
 *   "any_type", intent(OUT) :: field
 *   "any_type", intent(IN) :: field2
 *   integer, intent(IN) :: nElems, datatype, datasize, npak
 *
 * field2   : input array (unspecified type)
 * field    : output array (assumed to have the same type as field2)
 * nElems  : number of elements in arrays field and field2
 * datatype : what kind of data is found in array (see above)
 * npak    : number of bits per element to be used for packing (used to compute appropriate flag values)
 * is_byte  : non zero if array element length is one byte
 * is_short : non zero if integer (signed or not) data element length is two bytes
 * is_double: non zero if real data element length is eight bytes (double / real *8)
 *
 * the missing value will be replaced by a value greater than the largest value in field2 and
 * large enough not to create an after packing collision with the value of the actual largest
 * useful value in field2 (npak is needed for this calculation)
 * =================================================================================
 * SetMissingValueMapping (set_missing_value_mapping from Fortran)
 * set the functions used for encoding missing values and/or decoding encoded fields
 *
 * C interface:
 *   SetMissingValueMapping(int what, int datatype, int datasize, void *processor)
 *
 * FORTRAN interface:
 *   call set_missing_value_mapping(what, datatype, processor, is_byte, is_short, is_double)
 *   integer, intent(IN) :: what, datatype, is_byte, is_short, is_double
 *
 * processor is the address of the processing function called as
 *   decode_function(void *data, int npoints)
 *   n_missing_points = encode_function(void *dest, void *src, int npoints)
 * NOTE:
 *   the npoints argument is passed BY VALUE (Fortran beware, iso_c_binding necessary)
 * =================================================================================
 * =================================================================================
 * DecodeMissingValue / decode_missing_value
 * =================================================================================
 *
 * this subroutine will replace all occurrences the largest value in array field by the "missing data"
 * value for this kind (datatype) of data. it assumes that EncodeMissingValue / encode_missing_value
 * has been called before to set the largest value in an adequate manner.
 *
 * C interface:
 *   void DecodeMissingValue(void *field, int nElems, int datatype, int datasize);
 *
 * FORTRAN interface:
 *   subroutine decode_missing_value(field, nElems, datatype, datasize)
 *   "any_type", intent(INOUT) :: field
 *   integer, intent(IN) :: nElems, datatype, datasize
 *
 * see EncodeMissingValue / encode_missing_value for the meaning of the arguments
 * =================================================================================
 *
 * NOTE: this set of routines does not implement full defensive coding. SAFE calls from the
 *       standard file package are expected. USE WITH CARE.
*/
#include <stdlib.h>
#include <stdio.h>

#include <App.h>
#include <rmn/DlInterface.h>
#include <rmn/fst_missing.h>
#include "qstdir.h"

//! -1 not initialized, 1 active, 0 not active
static int plugmode = -1;
//! Default values for missing values
//! Very large float
static float float_missing_val = -1.0E+38;
//! Very large double
static double double_missing_val = -1.0E+38;
//! Largest negative 32 bit integer
static int int_missing_val = 0x80000000;
//! Largest negative 16 bit integer
static short short_missing_val = 0x8000;
//! Largest negative  8 bit integer
static signed char byte_missing_val = 0x80;
//! Largest 32 bit unsigned integer
static unsigned int uint_missing_val = 0xFFFFFFFF;
//! Largest 16 bit unsigned integer
static unsigned short ushort_missing_val = 0xFFFF;
//! Largest  8 bit unsigned integer
static unsigned char ubyte_missing_val = 0xFF;

static void (*set_plugin_missing_value_flags)(
    const float * const, const int * const, const unsigned int * const, const double * const,
    const short * const, const unsigned short * const, const signed char * const, const unsigned char * const
) = NULL;

// Fortran and C callable versions get get "magic values" used flag
#pragma weak missing_value_used_ = missing_value_used
#pragma weak missing_value_used__ = missing_value_used
int missing_value_used_(void);
int missing_value_used__(void);


//! Define missing values from FST_MISSING_VALUE and MISSING_VALUE_PLUGINS environment variables.
//! This function is not thread safe when the module has not been initialized, so its first call
//! should be protected by a mutex when using multiple threads.
//! \return 1 if MISSING_VALUE_FLAGS is defined, 0 otherwise
//!
//! If the MISSING_VALUE_PLUGINS environement variable is defined, it's
//! value is interpreted as the path to a shared library which must
//! specific functions, listed below, to encode and decode missing
//! values.
//!
//! The followin functions must be available in the shared library:
//! - float_encode
//! - double_encode
//! - int_encode
//! - short_encode
//! - byte_encode
//! - uint_encode
//! - ushort_encode
//! - ubyte_encode
//! - float_decode
//! - double_decode
//! - int_decode
//! - short_decode
//! - byte_decode
//! - uint_decode
//! - ushort_decode
//! - ubyte_decode
//! - set_plugin_missing_value_flags
int missing_value_used(void) {
    if (plugmode == -1) {
        const char * text = getenv("MISSING_VALUE_FLAGS");
        if (text == NULL) {
            plugmode = 0;
        } else {
            plugmode = 1;
            sscanf(text, "%g %d %u %lg %hd %hu %hhd %hhu",
                &float_missing_val,
                &int_missing_val,
                &uint_missing_val,
                &double_missing_val,
                &short_missing_val,
                &ushort_missing_val,
                &byte_missing_val,
                &ubyte_missing_val
            );
        }
        text = getenv("MISSING_VALUE_PLUGINS");
        if (text != NULL) {
            Lib_Log(APP_LIBFST,APP_INFO,"%s: opening plugin library '%s\n",__func__,text);
            void * handle = DlOpen(text, RTLD_NOW);
            if (handle != NULL) {
                SetMissingValueMapping(1, 1, 32, DlSym(handle, "float_decode"));
                SetMissingValueMapping(1, 1, 64, DlSym(handle, "double_decode"));
                SetMissingValueMapping(1, 2, 32, DlSym(handle, "uint_decode"));
                SetMissingValueMapping(1, 2, 8, DlSym(handle, "ubyte_decode"));
                SetMissingValueMapping(1, 2, 16, DlSym(handle, "ushort_decode"));
                SetMissingValueMapping(1, 4, 32, DlSym(handle, "int_decode"));
                SetMissingValueMapping(1, 4, 8, DlSym(handle, "byte_decode"));
                SetMissingValueMapping(1, 4, 16, DlSym(handle, "short_decode"));

                SetMissingValueMapping(2, 1, 32, DlSym(handle, "float_encode"));
                SetMissingValueMapping(2, 1, 64, DlSym(handle, "double_encode"));
                SetMissingValueMapping(2, 2, 32, DlSym(handle, "uint_encode"));
                SetMissingValueMapping(2, 2, 8, DlSym(handle, "ubyte_encode"));
                SetMissingValueMapping(2, 2, 16, DlSym(handle, "ushort_encode"));
                SetMissingValueMapping(2, 4, 32, DlSym(handle, "int_encode"));
                SetMissingValueMapping(2, 4, 8, DlSym(handle, "byte_encode"));
                SetMissingValueMapping(2, 4, 16, DlSym(handle, "short_encode"));
                set_plugin_missing_value_flags = (void(*)()) DlSym(handle, "set_plugin_missing_value_flags");
            } else {
                Lib_Log(APP_LIBFST,APP_WARNING,"%s: plugin library '%s' not found\n",__func__,text);
            }
        }
        if (set_plugin_missing_value_flags != NULL) {
            (*set_plugin_missing_value_flags)(
                &float_missing_val,
                &int_missing_val,
                &uint_missing_val,
                &double_missing_val,
                &short_missing_val,
                &ushort_missing_val,
                &byte_missing_val,
                &ubyte_missing_val
            );
        }
    }
    return plugmode;
}


//! Forcibly set(activate) or reset(deactivate) the missing value mode
//! C entry point
int ForceMissingValueUsage(
    //! [int] Set to 1 to enable, 0 to disable
    const int enable
) {
    return plugmode = (enable != 0) ? 1 : 0;
}


#pragma weak force_missing_value_used_ = force_missing_value_used
int force_missing_value_used_(const int * const enable);
#pragma weak force_missing_value_used__ = force_missing_value_used
int force_missing_value_used__(const int * const enable);

//! Forcibly enable or disable the "magic value" mode
//! \return 1 if enabled, 0 toherwise
int force_missing_value_used(
    //! [in] 1 to enable, 0 to disable
    const int * const enable
) {
    return ForceMissingValueUsage(*enable);
}


#pragma weak get_missing_value_flags_ = get_missing_value_flags
int get_missing_value_flags_(
    float * const missingFloatVal,
    int * const missingIntVal,
    unsigned int * const missingUIntVal,
    double * const missingDoubleVal,
    short * const missingShortVal,
    unsigned short * const missingUShortVal,
    signed char * const missingByteVal,
    unsigned char * const missingUByteVal
);
#pragma weak get_missing_value_flags__ = get_missing_value_flags
int get_missing_value_flags__(
    float * const missingFloatVal,
    int * const missingIntVal,
    unsigned int * const missingUIntVal,
    double * const missingDoubleVal,
    short * const missingShortVal,
    unsigned short * const missingUShortVal,
    signed char * const missingByteVal,
    unsigned char * const missingUByteVal
);

//! Get magic values used to flag missing data points from environment variable FST_MISSING_VALUE
//! Fortran and C callable
//! \return 1 if the "magic value" mode is active, 0 otherwise
int get_missing_value_flags(
    float * const missingFloatVal,
    int * const missingIntVal,
    unsigned int * const missingUIntVal,
    double * const missingDoubleVal,
    short * const missingShortVal,
    unsigned short * const missingUShortVal,
    signed char * const missingByteVal,
    unsigned char * const missingUByteVal
) {
    *missingFloatVal = float_missing_val;
    *missingIntVal = int_missing_val;
    *missingUIntVal = uint_missing_val;
    *missingDoubleVal = double_missing_val;
    *missingShortVal = short_missing_val;
    *missingUShortVal = ushort_missing_val;
    *missingByteVal = byte_missing_val;
    *missingUByteVal = ubyte_missing_val;
    return missing_value_used();
}


#pragma weak set_missing_value_flags_ = set_missing_value_flags
void set_missing_value_flags_(
    const float * const missingFloatVal,
    const int * const missingIntVal,
    const unsigned int * const missingUIntVal,
    const double * const missingDoubleVal,
    const short * const missingShortVal,
    const unsigned short * const missingUShortVal,
    const signed char * const missingByteVal,
    const unsigned char * const missingUByteVal
);
#pragma weak set_missing_value_flags__ = set_missing_value_flags
void set_missing_value_flags__(
    const float * const missingFloatVal,
    const int * const missingIntVal,
    const unsigned int * const missingUIntVal,
    const double * const missingDoubleVal,
    const short * const missingShortVal,
    const unsigned short * const missingUShortVal,
    const signed char * const missingByteVal,
    const unsigned char * const missingUByteVal
);

//! Set magic values used to flag missing data points
//! Fortran and C callable
void set_missing_value_flags(
    const float * const missingFloatVal,
    const int * const missingIntVal,
    const unsigned int * const missingUIntVal,
    const double * const missingDoubleVal,
    const short * const missingShortVal,
    const unsigned short * const missingUShortVal,
    const signed char * const missingByteVal,
    const unsigned char * const missingUByteVal
) {
    float_missing_val  = *missingFloatVal;
    int_missing_val    = *missingIntVal;
    uint_missing_val   = *missingUIntVal;
    double_missing_val = *missingDoubleVal;
    short_missing_val  = *missingShortVal;
    ushort_missing_val = *missingUShortVal;
    byte_missing_val   = *missingByteVal;
    ubyte_missing_val  = *missingUByteVal;
    /* values have been set, activate plug mode */
    plugmode = 1;
    if (set_plugin_missing_value_flags != NULL ) {
        (*set_plugin_missing_value_flags)(
            missingFloatVal,
            missingIntVal,
            missingUIntVal,
            missingDoubleVal,
            missingShortVal,
            missingUShortVal,
            missingByteVal,
            missingUByteVal);
    }
}

#ifdef USE_MACROS
/*
 * functions to find minimum and maximum values of a field while ignoring the missing value code
 * these functions return the number of missing values detected
*/

#define FLD_TYPE_ANAL(name, kind, flag)                        \
static int name(kind *z, int n , kind *zmax, kind *zmin)  {    \
  kind zma, zmi;                                               \
  int i, missing;                                              \
  i = 0; missing = 0;                                          \
  while( i<n && z[i] == flag ) {i++; missing++; }              \
  zma = zmi = z[i];                                            \
  while(++i < n) {                                             \
    if ( z[i] == flag ) {                                      \
      missing++;                                               \
    } else {                                                   \
      zmi = z[i] < zmi ? z[i] : zmi;                           \
      zma = z[i] > zma ? z[i] : zma;                           \
    }                                                          \
  }                                                            \
  *zmax = zma; *zmin = zmi;                                    \
  return missing;                                              \
}

FLD_TYPE_ANAL(fld_float_anal,  float,          float_missing_val)
FLD_TYPE_ANAL(fld_double_anal, double,         double_missing_val)
FLD_TYPE_ANAL(fld_int_anal,    int,            int_missing_val)
FLD_TYPE_ANAL(fld_uint_anal,   unsigned int,   uint_missing_val)
FLD_TYPE_ANAL(fld_short_anal,  short,          short_missing_val)
FLD_TYPE_ANAL(fld_ushort_anal, unsigned short, ushort_missing_val)
FLD_TYPE_ANAL(fld_byte_anal,   char,           byte_missing_val)
FLD_TYPE_ANAL(fld_ubyte_anal,  unsigned char,  ubyte_missing_val)

/*
 * routines to replace maximum value of field with missing value code
 */

#define FLD_TYPE_DECODE(name, kind, kind_anal, flag)            \
static void name(kind * const z, const int n) {                 \
  kind zma, zmi;                                                \
  int notused;                                                  \
  if (missing_value_used() == 0) return;                        \
  notused = kind_anal(z, n, &zma, &zmi);                        \
  while(n--) { if (*z == zma) *z=flag; z++; }                   \
  return;                                                       \
}

FLD_TYPE_DECODE(fst_double_decode_missing, double,         fld_double_anal, double_missing_val)
FLD_TYPE_DECODE(fst_float_decode_missing,  float,          fld_float_anal,  float_missing_val)
FLD_TYPE_DECODE(fst_int_decode_missing,    int,            fld_int_anal,    int_missing_val)
FLD_TYPE_DECODE(fst_uint_decode_missing,   unsigned int,   fld_uint_anal,   uint_missing_val)
FLD_TYPE_DECODE(fst_short_decode_missing,  short,          fld_short_anal,  short_missing_val)
FLD_TYPE_DECODE(fst_ushort_decode_missing, unsigned short, fld_ushort_anal, ushort_missing_val)
FLD_TYPE_DECODE(fst_byte_decode_missing,   char,           fld_byte_anal,   byte_missing_val)
FLD_TYPE_DECODE(fst_ubyte_decode_missing,  unsigned char,  fld_ubyte_anal,  ubyte_missing_val)

#else

//! Find minimum and maximum of field/array
//! \return Number of "missing" values
static int fld_float_anal(
    //! [in] Field/array to analyze
    const float * const field,
    //! [in] Number of elements in field/array
    const int nElems,
    //! [out] Maximum value in field/array
    float * const fieldMax,
    //! [out] Minimum value in field/array
    float * const fieldMin
) {
    int missing = 0;
    if (field[0] == float_missing_val) missing++;
    float max = field[0];
    float min = max;

    for (int i = 1; i < nElems; i++)  {
        if (max == float_missing_val) max = field[i];
        if (min == float_missing_val) min = field[i];
        if (field[i] == float_missing_val) {
            missing++;
        } else {
            if (field[i] < min) min = field[i];
            if (field[i] > max) max = field[i];
        }
    }

    *fieldMax = max;
    *fieldMin = min;
    return missing;
}


//! \copydoc fld_float_anal
static int fld_double_anal(
    const double * const field,
    const int nElems,
    double * const fieldMax,
    double * const fieldMin
) {
    int missing = 0;
    if (field[0] == double_missing_val) missing++;
    double max = field[0];
    double min = max;

    for (int i = 1; i < nElems; i++)  {
        if (max == double_missing_val) max = field[i];
        if (min == double_missing_val) min = field[i];
        if (field[i] == double_missing_val) {
            missing++;
        } else {
            if (field[i] < min) min = field[i];
            if (field[i] > max) max = field[i];
        }
    }

    *fieldMax = max;
    *fieldMin = min;
    return missing;
}


//! \copydoc fld_float_anal
static int fld_int_anal(
    const int * const field,
    const int nElems,
    int * const fieldMax,
    int * const fieldMin
) {
    int missing = 0;
    if (field[0] == int_missing_val) missing++;
    int max = field[0];
    int min = max;

    for (int i = 1; i < nElems; i++)  {
        if (max == int_missing_val) max = field[i];
        if (min == int_missing_val) min = field[i];
        if (field[i] == int_missing_val) {
            missing++;
        } else {
            if (field[i] < min) min = field[i];
            if (field[i] > max) max = field[i];
        }
    }

    *fieldMax = max;
    *fieldMin = min;
    return missing;
}


//! \copydoc fld_float_anal
static int fld_short_anal(
    const short * const field,
    const int nElems,
    short * const fieldMax,
    short * const fieldMin
) {
    int missing = 0;
    if (field[0] == short_missing_val) missing++;
    short max = field[0];
    short min = max;

    for (int i = 1; i < nElems; i++)  {
        if (max == short_missing_val) max = field[i];
        if (min == short_missing_val) min = field[i];
        if (field[i] == short_missing_val) {
            missing++;
        } else {
            if (field[i] < min) min = field[i];
            if (field[i] > max) max = field[i];
        }
    }

    *fieldMax = max;
    *fieldMin = min;
    return missing;
}


//! \copydoc fld_float_anal
static int fld_byte_anal(
    const char * const field,
    const int nElems,
    char * const fieldMax,
    char * const fieldMin
) {
    int missing = 0;
    if (field[0] == byte_missing_val) missing++;
    char max = field[0];
    char min = max;

    for (int i = 1; i < nElems; i++)  {
        if (max == byte_missing_val) max = field[i];
        if (min == byte_missing_val) min = field[i];
        if (field[i] == byte_missing_val) {
            missing++;
        } else {
            if (field[i] < min) min = field[i];
            if (field[i] > max) max = field[i];
        }
    }

    *fieldMax = max;
    *fieldMin = min;
    return missing;
}


//! \copydoc fld_float_anal
static int fld_uint_anal(
    const unsigned int * const field,
    const int nElems,
    unsigned int * const fieldMax,
    unsigned int * const fieldMin
) {
    int missing = 0;
    if (field[0] == uint_missing_val) missing++;
    unsigned int max = field[0];
    unsigned int min = max;

    for (int i = 1; i < nElems; i++)  {
        if (max == uint_missing_val) max = field[i];
        if (min == uint_missing_val) min = field[i];
        if (field[i] == uint_missing_val) {
            missing++;
        } else {
            if (field[i] < min) min = field[i];
            if (field[i] > max) max = field[i];
        }
    }

    *fieldMax = max;
    *fieldMin = min;
    return missing;
}


//! \copydoc fld_float_anal
static int fld_ushort_anal(
    const unsigned short * const field,
    const int nElems,
    unsigned short * const fieldMax,
    unsigned short * const fieldMin
) {
    int missing = 0;
    if (field[0] == ushort_missing_val) missing++;
    unsigned short max = field[0];
    unsigned short min = max;

    for (int i = 1; i < nElems; i++)  {
        if (max == ushort_missing_val) max = field[i];
        if (min == ushort_missing_val) min = field[i];
        if (field[i] == ushort_missing_val) {
            missing++;
        } else {
            if (field[i] < min) min = field[i];
            if (field[i] > max) max = field[i];
        }
    }

    *fieldMax = max;
    *fieldMin = min;
    return missing;
}


//! \copydoc fld_float_anal
static int fld_ubyte_anal(
    const unsigned char * const field,
    const int nElems,
    unsigned char * const fieldMax,
    unsigned char * const fieldMin
) {
    int missing = 0;
    if (field[0] == ubyte_missing_val) missing++;
    unsigned char max = field[0];
    unsigned char min = max;

    for (int i = 1; i < nElems; i++) {
        if (max == ubyte_missing_val) max = field[i];
        if (min == ubyte_missing_val) min = field[i];
        if (field[i] == ubyte_missing_val) {
            missing++;
        } else {
            if (field[i] < min) min = field[i];
            if (field[i] > max) max = field[i];
        }
    }

    *fieldMax = max;
    *fieldMin = min;
    return missing;
}


//! Replace maximum value of field with missing value code
static void fst_float_decode_missing(
    //! [in,out] Field/array in which to substitute the "missing" values
    float * const field,
    //! [in] Number of elements in the field/array
    const int nElems
) {
    if (missing_value_used() == 0) return;

    float max, min;
    fld_float_anal(field, nElems, &max, &min);

    for (int i = 0; i < nElems; i++)  {
        if (field[i] == max) {
            field[i] = float_missing_val;
        }
    }
}


//! \copydoc fst_float_decode_missing
static void fst_double_decode_missing(
    double * const field,
    const int nElems
) {
    if (missing_value_used() == 0) return;

    double max, min;
    fld_double_anal(field, nElems, &max, &min);

    for (int i = 0; i < nElems; i++)  {
        if (field[i] == max) {
            field[i] = double_missing_val;
        }
    }
}


//! \copydoc fst_float_decode_missing
static void fst_int_decode_missing(
    int * const field,
    const int nElems
) {
    if (missing_value_used() == 0) return;

    int max, min;
    fld_int_anal(field, nElems, &max, &min);

    for (int i = 0; i < nElems; i++)  {
        if (field[i] == max) {
            field[i] = int_missing_val;
        }
    }
}


//! \copydoc fst_float_decode_missing
static void fst_short_decode_missing(
    short * const field,
    const int nElems
) {
    if (missing_value_used() == 0) return;

    short max, min;
    fld_short_anal(field, nElems, &max, &min);

    for (int i = 0; i < nElems; i++)  {
        if (field[i] == max) {
            field[i] = short_missing_val;
        }
    }
}


//! \copydoc fst_float_decode_missing
static void fst_byte_decode_missing(
    char * const field,
    const int nElems
) {
    if (missing_value_used() == 0) return;

    char max, min;
    fld_byte_anal(field, nElems, &max, &min);

    for (int i = 0; i < nElems; i++)  {
        if (field[i] == max) {
            field[i] = byte_missing_val;
        }
    }
}


//! \copydoc fst_float_decode_missing
static void fst_uint_decode_missing(
    unsigned int * const field,
    const int nElems
) {
    if (missing_value_used() == 0) return;

    unsigned int max, min;
    fld_uint_anal(field, nElems, &max, &min);

    for (int i = 0; i < nElems; i++)  {
        if (field[i] == max) {
            field[i] = uint_missing_val;
        }
    }
}


//! \copydoc fst_float_decode_missing
static void fst_ushort_decode_missing(
    unsigned short * const field,
    const int nElems
) {
    if (missing_value_used() == 0) return;

    unsigned short max, min;
    fld_ushort_anal(field, nElems, &max, &min);

    for (int i = 0; i < nElems; i++)  {
        if (field[i] == max) {
            field[i] = ushort_missing_val;
        }
    }
}


//! \copydoc fst_float_decode_missing
static void fst_ubyte_decode_missing(
    unsigned char * const field,
    const int nElems
) {
    if (missing_value_used() == 0) return;

    unsigned char max, min;
    fld_ubyte_anal(field, nElems, &max, &min);

    for (int i = 0; i < nElems; i++)  {
        if (field[i] == max) {
            field[i] = ubyte_missing_val;
        }
    }
}
#endif


//! Replace missing value code with value suitable for packers
static int fst_double_encode_missing(
    //! [out] Output field/array
    double * const dst,
    //! [out] Input field/array
    const double * const src,
    //! [in] Number of elements in field/array
    const int nElems,
    //! [in] Number of bits per element to be used for packing
    const int npak
) {
    if (missing_value_used() == 0) return 0;

    const float factor[9] = { 2.0, 1.1, 0.55, 0.28, 0.14, 0.07, 0.035, 0.017, 0.01 };

    double max, min;
    int missing = fld_double_anal(src, nElems, &max, &min);
    if (missing == 0) return 0;

    double plug;
    if (npak > 8) {
        // >8 bits, add 1% of max-min to max
        plug = max + (max - min) * 0.01;
    } else {
        // <=8 bits, add appropriate fraction of max-min to max
        plug = max + (max - min) * factor[npak];
    }

    if (plug == max) {
        // field is constant, max == min
        plug = (max == 0.0) ? 1.0 : max * 2.0;
    }
    for (int i = 0; i < nElems; i++) {
        if (src[i] == double_missing_val) {
            dst[i] = plug; 
        } else {
            dst[i] = src[i];
        }
    }
    return missing;
}


//! \copydoc fst_double_encode_missing
static int fst_float_encode_missing(
    float * const dst,
    const float * const src,
    const int nElems,
    const int npak
) {
    if (missing_value_used() == 0) return 0;

    const float factor[9] = { 2.0, 1.1, 0.55, 0.28, 0.14, 0.07, 0.035, 0.017, 0.01 };

    float max, min;
    int missing = fld_float_anal(src, nElems, &max, &min);
    if (missing == 0) return 0;

    float plug;
    if (npak > 8) {
        // >8 bits, add 1% of max-min to max
        plug = max + (max - min) * 0.01;
    } else {
        // <=8 bits, add appropriate fraction of max-min to max
        plug = max + (max - min) * factor[npak];
    }

    if (plug == max) {
        // field is constant, max == min
        plug = (max == 0.0) ? 1.0 : max * 2.0;
    }
    for (int i = 0; i < nElems; i++) {
        if (src[i] == float_missing_val) {
            dst[i] = plug; 
        } else {
            dst[i] = src[i];
        }
    }
    return missing;
}


//! \copydoc fst_double_encode_missing
static int fst_int_encode_missing(
    int * const dst,
    const int * const src,
    const int nElems,
    const int npak
) {
    if (missing_value_used() == 0) return 0;

    int max, min;
    int missing = fld_int_anal(src, nElems, &max, &min);
    if (missing == 0) return 0;

    unsigned int plug2 = 0xFFFFFFFF;
    if (npak < 32) {
        plug2 = plug2 >> (32 - npak);
    }
    // largest positive number using npak bits
    plug2 = plug2 >> 1;

    // this code assumes packing of z with no bias removal
    int plug = plug2;
    if (plug > max) {
        plug = max + 1;
    } else {
        Lib_Log(APP_LIBFST,APP_WARNING,"%s: Maximum value >= encoded missing value flag\n",__func__);
    }
    for (int i = 0; i < nElems; i++) {
        if (src[i] == int_missing_val) {
            dst[i] = plug;
        } else {
            dst[i] = src[i];
        }
    }
    return missing;
}


//! \copydoc fst_double_encode_missing
static int fst_short_encode_missing(
    short * const dst,
    const short * const src,
    const int nElems,
    const int npak
) {
    if (missing_value_used() == 0) return 0;

    short max, min;
    int missing = fld_short_anal(src, nElems, &max, &min);
    if (missing == 0) return 0;

    unsigned short plug2 = 0xFFFF;
    if (npak < 16) {
        plug2 = plug2 >> (16 - npak);
    }
    // largest positive number using npak bits
    plug2 = plug2 >> 1;

    // this code assumes packing of z with no bias removal
    short plug = plug2;
    if (plug > max) {
        plug = max + 1;
    } else {
        Lib_Log(APP_LIBFST,APP_WARNING,"%s: Maximum value >= encoded missing value flag\n",__func__);
    }
    for (int i = 0; i < nElems; i++) {
        if (src[i] == short_missing_val) {
            dst[i] = plug;
        } else {
            dst[i] = src[i];
        }
    }
    return missing;
}


//! \copydoc fst_double_encode_missing
static int fst_byte_encode_missing(
    char * const dst,
    const char * const src,
    const int nElems,
    const int npak
) {
    if (missing_value_used() == 0) return 0;

    char max, min;
    int missing = fld_byte_anal(src, nElems, &max, &min);
    if (missing == 0) return 0;

    unsigned char plug2 = 0xFF;
    if (npak < 8) {
        plug2 = plug2 >> (8 - npak);
    }
    // largest positive number using npak bits
    plug2 = plug2 >> 1;

    // this code assumes packing of z with no bias removal
    char plug = plug2;
    if (plug > max) {
        plug = max + 1;
    } else {
        Lib_Log(APP_LIBFST,APP_WARNING,"%s: Maximum value >= encoded missing value flag\n",__func__);
    }
    for (int i = 0; i < nElems; i++) {
        if (src[i] == byte_missing_val) {
            dst[i] = plug;
        } else {
            dst[i] = src[i];
        }
    }
    return missing;
}


//! \copydoc fst_double_encode_missing
static int fst_uint_encode_missing(
    unsigned int * const dst,
    const unsigned int * const src,
    const int nElems,
    const int npak
) {
    if (missing_value_used() == 0) return 0;

    unsigned int max, min;
    unsigned int missing = fld_uint_anal(src, nElems, &max, &min);
    if (missing == 0) return 0;

    unsigned int plug = 0xFFFFFFFF;
    if (npak < 32) {
        plug = plug >> (32 - npak);
    }

    // this code assumes packing of z with no bias removal
    if (plug > max) {
        plug = max + 1;
    } else {
        Lib_Log(APP_LIBFST,APP_WARNING,"%s: Maximum value %u >= encoded missing value flag %u\n",__func__,max,plug);
    }
    for (int i = 0; i < nElems; i++) {
        if (src[i] == uint_missing_val) {
            dst[i] = plug;
        } else {
            dst[i] = src[i];
        }
    }
    return missing;
}


//! \copydoc fst_double_encode_missing
static int fst_ushort_encode_missing(
    unsigned short * const dst,
    const unsigned short * const src,
    const int nElems,
    const int npak
) {
    if (missing_value_used() == 0) return 0;

    unsigned short max, min;
    int missing = fld_ushort_anal(src, nElems, &max, &min);
    if (missing == 0) return 0;

    unsigned short plug = 0xFFFF;
    if (npak < 16) {
        plug = plug >> (16 - npak);
    }

    // this code assumes packing of z with no bias removal
    if (plug > max) {
        plug = max + 1;
    } else {
        Lib_Log(APP_LIBFST,APP_WARNING,"%s: Maximum value %hu >= encoded missing value flag %hu\n",__func__,max,plug);
    }
    for (int i = 0; i < nElems; i++) {
        if (src[i] == ushort_missing_val) {
            dst[i] = plug;
        } else {
            dst[i] = src[i];
        }
    }
    return missing;
}


//! \copydoc fst_double_encode_missing
static int fst_ubyte_encode_missing(
    unsigned char * const dst,
    const unsigned char * const src,
    const int nElems,
    const int npak
) {
    if (missing_value_used() == 0) return 0;

    unsigned char max, min;
    int missing = fld_ubyte_anal(src, nElems, &max, &min);
    if (missing == 0) return 0;

    unsigned char plug = 0xFF;
    if (npak < 8) {
        plug = plug >> (8 - npak);
    }

    // this code assumes packing of z with no bias removal
    if (plug > max) {
        plug = max + 1;
    } else {
        Lib_Log(APP_LIBFST,APP_WARNING,"%s: Maximum value %hu >= encoded missing value flag %hu\n",__func__,max,plug);
    }
    for (int i = 0; i < nElems; i++) {
        if (src[i] == ubyte_missing_val) {
            dst[i] = plug;
        } else {
            dst[i] = src[i];
        }
    }
    return missing;
}


//! Null decode; does nothing
static void fst_null_decode_missing(void *field, int nElems) {
    return;
}

static int (*__fst_float_encode_missing)(float* const, const float* const, const int, const int) = fst_float_encode_missing;
static int (*__fst_double_encode_missing)(double* const, const double* const, const int, const int) = fst_double_encode_missing;
static int (*__fst_int_encode_missing)(int* const, const int* const, const int, const int) = fst_int_encode_missing;
static int (*__fst_short_encode_missing)(short* const, const short* const, const int, const int) = fst_short_encode_missing;
static int (*__fst_byte_encode_missing)(char* const, const char* const, const int, const int) = fst_byte_encode_missing;
static int (*__fst_uint_encode_missing)(unsigned int* const, const unsigned int* const, const int, const int) = fst_uint_encode_missing;
static int (*__fst_ushort_encode_missing)(unsigned short* const, const unsigned short* const, const int, const int) = fst_ushort_encode_missing;
static int (*__fst_ubyte_encode_missing)(unsigned char* const, const unsigned char* const, const int, const int) = fst_ubyte_encode_missing;

static void (*__fst_float_decode_missing)(float* const, const int) = fst_float_decode_missing;
static void (*__fst_double_decode_missing)(double* const, const int) = fst_double_decode_missing;
static void (*__fst_int_decode_missing)(int* const, const int) = fst_int_decode_missing;
static void (*__fst_short_decode_missing)(short* const, const int) = fst_short_decode_missing;
static void (*__fst_byte_decode_missing)(char* const, const int) = fst_byte_decode_missing;
static void (*__fst_uint_decode_missing)(unsigned int* const, const int) = fst_uint_decode_missing;
static void (*__fst_ushort_decode_missing)(unsigned short* const, const int) = fst_ushort_decode_missing;
static void (*__fst_ubyte_decode_missing)(unsigned char* const, const int) = fst_ubyte_decode_missing;


//! Restore default mapping functions
void RestoreMissingValueMapping(void) {
    __fst_float_encode_missing = fst_float_encode_missing;
    __fst_double_encode_missing = fst_double_encode_missing;
    __fst_int_encode_missing = fst_int_encode_missing;
    __fst_short_encode_missing = fst_short_encode_missing;
    __fst_byte_encode_missing = fst_byte_encode_missing;
    __fst_uint_encode_missing = fst_uint_encode_missing;
    __fst_ushort_encode_missing = fst_ushort_encode_missing;
    __fst_ubyte_encode_missing = fst_ubyte_encode_missing;

    __fst_float_decode_missing = fst_float_decode_missing;
    __fst_double_decode_missing = fst_double_decode_missing;
    __fst_int_decode_missing = fst_int_decode_missing;
    __fst_short_decode_missing = fst_short_decode_missing;
    __fst_byte_decode_missing = fst_byte_decode_missing;
    __fst_uint_decode_missing = fst_uint_decode_missing;
    __fst_ushort_decode_missing = fst_ushort_decode_missing;
    __fst_ubyte_decode_missing = fst_ubyte_decode_missing;
}

/* Fortran entry points (calling C entry point RestoreMissingValueMapping) */
void restore_missing_value_mapping(void) { RestoreMissingValueMapping(); }
void restore_missing_value_mapping_(void) { RestoreMissingValueMapping(); }
void restore_missing_value_mapping__(void) { RestoreMissingValueMapping(); }


//! Change the missing value mapping function (C entry point)
void SetMissingValueMapping(
    //! [in] Mode
    //! | Value | Description                                                                                            |
    //! | ----: | :----------------------------------------------------------------------------------------------------- |
    //! |   < 0 | Set function to processor                                                                              |
    //! |   > 0 | Set function to original value, ignore processor                                                       |
    //! |  ±  1 | Decoding functions                                                                                     |
    //! |  ±  2 | Encoding functions                                                                                     |
    //! |  ± 11 | Selectively deactivate the selected decoding function (uses fst_null_decode_missing), ignore processor |
    const int mode,
    //! [in] Data type
    //! | Value | Description                   |
    //! |  ---: | :---------------------------- |
    //! |     0 | Binary, transparent           |
    //! |     1 | Floating point                |
    //! |     2 | Unsigned integer              |
    //! |     3 | Character (R4A in an integer) |
    //! |     4 | Signed integer                |
    //! |     5 | IEEE floating point           |
    //! |     6 | Floating point                |
    //! |     7 | Character string              |
    //! |     8 | Complex IEEE                  |
    //! |  +128 | With second stage packer      |
    //! |   +64 | Missing data flagging is used |
    const int datatype,
    //! [in] Array element length
    const int datasize,
    //! [in] Address of the processing function called as decode_function(void *data, int npoints)
    const void * const processor_
) {
    const void * processor = processor_;
    if (mode > 0 && processor == NULL) {
        // null pointer to function!
        return;
    }
    int lmode = (mode > 0) ? mode : -mode;
    if (lmode == 11) {
        // deactivate decoder for specified type
        lmode = 1;
        processor = (void *)fst_null_decode_missing;
    }
    if (lmode == 1) {
        // Replace decoding routine
        if (datatype == 1 || datatype == 5 || datatype == 6) {
            // float or IEEE
            if (datasize==64) {
                __fst_double_decode_missing = (mode > 0) ? (void(*)(double* const, const int)) processor : fst_double_decode_missing;
            } else {
                __fst_float_decode_missing = (mode > 0) ? (void(*)(float* const, const int)) processor : fst_float_decode_missing;
            }
        }
      if (datatype == 4) {
          // Signed
            if (datasize==16) {
                __fst_short_decode_missing = (mode > 0) ? (void(*)()) processor : fst_short_decode_missing;
            } else if (datasize==8) {
                __fst_byte_decode_missing = (mode > 0) ? (void(*)()) processor : fst_byte_decode_missing;
        } else {
            __fst_int_decode_missing = (mode > 0) ? (void(*)()) processor : fst_int_decode_missing;
        }
      }
      if (datatype == 2) {
          // Unsigned
            if (datasize==16) {
                __fst_ushort_decode_missing = (mode > 0) ? (void(*)()) processor : fst_ushort_decode_missing;
            } else if (datasize==8) {
                __fst_ubyte_decode_missing = (mode > 0) ? (void(*)()) processor : fst_ubyte_decode_missing;
            } else {
                __fst_uint_decode_missing = (mode > 0) ? (void(*)()) processor : fst_uint_decode_missing;
            }
        }
    }
    if (lmode == 2) {
        // Replace an encoding routine
        if (datatype == 1 || datatype == 5 || datatype == 6) {
            // float or IEEE
            if (datasize==64) {
                __fst_double_encode_missing = (mode > 0) ? (int(*)()) processor : fst_double_encode_missing;
            } else {
                __fst_float_encode_missing = (mode > 0) ? (int(*)()) processor : fst_float_encode_missing;
            }
        }
        if (datatype == 4) {
            // Signed
            if (datasize==16) {
                __fst_short_encode_missing = (mode > 0) ? (int(*)()) processor : fst_short_encode_missing;
            } else if (datasize==8) {
                __fst_byte_encode_missing = (mode > 0) ? (int(*)()) processor : fst_byte_encode_missing;
            } else {
                __fst_int_encode_missing = (mode > 0) ? (int(*)()) processor : fst_int_encode_missing;
            }
        }
        if (datatype == 2) {
            // Unsigned
            if (datasize==16) {
                __fst_ushort_encode_missing = (mode > 0) ? (int(*)()) processor : fst_ushort_encode_missing;
            } else if (datasize==8) {
                __fst_ubyte_encode_missing = (mode > 0) ? (int(*)()) processor : fst_ubyte_encode_missing;
            } else {
                __fst_uint_encode_missing = (mode > 0) ? (int(*)()) processor : fst_uint_encode_missing;
            }
        }
    }
}

/* Fortran entry points (calling C entry point SetMissingValueMapping) */
#pragma weak set_missing_value_mapping_ = set_missing_value_mapping
void set_missing_value_mapping_(
    const int * const mode,
    const int * const datatype,
    const void * const processor_,
    const int * const datasize
);
#pragma weak set_missing_value_mapping__ = set_missing_value_mapping
void set_missing_value_mapping__(
    const int * const mode,
    const int * const datatype,
    const void * const processor_,
    const int * const datasize
);
void set_missing_value_mapping(
    const int * const mode,
    const int * const datatype,
    const void * const processor_,
    const int * const datasize
) {
    SetMissingValueMapping(*mode, *datatype, *datasize, processor_);
}

//! Encode missing values in a field
//! C entry point
//!
//! Copy data from src into dst replacing values that have the
//! "missing data" value with values that are non detrimental to further
//! packing by the "standard file" packers.
//!
//! If proper encoding is not possible, the function returns zero
//! immediately, leaving dst untouched.
//!
//! \return Number of missing values found

int EncodeMissingValue(
    //! [out] Output field
    void * const dst,
    //! [in] Source field
    const void * const src,
    //! [in] Number of elements in src
    const int nElems,
    //! [in] Data type
    const int datatype,
    //! [in] If non zero, array element length is one byte
    const int datasize,
    //! [in] Number of packed bits per element
    const int npak
) {
    int missing = 0;

    if (missing_value_used() == 0) {
        // "magic value" mode off, return zero missing value found
        return 0;
    }
    int ldatatype = datatype & 0xF;
    if (ldatatype == 0 || ldatatype == 3 || ldatatype == 7 || ldatatype == 8) {
        // not valid for transparent or character types
        return 0;
    }
    if (ldatatype == 1 || ldatatype == 5 || ldatatype == 6) {
        // float or double
        if (datasize==64 || (ldatatype==5 && npak==64)) {
            missing = (*__fst_double_encode_missing)(dst, src, nElems, npak);
        } else {
            if (npak > 32) {
                // datalength > 32 not supported
                return 0;
            }
            missing = (*__fst_float_encode_missing)(dst, src, nElems, npak);
        }
    }
    if (ldatatype == 4) {
        // Signed
        if (datasize==16) {
            missing = (*__fst_short_encode_missing)(dst, src, nElems, npak);
        } else if (datasize==8) {
            missing = (*__fst_byte_encode_missing)(dst, src, nElems, npak);
        } else {
            missing = (*__fst_int_encode_missing)(dst, src, nElems, npak);
        }
    }
    if (ldatatype == 2) {
        // Unsigned
        if (datasize==16) {
            missing = (*__fst_ushort_encode_missing)(dst, src, nElems, npak);
        } else if (datasize==8) {
            missing = (*__fst_ubyte_encode_missing)(dst, src, nElems, npak);
        } else {
            missing = (*__fst_uint_encode_missing)(dst, src, nElems, npak);
        }
    }
    Lib_Log(APP_LIBFST,APP_DEBUG,"%s: %d missing values in %d data values replaced, base datatype=%d\n",__func__,missing,nElems,ldatatype);

    return missing;
}

/* Fortran entry points (calling C entry point EncodeMissingValue) */
#pragma weak encode_missing_value__ = encode_missing_value
#pragma weak encode_missing_value_  = encode_missing_value
int encode_missing_value_(
    void * const dst,
    const void * const src,
    const int * const nElems,
    const int * const datatype,
    const int * const datasize,
    const int * const npak
);
int encode_missing_value__(
    void * const dst,
    const void * const src,
    const int * const nElems,
    const int * const datatype,
    const int * const datasize,
    const int * const npak
);
int encode_missing_value(
    void * const dst,
    const void * const src,
    const int * const nElems,
    const int * const datatype,
    const int * const datasize,
    const int * const npak
) {
    return  EncodeMissingValue(dst, src, *nElems, *datatype, *datasize, *npak);
}


/* C entry point */
//! Replace all occurrences of the largest value in field/array by the "missing data" value
void DecodeMissingValue(
    //! [in,out] Field/array
    void * const field,
    //! [in] Number of elements in field/array
    const int nElems,
    //! [in] Data type
    //! | Value | Description                   |
    //! |  ---: | :---------------------------- |
    //! |     0 | Binary, transparent           |
    //! |     1 | Floating point                |
    //! |     2 | Unsigned integer              |
    //! |     3 | Character (R4A in an integer) |
    //! |     4 | Signed integer                |
    //! |     5 | IEEE floating point           |
    //! |     6 | Floating point                |
    //! |     7 | Character string              |
    //! |     8 | Complex IEEE                  |
    //! |  +128 | With second stage packer      |
    //! |   +64 | Missing data flagging is used |
    const int datatype,
    //! [in] Array element length
    const int datasize
) {
    if (missing_value_used() == 0) {
        // "magic value" mode off, do nothing
        return;
    }
    int ldatatype = datatype & 0xF;
    if (ldatatype == 0 || ldatatype == 3 || ldatatype == 7 || ldatatype == 8) {
        // not valid for complex, transparent or character types
        return;
    }
    if (ldatatype == 1 || ldatatype == 5 || ldatatype == 6) {
        if (datasize==64) {
            (*__fst_double_decode_missing)(field, nElems);
        } else {
            (*__fst_float_decode_missing)(field, nElems);
        }
    }
    if (ldatatype == 4) {
        if (datasize==16) {
            (*__fst_short_decode_missing)(field, nElems);
        } else if (datasize==8) {
            (*__fst_byte_decode_missing)(field, nElems);
        } else {
            (*__fst_int_decode_missing)(field, nElems);
        }
    }
    if (ldatatype == 2) {
        if (datasize==16) {
            (*__fst_ushort_decode_missing)(field, nElems);
        } else if (datasize==8) {
            (*__fst_ubyte_decode_missing)(field, nElems);
        } else {
            (*__fst_uint_decode_missing)(field, nElems);
        }
    }
}

/* Fortran entry points (calling C entry point DecodeMissingValue) */
#pragma weak decode_missing_value_  = decode_missing_value
void decode_missing_value_(
    void * const field,
    const int * const nElems,
    const int * const datatype,
    const int * const datasize
);
#pragma weak decode_missing_value__ = decode_missing_value
void decode_missing_value__(
    void * const field,
    const int * const nElems,
    const int * const datatype,
    const int * const datasize
);
void decode_missing_value(
    void * const field,
    const int * const nElems,
    const int * const datatype,
    const int * const datasize
) {
    DecodeMissingValue(field, *nElems, *datatype, *datasize);
}

#ifdef SELFTEST
#define ASIZE 16
/* test program , the 8 basic types are encoded/decoded
 * float, int, unsigned int, double, short, unsigned short, char, unsigned char
 * both the C and the FORTRAN interface are used in the test
*/
int main()
{
    float f, fa[ASIZE], fp[ASIZE];
    double d, da[ASIZE], dp[ASIZE];
    int i, ia[ASIZE], ip[ASIZE];
    unsigned int ui, uia[ASIZE], uip[ASIZE];
    int junk;
    short s, sa[ASIZE], sp[ASIZE];
    unsigned short us, usa[ASIZE], usp[ASIZE];
    signed char b, ba[ASIZE], bp[ASIZE];
    unsigned char ub, uba[ASIZE], ubp[ASIZE];
    int asize=ASIZE;
    int zero=0;
    int one=1;
    int two=2;
    int four=4;
    int sixteen=16;
    int thirtytwo=32;
    int sixtyfour=64;
    int fstat, istat, uistat, dstat, sstat, usstat, bstat, ubstat;

    RestoreMissingValueMapping();
#ifdef NODECODE
    SetMissingValueMapping(1, 1, fst_null_decode_missing, 32);
    SetMissingValueMapping(1, 1, fst_null_decode_missing, 64);
    SetMissingValueMapping(1, 2, fst_null_decode_missing, 32);
    SetMissingValueMapping(1, 2, fst_null_decode_missing, 8);
    SetMissingValueMapping(1, 2, fst_null_decode_missing, 16);
    SetMissingValueMapping(1, 4, fst_null_decode_missing, 32);
    SetMissingValueMapping(1, 4, fst_null_decode_missing, 8);
    SetMissingValueMapping(1, 4, fst_null_decode_missing, 16);
#endif
    SetMissingValueMapping(-1, 2, NULL, 32);  /* restore uint decoder */
    SetMissingValueMapping(-1, 2, NULL, 8);  /* restore ubyte decoder */
    SetMissingValueMapping(-1, 2, NULL, 16);  /* restore ushort decoder */
    for (i=0; i<ASIZE; i++) {
        fa[i]= i*1.0+.5;
        da[i]=fa[i]+1.2345;
        ia[i]=i-13;
        uia[i]=i+22;
        sa[i]=i-13;
        usa[i]=i+22;
        ba[i]=i-13;
        uba[i]=i+22;
        bp[i]=i-13;
        ubp[i]=i+22;
    }
    junk = get_missing_value_flags(&f, &i, &ui, &d, &s, &us, &b, &ub);
    f = -1.0E+37; d = -1.0E+37;
    set_missing_value_flags(&f, &i, &ui, &d, &s, &us, &b, &ub);
    fprintf(stderr, "%d float=%g, int=%d, uint=%u, double=%lg, short=%hd, ushort=%hu, byte=%hhd, ubyte=%hhu\n", junk, f, i, ui, d, s, us, b, ub);
    junk = missing_value_used();
    junk = get_missing_value_flags(&f, &i, &ui, &d, &s, &us, &b, &ub);
    fprintf(stderr, "%d float=%g, int=%d, uint=%u, double=%lg, short=%hd, ushort=%hu, byte=%hhd, ubyte=%hhu\n", junk, f, i, ui, d, s, us, b, ub);
    for (int ii = 0; ii < ASIZE; ii++) {
        fprintf(stderr, "%2d float=%12g, int=%12d, short=%8hd, byte=%8hhd, uint=%12u, ushort=%8hu, ubyte=%8hhu,  double=%12lg\n", ii, fa[ii], ia[ii], sa[ii], ba[ii], uia[ii], usa[ii], uba[ii], da[ii]);
    }
    fprintf(stderr, "================================================================\n");
    fa[0]=f; fa[ASIZE-1]=f;
    fstat=encode_missing_value(fp, fa, &asize, &one, &thirtytwo, &sixteen);
    da[0]=d; da[ASIZE-1]=d;
    dstat=encode_missing_value(dp, da, &asize, &one, &sixtyfour, &sixteen);
    ia[1]=i; ia[ASIZE-2]=i;
    istat=EncodeMissingValue(ip, ia, ASIZE, 4, 32, 8);
    sa[2]=s; sa[ASIZE-3]=s;
    sstat=EncodeMissingValue(sp, sa, ASIZE, 4, 16, 8);
    ba[3]=b; ba[ASIZE-4]=b;
    bstat=EncodeMissingValue(bp, ba, ASIZE, 4, 8, 8);
    uia[2]=ui; uia[ASIZE-3]=ui;
    uistat=EncodeMissingValue(uip, uia, ASIZE, 2, 32, 8);
    usa[3]=us; usa[ASIZE-4]=us;
    usstat=EncodeMissingValue(usp, usa, ASIZE, 2, 16, 8);
    uba[4]=ub; uba[ASIZE-5]=ub;
    ubstat=EncodeMissingValue(ubp, uba, ASIZE, 2, 8, 8);
    fprintf(stderr, "status values: %d %d %d %d %d %d %d %d\n", fstat, dstat, istat, sstat, bstat, uistat, usstat, ubstat);
    for (int ii = -1; ++ii < ASIZE; ) {
        fprintf(stderr, "%2d float=%12g, int=%12d, short=%8hd, byte=%8hhd, uint=%12u, ushort=%8hu, ubyte=%8hhu,  double=%12lg\n", ii, fp[ii], ip[ii], sp[ii], bp[ii], uip[ii], usp[ii], ubp[ii], dp[ii]);
    }
    fprintf(stderr, "================================================================\n");
    DecodeMissingValue(fp, ASIZE, 1, 32);
    DecodeMissingValue(dp, ASIZE, 1, 64);
    DecodeMissingValue(ip, ASIZE, 4, 32);
    decode_missing_value(sp, &asize, &four, &sixteen);
    DecodeMissingValue(bp, ASIZE, 4, 8);
    DecodeMissingValue(uip, ASIZE, 2, 32);
    decode_missing_value(usp, &asize, &two, &sizteen);
    DecodeMissingValue(ubp, ASIZE, 2, 8);
    for (int ii = 0; ii < ASIZE; ii++) {
        fprintf(stderr, "%2d float=%12g, int=%12d, short=%8hd, byte=%8hhd, uint=%12u, ushort=%8hu, ubyte=%8hhu,  double=%12lg\n", ii, fp[ii], ip[ii], sp[ii], bp[ii], uip[ii], usp[ii], ubp[ii], dp[ii]);
    }
    return 0;
}
#endif
