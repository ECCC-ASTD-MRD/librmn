//! \file fst_compression.c FST compression (packing) round-trip test
//!
//! Verifies that data written to FST files with various data types, element
//! sizes, and pack sizes is correctly recovered after a read-back.
//!
//! The test:
//! 1. Generates a synthetic 32x32x2 3D data set (float, double, int, uint64,
//!    uint8, uint16, and complex float/double).
//! 2. Writes every record in params_real, params_integer, and params_misc to
//!    both an RSF file and an XDF file, using the requested data_type /
//!    data_bits / pack_bits combination.
//! 3. Writes the records in params_fail and asserts that the write is
//!    rejected (pack_bits too small for the data type).
//! 4. Reads each record back and compares it against the original data:
//!    - Real data: relative L2-norm difference <= tol AND
//!      max per-element relative difference <= max_tol.
//!    - Integer and misc data: exact byte-for-byte match (64-bit integers in
//!      XDF are skipped because XDF does not support 64-bit integer records).
//!
//! Data types exercised (see src/fst/README.md "Data Types"):
//! - FST_TYPE_REAL_IEEE (E): IEEE float/double, lossless truncation
//! - FST_TYPE_REAL (F): recommended real type, lossy but cyclic
//! - FST_TYPE_REAL_OLD_QUANT (R): old quantification, lossy, non-cyclic
//! - FST_TYPE_UNSIGNED / FST_TYPE_SIGNED: integer types, exact round-trip
//! - FST_TYPE_BINARY: transparent raw storage, exact round-trip
//! - FST_TYPE_CHAR / FST_TYPE_STRING: 8-bit character types, exact round-trip
//! - FST_TYPE_COMPLEX: interleaved real/imaginary, exact round-trip (32/64 bits)
//! - FST_TYPE_TURBOPACK flag: additional lossless compression (2D only)
//! - FSTD_MISSING_FLAG: missing-value flag (64)
//!
//! Built and run via gen_test_target(SOURCES fst/fst_compression.c) in
//! tests/CMakeLists.txt. Produces compression.rsf and compression.xdf in
//! the build directory.

#include <math.h>

#include <App.h>
#include <rmn.h>

const char* test_filename_rsf = "compression.rsf"; //!< Name of the RSF test file
const char* test_filename_xdf = "compression.xdf"; //!< Name of the XDF test file

// const int NUM_DATA_X = 32;
// const int NUM_DATA_Y = 16;
// const int NUM_DATA_Z = 2;
const int NUM_DATA_X = 32; //!< Size of the test data along x
const int NUM_DATA_Y = 32; //!< Size of the test data along y
const int NUM_DATA_Z = 2;  //!< Size of the test data along z (number of levels)

static float* data_f = NULL;     //!< 32-bit IEEE float data
static double* data_d = NULL;    //!< 64-bit IEEE double data
static int8_t* data_i8 = NULL;   //!< 8-bit signed integer data (truncated from data_ull)
static int16_t* data_i16 = NULL; //!< 16-bit signed integer data (truncated from data_ull)
static int32_t* data_i = NULL;   //!< 32-bit signed integer data (truncated from data_ull)
static int64_t* data_l = NULL;   //!< 64-bit signed integer data (data_ull reinterpreted as signed)
static uint64_t* data_ull = NULL; //!< 64-bit unsigned integer data (see gen_value_uint64)
static uint32_t* data_umask = NULL; //!< 1-bit mask of data_i (used for the 1-bit pack test)
static uint8_t* data_u8 = NULL;  //!< 8-bit unsigned byte data (FST_TYPE_STRING / FST_TYPE_CHAR)
static uint16_t* data_u16 = NULL; //!< 16-bit unsigned short data (FST_TYPE_BINARY 16-bit)
static uint32_t* data_u32 = NULL; //!< 32-bit unsigned integer data (truncated from data_ull)
static float* data_c = NULL;     //!< 32-bit complex data, interleaved real/imaginary (FST_TYPE_COMPLEX)
static double* data_cd = NULL;   //!< 64-bit complex data, interleaved real/imaginary (FST_TYPE_COMPLEX)

#define MISSING 64 //!< Missing-value flag (same value as FSTD_MISSING_FLAG in rmn/fst98.h)

//! Description of one compression test case.
typedef struct {
    void** data;         //!< Pointer to the source data array (float*, double*, uint64_t*, ...)
    void** compare_data; //!< Pointer to the array used for comparison after read-back. May differ
                         //!< from data when the read-back element size is smaller (e.g. 64-bit
                         //!< source compared against a 32-bit array).
    int data_type;       //!< FST data type flag (FST_TYPE_REAL_IEEE, FST_TYPE_REAL, ...),
                         //!< optionally OR'd with FST_TYPE_TURBOPACK or FSTD_MISSING_FLAG
    int nk;              //!< Number of vertical levels to write (overrides NUM_DATA_Z)
    int compare_nk;      //!< Number of vertical levels to use when comparing the read-back data.
                         //!< Defaults to nk (0). Set to nk*2 for FST_TYPE_COMPLEX, whose
                         //!< read-back interleaves real/imaginary so it has twice the elements.
    int data_size;       //!< Element size in bits (data_bits in the fst_record)
    int pack_size;       //!< Requested storage size in bits (pack_bits in the fst_record)
    int compare_bits;    //!< Element size in bits to use when comparing the read-back data.
                         //!< Defaults to the read-back data_bits (0). Set to 8 for FST_TYPE_CHAR,
                         //!< whose read-back reports data_bits = 32 (chars are stored 4-per-32-bit
                         //!< word) but whose actual element size is 8 bits.
    double tol;          //!< Max allowed relative L2-norm difference (real data only)
    double max_tol;      //!< Max allowed per-element relative difference (real data only)
} test_params;

//! Real-data test cases.
//!
//! Covers FST_TYPE_REAL_IEEE (E), FST_TYPE_REAL (F), and FST_TYPE_REAL_OLD_QUANT (R),
//! each with and without FST_TYPE_TURBOPACK, at various pack sizes.
//! Tolerances increase as pack_size decreases (more bits are lost).
//! A pack_size equal to data_size must be lossless (tol = 0).
static const test_params params_real[] = {
    {.data = (void*)&data_f, .data_type = FST_TYPE_REAL_IEEE, .nk = NUM_DATA_Z,                  .data_size = 32, .pack_size = 32, .tol = 0.0, .max_tol = 0.0, .compare_data = (void*)&data_f},
    {.data = (void*)&data_f, .data_type = FST_TYPE_REAL_IEEE, .nk = NUM_DATA_Z,                  .data_size = 32, .pack_size = 28, .tol = 1e-6, .max_tol = 5e-5, .compare_data = (void*)&data_f},
    {.data = (void*)&data_f, .data_type = FST_TYPE_REAL_IEEE, .nk = NUM_DATA_Z,                  .data_size = 32, .pack_size = 24, .tol = 1.5e-5, .max_tol = 4e-5, .compare_data = (void*)&data_f},
    {.data = (void*)&data_f, .data_type = FST_TYPE_REAL_IEEE, .nk = NUM_DATA_Z,                  .data_size = 32, .pack_size = 20, .tol = 2.2e-4, .max_tol = 4.8e-4, .compare_data = (void*)&data_f},
    {.data = (void*)&data_f, .data_type = FST_TYPE_REAL_IEEE, .nk = NUM_DATA_Z,                  .data_size = 32, .pack_size = 16, .tol = 0.004, .max_tol = 0.008, .compare_data = (void*)&data_f},
    {.data = (void*)&data_f, .data_type = FST_TYPE_REAL_IEEE | FST_TYPE_TURBOPACK, .nk = NUM_DATA_Z, .data_size = 32, .pack_size = 32, .tol = 0.0, .max_tol = 0.0, .compare_data = (void*)&data_f},
    {.data = (void*)&data_f, .data_type = FST_TYPE_REAL_IEEE | FST_TYPE_TURBOPACK, .nk = 1,      .data_size = 32, .pack_size = 32, .tol = 0.0, .max_tol = 0.0, .compare_data = (void*)&data_f},
    {.data = (void*)&data_f, .data_type = FST_TYPE_REAL_IEEE | FST_TYPE_TURBOPACK, .nk = 1,      .data_size = 32, .pack_size = 28, .tol = 1e-6, .max_tol = 5e-5, .compare_data = (void*)&data_f},
    {.data = (void*)&data_f, .data_type = FST_TYPE_REAL_IEEE | FST_TYPE_TURBOPACK, .nk = 1,      .data_size = 32, .pack_size = 24, .tol = 2e-5, .max_tol = 4e-5, .compare_data = (void*)&data_f},
    {.data = (void*)&data_f, .data_type = FST_TYPE_REAL_IEEE | FST_TYPE_TURBOPACK, .nk = 1,      .data_size = 32, .pack_size = 20, .tol = 2.2e-4, .max_tol = 4.8e-4, .compare_data = (void*)&data_f},
    {.data = (void*)&data_f, .data_type = FST_TYPE_REAL_IEEE | FST_TYPE_TURBOPACK, .nk = 1,      .data_size = 32, .pack_size = 16, .tol = 0.004, .max_tol = 0.008, .compare_data = (void*)&data_f},
    {.data = (void*)&data_d, .data_type = FST_TYPE_REAL_IEEE, .nk = NUM_DATA_Z,                  .data_size = 64, .pack_size = 64, .tol = 0.0, .max_tol = 0.0, .compare_data = (void*)&data_d},
    {.data = (void*)&data_d, .data_type = FST_TYPE_REAL_IEEE, .nk = NUM_DATA_Z,                  .data_size = 64, .pack_size = 48, .tol = 0.0, .max_tol = 0.0, .compare_data = (void*)&data_d},
    {.data = (void*)&data_d, .data_type = FST_TYPE_REAL_IEEE, .nk = NUM_DATA_Z,                  .data_size = 64, .pack_size = 32, .tol = 0.0, .max_tol = 0.0, .compare_data = (void*)&data_f},
    {.data = (void*)&data_d, .data_type = FST_TYPE_REAL_IEEE, .nk = NUM_DATA_Z,                  .data_size = 64, .pack_size = 28, .tol = 1e-6, .max_tol = 2e-6, .compare_data = (void*)&data_f},
    {.data = (void*)&data_d, .data_type = FST_TYPE_REAL_IEEE, .nk = NUM_DATA_Z,                  .data_size = 64, .pack_size = 24, .tol = 2e-5, .max_tol = 4e-5, .compare_data = (void*)&data_f},
    {.data = (void*)&data_d, .data_type = FST_TYPE_REAL_IEEE, .nk = NUM_DATA_Z,                  .data_size = 64, .pack_size = 20, .tol = 2.2e-4, .max_tol = 4.8e-4, .compare_data = (void*)&data_f},
    {.data = (void*)&data_f, .data_type = FST_TYPE_REAL, .nk = NUM_DATA_Z,                       .data_size = 32, .pack_size = 32, .tol = 0.0, .max_tol = 0.0, .compare_data = (void*)&data_f},
    {.data = (void*)&data_f, .data_type = FST_TYPE_REAL, .nk = NUM_DATA_Z,                       .data_size = 32, .pack_size = 28, .tol = 0.0, .max_tol = 0.0, .compare_data = (void*)&data_f},
    {.data = (void*)&data_f, .data_type = FST_TYPE_REAL, .nk = NUM_DATA_Z,                       .data_size = 32, .pack_size = 24, .tol = 1e-7, .max_tol = 2e-4, .compare_data = (void*)&data_f},
    {.data = (void*)&data_f, .data_type = FST_TYPE_REAL, .nk = NUM_DATA_Z,                       .data_size = 32, .pack_size = 16, .tol = 1.1e-5, .max_tol = 2e-3, .compare_data = (void*)&data_f},
    {.data = (void*)&data_f, .data_type = FST_TYPE_REAL, .nk = NUM_DATA_Z,                       .data_size = 32, .pack_size = 12, .tol = 2e-4, .max_tol = 0.06, .compare_data = (void*)&data_f},
    {.data = (void*)&data_f, .data_type = FST_TYPE_REAL | FST_TYPE_TURBOPACK, .nk = 1,           .data_size = 32, .pack_size = 32, .tol = 0.0, .max_tol = 0.0, .compare_data = (void*)&data_f},
    {.data = (void*)&data_f, .data_type = FST_TYPE_REAL | FST_TYPE_TURBOPACK, .nk = 1,           .data_size = 32, .pack_size = 28, .tol = 0.0, .max_tol = 0.0, .compare_data = (void*)&data_f},
    {.data = (void*)&data_f, .data_type = FST_TYPE_REAL | FST_TYPE_TURBOPACK, .nk = 1,           .data_size = 32, .pack_size = 24, .tol = 1e-7, .max_tol = 2.3e-5, .compare_data = (void*)&data_f},
    {.data = (void*)&data_f, .data_type = FST_TYPE_REAL | FST_TYPE_TURBOPACK, .nk = 1,           .data_size = 32, .pack_size = 16, .tol = 2e-5, .max_tol = 2e-3, .compare_data = (void*)&data_f},
    {.data = (void*)&data_f, .data_type = FST_TYPE_REAL | FST_TYPE_TURBOPACK, .nk = 1,           .data_size = 32, .pack_size = 12, .tol = 2e-4, .max_tol = 0.06, .compare_data = (void*)&data_f},
    {.data = (void*)&data_f, .data_type = FST_TYPE_REAL_OLD_QUANT, .nk = NUM_DATA_Z,             .data_size = 32, .pack_size = 32, .tol = 0.0, .max_tol = 0.0, .compare_data = (void*)&data_f},
    {.data = (void*)&data_f, .data_type = FST_TYPE_REAL_OLD_QUANT, .nk = NUM_DATA_Z,             .data_size = 32, .pack_size = 28, .tol = 1e-8, .max_tol = 3e-6, .compare_data = (void*)&data_f},
    {.data = (void*)&data_f, .data_type = FST_TYPE_REAL_OLD_QUANT, .nk = NUM_DATA_Z,             .data_size = 32, .pack_size = 24, .tol = 1e-7, .max_tol = 2e-4, .compare_data = (void*)&data_f},
    {.data = (void*)&data_f, .data_type = FST_TYPE_REAL_OLD_QUANT, .nk = NUM_DATA_Z,             .data_size = 32, .pack_size = 16, .tol = 2e-5, .max_tol = 0.01, .compare_data = (void*)&data_f},
    {.data = (void*)&data_f, .data_type = FST_TYPE_REAL_OLD_QUANT | MISSING, .nk = NUM_DATA_Z,   .data_size = 32, .pack_size = 16, .tol = 2e-5, .max_tol = 0.01, .compare_data = (void*)&data_f},
    {.data = (void*)&data_f, .data_type = FST_TYPE_REAL_OLD_QUANT | FST_TYPE_TURBOPACK, .nk = 1, .data_size = 32, .pack_size = 32, .tol = 0.0, .max_tol = 0.0, .compare_data = (void*)&data_f},
    {.data = (void*)&data_f, .data_type = FST_TYPE_REAL_OLD_QUANT | FST_TYPE_TURBOPACK, .nk = 1, .data_size = 32, .pack_size = 28, .tol = 1e-8, .max_tol = 3e-6, .compare_data = (void*)&data_f},
    {.data = (void*)&data_f, .data_type = FST_TYPE_REAL_OLD_QUANT | FST_TYPE_TURBOPACK, .nk = 1, .data_size = 32, .pack_size = 24, .tol = 1e-7, .max_tol = 2.3e-5, .compare_data = (void*)&data_f},
    {.data = (void*)&data_f, .data_type = FST_TYPE_REAL_OLD_QUANT | FST_TYPE_TURBOPACK, .nk = 1, .data_size = 32, .pack_size = 16, .tol = 2.1e-5, .max_tol = 0.01, .compare_data = (void*)&data_f},
    {.data = (void*)&data_d, .data_type = FST_TYPE_REAL_OLD_QUANT, .nk = NUM_DATA_Z,             .data_size = 64, .pack_size = 64, .tol = 0.0, .max_tol = 0.0, .compare_data = (void*)&data_d},
    {.data = (void*)&data_d, .data_type = FST_TYPE_REAL_OLD_QUANT, .nk = NUM_DATA_Z,             .data_size = 64, .pack_size = 50, .tol = 0.0, .max_tol = 0.0, .compare_data = (void*)&data_d},
    {.data = (void*)&data_d, .data_type = FST_TYPE_REAL_OLD_QUANT, .nk = NUM_DATA_Z,             .data_size = 64, .pack_size = 32, .tol = 0.0, .max_tol = 0.0, .compare_data = (void*)&data_f},
    {.data = (void*)&data_d, .data_type = FST_TYPE_REAL_OLD_QUANT | MISSING, .nk = NUM_DATA_Z,   .data_size = 64, .pack_size = 32, .tol = 0.0, .max_tol = 0.0, .compare_data = (void*)&data_f},
    {.data = (void*)&data_d, .data_type = FST_TYPE_REAL_OLD_QUANT, .nk = NUM_DATA_Z,             .data_size = 64, .pack_size = 28, .tol = 2.3e-8, .max_tol = 3e-6, .compare_data = (void*)&data_f},
    {.data = (void*)&data_d, .data_type = FST_TYPE_REAL_OLD_QUANT, .nk = NUM_DATA_Z,             .data_size = 64, .pack_size = 24, .tol = 1e-7, .max_tol = 2.3e-5, .compare_data = (void*)&data_f},
    {.data = (void*)&data_d, .data_type = FST_TYPE_REAL_OLD_QUANT, .nk = NUM_DATA_Z,             .data_size = 64, .pack_size = 16, .tol = 2.1e-5, .max_tol = 0.01, .compare_data = (void*)&data_f},
    {.data = (void*)&data_d, .data_type = FST_TYPE_REAL_OLD_QUANT | MISSING, .nk = NUM_DATA_Z,   .data_size = 64, .pack_size = 16, .tol = 2.1e-5, .max_tol = 0.01, .compare_data = (void*)&data_f},

    // Double-precision (64-bit) variants.
    // REAL_IEEE | TURBOPACK at 64 bits: turbopack is stripped (pack_bits > 32), stored as
    // 64-bit IEEE (lossless). nk = 1 because turbopack is only supported for 2D data.
    {.data = (void*)&data_d, .data_type = FST_TYPE_REAL_IEEE | FST_TYPE_TURBOPACK, .nk = 1, .data_size = 64, .pack_size = 64, .tol = 0.0, .max_tol = 0.0, .compare_data = (void*)&data_d},
    // REAL_IEEE | TURBOPACK at 32 bits from a double source: double->float conversion, then
    // lossless turbopack. Compared against the float array (data_f).
    {.data = (void*)&data_d, .data_type = FST_TYPE_REAL_IEEE | FST_TYPE_TURBOPACK, .nk = 1, .data_size = 64, .pack_size = 32, .tol = 0.0, .max_tol = 0.0, .compare_data = (void*)&data_f},
    // FST_TYPE_REAL (F) from a double source at 64 bits: converted to 64-bit IEEE (lossless).
    {.data = (void*)&data_d, .data_type = FST_TYPE_REAL, .nk = NUM_DATA_Z, .data_size = 64, .pack_size = 64, .tol = 0.0, .max_tol = 0.0, .compare_data = (void*)&data_d},
    // FST_TYPE_REAL (F) from a double source at 32 bits: double->float conversion, then
    // lossless F32 pack. Compared against the float array (data_f).
    {.data = (void*)&data_d, .data_type = FST_TYPE_REAL, .nk = NUM_DATA_Z, .data_size = 64, .pack_size = 32, .tol = 0.0, .max_tol = 0.0, .compare_data = (void*)&data_f},
};

//! Integer test cases: FST_TYPE_UNSIGNED and FST_TYPE_SIGNED at 8/16/32/64 bits.
//! Each case uses source data of the matching width (e.g. 32-bit SIGNED uses data_i,
//! 16-bit UNSIGNED uses data_u16), so the round-trip is checked against the same-width
//! array. Integer round-trips must be exact (byte-for-byte), so no tolerance fields are set.
//! The mask case exercises a 1-bit pack of 32-bit unsigned data.
//!
//! The FST_TYPE_TURBOPACK cases use nk = 1 because turbopack is only supported for 2D data
//! (it is stripped at write time for nk > 1). UNSIGNED|TURBOPACK exercises the armn_compress
//! path at 8/16/32 bits; SIGNED|TURBOPACK locks in the reset-to-SIGNED behaviour (turbopack is
//! not supported for signed data, so it is written as plain SIGNED).
//! The nk = 1 (2D) non-turbopack cases exercise the 2D integer path.
static const test_params params_integer[] = {
    {.data = (void*)&data_ull, .data_type = FST_TYPE_UNSIGNED, .data_size = 64, .pack_size = 64, .nk = NUM_DATA_Z, .compare_data = (void*)&data_ull},
    {.data = (void*)&data_l,   .data_type = FST_TYPE_SIGNED,   .data_size = 64, .pack_size = 64, .nk = NUM_DATA_Z, .compare_data = (void*)&data_l},
    {.data = (void*)&data_u32, .data_type = FST_TYPE_UNSIGNED, .data_size = 32, .pack_size = 32, .nk = NUM_DATA_Z, .compare_data = (void*)&data_u32},
    {.data = (void*)&data_i,   .data_type = FST_TYPE_SIGNED,   .data_size = 32, .pack_size = 32, .nk = NUM_DATA_Z, .compare_data = (void*)&data_i},
    {.data = (void*)&data_u16, .data_type = FST_TYPE_UNSIGNED, .data_size = 16, .pack_size = 16, .nk = NUM_DATA_Z, .compare_data = (void*)&data_u16},
    {.data = (void*)&data_i16, .data_type = FST_TYPE_SIGNED,   .data_size = 16, .pack_size = 16, .nk = NUM_DATA_Z, .compare_data = (void*)&data_i16},
    {.data = (void*)&data_u8,  .data_type = FST_TYPE_UNSIGNED, .data_size =  8, .pack_size =  8, .nk = NUM_DATA_Z, .compare_data = (void*)&data_u8},
    {.data = (void*)&data_i8,  .data_type = FST_TYPE_SIGNED,   .data_size =  8, .pack_size =  8, .nk = NUM_DATA_Z, .compare_data = (void*)&data_i8},
    {.data = (void*)&data_umask, .data_type =FST_TYPE_UNSIGNED,.data_size =  32, .pack_size =  1, .nk = NUM_DATA_Z, .compare_data = (void*)&data_umask},
    // TURBOPACK integer cases (2D only)
    {.data = (void*)&data_u32, .data_type = FST_TYPE_UNSIGNED | FST_TYPE_TURBOPACK, .data_size = 32, .pack_size = 32, .nk = 1, .compare_data = (void*)&data_u32},
    {.data = (void*)&data_u16, .data_type = FST_TYPE_UNSIGNED | FST_TYPE_TURBOPACK, .data_size = 16, .pack_size = 16, .nk = 1, .compare_data = (void*)&data_u16},
    {.data = (void*)&data_u8,  .data_type = FST_TYPE_UNSIGNED | FST_TYPE_TURBOPACK, .data_size =  8, .pack_size =  8, .nk = 1, .compare_data = (void*)&data_u8},
    {.data = (void*)&data_i,   .data_type = FST_TYPE_SIGNED   | FST_TYPE_TURBOPACK, .data_size = 32, .pack_size = 32, .nk = 1, .compare_data = (void*)&data_i},
    // 2D (nk = 1) non-turbopack integer cases
    {.data = (void*)&data_u32, .data_type = FST_TYPE_UNSIGNED, .data_size = 32, .pack_size = 32, .nk = 1, .compare_data = (void*)&data_u32},
    {.data = (void*)&data_i,   .data_type = FST_TYPE_SIGNED,   .data_size = 32, .pack_size = 32, .nk = 1, .compare_data = (void*)&data_i},
    {.data = (void*)&data_u16, .data_type = FST_TYPE_UNSIGNED, .data_size = 16, .pack_size = 16, .nk = 1, .compare_data = (void*)&data_u16},
    {.data = (void*)&data_i16, .data_type = FST_TYPE_SIGNED,   .data_size = 16, .pack_size = 16, .nk = 1, .compare_data = (void*)&data_i16},
    {.data = (void*)&data_u8,  .data_type = FST_TYPE_UNSIGNED, .data_size =  8, .pack_size =  8, .nk = 1, .compare_data = (void*)&data_u8},
    {.data = (void*)&data_i8,  .data_type = FST_TYPE_SIGNED,   .data_size =  8, .pack_size =  8, .nk = 1, .compare_data = (void*)&data_i8},
};

//! "Misc" test cases: the non-real, non-integer FST types that were previously untested.
//! All of these round-trip exactly (byte-for-byte), so no tolerance fields are set.
//!
//! - FST_TYPE_BINARY: transparent raw storage. pack_bits is the element size in bits
//!   (8/16/32/64). The data is copied verbatim, so the round-trip is exact.
//! - FST_TYPE_CHAR: 8-bit characters packed 4-per-32-bit-word. The write path only uses
//!   ni*nj (nk is ignored), so it must be tested with nk = 1.
//! - FST_TYPE_STRING: 8-bit characters, one byte per element, full 3D (nk = NUM_DATA_Z).
//! - FST_TYPE_COMPLEX: interleaved real/imaginary. The read-back has twice the elements
//!   (compare_nk = nk*2). Only 32/32 and 64/64 are supported: a 64->32 (lossy) complex
//!   write is NOT supported by the write path (ieeepak would copy 32-bit words from a
//!   64-bit source without a double->float conversion), so it is intentionally not tested.
static const test_params params_misc[] = {
    // FST_TYPE_BINARY: raw storage at each element size
    {.data = (void*)&data_u8,   .data_type = FST_TYPE_BINARY, .data_size =  8, .pack_size =  8, .nk = NUM_DATA_Z, .compare_data = (void*)&data_u8},
    {.data = (void*)&data_u16,  .data_type = FST_TYPE_BINARY, .data_size = 16, .pack_size = 16, .nk = NUM_DATA_Z, .compare_data = (void*)&data_u16},
    {.data = (void*)&data_ull,  .data_type = FST_TYPE_BINARY, .data_size = 32, .pack_size = 32, .nk = NUM_DATA_Z, .compare_data = (void*)&data_ull},
    {.data = (void*)&data_ull,  .data_type = FST_TYPE_BINARY, .data_size = 64, .pack_size = 64, .nk = NUM_DATA_Z, .compare_data = (void*)&data_ull},
    // FST_TYPE_CHAR: 8-bit characters, 2D only (nk = 1). The read-back reports
    // data_bits = 32 (chars are stored 4-per-32-bit-word), so force the
    // comparison to use 8-bit elements via compare_bits.
    {.data = (void*)&data_u8,   .data_type = FST_TYPE_CHAR,   .data_size =  8, .pack_size =  8, .nk = 1, .compare_bits = 8, .compare_data = (void*)&data_u8},
    // FST_TYPE_STRING: 8-bit characters, full 3D. The read-back reports
    // data_bits = 32 (chars are stored 4-per-32-bit-word), so force the
    // comparison to use 8-bit elements via compare_bits.
    {.data = (void*)&data_u8,   .data_type = FST_TYPE_STRING, .data_size =  8, .pack_size =  8, .nk = NUM_DATA_Z, .compare_bits = 8, .compare_data = (void*)&data_u8},
    // FST_TYPE_COMPLEX: interleaved real/imaginary, lossless at 32 and 64 bits
    {.data = (void*)&data_c,    .data_type = FST_TYPE_COMPLEX, .data_size = 32, .pack_size = 32, .nk = NUM_DATA_Z, .compare_nk = 2 * NUM_DATA_Z, .compare_data = (void*)&data_c},
    {.data = (void*)&data_cd,   .data_type = FST_TYPE_COMPLEX, .data_size = 64, .pack_size = 64, .nk = NUM_DATA_Z, .compare_nk = 2 * NUM_DATA_Z, .compare_data = (void*)&data_cd},
};

//! Cases where the write must be rejected because pack_bits is too small
//! for the data type (e.g. 8-bit pack for 32-bit IEEE float).
//! The test asserts that fst24_write returns <= 0 for each of these.
static const test_params params_fail[] = {
    {.data = (void*)&data_f, .data_type = FST_TYPE_REAL_IEEE, .data_size = 32, .pack_size = 8,  .nk = NUM_DATA_Z, .tol = 0.45, .max_tol = 0.75, .compare_data = (void*)&data_f},
    {.data = (void*)&data_f, .data_type = FST_TYPE_REAL_IEEE | FST_TYPE_TURBOPACK, .data_size = 32, .pack_size = 14, .nk = 1, .tol = 0.015, .max_tol = 0.035, .compare_data = (void*)&data_f},
    {.data = (void*)&data_f, .data_type = FST_TYPE_REAL_IEEE | FST_TYPE_TURBOPACK, .data_size = 32, .pack_size = 8, .nk = 1, .tol = 0.45, .max_tol = 0.8, .compare_data = (void*)&data_f},
};

const int NUM_CASES_REAL    = sizeof(params_real) / sizeof(test_params);    //!< Number of real-data test cases
const int NUM_CASES_INTEGER = sizeof(params_integer) / sizeof(test_params); //!< Number of integer test cases
const int NUM_CASES_MISC    = sizeof(params_misc) / sizeof(test_params);    //!< Number of misc (binary/char/string/complex) test cases
const int NUM_CASES_FAIL    = sizeof(params_fail) / sizeof(test_params);    //!< Number of expected-failure cases

//! Generate a smooth, bounded real value for grid point (i, j, k).
//! The value is a sum of sin/cos in the horizontal and a power of 2 in the
//! vertical, giving a range of roughly [-2, 4]. Values are finite by
//! construction; a non-finite result is a hard error.
//! \return The generated value
double gen_value_real(const int i, const int j, const int k, const int num_x, const int num_y, const int num_z) {
    double val = sin((double)i / ((double)num_x / 2)) + cos((double)j / ((double)num_y / 4)) + pow(2.0, (double)k / num_z);
    if (!isfinite(val)) {
        App_Log(APP_ERROR, "%s: Woahhh got a NaN at [%d, %d, %d]\n", __func__, i, j, k);
        exit(-1);
    }
    if (fabs(val) > 4.0) {
        App_Log(APP_WARNING, "%s: val is so large! %f\n", val);
    }
    return val;
}

//! Generate a uint64 value that exercises many bit positions.
//! The grid indices are combined into a linear index and mixed with a
//! multiplicative hash so that bits are spread across the full 64-bit range.
//! This ensures that truncation to any smaller integer width still produces
//! values that vary across grid points and exercise the full range of the type.
//! \return The generated value
uint64_t gen_value_uint64(const uint64_t i, const uint64_t j, const uint64_t k, const int num_x, const int num_y, const int num_z) {
    const uint64_t index = (i * (uint64_t)num_y + j) * (uint64_t)num_z + k;
    uint64_t v = index * 0x9E3779B97F4A7C15ULL;
    v ^= v >> 32;
    return v;
}

//! Allocate and fill all data arrays (float, double, int32, uint32 mask,
//! int64, uint64, uint8, uint16, and complex float/double) with the synthetic
//! values. Called once from main(); subsequent calls are no-ops.
void make_data() {
    if (data_f != NULL || data_d != NULL) return;
    data_f = (float*) malloc(NUM_DATA_X * NUM_DATA_Y * NUM_DATA_Z * sizeof(float));
    data_d = (double*) malloc(NUM_DATA_X * NUM_DATA_Y * NUM_DATA_Z * sizeof(double));
    data_i8 = (int8_t*) malloc(NUM_DATA_X * NUM_DATA_Y * NUM_DATA_Z * sizeof(int8_t));
    data_i16 = (int16_t*) malloc(NUM_DATA_X * NUM_DATA_Y * NUM_DATA_Z * sizeof(int16_t));
    data_i = (int32_t*) malloc(NUM_DATA_X * NUM_DATA_Y * NUM_DATA_Z * sizeof(int32_t));
    data_l = (int64_t*) malloc(NUM_DATA_X * NUM_DATA_Y * NUM_DATA_Z * sizeof(int64_t));
    data_umask = (uint32_t*) malloc(NUM_DATA_X * NUM_DATA_Y * NUM_DATA_Z * sizeof(int32_t));
    data_ull = (uint64_t*) malloc(NUM_DATA_X * NUM_DATA_Y * NUM_DATA_Z * sizeof(uint64_t));
    data_u8 = (uint8_t*) malloc(NUM_DATA_X * NUM_DATA_Y * NUM_DATA_Z * sizeof(uint8_t));
    data_u16 = (uint16_t*) malloc(NUM_DATA_X * NUM_DATA_Y * NUM_DATA_Z * sizeof(uint16_t));
    data_u32 = (uint32_t*) malloc(NUM_DATA_X * NUM_DATA_Y * NUM_DATA_Z * sizeof(uint32_t));
    data_c = (float*) malloc(NUM_DATA_X * NUM_DATA_Y * NUM_DATA_Z * 2 * sizeof(float));
    data_cd = (double*) malloc(NUM_DATA_X * NUM_DATA_Y * NUM_DATA_Z * 2 * sizeof(double));

    if (data_f == NULL || data_d == NULL || data_i8 == NULL || data_i16 == NULL || data_i == NULL ||
        data_l == NULL || data_umask == NULL || data_ull == NULL || data_u8 == NULL || data_u16 == NULL ||
        data_u32 == NULL || data_c == NULL || data_cd == NULL) {
        App_Log(APP_ERROR, "%s: Unable to allocate enough space\n", __func__);
        exit(-1);
    }

    for (int i = 0; i < NUM_DATA_X; i++) {
        for (int j = 0; j < NUM_DATA_Y; j++) {
            for (int k = 0; k < NUM_DATA_Z; k++) {
                const int index = (i * NUM_DATA_Y + j) * NUM_DATA_Z + k;
                data_d[index] = gen_value_real(i, j, k, NUM_DATA_X, NUM_DATA_Y, NUM_DATA_Z);
                data_f[index] = (float)data_d[index];
                data_ull[index] = gen_value_uint64(i, j, k, NUM_DATA_X, NUM_DATA_Y, NUM_DATA_Z);
                // Signed and unsigned integer data at each width: derived from the uint64
                // value so that truncation to smaller widths produces detectable,
                // non-trivial patterns that exercise the full range of each type.
                data_i8[index] = (int8_t)(data_ull[index] & 0xff);
                data_i16[index] = (int16_t)(data_ull[index] & 0xffff);
                data_i[index] = (int32_t)(data_ull[index] & 0xffffffff);
                data_l[index] = (int64_t)data_ull[index];
                data_u8[index] = (uint8_t)(data_ull[index] & 0xff);
                data_u16[index] = (uint16_t)(data_ull[index] & 0xffff);
                data_u32[index] = (uint32_t)(data_ull[index] & 0xffffffff);
                data_umask[index] = (uint32_t)(data_i[index] & 1);
                // Complex data: interleaved real/imaginary. The real part is the synthetic
                // value and the imaginary part is a shifted variant, so both halves are
                // non-trivial and distinct.
                const double re = data_d[index];
                const double im = gen_value_real(i + 1, j + 1, k, NUM_DATA_X, NUM_DATA_Y, NUM_DATA_Z);
                data_c[2 * index] = (float)re;
                data_c[2 * index + 1] = (float)im;
                data_cd[2 * index] = re;
                data_cd[2 * index + 1] = im;
            }
        }
    }
    // fprintf(stderr, "\n");
}

//! Compare two float arrays element-by-element.
//! Passes if BOTH conditions hold:
//! - relative L2-norm difference ||a-b|| / ||a|| <= tol
//! - max per-element |a-b|/|a| <= max_tol
//! On failure, prints the norms, the worst element, and a 2-D slice of both
//! arrays for visual inspection.
//! \return 0 on success, -1 on failure
int compare_data_f(const float* a, const float* b, const int num_x, const int num_y, const int num_z,
                   const float tol, const float max_tol) {
    double total_diff = 0.0;
    double total_a = 0.0;
    double max_diff = 0.0;
    double max_diff_a = 0.0;
    double max_diff_b = 0.0;
    for (int i = 0; i < num_x; i++) {
        for (int j = 0; j < num_y; j++) {
            for (int k = 0; k < num_z; k++) {
                const int index = (i * num_y + j) * num_z + k;
                const double diff = a[index] - b[index];
                total_diff += diff * diff;
                total_a += a[index] * a[index];
                if (fabs(diff / a[index]) > max_diff) {
                    max_diff = fabs(diff / a[index]);
                    max_diff_a = a[index];
                    max_diff_b = b[index];
                }
            }
        }
    }

    const double diff_norm = sqrt(total_diff);
    const double a_norm = sqrt(total_a);
    const double rel_diff = diff_norm / a_norm;

    if (rel_diff > tol || max_diff > max_tol) {
        App_Log(APP_ERROR, "%s: Relative difference = %.2e (diff norm %.2e, a norm %.2e)\n",
                __func__, rel_diff, diff_norm, a_norm);
        App_Log(APP_ERROR, "%s: Max diff = %.2e (a = %.3f, b = %.3f)\n", __func__, max_diff, max_diff_a, max_diff_b);


        char* buffer = malloc(40000);
        char* ptr = buffer;
        const int NUM_COL = num_x > 16 ? 16 : num_x;
        const int NUM_ROW = num_y * num_z > 33 ? 33 : num_y * num_z;
        const int COL_WIDTH = 7;
        for (int j = 0; j < NUM_ROW; j++) {
            sprintf(ptr, "%2d a: ", j);
            ptr += 6;
            for (int i = 0; i < NUM_COL; i++) {
                sprintf(ptr, " %6.3f", a[i + j * num_x]);
                ptr += COL_WIDTH;
            }
            ptr[0] = '\n'; ptr++;
            sprintf(ptr, "   b: ");
            ptr += 6;
            for (int i = 0; i < NUM_COL; i++) {
                sprintf(ptr, " %6.3f", b[i + j * num_x]);
                ptr += COL_WIDTH;
            }
            ptr[0] = '\n'; ptr++;
        }
        ptr[0] = '\0';

        App_Log(APP_ERROR, "Values: \n%s\n", buffer);

        free(buffer);
        return -1;
    }

    return 0;
}

//! Double-precision variant of compare_data_f. Same pass criteria, plus
//! explicit checks that the running sums stay finite (guards against
//! overflow with very large data values).
//! \return 0 on success, -1 on failure
int compare_data_d(const double* a, const double* b, const int num_x, const int num_y, const int num_z,
                   const double tol, const double max_tol) {
    double total_diff = 0.0;
    double total_a = 0.0;
    double max_diff = 0.0;
    double max_diff_a = 0.0;
    double max_diff_b = 0.0;
    for (int i = 0; i < num_x; i++) {
        for (int j = 0; j < num_y; j++) {
            for (int k = 0; k < num_z; k++) {
                const int index = (i * num_y + j) * num_z + k;
                const double diff = a[index] - b[index];
                total_diff += diff * diff;
                total_a += a[index] * a[index];
                if (!isfinite(total_a)) {
                    App_Log(APP_ERROR, "%s: total A = %f, a[%d, %d, %d] = %f\n",
                            __func__, total_a, i, j, k, a[index]);
                    exit(-1);
                }
                if (!isfinite(total_diff)) {
                    App_Log(APP_ERROR, "%s: total diff = %f, diff[%d, %d, %d] = %f, a = %f, b = %f\n",
                            __func__, total_diff, i, j, k, diff, a[index], b[index]);
                    exit(-1);
                }
                if (fabs(diff / a[index]) > max_diff) {
                    max_diff = fabs(diff / a[index]);
                    max_diff_a = a[index];
                    max_diff_b = b[index];
                }
            }
        }
    }

    const double diff_norm = sqrt(total_diff);
    const double a_norm = sqrt(total_a);
    const double rel_diff = diff_norm / a_norm;

    if (rel_diff > tol || max_diff > max_tol) {
        App_Log(APP_ERROR, "%s: Relative difference = %.2e (diff norm %.2e, a norm %.2e)\n",
                __func__, rel_diff, diff_norm, a_norm);
        App_Log(APP_ERROR, "%s: Max diff = %.2e (a = %.3f, b = %.3f)\n", __func__, max_diff, max_diff_a, max_diff_b);


        char* buffer = malloc(40000);
        char* ptr = buffer;
        const int NUM_COL = num_x > 16 ? 16 : num_x;
        const int NUM_ROW = num_y * num_z > 33 ? 33 : num_y * num_z;
        const int COL_WIDTH = 7;
        for (int j = 0; j < NUM_ROW; j++) {
            sprintf(ptr, "%2d a: ", j);
            ptr += 6;
            for (int i = 0; i < NUM_COL; i++) {
                sprintf(ptr, " %6.3f", a[i + j * num_x]);
                ptr += COL_WIDTH;
            }
            ptr[0] = '\n'; ptr++;
            sprintf(ptr, "   b: ");
            ptr += 6;
            for (int i = 0; i < NUM_COL; i++) {
                sprintf(ptr, " %6.3f", b[i + j * num_x]);
                ptr += COL_WIDTH;
            }
            ptr[0] = '\n'; ptr++;
        }
        ptr[0] = '\0';

        App_Log(APP_ERROR, "Values: \n%s\n", buffer);

        free(buffer);
        return -1;
    }

    return 0;
}

//! Exact byte-for-byte comparison for integer data.
//! Interprets both arrays as raw bytes of num_bits-wide elements and
//! requires every byte to match. On the first mismatch, prints a hex dump
//! of a 2-D slice of both arrays.
//! \return 0 on success, -1 on failure
int compare_data_bytes(const uint8_t* a, const uint8_t* b, const int num_x, const int num_y, const int num_z, const int num_bits) {
    const int bytes_per_elem = num_bits / 8;
    for (uint64_t i = 0; i < num_x * num_y * num_z * bytes_per_elem; i++) {
        if (a[i] != b[i]) {
            App_Log(APP_ERROR, "%s: Byte %lu is different (%u vs %u)\n", __func__, i, a[i], b[i]);

            char* buffer = malloc(40000);
            memset(buffer, 0, 40000);
            char* ptr = buffer;

            const int row_size = num_z * num_y * bytes_per_elem;
            const int NUM_COL = row_size > 40 ? 40 : row_size;
            const int NUM_ROW = num_x > 33 ? 33 : num_x;

            for (int j = 0; j < NUM_ROW; j++) {
                ptr += sprintf(ptr, "%2d a: ", j);
                for (int i = 0; i < NUM_COL; i++) {
                    ptr += sprintf(ptr, "%02x", a[i + j * row_size]);
                    if (i % 4 == 3) { ptr[0] = ' '; ptr++; }
                }
                ptr[0] = '\n'; ptr++;
                ptr += sprintf(ptr, "   b: ");
                for (int i = 0; i < NUM_COL; i++) {
                    ptr += sprintf(ptr, "%02x", b[i + j * row_size]);
                    if (i % 4 == 3) { ptr[0] = ' '; ptr++; }
                }
                ptr[0] = '\n'; ptr++;
            }

            App_Log(APP_ERROR, "Values: \n%s\n", buffer);
            free(buffer);
            return -1;
        }
    }

    return 0;
}

//! Create a fresh RSF or XDF test file and write all records into it.
//! All params_real records are written first (ip1 = 1, 2, ...), then all
//! params_integer records (ip1 continues incrementing), and finally the
//! params_fail records are attempted; each must fail.
//! \return 0 on success, -1 if any write (or expected-fail) misbehaves
int create_file(const int is_rsf) {
    const char* test_filename = is_rsf ? test_filename_rsf : test_filename_xdf;
    remove(test_filename);

    const char* options = is_rsf ? "RSF+R/W" : "XDF+R/W";
    fst_file* test_file = fst24_open(test_filename, options);
    if (test_file == NULL) {
        App_Log(APP_ERROR, "Unable to open new test file with name %s and options %s\n", test_filename, options);
        return -1;
    }

    fst_record rec = default_fst_record;
    rec.ni = NUM_DATA_X;
    rec.nj = NUM_DATA_Y;
    rec.nk = NUM_DATA_Z;
    rec.dateo= 458021600;
    rec.deet = 300;
    rec.npas = 0;
    rec.ip1  = 0;
    rec.ip2  = 10;
    rec.ip3  = 100;
    strcpy(rec.typvar, "P");
    strcpy(rec.nomvar, "WAVE");
    strcpy(rec.etiket, "compression");
    strcpy(rec.grtyp, "X");
    rec.ig1   = 0;
    rec.ig2   = 0;
    rec.ig3   = 0;
    rec.ig4   = 0;

    App_Log(APP_ALWAYS, "%s: NUM_CASES = %d (%d real, %d integer, %d misc)\n",
            __func__, NUM_CASES_REAL + NUM_CASES_INTEGER + NUM_CASES_MISC,
            NUM_CASES_REAL, NUM_CASES_INTEGER, NUM_CASES_MISC);

    App_Log(APP_ALWAYS, "%s: Writing %d REAL records\n", __func__, NUM_CASES_REAL);
    for (int i = 0; i < NUM_CASES_REAL; i++) {
        rec.ip1++;
        rec.data  = *(params_real[i].data);
        rec.data_type = params_real[i].data_type;
        rec.data_bits = params_real[i].data_size;
        rec.pack_bits = params_real[i].pack_size;
        rec.nk    = params_real[i].nk;
        if (fst24_write(test_file, &rec, 0) <= 0) {
            App_Log(APP_ERROR, "%s: Could not write record to test file\n", __func__);
            fst24_record_print(&rec);
            return -1;
        }
    }

    App_Log(APP_ALWAYS, "%s: Writing %d INTEGER records\n", __func__, NUM_CASES_INTEGER);
    for (int i = 0; i < NUM_CASES_INTEGER; i++) {
        rec.ip1++;
        rec.data  = *(params_integer[i].data);
        rec.data_type = params_integer[i].data_type;
        rec.data_bits = params_integer[i].data_size;
        rec.pack_bits = params_integer[i].pack_size;
        rec.nk    = params_integer[i].nk;
        if (fst24_write(test_file, &rec, 0) <= 0) {
            App_Log(APP_ERROR, "%s: Could not write record to test file\n", __func__);
            fst24_record_print(&rec);
            return -1;
        }
    }

    App_Log(APP_ALWAYS, "%s: Writing %d MISC records\n", __func__, NUM_CASES_MISC);
    for (int i = 0; i < NUM_CASES_MISC; i++) {
        rec.ip1++;
        rec.data  = *(params_misc[i].data);
        rec.data_type = params_misc[i].data_type;
        rec.data_bits = params_misc[i].data_size;
        rec.pack_bits = params_misc[i].pack_size;
        rec.nk    = params_misc[i].nk;
        if (fst24_write(test_file, &rec, 0) <= 0) {
            App_Log(APP_ERROR, "%s: Could not write record to test file\n", __func__);
            fst24_record_print(&rec);
            return -1;
        }
    }

    App_Log(APP_ALWAYS, "%s: Expecting %d write failures\n", __func__, NUM_CASES_FAIL);
    for (int i = 0; i < NUM_CASES_FAIL; i++) {
        rec.ip1++;
        rec.data  = *(params_fail[i].data);
        rec.data_type = params_fail[i].data_type;
        rec.data_bits = params_fail[i].data_size;
        rec.pack_bits = params_fail[i].pack_size;
        rec.nk    = params_fail[i].nk;
        if (fst24_write(test_file, &rec, 0) > 0) {
            App_Log(APP_ERROR, "%s: Should not have been able to write fail record %d to file\n",
                    __func__, i);
            fst24_record_print(&rec);
            return -1;
        }
    }

    if (fst24_close(test_file) <= 0) {
        App_Log(APP_ERROR, "Unable to close new file %s\n", test_filename);
        return -1;
    }

    return 0;
}

//! Full round-trip test for one file format (RSF or XDF).
//! 1. Calls create_file() to write all records.
//! 2. Re-opens the file read-only and reads records back in order.
//! 3. Compares each real record against the original data using the
//!    tolerance from the corresponding params_real entry.
//! 4. Compares each integer record byte-for-byte (skipping 64-bit
//!    integers in XDF, which are not supported).
//! 5. Compares each misc record (binary/char/string/complex) byte-for-byte,
//!    using compare_nk for the number of vertical levels (complex has 2x).
//! \return 0 on success, -1 on any failure
int test_compression(const int is_rsf) {

    const char* test_filename = is_rsf ? test_filename_rsf : test_filename_xdf;

    if (create_file(is_rsf) < 0) {
        App_Log(APP_ERROR, "Problem when creating file for test\n");
        return -1;
    }

    fst_file* test_file = fst24_open(test_filename, "R/O");
    if (test_file == NULL) {
        App_Log(APP_ERROR, "Unable to open recently-created test file with name %s\n", test_filename);
        return -1;
    }

    fst_record rec_read = default_fst_record;
    fst_query* query = fst24_new_query(test_file, NULL, NULL);
    for (int i = 0; i < NUM_CASES_REAL; i++) {
        if (fst24_read_next(query, &rec_read) <= 0) {
            App_Log(APP_ERROR, "Unable to read record from file\n");
            return -1;
        }

        if (*(params_real[i].compare_data) == data_d) {
            if (compare_data_d(data_d, rec_read.data, NUM_DATA_X, NUM_DATA_Y, params_real[i].nk,
                            params_real[i].tol, params_real[i].max_tol)
                != 0)
            {
                App_Log(APP_ERROR, "%s: Data read is not the same (type %d, pack %d, size %d)\n",
                        __func__, rec_read.data_type, rec_read.pack_bits, rec_read.data_bits);
                return -1;
            }
        }
        else if(*(params_real[i].compare_data) == data_f) {
            if (compare_data_f(data_f, rec_read.data, NUM_DATA_X, NUM_DATA_Y, params_real[i].nk,
                            params_real[i].tol, params_real[i].max_tol)
                != 0)
            {
                App_Log(APP_ERROR, "%s: Data read is not the same (type %d, pack %d, size %d)\n",
                        __func__, rec_read.data_type, rec_read.pack_bits, rec_read.data_bits);
                return -1;
            }
        }
    }
    for (int i = 0; i < NUM_CASES_INTEGER; i++) {
        if (fst24_read_next(query, &rec_read) <= 0) {
            App_Log(APP_ERROR, "Unable to read record from file\n");
            return -1;
        }

        // App_Log(APP_INFO, "data_bits = %d, pack_bits = %d\n", rec_read.data_bits, rec_read.pack_bits);
        if (!is_rsf && rec_read.pack_bits == 64) {
            App_Log(APP_INFO, "%s: Skipping data check for 64-bit integer in XDF files\n", __func__);
            continue;
        }

        if (compare_data_bytes(*(params_integer[i].compare_data), rec_read.data, NUM_DATA_X, NUM_DATA_Y, params_integer[i].nk, rec_read.data_bits) != 0) {
            App_Log(APP_ERROR, "%s: Data read is not the same (type %d, pack %d, size %d)\n",
                    __func__, rec_read.data_type, rec_read.pack_bits, rec_read.data_bits);
            return -1;
        }
    }
    for (int i = 0; i < NUM_CASES_MISC; i++) {
        if (fst24_read_next(query, &rec_read) <= 0) {
            App_Log(APP_ERROR, "Unable to read record from file\n");
            return -1;
        }

        // All misc types round-trip exactly. Complex read-back has twice the
        // elements (interleaved real/imaginary), so use compare_nk for num_z.
        // CHAR read-back reports data_bits = 32 (4 chars per 32-bit word), so
        // use compare_bits (8) for the element size when it is set.
        const int cmp_nk = params_misc[i].compare_nk ? params_misc[i].compare_nk : params_misc[i].nk;
        const int cmp_bits = params_misc[i].compare_bits ? params_misc[i].compare_bits : rec_read.data_bits;
        if (compare_data_bytes(*(params_misc[i].compare_data), rec_read.data, NUM_DATA_X, NUM_DATA_Y, cmp_nk, cmp_bits) != 0) {
            App_Log(APP_ERROR, "%s: Data read is not the same (type %d, pack %d, size %d)\n",
                    __func__, rec_read.data_type, rec_read.pack_bits, rec_read.data_bits);
            return -1;
        }
    }


    // fst24_record_free(&rec);
    fst24_record_free(&rec_read);
    fst24_query_free(query);

    if (fst24_close(test_file) <= 0) {
        App_Log(APP_ERROR, "Unable to close new file %s\n", test_filename);
        return -1;
    }

    return 0;
}

//! Entry point: generate data, run the full round-trip test for RSF, then
//! for XDF, and report success.
//! \return 0 on success, -1 on failure
int main(void) {

    make_data();

    App_Log(APP_ALWAYS, "Testing RSF\n");
    if (test_compression(1) != 0) return -1;

    App_Log(APP_ALWAYS, "Testing XDF\n");
    if (test_compression(0) != 0) return -1;

    free(data_f);
    free(data_d);
    free(data_i8);
    free(data_i16);
    free(data_i);
    free(data_l);
    free(data_umask);
    free(data_ull);
    free(data_u8);
    free(data_u16);
    free(data_u32);
    free(data_c);
    free(data_cd);

    App_Log(APP_ALWAYS, "Test successful\n");

    return 0;
}
