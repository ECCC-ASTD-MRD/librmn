#include "fst24_record_internal.h"

#include <ctype.h>
#include <string.h>

#include <App.h>
#include "rmn/Meta.h"
#include "rmn/convert_ip.h"
#include "rsf_internal.h"

//! List of field/grid descriptor records
static const char *FST_DESCRIPTOR[]={ ">>","^^","^>","!!","##","#>>#","#^^#","####","HY","PROJ","MTRX",NULL };

const char** fst24_record_get_descriptors(void) {
   return(FST_DESCRIPTOR);
}

//! Check if an fst_record is a field/reference descriptor
//! \return TRUE (1) if it is, FALSE (0) otherwise
int32_t fst24_record_is_descriptor(const fst_record* const record) {

    const char *desc;
    int         d=0;

    while((desc=FST_DESCRIPTOR[d++])) {
        if (!strncmp(record->nomvar,desc,FST_NOMVAR_LEN)) {
            return(1);
        }
    }

   return(0);
}

//! Check if two fst_record structs point to the exact same record, in the same file
//! \return TRUE (1) if they are the same, FALSE (0) if not or if they do not point to any specific record in a file
int32_t fst24_record_is_same(const fst_record* const a, const fst_record* const b) {
    if (!fst24_record_is_valid(a) || a->do_not_touch.handle < 0 ||
        !fst24_record_is_valid(b) || b->do_not_touch.handle < 0)
        return 0;
    return (a->do_not_touch.handle == b->do_not_touch.handle);
}

//! Creates a new record and assign the data pointer or allocate data memory
//! \return new record
fst_record* fst24_record_new(
    void   *data,   //!< Data pointer to assign, or allocate internal array if NULL
    int32_t type,   //!< Data type
    int32_t nbits,  //!< Number of bits per data element
    int32_t ni,     //!< I horizontal size
    int32_t nj,     //!< J horizontal size
    int32_t nk      //!< K vertical size
) {
    fst_record* result = (fst_record*)malloc(sizeof(fst_record));

    if (result) {
        memcpy(result, &default_fst_record, sizeof(fst_record));

        result->ni = ni;
        result->nj = nj;
        result->nk = nk;
        result->data_bits = nbits;
        result->data_type = type;
        result->do_not_touch.alloc = fst24_record_data_size(result);
        result->data = data;
        if (data == NULL) {
            // No data pointer passed, allocate data array
            if (!(result->data = (void*)malloc(result->do_not_touch.alloc))) {
                Lib_Log(APP_LIBFST, APP_ERROR, "%s: Unable to allocate record data (%ix%ix%i)\n", __func__,ni,nj,nk);
            }
        } else {
          // Using an assigned pointer
         result->do_not_touch.flags = FST_REC_ASSIGNED;
        }
    } else {
        Lib_Log(APP_LIBFST, APP_ERROR, "%s: Unable to allocate record\n",__func__);
    }
    return(result);
}

//! Free a record. Its data will only be freed if it is *not* managed by the user.
//! \return TRUE (1) if no error, FALSE (0) if an error is detected
int32_t fst24_record_free(
    fst_record* record      //!< [in] record pointer
) {

    if ((record->data != NULL) && (record->do_not_touch.alloc > 0)) {
        record->do_not_touch.alloc = 0;
        if (!(record->do_not_touch.flags & FST_REC_ASSIGNED))
            free(record->data);
        record->data = NULL; 
    }
    Meta_Free(record->metadata);
    record->metadata = NULL;
    return(TRUE);
}

//! Print all members of the given record struct
void fst24_record_print(const fst_record* record) {
    Lib_Log(APP_LIBFST, APP_ALWAYS,
        "\n"
        "  Version: %ld\n"
        "  File: 0x%x\n"
        "  Data: 0x%x\n"
        "  Metadata: 0x%x\n"
        "  Flags:  %ld\n"
        "  Alloc:  %ld\n"
        "  Handle: 0x%x\n"
        "  dateo: %d\n"
        "  datev: %d\n"
        "  data_type: %d\n"
        "  data_bits: %d\n"
        "  pack_bits: %d\n"
        "  ni x nj x nk: %d x %d x %d (%d) elements\n"
        "  metadata size: %d bytes\n"
        "  deet: %d, npas: %d\n"
        "  ip1-3: %d, %d, %d\n"
        "  ig1-4: %d, %d, %d, %d\n"
        "  typvar: \"%s\"\n"
        "  grtyp:  \"%s\"\n"
        "  nomvar: \"%s\"\n"
        "  etiket: \"%s\"\n",
        record->do_not_touch.version, record->file, record->data, record->metadata, record->do_not_touch.flags,
        record->do_not_touch.alloc, record->do_not_touch.handle, 
        record->dateo, record->datev, record->data_type, record->data_bits, record->pack_bits,
        record->ni, record->nj, record->nk, record->ni * record->nj * record->nk,
        record->num_meta_bytes,
        record->deet, record->npas, record->ip1, record->ip2, record->ip3,
        record->ig1, record->ig2, record->ig3, record->ig4,
        record->typvar, record->grtyp, record->nomvar, record->etiket
    );
}

//! Pad the given string buffer with spaces. See \ref fst24_record_print_short
//! \return pointer to the buffer
char* pad(
    char* buffer,               //!< [in,out] String we want to pad
    const int num_written,      //!< From what offset to start the padding
    const size_t target_length  //!< Final desired length of the string
) {
    if (num_written >= 0 && (target_length > (size_t)num_written)) {
        memset(buffer + num_written, ' ', target_length - num_written);
        buffer[target_length] = '\0';
    }

    return buffer;
}

//! Add a character string to the line to be printed. If the string is shorter than
//! the requested length, it will be padded with spaces. See \ref fst24_record_print_short
//! \return Pointer to after the (new) last character of the line to be printed
char* add_str(
    char* buffer,       //!< [in,out] buffer containing the line to be printed
    const char* string, //!< [in] The string we are adding to the line
    const size_t length //!< [in] The width (in number of characters) that will be added to the line.
) {
    const char format[10] = {' ', '%', '0' + length / 10, '0' + length % 10, 's', '\0'};
    const int num_written = snprintf(buffer, length + 2, format, string);
    pad(buffer, num_written, length + 1);
    return buffer + length + 1;
}

//! Add an integer to the line to be printed. See \ref fst24_record_print_short.
//! \return Pointer to after the (new) last character of the line to be printed
char* add_int(
    char* buffer,           //!< [in,out] Buffer containing the line to be printed
    const int64_t num,      //!< Value that we want to print
    const size_t length,    //!< Width (in characters) that we want to add to the line
    const int with_zeros    //!< Whether we want the printed number to be left-padded with zeros
) {
    if (length > 10 || with_zeros) {
        const char format[10]   = {' ', '%', '0' + length / 10, '0' + length % 10, 'l', 'd', '\0'};
        snprintf(buffer, length + 2, format, num);
    }
    else {
        const char format[10] = {' ', '%', '0' + length, 'l', 'd', '\0'};
        snprintf(buffer, length + 2, format, num);
    }

    return buffer + length + 1;
}

//! Add a date (from a timestamp) to the line to be printed. See fst24_record_print_short.
//! \return Pointer to after the (new) last character of the line to be printed
char* add_date(
    char* buffer,           //!< [in,out] Buffer containing the line to be printed
    const int64_t datestamp //!< Timestamp to be translated and added to the line
) {
    char* result = buffer;
    int32_t dat2, dat3;
    int32_t minus3 = -3;
    int32_t small_date = datestamp;
    newdate_c(&small_date, &dat2, &dat3, &minus3);

    result = add_int(result, dat2, 8, 1);
    result = add_int(result, dat3 / 100, 6, 1);
    result = add_str(result, "", 0);
    return result;
}

//! A a "level" (?) to the line to be printed. If the variable cannot be
//! correctly translated, only dashes will be printed.
//! See \ref fst24_record_print_short.
//! \return Pointer to after the (new) last character of the line to be printed
char* add_level(
    char* buffer,           //!< [in,out] Buffer containing the line to be printed
    const char* varname,    //!< [in] Name of the variable for which we are printing the level
    const int32_t ip1,      //!< IP1 associated with the record
    const size_t length     //!< Width of the text we are adding. Will pad with spaces if necessary
) {

    if (!FstCanTranslateName(varname)) return add_str(buffer, "---", length);

    // good old level option
    char c_level[16];
    // char v_level[16];
    int mode = -1;
    int flag = 1;
    int32_t ip1_f = ip1;
    float level;
    int kind, posc, posv;
    f77name(convip_plus)(&ip1_f, &level, &kind, &mode, c_level, &flag, (F2Cl) 15);
    c_level[15] = '\0';
    /* blank initialisation */
    snprintf(buffer, length + 2, "%s", "                               ");
    posc = 14;
    posv = 14;
    /* skip blanks and right justify string */
    while ((posc >= 0) && (isspace(c_level[posc]))) {
        posc--;
    }
    if (isdigit(c_level[posc])) {
        posv -= 3;
    }
    while ((posv >= 0) && (posc >= 0)) {
        buffer[posv] = c_level[posc];
        posv--;
        posc--;
    }

    return buffer + length + 1;
}

//! Add the 3 IPs to the line to be printed, decoded if possible. See \ref fst24_record_print_short.
//! \return Pointer to after the (new) last character of the line to be printed
char* add_ips(
    char* buffer,           //!< [in,out] Buffer containing the line to be printed
    const char* varname,    //!< Name of the corresponding variable
    const int32_t ip1,
    const int32_t ip2,
    const int32_t ip3,
    const size_t length     //!< Width of the text we are adding. Will pad with spaces if necessary
) {
    if (!FstCanTranslateName(varname)) {
        char* result = buffer;
        result = add_int(result, ip1, 10, 0);
        result = add_int(result, ip2, 10, 0);
        result = add_int(result, ip3, 10, 0);
        return result;
    }

    // full IP1/IP2/IP3 triplet decoding
    float p1, p2, p3;
    int kind1, kind2, kind3;
    int StatusIP = ConvertIPtoPK(&p1, &kind1, &p2, &kind2, &p3, &kind3, ip1, ip2, ip3);
    if (kind1 < 0 || kind2 < 0 || kind3 < 0 || (StatusIP & CONVERT_ERROR) ) {
        /* decode error somewhere */
        kind1 = 15; kind2 = 15; kind3 = 15;  /* integer code P = IP */
        p1 = ip1; p2 = ip2; p3 = ip3;
    }
    kind1 &= 0x1F; kind2 &= 0x1F; kind3 &= 0x1F;   /* force modulo 32 */
    const int num_written = snprintf(buffer, length + 2, " %10g%s %10g%s %10g%s", p1, kinds(kind1), p2, kinds(kind2), p3, kinds(kind3));
    pad(buffer, num_written, length + 1);

    return buffer + length + 1;
}

//! Add a datatype to the line we are printing. See \ref fst24_record_print_short.
//! \return Pointer to after the (new) last character of the line to be printed
char* add_datatype(
    char* buffer,           //!< [in,out] Buffer containing the line to be printed
    const int32_t datatype, //!< ID of the datatype
    const int32_t nbits_compressed,   //!< Size in bits of the type (compressed)
    const int32_t nbits_uncompressed, //!< Size in bits of the type (uncompressed)
    const size_t length     //!< Width of the text we are adding. Will pad with spaces if necessary
) {
    const char m = has_type_missing(datatype) ? 'm' : ' ';
    const char cdt[9] = {'X', 'R', 'I', 'C', 'S', 'E', 'F', 'A', 'Z'};
    const char letter = cdt[datatype & 0x3f]; // Suppress bits 64 and 128
    const char datatype_c = is_type_turbopack(datatype) ? tolower(letter) : letter;

    const int num_written = snprintf(buffer, 10, " %1c%1c%2d %3d", datatype_c, m, nbits_compressed, nbits_uncompressed);
    pad(buffer, num_written, length + 1);

    return buffer + length + 1;
}

//! Add grid information to the line we are printing. See \ref fst24_record_print_short.
//! \return Pointer to after the (new) last character of the line to be printed
char* add_grid_info(
    char* buffer,       //!< [in,out] Buffer containing the line to be printed
    const char* grtyp,
    const int32_t ig1,
    const int32_t ig2,
    const int32_t ig3,
    const int32_t ig4,
    const size_t length //!< Width of the text we are adding. Will pad with spaces if necessary
) {
    char pg1[7], pg2[7], pg3[8], pg4[8];
    igapg_c(grtyp, pg1, pg2, pg3, pg4, &ig1, &ig2, &ig3, &ig4);
    pg1[6] = '\0'; pg2[6] = '\0'; pg3[7] = '\0'; pg4[7] = '\0';
    const int num_written = snprintf(buffer, 33, " %1s %6s %6s %7s %7s", grtyp, pg1, pg2, pg3, pg4);
    pad(buffer, num_written, length + 1);

    return buffer + length + 1;
}

//! Add the 4 IGs to the line we are printing. See \ref fst24_record_print_short.
//! \return Pointer to after the (new) last character of the line to be printed
char* add_ig1234(
    char* buffer,       //!< [in,out] Buffer containing the line to be printed
    const char* grtyp,
    const int32_t ig1,
    const int32_t ig2,
    const int32_t ig3,
    const int32_t ig4,
    const size_t length //!< Width of the text we are adding. Will pad with spaces if necessary
) {
    char* start = buffer;
    buffer = add_str(buffer, grtyp, 1);
    buffer = add_int(buffer, ig1, 5, 0);
    buffer = add_int(buffer, ig2, 5, 0);
    buffer = add_int(buffer, ig3, 5, 0);
    buffer = add_int(buffer, ig4, 5, 0);

    if ((size_t)(buffer - start) < length) buffer += snprintf(buffer, length - (buffer - start), "                       ");

    return buffer;
}

//! Print record information on one line (with an optional header)
void fst24_record_print_short(
    const fst_record* const record, //!< [in] The record we want to print
    const fst_record_fields* const fields, //!< [in,optional] What fields should we print (default choice if NULL)
    const int print_header, //!< [in] Print header if not 0
    const char* const prefix //!< [in,optional] Line prefix
) {
    const fst_record_fields width = (fst_record_fields) {
        .nomvar = 4,
        .typvar = 2,
        .etiket = 12,

        .deet = 8,
        .npas = 8,

        .nijk = 9,

        .ip1 = 9,
        .ip2 = 9,
        .ip3 = 9,
        .decoded_ip = 38,

        .dateo = 16,
        .datev = 16,
        .datestamps = 9,
        .level = 15,

        .data_type = 8,
        .grid_info = 31,
        .ig1234 = 25
    };

    fst_record_fields to_print = default_fields;
    if (fields != NULL) to_print = *fields;

    char buffer[2048];
    buffer[0]='\0';
    if (print_header) {
        char* current = buffer;

        if (prefix != NULL) current = add_str(current, "", strlen(prefix));
        if (to_print.nomvar > 0) current = add_str(current, "NOMV", width.nomvar);
        if (to_print.typvar > 0) current = add_str(current, "TV", width.typvar);
        if (to_print.etiket > 0) current = add_str(current, "ETIQUETTE   ", width.etiket);
        if (to_print.nijk > 0) {
            current = add_str(current, "NI", width.nijk);
            current = add_str(current, "NJ", width.nijk);
            current = add_str(current, "NK", width.nijk);
        }
        if (to_print.dateo > 0 && to_print.datestamps > 0)  current = add_str(current, "DATE-O", width.datestamps);
        if (to_print.dateo > 0 && to_print.datestamps <= 0) current = add_str(current, "(DATE-O  h m s) ", width.dateo);
        if (to_print.datev > 0 && to_print.datestamps > 0)  current = add_str(current, "DATE-V", width.datestamps);
        if (to_print.datev > 0 && to_print.datestamps <= 0) current = add_str(current, "(DATE-V  h m s) ", width.datev);

        if (to_print.level > 0) current = add_str(current, "LEVEL", width.level);
        if (to_print.decoded_ip > 0) current = add_str(current, " ------------- IPs -------------    ", width.decoded_ip);

        if (to_print.ip1) current = add_str(current, "IP1", width.ip1);
        if (to_print.ip2) current = add_str(current, "IP2", width.ip2);
        if (to_print.ip3) current = add_str(current, "IP3", width.ip3);

        if (to_print.deet) current = add_str(current, "DEET", width.deet);
        if (to_print.npas) current = add_str(current, "NPAS", width.npas);

        if (to_print.data_type) current = add_str(current, "DTYP SIZ", width.data_type);

        if (to_print.grid_info) current = add_str(current, "G    XG1    XG2     XG3     XG4", width.grid_info);
        else if (to_print.ig1234) current = add_str(current, "G   IG1   IG2   IG3   IG4", width.ig1234);

        Lib_Log(APP_LIBFST, APP_VERBATIM, "%s\n\n", buffer);
    }

    char* current = buffer;
    if (prefix != NULL) current = add_str(current, prefix, strlen(prefix));
    if (to_print.nomvar > 0) current = add_str(current, record->nomvar, width.nomvar);
    if (to_print.typvar > 0) current = add_str(current, record->typvar, width.typvar);
    if (to_print.etiket > 0) current = add_str(current, record->etiket, width.etiket);
    if (to_print.nijk > 0) {
        current = add_int(current, record->ni, width.nijk, 0);
        current = add_int(current, record->nj, width.nijk, 0);
        current = add_int(current, record->nk, width.nijk, 0);
    }
    if (to_print.dateo > 0 && to_print.datestamps > 0)  current = add_int(current, record->dateo, width.datestamps, 1);
    if (to_print.dateo > 0 && to_print.datestamps <= 0) current = add_date(current, record->dateo);
    if (to_print.datev > 0 && to_print.datestamps > 0)  current = add_int(current, record->datev, width.datestamps, 1);
    if (to_print.datev > 0 && to_print.datestamps <= 0) current = add_date(current, record->datev);

    if (to_print.level > 0) current = add_level(current, record->nomvar, record->ip1, width.level);
    if (to_print.decoded_ip > 0) current = add_ips(current, record->nomvar, record->ip1, record->ip2, record->ip3, width.decoded_ip);

    if (to_print.ip1) current = add_int(current, record->ip1, width.ip1, 0);
    if (to_print.ip2) current = add_int(current, record->ip2, width.ip2, 0);
    if (to_print.ip3) current = add_int(current, record->ip3, width.ip3, 0);

    if (to_print.deet) current = add_int(current, record->deet, width.deet, 0);
    if (to_print.npas) current = add_int(current, record->npas, width.npas, 0);

    if (to_print.data_type) current = add_datatype(current, record->data_type, record->pack_bits, record->data_bits, width.data_type);

    if (to_print.grid_info) current = add_grid_info(current, record->grtyp, record->ig1, record->ig2,
                                                    record->ig3, record->ig4, width.grid_info);
    else if (to_print.ig1234) current = add_ig1234(current, record->grtyp, record->ig1, record->ig2,
                                                    record->ig3, record->ig4, width.ig1234);

    Lib_Log(APP_LIBFST, APP_VERBATIM, "%s\n", buffer);

    if (to_print.metadata > 0) {
        fst24_read_metadata((fst_record*)record);
        Lib_Log(APP_LIBFST, APP_VERBATIM, "%s\n", Meta_Stringify(record->metadata,JSON_C_TO_STRING_SPACED));
    }

}

//! Check whether the given record struct is valid (does not check its content, just its version token)
int32_t fst24_record_is_valid(const fst_record* record) {
    if (record == NULL) {
        Lib_Log(APP_LIBFST, APP_DEBUG, "%s: Record pointer is NULL\n", __func__);
        return 0;
    }
    
    if (record->do_not_touch.version != default_fst_record.do_not_touch.version) {
        Lib_Log(APP_LIBFST, APP_DEBUG, "%s: Version number (%ld) is wrong (should be %ld)! This means that either:\n"
                "  - The given address does not point to an initialized fst_record struct\n"
                "  - Your program was not compiled with the same version it is linked to\n",
                __func__, record->do_not_touch.version, default_fst_record.do_not_touch.version);
        return 0;
    }

    return 1;
}

int32_t is_valid_int_param(const int32_t val, const int32_t min, const int32_t max, const char* name) {
    if (val < min || val  > max) {
        Lib_Log(APP_LIBFST, APP_ERROR, "%s: %s = %d, but must be between %d and %d\n",
                __func__, name, val, min, max);
        return 0;
    }

    return 1;
}

//! Validate the parameters of the given record (RSF only).
//! Will exit with an error if they are not valid.
int32_t fst24_record_validate_params(const fst_record* record) {
    int32_t all_good = 1;

    if (record->data == NULL) {
        Lib_Log(APP_LIBFST, APP_ERROR, "%s: Record has no data\n", __func__);
        all_good = 0;
    }

    if (!is_valid_int_param(record->ni, 1, NI_MAX, "ni")) all_good = 0;
    if (!is_valid_int_param(record->nj, 1, NJ_MAX, "nj")) all_good = 0;
    if (!is_valid_int_param(record->nk, 1, NK_MAX, "nk")) all_good = 0;
    if (!is_valid_int_param(record->deet, 0, DEET_MAX, "deet")) all_good = 0;
    if (!is_valid_int_param(record->npas, 0, NPAS_MAX, "npas")) all_good = 0;
    if (!is_valid_int_param(record->ig1, 0, IG1_MAX, "ig1")) all_good = 0;
    if (!is_valid_int_param(record->ig2, 0, IG2_MAX, "ig2")) all_good = 0;
    if (!is_valid_int_param(record->ig3, 0, IG3_MAX, "ig3")) all_good = 0;
    if (!is_valid_int_param(record->ig4, 0, IG4_MAX, "ig4")) all_good = 0;
    if (!is_valid_int_param(record->ip1, 0, IP1_MAX, "ip1")) all_good = 0;
    if (!is_valid_int_param(record->ip2, 0, IP2_MAX, "ip2")) all_good = 0;
    if (!is_valid_int_param(record->ip3, 0, IP3_MAX, "ip3")) all_good = 0;
    if (!is_valid_int_param(record->data_bits, 1, 64, "data_bits")) all_good = 0;
    if (!is_valid_int_param(record->pack_bits, 1, 64, "pack_bits")) all_good = 0;

    const int32_t valid_types[] = { FST_TYPE_BINARY,
                                    FST_TYPE_CHAR,
                                    FST_TYPE_STRING,
                                    FST_TYPE_SIGNED,
                                    FST_TYPE_UNSIGNED,
                                    FST_TYPE_REAL,
                                    FST_TYPE_REAL_IEEE,
                                    FST_TYPE_REAL_OLD_QUANT,
                                    FST_TYPE_COMPLEX };
    const int32_t base_type = base_fst_type(record->data_type);
    int32_t ok_type = 0;
    for (unsigned int i = 0; i < sizeof(valid_types) / sizeof(int32_t); i++) {
        if (base_type == valid_types[i]) {
            ok_type = 1; break;
        }
    }
    if (!ok_type) {
        Lib_Log(APP_LIBFST, APP_ERROR, "%s: Unrecognized data type in record\n", __func__);
        all_good = 0;
    }

    if (!all_good) return -1;

    return 0;
}

static inline void get_all_ips(const int initial_ip, int* new_ip, int* old_ip) {
    int ip = initial_ip;
    float val;
    int kind;
    ConvertIp(&ip, &val, &kind, -1); // Retrieve value
    ConvertIp(new_ip, &val, &kind, 2); // Encode with new encoding
    *old_ip = -9999;
    if (kind < 4) {
        ConvertIp(old_ip, &val, &kind, 3); // Encode with old encoding
    }
}

//! Encode the information of the given record into a directory metadata struct, that
//! will directly be used for searching in a file
void make_search_criteria(
    const fst_record* record,   //!< [in] The information we are looking for
    fst_query* const query      //!< [in,out] Query for which we are making the criteria
) {
    // Check for error
    if (!fst24_record_is_valid(record) || (query == NULL)) {
        Lib_Log(APP_LIBFST, APP_ERROR, "%s: Invalid record or query\n", __func__);
        return;
    }

    search_metadata* criteria = &(query->criteria);
    search_metadata* mask     = &(query->mask);

    // Reset search mask
    memset(mask, 0xff, sizeof(search_metadata));

    // fst98 criteria
    {
        stdf_dir_keys* fst98_meta = &criteria->fst98_meta;
        stdf_dir_keys* fst98_mask = &mask->fst98_meta;

        fst98_mask->pad1 = 0;
        fst98_mask->pad2 = 0;
        fst98_mask->pad3 = 0;
        fst98_mask->pad5 = 0;
        fst98_mask->pad6 = 0;
        fst98_mask->pad7 = 0;
        fst98_mask->deleted = 0;
        fst98_mask->select = 0;
        fst98_mask->lng = 0;
        fst98_mask->addr = 0;
        fst98_mask->ubc = 0;
        fst98_mask->levtyp = 0;

        if (record->dateo != default_fst_record.dateo) {
            Lib_Log(APP_LIBFST, APP_DEBUG, "%s: Searching by origin date (dateo) has not been implemented yet\n", __func__);
        }

        fst98_meta->date_stamp = stamp_from_date(record->datev);
        if (!query->options.stamp_norun) fst98_mask->date_stamp &= ~(0x7);
        if (record->datev == default_fst_record.datev) fst98_mask->date_stamp = 0;

        fst98_meta->ni = record->ni;
        if ((record->ni == default_fst_record.ni)) fst98_mask->ni = 0;

        fst98_meta->nj = record->nj;
        if ((record->nj == default_fst_record.nj)) fst98_mask->nj = 0;

        fst98_meta->nk = record->nk;
        if ((record->nk == default_fst_record.nk)) fst98_mask->nk = 0;

        fst98_meta->datyp = record->data_type;
        if (record->data_type == default_fst_record.data_type) fst98_mask->datyp = 0;

        fst98_meta->dasiz = record->data_bits;
        if (record->data_bits == default_fst_record.data_bits) fst98_mask->dasiz = 0;

        fst98_meta->nbits = abs(record->pack_bits);
        if (record->pack_bits == default_fst_record.pack_bits) fst98_mask->nbits = 0;

        fst98_meta->npas = record->npas;
        if ((record->npas == default_fst_record.npas)) fst98_mask->npas = 0;

        fst98_meta->deet = record->deet;
        if ((record->deet == default_fst_record.deet)) fst98_mask->deet = 0;

        // IPs
        {
            fst98_meta->ip1 = record->ip1;
            if ((record->ip1 == default_fst_record.ip1)) fst98_mask->ip1 = 0;

            fst98_meta->ip2 = record->ip2;
            if ((record->ip2 == default_fst_record.ip2)) fst98_mask->ip2 = 0;

            fst98_meta->ip3 = record->ip3;
            if ((record->ip3 == default_fst_record.ip3)) fst98_mask->ip3 = 0;

            if (query->options.ip1_all > 0) {
                get_all_ips(record->ip1, &query->ip1s[0], &query->ip1s[1]);
                fst98_mask->ip1 = 0;
            }
            if (query->options.ip2_all > 0) {
                get_all_ips(record->ip2, &query->ip2s[0], &query->ip2s[1]);
                fst98_mask->ip2 = 0;
            }
            if (query->options.ip3_all > 0) {
                get_all_ips(record->ip3, &query->ip3s[0], &query->ip3s[1]);
                fst98_mask->ip3 = 0;
            }
        }

        char nomvar[FST_NOMVAR_LEN];
        copy_record_string(nomvar, record->nomvar, FST_NOMVAR_LEN);
        fst98_meta->nomvar =
            (ascii6(nomvar[0]) << 18) |
            (ascii6(nomvar[1]) << 12) |
            (ascii6(nomvar[2]) <<  6) |
            (ascii6(nomvar[3]));
        if (fst98_meta->nomvar == 0) fst98_mask->nomvar = 0;

        char typvar[FST_TYPVAR_LEN];
        copy_record_string(typvar, record->typvar, FST_TYPVAR_LEN);
        fst98_meta->typvar =
            (ascii6(typvar[0]) << 6) |
            (ascii6(typvar[1]));
        if (fst98_meta->typvar == 0) fst98_mask->typvar = 0;

        char etiket[FST_ETIKET_LEN];
        copy_record_string(etiket, record->etiket, FST_ETIKET_LEN);
        fst98_meta->etik15 =
            (ascii6(etiket[0]) << 24) |
            (ascii6(etiket[1]) << 18) |
            (ascii6(etiket[2]) << 12) |
            (ascii6(etiket[3]) <<  6) |
            (ascii6(etiket[4]));

        fst98_meta->etik6a =
            (ascii6(etiket[5]) << 24) |
            (ascii6(etiket[6]) << 18) |
            (ascii6(etiket[7]) << 12) |
            (ascii6(etiket[8]) <<  6) |
            (ascii6(etiket[9]));

        fst98_meta->etikbc =
            (ascii6(etiket[10]) <<  6) |
            (ascii6(etiket[11]));

        if ((fst98_meta->etik15 == 0) && (fst98_meta->etik6a == 0)) {
            fst98_mask->etik15 = 0;
            fst98_mask->etik6a = 0;
            fst98_mask->etikbc = 0;
        }

        fst98_meta->ig4  = record->ig4;
        fst98_meta->ig2a = record->ig2 >> 16;
        fst98_meta->ig1  = record->ig1;
        fst98_meta->ig2b = record->ig2 >> 8;
        fst98_meta->ig3  = record->ig3;
        fst98_meta->ig2c = record->ig2 & 0xff;
        if (record->ig1 == default_fst_record.ig1) fst98_mask->ig1 = 0;
        if (record->ig2 == default_fst_record.ig2) fst98_mask->ig2a =  fst98_mask->ig2b =  fst98_mask->ig2c = 0;
        if (record->ig3 == default_fst_record.ig3) fst98_mask->ig3 = 0;
        if (record->ig4 == default_fst_record.ig4) fst98_mask->ig4 = 0;

        char grtyp[FST_GTYP_LEN];
        copy_record_string(grtyp, record->grtyp, FST_GTYP_LEN);
        fst98_meta->gtyp = grtyp[0];
        if (grtyp[0] == ' ') fst98_mask->gtyp = 0;
    } // fst98 criteria
}

//! Update fst_record attributes from the given search metadata. The data and allocation status will remain untouched
void fill_with_search_meta(
    fst_record* record,             //!< [in,out] The record we want to update
    const search_metadata* meta,    //!< The packed searched metadata
    const fst_file_type type        //!< File type (RSF, XDF, other?)
) {

    record->do_not_touch.fst_version = default_fst_record.do_not_touch.fst_version;
    record->do_not_touch.num_search_keys = sizeof(stdf_dir_keys) / sizeof(uint32_t); // Default for XDF
    record->do_not_touch.extended_meta_size = 0;

    const stdf_dir_keys* fst98_meta = &(meta->fst98_meta); // Default for XDF

    if (type != FST_XDF) {
        uint8_t rsf_version;
        rsf_rec_class dummy_rc;
        rsf_rec_type dummy_rt;
        extract_meta0(meta->rsf_reserved[0], &rsf_version, &dummy_rc, &dummy_rt);

        uint32_t reserved0 = meta->fst24_reserved[0];

        if (rsf_version == 0) {
            // RSF version 0 record structure (FST version can only be 0, in that case)
            search_metadata_version_0_0* meta_old = (search_metadata_version_0_0*)meta;
            reserved0 = meta_old->fst24_reserved[0];
            fst98_meta = &(meta_old->fst98_meta);
        }

        decode_fst24_reserved_0(reserved0,
                                &record->do_not_touch.fst_version,
                                &record->do_not_touch.num_search_keys,
                                &record->do_not_touch.extended_meta_size);
    }

    if (record->do_not_touch.fst_version > FST24_VERSION_COUNT) {
        Lib_Log(APP_LIBFST, APP_FATAL, "%s: Interpreting search metadata with a library from an earlier FST version (%d)"
                " than the one used to write the record (%d). We might run into issues (or not).\n",
                __func__, FST24_VERSION_COUNT, record->do_not_touch.fst_version);

        fst_record_copy_info(record, NULL);
        return;
    }

    // fst98 metadata
    {
        stdf_special_parms cracked;
        crack_std_parms(fst98_meta, &cracked);

        record->dateo = cracked.date_stamp;
        record->datev = cracked.date_valid;

        record->ni = fst98_meta->ni;
        record->nj = fst98_meta->nj;
        record->nk = fst98_meta->nk;
        record->data_type = fst98_meta->datyp;
        record->data_bits = fst98_meta->dasiz;
        record->pack_bits = fst98_meta->nbits;

        if (record->data_bits == 0) {
            if (record->pack_bits > 32) {
                record->data_bits = 64;
            } else {
                record->data_bits = 32;
            }
        }
        
        // Sanity check
        if (record->pack_bits > record->data_bits) {
            Lib_Log(APP_LIBFST, APP_WARNING,
                "%s: pack_bits (%d) is greater than data_bits (%d). This might cause issues\n",
                __func__, record->pack_bits, record->data_bits);
        }

        record->deet = fst98_meta->deet;
        record->npas = fst98_meta->npas;

        strncpy(record->nomvar, cracked.nomvar, FST_NOMVAR_LEN);
        strncpy(record->typvar, cracked.typvar, FST_TYPVAR_LEN);
        strncpy(record->etiket, cracked.etiket, FST_ETIKET_LEN);
        strncpy(record->grtyp,  cracked.gtyp,   FST_GTYP_LEN);

        record->ip1 = fst98_meta->ip1;
        record->ip2 = fst98_meta->ip2;
        record->ip3 = fst98_meta->ip3;

        record->ig1 = fst98_meta->ig1;
        record->ig2 = cracked.ig2;
        record->ig3 = fst98_meta->ig3;
        record->ig4 = fst98_meta->ig4;
    } // fst98 metadata

    record->do_not_touch.unpacked_data_size = fst24_record_data_size(record) / sizeof(uint32_t);

    // Here, we implement reading content for next-generation FST (anything that goes beyond the stdf_dir_keys struct)

    Meta_Free(record->metadata);
    record->metadata = NULL;
    record->do_not_touch.stringified_meta = NULL;

    if (record->do_not_touch.fst_version <= 0) {
        // Nothing in particular
    }
    else if (record->do_not_touch.fst_version <= 1) {
        if (record->do_not_touch.extended_meta_size > 0) {
            // Right after the search keys
            record->do_not_touch.stringified_meta = (uint32_t *)meta + record->do_not_touch.num_search_keys;
        }
    }
    else {
        Lib_Log(APP_LIBFST, APP_FATAL, "%s: Gotta decide what to do with new metadata in FST version number %d\n",
                __func__, record->do_not_touch.fst_version);
    }
}

//! Copy the legacy metadata and extended metadata
//!   FST_META_ALL  : All meta data
//!   FST_META_TIME : Time related metadata (dateo,datev,deet,npas)
//!   FST_META_GRID : Grid related metadata (grtyp,ig1-4)
//!   FST_META_INFO : Variable metadata (nomvar,typvar,etiket,ip1-3)
//!   FST_META_SIZE : Data type and size related metadata (data_type,data_bits,pack_bits,ni,nj,nk)
//!   FST_META_EXT  : Extended metadata
//! \return 1 if we were able to do the copy,
//!         0 otherwise
int32_t fst24_record_copy_metadata(
    fst_record* dest,            //!< Destination record
    const fst_record* src,      //!< Source record
    const int what            //!< select which part of the metadata to copy (default: FST_META_ALL) thay can be combined with + (ie: FST_META_TIME+FST_META_INFO)
) {

    if (dest == NULL || src == NULL) return 0;
    if (dest->do_not_touch.version != src->do_not_touch.version) {
        Lib_Log(APP_LIBFST, APP_ERROR, "%s: Incompatible record version (%d != %d)\n",__func__,dest->do_not_touch.version,src->do_not_touch.version);    
        return 0;
    }

    if (what&FST_META_TIME) {
       dest->dateo = src->dateo;
       dest->datev = src->datev;
       dest->deet = src->deet;
       dest->npas = src->npas;
    }
    if (what&FST_META_TYPE) {
       dest->data_type = src->data_type;
       dest->data_bits = src->data_bits;
       dest->pack_bits = src->pack_bits;
    }
    if (what&FST_META_SIZE) {
       dest->ni = src->ni;
       dest->nj = src->nj;
       dest->nk = src->nk;
    }
    if (what&FST_META_INFO) {
       dest->ip1 = src->ip1;
       dest->ip2 = src->ip2;
       dest->ip3 = src->ip3;

       strncpy(dest->typvar, src->typvar, FST_TYPVAR_LEN);
       strncpy(dest->nomvar, src->nomvar, FST_NOMVAR_LEN);
       strncpy(dest->etiket, src->etiket, FST_ETIKET_LEN);
    }

    if (what&FST_META_GRID) {
       strncpy(dest->grtyp, src->grtyp, FST_GTYP_LEN);
       dest->ig1 = src->ig1;
       dest->ig2 = src->ig2;
       dest->ig3 = src->ig3;
       dest->ig4 = src->ig4;
    }

    if (what&FST_META_EXT) {
        if (src->metadata) {
            if (dest->metadata) {
                Meta_Free(dest->metadata);
            }
            dest->metadata=Meta_Copy(src->metadata);
        }
    }

    return 1;
}

//! \return 1 if the given two records have the same metadata (does not check extended meta).
//!         0 otherwise
int32_t fst24_record_has_same_info(const fst_record* a, const fst_record* b) {
    if (a == NULL || b == NULL) return 0;
    if (a->do_not_touch.version != b->do_not_touch.version) return 0;
    // if (a->do_not_touch.flags != b->do_not_touch.flags) return 0;
    // if (a->do_not_touch.alloc != b->do_not_touch.alloc) return 0;
    if (a->dateo != b->dateo) return 0;
//    if (a->datev != b->datev) return 0; // not to be included int check as it is derived from other info    
    if (a->data_type != b->data_type) return 0;
    if (a->data_bits != b->data_bits) return 0;
    if (a->pack_bits != b->pack_bits) return 0;
    if (a->ni != b->ni) return 0;
    if (a->nj != b->nj) return 0;
    if (a->nk != b->nk) return 0;
    // if (a->num_meta_bytes != b->num_meta_bytes) return 0; // We don't check because it is determined automatically at write-time
    if (a->deet != b->deet) return 0;
    if (a->npas != b->npas) return 0;
    if (a->ip1 != b->ip1) return 0;
    if (a->ip2 != b->ip2) return 0;
    if (a->ip3 != b->ip3) return 0;
    if (a->ig1 != b->ig1) return 0;
    if (a->ig2 != b->ig2) return 0;
    if (a->ig3 != b->ig3) return 0;
    if (a->ig4 != b->ig4) return 0;
    if (!is_same_record_string(a->typvar, b->typvar, FST_TYPVAR_LEN)) return 0;
    if (!is_same_record_string(a->grtyp, b->grtyp, FST_GTYP_LEN)) return 0;
    if (!is_same_record_string(a->nomvar, b->nomvar, FST_NOMVAR_LEN)) return 0;
    if (!is_same_record_string(a->etiket, b->etiket, FST_ETIKET_LEN)) return 0;

    return 1;
}

//! Check whether two records have the same FST metadata (including extended metadata)
//! \return 1 if the given records have the same metadata, 0 otherwise
int32_t fst24_record_has_same_meta(const fst_record* a, const fst_record* b) {
    if (!fst24_record_has_same_info(a, b)) return 0;
    // We don't compare meta size, since it may have changed (become smaller) afterwards. What matters is the content.
    if ((a->metadata == NULL) != (b->metadata == NULL)) return 0;
    if (a->metadata != NULL && b->metadata != NULL) {
        if (strcmp(a->do_not_touch.stringified_meta, b->do_not_touch.stringified_meta) != 0) return 0;
    }

    return 1;
}

//! Print every difference between the attributes of the given 2 fst_struct
void fst24_record_diff(const fst_record* a, const fst_record* b) {
    if (a == NULL || b == NULL) {
        Lib_Log(APP_LIBFST, APP_ALWAYS, "%s: One of the records is NULL! (a = 0x%x, b = 0x%x)\n", __func__, a, b);
    }

    if (a->do_not_touch.version != b->do_not_touch.version)
        Lib_Log(APP_LIBFST, APP_ALWAYS, "%s: Version: a = %d, b = %d)\n", __func__, a->do_not_touch.version, b->do_not_touch.version);
    if (a->data != b->data)
        Lib_Log(APP_LIBFST, APP_ALWAYS, "%s: Data:    a = 0x%x, b = 0x%x)\n", __func__, a->data, b->data);
    if (a->metadata != b->metadata)
        Lib_Log(APP_LIBFST, APP_ALWAYS, "%s: Metadata:a = 0x%x, b = 0x%x)\n", __func__, a->metadata, b->metadata);
    if (a->do_not_touch.flags != b->do_not_touch.flags)
        Lib_Log(APP_LIBFST, APP_ALWAYS, "%s: Flags:   a = %d, b = %d)\n", __func__, a->do_not_touch.flags, b->do_not_touch.flags);
    if (a->do_not_touch.alloc != b->do_not_touch.alloc)
        Lib_Log(APP_LIBFST, APP_ALWAYS, "%s: Alloc:   a = %d, b = %d)\n", __func__, a->do_not_touch.alloc, b->do_not_touch.alloc);
    if (a->dateo != b->dateo)
        Lib_Log(APP_LIBFST, APP_ALWAYS, "%s: Dateo:   a = %d, b = %d)\n", __func__, a->dateo, b->dateo);
    if (a->datev != b->datev)
        Lib_Log(APP_LIBFST, APP_ALWAYS, "%s: Datev:   a = %d, b = %d)\n", __func__, a->datev, b->datev);
    if (a->do_not_touch.handle != b->do_not_touch.handle)
        Lib_Log(APP_LIBFST, APP_ALWAYS, "%s: Handle:  a = 0x%llx, b = 0x%llx)\n", __func__, a->do_not_touch.handle, b->do_not_touch.handle);
    if (a->data_type != b->data_type)
        Lib_Log(APP_LIBFST, APP_ALWAYS, "%s: data_type:   a = %d, b = %d)\n", __func__, a->data_type, b->data_type);
    if (a->data_bits != b->data_bits)
        Lib_Log(APP_LIBFST, APP_ALWAYS, "%s: data_bits:   a = %d, b = %d)\n", __func__, a->data_bits, b->data_bits);
    if (a->pack_bits != b->pack_bits)
        Lib_Log(APP_LIBFST, APP_ALWAYS, "%s: pack_bits:    a = %d, b = %d)\n", __func__, a->pack_bits, b->pack_bits);
    if (a->ni != b->ni)
        Lib_Log(APP_LIBFST, APP_ALWAYS, "%s: ni:      a = %d, b = %d)\n", __func__, a->ni, b->ni);
    if (a->nj != b->nj)
        Lib_Log(APP_LIBFST, APP_ALWAYS, "%s: nj:      a = %d, b = %d)\n", __func__, a->nj, b->nj);
    if (a->nk != b->nk)
        Lib_Log(APP_LIBFST, APP_ALWAYS, "%s: nk:      a = %d, b = %d)\n", __func__, a->nk, b->nk);
    if (a->num_meta_bytes != b->num_meta_bytes)
        Lib_Log(APP_LIBFST, APP_ALWAYS, "%s: num_meta_bytes:      a = %d, b = %d)\n", __func__, a->num_meta_bytes, b->num_meta_bytes);
    if (a->deet != b->deet)
        Lib_Log(APP_LIBFST, APP_ALWAYS, "%s: deet:    a = %d, b = %d)\n", __func__, a->deet, b->deet);
    if (a->npas != b->npas)
        Lib_Log(APP_LIBFST, APP_ALWAYS, "%s: npas:    a = %d, b = %d)\n", __func__, a->npas, b->npas);
    if (a->ip1 != b->ip1)
        Lib_Log(APP_LIBFST, APP_ALWAYS, "%s: ip1:     a = %d, b = %d)\n", __func__, a->ip1, b->ip1);
    if (a->ip2 != b->ip2)
        Lib_Log(APP_LIBFST, APP_ALWAYS, "%s: ip2:     a = %d, b = %d)\n", __func__, a->ip2, b->ip2);
    if (a->ip3 != b->ip3)
        Lib_Log(APP_LIBFST, APP_ALWAYS, "%s: ip3:     a = %d, b = %d)\n", __func__, a->ip3, b->ip3);
    if (a->ig1 != b->ig1)
        Lib_Log(APP_LIBFST, APP_ALWAYS, "%s: ig1:     a = %d, b = %d)\n", __func__, a->ig1, b->ig1);
    if (a->ig2 != b->ig2)
        Lib_Log(APP_LIBFST, APP_ALWAYS, "%s: ig2:     a = %d, b = %d)\n", __func__, a->ig2, b->ig2);
    if (a->ig3 != b->ig3)
        Lib_Log(APP_LIBFST, APP_ALWAYS, "%s: ig3:     a = %d, b = %d)\n", __func__, a->ig3, b->ig3);
    if (a->ig4 != b->ig4)
        Lib_Log(APP_LIBFST, APP_ALWAYS, "%s: ig4:     a = %d, b = %d)\n", __func__, a->ig4, b->ig4);

    if (!is_same_record_string(a->typvar, b->typvar, FST_TYPVAR_LEN))
        Lib_Log(APP_LIBFST, APP_ALWAYS, "%s: typvar:  a = \"%3s\", b = \"%3s\"\n", __func__, a->typvar, b->typvar);
    if (!is_same_record_string(a->grtyp, b->grtyp, FST_GTYP_LEN))
        Lib_Log(APP_LIBFST, APP_ALWAYS, "%s: grtyp:   a = \"%3s\", b = \"%3s\"\n", __func__, a->grtyp, b->grtyp);
    if (!is_same_record_string(a->nomvar, b->nomvar, FST_NOMVAR_LEN))
        Lib_Log(APP_LIBFST, APP_ALWAYS, "%s: nomvar:  a = \"%3s\", b = \"%3s\"\n", __func__, a->nomvar, b->nomvar);
    if (!is_same_record_string(a->etiket, b->etiket, FST_ETIKET_LEN))
        Lib_Log(APP_LIBFST, APP_ALWAYS, "%s: etiket:  a = \"%3s\", b = \"%3s\"\n", __func__, a->etiket, b->etiket);

    if (a->do_not_touch.stringified_meta && b->do_not_touch.stringified_meta) {
        if (strcmp(a->do_not_touch.stringified_meta, b->do_not_touch.stringified_meta) != 0) {
            Lib_Log(APP_LIBFST, APP_ALWAYS, "%s: extended metadata \n  a: %s\n  b: %s\n", __func__,
                a->do_not_touch.stringified_meta, b->do_not_touch.stringified_meta);
        }
    }
}

//! To be called from fortran. Determine whether the given FST record pointer matches the default
//! fst_record struct.
//! \return 0 if they match, -1 if not
int32_t fst24_validate_default_record(
    const fst_record* fortran_record, //!< Pointer to a default-initialized fst_record[_c] struct
    const size_t fortran_size         //!< Size of the fst_record_c struct in Fortran
) {
    if (fortran_record == NULL) {
        Lib_Log(APP_LIBFST, APP_ERROR, "%s: Called with NULL pointer\n", __func__);
        return -1;
    }

    if (sizeof(fst_record) != fortran_size) {
        Lib_Log(APP_LIBFST, APP_ERROR, "%s: Size C != size Fortran (%d != %d)\n",
                __func__, sizeof(fst_record), fortran_size);
        return -1;
    }

    if (memcmp(&default_fst_record, fortran_record, sizeof(fst_record)) != 0) {
        Lib_Log(APP_LIBFST, APP_ERROR, "%s: Doing record diff!\n", __func__);
        if (fst24_record_has_same_info(&default_fst_record, fortran_record)) {
            for (unsigned int i = 0; i < sizeof(fst_record) / 4; i += 4) {
                const uint32_t* c = (const uint32_t*)&default_fst_record;
                const uint32_t* f = (const uint32_t*)fortran_record;
                fprintf(stderr, "c 0x %.8x %.8x %.8x %.8x\n", c[i], c[i+1], c[i+2], c[i+3]);
                fprintf(stderr, "f 0x %.8x %.8x %.8x %.8x\n", f[i], f[i+1], f[i+2], f[i+3]);
            }
        }
        fst24_record_diff(&default_fst_record, fortran_record);
        fst24_record_print(&default_fst_record);
        fst24_record_print(fortran_record);
        return -1;
    }

    return 0;
}

//! Debug function. Print record attributes that are not at their default value.
void print_non_wildcards(const fst_record* const record) {
    char buffer[1024];
    char* ptr = buffer;

    if (record->dateo != default_fst_record.dateo) ptr += snprintf(ptr, 30, "dateo=%d ", record->dateo);
    if (record->datev != default_fst_record.datev) ptr += snprintf(ptr, 30, "datev=%d ", record->datev);
    if (record->data_type != default_fst_record.data_type) ptr += snprintf(ptr, 20, "data_type=%d ", record->data_type);
    if (record->data_bits != default_fst_record.data_bits) ptr += snprintf(ptr, 20, "data_bits=%d ", record->data_bits);
    if (record->pack_bits != default_fst_record.pack_bits) ptr += snprintf(ptr, 20, "pack_bits=%d ", record->pack_bits);
    if (record->ni != default_fst_record.ni)       ptr += snprintf(ptr, 20, "ni=%d ", record->ni);
    if (record->nj != default_fst_record.nj)       ptr += snprintf(ptr, 20, "nj=%d ", record->nj);
    if (record->nk != default_fst_record.nk)       ptr += snprintf(ptr, 20, "nk=%d ", record->nk);
    if (record->deet != default_fst_record.deet)   ptr += snprintf(ptr, 30, "deet=%d ", record->deet);
    if (record->npas != default_fst_record.npas)   ptr += snprintf(ptr, 30, "npas=%d ", record->npas);
    if (record->ip1 != default_fst_record.ip1)     ptr += snprintf(ptr, 30, "ip1=%d ", record->ip1);
    if (record->ip2 != default_fst_record.ip2)     ptr += snprintf(ptr, 30, "ip2=%d ", record->ip2);
    if (record->ip3 != default_fst_record.ip3)     ptr += snprintf(ptr, 30, "ip3=%d ", record->ip3);
    if (record->ig1 != default_fst_record.ig1)     ptr += snprintf(ptr, 30, "ig1=%d ", record->ig1);
    if (record->ig2 != default_fst_record.ig2)     ptr += snprintf(ptr, 30, "ig2=%d ", record->ig2);
    if (record->ig3 != default_fst_record.ig3)     ptr += snprintf(ptr, 30, "ig3=%d ", record->ig3);
    if (record->ig4 != default_fst_record.ig4)     ptr += snprintf(ptr, 30, "ig4=%d ", record->ig4);

    if (strncasecmp(record->typvar, default_fst_record.typvar, FST_TYPVAR_LEN) != 0)
        ptr += snprintf(ptr, 30, "typvar='%s' ", record->typvar);
    if (strncasecmp(record->grtyp, default_fst_record.grtyp, FST_GTYP_LEN) != 0)
        ptr += snprintf(ptr, 30, "grtyp='%s' ", record->grtyp);
    if (strncasecmp(record->nomvar, default_fst_record.nomvar, FST_NOMVAR_LEN) != 0)
        ptr += snprintf(ptr, 30, "nomvar='%s' ", record->nomvar);
    if (strncasecmp(record->etiket, default_fst_record.etiket, FST_ETIKET_LEN) != 0)
        ptr += snprintf(ptr, 30, "etiket='%s' ", record->etiket);

    ptr[0] = '\0';
    if (ptr == buffer) sprintf(ptr, "[none]");

    Lib_Log(APP_LIBFST, APP_ALWAYS, "criteria: %s\n", buffer);
}

void print_search_meta(const search_metadata* const keys, const fst_file_type type) {
    fst_record r;
    fill_with_search_meta(&r, keys, type);
    print_non_wildcards(&r);
}

//! Copy record information (including metadata *pointer*) into destination, while preserving
//! the data pointer and the allocation flag and status;
//! *Does not free any memory!*
void fst_record_copy_info(fst_record* const dest, const fst_record* const src) {
    const fst_record* actual_src = src ? src : &default_fst_record;
    Meta_Free(dest->metadata);
    const int64_t flags = dest->do_not_touch.flags;
    const int64_t alloc = dest->do_not_touch.alloc;
    void* const data  = dest->data;
    *dest = *actual_src;
    dest->do_not_touch.flags = flags;
    dest->do_not_touch.alloc = alloc;
    dest->data  = data;
    dest->metadata = json_object_get(actual_src->metadata); // This increments its reference count
}
