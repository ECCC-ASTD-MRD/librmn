#include "fst24_record_internal.h"

#include <ctype.h>
#include <string.h>

#include <App.h>
#include "Meta.h"
#include "rmn/convert_ip.h"

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
        result->dasiz = nbits;
        result->datyp = type;
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
        "  dateo: %ld\n"
        "  datev: %ld\n"
        "  datyp: %d\n"
        "  dasiz: %d\n"
        "  npak: %d\n"
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
        record->dateo, record->datev, record->datyp, record->dasiz, record->npak,
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
    if (target_length > num_written) {
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

//! Add a date (from a timestamp) to the line to be printed. See \ref fst24_record_print_short.
//! \return Pointer to after the (new) last character of the line to be printed
char* add_date(
    char* buffer,           //!< [in,out] Buffer containing the line to be printed
    const int64_t datestamp //!< Timestamp to be translated and added to the line
) {
    char* result = buffer;
    int32_t dat2, dat3;
    int32_t minus3 = -3;
    int32_t small_date = datestamp;
    f77name(newdate)(&small_date, &dat2, &dat3, &minus3);

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
    snprintf(buffer, length, "%s", "                      ");
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
    F2Cl lc1 = 1, lc2 = 7, lc3 = 7, lc4 = 8, lc5 = 8;
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
    buffer = add_str(buffer, grtyp, 1);
    buffer = add_int(buffer, ig1, 5, 0);
    buffer = add_int(buffer, ig2, 5, 0);
    buffer = add_int(buffer, ig3, 5, 0);
    buffer = add_int(buffer, ig4, 5, 0);
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

        .datyp = 8,
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

        if (to_print.datyp) current = add_str(current, "DTYP SIZ", width.datyp);

        if (to_print.grid_info) current = add_str(current, "G    XG1    XG2     XG3     XG4", width.grid_info);
        else if (to_print.ig1234) current = add_str(current, "G   IG1   IG2   IG3   IG4", width.ig1234);

        Lib_Log(APP_LIBFST, prefix?APP_INFO:APP_VERBATIM, "%s\n", buffer);
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

    if (to_print.datyp) current = add_datatype(current, record->datyp, -record->npak, record->dasiz, width.datyp);

    if (to_print.grid_info) current = add_grid_info(current, record->grtyp, record->ig1, record->ig2,
                                                    record->ig3, record->ig4, width.grid_info);
    else if (to_print.ig1234) current = add_ig1234(current, record->grtyp, record->ig1, record->ig2,
                                                    record->ig3, record->ig4, width.ig1234);

    Lib_Log(APP_LIBFST, prefix?APP_INFO:APP_VERBATIM, "%s\n", buffer);

    if (to_print.metadata > 0) {
        fst24_read_metadata((fst_record*)record);
        Lib_Log(APP_LIBFST, APP_VERBATIM, "%s\n", Meta_Stringify(record->metadata));
    }

}

//! Check whether the given record struct is valid (does not check its content, just its version token)
int32_t fst24_record_is_valid(const fst_record* record) {
    if (record == NULL) {
        Lib_Log(APP_LIBFST, APP_ERROR, "%s: Record pointer is NULL\n", __func__);
        return 0;
    }
    
    if (record->do_not_touch.version != default_fst_record.do_not_touch.version) {
        Lib_Log(APP_LIBFST, APP_ERROR, "%s: Version number (%ld) is wrong (should be %ld)! This means that either:\n"
                "  - The given address does not point to an initialized fst_record struct\n"
                "  - Your program was not compiled with the same version it is linked to\n",
                __func__, record->do_not_touch.version, default_fst_record.do_not_touch.version);
        return 0;
    }

    return 1;
}

//! Validate the parameters of the given record.
//! Will exit with an error if they are not valid.
int32_t fst24_record_validate_params(const fst_record* record) {
    VALID(record->ni, 1, NI_MAX, "ni")
    VALID(record->nj, 1, NJ_MAX, "nj")
    VALID(record->nk, 1, NK_MAX, "nk")
    VALID(record->deet, 0, DEET_MAX, "deet")
    VALID(record->npas, 0, NPAS_MAX, "npas")
    // VALID(record->nbits, 1, NBITS_MAX, "nbits")
    VALID(record->ig1, 0, IG1_MAX, "ig1")
    VALID(record->ig2, 0, IG2_MAX, "ig2")
    VALID(record->ig3, 0, IG3_MAX, "ig3")
    VALID(record->ig4, 0, IG4_MAX, "ig4")
    VALID(record->ip1, 0, IP1_MAX, "ip1")
    VALID(record->ip2, 0, IP2_MAX, "ip2")
    VALID(record->ip3, 0, IP3_MAX, "ip3")

    return 0;
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
    {
        uint32_t *pmask = (uint32_t *) mask;
        for (uint32_t i = 0; i < (sizeof(search_metadata) / sizeof(uint32_t)); i++) {
            pmask[i] = 0xffffffff;
        }
    }

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
            Lib_Log(APP_LIBFST, APP_WARNING, "%s: Searching by origin date (dateo) has not been implemented yet\n", __func__);
        }

        fst98_meta->date_stamp = stamp_from_date(record->datev);
        fst98_mask->date_stamp &= ~(0x7);
        if (record->datev == default_fst_record.datev) fst98_mask->date_stamp = 0;

        fst98_meta->ni = record->ni;
        if ((record->ni == default_fst_record.ni)) fst98_mask->ni = 0;
        else Lib_Log(APP_LIBFST, APP_ALWAYS, "%s: Searching by ni (%d)\n", __func__, record->ni);

        fst98_meta->nj = record->nj;
        if ((record->nj == default_fst_record.nj)) fst98_mask->nj = 0;

        fst98_meta->nk = record->nk;
        if ((record->nk == default_fst_record.nk)) fst98_mask->nk = 0;

        fst98_meta->datyp = record->datyp;
        if (record->datyp == default_fst_record.datyp) fst98_mask->datyp = 0;

        fst98_meta->dasiz = record->dasiz;
        if (record->dasiz == default_fst_record.dasiz) fst98_mask->dasiz = 0;

        fst98_meta->nbits = abs(record->npak);
        if (record->npak >= default_fst_record.npak) fst98_mask->nbits = 0;

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
                int ip1 = record->ip1;
                float val;
                int kind;
                ConvertIp(&ip1, &val, &kind, -1); // Retrieve value
                ConvertIp(&query->ip1s[0], &val, &kind, 2); // Encode with new encoding
                ConvertIp(&query->ip1s[1], &val, &kind, 3); // Encode with old encoding
                fst98_mask->ip1 = 0;
            }
            if (query->options.ip2_all > 0) {
                float val;
                int kind;
                int ip2 = record->ip2;
                ConvertIp(&ip2, &val, &kind, -1); // Retrieve value
                ConvertIp(&query->ip2s[0], &val, &kind, 2); // Encode with new encoding
                ConvertIp(&query->ip2s[1], &val, &kind, 3); // Encode with old encoding
                fst98_mask->ip2 = 0;
            }
            if (query->options.ip3_all > 0) {
                float val;
                int kind;
                int ip3 = record->ip3;
                ConvertIp(&ip3, &val, &kind, -1); // Retrieve value
                ConvertIp(&query->ip3s[0], &val, &kind, 2); // Encode with new encoding
                ConvertIp(&query->ip3s[1], &val, &kind, 3); // Encode with old encoding
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
            (ascii6(record->etiket[0]) << 24) |
            (ascii6(record->etiket[1]) << 18) |
            (ascii6(record->etiket[2]) << 12) |
            (ascii6(record->etiket[3]) <<  6) |
            (ascii6(record->etiket[4]));

        fst98_meta->etik6a =
            (ascii6(record->etiket[5]) << 24) |
            (ascii6(record->etiket[6]) << 18) |
            (ascii6(record->etiket[7]) << 12) |
            (ascii6(record->etiket[8]) <<  6) |
            (ascii6(record->etiket[9]));

        fst98_meta->etikbc =
            (ascii6(record->etiket[10]) <<  6) |
            (ascii6(record->etiket[11]));

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
        if (record->grtyp[0] == ' ') fst98_mask->gtyp = 0;
    } // fst98 criteria
}

void fill_with_search_meta(fst_record* record, const search_metadata* meta, const fst_file_type type) {

    // Check version first
    uint8_t version = 0;
    uint8_t num_criteria = sizeof(stdf_dir_keys) / sizeof(uint32_t);

    if (type != FST_XDF) {
        decode_fst24_reserved_0(meta->fst24_reserved[0], &version, &num_criteria);
    }

    if (version > FST24_VERSION_COUNT) {
        Lib_Log(APP_LIBFST, APP_WARNING, "%s: Interpreting search metadata with a library from an earlier version (%d)"
                " than the one used to write the record (%d). We might run into issues (or not).\n",
                __func__, FST24_VERSION_COUNT, version);
    }

    // fst98 metadata
    {
        const stdf_dir_keys* fst98_meta = &(meta->fst98_meta);
        stdf_special_parms cracked;
        crack_std_parms(fst98_meta, &cracked);

        record->dateo = cracked.date_stamp;
        record->datev = cracked.date_valid;

        record->ni = fst98_meta->ni;
        record->nj = fst98_meta->nj;
        record->nk = fst98_meta->nk;
        record->datyp = fst98_meta->datyp;
        record->dasiz = fst98_meta->dasiz;
        if (record->dasiz == 0) record->dasiz = 32;
        record->npak = -fst98_meta->nbits;

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

    // Here, we implement reading content for next-generation FST (anything that goes beyond the stdf_dir_keys struct)
}

//! Decode the given directory metadata into an fst_record struct
//! \return The decoded directory metadata
fst_record record_from_search_meta(
    const search_metadata* meta,  //!< Encoded search (directory) metadata (as read from file)
    const fst_file_type type
) {
    fst_record result = default_fst_record;
    fill_with_search_meta(&result, meta, type);
    return result;
}

//! \return 1 if the given two records have the same parameters (*except their pointers and handles*),
//!         0 otherwise
int32_t fst24_record_copy_metadata(fst_record* a, const fst_record* b) {

    if (a == NULL || b == NULL) return 0;
    if (a->do_not_touch.version != b->do_not_touch.version) return 0;

    a->dateo = b->dateo;
    a->datev = b->datev;
//    a->datyp = b->datyp;
//    a->dasiz = b->dasiz;
//    a->npak = b->npak;
//    a->ni = b->ni;
//    a->nj = b->nj;
//    a->nk = b->nk;
    a->deet = b->deet;
    a->npas = b->npas;
    a->ip1 = b->ip1;
    a->ip2 = b->ip2;
    a->ip3 = b->ip3;
    a->ig1 = b->ig1;
    a->ig2 = b->ig2;
    a->ig3 = b->ig3;
    a->ig4 = b->ig4;
    strncpy(a->typvar, b->typvar, FST_TYPVAR_LEN);
    strncpy(a->grtyp, b->grtyp, FST_GTYP_LEN);
    strncpy(a->nomvar, b->nomvar, FST_NOMVAR_LEN);
    strncpy(a->etiket, b->etiket, FST_ETIKET_LEN);

    if (b->metadata) {
        if (a->metadata) {
            Meta_Free(a->metadata);
        }
        a->metadata=Meta_Copy(b->metadata);
    }

    return 1;
}

//! \return 1 if the given two records have the same parameters (*except their pointers and handles*),
//!         0 otherwise
int32_t fst24_record_has_same_info(const fst_record* a, const fst_record* b) {
    if (a == NULL || b == NULL) return 0;
    if (a->do_not_touch.version != b->do_not_touch.version) return 0;
    // if (a->do_not_touch.flags != b->do_not_touch.flags) return 0;
    // if (a->do_not_touch.alloc != b->do_not_touch.alloc) return 0;
    if (a->dateo != b->dateo) return 0;
//    if (a->datev != b->datev) return 0; // not to be included int check as it is derived from other info    
    if (a->datyp != b->datyp) return 0;
    if (a->dasiz != b->dasiz) return 0;
    if (a->npak != b->npak) return 0;
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
        Lib_Log(APP_LIBFST, APP_ALWAYS, "%s: Handle:  a = %d, b = %d)\n", __func__, a->do_not_touch.handle, b->do_not_touch.handle);
    if (a->datyp != b->datyp)
        Lib_Log(APP_LIBFST, APP_ALWAYS, "%s: Datyp:   a = %d, b = %d)\n", __func__, a->datyp, b->datyp);
    if (a->dasiz != b->dasiz)
        Lib_Log(APP_LIBFST, APP_ALWAYS, "%s: Dasiz:   a = %d, b = %d)\n", __func__, a->dasiz, b->dasiz);
    if (a->npak != b->npak)
        Lib_Log(APP_LIBFST, APP_ALWAYS, "%s: Npak:    a = %d, b = %d)\n", __func__, a->npak, b->npak);
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
            for (int i = 0; i < sizeof(fst_record) / 4; i += 4) {
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

void print_non_wildcards(const fst_record* const record) {
    char buffer[1024];
    char* ptr = buffer;

    if (record->dateo != default_fst_record.dateo) ptr += snprintf(ptr, 30, "dateo=%ld ", record->dateo);
    if (record->datev != default_fst_record.datev) ptr += snprintf(ptr, 30, "datev=%ld ", record->datev);
    if (record->datyp != default_fst_record.datyp) ptr += snprintf(ptr, 20, "datyp=%d ", record->datyp);
    if (record->dasiz != default_fst_record.dasiz) ptr += snprintf(ptr, 20, "dasiz=%d ", record->dasiz);
    if (record->npak != default_fst_record.npak)   ptr += snprintf(ptr, 20, "npak=%d ", record->npak);
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
