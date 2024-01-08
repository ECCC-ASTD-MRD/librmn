#include "fst_record_internal.h"

#include <App.h>

//! Creates a new record and assign the data pointer or allocate data memory
//! \return new record
fst_record fst23_record_init(
    void   *data,   //!< Data pointer to assign, or allocate inernal array in NULL
    int32_t type,   //!< Data type
    int32_t nbits,  //!< Number of bits per data element
    int32_t ni,     //!< I horizontal size
    int32_t nj,     //!< J horizontal size
    int32_t nk      //!< K vertical size
) {

    fst_record result = default_fst_record;

    result.ni=ni;
    result.nj=nj;
    result.nk=nk;
    result.dasiz=nbits;
    result.datyp=type;

    if (!(result.data=data)) {
       // No data pointer passed, allocate data array
       if (!(result.data=(void*)malloc(FST_REC_SIZE((&result))))) {
          Lib_Log(APP_ERROR,APP_LIBFST, "%s: Unable to allocate record data (%ix%ix%i)\n", __func__,ni,nj,nk);
       }
    } else {
       // Using an assigned pointer
       result.flags!=FST_REC_ASSIGNED;
    }

    return(result);
}

int32_t fst23_record_free(fst_record* record) {

   if (record->data && record->flags&FST_REC_ASSIGNED) {
      free(record->data);
      record->data=NULL; 
   }
   return(TRUE);
}

void fst23_record_print(const fst_record* record) {
    Lib_Log(APP_LIBFST, APP_ALWAYS,
        "\n"
        "  Version: %ld\n"
        "  Data: 0x%x\n"
        "  Metadata: 0x%x\n"
        "  Handle: 0x%x\n"
        "  date (v?): %ld\n"
        "  datyp: %d\n"
        "  dasiz: %d\n"
        "  npak: %d\n"
        "  ni x nj x nk: %d x %d x %d (%d) elements\n"
        "  deet: %d, npas: %d\n"
        "  ip1-3: %d, %d, %d\n"
        "  ig1-4: %d, %d, %d, %d\n"
        "  typvar: %s\n"
        "  grtyp:  %s\n"
        "  nomvar: %s\n"
        "  etiket: %s\n",
        record->version, record->data, record->metadata, record->handle, 
        record->dateo, record->datyp, record->dasiz, record->npak,
        record->ni, record->nj, record->nk, record->ni * record->nj * record->nk,
        record->deet, record->npas, record->ip1, record->ip2, record->ip3,
        record->ig1, record->ig2, record->ig3, record->ig4,
        record->typvar, record->grtyp, record->nomvar, record->etiket
    );
}

int32_t fst23_record_is_valid(const fst_record* record) {
    if (record == NULL) {
        Lib_Log(APP_ERROR, APP_LIBFST, "%s: Record pointer is NULL\n", __func__);
        return 0;
    }
    
    if (record->version != default_fst_record.version) {
        Lib_Log(APP_ERROR, APP_LIBFST, "%s: Version number is wrong! This means your program (%ld) was not compiled"
                " with the same version it is linked to (%ld)...\n", __func__, record->version, default_fst_record.version);
        return 0;
    }

    return 1;
}

int32_t fst23_record_validate_params(const fst_record* record) {
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

void make_search_criteria(const fst_record* record, stdf_dir_keys* criteria, stdf_dir_keys* mask) {
    // Check for error
    if (!fst23_record_is_valid(record) || criteria == NULL || mask == NULL) {
        Lib_Log(APP_LIBFST, APP_ERROR, "%s: Invalid record or criteria/mask pointers\n", __func__);
        return;
    }

    const unsigned int u_datev = (unsigned int) record->dateo;

    // Reset search mask
    {
        uint32_t *pmask = (uint32_t *) mask;
        for (uint32_t i = 0; i < (sizeof(stdf_dir_keys) / sizeof(uint32_t)); i++) {
            pmask[i] = -1;
        }
    }

    mask->pad1 = 0;
    mask->pad2 = 0;
    mask->pad3 = 0;
    mask->dasiz = 0;
    mask->pad5 = 0;
    mask->pad6 = 0;
    mask->pad7 = 0;
    mask->deleted = 0;
    mask->select = 0;
    mask->lng = 0;
    mask->addr = 0;
    mask->deet = 0;
    mask->nbits = 0;
    mask->ni = 0;
    mask->gtyp = 0;
    mask->nj = 0;
    mask->datyp = 0;
    mask->nk = 0;
    mask->ubc = 0;
    mask->npas = 0;
    mask->ig4 = 0;
    mask->ig2a = 0;
    mask->ig1 = 0;
    mask->ig2b = 0;
    mask->ig3 = 0;
    mask->ig2c = 0;
    mask->levtyp = 0;

    criteria->date_stamp = 8 * (u_datev/10) + (u_datev % 10);
    mask->date_stamp &= ~(0x7);
    if (record->dateo == -1) mask->date_stamp = 0;

    criteria->ip1 = record->ip1;
    if ((record->ip1 == -1)) mask->ip1 = 0;

    criteria->ip2 = record->ip2;
    if ((record->ip2 == -1)) mask->ip2 = 0;

    criteria->ip3 = record->ip3;
    if ((record->ip3 == -1)) mask->ip3 = 0;

    criteria->nomvar = (ascii6(record->nomvar[0]) << 18) |
                       (ascii6(record->nomvar[1]) << 12) |
                       (ascii6(record->nomvar[2]) <<  6) |
                       (ascii6(record->nomvar[3]));
    if (criteria->nomvar == 0) mask->nomvar = 0;

    criteria->typvar = (ascii6(record->typvar[0]) << 6) |
                       (ascii6(record->typvar[1]));
    if (criteria->typvar == 0) mask->typvar = 0;

    criteria->etik15 = (ascii6(record->etiket[0]) << 24) |
                       (ascii6(record->etiket[1]) << 18) |
                       (ascii6(record->etiket[2]) << 12) |
                       (ascii6(record->etiket[3]) <<  6) |
                       (ascii6(record->etiket[4]));

    criteria->etik6a = (ascii6(record->etiket[5]) << 24) |
                       (ascii6(record->etiket[6]) << 18) |
                       (ascii6(record->etiket[7]) << 12) |
                       (ascii6(record->etiket[8]) <<  6) |
                       (ascii6(record->etiket[9]));

    criteria->etikbc = (ascii6(record->etiket[10]) <<  6) |
                       (ascii6(record->etiket[11]));

    if ((criteria->etik15 == 0) && (criteria->etik6a == 0)) {
        mask->etik15 = 0;
        mask->etik6a = 0;
        mask->etikbc = 0;
    }
}

fst_record record_from_dir_keys(const stdf_dir_keys* keys) {
    fst_record result = default_fst_record;

    stdf_special_parms cracked;
    crack_std_parms(keys, &cracked);

    result.dateo = cracked.date_valid;

    result.ni = keys->ni;
    result.nj = keys->nj;
    result.nk = keys->nk;
    result.datyp = keys->datyp;
    result.dasiz = keys->dasiz;
    result.npak = -keys->nbits;

    result.deet = keys->deet;
    result.npas = keys->npas;

    strncpy(result.nomvar, cracked.nomvar, FST_NOMVAR_LEN);
    strncpy(result.typvar, cracked.typvar, FST_TYPVAR_LEN);
    strncpy(result.etiket, cracked.etiket, FST_ETIKET_LEN);
    strncpy(result.grtyp,  cracked.gtyp,   FST_GTYP_LEN);

    result.ip1 = keys->ip1;
    result.ip2 = keys->ip2;
    result.ip3 = keys->ip3;

    result.ig1 = keys->ig1;
    result.ig2 = cracked.ig2;
    result.ig3 = keys->ig3;
    result.ig4 = keys->ig4;

    return result;
}
