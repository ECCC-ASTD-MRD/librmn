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

    char nomvar[FST_NOMVAR_LEN];
    copy_record_string(nomvar, record->nomvar, FST_NOMVAR_LEN);
    criteria->nomvar = (ascii6(nomvar[0]) << 18) |
                       (ascii6(nomvar[1]) << 12) |
                       (ascii6(nomvar[2]) <<  6) |
                       (ascii6(nomvar[3]));
    if (criteria->nomvar == 0) mask->nomvar = 0;

    char typvar[FST_TYPVAR_LEN];
    copy_record_string(typvar, record->typvar, FST_TYPVAR_LEN);
    criteria->typvar = (ascii6(typvar[0]) << 6) |
                       (ascii6(typvar[1]));
    if (criteria->typvar == 0) mask->typvar = 0;

    char etiket[FST_ETIKET_LEN];
    copy_record_string(etiket, record->etiket, FST_ETIKET_LEN);
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

    char grtyp[FST_GTYP_LEN];
    copy_record_string(grtyp, record->grtyp, FST_GTYP_LEN);
    criteria->gtyp = grtyp[0];
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

void fst23_record_diff(const fst_record* a, const fst_record* b) {
    if (a == NULL || b == NULL) {
        Lib_Log(APP_LIBFST, APP_ALWAYS, "%s: One of the records is NULL! (a = 0x%x, b = 0x%x)\n", __func__, a, b);
    }

    if (a->version != b->version)
        Lib_Log(APP_LIBFST, APP_ALWAYS, "%s: Version: a = %d, b = %d)\n", __func__, a->version, b->version);
    if (a->data != b->data)
        Lib_Log(APP_LIBFST, APP_ALWAYS, "%s: Data:    a = 0x%x, b = 0x%x)\n", __func__, a->data, b->data);
    if (a->metadata != b->metadata)
        Lib_Log(APP_LIBFST, APP_ALWAYS, "%s: Metadata:a = 0x%x, b = 0x%x)\n", __func__, a->metadata, b->metadata);
    if (a->flags != b->flags)
        Lib_Log(APP_LIBFST, APP_ALWAYS, "%s: Flags:   a = %d, b = %d)\n", __func__, a->flags, b->flags);
    if (a->dateo != b->dateo)
        Lib_Log(APP_LIBFST, APP_ALWAYS, "%s: Dateo:   a = %d, b = %d)\n", __func__, a->dateo, b->dateo);
    if (a->datev != b->datev)
        Lib_Log(APP_LIBFST, APP_ALWAYS, "%s: Datev:   a = %d, b = %d)\n", __func__, a->datev, b->datev);
    if (a->handle != b->handle)
        Lib_Log(APP_LIBFST, APP_ALWAYS, "%s: Handle:  a = %d, b = %d)\n", __func__, a->handle, b->handle);
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

    if (strncmp(a->typvar, b->typvar, FST_TYPVAR_LEN) != 0)
        Lib_Log(APP_LIBFST, APP_ALWAYS, "%s: typvar:  a = \"%3s\", b = \"%3s\"\n", __func__, a->typvar, b->typvar);
    if (strncmp(a->grtyp, b->grtyp, FST_GTYP_LEN) != 0)
        Lib_Log(APP_LIBFST, APP_ALWAYS, "%s: grtyp:   a = \"%3s\", b = \"%3s\"\n", __func__, a->grtyp, b->grtyp);
    if (strncmp(a->nomvar, b->nomvar, FST_NOMVAR_LEN) != 0)
        Lib_Log(APP_LIBFST, APP_ALWAYS, "%s: nomvar:  a = \"%3s\", b = \"%3s\"\n", __func__, a->nomvar, b->nomvar);
    if (strncmp(a->etiket, b->etiket, FST_ETIKET_LEN) != 0)
        Lib_Log(APP_LIBFST, APP_ALWAYS, "%s: etiket:  a = \"%3s\", b = \"%3s\"\n", __func__, a->etiket, b->etiket);
}
