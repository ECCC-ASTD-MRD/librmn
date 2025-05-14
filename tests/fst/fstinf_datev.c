#include <App.h>
#include <rmn.h>

const char* filename_rsf = "fstinf_datev.rsf";
const char* filename_xdf = "fstinf_datev.xdf";

int ORIGIN_DATE = 20250506;
int ORIGIN_TIME = 0;
const int DEET = 24;
const int INITIAL_NPAS = 10;

int create_file(const int is_rsf) {
    const char* filename = is_rsf ? filename_rsf : filename_xdf;
    const char* options = is_rsf ? "RSF+R/W" : "XDF+R/W";

    remove(filename);

    fst_file* f = fst24_open(filename, options);
    if (f == NULL) { return -1; }
    fst_record rec = default_fst_record;
    float dummy_data[1] = {0.f};
    rec.data = dummy_data;
    rec.data_type = FST_TYPE_REAL;
    rec.data_bits = 32;
    rec.pack_bits = 32;

    sprintf(rec.nomvar, "A");
    sprintf(rec.etiket, "TEST_FSTINF");

    rec.ip1 = 1;
    rec.ip2 = 0;
    rec.ip3 = 0;
    rec.ni = 1;
    rec.nj = 1;
    rec.nk = 1;
    rec.ig1 = 0;
    rec.ig2 = 0;
    rec.ig3 = 0;
    rec.ig4 = 0;

    rec.deet = DEET;
    rec.npas = INITIAL_NPAS;
    const int mode = 3;
    newdate_c(&rec.dateo, &ORIGIN_DATE, &ORIGIN_TIME, &mode);
    // rec.dateo = ORIGIN_DATE;
    rec.datev = 0;

    App_Log(APP_INFO, "%s: dateo = %d\n", __func__, rec.dateo);

    if (fst24_write(f, &rec, FST_NO) != TRUE) { return -1; }
    rec.npas += 2;
    if (fst24_write(f, &rec, FST_NO) != TRUE) { return -1; }
    if (fst24_close(f) != TRUE) { return -1; }

    return 0;
}

int test_datev(const int is_rsf) {

    if (create_file(is_rsf) != 0) return -1;

    const char* filename = is_rsf ? filename_rsf : filename_xdf;

    int iun = 0;
    c_fnom(&iun, filename, "RND+R/O", 0);
    c_fstouv(iun, "RND+R/O");

    int desired_dateo;
    const int mode = 3;
    newdate_c(&desired_dateo, &ORIGIN_DATE, &ORIGIN_TIME, &mode);
    const int DESIRED_DATEV = get_valid_date32(desired_dateo, DEET, INITIAL_NPAS + 2);

    int ni, nj, nk;
    const int handle = c_fstinf(iun, &ni, &nj, &nk, DESIRED_DATEV, "", -1, -1, -1, "", "");
    
    if (handle < 0) {
        App_Log(APP_ERROR, "Could not get record\n");
        return -1;
    }

    int dateo, deet, npas;
    int nbits, datyp;
    int ip1, ip2, ip3;
    int ig1, ig2, ig3, ig4;
    char typvar[FST_TYPVAR_LEN + 1]; memset(typvar, 0, sizeof(typvar));
    char nomvar[FST_NOMVAR_LEN + 1]; memset(nomvar, 0, sizeof(nomvar));
    char grtyp[FST_GTYP_LEN + 1]; memset(grtyp, 0, sizeof(grtyp));
    char etiket[FST_ETIKET_LEN + 1]; memset(etiket, 0, sizeof(etiket));
    int swa, lng, dltf, ubc;
    int extra1, extra2, extra3;
    c_fstprm(handle, &dateo, &deet, &npas, &ni, &nj, &nk,  &nbits, &datyp, &ip1, &ip2, &ip3,
             typvar, nomvar, etiket, grtyp, &ig1, &ig2, &ig3, &ig4, &swa, &lng, &dltf, &ubc, &extra1, &extra2, &extra3);

    const int datev = get_valid_date32(dateo, deet, npas);
    App_Log(APP_INFO, "Handle = %d, typvar = '%s', nomvar = '%s', dateo = %d, datev = %d, ip123 = %d, %d, %d, etiket = '%s'\n",
            handle, typvar, nomvar, dateo, datev, ip1, ip2, ip3, etiket);
    if (datev != DESIRED_DATEV) {
        App_Log(APP_ERROR, "Expected datev = %d, got %d\n", DESIRED_DATEV, datev);
        return -1;
    }

    c_fstfrm(iun);
    c_fclos(iun);

    return 0;
}

int main(void) {
    App_Log(APP_INFO, "Testing RSF\n");
    if (test_datev(1) != 0) return -1;
    App_Log(APP_INFO, "Testing XDF\n");
    if (test_datev(0) != 0) return -1;
    App_Log(APP_INFO, "Test successful\n");
    return 0;
}
