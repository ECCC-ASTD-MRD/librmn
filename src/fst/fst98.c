//! \file
//! Low level implementation of fst functions

#include <stdio.h>
#include <unistd.h>

#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <strings.h>
#include <math.h>
#include <sys/types.h>
#include <regex.h>
#include <ctype.h>

#include <pthread.h>

#include <App.h>
#include <rmn/c_wkoffit.h>
#include <rmn/excdes_new.h>
#include <rmn/fst_missing.h>

#include "fst98_internal.h"
#include "fstcvt2.h"
#include "primitives/fnom_internal.h"
#include <rmn/convert_ip.h>
#include "xdf98.h"

#define Max_Ipvals 50


const char * const FST_TYPE_NAMES[] = {
    "FST_TYPE_BINARY",
    "FST_TYPE_OLD_QUANT",
    "FST_TYPE_UNSIGNED",
    "FST_TYPE_CHAR",
    "FST_TYPE_SIGNED",
    "FST_TYPE_REAL_IEEE",
    "FST_TYPE_REAL",
    "FST_TYPE_STRING",
    "FST_TYPE_COMPLEX"
};

static int ip_nb[3] = {0, 0, 0};
int ip1s_flag = 0;
int ip2s_flag = 0;
int ip3s_flag = 0;
static int dejafait_xdf_1 = 0;
static int dejafait_xdf_2 = 0;

static int ips_tab[3][Max_Ipvals];

static uint32_t link_list[1024];
static int stdf_version = 200001;
static int first_linked_file = -1;

//! Downgrade 64 bit field to 32 bit field when reading
int downgrade_32 = 0;
//! ARMNLIB environment variable
static char *ARMNLIB = NULL;
//! Print on debug file when available
static char *debug_filename = NULL;
//! Filter file, desire/exclure
static char *requetes_filename = NULL;
//! Used for datatype remapping
int remap_table[2][10];
//! Number of datatype remapping,  0 = no remapping
int nb_remap = 0;
//! backend type (XDF or RSF)
static char *fst_backend = NULL;
//! Segment size for RSF, when writing in parallel (in MB)
static int32_t segment_size_mb = 1000;

#define PRNT_OPTIONS_LEN 128

//! What is printed with fstecr
char prnt_options[PRNT_OPTIONS_LEN] = "NINJNK+DATESTAMPO+IP1+IG1234";

static int kinds_table_init = 1;
static char kind_chars[96];

//! By default ignore names starting with >!^
static char exception_vars[256] = "~^[<>!^]";
static int read_done = 0;
static regex_t pattern;

static int turbocomp_mode = 0;
static char *comptab[2] = {"FAST", "BEST"};

static pthread_mutex_t fst98_mutex = PTHREAD_MUTEX_INITIALIZER;

fstd_usage_info* fst98_open_files = NULL; //!< Information about all currently open FST files
int MAX_FST98_FILES = 0; //!< How many FST98 files can be open simultaneously

static int initialize_fst98(void);

// Callable from Fortran
int32_t is_type_real_f(const int32_t type_flag) { return is_type_real(type_flag); }
int32_t is_type_complex_f(const int32_t type_flag) { return is_type_complex(type_flag); }
int32_t is_type_turbopack_f(const int32_t type_flag) { return is_type_turbopack(type_flag); }
int32_t has_type_missing_f(const int32_t type_flag) { return has_type_missing(type_flag); }
int32_t is_type_integer_f(const int32_t type_flag) { return is_type_integer(type_flag); }
int32_t base_fst_type_f(const int32_t type_flag) { return base_fst_type(type_flag); }


static void str_cp_init(char * const dst, const int dstLen, const char * const src, const int srcLen) {
    for (int i = 0; i < dstLen - 1; i++) {
        dst[i] = (i < srcLen) ? src[i] : ' ';
    }
    dst[dstLen - 1] = '\0';
}

//! Copy a FSTD record string into the given buffer. The buffer has to have at least max_length bytes.
//! Any character beyond the input string (up to max_length) will be set to ' '
//! The destination string will be NULL-terminated
void copy_record_string(
    char* const dest,           //!< Buffer into which the string will be copied
    const char* const src,      //!< The string to copy
    const int32_t max_length    //!< Maximum size of the final string
) {
    const size_t src_length = strlen(src);
    const size_t actual_max_length = max_length > 0 ? max_length : 0;
    unsigned int i;
    for (i = 0; i < actual_max_length && i < src_length; i++) {
        dest[i] = src[i];
    }
    for (; i < actual_max_length - 1; i++) {
        dest[i] = ' ';
    }
    dest[actual_max_length - 1] = '\0';
}

//! This function is slow and possibly not perfect
int32_t is_same_record_string(const char * const str_a, const char * const str_b, const int32_t max_length) {
    char copy_a[max_length];
    char copy_b[max_length];
    copy_record_string(copy_a, str_a, max_length);
    copy_record_string(copy_b, str_b, max_length);
    for (int i = 0; i < max_length; i++) {
        if (toupper(copy_a[i]) != toupper(copy_b[i])) return 0;
    }

    return 1;
}

//! Get record valid date from origin date, timestep size and count
uint32_t get_valid_date32(
    const int64_t origin_date,      //!< Start date of the run
    const int32_t timestep_size,    //!< Size of the timestep in seconds
    const int32_t timestep_num      //!< Timestep number
) {
    const int64_t delta_seconds = (int64_t) timestep_size * timestep_num;
    if (delta_seconds > 0) {
        int32_t f_datev = origin_date;
        double nhours = ((double) delta_seconds) / 3600.0;
        incdatr_c(&f_datev, &f_datev, &nhours);
        return f_datev;
    }

    return (uint32_t)origin_date;
}

//! Copy items of a certain size into an array of items with a larger size. If the input array is of type float, it
//! will be converted to double. Only performs copies from size [8, 16, 32] to size [16, 32, 64]. 
//! *The source and destination array must not point to the same location!*
void upgrade_size(
    void* dest,             //!< [out] Destination array, into which the items are copied
    const int dest_size,    //!< [in] Size of items in destination array, in bits
    void* src,              //!< [in] Source array, from which the items are copied
    const int src_size,     //!< [in] Size of items in source array, in bits
    const int64_t num_elem, //!< [in] Number of items to copy
    const int is_integer    //!< [in] Whether we are copying integers (or float)
) {
    if (is_integer) {
        if (dest_size == 16) {
            int16_t* d = dest;
            if (src_size == 8) {
                int8_t* s = src;
                for (int i = num_elem - 1; i >= 0; i--) d[i] = s[i];
                return;
            }
        }
        else if (dest_size == 32) {
            int32_t* d = dest;
            if (src_size == 8) {
                int8_t* s = src;
                for (int i = num_elem - 1; i >= 0; i--) d[i] = s[i];
                return;
            }
            else if (src_size == 16) {
                int16_t* s = src;
                for (int i = num_elem - 1; i >= 0; i--) d[i] = s[i];
                return;
            }
        }
        else if (dest_size == 64) {
            int64_t* d = dest;
            if (src_size == 8) {
                int8_t* s = src;
                for (int i = num_elem - 1; i >= 0; i--) d[i] = s[i];
                return;
            }
            else if (src_size == 16) {
                int16_t* s = src;
                for (int i = num_elem - 1; i >= 0; i--) d[i] = s[i];
                return;
            }
            else if (src_size == 32) {
                int32_t* s = src;
                for (int i = num_elem - 1; i >= 0; i--) d[i] = s[i];
                return;
            }
        }
    }
    else {
        if (dest_size == 64 && src_size == 32) {
            register double* d = dest;
            register float* s = src;
            for (int i = num_elem - 1; i >= 0; i--) d[i] = (double)s[i];
            return;
        }
    }

    Lib_Log(APP_LIBFST, APP_ERROR, "%s: Unhandled sizes: dest %d, src %d, is_integer = %d\n",
            __func__, dest_size, src_size, is_integer);
}

void memcpy_8_16(int16_t *p16, const int8_t *p8, int nb) {
    for (int i = 0; i < nb; i++) {
        *p16++ = *p8++;
    }
}


void memcpy_16_8(int8_t *p8, const int16_t *p16, int nb) {
    for (int i = 0; i < nb; i++) {
        *p8++ = *p16++;
    }
}


void memcpy_16_32(int32_t *p32, const int16_t *p16, int nbits, int nb) {
    int16_t mask = ~ (0xffff << nbits);
    for (int i = 0; i < nb; i++) {
        *p32++ = *p16++ & mask;
    }
}

void memcpy_32_16(int16_t *p16, const int32_t * p32, int nbits, int nb) {
    int32_t mask = ~ (0xffffffff << nbits);
    for (int i = 0; i < nb; i++) {
        *p16++ = *p32++ & mask;
    }
}

static void init_open_file(fstd_usage_info* info) {
    info->query = new_fst_query(NULL);
    info->query.num_criteria = (sizeof(info->query.criteria.fst98_meta) +
                                sizeof(info->query.criteria.fst98_reserved)) / sizeof(int32_t);
    info->next_file = -1;
    info->is_open = 1;
}


//! Reset to zeros ip1-2-3 tables and counters
//! \return Always 0
int init_ip_vals(void)
{
    for (int i = 0; i < Max_Ipvals; i++) {
        for (int j = 0; j < 3; j++) {
            ips_tab[j][i] = -1;
        }
    }

    for (int j = 0; j < 3; j++) {
        ip_nb[j] = 0;
    }

    ip1s_flag = 0;
    ip2s_flag = 0;
    ip3s_flag = 0;
    return 0;
}


//! Compares different coded values of an ip for equality
int ip_is_equal(
    //! [in] First value in the table of coded value to compare with
    int target,
    //! [in] Current ip record value to compare
    const int ip,
    //! [in] Index (1, 2, 3) representing ip1, ip2 or ip3 comparaisons
    int ind
) {
    int kind1, kind2, exp1, exp2;
    long long mantis1, mantis2;

    // ipold_0_9: table of old ip1 0-9 (mb) values encoded oldstyle with convip
    int ipold_0_9[10] = {0, 1820, 1840, 1860, 1880, 1900, 1920, 1940, 1960, 1980};

    // ind is passed in base 1, for ip1 ip2 or ip3
    ind--;

    if (target != ips_tab[ind][0]) {
        Lib_Log(APP_LIBFST, APP_ERROR, "%s: target not first element of ip table, target = %d ips_tab[%d]=%d\n", __func__, target, ind, ips_tab[ind][0]);
        return 0;
    }

    // number of elements in ip[1-2-3] table
    for (int j = 0; j < ip_nb[ind]; j++) {
        target = ips_tab[ind][j];
        if (ip == target) return 1;
        if (ip < 10)
            if (ipold_0_9[ip] == target) return 1;
        if ((ip > 32767) && (target > 32767)) {
            kind1 = (ip >> 24) & 0xF;
            kind2 = (target >> 24) & 0xF;
            if (kind1 != kind2) continue;
            exp1 = (ip >> 20) & 0xF;
            exp2 = (target >> 20) & 0xF;
            mantis1 = ip & 0xFFFFF;
            if (mantis1 > 1000000) mantis1 = -(mantis1 - 1000000);
            mantis2 = target & 0xFFFFF;
            if (mantis2 > 1000000) mantis2 = -(mantis2 - 1000000);
            // mantis1 and mantis2 must be same sign
            if ((mantis1 ^ mantis2) < 0) continue;

            if (exp1 > exp2) {
                while (exp1 > exp2) {
                    exp2++;
                    mantis2 *= 10;
                }
            } else {
                while (exp2 > exp1) {
                    exp1++;
                    mantis1 *= 10;
                }
            }

            if (labs(mantis1-mantis2) <= 1) return 1;
        }
    }
    return 0;
}

//! Determine whether the file corresponding to the given unit number is of type RSF
//! \return 1 if the file is RSF, 0 if not, or if there is an error.
int32_t c_fst_is_rsf(const int iun) {
    int32_t index_fnom;
    const int32_t status = is_rsf(iun, &index_fnom);
    return status == 1;
}

//! Reassembles split variables
void crack_std_parms(
    //! [in] Directory entry that contains the parameters
    const stdf_dir_keys * const stdf_entry,
    //! [out] Reassembled parameters
    stdf_special_parms * const cracked_parms
) {
    for (int i = 0; i <= 4; i++) {
        cracked_parms->etiket[i] = ((stdf_entry->etik15 >> ((4-i)*6)) & 0x3f) + 32;
    }

    for (int i = 5; i <= 9; i++) {
        cracked_parms->etiket[i] = ((stdf_entry->etik6a >> ((9-i)*6)) & 0x3f) + 32;
    }

    cracked_parms->etiket[10] = ((stdf_entry->etikbc >> 6) & 0x3f) + 32;
    cracked_parms->etiket[11] = (stdf_entry->etikbc  & 0x3f) + 32;
    cracked_parms->etiket[12] = '\0';

    for (int i = 0; i <= 3; i++) {
        cracked_parms->nomvar[i] = ((stdf_entry->nomvar >> ((3-i)*6)) & 0x3f) + 32;
    }

    cracked_parms->nomvar[4] = '\0';

    cracked_parms->typvar[0] = ((stdf_entry->typvar >> 6) & 0x3f) + 32;
    cracked_parms->typvar[1] = ((stdf_entry->typvar  & 0x3f)) + 32;
    cracked_parms->typvar[2] = '\0';

    cracked_parms->gtyp[0] = stdf_entry->gtyp;
    cracked_parms->gtyp[1] = '\0';

    cracked_parms->ig2 = (stdf_entry->ig2a << 16) | (stdf_entry->ig2b << 8) | stdf_entry->ig2c;

    // De-octalise the date_stamp and convert from valid date to origin date
    int run = stdf_entry->date_stamp & 0x7;
    unsigned int datexx = (stdf_entry->date_stamp >> 3) * 10 + run;
    double r8_diff = -((((double)stdf_entry->deet) * ((double)stdf_entry->npas))/3600.0);
    cracked_parms->date_valid = datexx;
    cracked_parms->date_stamp = datexx;
    int32_t ftn_date = (int32_t) datexx;
    if ((stdf_entry->deet * stdf_entry->npas) != 0) {
        // compute origin date
        incdatr_c(&ftn_date, &ftn_date, &r8_diff);
        cracked_parms->date_stamp = (int) ftn_date;
    }

    cracked_parms->aammjj = 0;
    cracked_parms->hhmmss = 0;
}


//! Translate kind code to 2 char string
//! \return corresponding kind string
char *kinds(
    //! [in] Kind code
    int kind
) {
    // Initialize table if first call
    if (kinds_table_init) {
        for(int i = 0; i <= 31; i++) {
            KindToString(i, &kind_chars[3 * i], &kind_chars[3 * i + 1]);
            kind_chars[3 * i + 2] = '\0';
        }
        kinds_table_init = 0;
    }
    return &kind_chars[3 * kind];
}


//! Prints the standard file record descriptors
void print_std_parms(
    //! [in] Directory entry that contains the descriptors
    const stdf_dir_keys * const stdf_entry,
    //! [in] Preamble string
    const char * const pre,
    //! [in] List of fields to print
    const char * const option,
    //! [in] Print header if true (!= 0)
    const int header
) {
    char cmsgp = ' '; // initialize for case where there are no missing value(s) in record
    int dat2, dat3;
    int minus3 = -3;
    int iip1, kind;
    int ig1, ig2, ig3, ig4;
    char c_level[16], pg1[7], pg2[7], pg3[8], pg4[8];
    char h_dims[23], h_dateo[16], h_stampo[10], h_datev[27], h_level[16], h_ip1[10], h_grid[32];
    char v_dims[23], v_dateo[16], v_stampo[10], v_datev[27], v_level[16], v_ip1[10], v_grid[32];
    char h_decoded[39], v_decoded[39];
    char h_nomv[5], h_typv[3], h_etiq[13], h_ip23[20], h_deet[9], h_npas[9], h_dty[9], h_siz[4];
    char v_nomv[5], v_typv[3], v_etiq[13], v_ip23[20], v_deet[9], v_npas[9], v_dty[9], v_siz[4];
    int posc, posv;

    Lib_Log(APP_LIBFST, APP_DEBUG, "%s: option=%s\n", __func__, option);
    stdf_special_parms cracked;
    crack_std_parms(stdf_entry, &cracked);

    if (header > 0) {
        // build and print header line

        if (strstr(option, "NONOMV")) {
            h_nomv[0] = '\0';
        } else {
            snprintf(h_nomv, sizeof(h_nomv), "%s", "NOMV");
        }

        if (strstr(option, "NOTYPV")) {
            h_typv[0] = '\0';
        } else {
            snprintf(h_typv, sizeof(h_typv), "%s", "TV");
        }

        if (strstr(option, "NOETIQ")) {
            h_etiq[0] = '\0';
        } else {
            snprintf(h_etiq, sizeof(h_etiq), "%s", "  ETIQUETTE ");
        }

        if (strstr(option, "NINJNK")) {
            snprintf(h_dims, sizeof(h_dims), "%s", "      NI      NJ    NK");
        } else {
            h_dims[0] = '\0';
        }

        if (strstr(option, "DATEO")) {
            // snprintf(h_dateo, "%s", "YYYYMMDD HHMMSS");
            snprintf(h_dateo, sizeof(h_dateo), "%s", "(DATE-O  h m s)");
        } else {
            h_dateo[0] = '\0';
        }

        if (strstr(option, "DATESTAMPO")) {
            snprintf(h_stampo, sizeof(h_stampo), "%s", "  STAMP-O");
        } else {
            h_stampo[0] = '\0';
        }

        if (strstr(option, "DATEV")) {
            // snprintf(h_datev, "%s", "YYYYMMDD HHMMSS     DATEV");
            snprintf(h_datev, sizeof(h_datev), "%s", "(DATE-V  h m s)   STAMP-V");
        } else {
            h_datev[0] = '\0';
        }

        if (strstr(option, "LEVEL")) {
            snprintf(h_level, sizeof(h_level), "%s", "       LEVEL   ");
        } else {
            h_level[0] = '\0';
        }

        if (strstr(option, "IPALL")) {
            snprintf(h_decoded, sizeof(h_decoded), "%s", "          DECODED IP1/IP2/IP3         ");
        } else {
            h_decoded[0] = '\0';
        }

        if (strstr(option, "IP1")) {
            snprintf(h_ip1, sizeof(h_ip1), "%s", "      IP1");
        } else {
            h_ip1[0] = '\0';
        }

        if (strstr(option, "NOIP23")) {
            h_ip23[0] = '\0';
        } else {
            snprintf(h_ip23, sizeof(h_ip23), "%s", "      IP2       IP3");
        }

        if (strstr(option, "NODEET")) {
            h_deet[0] = '\0';
        } else {
            snprintf(h_deet, sizeof(h_deet), "%s", "    DEET");
        }

        if (strstr(option, "NONPAS")) {
            h_npas[0] = '\0';
        } else {
            snprintf(h_npas, sizeof(h_npas), "%s", "    NPAS");
        }

        if (strstr(option, "NODTY")) {
            h_dty[0] = '\0';
        } else {
            snprintf(h_dty, sizeof(h_dty), "%s", "DTY ");
        }

        if (strstr(option, "DASIZ")) {
            snprintf(h_siz, sizeof(h_siz), "%s", "SIZ");
        } else {
            h_siz[0] = '\0';
        }

        if (strstr(option, "GRIDINFO")) {
            snprintf(h_grid, sizeof(h_grid), "%s", "G    XG1    XG2     XG3     XG4");
        } else {
            if (strstr(option, "IG1234")) {
                snprintf(h_grid, sizeof(h_grid), "%s", "G   IG1   IG2   IG3   IG4");
            } else {
                h_grid[0] = '\0';
            }
        }

        fprintf(stdout, "\n       %s %s %s %s %s %s %s %s %s %s %s %s %s  %s %s  %s\n\n",
                h_nomv, h_typv, h_etiq, h_dims, h_dateo, h_stampo, h_datev, h_level, h_decoded,
                h_ip1, h_ip23, h_deet, h_npas, h_dty, h_siz, h_grid);
        // fprintf(stdout, "\n       NOMV TV ETIQUETTE       NI    NJ    NK %s %s %s %s %s   IP2   IP3     DEET     NPAS  DTY  %s\n\n", h_dateo, h_stampo, h_datev, h_level, h_ip1, h_grid);
    } // if (header)


    if (strstr(option, "NONOMV")) {
        v_nomv[0] = '\0';
    } else {
        snprintf(v_nomv, sizeof(v_nomv), "%4s", cracked.nomvar);
    }

    if (strstr(option, "NOTYPV")) {
        v_typv[0] = '\0';
    } else {
        snprintf(v_typv, sizeof(v_typv), "%2s", cracked.typvar);
    }

    if (strstr(option, "NOETIQ")) {
        v_etiq[0] = '\0';
    } else {
        snprintf(v_etiq, sizeof(v_etiq), "%12s", cracked.etiket);
    }

    if (strstr(option, "NINJNK")) {
        snprintf(v_dims, sizeof(v_dims), " %7d %7d %5d", stdf_entry->ni, stdf_entry->nj, stdf_entry->nk);
    } else {
        v_dims[0] = '\0';
    }

    if (strstr(option, "DATEO")) {
        newdate_c(&cracked.date_stamp, &dat2, &dat3, &minus3);
        snprintf(v_dateo, sizeof(v_dateo), "%08d %06d", dat2, dat3/100);
    } else {
        v_dateo[0] = '\0';
    }

    if (strstr(option, "DATESTAMPO")) {
        snprintf(v_stampo, sizeof(v_stampo), "%09d", cracked.date_stamp);
    } else {
        v_stampo[0] = '\0';
    }

    if (strstr(option, "DATEV")) {
        newdate_c(&cracked.date_valid, &dat2, &dat3, &minus3);
        if (cracked.date_valid < -1) {
            snprintf(v_datev, sizeof(v_datev), "%08d %06d %10d", dat2, dat3/100, cracked.date_valid);
        } else {
            snprintf(v_datev, sizeof(v_datev), "%08d %06d %09d", dat2, dat3/100, cracked.date_valid);
        }
    } else {
        v_datev[0] = '\0';
    }

    v_level[0] = '\0';
    v_decoded[0] = '\0';
    if ( strstr(option, "LEVEL") || strstr(option, "IPALL") ) {
        iip1 = stdf_entry->ip1;

        if (! FstCanTranslateName(cracked.nomvar)) {
            snprintf(c_level, sizeof(c_level), "%12d   ", iip1);
            if (strstr(option, "LEVEL")) snprintf(v_level, sizeof(v_level), "%15s", "     -----     ");
            if (strstr(option, "IPALL")) snprintf(v_decoded, sizeof(v_decoded), "[%10d] [%10d] [%10d]", stdf_entry->ip1, stdf_entry->ip2, stdf_entry->ip3);
        } else {
            // not a special variable
            if (strstr(option, "LEVEL")) {
                // good old level option
                int mode = -1;
                int flag = 1;
                float level;
                f77name(convip_plus)(&iip1, &level, &kind, &mode, c_level, &flag, (F2Cl) 15);
                c_level[15] = '\0';
                // blank initialisation
                snprintf(v_level, sizeof(v_level), "%s", "               ");
                posc = 14;
                posv = 14;
                // skip blanks and right justify string
                while ((posc >= 0) && (isspace(c_level[posc]))) {
                    posc--;
                }
                if (isdigit(c_level[posc])) {
                    posv -= 3;
                }
                while ((posv >= 0) && (posc >= 0)) {
                    v_level[posv] = c_level[posc];
                    posv--;
                    posc--;
                }
            }
            if (strstr(option, "IPALL")) {
                // full IP1/IP2/IP3 triplet decoding
                float p1, p2, p3;
                int kind1, kind2, kind3;
                int StatusIP = ConvertIPtoPK(&p1, &kind1, &p2, &kind2, &p3, &kind3, stdf_entry->ip1, stdf_entry->ip2, stdf_entry->ip3);
                if (kind1 < 0 || kind2 < 0 || kind3 < 0 || (StatusIP & CONVERT_ERROR) ) {
                    // decode error somewhere
                    kind1 = 15; kind2 = 15; kind3 = 15;  // integer code P = IP
                    p1 = stdf_entry->ip1; p2 = stdf_entry->ip2; p3 = stdf_entry->ip3;
                }
                kind1 &= 0x1F; kind2 &= 0x1F; kind3 &= 0x1F;   // force modulo 32
                snprintf(v_decoded, sizeof(v_decoded), "%10g%s %10g%s %10g%s", p1, kinds(kind1), p2, kinds(kind2), p3, kinds(kind3));
            }
        } // special variable, no decoding
    }

    if (strstr(option, "IP1")) {
        snprintf(v_ip1, sizeof(v_ip1), "%9d", stdf_entry->ip1);
    } else {
        v_ip1[0] = '\0';
    }

    if (strstr(option, "NOIP23")) {
        v_ip23[0] = '\0';
    } else {
        snprintf(v_ip23, sizeof(v_ip23), "%9d %9d", stdf_entry->ip2, stdf_entry->ip3);
    }

    if (strstr(option, "NODEET")) {
        v_deet[0] = '\0';
    } else {
        snprintf(v_deet, sizeof(v_deet), "%8d", stdf_entry->deet);
    }

    if (strstr(option, "NONPAS")) {
        v_npas[0] = '\0';
    } else {
        snprintf(v_npas, sizeof(v_npas), "%8d", stdf_entry->npas);
    }

    // m will be added to data type if there are missing values in record
    if (stdf_entry->datyp & FSTD_MISSING_FLAG) cmsgp = 'm';
    if (strstr(option, "NODTY")) {
        v_dty[0] = '\0';
    } else {
        // force lower case data type code if compressed
        const char cdt[9] = {'X', 'R', 'I', 'C', 'S', 'E', 'F', 'A', 'Z'};
        if (stdf_entry->datyp > 128) {
            // suppress bits for 64 and 128
            snprintf(v_dty, sizeof(v_dty), "%1c%1c%2d %3d", tolower(cdt[stdf_entry->datyp&0x3F]), cmsgp, stdf_entry->nbits, stdf_entry->dasiz);
        } else {
            // suppress bits for 64 and 128
            snprintf(v_dty, sizeof(v_dty), "%1c%1c%2d %3d", cdt[stdf_entry->datyp&0x3F], cmsgp, stdf_entry->nbits, stdf_entry->dasiz);
        }
    }

    if (strstr(option, "DASIZ")) {
        snprintf(v_siz, sizeof(v_siz), "%3d", stdf_entry->dasiz);
    } else {
        v_siz[0] = '\0';
    }

    if (strstr(option, "GRIDINFO")) {
        ig1 = stdf_entry->ig1; ig2 = cracked.ig2;
        ig3 = stdf_entry->ig3; ig4 = stdf_entry->ig4;
        igapg_c(cracked.gtyp, pg1, pg2, pg3, pg4, &ig1, &ig2, &ig3, &ig4);
        pg1[6] = '\0'; pg2[6] = '\0'; pg3[7] = '\0'; pg4[7] = '\0';
        snprintf(v_grid, sizeof(v_grid), "%1s %6s %6s %7s %7s", cracked.gtyp, pg1, pg2, pg3, pg4);
    } else {
        if (strstr(option, "IG1234")) {
            snprintf(v_grid, sizeof(v_grid), "%1s %5d %5d %5d %5d", cracked.gtyp, stdf_entry->ig1, cracked.ig2, stdf_entry->ig3, stdf_entry->ig4);
        } else {
            v_grid[0] = '\0';
        }
    }

    if (header==-1) {
       Lib_Log(APP_LIBFST, APP_ALWAYS, "%s %s %s %s %s %s %s %s %s %s %s %s %s %s  %s %s  %s\n",
           pre, v_nomv, v_typv, v_etiq, v_dims, v_dateo, v_stampo, v_datev, v_level, v_decoded, v_ip1, v_ip23, v_deet, v_npas, v_dty, v_siz, v_grid);
    } else {
       fprintf(stdout, "%s %s %s %s %s %s %s %s %s %s %s %s %s %s  %s %s  %s\n",
           pre, v_nomv, v_typv, v_etiq, v_dims, v_dateo, v_stampo, v_datev, v_level, v_decoded, v_ip1, v_ip23, v_deet, v_npas, v_dty, v_siz, v_grid);
    }
}


//! \copydoc c_fstapp
//! XDF version
int c_fstapp_xdf(
    //! [in] Unit number associated to the file
    int iun,
    //! [in] Kept for backward compatibility, but unused
    char *option
) {
    (void)option; // unused

    int index_fnom = get_fnom_index(iun);
    if (index_fnom == -1) {
        Lib_Log(APP_LIBFST, APP_ERROR, "%s: file (unit=%d) is not connected with fnom\n", __func__, iun);
        return ERR_NO_FNOM;
    }

    int index = file_index_xdf(iun);
    if (index  == ERR_NO_FILE) {
        Lib_Log(APP_LIBFST, APP_ERROR, "%s: file (unit=%d) is not open\n", __func__, iun);
        return ERR_NO_FILE;
    }

    file_table_entry * fte = file_table[index];

    if (!fte->xdf_seq) {
        Lib_Log(APP_LIBFST, APP_WARNING, "%s: ile (unit=%d) is not sequential\n", __func__, iun);
        return ERR_BAD_FTYPE;
    }

    int end_of_file = 0;
    int width = W64TOWD(fte->primary_len);

    if (fte->fstd_vintage_89) {
        while (!end_of_file) {
            int nw = c_waread2(iun, fte->head_keys, fte->cur_addr, width);
            xdf_record_header * header = (xdf_record_header *) fte->head_keys;
            if (nw < width) {
                end_of_file = 1;
                header->idtyp = 127;
                header->lng = 1;
                break;
            }
            seq_dir_keys * seq_entry = (seq_dir_keys *) fte->head_keys;
            if (seq_entry->eof > 0) {
                header->idtyp = 112 + seq_entry->eof;
                header->lng = 1;
                end_of_file = 1;
                break;
            }
            header->lng = ((seq_entry->lng +3) >> 2) + 15;
            fte->cur_addr += W64TOWD(header->lng);
        }
    } else {
        while (!end_of_file) {
            int nw = c_waread2(iun, fte->head_keys, fte->cur_addr, width);
            xdf_record_header * header = (xdf_record_header *) fte->head_keys;
            if (nw < W64TOWD(1)) {
                end_of_file = 1;
                header->idtyp = 127;
                header->lng = 1;
                break;
            }
            if ((header->idtyp >= 112) && (header->idtyp <= 127)) {
                end_of_file = 1;
                break;
            }
            fte->cur_addr += W64TOWD(header->lng);
        }
    }
    fte->nxtadr = fte->cur_addr;
    return 0;
}


//! Position at the end of a sequential file for an append
int c_fstapp(
    //! [in] Unit number associated to the file
    int iun,
    //! [in] Kept for backward compatibility, but unused
    char *option
) {
    int index_fnom;
    const int rsf_status = is_rsf(iun, &index_fnom);

    if (index_fnom == -1) {
        Lib_Log(APP_LIBFST, APP_ERROR, "%s: file (unit=%d) is not connected with fnom\n", __func__, iun);
        return ERR_NO_FNOM;
    }

    if (rsf_status == 1) {
        Lib_Log(APP_LIBFST, APP_WARNING, "%s: \"Append\" operation does not make sense on an RSF file (unit %d).\n",
                __func__, iun);
        return 0;
    }
    else if (rsf_status == 0) {
        return c_fstapp_xdf(iun, option);
    }

    return rsf_status;
}

//! Checkpoint. Clear buffers, rewrite headers.
//! XDF version.
int c_fstckp_xdf(
    //! [in] Unit number associated to the file
    const int iun
) {
    const int index_fnom = get_fnom_index(iun);
    if (index_fnom == -1) {
        Lib_Log(APP_LIBFST, APP_ERROR, "%s: file (unit=%d) is not connected with fnom\n", __func__, iun);
        return ERR_NO_FNOM;
    }

    const int index = file_index_xdf(iun);
    if (index == ERR_NO_FILE) {
        Lib_Log(APP_LIBFST, APP_ERROR, "%s: file (unit=%d, index=%d) is not open\n", __func__, iun, index);
        return ERR_NO_FILE;
    }

    // checkpoint mode, not a complete close
    xdf_checkpoint = 1;
    const int ier = c_xdfcls(iun);
    return ier;
}

//! Checkpoint. Clear buffers, rewrite headers.
int c_fstckp(
    //! [in] Unit number associated to the file
    const int iun
) {
    int index_fnom;
    const int rsf_status = is_rsf(iun, &index_fnom);

    if (index_fnom == -1) {
        Lib_Log(APP_LIBFST, APP_ERROR, "%s: file (unit=%d) is not connected with fnom\n", __func__, iun);
        return ERR_NO_FNOM;
    }

    if (rsf_status == 1) {
        return c_fstckp_rsf(iun, index_fnom);
    }
    else if (rsf_status == 0) {
        return c_fstckp_xdf(iun);
    }

    return rsf_status;
}


//! Set data size for the elements passed to fstecr and fstlir
int c_fst_data_length(
    //! [in] Data length and kind
    //! | length_type | Description       |
    //! | ----------: | :---------------- |
    //! |           1 | Byte              |
    //! |           2 | Short (16 bits)   |
    //! |           4 | Regular (32 bits) |
    //! |           8 | Double (64 bits)  |
    const int length_type
) {
    switch (length_type) {
        case 1:
            xdf_byte = 1;
            xdf_short = 0;
            xdf_double = 0;
            break;

        case 2:
            xdf_byte = 0;
            xdf_short = 1;
            xdf_double = 0;
            break;

        case 4:
            xdf_byte = 0;
            xdf_short = 0;
            xdf_double = 0;
            break;

        case 8:
            xdf_byte = 0;
            xdf_short = 0;
            xdf_double = 1;
            break;

        default:
            Lib_Log(APP_LIBFST, APP_ERROR, "%s: c_fst_data_length invalid length type=%d\n", __func__, length_type);
            xdf_byte = 0;
            xdf_short = 0;
            xdf_double = 0;
            break;
    }
    return 0;
}

//! \copydoc c_fstecr
//! XDF version
int c_fstecr_xdf(
    //! [in] Field to write to the file
    const void * const field_in,
    //! [in] Work field (kept for backward compatibility)
    const void * const work,
    //! [in] Number of bits kept for the elements of the field
    const int npak,
    //! [in] Unit number associated to the file in which to write the field
    const int iun,
    //! [in] Origin date timestamp
    const int dateo,
    //! [in] Length of the time steps in seconds
    const int deet,
    //! [in] Time step number
    const int npas,
    //! [in] First dimension of the data field
    const int ni,
    //! [in] Second dimension of the data field
    const int nj,
    //! [in] Third dimension of the data field
    const int nk,
    //! [in] Vertical level
    const int ip1,
    //! [in] Forecast hour
    const int ip2,
    //! [in] User defined identifier
    const int ip3,
    //! [in] Type of field (forecast, analysis, climatology)
    const char * const in_typvar,
    //! [in] Variable name
    const char * const in_nomvar,
    //! [in] Label
    const char * const in_etiket,
    //! [in] Type of geographical projection
    const char * const in_grtyp,
    //! [in] First grid descriptor
    const int ig1,
    //! [in] Second grid descriptor
    const int ig2,
    //! [in] Third grid descriptor
    const int ig3,
    //! [in] Fourth grid descriptor
    const int ig4,
    //! [in] Data type of elements
    const int in_datyp_ori,
    //! [in] Rewrite existing record, skip or append otherwise
    const int rewrit
) {
    (void)work; // unused

    float* field_f = NULL; // float version of the data
    uint32_t* field_missing = NULL; // data with missing values transformed

    // will be cancelled later if not supported or no missing values detected
    //  missing value feature used flag
    int is_missing = in_datyp_ori & FSTD_MISSING_FLAG;
    // suppress missing value flag (64)
    int in_datyp = in_datyp_ori & ~FSTD_MISSING_FLAG;
    if (is_type_complex(in_datyp)) {
        if (in_datyp_ori != FST_TYPE_COMPLEX) {
           Lib_Log(APP_LIBFST, APP_WARNING, "%s: compression and/or missing values not supported, data type %d reset to %d (complex)\n",
                __func__, in_datyp_ori, 8);
        }
        // missing values not supported for complex type
        is_missing = 0;
        // extra compression not supported for complex type
        in_datyp = FST_TYPE_COMPLEX;
    }

    // 512+256+32+1 no interference with turbo pack (128) and missing value (64) flags
    int datyp = in_datyp == FST_TYPE_MAGIC ? 1 : in_datyp;

    PackFunctionPointer packfunc = (xdf_double) || (in_datyp == FST_TYPE_MAGIC) ? &compact_p_double : &compact_p_float;
    double dmin = 0.0;
    double dmax = 0.0;

    int index_fnom = get_fnom_index(iun);
    if (index_fnom == -1) {
        Lib_Log(APP_LIBFST, APP_ERROR, "%s: file (unit=%d) is not connected with fnom\n", __func__, iun);
        return ERR_NO_FNOM;
    }

    int index = file_index_xdf(iun);
    if (index == ERR_NO_FILE) {
        Lib_Log(APP_LIBFST, APP_ERROR, "%s: file (unit=%d) is not open\n", __func__, iun);
        return ERR_NO_FILE;
    }

    file_table_entry * fte = file_table[index];

    if (! fte->cur_info->attr.std) {
        Lib_Log(APP_LIBFST, APP_ERROR, "%s: file (\"%s\", unit=%d) is not a RPN standard file\n", __func__, fte->cur_info->file_name, iun);
        return ERR_NO_FILE;
    }

    if (fte->fstd_vintage_89) {
        Lib_Log(APP_LIBFST, APP_FATAL, "%s: can not write (\"%s\", unit=%d) on an old (version 89) RPN standard file\n", __func__,
            fte->cur_info->file_name, iun);
        return ERR_NO_WRITE;
    }

    if (fte->cur_info->attr.read_only) {
        Lib_Log(APP_LIBFST, APP_ERROR, "%s: file (\"%s\", unit=%d) not open with write permission\n", __func__, fte->cur_info->file_name, iun);
        return ERR_NO_WRITE;
    }

    int nbits;
    if (npak == 0) {
        nbits = FTN_Bitmot;
    } else {
        nbits = (npak < 0) ? -npak : Max(1, FTN_Bitmot / Max(1, npak));
    }
    int _nk = Max(1, nk);

    if (base_fst_type(datyp) == FST_TYPE_REAL_IEEE && nbits < 16) {
        Lib_Log(APP_LIBFST, APP_ERROR, "%s: Writing a truncated IEEE float with less than 16 bits is not allowed\n",
                __func__);
        return ERR_BAD_DATYP;
    }

    if ( (in_datyp_ori == (FST_TYPE_REAL_IEEE | FST_TYPE_TURBOPACK)) && (nbits > 32) ) {
        Lib_Log(APP_LIBFST, APP_WARNING, "%s: extra compression not supported for IEEE when nbits > 32, "
                "data type FST_TYPE_REAL_IEEE | FST_TYPE_TURBOPACK (%d) reset to FST_TYPE_REAL_IEEE (%d) (IEEE)\n", __func__,
                FST_TYPE_REAL_IEEE | FST_TYPE_TURBOPACK, FST_TYPE_REAL_IEEE);
        // extra compression not supported
        in_datyp = FST_TYPE_REAL_IEEE;
        datyp = in_datyp;
    }

    if (is_type_turbopack(datyp) && _nk > 1) {
        Lib_Log(APP_LIBFST, APP_WARNING, "%s: Turbo compression not supported for 3D data. We will disable it.\n", __func__);
        datyp &= FST_TYPE_TURBOPACK;
    }

    if ((base_fst_type(in_datyp) == FST_TYPE_REAL_OLD_QUANT) && ((nbits == 31) || (nbits == 32)) && !image_mode_copy) {
        // R32 to E32 automatic conversion
        datyp = FST_TYPE_REAL_IEEE;
        if (is_type_turbopack(in_datyp)) datyp |= FST_TYPE_TURBOPACK;
        nbits = 32;
    }

    // flag 64 bit IEEE (type 5 or 8)
    int IEEE_64 = 0;
    // 64 bits real IEEE
    if ( (base_fst_type(in_datyp) == FST_TYPE_REAL_IEEE) && (nbits == 64) ) IEEE_64 = 1;
    // 64 bits complex IEEE
    if ( is_type_complex(in_datyp) && (nbits == 64) ) IEEE_64 = 1;

    // validate range of arguments
    VALID(ni, 1, NI_MAX, "ni")
    VALID(nj, 1, NJ_MAX, "nj")
    VALID(_nk, 1, NK_MAX, "nk")
    VALID(deet, 0, DEET_MAX, "deet")
    VALID(npas, 0, NPAS_MAX, "npas")
    VALID(nbits, 1, NBITS_MAX, "nbits")
    VALID(ig1, 0, IG1_MAX, "ig1")
    VALID(ig2, 0, IG2_MAX, "ig2")
    VALID(ig3, 0, IG3_MAX, "ig3")
    VALID(ig4, 0, IG4_MAX, "ig4")
    VALID(ip1, 0, IP1_MAX, "ip1")
    VALID(ip2, 0, IP2_MAX, "ip2")
    VALID(ip3, 0, IP3_MAX, "ip3")
    VALID(ni * nj * _nk * nbits / FTN_Bitmot, 0, MAX_RECORD_LENGTH, "record length > 128MB");

    const uint32_t datev = get_valid_date32(dateo, deet, npas);

    if ((npak == 0) || (npak == 1)) {
        // no compaction
        datyp = FST_TYPE_BINARY;
    }

    // allocate and initialize a buffer interface for xdfput
    // an extra 512 bytes are allocated for cluster alignment purpose (seq)

    if (! image_mode_copy) {
        for (int i = 0; i < nb_remap; i++) {
            if (datyp == remap_table[0][i]) {
                datyp = remap_table[1][i];
                // printf("Debug+ remapping %d to %d\n", remap_table[0][i], datyp);
            }
        }
    }

    if (is_type_real(datyp) && nbits > 32) {
        datyp = FST_TYPE_REAL_IEEE;
        nbits = 64;
    }

    if ((base_fst_type(datyp) == FST_TYPE_REAL)) { 
        if (nbits > 24) {
            if (! dejafait_xdf_1) {
                Lib_Log(APP_LIBFST, APP_INFO, "%s: nbits > 24, writing E32 instead of F%2d\n", __func__, nbits);
                dejafait_xdf_1 = 1;
            }
            datyp = FST_TYPE_REAL_IEEE | (is_type_turbopack(datyp) ? FST_TYPE_TURBOPACK : 0);
            nbits = 32;
        }
        else if (nbits > 16) {
            if (! dejafait_xdf_2) {
                Lib_Log(APP_LIBFST, APP_INFO, "%s: nbits > 16, writing R%2d instead of F%2d\n", __func__, nbits, nbits);
                dejafait_xdf_2 = 1;
            }
            datyp = FST_TYPE_REAL_OLD_QUANT; // No turbopack for R with >16 bits
        }
    }

    // no extra compression if nbits > 16 (except for IEEE reals)
    if ((nbits > 16) && (datyp != (FST_TYPE_REAL_IEEE | FST_TYPE_TURBOPACK))) datyp = base_fst_type(datyp);

    // Determine data_nbits (uncompressed datatype size)
    int8_t data_nbits = 0;
    if (is_type_real(datyp) || is_type_complex(datyp)) {
        data_nbits = (xdf_double || IEEE_64) ? 64 : 32;
        if (data_nbits == 64) {
            if (nbits <= 32) {
                // We convert now from double to float
                data_nbits = 32;
                field_f = malloc(ni * nj * _nk * sizeof(float));
                const double * const field_d = field_in;
                for (int i = 0; i < ni * nj * _nk; i++) {
                    field_f[i] = (float)field_d[i];
                }
            }
            else if (nbits != 64) {
                Lib_Log(APP_LIBFST, APP_WARNING, "%s: Requested %d packed bits for 64-bit reals, but we can only do"
                        " 64 or less than 32. Will store 64 bits.\n", __func__, nbits);
                nbits = 64;
            }
        }
    } else if (is_type_integer(datyp)) {
        data_nbits = xdf_byte   ?  8 :
                     xdf_short  ? 16 :
                     xdf_double ? 64 :
                                  32;
    }

    int minus_nbits = -nbits;
    int header_size;
    int stream_size;
    int nw;
    switch (datyp) {
        case FST_TYPE_REAL: {
            int p1out;
            int p2out;
            c_float_packer_params(&header_size, &stream_size, &p1out, &p2out, ni * nj * _nk);
            nw = ((header_size + stream_size) * 8 + 63) / 64;
            header_size /= sizeof(int32_t);
            stream_size /= sizeof(int32_t);
            break;
        }

        case FST_TYPE_COMPLEX:
            nw = 2 * ((ni * nj * _nk * nbits + 63) / 64);
            break;

        case FST_TYPE_REAL_OLD_QUANT | FST_TYPE_TURBOPACK:
            // 120 bits (floatpack header)+8, 32 bits (extra header)
            nw = (ni * nj * _nk * Max(nbits, 16) + 128 + 32 + 63) / 64;
            break;

        case FST_TYPE_UNSIGNED | FST_TYPE_TURBOPACK:
            // 32 bits (extra header)
            nw = (ni * nj * _nk * Max(nbits, 16) + 32 + 63) / 64;
            break;

        case FST_TYPE_REAL | FST_TYPE_TURBOPACK: {
            int p1out;
            int p2out;
            c_float_packer_params(&header_size, &stream_size, &p1out, &p2out, ni * nj * _nk);
            nw = ((header_size + stream_size) * 8 + 32 + 63) / 64;
            stream_size /= sizeof(int32_t);
            header_size /= sizeof(int32_t);
            break;
        }

        default:
            nw = (ni * nj * _nk * nbits + 120 + 63) / 64;
            break;
    }

    nw = W64TOWD(nw);

    int keys_len = W64TOWD((fte->primary_len + fte->info_len));
    buffer_interface_ptr buffer = (buffer_interface_ptr) malloc((10 + keys_len + nw + 128) * sizeof(int));
    if (buffer) {
        memset(buffer, 0, (10 + keys_len + nw + 128) * sizeof(int));
    } else {
        Lib_Log(APP_LIBFST, APP_FATAL, "%s: memory is full, was trying to allocate %ld bytes\n", __func__, (10 + keys_len + nw + 128) * sizeof(int));
        return ERR_MEM_FULL;
    }
    const int bitmot = 32;
    buffer->nwords = 10 + keys_len + nw;
    buffer->nbits = (keys_len + nw) * bitmot;
    buffer->record_index = RECADDR;
    buffer->data_index = buffer->record_index + W64TOWD((fte->primary_len + fte->info_len));
    buffer->iun = iun;
    buffer->aux_index = buffer->record_index + W64TOWD(fte->primary_len);
    buffer->data[buffer->aux_index] = 0;
    buffer->data[buffer->aux_index+1] = 0;

    char typvar[FST_TYPVAR_LEN];
    copy_record_string(typvar, in_typvar, FST_TYPVAR_LEN);
    char nomvar[FST_NOMVAR_LEN];
    copy_record_string(nomvar, in_nomvar, FST_NOMVAR_LEN);
    char etiket[FST_ETIKET_LEN];
    copy_record_string(etiket, in_etiket, FST_ETIKET_LEN);
    char grtyp[FST_GTYP_LEN];
    copy_record_string(grtyp, in_grtyp, FST_GTYP_LEN);

    // set stdf_entry to address of buffer->data for building keys
    stdf_dir_keys * stdf_entry = (stdf_dir_keys *) &(buffer->data);
    stdf_entry->deleted = 0;
    stdf_entry->select = 1;
    stdf_entry->lng = 0xffffff;
    stdf_entry->addr = 0xffffffff;
    stdf_entry->deet = deet;
    stdf_entry->nbits = nbits;
    stdf_entry->ni = ni;
    stdf_entry->gtyp = grtyp[0];
    stdf_entry->nj = nj;
    // propagate missing values flag
    stdf_entry->datyp = datyp | is_missing;
    // this value may be changed later in the code to eliminate improper flags
    stdf_entry->nk = _nk;
    stdf_entry->ubc = 0;
    stdf_entry->npas = npas;
    stdf_entry->pad7 = 0;
    stdf_entry->ig4 = ig4;
    stdf_entry->ig2a = ig2 >> 16;
    stdf_entry->ig1 = ig1;
    stdf_entry->ig2b = ig2 >> 8;
    stdf_entry->ig3 = ig3;
    stdf_entry->ig2c = ig2 & 0xff;
    stdf_entry->etik15 =
        (ascii6(etiket[0]) << 24) |
        (ascii6(etiket[1]) << 18) |
        (ascii6(etiket[2]) << 12) |
        (ascii6(etiket[3]) <<  6) |
        (ascii6(etiket[4]));
    stdf_entry->pad1 = 0;
    stdf_entry->etik6a =
        (ascii6(etiket[5]) << 24) |
        (ascii6(etiket[6]) << 18) |
        (ascii6(etiket[7]) << 12) |
        (ascii6(etiket[8])<<  6) |
        (ascii6(etiket[9]));
    stdf_entry->pad2 = 0;
    stdf_entry->etikbc =
        (ascii6(etiket[10]) <<  6) |
        (ascii6(etiket[11]));
    stdf_entry->typvar =
        (ascii6(typvar[0]) <<  6) |
        (ascii6(typvar[1]));
    stdf_entry->pad3 = 0;
    stdf_entry->nomvar =
        (ascii6(nomvar[0]) << 18) |
        (ascii6(nomvar[1]) << 12) |
        (ascii6(nomvar[2]) <<  6) |
        (ascii6(nomvar[3]));
    stdf_entry->dasiz = data_nbits;
    stdf_entry->ip1 = ip1;
    stdf_entry->levtyp = 0;
    stdf_entry->ip2 = ip2;
    stdf_entry->pad5 = 0;
    stdf_entry->ip3 = ip3;
    stdf_entry->pad6 = 0;
    stdf_entry->date_stamp = stamp_from_date(datev);

    int handle = 0;

    if ((rewrit) && (!fte->xdf_seq)) {
        // find handle for rewrite operation
        int niout, njout, nkout;

        match_fn old_file_filter = xdf_set_file_filter(iun, NULL); // Remove file filter for this particular search
        handle = c_fstinf(iun, &niout, &njout, &nkout, -1, etiket, ip1, ip2, ip3, typvar, nomvar);
        xdf_set_file_filter(iun, old_file_filter); // Put file filter back

        if (handle < 0) {
            // append mode for xdfput
            handle = 0;
        } else if (rewrit==FST_SKIP) {
            if (field_f != NULL) free(field_f);
            free(buffer);
            return 0; // Success
        }
    }

    const uint32_t * field_u32 = field_in;
    if (field_f != NULL) {
        field_u32 = (uint32_t*)field_f;
        packfunc = &compact_p_float; // Use corresponding packing function
    }
    if (image_mode_copy) {
        // no pack/unpack, used by editfst
        if (is_type_turbopack(datyp)) {
            // first element is length
            int lngw = field_u32[0];
            // fprintf(stderr, "Debug+ datyp=%d ecriture mode image lngw=%d\n", datyp, lngw);
            buffer->nbits = (keys_len + lngw) * bitmot;
            for (int i = 0; i < lngw + 1; i++) {
                buffer->data[keys_len + i] = field_u32[i];
            }
        } else {
            int lngw;
            if (datyp == FST_TYPE_REAL) {
                int p1out;
                int p2out;
                c_float_packer_params(&header_size, &stream_size, &p1out, &p2out, ni * nj * _nk);
                lngw = (header_size + stream_size) * 8;
            } else {
                lngw = ni * nj * _nk * nbits;
            }
            if (datyp == FST_TYPE_REAL_OLD_QUANT) lngw += 120;
            if (datyp == FST_TYPE_CHAR) lngw = ni * nj * 8;
            lngw = (lngw + bitmot - 1) / bitmot;
            for (int i = 0; i < lngw; i++) {
                buffer->data[keys_len+i] = field_u32[i];
            }
        }
    } else {
        // not image mode copy
        // time to fudge field if missing value feature is used

        // number of bytes per data item
        int sizefactor = 4;
        if (xdf_byte)  sizefactor = 1;
        if (xdf_short) sizefactor = 2;
        if (xdf_double | IEEE_64) sizefactor = 8;
        // put appropriate values into field after allocating it
        if (is_missing) {
            field_missing = malloc(ni * nj * _nk * sizefactor);
            if (EncodeMissingValue(field_missing, field_in, ni * nj * _nk, in_datyp, sizefactor*8, nbits) > 0) {
                field_u32 = field_missing;
                if (field_f != NULL) packfunc = &compact_p_double;
            } else {
                field_u32 = field_f == NULL ? field_in : field_f;
                Lib_Log(APP_LIBFST, APP_INFO, "%s: NO missing value, data type %d reset to %d\n", __func__, stdf_entry->datyp, datyp);
                // cancel missing data flag in data type
                stdf_entry->datyp = datyp;
                is_missing = 0;
            }
        }

        switch (datyp) {

            case FST_TYPE_BINARY:
            case FST_TYPE_BINARY | FST_TYPE_TURBOPACK: {
                // transparent mode
                if (is_type_turbopack(datyp)) {
                    Lib_Log(APP_LIBFST,APP_WARNING, "%s: extra compression not available, data type %d reset to FST_TYPE_BINARY (%d)\n", __func__, stdf_entry->datyp, FST_TYPE_BINARY);
                    datyp = FST_TYPE_BINARY;
                    stdf_entry->datyp = datyp;
                }
                int lngw = ((ni * nj * _nk * nbits) + bitmot - 1) / bitmot;
                for (int i = 0; i < lngw; i++) {
                    buffer->data[keys_len+i] = field_u32[i];
                }
                break;
            }

            case FST_TYPE_REAL_OLD_QUANT:
            case FST_TYPE_REAL_OLD_QUANT | FST_TYPE_TURBOPACK: {
                // floating point
                double tempfloat = 99999.0;
                if (is_type_turbopack(datyp) && (nbits <= 16)) {
                    // use an additional compression scheme
                    // nbits>64 flags a different packing
                    packfunc(field_u32, &(buffer->data[keys_len+1]), &(buffer->data[keys_len+5]),
                        ni * nj * _nk, nbits + 64 * Max(16, nbits), 0, xdf_stride, 0, &tempfloat, &dmin, &dmax);
                    int compressed_lng = armn_compress((unsigned char *)&(buffer->data[keys_len+5]), ni, nj, _nk, nbits, 1, 1);
                    if (compressed_lng < 0) {
                        stdf_entry->datyp = FST_TYPE_REAL_OLD_QUANT;
                        packfunc(field_u32, &(buffer->data[keys_len]), &(buffer->data[keys_len+3]),
                            ni * nj * _nk, nbits, 24, xdf_stride, 0, &tempfloat, &dmin, &dmax);
                    } else {
                        int nbytes = 16 + compressed_lng;
                        // fprintf(stderr, "Debug+ apres armn_compress nbytes=%d\n", nbytes);
                        nw = (nbytes * 8 + 63) / 64;
                        nw = W64TOWD(nw);
                        buffer->data[keys_len] = nw;
                        // fprintf(stderr, "Debug+ pack buffer->data[keys_len]=%d\n", buffer->data[keys_len]);
                        buffer->nbits = (keys_len + nw) * bitmot;
                    }
                } else {
                    packfunc(field_u32, &(buffer->data[keys_len]), &(buffer->data[keys_len+3]),
                        ni * nj * _nk, nbits, 24, xdf_stride, 0, &tempfloat, &dmin, &dmax);
                }
                break;
            }

            case FST_TYPE_UNSIGNED:
            case FST_TYPE_UNSIGNED | FST_TYPE_TURBOPACK: {
                // integer, short integer or byte stream
                int offset = is_type_turbopack(datyp) ? 1 :0;
                if (is_type_turbopack(datyp)) {
                    if (xdf_short) {
                        stdf_entry->nbits = Min(16, nbits);
                        nbits = stdf_entry->nbits;
                        memcpy(&(buffer->data[keys_len+offset]), field_u32, ni * nj * _nk * 2);
                    } else if (xdf_byte) {
                        stdf_entry->nbits = Min(8, nbits);
                        nbits = stdf_entry->nbits;
                        memcpy_8_16((int16_t *)&(buffer->data[keys_len+offset]), (int8_t *)field_u32, ni * nj * _nk);
                    } else {
                        memcpy_32_16((short *)&(buffer->data[keys_len+offset]), (const int32_t *)field_u32, nbits, ni * nj * _nk);
                    }
                    int compressed_lng = armn_compress((unsigned char *)&(buffer->data[keys_len+offset]), ni, nj, _nk, nbits, 1, 0);
                    if (compressed_lng < 0) {
                        stdf_entry->datyp = FST_TYPE_UNSIGNED;
                        compact_p_integer(field_u32, (void *) NULL, &(buffer->data[keys_len + offset]),
                            ni * nj * _nk, nbits, 0, xdf_stride, 0);
                    } else {
                        int nbytes = 4 + compressed_lng;
                        // fprintf(stderr, "Debug+ fstecr armn_compress compressed_lng=%d\n", compressed_lng);
                        nw = (nbytes * 8 + 63) / 64;
                        nw = W64TOWD(nw);
                        buffer->data[keys_len] = nw;
                        buffer->nbits = (keys_len + nw) * bitmot;
                    }
                } else {
                    if (xdf_short) {
                        stdf_entry->nbits = Min(16, nbits);
                        nbits = stdf_entry->nbits;
                        compact_p_short(field_u32, (void *) NULL, &(buffer->data[keys_len+offset]),
                            ni * nj * _nk, nbits, 0, xdf_stride);
                    } else if (xdf_byte) {
                        compact_p_char(field_u32, (void *) NULL, &(buffer->data[keys_len]),
                            ni * nj * _nk, Min(8, nbits), 0, xdf_stride);
                        stdf_entry->nbits = Min(8, nbits);
                        nbits = stdf_entry->nbits;
                    } else {
                        compact_p_integer(field_u32, (void *) NULL, &(buffer->data[keys_len + offset]),
                            ni * nj * _nk, nbits, 0, xdf_stride, 0);
                    }
                }
                break;
            }


            case FST_TYPE_CHAR:
            case FST_TYPE_CHAR | FST_TYPE_TURBOPACK:
                // character
                {
                    int nc = (ni * nj + 3) / 4;
                    if (is_type_turbopack(datyp)) {
                        Lib_Log(APP_LIBFST,APP_WARNING, "%s: extra compression not available, data type %d reset to FST_TYPE_CHAR (%d)\n",
                                __func__, stdf_entry->datyp, FST_TYPE_CHAR);
                        datyp = FST_TYPE_CHAR;
                        stdf_entry->datyp = datyp;
                    }
                    compact_p_integer(field_u32, (void *) NULL, &(buffer->data[keys_len]), nc, 32, 0, xdf_stride, 0);
                    stdf_entry->nbits = 8;
                }
                break;

            case FST_TYPE_SIGNED:
            case FST_TYPE_SIGNED | FST_TYPE_TURBOPACK: {
                // signed integer
                if (is_type_turbopack(datyp)) {
                    Lib_Log(APP_LIBFST, APP_WARNING, "%s: extra compression not supported, data type %d reset to FST_TYPE_SIGNED (%d)\n", __func__, stdf_entry->datyp, is_missing | FST_TYPE_SIGNED);
                    datyp = FST_TYPE_SIGNED;
                }
                // turbo compression not supported for this type, revert to normal mode
                stdf_entry->datyp = is_missing | FST_TYPE_SIGNED;
#ifdef use_old_signed_pack_unpack_code
                // fprintf(stderr, "OLD PACK CODE======================================\n");
                int32_t* field3 = (int32_t *)field_u32;
                if (xdf_short || xdf_byte) {
                    field3 = (int *)malloc(ni * nj * _nk*sizeof(int));
                    short * s_field = (short *)field_u32;
                    signed char * b_field = (signed char *)field_u32;
                    if (xdf_short) for (int i = 0; i < ni * nj * _nk;i++) { field3[i] = s_field[i]; };
                    if (xdf_byte)  for (int i = 0; i < ni * nj * _nk;i++) { field3[i] = b_field[i]; };
                }
                compact_p_integer(field3, (void *) NULL, &(buffer->data[keys_len]), ni * nj * _nk,
                    nbits, 0, xdf_stride, 1);
                if (field3 != (int32_t*)field_u32) free(field3);
#else
                // fprintf(stderr, "NEW PACK CODE======================================\n");
                if (xdf_short) {
                    compact_p_short(field_u32, (void *) NULL, &(buffer->data[keys_len]), ni * nj * _nk,
                        nbits, 0, xdf_stride, 7);
                } else if (xdf_byte) {
                    compact_p_char(field_u32, (void *) NULL, &(buffer->data[keys_len]), ni * nj * _nk,
                        nbits, 0, xdf_stride, 11);
                } else {
                    compact_p_integer(field_u32, (void *) NULL, &(buffer->data[keys_len]), ni * nj * _nk,
                        nbits, 0, xdf_stride, 1);
                }
#endif
                break;
            }
            case FST_TYPE_REAL_IEEE:
            case FST_TYPE_COMPLEX:
            case FST_TYPE_REAL_IEEE | FST_TYPE_TURBOPACK:
            case FST_TYPE_COMPLEX | FST_TYPE_TURBOPACK:
                // IEEE and IEEE complex representation
                {
                    int32_t f_ni = (int32_t) ni;
                    int32_t f_njnk = nj * _nk;
                    int32_t f_zero = 0;
                    int32_t f_one = 1;
                    int32_t f_minus_nbits = (int32_t) minus_nbits;
                    if (datyp == (FST_TYPE_COMPLEX | FST_TYPE_TURBOPACK)) {
                        Lib_Log(APP_LIBFST, APP_WARNING, "%s: extra compression not available for complex data, data type %d reset to FST_TYPE_COMPLEX (%d)\n",
                                __func__, stdf_entry->datyp, FST_TYPE_COMPLEX);
                        datyp = FST_TYPE_COMPLEX;
                        stdf_entry->datyp = datyp;
                    }
                    if (datyp == (FST_TYPE_REAL_IEEE | FST_TYPE_TURBOPACK)) {
                        // use an additionnal compression scheme
                        int compressed_lng = c_armn_compress32((unsigned char *)&(buffer->data[keys_len+1]), (float *)field_u32, ni, nj, _nk, nbits);
                        if (compressed_lng < 0) {
                            stdf_entry->datyp = FST_TYPE_REAL_IEEE;
                            f77name(ieeepak)((int32_t *)field_u32, (int32_t *)&(buffer->data[keys_len]), &f_ni, &f_njnk, &f_minus_nbits,
                                &f_zero, &f_one);
                        } else {
                            int nbytes = 16 + compressed_lng;
                            nw = (nbytes * 8 + 63) / 64;
                            nw = W64TOWD(nw);
                            buffer->data[keys_len] = nw;
                            buffer->nbits = (keys_len + nw) * bitmot;
                        }
                    } else {
                        if (datyp == FST_TYPE_COMPLEX) f_ni = f_ni * 2;
                        f77name(ieeepak)((int32_t*)field_u32, (int32_t *)&(buffer->data[keys_len]), &f_ni, &f_njnk, &f_minus_nbits,
                            &f_zero, &f_one);
                    }
                }
                break;

            case FST_TYPE_REAL:
            case FST_TYPE_REAL | FST_TYPE_TURBOPACK:
                // floating point, new packers

                if (is_type_turbopack(datyp) && (nbits <= 16)) {
                    // use an additional compression scheme
                    c_float_packer((float *)field_u32, nbits, (int32_t *)&(buffer->data[keys_len+1]),
                                   (int32_t *)&(buffer->data[keys_len+1+header_size]), ni * nj * _nk);
                    int compressed_lng = armn_compress((unsigned char *)&(buffer->data[keys_len+1+header_size]), ni, nj, _nk, nbits, 1, 1);
                    if (compressed_lng < 0) {
                        stdf_entry->datyp = FST_TYPE_REAL;
                        c_float_packer((float *)field_u32, nbits, (int32_t *)&(buffer->data[keys_len]),
                                        (int32_t *)&(buffer->data[keys_len+header_size]), ni * nj * _nk);
                    } else {
                        int nbytes = 16 + (header_size*4) + compressed_lng;
                        // fprintf(stderr, "Debug+ apres armn_compress nbytes=%d\n", nbytes);
                        nw = (nbytes * 8 + 63) / 64;
                        nw = W64TOWD(nw);
                        buffer->data[keys_len] = nw;
                        // fprintf(stderr, "Debug+ pack buffer->data[keys_len]=%d\n", buffer->data[keys_len]);
                        buffer->nbits = (keys_len + nw) * bitmot;
                    }
                } else {
                    c_float_packer((float *)field_u32, nbits, (int32_t *)&(buffer->data[keys_len]),
                                   (int32_t *)&(buffer->data[keys_len+header_size]), ni * nj * _nk);
                    // fprintf(stderr, "Debug+ fstecr apres float_packer buffer->data=%8X\n", buffer->data[keys_len]);
                }
                break;


            case FST_TYPE_STRING:
            case FST_TYPE_STRING | FST_TYPE_TURBOPACK:
                // character string
                if (is_type_turbopack(datyp)) {
                    Lib_Log(APP_LIBFST, APP_WARNING, "%s: extra compression not available, data type %d reset to FST_TYPE_STRING (%d)\n",
                            __func__, stdf_entry->datyp, FST_TYPE_STRING);
                    datyp = FST_TYPE_STRING;
                    stdf_entry->datyp = datyp;
                }
                compact_p_char(field_u32, (void *) NULL, &(buffer->data[keys_len]), ni * nj * _nk, 8, 0, xdf_stride);
                break;

            default:
                Lib_Log(APP_LIBFST, APP_ERROR, "%s: (unit=%d) invalid datyp=%d\n", __func__, iun, datyp);
                return ERR_BAD_DATYP;
        } // end switch
    } // end if image mode copy

    // write record to file and add entry to directory
    int ier = c_xdfput(iun, handle, buffer);
    if (Lib_LogLevel(APP_LIBFST, NULL) >= APP_INFO) {
        char string[14];
        snprintf(string, sizeof(string), "Write(%d)", iun);
        print_std_parms(stdf_entry, string, prnt_options, -1);
    }

    if (field_f != NULL) free(field_f);
    if (field_missing != NULL) free(field_missing);
    free(buffer);

    xdf_double = 0;
    xdf_short = 0;
    xdf_byte = 0;
    return ier;
}


//! Write a field into a rpn file
int c_fstecr(
    //! [in] Field to write to the file
    const void * const field_in,
    //! [in] Work field (kept for backward compatibility)
    const void * const work,
    //! [in] Number of bits kept for the elements of the field
    const int npak,
    //! [in] Unit number associated to the file in which to write the field
    const int iun,
    //! [in] Origin date timestamp
    const int dateo,
    //! [in] Length of the time steps in seconds
    const int deet,
    //! [in] Time step number
    const int npas,
    //! [in] First dimension of the data field
    const int ni,
    //! [in] Second dimension of the data field
    const int nj,
    //! [in] Third dimension of the data field
    const int nk,
    //! [in] Vertical level
    const int ip1,
    //! [in] Forecast hour
    const int ip2,
    //! [in] User defined identifier
    const int ip3,
    //! [in] Type of field (forecast, analysis, climatology)
    const char * const in_typvar,
    //! [in] Variable name
    const char * const in_nomvar,
    //! [in] Label
    const char * const in_etiket,
    //! [in] Type of geographical projection
    const char * const in_grtyp,
    //! [in] First grid descriptor
    const int ig1,
    //! [in] Second grid descriptor
    const int ig2,
    //! [in] Third grid descriptor
    const int ig3,
    //! [in] Fourth grid descriptor
    const int ig4,
    //! [in] Data type of elements
    //! | datyp | Description                                  |
    //! | ----: | :------------------------------------------- |
    //! |     0 | Binary, transparent                          |
    //! |     1 | Floating point                               |
    //! |     2 | Unsigned integer                             |
    //! |     3 | Character (R4A in an integer)                |
    //! |     4 | Signed integer                               |
    //! |     5 | IEEE floating point                          |
    //! |     6 | Floating point (16 bit, made for compressor) |
    //! |     7 | Character string                             |
    //! |     8 | Complex IEEE                                 |
    //! |   130 | Compressed short integer  (128+2)            |
    //! |   133 | Compressed IEEE           (128+5)            |
    //! |   134 | Compressed floating point (128+6)            |
    //! | +128  | Second stage packer active                   |
    //! | +64   | Missing value convention used                |
    const int datyp,
    //! [in] Rewrite existing record, append otherwise
    //! If true, records match if all metadata is the same with the exception of \p dateo and **datev** (\p dateo + \p deet * \p npas) which are not checked for this purpose
    const int rewrit
) {
    int index_fnom;
    const int rsf_status = is_rsf(iun, &index_fnom);

    if (index_fnom == -1) {
        Lib_Log(APP_LIBFST, APP_ERROR, "%s: file (unit=%d) is not connected with fnom\n", __func__, iun);
        return ERR_NO_FNOM;
    }

    if (rsf_status == 1) {
        return c_fstecr_rsf(field_in, work, npak, iun, index_fnom, dateo, deet, npas, ni, nj, nk, ip1, ip2, ip3,
                            in_typvar, in_nomvar, in_etiket, in_grtyp, ig1, ig2, ig3, ig4, datyp, rewrit);
    } else if (rsf_status == 0) {
        return c_fstecr_xdf(field_in, work, npak, iun, dateo, deet, npas, ni, nj, nk, ip1, ip2, ip3,
                            in_typvar, in_nomvar, in_etiket, in_grtyp, ig1, ig2, ig3, ig4, datyp, rewrit);
    }

    return rsf_status;
}


//! Edits the directory content of a RPN standard file
int c_fst_edit_dir_plus_xdf(
    //! [in] Handle of the directory entry to edit
    int handle,
    int date,
    int deet,
    int npas,
    int ni, //!< Ignored, can't change that one
    int nj, //!< Ignored, can't change that one
    int nk, //!< Ignored, can't change that one
    int ip1,
    int ip2,
    int ip3,
    char *in_typvar,
    char *in_nomvar,
    char *in_etiket,
    char *in_grtyp,
    int ig1,
    int ig2,
    int ig3,
    int ig4,
    int datyp //!< Ignore, can't change that one
) {
    // Ignored parameters
    (void) ni;
    (void) nj;
    (void) nk;
    (void) datyp;

    int index = INDEX_FROM_HANDLE(handle);
    if ((index < 0) || (index >= MAX_XDF_FILES)) {
        Lib_Log(APP_LIBFST, APP_ERROR, "%s: invalid handle=%d\n", __func__, handle);
        return ERR_BAD_HNDL;
    }

    file_table_entry * fte = file_table[index];

    if (! fte->cur_info->attr.std) {
        Lib_Log(APP_LIBFST, APP_ERROR, "%s: file \"%s\" (unit=%d) is not a RPN standard file\n",
                __func__, fte->cur_info->file_name, fte->iun);
        return ERR_NO_FILE;
    }

    if (!fte->cur_info->attr.write_mode) {
        Lib_Log(APP_LIBFST, APP_ERROR, "%s: file \"%s\" (unit=%d) is open in read-only mode\n",
                __func__, fte->cur_info->file_name, fte->iun);
        return ERR_NO_WRITE;
    }

    if (fte->xdf_seq) {
        Lib_Log(APP_LIBFST, APP_ERROR, "%s: file \"%s\" (unit=%d) is sequential (we can't edit it)\n",
                __func__, fte->cur_info->file_name, fte->iun);
        return ERR_NO_FILE;
    }

    char typvar[FST_TYPVAR_LEN];
    char nomvar[FST_NOMVAR_LEN];
    char etiket[FST_ETIKET_LEN];
    char grtyp[FST_GTYP_LEN];
    copy_record_string(typvar, in_typvar, FST_TYPVAR_LEN);
    copy_record_string(nomvar, in_nomvar, FST_NOMVAR_LEN);
    copy_record_string(etiket, in_etiket, FST_ETIKET_LEN);
    copy_record_string(grtyp, in_grtyp, FST_GTYP_LEN);

    int pageno = PAGENO_FROM_HANDLE(handle);
    if (pageno > fte->npages) {
        // page is not in current file
        Lib_Log(APP_LIBFST, APP_ERROR, "%s: invalid handle, invalid page number\n", __func__);
        return ERR_BAD_PAGENO;
    }

    int recno = RECORD_FROM_HANDLE(handle);
    // printf("Debug+ c_fst_edit_dir pageno=%d recno=%d\n", pageno, recno);
    int width = W64TOWD(fte->primary_len);
    uint32_t * entry = (fte->dir_page[pageno])->dir.entry + recno * width;
    stdf_dir_keys * stdf_entry = (stdf_dir_keys *) entry;

    if (grtyp[0] != ' ') stdf_entry->gtyp = grtyp[0];

    if (deet != -1) stdf_entry->deet = deet;
    if (npas != -1) stdf_entry->npas = npas;
    if (ig1 != -1) stdf_entry->ig1 = ig1;
    if (ig2 != -1) {
        stdf_entry->ig2a = ig2 >> 16;
        stdf_entry->ig2b = ig2 >> 8;
        stdf_entry->ig2c = ig2 & 0xff;
    }
    if (ig3 != -1) stdf_entry->ig3 = ig3;
    if (ig4 != -1) stdf_entry->ig4 = ig4;
    if (strcmp(etiket, "            ") != 0) {
        stdf_entry->etik15 =
        (ascii6(etiket[0]) << 24) |
        (ascii6(etiket[1]) << 18) |
        (ascii6(etiket[2]) << 12) |
        (ascii6(etiket[3]) <<  6) |
        (ascii6(etiket[4]));
        stdf_entry->etik6a =
        (ascii6(etiket[5]) << 24) |
        (ascii6(etiket[6]) << 18) |
        (ascii6(etiket[7]) << 12) |
        (ascii6(etiket[8])<<  6) |
        (ascii6(etiket[9]));
        stdf_entry->etikbc =
        (ascii6(etiket[10]) <<  6) |
        (ascii6(etiket[11]));
    }
    if (strcmp(typvar, "  ") != 0) {
        stdf_entry->typvar =
        (ascii6(typvar[0]) <<  6) |
        (ascii6(typvar[1]));
    }
    if (strcmp(nomvar, "    ") != 0) {
        stdf_entry->nomvar =
        (ascii6(nomvar[0]) << 18) |
        (ascii6(nomvar[1]) << 12) |
        (ascii6(nomvar[2]) <<  6) |
        (ascii6(nomvar[3]));
    }

    if (ip1 != -1) stdf_entry->ip1 = ip1;
    if (ip2 != -1) stdf_entry->ip2 = ip2;
    if (ip3 != -1) stdf_entry->ip3 = ip3;
    if (date != -1) stdf_entry->date_stamp = 8 * (date/10) + (date % 10);

    stdf_special_parms cracked;
    crack_std_parms(stdf_entry, &cracked);
    char string[20];
    snprintf(string, sizeof(string), "%5d-", recno);
    if (Lib_LogLevel(APP_LIBFST, NULL)>=APP_INFO) {
        print_std_parms(stdf_entry, string, "NINJNK+DATEO+IP1+IG1234", 1);
    }
    fte->dir_page[pageno]->modified = 1;
    fte->modified = 1;
    return 0;
}

//! Edits the directory content of a RPN standard file
int c_fst_edit_dir_plus(
    //! [in] Handle of the directory entry to edit
    int handle,
    int date,
    int deet,
    int npas,
    int ni,
    int nj,
    int nk,
    int ip1,
    int ip2,
    int ip3,
    char *in_typvar,
    char *in_nomvar,
    char *in_etiket,
    char *in_grtyp,
    int ig1,
    int ig2,
    int ig3,
    int ig4,
    int datyp
) {
    const int32_t key_type = RSF_Key32_type(handle);

    if (key_type == 1) {
        Lib_Log(APP_LIBFST, APP_WARNING,
                "%s: You can't just edit a directory entry once a record has been written to an RSF file\n",
                __func__);
        return ERR_WRONG_FTYPE;
    }
    else if (key_type == 0) {
        return c_fst_edit_dir_plus_xdf(handle, date, deet, npas, ni, nj, nk, ip1, ip2, ip3,
                                       in_typvar, in_nomvar, in_etiket, in_grtyp,
                                       ig1, ig2, ig3, ig4, datyp);
    }

    return ERR_BAD_HNDL;
}

//! Wrapper of \link c_fst_edit_dir_plus \endlink for backward compatibility
int c_fst_edit_dir(
    int handle,
    int date,
    int deet,
    int npas,
    int ni, //!< Ignored
    int nj, //!< Ignored
    int nk, //!< Ignored
    int ip1,
    int ip2,
    int ip3,
    char *in_typvar,
    char *in_nomvar,
    char *in_etiket,
    char *in_grtyp,
    int ig1,
    int ig2,
    int ig3,
    int ig4,
    int datyp //!< Ignored
) {
    (void)ni;
    (void)nj;
    (void)nk;
    (void)datyp;
    (void)in_grtyp;
    return c_fst_edit_dir_plus(handle, date, deet, npas, -1, -1, -1, ip1, ip2, ip3, in_typvar, in_nomvar, in_etiket, " ", ig1, ig2, ig3, ig4, -1);
}


//! Delete a record
//! XDF version
int c_fsteff_xdf(
    //! Handle of the record to delete
    int handle
) {
    int index = INDEX_FROM_HANDLE(handle);
    if ((index < 0) || (index >= MAX_XDF_FILES)) {
        Lib_Log(APP_LIBFST, APP_ERROR, "%s: invalid handle=%d\n", __func__, handle);
        return ERR_BAD_HNDL;
    }

    file_table_entry *fte = file_table[index];
    if (fte == NULL) {
        Lib_Log(APP_LIBFST, APP_ERROR, "%s: invalid handle=%d\n", __func__, handle);
        return ERR_BAD_HNDL;
    }

    if (! fte->cur_info->attr.std) {
        Lib_Log(APP_LIBFST, APP_ERROR, "%s: file (unit=%d) is not a RPN standard filed\n", __func__, fte->iun);
        return ERR_NO_FILE;
    }

    if (fte->fstd_vintage_89) {
        Lib_Log(APP_LIBFST, APP_FATAL, "%s: can not write (unit=%d) on an old (version 89) RPN standard file\n", __func__, fte->iun);
        return ERR_NO_WRITE;
    }

    return c_xdfdel(handle);
}

//! Delete a record
//! \return 0 on success, negative number on failure
int c_fsteff(
    //! Handle of the record to delete
    int handle
) {
    const int32_t key_type = RSF_Key32_type(handle);

    if (key_type == 1) {
        RSF_handle file_handle = RSF_Key32_to_handle(handle);
        return c_fsteff_rsf(file_handle, handle);
    }
    else if (key_type == 0) {
        return c_fsteff_xdf(handle);
    }

    return ERR_BAD_HNDL;
}


//! Get the level of end of file for the sequential file
//! XDF version
int c_fsteof_xdf(
    //! [in] Unit number associated to the file
    int iun
) {
    int index_fnom = get_fnom_index(iun);
    if (index_fnom == -1) {
        Lib_Log(APP_LIBFST, APP_ERROR, "%s: file (unit=%d) is not connected with fnom\n", __func__, iun);
        return ERR_NO_FNOM;
    }

    int index = file_index_xdf(iun);
    if (index == ERR_NO_FILE) {
        Lib_Log(APP_LIBFST, APP_ERROR, "%s: file (unit=%d) is not open\n", __func__, iun);
        return ERR_NO_FILE;
    }

    file_table_entry *fte = file_table[index];

    if (!fte->xdf_seq) {
        return 0;
    }

    int eof = 0;
    if (!fte->fstd_vintage_89) {
        xdf_record_header *header = (xdf_record_header *) fte->head_keys;
        if ((header->idtyp >= 112) && (header->idtyp <= 127)) eof = header->idtyp - 112;
    } else {
        seq_dir_keys *seq_entry = (seq_dir_keys *) fte->head_keys;
        if (seq_entry->eof > 0) eof = (seq_entry->eof == 31) ? 15 : seq_entry->eof;
    }

    return eof;
}

//! Get the level of end of file for the sequential file
int c_fsteof(
    //! [in] Unit number associated to the file
    int iun
) {
    int index_fnom;
    const int rsf_status = is_rsf(iun, &index_fnom);

    if (index_fnom == -1) {
        Lib_Log(APP_LIBFST, APP_ERROR, "%s: file (unit=%d) is not connected with fnom\n", __func__, iun);
        return ERR_NO_FNOM;
    }

    if (rsf_status == 1) {
        Lib_Log(APP_LIBFST, APP_WARNING, "%s: Operation does not make sense on an RSF file\n", __func__);
        return 0;
    }
    else if (rsf_status == 0) {
        return c_fsteof_xdf(iun);
    }

    return rsf_status;
}

//! \copydoc c_fstfrm
//! XDF version
int c_fstfrm_xdf(
    //! [in] Unit number associated to the file
    const int iun
) {
    return c_xdfcls(iun);
}

//! Close a RPN standard file
int c_fstfrm(
    //! [in] Unit number associated to the file
    const int iun
) {
    //! \return -1 if there's an error
    //! \see c_fstouv

    int index_fnom = -1;
    const int rsf_status = is_rsf(iun, &index_fnom);

    if (index_fnom == -1) {
        Lib_Log(APP_LIBFST, APP_ERROR, "%s: file (unit=%d) is not connected with fnom\n", __func__, iun);
        return ERR_NO_FNOM;
    }

    fst98_open_files[index_fnom].is_open = 0;

    if (rsf_status == 1) {
        return c_fstfrm_rsf(iun, index_fnom);
    }
    else if (rsf_status == 0) {
        return c_fstfrm_xdf(iun);
    }

    return rsf_status;
}

//! Locate the first record that matches the search keys
int c_fstinf(
    //! [in] Unit number associated to the file
    const int iun,
    //! [out] Dimension 1 of the data field
    int * const ni,
    //! [out] Dimension 2 of the data field
    int * const nj,
    //! [out] Dimension 3 of the data field
    int * const nk,
    //! [in] Validity date
    const int datev,
    //! [in] Label
    const char * const etiket,
    //! [in] Vertical level
    const int ip1,
    //! [in] Forecast hour
    const int ip2,
    //! [in] User defined identifier
    const int ip3,
    //! [in] Type of field
    const char * const typvar,
    //! [in] Variable name
    const char * const nomvar
) {
    return c_fstinfx(-2, iun, ni, nj, nk, datev, etiket, ip1, ip2, ip3, typvar, nomvar);
}


//! \copydoc c_fstinfx
//! XDF version
int c_fstinfx_xdf(
    //! [in] Handle from which the search begins.  Start from the beginning when handle = -2
    const int handle,
    //! [in] Unit number associated to the file
    const int iun,
    //! [out] Dimension 1 of the data field
    int * const ni,
    //! [out] Dimension 2 of the data field
    int * const nj,
    //! [out] Dimension 3 of the data field
    int * const nk,
    //! [in] Validity date
    const int datev,
    //! [in] Label
    const char * const in_etiket,
    //! [in] Vertical level
    const int ip1,
    //! [in] Forecast hour
    const int ip2,
    //! [in] User defined identifier
    const int ip3,
    //! [in] Type of field
    const char * const in_typvar,
    //! [in] Variable name
    const char * const in_nomvar
) {
    char etiket[FST_ETIKET_LEN];
    char typvar[FST_TYPVAR_LEN];
    char nomvar[FST_NOMVAR_LEN];

    copy_record_string(etiket, in_etiket, FST_ETIKET_LEN);
    copy_record_string(typvar, in_typvar, FST_TYPVAR_LEN);
    copy_record_string(nomvar, in_nomvar, FST_NOMVAR_LEN);
    Lib_Log(APP_LIBFST, APP_DEBUG, "%s: iun %d recherche: datev=%d etiket=[%s] ip1=%d ip2=%d ip3=%d typvar=[%s] nomvar=[%s] (handle = %d, IP flags = %d%d%d)\n",
            __func__, iun, datev, etiket, ip1, ip2, ip3, typvar, nomvar, handle, ip1s_flag, ip2s_flag, ip3s_flag);

    int index_fnom = get_fnom_index(iun);
    if (index_fnom == -1) {
        Lib_Log(APP_LIBFST, APP_ERROR, "%s: file (unit=%d) is not connected with fnom\n", __func__, iun);
        return ERR_NO_FNOM;
    }

    int index = file_index_xdf(iun);
    if (index == ERR_NO_FILE) {
        Lib_Log(APP_LIBFST, APP_ERROR, "%s: file (unit=%d) is not open\n", __func__, iun);
        return ERR_NO_FILE;
    }

    stdf_dir_keys *stdf_entry = (stdf_dir_keys *) calloc(1, sizeof(stdf_dir_keys));
    stdf_dir_keys *search_mask = (stdf_dir_keys *) calloc(1, sizeof(stdf_dir_keys));

    uint32_t * pkeys = stdf_entry->words;
    uint32_t * pmask = search_mask->words;

    memset(search_mask, 0xff, sizeof(stdf_dir_keys));

    search_mask->pad1 = 0;
    search_mask->pad2 = 0;
    search_mask->pad3 = 0;
    search_mask->dasiz = 0;
    search_mask->pad5 = 0;
    search_mask->pad6 = 0;
    search_mask->pad7 = 0;
    search_mask->deleted = 0;
    search_mask->select = 0;
    search_mask->lng = 0;
    search_mask->addr = 0;
    search_mask->deet = 0;
    search_mask->nbits = 0;
    search_mask->ni = 0;
    search_mask->gtyp = 0;
    search_mask->nj = 0;
    search_mask->datyp = 0;
    search_mask->nk = 0;
    search_mask->ubc = 0;
    search_mask->npas = 0;
    search_mask->ig4 = 0;
    search_mask->ig2a = 0;
    search_mask->ig1 = 0;
    search_mask->ig2b = 0;
    search_mask->ig3 = 0;
    search_mask->ig2c = 0;
    search_mask->levtyp = 0;

    const unsigned int u_datev = datev;
    stdf_entry->date_stamp = 8 * (u_datev/10) + (u_datev % 10);
    search_mask->date_stamp &= ~(0x7);
    if (datev == -1) search_mask->date_stamp = 0;

    stdf_entry->ip1 = ip1;
    if ((ip1 == -1) || (ip1s_flag)) search_mask->ip1 = 0;

    stdf_entry->ip2 = ip2;
    if ((ip2 == -1) || (ip2s_flag)) search_mask->ip2 = 0;

    stdf_entry->ip3 = ip3;
    if ((ip3 == -1) || (ip3s_flag)) search_mask->ip3 = 0;

    stdf_entry->nomvar = (ascii6(nomvar[0]) << 18) |
                        (ascii6(nomvar[1]) << 12) |
                        (ascii6(nomvar[2]) <<  6) |
                        (ascii6(nomvar[3]));
    if (stdf_entry->nomvar == 0) search_mask->nomvar = 0;

    stdf_entry->typvar = (ascii6(typvar[0]) << 6) |
                        (ascii6(typvar[1]));
    if (stdf_entry->typvar == 0) search_mask->typvar = 0;

    stdf_entry->etik15 = (ascii6(etiket[0]) << 24) |
                        (ascii6(etiket[1]) << 18) |
                        (ascii6(etiket[2]) << 12) |
                        (ascii6(etiket[3]) <<  6) |
                        (ascii6(etiket[4]));

    stdf_entry->etik6a = (ascii6(etiket[5]) << 24) |
                        (ascii6(etiket[6]) << 18) |
                        (ascii6(etiket[7]) << 12) |
                        (ascii6(etiket[8]) <<  6) |
                        (ascii6(etiket[9]));

    stdf_entry->etikbc = (ascii6(etiket[10]) <<  6) |
                        (ascii6(etiket[11]));

    if ((stdf_entry->etik15 == 0) && (stdf_entry->etik6a == 0)) {
        search_mask->etik15 = 0;
        search_mask->etik6a = 0;
        search_mask->etikbc = 0;
    }
    pkeys += W64TOWD(1);
    pmask += W64TOWD(1);
    int lhandle = handle;
    file_table_entry *fte = file_table[index];
    if (lhandle == -2) {
        // means handle not specified
        if (fte->xdf_seq) {
            lhandle = c_xdfloc2(iun, -1, pkeys, 16, pmask);
        } else {
            lhandle = c_xdfloc2(iun, 0, pkeys, 16, pmask);
        }
    } else {
        if (lhandle > 0) {
            int index_h = INDEX_FROM_HANDLE(lhandle);
            if (index_h != index) {
                Lib_Log(APP_LIBFST, APP_ERROR, "%s: invalid handle=%d, or iun=%d\n", __func__, lhandle, iun);
                free(stdf_entry);
                free(search_mask);
                return ERR_BAD_HNDL;
            }
        }
        lhandle = c_xdfloc2(iun, lhandle, pkeys, 16, pmask);
    }

    if (lhandle < 0) {
        Lib_Log(APP_LIBFST, APP_DEBUG, "%s: (unit=%d) record not found, errcode=%d\n", __func__, iun, lhandle);
        free(stdf_entry);
        free(search_mask);
        return lhandle;
    }
    int addr, lng, idtyp;
    c_xdfprm(lhandle, &addr, &lng, &idtyp, pkeys, 16);

    if (ip1s_flag || ip2s_flag || ip3s_flag) {
        int nomatch = 1;
        while ((lhandle >=  0) && (nomatch)) {
            nomatch = 0;
            if ((ip1s_flag) && (ip1 >= 0)) {
                if (ip_is_equal(ip1, stdf_entry->ip1, 1) == 0) {
                    nomatch = 1;
                } else if ((ip2s_flag) && (ip2 >= 0)) {
                    if (ip_is_equal(ip2, stdf_entry->ip2, 2) == 0) {
                        nomatch = 1;
                    } else if ((ip3s_flag) && (ip3 >= 0)) {
                        if (ip_is_equal(ip3, stdf_entry->ip3, 3) == 0) {
                            nomatch = 1;
                        }
                    }
                }
            }
            if (nomatch) {
                lhandle = c_xdfloc2(iun, -1, pkeys, 16, pmask);
                if (lhandle >= 0) c_xdfprm(lhandle, &addr, &lng, &idtyp, pkeys, 16);
            }
        }
    }
    if (ip1s_flag || ip2s_flag || ip3s_flag) {
        // arranger les masques de recherches pour un fstsui
        if (ip1s_flag) search_mask->ip1 = 0xFFFFFFF;
        if (ip2s_flag) search_mask->ip2 = 0xFFFFFFF;
        if (ip3s_flag) search_mask->ip3 = 0xFFFFFFF;
        fte->build_primary(fte->target, pkeys, fte->cur_mask, pmask, index, 1);
        init_ip_vals();
    }
    *ni = stdf_entry->ni;
    *nj = stdf_entry->nj;
    *nk = stdf_entry->nk;
    free(stdf_entry);
    free(search_mask);
    return lhandle;
}

//! Locate the next record that matches the search keys continuing from the position corresponding to the provided handle
int c_fstinfx(
    //! [in] Handle from which the search begins.  Start from the beginning when handle = -2
    const int handle,
    //! [in] Unit number associated to the file
    const int iun,
    //! [out] Dimension 1 of the data field
    int * const ni,
    //! [out] Dimension 2 of the data field
    int * const nj,
    //! [out] Dimension 3 of the data field
    int * const nk,
    //! [in] Validity date
    const int datev,
    //! [in] Label
    const char * const in_etiket,
    //! [in] Vertical level
    const int ip1,
    //! [in] Forecast hour
    const int ip2,
    //! [in] User defined identifier
    const int ip3,
    //! [in] Type of field
    const char * const in_typvar,
    //! [in] Variable name
    const char * const in_nomvar
) {
    int index_fnom;
    is_rsf(iun, &index_fnom);

    if (index_fnom == -1) {
        Lib_Log(APP_LIBFST, APP_ERROR, "%s: file (unit=%d) is not connected with fnom\n", __func__, iun);
        return ERR_NO_FNOM;
    }

    fstd_usage_info* info = &fst98_open_files[index_fnom];
    info->inf_param.datev = datev;
    info->inf_param.ip1 = ip1;
    info->inf_param.ip2 = ip2;
    info->inf_param.ip3 = ip3;
    strncpy(info->inf_param.nomvar, in_nomvar, FST_NOMVAR_LEN);
    strncpy(info->inf_param.typvar, in_typvar, FST_TYPVAR_LEN);
    strncpy(info->inf_param.etiket, in_etiket, FST_ETIKET_LEN);

    int status = -1;
    while (index_fnom >= 0) {
        Lib_Log(APP_LIBFST, APP_DEBUG, "%s: Looking at file %d (iun %d), type %s, next %d, IP all flags %d %d %d\n",
                __func__, index_fnom, FGFDT[index_fnom].iun, FGFDT[index_fnom].attr.rsf ? "RSF" : "XDF", fst98_open_files[index_fnom].next_file,
                ip1s_flag, ip2s_flag, ip3s_flag);

        fst98_open_files[index_fnom].search_done = 0;

        if (FGFDT[index_fnom].attr.rsf == 1) {
            status = c_fstinfx_rsf(handle, FGFDT[index_fnom].iun, index_fnom, ni, nj, nk, datev, in_etiket, ip1, ip2, ip3, in_typvar,
                                   in_nomvar, 0);
        }
        else if (FGFDT[index_fnom].attr.rsf == 0) {
            status = c_fstinfx_xdf(handle, FGFDT[index_fnom].iun, ni, nj, nk, datev, in_etiket, ip1, ip2, ip3, in_typvar, in_nomvar);
        }

        if (status > 0) return status; // Found it!

        // Stop if error, but we continue looking if it's just because we didn't find a match
        if (status < 0 && status != ERR_NOT_FOUND) return status;

        fst98_open_files[index_fnom].search_done = 1; // Indicate search is complete in this file
        index_fnom = fst98_open_files[index_fnom].next_file; // Go to next file in linked list
    }

    if (ip1s_flag || ip2s_flag || ip3s_flag) init_ip_vals();

    return status;
}


//! \copydoc c_fstinl()
//! XDF version
int c_fstinl_xdf(
    //! [in] Unit number associated to the file in which to search
    int iun,
    //! [out] First dimension of the field
    int *ni,
    //! [out] Second dimension of the field
    int *nj,
    //! [out] Third dimension of the field
    int *nk,
    //! [in] Validity date
    int datev,
    //! [in] Label
    char *etiket,
    //! [in] Vertical level
    int ip1,
    //! [in] Forecast hour
    int ip2,
    //! [in] User defined identifier
    int ip3,
    //! [in] Type of field
    char *typvar,
    //! [in] Variable name
    char *nomvar,
    //! [out] List of handles of the matching records
    int *liste,
    //! [out] Number of elements for the list (number of matching records)
    int *infon,
    //! [in] List size (maximum number of matches)
    int nmax
) {
    Lib_Log(APP_LIBFST, APP_DEBUG, "%s: iun %d recherche: datev=%d etiket=[%s] ip1=%d ip2=%d ip3=%d typvar=[%s] nomvar=[%s]\n",
            __func__, iun, datev, etiket, ip1, ip2, ip3, typvar, nomvar);

    int handle = c_fstinfx_xdf(-2, iun, ni, nj, nk, datev, etiket, ip1, ip2, ip3, typvar, nomvar);

    if (handle < 0) {
        *infon = 0;
        return 0;
    }

    int nijkmax = (*ni) * (*nj) * (*nk);
    int nimax = *ni;
    int njmax = *nj;
    int nkmax = *nk;

    int nfound = 0;
    while ((handle >= 0) && (nfound < nmax)) {
        liste[nfound] = handle;
        nfound++;
        if (nfound >= nmax) break;
        handle = c_fstsui_xdf(iun, ni, nj, nk);
        if ( ((*ni) * (*nj) * (*nk)) > nijkmax ) {
            nimax = *ni;
            njmax = *nj;
            nkmax = *nk;
            nijkmax = (*ni) * (*nj) * (*nk);
        }
    }
    *ni = nimax;
    *nj = njmax;
    *nk = nkmax;
    *infon = nfound;
    Lib_Log(APP_LIBFST, APP_DEBUG, "%s: nombre trouve=%d nmax=%d\n", __func__, nfound, nmax);

    while ( (handle = c_fstsui_xdf(iun, ni, nj, nk)) >= 0 ) nfound++;
    if (nfound > nmax) {
        Lib_Log(APP_LIBFST, APP_ERROR, "%s: number of records found (%d) > nmax specified (%d)\n", __func__, nfound, nmax);
        return -nfound;
    } else {
        return 0;
    }
}

//! Locates all the records that matches the search keys
//! \return 0 on success, error code otherwise
int c_fstinl(
    //! [in] Unit number associated to the file in which to search
    int iun,
    //! [out] First dimension of the field
    int *ni,
    //! [out] Second dimension of the field
    int *nj,
    //! [out] Third dimension of the field
    int *nk,
    //! [in] Validity date
    int datev,
    //! [in] Label
    char *etiket,
    //! [in] Vertical level
    int ip1,
    //! [in] Forecast hour
    int ip2,
    //! [in] User defined identifier
    int ip3,
    //! [in] Type of field
    char *typvar,
    //! [in] Variable name
    char *nomvar,
    //! [out] List of handles of the matching records
    int *liste,
    //! [out] Number of elements for the list (number of matching records)
    int *infon,
    //! [in] List size (maximum number of matches)
    int nmax
) {
    int index_fnom = get_fnom_index(iun);
    if (index_fnom == -1) {
        Lib_Log(APP_LIBFST, APP_ERROR, "%s: file (unit=%d) is not connected with fnom\n", __func__, iun);
        return ERR_NO_FNOM;
    }

    int status = -1;
    int total_found = 0;
    int num_files = 0;
    while (index_fnom >= 0) {
        Lib_Log(APP_LIBFST, APP_DEBUG, "%s: Looking at file %d (iun %d), type %s, next %d\n",
                __func__, index_fnom, FGFDT[index_fnom].iun, FGFDT[index_fnom].attr.rsf ? "RSF" : "XDF", fst98_open_files[index_fnom].next_file);
        num_files++;

        if (FGFDT[index_fnom].attr.rsf == 1) {
            status = c_fstinl_rsf(FGFDT[index_fnom].iun, index_fnom, ni, nj, nk, datev, etiket, ip1, ip2, ip3,
                                  typvar, nomvar, liste + total_found, infon, nmax - total_found);
        }
        else {
            status = c_fstinl_xdf(FGFDT[index_fnom].iun, ni, nj, nk, datev, etiket, ip1, ip2, ip3, typvar, nomvar,
                                  liste + total_found, infon, nmax - total_found);
        }

        if (status < 0) return status;

        total_found += *infon;
        index_fnom = fst98_open_files[index_fnom].next_file;
    }

    if (ip1s_flag || ip2s_flag || ip3s_flag) init_ip_vals();

    Lib_Log(APP_LIBFST, APP_DEBUG, "%s: Found %d records in %d files\n", __func__, total_found, num_files);

    *infon = total_found;
    return status;
}

//! Search for a record that matches the search keys and check that the remaining parmeters match the record descriptors
int c_fstlic(
    //! [out] Field to be read
    void *field,
    //! [in] Unit number associated to the file
    int iun,
    //! [in] First dimension of the data field. Only used to check that the matches with the field found.
    int niin,
    //! [in] Second dimension of the data field. Only used to check that the matches with the field found.
    int njin,
    //! [in] Third dimension of the data field. Only used to check that the matches with the field found.
    int nkin,
    //! [in] Valid date
    int datein,
    //! [in] Label
    char *etiketin,
    //! [in] Vertical level
    int ip1in,
    //! [in] Forecast hour
    int ip2in,
    //! [in] User defined identifier
    int ip3in,
    //! [in] Type of field
    char *typvarin,
    //! [in] Variable name
    char *nomvarin,
    //! [in] First grid descriptor
    int ig1in,
    //! [in] Second grid descriptor
    int ig2in,
    //! [in] Third grid descriptor
    int ig3in,
    //! [in] Fourth grid descriptor
    int ig4in,
    //! [in] Type of geographical projection
    char *grtypin
) {
    int handle, ier;
    int ni, nj, nk, date, ip1, ip2, ip3, ig1, ig2, ig3, ig4;
    int nbits, swa, ubc, lng, dltf, xtra1, xtra2, xtra3, deet, npas, datyp;
    char etiket[13] = {' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', '\0'};
    char typvar[3] = {' ', ' ', '\0'};
    char nomvar[5] = {' ', ' ', ' ', ' ', '\0'};
    char grtyp[2] = {' ', '\0'};

    handle = c_fstinf(iun, &ni, &nj, &nk, datein, etiketin, ip1in, ip2in, ip3in, typvarin, nomvarin);

    if (handle < 0) {
        Lib_Log(APP_LIBFST, APP_ERROR, "%s: (unit=%d) record not found\n", __func__, iun);
        return ERR_NOT_FOUND;
    }

    if ((niin != ni) || (njin != nj) || (nkin != nk)) {
        Lib_Log(APP_LIBFST, APP_ERROR, "%s: unit=%d, ni or nj or nk error\n\tuser ni=%d, file ni=%d\n\tuser nj=%d, file nj=%d\tuser nk=%d, file nk=%d\n", __func__, iun, niin, ni, njin, nj, nkin, nk);
        return -1;
    }

    ier = c_fstprm(handle, &date, &deet, &npas, &ni, &nj, &nk, &nbits, &datyp, &ip1,
                        &ip2, &ip3, typvar, nomvar, etiket, grtyp, &ig1, &ig2,
                        &ig3, &ig4, &swa, &lng, &dltf, &ubc, &xtra1, &xtra2, &xtra3);

    if ((strcmp(grtypin, grtyp) != 0) || (ig1in != ig1) ||
        (ig2in != ig2) || (ig3in != ig3) || (ig4in != ig4)) {
        Lib_Log(APP_LIBFST, APP_ERROR, "%s: unit=%d, grtyp ig1 ig2 ig3 ig4 error\n\tuser grtyp=%s, file grtyp=%s\n\tuser ig1=%d, file ig1=%d\n\tuser ig2=%d, file ig2=%d\n\tuser ig3=%d, file ig3=%d\n\tuser ig4=%d, file ig4=%d\n", __func__, iun, grtypin, grtyp, ig1in, ig1, ig2in, ig2, ig3in, ig3, ig4in, ig4);
        return -1;
    }

    ier = c_fstlir(field, iun, &ni, &nj, &nk, datein, etiketin, ip1in, ip2in, ip3in, typvarin, nomvarin);
    return ier;
}


//! Reads the next record that matches the search keys
//! \return Record key on success, a negative number on error.
int c_fstlir(
    //! [out] Data field to be read
    void *field,
    //! [in] Unit number associated to the file
    int iun,
    //! [out] First of the data field
    int *ni,
    //! [out] Second of the data field
    int *nj,
    //! [out] Third of the data field
    int *nk,
    //! [in] Validity date
    int datev,
    //! [in] Label
    char *etiket,
    //! [in] Vertical level
    int ip1,
    //! [in] Forecast hour
    int ip2,
    //! [in] User defined identifier
    int ip3,
    //! [in] Type of field
    char *typvar,
    //! [in] Variable name
    char *nomvar
) {
    // Means handle will be discarded
    int handle = -2;

    return c_fstlirx(field, handle, iun, ni, nj, nk, datev, etiket, ip1, ip2, ip3, typvar, nomvar);
}


//! \copydoc c_fstlirx
//! XDF version
int c_fstlirx_xdf(
    //! [out] Field to be read
    void *field,
    //! [in] Record handle from which the search begins
    int handle,
    //! [in] Unit number associated to the file
    int iun,
    //! [out] First of the data field
    int *ni,
    //! [out] Second of the data field
    int *nj,
    //! [out] Third of the data field
    int *nk,
    //! [in] Validity date
    int datev,
    //! [in] Label
    char *etiket,
    //! [in] Vertical level
    int ip1,
    //! [in] Forecast hour
    int ip2,
    //! [in] User defined identifier
    int ip3,
    //! [in] Type of field
    char *typvar,
    //! [in] Variable name
    char *nomvar
) {
    int ier;

    handle = c_fstinfx_xdf(handle, iun, ni, nj, nk, datev, etiket, ip1, ip2, ip3, typvar, nomvar);
    if (handle < 0) {
        Lib_Log(APP_LIBFST, APP_WARNING, "%s: c_fstlirx: (unit=%d) record not found, errcode=%d\n", __func__, iun, handle);
        return handle;
    }

    ier = c_fstluk(field, handle, ni, nj, nk);
    if (ier < 0) {
        return ier;
    } else {
        return handle;
    }
}

//! Reads the next record that matches the search keys.  The search begins at the position given by handle.
//! \return Record key on success, a negative number on error.
int c_fstlirx(
    //! [out] Field to be read
    void *field,
    //! [in] Record handle from which the search begins
    int handle,
    //! [in] Unit number associated to the file
    int iun,
    //! [out] First of the data field
    int *ni,
    //! [out] Second of the data field
    int *nj,
    //! [out] Third of the data field
    int *nk,
    //! [in] Validity date
    int datev,
    //! [in] Label
    char *etiket,
    //! [in] Vertical level
    int ip1,
    //! [in] Forecast hour
    int ip2,
    //! [in] User defined identifier
    int ip3,
    //! [in] Type of field
    char *typvar,
    //! [in] Variable name
    char *nomvar
) {
    int index_fnom;
    is_rsf(iun, &index_fnom);

    if (index_fnom == -1) {
        Lib_Log(APP_LIBFST, APP_ERROR, "%s: file (unit=%d) is not connected with fnom\n", __func__, iun);
        return ERR_NO_FNOM;
    }

    int status = -1;
    while (index_fnom >= 0) {
        Lib_Log(APP_LIBFST, APP_DEBUG, "%s: Looking at file %d (iun %d), type %s, next %d\n",
                __func__, index_fnom, FGFDT[index_fnom].iun, FGFDT[index_fnom].attr.rsf ? "RSF" : "XDF", fst98_open_files[index_fnom].next_file);

        if (FGFDT[index_fnom].attr.rsf == 1) {
            status = c_fstlirx_rsf(field, handle, FGFDT[index_fnom].iun, index_fnom, ni, nj, nk, datev, etiket, ip1, ip2, ip3, typvar, nomvar);
        }
        else if (FGFDT[index_fnom].attr.rsf == 0) {
            status = c_fstlirx_xdf(field, handle, FGFDT[index_fnom].iun, ni, nj, nk, datev, etiket, ip1, ip2, ip3, typvar, nomvar);
        }

        if (status > 0) return status; // Found it!

        // Stop if error, but we continue looking if it's just because we didn't find a match
        if (status < 0 && status != ERR_NOT_FOUND) return status;

        index_fnom = fst98_open_files[index_fnom].next_file;
    }

    if (ip1s_flag || ip2s_flag || ip3s_flag) init_ip_vals();

    return status;
}

//! \copydoc c_fstlis
//! XDF version
int c_fstlis_xdf(
    //! [out] Field to be read
    void *field,
    //! [in] Unit number associated to the file
    int iun,
    //! [out] First of the data field
    int *ni,
    //! [out] Second of the data field
    int *nj,
    //! [out] Third of the data field
    int *nk
) {
    uint32_t *primk = NULL;

    int index_fnom = get_fnom_index(iun);
    if (index_fnom == -1) {
        Lib_Log(APP_LIBFST, APP_ERROR, "%s: file (unit=%d) is not connected with fnom\n", __func__, iun);
        return ERR_NO_FNOM;
    }

    int index = file_index_xdf(iun);
    if (index == ERR_NO_FILE) {
        Lib_Log(APP_LIBFST, APP_ERROR, "%s: file (unit=%d) is not open\n", __func__, iun);
        return ERR_NO_FILE;
    }

    // position to the next record that matches the last search criterias
    // find next with handle=-1 and nprim=0
    int handle = c_xdfloc(iun, -1, primk, 0);
    if (handle < 0) {
        Lib_Log(APP_LIBFST, APP_WARNING, "%s: (unit=%d) record not found, errcode=%d\n", __func__, iun, handle);
        return handle;
    }

    return c_fstluk(field, handle, ni, nj, nk);
}

//! Reads the next record that matches the last search criterias
int c_fstlis(
    //! [out] Field to be read
    void *field,
    //! [in] Unit number associated to the file
    int iun,
    //! [out] First of the data field
    int *ni,
    //! [out] Second of the data field
    int *nj,
    //! [out] Third of the data field
    int *nk
) {
    int index_fnom;
    is_rsf(iun, &index_fnom);

    if (index_fnom == -1) {
        Lib_Log(APP_LIBFST, APP_ERROR, "%s: file (unit=%d) is not connected with fnom\n", __func__, iun);
        return ERR_NO_FNOM;
    }

    int status = -1;
    while (index_fnom >= 0) {
        Lib_Log(APP_LIBFST, APP_DEBUG, "%s: Looking at file %d (iun %d), type %s, next %d\n",
                __func__, index_fnom, FGFDT[index_fnom].iun, FGFDT[index_fnom].attr.rsf ? "RSF" : "XDF", fst98_open_files[index_fnom].next_file);

        if (FGFDT[index_fnom].attr.rsf == 1) {
            status = c_fstlis_rsf(field, FGFDT[index_fnom].iun, index_fnom, ni, nj, nk);
        }
        else if (FGFDT[index_fnom].attr.rsf == 0) {
            status = c_fstlis_xdf(field, FGFDT[index_fnom].iun, ni, nj, nk);
        }

        if (status > 0) return status; // Found it!

        // Stop if error, but we continue looking if it's just because we didn't find a match
        if (status < 0 && status != ERR_NOT_FOUND) return status;

        index_fnom = fst98_open_files[index_fnom].next_file;
    }

    if (ip1s_flag || ip2s_flag || ip3s_flag) init_ip_vals();

    return status;
}


//! \copydoc c_fstluk
//! XDF version
int c_fstluk_xdf(
    //! [out] Pointer to where the data read will be placed.  Must be allocated!
    void * const vfield,
    //! [in] Handle of the record to be read
    const int handle,
    //! [out] Dimension 1 of the data field
    int * const ni,
    //! [out] Dimension 2 of the data field
    int * const nj,
    //! [out] Dimension 3 of the data field
    int * const nk
) {
    uint32_t *field = vfield;

    // fprintf(stderr, "Debug+ c_fstluk(field=%p, handle=%i, ni=%i, nj=%i, nj=%i)\n", field, handle, *ni, *nj, *nk);

    // printf("sizeof(stdf_dir_keys) = %d\n", sizeof(stdf_dir_keys));

    stdf_dir_keys stdf_entry;
    // printf("Debug+ c_fstluk - &stdf_entry = %p\n", &stdf_entry);
    uint32_t * pkeys = stdf_entry.words;
    // printf("Debug+ c_fstluk - pkeys = %p\n", pkeys);
    pkeys += W64TOWD(1);
    // printf("Debug+ c_fstluk - pkeys = %p\n", pkeys);

    // printf("Debug+ c_fstluk - c_xdfprm(handle, &addr, &lng, &idtyp, pkeys, 16);\n");
    int addr, lng, idtyp;
    int ier = c_xdfprm(handle, &addr, &lng, &idtyp, pkeys, 16);
    if (ier < 0) return ier;

    *ni = stdf_entry.ni;
    *nj = stdf_entry.nj;
    *nk = stdf_entry.nk;
    // Get missing data flag
    int has_missing = stdf_entry.datyp & FSTD_MISSING_FLAG;
    // Suppress missing data flag
    stdf_entry.datyp = stdf_entry.datyp & ~FSTD_MISSING_FLAG;
    xdf_datatyp = stdf_entry.datyp;

    UnpackFunctionPointer packfunc = xdf_double ? &compact_u_double : &compact_u_float;
    double dmin=0.0, dmax=0.0;

    lng = W64TOWD(lng);
    if ((xdf_datatyp == FST_TYPE_REAL_OLD_QUANT) || (xdf_datatyp == FST_TYPE_REAL_IEEE)) {
        lng = (xdf_double) ? 2 * lng : lng;
    }

    int lng2;
    int header_size, stream_size, p1out, p2out;
    if ((xdf_datatyp == FST_TYPE_REAL) || (xdf_datatyp == (FST_TYPE_REAL | FST_TYPE_TURBOPACK))) {
        // New packer
        // printf("Debug+ fstluk - c_float_packer_params(&header_size, &stream_size, &p1out, &p2out, (*ni) * (*nj) * (*nk))\n");
        c_float_packer_params(&header_size, &stream_size, &p1out, &p2out, (*ni) * (*nj) * (*nk));
        header_size /= sizeof(int32_t);
        lng2 = 1 + ((*ni * *nj * *nk * 16 + 32 + 31) / 32) + header_size + 20;
    } else if (xdf_datatyp == (FST_TYPE_REAL_IEEE | FST_TYPE_TURBOPACK)) {
        // Compressed ieee
        lng2 = 1 + lng;
    } else if (is_type_turbopack(xdf_datatyp)) {
        // 16 for 16 bits armn_compress
        lng2 = 4 + ((*ni * *nj * *nk * 16 + 32 + 31) / 32) + 20;
        // fprintf(stderr, "Debug+ fstluk ni=%d nj=%d nk=%d lng=%d lng2=%d\n", *ni, *nj, *nk, lng, lng2);
    } else {
        lng2 = lng;

    }

    // printf("Debug+ fstluk lng2 = %d\n", lng2);

    // Allocate 8 more bytes in case of realingment for 64 bit data
    const size_t work_field_size = 8 + (lng2 + 10) * sizeof(int);
    void* work_field = malloc(work_field_size);
    memset(work_field, 0, work_field_size);

    // printf("Debug+ fstluk - buf = (buffer_interface_ptr) workField\n");
    buffer_interface_ptr buf = (buffer_interface_ptr) work_field;
    const size_t misalignment = (((&(buf->data[0]) - &(buf->nwords)) * sizeof(int)) & 0x7);
    if (misalignment > 0) {
        // Realign buf to make sure that buf->data is 64bit align
        buf = (buffer_interface_ptr) ((char*)work_field + misalignment);
    }
    // negative value means get data only
    buf->nwords = -(lng + 10);
    buf->nbits = (unsigned)-1;
    // printf("Debug+ fstluk - c_xdfget2(handle, buf, stdf_aux_keys)\n");
    int stdf_aux_keys[2];
    ier = c_xdfget2(handle, buf, stdf_aux_keys);
    if (ier < 0) return ier;

    if ((stdf_aux_keys[0] != 0) && (stdf_aux_keys[1] != 0)) {
        Lib_Log(APP_LIBFST, APP_FATAL, "%s: wrong version of fstd98 (%d), recompile with a more recent version (aux_keys[0]=%d, aux_keys[1]=%d)\n", __func__, stdf_version, stdf_aux_keys[0], stdf_aux_keys[1]);
        return ERR_STDF_VERSION;
    }

    int nelm = stdf_entry.ni * stdf_entry.nj * stdf_entry.nk;
    if (stdf_entry.datyp == 8) nelm *= 2;

    int npak = -(stdf_entry.nbits);
    const int bitmot = 32;
    if (image_mode_copy) {
        // No pack/unpack, used by editfst
        if (is_type_turbopack(stdf_entry.datyp)) {
            int lngw = buf->data[0];
            // fprintf(stderr, "Debug+ lecture mode image lngw=%d\n", lngw);
            for (int i = 0; i < lngw + 1; i++) {
                field[i] =  buf->data[i];
            }
        } else {
            int lngw = nelm * stdf_entry.nbits;
            if (stdf_entry.datyp == FST_TYPE_REAL_OLD_QUANT) lngw += 120;
            if (stdf_entry.datyp == FST_TYPE_CHAR) lngw = *ni * *nj * 8;
            if (stdf_entry.datyp == FST_TYPE_REAL) {
                c_float_packer_params(&header_size, &stream_size, &p1out, &p2out, nelm);
                lngw = (header_size + stream_size) * 8;
            }
            lngw = (lngw + bitmot - 1) / bitmot;
            for (int i = 0; i < lngw; i++) {
                field[i] =  buf->data[i];
            }
        }
    } else {
        switch (stdf_entry.datyp) {
            case FST_TYPE_BINARY: {
                // Raw binary
                // printf("Debug+ fstluk - Raw binary\n");
                int lngw = ((nelm * stdf_entry.nbits) + bitmot - 1) / bitmot;
                for (int i = 0; i < lngw; i++) {
                    field[i] = buf->data[i];
                }
                break;
            }

            case FST_TYPE_REAL_OLD_QUANT:
            case FST_TYPE_REAL_OLD_QUANT | FST_TYPE_TURBOPACK: {
                // Floating Point
                // printf("Debug+ fstluk - Floating Point\n");
                double tempfloat = 99999.0;
                if (is_type_turbopack(stdf_entry.datyp)) {
                    // fprintf(stderr, "Debug+ unpack buf->data=%d\n", *(buf->data));
                    armn_compress((unsigned char *)(buf->data + 5), *ni, *nj, *nk, stdf_entry.nbits, 2, 1);
                    // fprintf(stderr, "Debug+ buf->data + 4 + (nbytes / 4) - 1 = %X buf->data + 4 + (nbytes / 4) = %X \n", *(buf->data + 4 + (nbytes / 4) - 1), *(buf->data + 4 + (nbytes / 4)));
                    packfunc(field, buf->data + 1, buf->data + 5, nelm, stdf_entry.nbits + 64 * Max(16, stdf_entry.nbits),
                             0, xdf_stride, 0, &tempfloat, &dmin, &dmax);
                } else {
                    packfunc(field, buf->data, buf->data + 3, nelm, stdf_entry.nbits, 24, xdf_stride, 0, &tempfloat, &dmin , &dmax);
                }
                break;
            }

            case FST_TYPE_UNSIGNED:
            case FST_TYPE_UNSIGNED | FST_TYPE_TURBOPACK:
                {
                    // Integer, short integer or byte stream
                    // printf("Debug+ fstluk - Integer, short integer or byte stream\n");
                    int offset = is_type_turbopack(stdf_entry.datyp) ? 1 : 0;
                    if (xdf_short) {
                        if (is_type_turbopack(stdf_entry.datyp)) {
                            int nbytes = armn_compress((unsigned char *)(buf->data + offset), *ni, *nj, *nk, stdf_entry.nbits, 2, 0);
                            // printf("Debug+ fstluk mode short compress nbytes=%d\n", nbytes);
                            memcpy(field, buf->data + offset, nbytes);
                        } else {
                            ier = compact_u_short(field, (void *) NULL, buf->data + offset, nelm, stdf_entry.nbits, 0, xdf_stride);
                        }
                    }  else if (xdf_byte) {
                        if (is_type_turbopack(stdf_entry.datyp)) {
                            armn_compress((unsigned char *)(buf->data + offset), *ni, *nj, *nk, stdf_entry.nbits, 2, 0);
                            // printf("Debug+ fstluk xdf_byte armn_compress nbytes=%d nelm=%d\n", nbytes, nelm);
                            memcpy_16_8((int8_t *)field, (int16_t *)(buf->data + offset), nelm);
                        } else {
                            ier = compact_u_char(field, (void *) NULL, buf->data, nelm, 8, 0, xdf_stride);
                        }
                    } else {
                        if (is_type_turbopack(stdf_entry.datyp)) {
                            armn_compress((unsigned char *)(buf->data + offset), *ni, *nj, *nk, stdf_entry.nbits, 2, 0);
                            // printf("Debug+ fstluk mode int compress nbytes=%d\n", nbytes);
                            memcpy_16_32((int32_t *)field, (int16_t *)(buf->data + offset), stdf_entry.nbits, nelm);
                        } else {
                            ier = compact_u_integer(field, (void *) NULL, buf->data + offset, nelm, stdf_entry.nbits, 0, xdf_stride, 0);
                        }
                    }
                    break;
                }

            case FST_TYPE_CHAR: {
                // Character
                // printf("Debug+ fstluk - Character\n");
                int nc = (nelm + 3) / 4;
                ier = compact_u_integer(field, (void *) NULL, buf->data, nc, 32, 0, xdf_stride, 0);
                break;
            }


            case FST_TYPE_SIGNED: {
                // Signed integer
                // printf("Debug+ fstluk - Signed integer\n");
#ifdef use_old_signed_pack_unpack_code
                // fprintf(stderr, "OLD UNPACK CODE ======================================\n");
                int32_t *field_out;
                short *s_field_out = (short *)field;
                signed char *b_field_out = (signed char *)field;
                if (xdf_short || xdf_byte) {
                    field_out = malloc(nelm * sizeof(int));
                } else {
                    field_out = (int32_t *)field;
                }
                ier = compact_u_integer(field_out, (void *) NULL, buf->data, nelm, stdf_entry.nbits, 0, xdf_stride, 1);
                if (xdf_short) {
                    for (int i = 0; i < nelm; i++) {
                        s_field_out[i] = field_out[i];
                    }
                }
                else if (xdf_byte) {
                    for (int i = 0; i < nelm; i++) {
                        b_field_out[i] = field_out[i];
                    }
                }
                if (field_out != (int32_t*)field) free(field_out);
#else
                // fprintf(stderr, "NEW UNPACK CODE ======================================\n");
                if (xdf_short) {
                    ier = compact_u_short(field, (void *) NULL, buf->data, nelm, stdf_entry.nbits, 0, xdf_stride, 8);
                } else if (xdf_byte) {
                    ier = compact_u_char(field, (void *) NULL, buf->data, nelm, stdf_entry.nbits, 0, xdf_stride, 12);
                } else {
                    ier = compact_u_integer(field, (void *) NULL, buf->data, nelm, stdf_entry.nbits, 0, xdf_stride, 1);
                }
#endif
                break;
            }
            case FST_TYPE_REAL_IEEE:
            case FST_TYPE_COMPLEX: {
                // IEEE representation
                // printf("Debug+ fstluk - IEEE representation\n");
                register int32_t temp32, *src, *dest;
                if ((downgrade_32) && (stdf_entry.nbits == 64)) {
                    // Downgrade 64 bit to 32 bit
                    float * ptr_real = (float *) field;
                    double * ptr_double = (double *) buf->data;
#if defined(Little_Endian)
                    src = (int32_t *) buf->data;
                    dest = (int32_t *) buf->data;
                    for (int i = 0; i < nelm; i++) {
                        temp32 = *src++;
                        *dest++ = *src++;
                        *dest++ = temp32;
                    }
#endif
                    for (int i = 0; i < nelm; i++) {
                        *ptr_real++ = *ptr_double++;
                    }
                } else {
                    int32_t f_one = 1;
                    int32_t f_zero = 0;
                    int32_t f_mode = 2;
                    f77name(ieeepak)((int32_t *)field, (int32_t *)buf->data, &nelm, &f_one, &npak, &f_zero, &f_mode);
                }

                break;
            }

            case FST_TYPE_REAL:
            case FST_TYPE_REAL | FST_TYPE_TURBOPACK: {
                // Floating point, new packers
                // printf("Debug+ fstluk - Floating point, new packers (6, 134)\n");
                int nbits;
                if (is_type_turbopack(stdf_entry.datyp)) {
                    armn_compress((unsigned char *)(buf->data + 1 + header_size), *ni, *nj, *nk, stdf_entry.nbits, 2, 1);
                    // fprintf(stderr, "Debug+ buf->data+4+(nbytes/4)-1=%X buf->data+4+(nbytes/4)=%X \n",
                    //    *(buf->data+4+(nbytes/4)-1), *(buf->data+4+(nbytes/4)));

                    c_float_unpacker((float *)field, (int32_t *)(buf->data + 1), (int32_t *)(buf->data + 1 + header_size), nelm, &nbits);
                } else {
                    c_float_unpacker((float *)field, (int32_t *)buf->data, (int32_t *)(buf->data + header_size), nelm, &nbits);
                }
                break;
            }

            case FST_TYPE_REAL_IEEE | FST_TYPE_TURBOPACK: {
                // Floating point, new packers
                // printf("Debug+ fstluk - Floating point, new packers (133)\n");
                c_armn_uncompress32((float *)field, (unsigned char *)(buf->data + 1), *ni, *nj, *nk, stdf_entry.nbits);
                break;
            }

            case FST_TYPE_STRING:
                // Character string
                // printf("Debug+ fstluk - Character string\n");
                // printf("Debug fstluk compact_u_char xdf_stride=%d nelm =%d\n", xdf_stride, nelm);
                ier = compact_u_char(field, (void *) NULL, buf->data, nelm, 8, 0, xdf_stride);
                break;

            default:
                Lib_Log(APP_LIBFST, APP_ERROR, "%s: invalid datyp=%d\n", __func__, stdf_entry.datyp);
                return ERR_BAD_DATYP;
        } // switch
    }

    if (Lib_LogLevel(APP_LIBFST, NULL) >= APP_INFO) {
        char string[20];
        snprintf(string, sizeof(string), "Read(%d)", buf->iun);
        stdf_entry.datyp = stdf_entry.datyp | has_missing;
        print_std_parms(&stdf_entry, string, prnt_options, -1);
    }
    if (has_missing) {
        // Replace "missing" data points with the appropriate values given the type of data (int/float)
        // if nbits = 64 and IEEE , set xdf_double
        if ((stdf_entry.datyp & 0xF) == 5 && stdf_entry.nbits == 64 ) xdf_double = 1;
        int sz=(xdf_double?64:(xdf_short?16:(xdf_byte?8:32)));
        // printf("Debug+ fstluk - DecodeMissingValue\n");
        DecodeMissingValue(field , (*ni) * (*nj) * (*nk) , xdf_datatyp & 0x3F, sz);
    }

    // Upgrade size, if necessary
    if (xdf_double && (stdf_entry.dasiz < 64 && stdf_entry.dasiz > 0) && !image_mode_copy) {
        const int base_type = base_fst_type(stdf_entry.datyp);
        if (base_type == FST_TYPE_REAL_IEEE || base_type == FST_TYPE_REAL) {
            float f[nelm];
            memcpy(f, field, nelm * sizeof(float));
            upgrade_size(field, 64, f, 32, nelm, 0);
        }
        else if (base_type == FST_TYPE_SIGNED || base_type == FST_TYPE_UNSIGNED) {
            int32_t x[nelm];
            memcpy(x, field, nelm * sizeof(int32_t));
            upgrade_size(field, 64, x, 32, nelm, 1);
        }
    }

    xdf_double = 0;
    xdf_short = 0;
    xdf_byte = 0;

    free(work_field);

    return handle;
}


//! Read the record corresponding to the provided handle
int c_fstluk(
    //! [out] Pointer to where the data read will be placed.  Must be allocated!
    void * const vfield,
    //! [in] Handle of the record to be read
    const int handle,
    //! [out] Dimension 1 of the data field
    int * const ni,
    //! [out] Dimension 2 of the data field
    int * const nj,
    //! [out] Dimension 3 of the data field
    int * const nk
) {
    const int32_t key_type = RSF_Key32_type(handle);

    if (key_type == 1) {
        RSF_handle file_handle = RSF_Key32_to_handle(handle);
        return c_fstluk_rsf(vfield, file_handle, handle, ni, nj, nk);
    } else if (key_type == 0) {
        return c_fstluk_xdf(vfield, handle, ni, nj, nk);
    }

    Lib_Log(APP_LIBFST, APP_ERROR, "%s: Key 0x%x does not seem to be valid (for either XDF or RSF)\n", __func__, handle);

    return ERR_BAD_HNDL;
}


//! Helper function for c_fstmsq
static inline char isignore(const char chr) {
    return (chr == '*') ? 0 : 0x3f;
}


//! Helper function for c_fstmsq
static inline char inv_isignore(const char chr) {
    return (chr == 0x3f) ? ' ' : '*';
}

//! \copydoc c_fstmsq
//! XDF version
int c_fstmsq_xdf(
    //! [in] Unit number associated to the file
    const int iun,
    //! [in, out] Mask for vertical level
    int *mip1,
    //! [in, out] Mask for the forecast hour
    int *mip2,
    //! [in, out] Mask for the user defined identifier
    int *mip3,
    //! [in, out] Mask for the label
    char *metiket,
    //! [in] Operation: Set when 0, Get otherwise
    const int getmode
) {

    int index = file_index_xdf(iun);
    if (index == ERR_NO_FILE) {
        Lib_Log(APP_LIBFST, APP_ERROR, "%s: file (unit=%d) is not open\n", __func__, iun);
        return ERR_NO_FILE;
    }

    file_table_entry *fte = file_table[index];

    if (! fte->cur_info->attr.std) {
        Lib_Log(APP_LIBFST, APP_ERROR, "%s: file (unit=%d) is not a RPN standard file\n", __func__, iun);
        return ERR_NO_FILE;
    }

    stdf_dir_keys * search_mask = (stdf_dir_keys *) fte->srch_mask;
    if (getmode) {
        *mip1 = ~(search_mask->ip1) & 0xfffffff;
        *mip2 = ~(search_mask->ip2) & 0xfffffff;
        *mip3 = ~(search_mask->ip3) & 0xfffffff;
        for (int i = 0; i <= 4; i++) {
            metiket[i] = inv_isignore( ((search_mask->etik15 >> ((4-i)*6)) & 0x3f) );
        }

        for (int i = 5; i <= 9; i++) {
            metiket[i] = inv_isignore( ((search_mask->etik6a >> ((9-i)*6)) & 0x3f) );
        }

        metiket[10] = inv_isignore( ((search_mask->etikbc >> 6) & 0x3f) );
        metiket[11] = inv_isignore((search_mask->etikbc  & 0x3f));
        metiket[12] = '\0';
    } else {
        search_mask->ip1 = ~(*mip1) & 0xfffffff;
        search_mask->ip2 = ~(*mip2) & 0xfffffff;
        search_mask->ip3 = ~(*mip3) & 0xfffffff;
        search_mask->etik15 =
            (isignore(metiket[0]) << 24) |
            (isignore(metiket[1]) << 18) |
            (isignore(metiket[2]) << 12) |
            (isignore(metiket[3]) <<  6) |
            (isignore(metiket[4]));
        search_mask->etik6a =
            (isignore(metiket[5]) << 24) |
            (isignore(metiket[6]) << 18) |
            (isignore(metiket[7]) << 12) |
            (isignore(metiket[8])<<  6) |
            (isignore(metiket[9]));
        search_mask->etikbc =
            (isignore(metiket[10]) <<  6) |
            (isignore(metiket[11]));
    }
    return 0;
}

//! Mask a portion of the research keys
int c_fstmsq(
    //! [in] Unit number associated to the file
    const int iun,
    //! [in, out] Mask for vertical level
    int *mip1,
    //! [in, out] Mask for the forecast hour
    int *mip2,
    //! [in, out] Mask for the user defined identifier
    int *mip3,
    //! [in, out] Mask for the label
    char *metiket,
    //! [in] Operation: Set when 0, Get otherwise
    const int getmode
) {
    int index_fnom;
    const int rsf_status = is_rsf(iun, &index_fnom);

    if (index_fnom == -1) {
        Lib_Log(APP_LIBFST, APP_ERROR, "%s: file (unit=%d) is not connected with fnom\n", __func__, iun);
        return ERR_NO_FNOM;
    }

    if (rsf_status == 1) {
        return c_fstmsq_rsf(iun, index_fnom, mip1, mip2, mip3, metiket, getmode);
    }
    else if (rsf_status == 0) {
        return c_fstmsq_xdf(iun, mip1, mip2, mip3, metiket, getmode);
    }

    return rsf_status;
}

//! \copydoc c_fstnbr
//! XDF version
int c_fstnbr_xdf(
    //! [in] Unit number associated to the file
    const int iun
) {
    int index_fnom = get_fnom_index(iun);
    if (index_fnom == -1) {
        Lib_Log(APP_LIBFST, APP_ERROR, "%s: file (unit=%d) is not connected with fnom\n", __func__, iun);
        return ERR_NO_FNOM;
    }

    int index;
    file_table_entry *fte;
    if ((index = file_index_xdf(iun)) == ERR_NO_FILE) {
        return index;
    }

    fte = file_table[index];
    return fte->nrecords;
}

//! Get the number of records of the file
int c_fstnbr(
    //! [in] Unit number associated to the file
    const int iun
) {
    int index_fnom = get_fnom_index(iun);
    if (index_fnom == -1) {
        Lib_Log(APP_LIBFST, APP_ERROR, "%s: file (unit=%d) is not connected with fnom\n", __func__, iun);
        return ERR_NO_FNOM;
    }

    if (initialize_fst98() <= 0) return -1; // Error message should already be printed

    int status = -1;
    int total_num_records = 0;
    while (index_fnom >= 0) {
        Lib_Log(APP_LIBFST, APP_DEBUG, "%s: Looking at file %d (%s, iun %d), possible type %s (open = %d), next %d\n",
                __func__, index_fnom, FGFDT[index_fnom].file_name, FGFDT[index_fnom].iun,
                FGFDT[index_fnom].attr.rsf ? "RSF" : "XDF",
                fst98_open_files[index_fnom].is_open, fst98_open_files[index_fnom].next_file);

        if (fst98_open_files[index_fnom].is_open == 0) {
            status = c_fstouv(iun, "RND+R/O");
            if (status < 0) return status;
            c_fstfrm(iun);
        } else if (FGFDT[index_fnom].attr.rsf == 1) {
            status = c_fstnbr_rsf(index_fnom);
        } else {
            status = c_fstnbr_xdf(FGFDT[index_fnom].iun);
        }

        if (status < 0) return status;

        total_num_records += status;
        index_fnom = fst98_open_files[index_fnom].next_file;
    }

    return total_num_records;
}


//! Get the number of valid records (excluding deleted records, including ones that were added since opening) in a file
//! XDF version
int c_fstnbrv_xdf(
    //! [in] Unit number associated to the file
    int iun
) {
    int index, index_fnom, nrec;
    file_table_entry *fte;

    index_fnom = get_fnom_index(iun);
    if (index_fnom == -1) {
        Lib_Log(APP_LIBFST, APP_ERROR, "%s: file (unit=%d) is not connected with fnom\n", __func__, iun);
        return ERR_NO_FNOM;
    }

    if ((index = file_index_xdf(iun)) == ERR_NO_FILE) {
        c_fstouv(iun, "RND");
        index = file_index_xdf(iun);
        fte = file_table[index];
        nrec = fte->header->nrec;
        c_fstfrm(iun);
        return nrec;
    }

    fte = file_table[index];
    return fte->header->nrec;
}

//! Get the number of valid records (excluding deleted records, including ones that were added since opening) in a file
int c_fstnbrv(
    //! [in] Unit number associated to the file
    int iun
) {
    int index_fnom = get_fnom_index(iun);
    if (index_fnom == -1) {
        Lib_Log(APP_LIBFST, APP_ERROR, "%s: file (unit=%d) is not connected with fnom\n", __func__, iun);
        return ERR_NO_FNOM;
    }

    int status = -1;
    int total_num_records = 0;
    while (index_fnom >= 0) {
        Lib_Log(APP_LIBFST, APP_DEBUG, "%s: Looking at file %d (iun %d), type %s, next %d\n",
                __func__, index_fnom, FGFDT[index_fnom].iun, FGFDT[index_fnom].attr.rsf ? "RSF" : "XDF", fst98_open_files[index_fnom].next_file);
        if (FGFDT[index_fnom].attr.rsf == 1) {
            status = c_fstnbrv_rsf(index_fnom);
        }
        else {
            status = c_fstnbrv_xdf(FGFDT[index_fnom].iun);
        }

        if (status < 0) return status;

        total_num_records += status;
        index_fnom = fst98_open_files[index_fnom].next_file;
    }

    return total_num_records;
}


//! Print, get, or set a fstd or xdf global option
int c_fstopc(
    //! [in] Option name
    const char * const option,
    //! [in] Value
    const char * const value,
    //! [in] Operation mode (0: set option, 1: print option, 2: get option)
    const int getmode
) {
    int val = 0;

    if (strcmp(option, "MSGLVL") == 0) {
        if (getmode) {
            if (getmode == 2) val = App->LogLevel[APP_LIBFST];
        } else {
            val = Lib_LogLevel(APP_LIBFST, (char*)value);
        }
        if (getmode == 1) {
            Lib_Log(APP_LIBFST, APP_INFO, "%s: option 'MSGLVL' , %d\n", __func__, App->LogLevel[APP_LIBFST]);
        }
        return val;
    }

    if (strcmp(option, "TOLRNC") == 0) {
        Lib_Log(APP_LIBFST, APP_WARNING, "%s: TOLRNC option is deprecated\n", __func__);
        return val;
    }

    if (strcmp(option, "PRINTOPT") == 0) {
        if (getmode){
            if (getmode == 2) val = 0;
        } else {
            snprintf(prnt_options, PRNT_OPTIONS_LEN, "%s", value);
        }
        if (getmode == 1) {
            Lib_Log(APP_LIBFST, APP_INFO, "%s: option PRINTOPT='%s'\n", __func__, prnt_options);
        }
        return val;
    }

    if (strcmp(option, "TURBOCOMP") == 0) {
        if (getmode) {
            if (getmode == 2) val = turbocomp_mode;
        } else {
            for (int i = 0; i < 2; i++) {
                if (strcmp(comptab[i], value) == 0) {
                    turbocomp_mode = i;
                    break;
                }
            }
            c_armn_compress_setlevel(turbocomp_mode);
        }
        if (getmode == 1) {
            Lib_Log(APP_LIBFST, APP_INFO, "%s: option TURBOCOMP=%s\n", __func__, comptab[turbocomp_mode]);
        }
        return val;
    }

    Lib_Log(APP_LIBFST, APP_ERROR, "%s: unknown option '%s'\n", __func__, option);
    return val;
}


//! Print, get, or set a fstd or xdf global option
int c_fstopi(
    //! [in] Option name (MSGLVL, TOLRNC, TURBOCOMP)
    const char * const option,
    //! [in] Value
    const int value,
    //! [in] Operation mode (1: print option, 0: set option, 2: get option)
    const int getmode
) {
    int val = 0;

    if (strcmp(option, "MSGLVL") == 0) {
        Lib_Log(APP_LIBFST, APP_INFO, "%s: MSGLVL option is deprecated\n", __func__);
        if (getmode) {
            if (getmode == 2) val = App->LogLevel[APP_LIBFST];
            } else {
                App->LogLevel[APP_LIBFST] = (TApp_LogLevel)value;
            }
            if (getmode == 1) {
                Lib_Log(APP_LIBFST, APP_INFO, "%s: option 'MSGLVL' , %d\n", __func__, App->LogLevel[APP_LIBFST]);
            }
        return val;
    }

    if (strcmp(option, "TOLRNC") == 0) {
        Lib_Log(APP_LIBFST, APP_INFO, "%s: TOLRNC option is deprecated\n", __func__);
        return val;
    }

    if (strcmp(option, "TURBOCOMP") == 0) {
        if (getmode) {
            if (getmode == 2) val = turbocomp_mode;
        } else {
            if (value == 0 || value == 1){
                turbocomp_mode = value;
                c_armn_compress_setlevel(turbocomp_mode);
            }
        }
        if (getmode == 1) {
            Lib_Log(APP_LIBFST, APP_INFO, "%s: option TURBOCOMP=%s , %d\n", __func__, comptab[turbocomp_mode], turbocomp_mode);
        }
        return val;
    }

    Lib_Log(APP_LIBFST, APP_ERROR, "%s: unknown option '%s'\n", __func__, option);
    return val;
}


//! Print, get, or set a fstd or xdf global option
int c_fstopl(
    //! [in] Option name (FASTIO, IMAGE, REDUCTION32)
    const char * const option,
    //! [in, out] Value
    const int value,
    //! [in] Operation mode (1: print option, 0: set option, 2: get option)
    const int getmode
) {
    int val = 0;

    if (strcmp(option, "FASTIO") == 0) {
        if (getmode) {
            if (getmode == 2) val = 0;
        }
        if (getmode == 1) {
            Lib_Log(APP_LIBFST, APP_INFO, "%s: FASTIO mode NOT ACTIVE\n", __func__);
        }
        return val;
    }

    if (strcmp(option, "IMAGE") == 0) {
        if (getmode) {
            if (getmode == 2) val = image_mode_copy;
        } else {
            image_mode_copy = value;
        }
        if (getmode == 1) {
            Lib_Log(APP_LIBFST, APP_INFO, "%s: option IMAGE_MODE_COPY=%d\n", __func__, image_mode_copy);
        }
        return val;
    }

    if (strcmp(option, "REDUCTION32") == 0) {
        if (getmode) {
            if (getmode == 2) val = downgrade_32;
        } else {
            downgrade_32 = value;
        }
        if (getmode == 1) {
            Lib_Log(APP_LIBFST, APP_INFO, "%s: option REDUCTION32=%d\n", __func__, downgrade_32);
        }
        return val;
    }

    Lib_Log(APP_LIBFST, APP_ERROR, "%s: unknown option '%s'\n", __func__, option);
    return val;
}


//! This function does nothing
//! \return Always 0
int c_fstopr(
    //! [in] Option name
    char *option,
    //! [in] Value
    float value,
    //! [in] Operation mode (0: set option, 1: print option, 2: get option)
    int getmode
) {
    (void)option;
    (void)value;
    (void)getmode;
    // No current float variable to be set for now
    return 0;
}


//! Check FSTD file for corruption
//! @return 0 when valid; -1 otherwise
int c_fstcheck_xdf(
    //! [in]  Path to the file
    const char * const filePath
) {
    return c_xdfcheck(filePath);
}


//! Check FSTD file for corruption
//! @return 0 when valid; -1 otherwise
int c_fstcheck(
    //! [in]  Path to the file
    const char * const filePath
) {
    const int32_t type = c_wkoffit(filePath, strlen(filePath));
    if (type == WKF_STDRSF) {
        if (RSF_Basic_check(filePath)) return 0;
        return 1;
    } else {
        return c_fstcheck_xdf(filePath);
    }
}

//! Initialize global fst98 structures and variables.
//! Uses the fst98 mutex.
//! \return The maximum number of FST98 files that can be open simultaneously, or 0 if there is an error
static int initialize_fst98(void) {
    if (MAX_FST98_FILES > 0) return MAX_FST98_FILES; // fst98 already initialized
    if (MAX_FNOM_FILES <= 0) {
        Lib_Log(APP_LIBFST, APP_ERROR, "%s: Cannot initialize fst (98) if fnom has never been called/initialized\n", __func__);
        return MAX_FNOM_FILES;
    }

    // --- START critical region ---
    pthread_mutex_lock(&fst98_mutex);

    if (MAX_FST98_FILES > 0) goto unlock; // Check again after locking, in case someone else was initializing

    fst98_open_files = (fstd_usage_info*) calloc(MAX_FNOM_FILES, sizeof(fstd_usage_info));
    if (fst98_open_files == NULL) {
        Lib_Log(APP_LIBFST, APP_ERROR, "%s: Unable to allocate fst98 open files table (%d files)\n",
                __func__, MAX_FNOM_FILES);
        goto unlock;
    }

    
    c_env_var_cracker("FST_OPTIONS", c_fst_env_var, "C"); // Obtain options from environment variable
    C_requetes_init(requetes_filename, debug_filename);
    init_ip_vals();

    MAX_FST98_FILES = MAX_FNOM_FILES; // This signals to other threads that the API is initialized

unlock:
    pthread_mutex_unlock(&fst98_mutex);
    // --- END critical region ---

    return MAX_FST98_FILES;
}

//! Open a RPN standard file
int c_fstouv(
    //! [in] Unit number associated to the file
    const int iun,
    //! [in] Random or sequential access
    const char * const options
) {
    //! \return Number of records in file
    //! \see c_fstfrm

    // Check fnom index first, because we can't initialize the fst98 library if fnom is not itself initialized
    int i = get_fnom_index(iun);
    if (i == -1) {
        Lib_Log(APP_LIBFST, APP_ERROR, "%s: file (unit=%d) is not connected with fnom\n", __func__, iun);
        return ERR_NO_FNOM;
    }

    if (initialize_fst98() <= 0) return -1; // Error message should already be printed

    Lib_Log(APP_LIBFST, APP_DEBUG, "%s: Opening iun %d, fnom index %d, with options %s\n", __func__, iun, i, options);

    int ier = -1;

    char appl[5];
    if (strcasestr(options, "RND")) {
        // standard random
        snprintf(appl, sizeof(appl), "%s", "STDR");
    } else {
        // standard sequential
        snprintf(appl, sizeof(appl), "%s", "STDS");
    }

    // Determine if we're opening in read or write mode
    int read_only = FGFDT[i].attr.read_only;
    if (strcasestr(options, "R/O")) {
        read_only = 1;
    }
    if (strcasestr(options, "R/W")) {
        read_only = 0;
    }

    // Update file attribute, in case we just contradicted it
    if (read_only == 0) {
        FGFDT[i].attr.read_only = 0;
        FGFDT[i].attr.write_mode = 1;
    }
    else {
        FGFDT[i].attr.read_only = 1;
        FGFDT[i].attr.write_mode = 0;
    }

    // Look for attribute volatile (file will be erased on process end)
    if (strcasestr(options, "VOLATILE")) {
        FGFDT[i].attr.volatil = 1;
    }

    // Check for backend type
    int is_rsf = FALSE;
    if (strcasestr(options, "RSF")) {
        is_rsf = TRUE;
    } else if (strcasestr(options, "XDF")) {
        is_rsf = FALSE;
    } else if (fst_backend && strncasecmp("RSF", fst_backend, 3)==0) {
        is_rsf = TRUE;
    } else if (fst_backend && strncasecmp("XDF", fst_backend, 3)==0)  {
        is_rsf = FALSE;
    }

    // Determine whether we open for parallel write, if RSF
    const int32_t seg_size = (strcasestr(options, "PARALLEL") != NULL) ? segment_size_mb : 0;

    const rsf_open_mode_type open_mode = read_only ? RSF_RO : RSF_RW;
    //  (strcasestr(options, "FUSE") != NULL) ? RSF_FUSE : RSF_RW;

    FGFDT[i].attr.std = 1; // force attribute to standard file
    const int iwko = c_wkoffit(FGFDT[i].file_name, strlen(FGFDT[i].file_name));
    if (FGFDT[i].attr.remote) {
        if ((FGFDT[i].eff_file_size == 0) && (! FGFDT[i].attr.old)) {
            if (read_only) {
                Lib_Log(APP_LIBFST, APP_ERROR, "%s: Trying to open in read-only mode a file that does not exist: %s\n",
                        __func__, FGFDT[i].file_name);
                ier = ERR_NO_FILE;
            }
            else if (is_rsf) {
                ier = c_fstouv_rsf(i, RSF_RW, seg_size);
            }
            else {
                ier = c_xdfopn(iun, "CREATE", (word_2 *) &stdfkeys, 16, (word_2 *) &stdf_info_keys, 2, appl);
            }
        } else {
            if (iwko == WKF_STDRSF) {
                Lib_Log(APP_LIBFST, APP_ERROR, "%s: Should open as RSF, but this branch is not implemented.......\n", __func__);
                ier = -1;
            }
            else {
                ier = c_xdfopn(iun, read_only?"READ":"R-W", (word_2 *) &stdfkeys, 16, (word_2 *) &stdf_info_keys, 2, appl);
            }
        }
    } else {
        if ((iwko <= -2) && (! FGFDT[i].attr.old)) {
            if (read_only) {
                Lib_Log(APP_LIBFST, APP_ERROR, "%s: Trying to open in read-only mode a file that does not exist: %s\n",
                        __func__, FGFDT[i].file_name);
                ier = ERR_NO_FILE;
            }
            else if (is_rsf) {
                ier = c_fstouv_rsf(i, RSF_RW, seg_size);
            }
            else {
                ier = c_xdfopn(iun, "CREATE", (word_2 *) &stdfkeys, 16, (word_2 *) &stdf_info_keys, 2, appl);
            }
        } else {
            if (iwko == WKF_STDRSF) {
                ier = c_fstouv_rsf(i, open_mode, seg_size);
            }
            else if (iwko == WKF_SEQUENTIEL98 || iwko == WKF_RANDOM98 || iwko == WKF_SEQUENTIEL89 || iwko == WKF_RANDOM89) {
                ier = c_xdfopn(iun, read_only?"READ":"R-W", (word_2 *) &stdfkeys, 16, (word_2 *) &stdf_info_keys, 2, appl);
            }
            else {
                ier = ERR_WRONG_FTYPE;
            }
        }
    }

    if (ier < 0) return ier;

    init_open_file(&fst98_open_files[i]);

    int nrec = c_fstnbr(iun);
    return nrec;
}


//! Get all the descriptors of a record
//! XDF version
int c_fstprm_xdf(
    //! [in] Handle of the record from which to retreive the descriptors
    int handle,
    //! [out] Origin date time stamp
    int *dateo,
    //! [out] Duration of time steps in seconds
    int *deet,
    //! [out] Time step number
    int *npas,
    //! [out] First dimension of the data field
    int *ni,
    //! [out] Second dimension of the data field
    int *nj,
    //! [out] Third dimension of the data field
    int *nk,
    //! [out] Number of bits kept for each elements of the field
    int *nbits,
    //! [out] Data type of the elements
    int *datyp,
    //! [out] Vertical level
    int *ip1,
    //! [out] Forecast hour
    int *ip2,
    //! [out] User defined identifier
    int *ip3,
    //! [out] Type of field (forecast, analysis, climatology)
    char *typvar,
    //! [out] Variable name
    char *nomvar,
    //! [out] Label
    char *etiket,
    //! [out] Type of geographical projection
    char *grtyp,
    //! [out] First grid descriptor
    int *ig1,
    //! [out] Second grid descriptor
    int *ig2,
    //! [out] Third grid descriptor
    int *ig3,
    //! [out] Fourth grid descriptor
    int *ig4,
    //! [out] Starting word address
    int *swa,
    //! [out] Record length in 32-bit units
    int *lng,
    //! [out] Delete flag
    int *dltf,
    //! [out] Unused bit count
    int *ubc,
    //! [out] Extra parameter
    int *extra1,
    //! [out] Extra parameter
    int *extra2,
    //! [out] Extra parameter
    int *extra3
) {
    stdf_dir_keys *stdf_entry;
    stdf_special_parms cracked;
    uint32_t *pkeys;
    int ier, addr, idtyp, xdflng, l1, l2, l3, l4;

    stdf_entry = (stdf_dir_keys *) calloc(1, sizeof(stdf_dir_keys));
    pkeys = stdf_entry->words;
    pkeys += W64TOWD(1);

    ier = c_xdfprm(handle, &addr, &xdflng, &idtyp, pkeys, 16);

    crack_std_parms(stdf_entry, &cracked);

    *ni = stdf_entry->ni;
    *nj = stdf_entry->nj;
    *nk = stdf_entry->nk;
    *dateo = cracked.date_stamp;
    *deet = stdf_entry->deet;
    *npas = stdf_entry->npas;
    *nbits = stdf_entry->nbits;
    *datyp = stdf_entry->datyp;
    *ip1 = stdf_entry->ip1;
    *ip2 = stdf_entry->ip2;
    *ip3 = stdf_entry->ip3;
    *ig1 = stdf_entry->ig1;
    *ig2 = cracked.ig2;
    *ig3 = stdf_entry->ig3;
    *ig4 = stdf_entry->ig4;
    *swa = addr;
    *lng = W64TOWD(xdflng);
    *dltf = stdf_entry->deleted;
    *ubc = stdf_entry->ubc;
    // new, use to be undefined
    *extra1 = cracked.date_valid;
    *extra2 = 0;
    *extra3 = 0;

    for (l1 = 0; (typvar[l1] != '\0') && (l1 < 2); l1++);
    for (l2 = 0; (nomvar[l2] != '\0') && (l2 < 4); l2++);
    for (l3 = 0; (etiket[l3] != '\0') && (l3 < 12); l3++);
    l4 = 1;
    strncpy(typvar, cracked.typvar, l1);
    strncpy(nomvar, cracked.nomvar, l2);
    strncpy(etiket, cracked.etiket, l3);
    strncpy(grtyp, cracked.gtyp, l4);
    free(stdf_entry);
    return ier;
}

//! Get all the descriptors of a record
int c_fstprm(
    //! [in] Handle of the record from which to retreive the descriptors
    int handle,
    //! [out] Origin date time stamp
    int *dateo,
    //! [out] Duration of time steps in seconds
    int *deet,
    //! [out] Time step number
    int *npas,
    //! [out] First dimension of the data field
    int *ni,
    //! [out] Second dimension of the data field
    int *nj,
    //! [out] Third dimension of the data field
    int *nk,
    //! [out] Number of bits kept for each elements of the field
    int *nbits,
    //! [out] Data type of the elements
    int *datyp,
    //! [out] Vertical level
    int *ip1,
    //! [out] Forecast hour
    int *ip2,
    //! [out] User defined identifier
    int *ip3,
    //! [out] Type of field (forecast, analysis, climatology)
    char *typvar,
    //! [out] Variable name
    char *nomvar,
    //! [out] Label
    char *etiket,
    //! [out] Type of geographical projection
    char *grtyp,
    //! [out] First grid descriptor
    int *ig1,
    //! [out] Second grid descriptor
    int *ig2,
    //! [out] Third grid descriptor
    int *ig3,
    //! [out] Fourth grid descriptor
    int *ig4,
    //! [out] Starting word address
    int *swa,
    //! [out] Record length in 32-bit units
    int *lng,
    //! [out] Delete flag
    int *dltf,
    //! [out] Unused bit count
    int *ubc,
    //! [out] Extra parameter
    int *extra1,
    //! [out] Extra parameter
    int *extra2,
    //! [out] Extra parameter
    int *extra3
) {
    const int32_t key_type = RSF_Key32_type(handle);
    if (key_type == 1) {
        int dasiz;
        RSF_handle file_handle = RSF_Key32_to_handle(handle);
        return c_fstprm_rsf(file_handle, handle, dateo, deet, npas, ni, nj, nk, nbits, datyp, &dasiz, ip1, ip2, ip3, typvar,
                            nomvar, etiket, grtyp, ig1, ig2, ig3, ig4, swa, lng, dltf, ubc, extra1, extra2, extra3);
    } else if (key_type == 0) {
        return c_fstprm_xdf(handle, dateo, deet, npas, ni, nj, nk, nbits, datyp, ip1, ip2, ip3, typvar, nomvar,
                            etiket, grtyp, ig1, ig2, ig3, ig4, swa, lng, dltf, ubc, extra1, extra2, extra3);
    }

    return ERR_BAD_HNDL;
}


//! Reset all the flags previously set by ip(1-3)_val
void c_fstreset_ip_flags(void) {
    init_ip_vals();
}


//! Rewinds a RPN standard sequential file
int c_fstrwd_xdf(
    //! [in] Unit number associated to the file
    int iun
) {
    int index_fnom = get_fnom_index(iun);
    if (index_fnom == -1) {
        Lib_Log(APP_LIBFST, APP_ERROR, "%s: file (unit=%d) is not connected with fnom\n", __func__, iun);
        return ERR_NO_FNOM;
    }

    int index = file_index_xdf(iun);
    if (index  == ERR_NO_FILE) {
        Lib_Log(APP_LIBFST, APP_ERROR, "%s: file (unit=%d) is not open\n", __func__, iun);
        return ERR_NO_FILE;
    }

    file_table_entry *fte = file_table[index];

    if (! fte->cur_info->attr.std) {
        Lib_Log(APP_LIBFST, APP_ERROR, "%s: file (unit=%d) is not a RPN standard file\n", __func__, iun);
        return ERR_NO_FILE;
    }

    if (! fte->xdf_seq) {
        Lib_Log(APP_LIBFST, APP_WARNING, "%s: file (unit=%d) is not sequential\n", __func__, iun);
        return ERR_BAD_FTYPE;
    }

    fte->cur_addr = fte->seq_bof;
    fte->valid_pos = 0;
    return 0;
}

//! Rewinds a RPN standard sequential file
int c_fstrwd(
    //! [in] Unit number associated to the file
    int iun
) {
    int index_fnom;
    const int rsf_status = is_rsf(iun, &index_fnom);

    if (index_fnom == -1) {
        Lib_Log(APP_LIBFST, APP_ERROR, "%s: file (unit=%d) is not connected with fnom\n", __func__, iun);
        return ERR_NO_FNOM;
    }

    if (rsf_status == 1) {
        Lib_Log(APP_LIBFST, APP_WARNING, "%s: Can't rewind an RSF file\n", __func__);
        return 0;
    }
    else if (rsf_status == 0) {
        return c_fstrwd_xdf(iun);
    }

    return rsf_status;
}


//! Skip nrec records forward or backward in the sequential file
int c_fstskp_xdf(
    //! [in] Unit number associated to the file
    int iun,
    //! Number of records to skip.  A negative number means backward.
    int nrec
) {
    //! \return 0 on success, error code otherwise

    int index_fnom, index, i, nw, cur_pos, dim;
    file_table_entry *fte;
    xdf_record_header header64;
    postfix_seq postfix;
    seq_dir_keys seq_entry;

    index_fnom = get_fnom_index(iun);
    if (index_fnom == -1) {
        Lib_Log(APP_LIBFST, APP_ERROR, "%s: file (unit=%d) is not connected with fnom\n", __func__, iun);
        return ERR_NO_FNOM;
    }

    if ((index = file_index_xdf(iun)) == ERR_NO_FILE) {
        Lib_Log(APP_LIBFST, APP_ERROR, "%s: file (unit=%d) is not open\n", __func__, iun);
        return ERR_NO_FILE;
    }

    fte = file_table[index];

    if (!fte->xdf_seq) {
        Lib_Log(APP_LIBFST, APP_WARNING, "%s: file (unit=%d) is not sequential\n", __func__, iun);
        return ERR_BAD_FTYPE;
    }

    if (! fte->fstd_vintage_89) {
        if (nrec > 0) {
            // skip forward
            for (i = 0; i < nrec; i++) {
                nw = c_waread2(iun, &header64, fte->cur_addr, W64TOWD(1));
                if ((nw != W64TOWD(1)) || ((header64.idtyp >= 112) && (header64.idtyp <= 127))) {
                    Lib_Log(APP_LIBFST, APP_INFO, "%s: (unit %d) skip to end of file\n", __func__, iun);
                    break;
                }
                cur_pos = fte->cur_addr;
                fte->cur_addr += W64TOWD(header64.lng);
                c_waread(iun, &postfix, fte->cur_addr, W64TOWD(2));
                if ((postfix.idtyp == 0) && (postfix.lng == 2) && (postfix.addr == (unsigned)-1)) {
                    // skip postfix also
                    fte->cur_addr += W64TOWD(2);
                } else {
                    Lib_Log(APP_LIBFST, APP_FATAL, "%s: file (unit=%d) has invalid or no record postfix\n", __func__, iun);
                    return ERR_NO_POSTFIX;
                }
            }
        } else {
            nrec = -nrec;
            for (i = 0; i < nrec; i++) {
                if ((fte->cur_addr - W64TOWD(2)) > fte->seq_bof) {
                    c_waread(iun, &postfix, (fte->cur_addr - W64TOWD(2)), W64TOWD(2));
                    if ((postfix.idtyp == 0) && (postfix.lng == 2) && (postfix.addr == (unsigned)-1)) {
                        fte->cur_addr = W64TOWD( (postfix.prev_addr - 1) )+1;
                    } else {
                        Lib_Log(APP_LIBFST, APP_FATAL, "%s: file (unit=%d) has no record postfix\n", __func__, iun);
                        return ERR_NO_POSTFIX;
                    }
                    c_waread(iun, &header64, fte->cur_addr, W64TOWD(1));
                    if (header64.addr != (WDTO64( (fte->cur_addr -1) )+1)) {
                        Lib_Log(APP_LIBFST, APP_FATAL, "%s: file (unit=%d), postfix address (%d) not equal to record address (%d)\n", __func__, iun, (WDTO64((fte->cur_addr -1))+1), header64.addr);
                        return ERR_NO_POSTFIX;
                    }
                }
            }
        }
    } else {
        // old sequential standard file
        dim = sizeof(seq_entry) / sizeof(int32_t);
        if (nrec < 0) {
            // skip forward
            for (i = 0; i < nrec; i++) {
                nw = c_waread2(iun, &seq_entry, fte->cur_addr, dim);
                if ((nw != dim) || (seq_entry.eof > 0)) {
                    Lib_Log(APP_LIBFST, APP_INFO, "%s: (unit %d) skip to end of file\n", __func__, iun);
                    break;
                }
                fte->cur_addr += W64TOWD( (((seq_entry.lng + 3) >> 2) + 15) );
            }
        } else {
            // skip backward
            nrec = -nrec;
            for (i = 0; i < nrec; i++) {
                if (fte->cur_addr <= 1) {
                    fte->cur_addr = 1;
                    break;
                }
                c_waread(iun, &seq_entry, fte->cur_addr, dim);
                cur_pos = fte->cur_addr;
                fte->cur_addr = seq_entry.swa_last * W64TOWD(15) + 1;
                if (fte->cur_addr >= cur_pos) {
                    // not moving backward anymore
                    fte->cur_addr = 1;
                    break;
                }
            }
        }
    }
    return 0;
}

//! Skip nrec records forward or backward in the sequential file
int c_fstskp(
    //! [in] Unit number associated to the file
    int iun,
    //! Number of records to skip.  A negative number means backward.
    int nrec
) {
    int index_fnom;
    const int rsf_status = is_rsf(iun, &index_fnom);

    if (index_fnom == -1) {
        Lib_Log(APP_LIBFST, APP_ERROR, "%s: file (unit=%d) is not connected with fnom\n", __func__, iun);
        return ERR_NO_FNOM;
    }

    if (rsf_status == 1) {
        Lib_Log(APP_LIBFST, APP_WARNING, "%s: Can't \"skip\" records in an RSF file\n", __func__);
        return 0;
    }
    else if (rsf_status == 0) {
        return c_fstskp_xdf(iun, nrec);
    }

    return rsf_status;
}


//! \copydoc c_fstsui
//! XDF version
int c_fstsui_xdf(
    //! [in] Unit number associated to the file
    int iun,
    //! [out] Dimension 1 of the data field
    int *ni,
    //! [out] Dimension 2 of the data field
    int *nj,
    //! [out] Dimension 3 of the data field
    int *nk
) {
    uint32_t *primk = NULL;

    // position to the next record that matches the last search criterias
    int handle = c_xdfloc(iun, -1, primk, 0); // find next with handle = -1 and nprim = 0
    if (handle < 0) {
        Lib_Log(APP_LIBFST, APP_DEBUG, "%s: record not found, errcode=%d\n", __func__, handle);
        return handle;
    }

    stdf_dir_keys * stdf_entry = (stdf_dir_keys *) calloc(1, sizeof(stdf_dir_keys));
    uint32_t * pkeys = stdf_entry->words;
    pkeys += W64TOWD(1);

    int addr, lng, idtyp;
    c_xdfprm(handle, &addr, &lng, &idtyp, pkeys, 16);
    *ni = stdf_entry->ni;
    *nj = stdf_entry->nj;
    *nk = stdf_entry->nk;
    free(stdf_entry);
    return handle;
}

//! Find the next record that matches the last search criterias
int c_fstsui(
    //! [in] Unit number associated to the file
    int iun,
    //! [out] Dimension 1 of the data field
    int *ni,
    //! [out] Dimension 2 of the data field
    int *nj,
    //! [out] Dimension 3 of the data field
    int *nk
) {
    int index_fnom;
    is_rsf(iun, &index_fnom);

    if (index_fnom == -1) {
        Lib_Log(APP_LIBFST, APP_ERROR, "%s: file (unit=%d) is not connected with fnom\n", __func__, iun);
        return ERR_NO_FNOM;
    }

    int status = -1;

    // In case the file to search belongs to a linked list, find the next file in that list
    // that was not fully searched yet
    while (fst98_open_files[index_fnom].search_done == 1) {
        index_fnom = fst98_open_files[index_fnom].next_file;
        if (index_fnom < 0) return ERR_NOT_FOUND;
    }

    Lib_Log(APP_LIBFST, APP_DEBUG, "%s: Looking at file %d (iun %d), type %s, next %d\n",
            __func__, index_fnom, FGFDT[index_fnom].iun, FGFDT[index_fnom].attr.rsf ? "RSF" : "XDF", fst98_open_files[index_fnom].next_file);

    // Look for the next record in that not-finished-searching file
    if (FGFDT[index_fnom].attr.rsf == 1) {
        status = c_fstsui_rsf(FGFDT[index_fnom].iun, index_fnom, ni, nj, nk);
    }
    else if (FGFDT[index_fnom].attr.rsf == 0) {
        status = c_fstsui_xdf(FGFDT[index_fnom].iun, ni, nj, nk);
    }

    if (status > 0) return status; // Found it!

    // Stop if error, unless it's just because we didn't find a match. In that case, we will continue looking (in the next file in list)
    if (status < 0 && status != ERR_NOT_FOUND) return status;

    // We're done searching this file
    fst98_open_files[index_fnom].search_done = 1;

    // We will now be looking at the next file in the linked list
    const fstd_usage_info* old_info = &fst98_open_files[index_fnom];
    index_fnom = old_info->next_file;
    if (index_fnom >= 0) {
        // Return whatever we find with fstinf. It will look in all further files in the linked list, if necessary. It will
        // also set up the next call to fstsui, if needed.
        return c_fstinf(FGFDT[index_fnom].iun, ni, nj, nk,
            old_info->inf_param.datev, old_info->inf_param.etiket, old_info->inf_param.ip1, old_info->inf_param.ip2, old_info->inf_param.ip3,
            old_info->inf_param.typvar, old_info->inf_param.nomvar);
    }

    return -1;
}


//! Get the version number
int c_fst_version(void) {
    return stdf_version;
}


//! \copydoc c_fstvoi
int c_fstvoi_xdf(
    //! [in] Unit number associated to the file
    const int iun,
    //! [in] List of fields to print
    const char * const options,
    const int print_stats,              //!< Whether to print stats after record info
    int64_t* const total_num_entries,         //!< [in,out] Accumulate number of entries (for linked files)
    int64_t* const total_num_valid_records,   //!< [in,out] Accumulate number of valid record  (for linked files)
    int64_t* const total_file_size,           //!< [in,out] Accumulate total size of linked files
    int64_t* const total_num_writes,          //!< [in,out] Accumulate write count (for linked files)
    int64_t* const total_num_rewrites,        //!< [in,out] Accumulate rewrite count (for linked files)
    int64_t* const total_num_erasures,        //!< [in,out] Accumulate erasure count (for linked files)
    int64_t* const total_erased_size          //!< [in,out] Accumulate size of erased records (for linked files)
) {
    int index_fnom = get_fnom_index(iun);
    if (index_fnom == -1) {
        Lib_Log(APP_LIBFST, APP_ERROR, "%s: file (unit=%d) is not connected with fnom\n", __func__, iun);
        return ERR_NO_FNOM;
    }

    int fileIdx = file_index_xdf(iun);
    if (fileIdx == ERR_NO_FILE) {
        Lib_Log(APP_LIBFST, APP_ERROR, "%s: file (unit=%d) is not open\n", __func__, iun);
        return ERR_NO_FILE;
    }

    file_table_entry* fte = file_table[fileIdx];

    if (! fte->cur_info->attr.std) {
        Lib_Log(APP_LIBFST, APP_ERROR, "%s: file (unit=%d) is not a RPN standard file\n", __func__, iun);
        return ERR_NO_FILE;
    }

    int64_t erased_size = 0;
    int nrec = 0;
    int width = W64TOWD(fte->primary_len);
    stdf_dir_keys* stdf_entry;
    xdf_record_header* header;
    char string[20];
    if (! fte->xdf_seq) {
        for (int i = 0; i < fte->npages; i++) {
            uint32_t* entry = (fte->dir_page[i])->dir.entry;
            for (unsigned int j = 0; j < (fte->dir_page[i])->dir.nent; j++) {
                header = (xdf_record_header *) entry;
                if (header->idtyp < 112) {
                    stdf_entry = (stdf_dir_keys *) entry;
                    snprintf(string, sizeof(string), "%5ld-", *total_num_valid_records + nrec);
                    print_std_parms(stdf_entry, string, options, (((*total_num_valid_records + nrec) % 70) == 0));
                    nrec++;
                }
                else if ((header->idtyp & 0x7E) == 0x7E) { // A deleted record
                    erased_size += header->lng;
                }
                entry += width;
            }
        }
    } else {
        // xdf sequential
        int end_of_file = 0;
        while (! end_of_file) {
            int nw = c_waread2(iun, fte->head_keys, fte->cur_addr, width);
            header = (xdf_record_header *) fte->head_keys;
            if ((header->idtyp >= 112) || (nw < W64TOWD(1))) {
                if ((header->idtyp >= 112) && (header->idtyp < 127)) {
                    fte->cur_addr += W64TOWD(1);
                }
                end_of_file = 1;
                break;
            }
            if (fte->fstd_vintage_89) {
                // old sequential standard
                if ((stdf_entry = calloc(1, sizeof(stdf_dir_keys))) == NULL) {
                    Lib_Log(APP_LIBFST, APP_FATAL, "%s: memory is full\n", __func__);
                    return ERR_MEM_FULL;
                }
                seq_dir_keys* seq_entry = (seq_dir_keys *) fte->head_keys;
                if (seq_entry->dltf) {
                    fte->cur_addr += W64TOWD( (((seq_entry->lng + 3) >> 2)+15) );
                    free(stdf_entry);
                    continue;
                }
                if (seq_entry->eof > 0) {
                    if (seq_entry->eof < 15) {
                        fte->cur_addr += W64TOWD(1);
                    }
                    end_of_file = 1;
                    free(stdf_entry);
                    break;
                }
                stdf_entry->deleted = 0;
                stdf_entry->select = 1;
                stdf_entry->lng = ((seq_entry->lng + 3) >> 2) + 15;
                stdf_entry->addr = (seq_entry->swa >> 2) +1;
                stdf_entry->deet = seq_entry->deet;
                stdf_entry->nbits = seq_entry->nbits;
                stdf_entry->ni = seq_entry->ni;
                stdf_entry->gtyp = seq_entry->grtyp;
                stdf_entry->nj = seq_entry->nj;
                stdf_entry->datyp = seq_entry->datyp;
                stdf_entry->nk = seq_entry->nk;
                stdf_entry->ubc = 0;
                stdf_entry->npas = (seq_entry->npas2 << 16) |
                seq_entry->npas1;
                stdf_entry->pad7 = 0;
                stdf_entry->ig4 = seq_entry->ig4;
                stdf_entry->ig2a = 0;
                stdf_entry->ig1 = seq_entry->ig1;
                stdf_entry->ig2b = seq_entry->ig2 >> 8;
                stdf_entry->ig3 = seq_entry->ig3;
                stdf_entry->ig2c = seq_entry->ig2 & 0xff;
                stdf_entry->etik15 =
                (ascii6(seq_entry->etiq14 >> 24) << 24) |
                (ascii6((seq_entry->etiq14 >> 16) & 0xff) << 18) |
                (ascii6((seq_entry->etiq14 >>  8) & 0xff) << 12) |
                (ascii6((seq_entry->etiq14      ) & 0xff) <<  6) |
                (ascii6((seq_entry->etiq56 >>  8) & 0xff));
                stdf_entry->pad1 = 0;
                stdf_entry->etik6a =
                (ascii6((seq_entry->etiq56      ) & 0xff) << 24) |
                (ascii6((seq_entry->etiq78 >>  8) & 0xff) << 18) |
                (ascii6((seq_entry->etiq78      ) & 0xff) << 12);
                stdf_entry->pad2 = 0;
                stdf_entry->etikbc = 0;
                stdf_entry->typvar = ascii6(seq_entry->typvar) << 6;
                stdf_entry->pad3 = 0;
                stdf_entry->nomvar =
                (ascii6((seq_entry->nomvar >>  8) & 0xff) << 18) |
                (ascii6((seq_entry->nomvar      ) & 0xff) << 12);
                stdf_entry->dasiz = 0;
                stdf_entry->ip1 = seq_entry->ip1;
                stdf_entry->levtyp = 0;
                stdf_entry->ip2 = seq_entry->ip2;
                stdf_entry->pad5 = 0;
                stdf_entry->ip3 = seq_entry->ip3;
                stdf_entry->pad6 = 0;
                stdf_entry->date_stamp = seq_entry->date;
                int deet = stdf_entry->deet;
                int npas = stdf_entry->npas;
                long long deetnpas = npas;
                deetnpas = deetnpas * deet;
                if ((deetnpas % 3600) != 0) {
                    /* recompute datev to take care of rounding used with 1989 version
                    * de-octalise the date_stamp */
                    int run = stdf_entry->date_stamp & 0x7;
                    unsigned int datexx = (stdf_entry->date_stamp >> 3) * 10 + run;

                    int32_t f_datev = (int32_t) datexx;
                    long long i_nhours = (deetnpas - ((deetnpas+1800)/3600)*3600);
                    double nhours = i_nhours;
                    nhours = (nhours / 3600.0);
                    incdatr_c(&f_datev, &f_datev, &nhours);
                    datexx = (unsigned int) f_datev;
                    // re-octalise the date_stamp
                    stdf_entry->date_stamp = 8 * (datexx/10) + (datexx % 10);
                }
                snprintf(string, sizeof(string), "%5ld-", *total_num_valid_records + nrec);
                print_std_parms(stdf_entry, string, options, (((*total_num_valid_records + nrec) % 70) == 0));
                nrec++;
                fte->cur_addr += W64TOWD( (((seq_entry->lng + 3) >> 2)+15) );
                free(stdf_entry);
            }  else {
                if ((header->idtyp < 1) || (header->idtyp > 127)) {
                    fte->cur_addr += W64TOWD(header->lng);
                    continue;
                }
                stdf_entry = (stdf_dir_keys *) fte->head_keys;
                snprintf(string, sizeof(string), "%5ld-", *total_num_valid_records + nrec);
                print_std_parms(stdf_entry, string, options, (((*total_num_valid_records + nrec) % 70) == 0));
                nrec++;
                fte->cur_addr += W64TOWD(header->lng);
            }
        } // end while
    }

    *total_num_valid_records += nrec;
    if (!fte->xdf_seq && !fte->fstd_vintage_89) {
        *total_num_entries += fte->header->nrec;
        *total_file_size += fte->header->fsiz * 8;
        *total_num_writes += fte->header->nxtn;
        *total_num_rewrites += fte->header->nrwr;
        *total_num_erasures += fte->header->neff - fte->header->nrwr;
        *total_erased_size += erased_size * sizeof(uint64_t);
    }

    if (print_stats) {
        fprintf(stdout, "\nSTATISTICS for file %s, unit=%d\n\n", FGFDT[index_fnom].file_name, iun);
        if (fte->fstd_vintage_89) {
            snprintf(string, sizeof(string), "Version 1989");
        } else {
            snprintf(string, sizeof(string), "Version 1998");
        }
        if (fte->xdf_seq) {
            fprintf(stdout, "%d records in sequential RPN standard file (%s)\n", nrec, string);
        } else {
            if (! fte->fstd_vintage_89) {
                fprintf(stdout, "Number of directory entries \t %d\n", fte->header->nrec);
                fprintf(stdout, "Number of valid records     \t %d\n", nrec);
                fprintf(stdout, "File size                   \t %d Words (%.3f MB)\n",
                        W64TOWD(fte->header->fsiz), (fte->header->fsiz * sizeof(uint64_t)) / (1024.f * 1024.f));
                fprintf(stdout, "Number of writes            \t %d\n", fte->header->nxtn);
                fprintf(stdout, "Number of rewrites          \t %d\n", fte->header->nrwr);
                fprintf(stdout, "Number of erasures          \t %d (%ld words, %.3f MB)\n",
                        fte->header->neff - fte->header->nrwr, W64TOWD(erased_size),
                        (erased_size * sizeof(uint64_t)) / (1024.f * 1024.f));
            }
            fprintf(stdout, "\n%d records in random RPN standard file (%s)\n\n", nrec, string);
        }
    }

    return 0;
}

//! Print the directory of a RPN standard file
int c_fstvoi(
    //! [in] Unit number associated to the file
    const int iun,
    //! [in] List of fields to print
    const char * const options
) {
    int index_fnom = get_fnom_index(iun);
    if (index_fnom == -1) {
        Lib_Log(APP_LIBFST, APP_ERROR, "%s: file (unit=%d) is not connected with fnom\n", __func__, iun);
        return ERR_NO_FNOM;
    }

    const int print_single_stats = (fst98_open_files[index_fnom].next_file < 0);
    int status = -1;
    int num_files = 0;

    int64_t total_num_entries       = 0;
    int64_t total_num_valid_records = 0;
    int64_t total_file_size         = 0;
    int64_t total_num_writes        = 0;
    int64_t total_num_rewrites      = 0;
    int64_t total_num_erasures      = 0;
    int64_t total_erased_size       = 0;

    while (index_fnom >= 0) {
        Lib_Log(APP_LIBFST, APP_DEBUG, "%s: Looking at file %d (iun %d), type %s, next %d\n",
                __func__, index_fnom, FGFDT[index_fnom].iun, FGFDT[index_fnom].attr.rsf ? "RSF" : "XDF", fst98_open_files[index_fnom].next_file);
        num_files++;

        if (FGFDT[index_fnom].attr.rsf == 1) {
            status = c_fstvoi_rsf(FGFDT[index_fnom].iun, index_fnom, options, print_single_stats, &total_num_entries,
                                  &total_num_valid_records, &total_file_size, &total_num_writes, &total_num_rewrites,
                                  &total_num_erasures, &total_erased_size);
        }
        else {
            status = c_fstvoi_xdf(FGFDT[index_fnom].iun, options, print_single_stats, &total_num_entries,
                                  &total_num_valid_records, &total_file_size, &total_num_writes, &total_num_rewrites,
                                  &total_num_erasures, &total_erased_size);
        }

        if (status < 0) return status;

        index_fnom = fst98_open_files[index_fnom].next_file;
    }

    if (!print_single_stats) {
        fprintf(stdout, "Statistics for %d linked files: \n", num_files);
        fprintf(stdout, "Number of directory entries   \t %ld\n", total_num_entries);
        fprintf(stdout, "Number of valid records       \t %ld\n", total_num_valid_records);
        fprintf(stdout, "File size                     \t %ld bytes (%.3f MB)\n",
                total_file_size, total_file_size / (1024.f * 1024.f));
        fprintf(stdout, "Number of writes              \t %ld\n", total_num_writes);
        fprintf(stdout, "Number of rewrites (XDF only) \t %ld\n", total_num_rewrites);
        fprintf(stdout, "Number of erasures (XDF only) \t %ld (%ld bytes, %.3f MB)\n",
                total_num_erasures, total_erased_size, total_erased_size  / (1024.f * 1024.f));
    }

    return status;
}


//! Write a logical end of file on a sequential file
int c_fstweo_xdf(
    //! [in] Unit number associated to the file
    int iun,
    //! [in] Level of logical end of file
    int level
) {
    int index_fnom, index;
    file_table_entry *fte;
    xdf_record_header header64;

    index_fnom = get_fnom_index(iun);
    if (index_fnom == -1) {
        Lib_Log(APP_LIBFST, APP_ERROR, "%s: file (unit=%d) is not connected with fnom\n", __func__, iun);
        return ERR_NO_FNOM;
    }

    if ((index = file_index_xdf(iun)) == ERR_NO_FILE) {
        Lib_Log(APP_LIBFST, APP_ERROR, "%s: file (unit=%d) is not open\n", __func__, iun);
        return ERR_NO_FILE;
    }

    fte = file_table[index];

    if (!fte->xdf_seq) {
        Lib_Log(APP_LIBFST, APP_WARNING, "%s: file (unit=%d) is not sequential\n", __func__, iun);
        return ERR_BAD_FTYPE;
    }

    VALID(level, 1, 15, "level");
    if (level < 15) {
        header64.idtyp = 112 + level;
        header64.lng = 1;
        header64.addr = fte->cur_addr;
        fte->nxtadr = fte->cur_addr;
        c_wawrit(iun, &header64, fte->nxtadr, W64TOWD(1));
        fte->nxtadr += W64TOWD(1);
        fte->cur_addr += W64TOWD(1);
    }
    header64.idtyp = 127;
    header64.addr = fte->cur_addr;
    c_wawrit(iun, &header64, fte->cur_addr, W64TOWD(1));
    fte->nxtadr = fte->cur_addr;
    return 0;
}

//! Write a logical end of file on a sequential file
int c_fstweo(
    //! [in] Unit number associated to the file
    int iun,
    //! [in] Level of logical end of file
    int level
) {
    int index_fnom;
    const int rsf_status = is_rsf(iun, &index_fnom);

    if (index_fnom == -1) {
        Lib_Log(APP_LIBFST, APP_ERROR, "%s: file (unit=%d) is not connected with fnom\n", __func__, iun);
        return ERR_NO_FNOM;
    }

    if (rsf_status == 1) {
        Lib_Log(APP_LIBFST, APP_WARNING, "%s: This function does nothing on an RSF file\n", __func__);
        return 0;
    }
    else if (rsf_status == 0) {
        return c_fstweo_xdf(iun, level);
    }

    return rsf_status;
}


//! Process all sub options from the FST_OPTIONS environment variable
void c_fst_env_var(
    //! [in] Subotion name
    char *cle,
    //! [in] Index of suboption values
    int index,
    //! [in] Suboption value
    char *content
) {
    char *carac;

    if (strcasecmp(cle, "TURBOCOMP") == 0) {
        carac = content;
        while (*carac) {
            *carac = toupper(*carac);
            carac++;
        }
        c_fstopc("TURBOCOMP", content, 0);
    } else if (strcasecmp(cle, "DATATYPE_REMAP") == 0) {
        remap_table[(index-1) % 2][(index-1) / 2] = atoi(content);
        // fprintf(stderr, "Debug+ remap_table[%d][%d]=%d\n", (index-1)%2, (index-1)/2, remap_table[(index-1) % 2][(index-1) / 2]);
        nb_remap = index / 2;
    } else if (strcasecmp(cle, "DEBUGFILE") == 0) {
        debug_filename = malloc(256);
        strncpy(debug_filename, content, 256);
        // fprintf(stderr, "Debug+ debug_filename=%s\n", debug_filename);
    } else if (strcasecmp(cle, "FST_FILTER_FILE") == 0) {
        requetes_filename = malloc(256);
        strncpy(requetes_filename, content, 256);
        // fprintf(stderr, "Debug+ requetes_filename=%s\n", requetes_filename);
    } else if (strcasecmp(cle, "BACKEND") == 0) {
        fst_backend = malloc(4);
        strncpy(fst_backend, content, 4);
        Lib_Log(APP_LIBFST, APP_DEBUG, "%s: fst_backend = %s\n", __func__, fst_backend);
    } else if (strcasecmp(cle, "SEGMENT_SIZE_MB") == 0) {
        const int32_t seg_size_tmp = atoi(content);
        if (seg_size_tmp > 0) {
            segment_size_mb = seg_size_tmp;
        }
        else {
            Lib_Log(APP_LIBFST, APP_WARNING, "%s: Invalid value for key %s (%s). Must be a positive integer."
                    " Keeping default value %d\n", __func__, cle, content, segment_size_mb);
        }
    } else {
        Lib_Log(APP_LIBFST, APP_WARNING, "%s: cle %s non reconnue, index=%d valeur=%s\n", __func__, cle, index, content);
    }

    // for (int i = 0; i < nb_remap; i++) {
    //     fprintf(stderr, "datatyp %d remap to %d\n", remap_table[0][i], remap_table[1][i]);
    // }
    // fprintf(stderr, "-----------------------------------------------\n");
}


//! Generate all possible coded ip1 values for a given level
int c_ip1_all(
    //! [in] IP1 level (float value)
    const float level,
    //! [in] Level kind as defined by \link convip
    const int kind
) {
    //! \warning Bloody global variable that prevents re-entrancy!
    ip1s_flag = 1;

    // Need to copy the inputs to tmp vars since ConvertIp can operate
    // in different modes and does not guaranty that parameters will not
    // be modified when they shouldn't
    int lkind = kind;
    float llevel = level;

    int ip_new = 0;
    ConvertIp(&ip_new, &llevel, &lkind, 2);
    ips_tab[0][ip_nb[0]] = ip_new;
    ip_nb[0]++;
    if (ip_nb[0] >= Max_Ipvals) {
        Lib_Log(APP_LIBFST, APP_ERROR, "%s: ip1 table full (ip_nb=%d)\n", __func__, ip_nb[0]);
        return -1;
    }

    int ip_old = 0;
    if (lkind < 4) {
        ConvertIp(&ip_old, &llevel, &lkind, 3);
    } else {
        // no valid value for oldtype
        ip_old = -9999;
    }
    ips_tab[0][ip_nb[0]] = ip_old;
    ip_nb[0]++;

    if (ip_nb[0] > Max_Ipvals) {
        Lib_Log(APP_LIBFST, APP_ERROR, "%s: ip1 table full (i1_ind=%d)\n", __func__, ip_nb[0]);
        return -1;
    }
    // printf("Debug+ c_ip1_all llevel=%fte lkind=%d ip_new=%d ip_old=%d\n", llevel, lkind, ip_new, ip_old);
    return ip_new;
}


//! Generate all possible coded ip2 values for a given level
int c_ip2_all(
    //! [in] IP2 level (float value)
    const float level,
    //! [in] Level kind as defined in convip
    const int kind
) {
    //! \warning Bloody global variable that prevents re-entrancy!
    ip2s_flag = 1;

    // Need to copy the inputs to tmp vars since ConvertIp can operate
    // in different modes and does not guaranty that parameters will not
    // be modified when they shouldn't
    int lkind = kind;
    float llevel = level;

    int ip_new = 0;
    ConvertIp(&ip_new, &llevel, &lkind, 2);
    ips_tab[1][ip_nb[1]] = ip_new;
    ip_nb[1]++;
    if (ip_nb[1] >= Max_Ipvals) {
        Lib_Log(APP_LIBFST, APP_ERROR, "%s: ip2 table full (ip_nb=%d)\n", __func__, ip_nb[1]);
        return -1;
    }

    int ip_old = 0;
    if (lkind < 4) {
        ConvertIp(&ip_old, &llevel, &lkind, 3);
    } else {
        // no valid value for oldtype
        ip_old = -9999;
    }
    ips_tab[1][ip_nb[1]] = ip_old;
    ip_nb[1]++;

    if (ip_nb[1] > Max_Ipvals) {
        Lib_Log(APP_LIBFST, APP_ERROR, "%s: ip2 table full (i2_ind=%d)\n", __func__, ip_nb[1]);
        return -1;
    }
    return ip_new;
}


//! Generate all possible coded ip3 values
int c_ip3_all(
    //! [in] IP3  (float value)
    const float level,
    //! [in] Level kind as defined in convip
    const int kind
) {
    //! \warning Bloody global variable that prevents re-entrancy!
    ip3s_flag = 1;

    // Need to copy the inputs to tmp vars since ConvertIp can operate
    // in different modes and does not guaranty that parameters will not
    // be modified when they shouldn't
    int lkind = kind;
    float llevel = level;

    int ip_new = 0;
    ConvertIp(&ip_new, &llevel, &lkind, 2);
    ips_tab[2][ip_nb[2]] = ip_new;
    ip_nb[2]++;
    if (ip_nb[2] >= Max_Ipvals) {
        Lib_Log(APP_LIBFST, APP_ERROR, "%s: ip3 table full (ip_nb=%d)\n", __func__, ip_nb[2]);
        return -1;
    }

    int ip_old = 0;
    if (lkind < 4) {
        ConvertIp(&ip_old, &llevel, &lkind, 3);
    } else {
        // no valid value for oldtype
        ip_old = -9999;
    }
    ips_tab[2][ip_nb[2]] = ip_old;
    ip_nb[2]++;

    if (ip_nb[2] > Max_Ipvals) {
        Lib_Log(APP_LIBFST, APP_ERROR, "%s: ip3 table full (i3_ind=%d)\n", __func__, ip_nb[2]);
        return -1;
    }
    return ip_new;
}


//! Generate coded ip1 value for a given level (shorthand for convip)
int c_ip1_val(
    //! [in] IP1 level (float value)
    const float level,
    //! [in] Level kind as defined in convip
    const int kind
) {
    //! \warning Bloody global variable that prevents re-entrancy!
    ip1s_flag = 1;

    // Need to copy the inputs to tmp vars since ConvertIp can operate
    // in different modes and does not guaranty that parameters will not
    // be modified when they shouldn't
    int lkind = kind;
    float llevel = level;

    int ip_new = 0;
    ConvertIp(&ip_new, &llevel, &lkind, 2);
    ips_tab[0][ip_nb[0]] = ip_new;
    ip_nb[0]++;
    if (ip_nb[0] >= Max_Ipvals) {
        Lib_Log(APP_LIBFST, APP_ERROR, "%s: ip1 table full (ip_nb=%d)\n", __func__, ip_nb[0]);
        return -1;
    }
    return ip_new;
}


//! Generate coded ip2 value for a given level (shorthand for convip)
int c_ip2_val(
    //! [in] IP2 level (float value)
    const float level,
    //! [in] Level kind as defined in convip
    const int kind
) {
    //! \warning Bloody global variable that prevents re-entrancy!
    ip2s_flag = 1;

    // Need to copy the inputs to tmp vars since ConvertIp can operate
    // in different modes and does not guaranty that parameters will not
    // be modified when they shouldn't
    int lkind = kind;
    float llevel = level;

    int ip_new = 0;
    ConvertIp(&ip_new, &llevel, &lkind, 2);
    ips_tab[1][ip_nb[1]] = ip_new;
    ip_nb[1]++;
    if (ip_nb[1] >= Max_Ipvals) {
        Lib_Log(APP_LIBFST, APP_ERROR, "%s: ip2 table full (ip_nb=%d)\n", __func__, ip_nb[1]);
        return -1;
    }
    return ip_new;
}


//! Generates coded ip3 value for a ip3 (shorthand for convip)
int c_ip3_val(
    //! [in] IP3 level (float value)
    float level,
    //! [in] Level kind as defined in convip
    int kind
) {
    //! \warning Bloody global variable that prevents re-entrancy!
    ip3s_flag = 1;

    // Need to copy the inputs to tmp vars since ConvertIp can operate
    // in different modes and does not guaranty that parameters will not
    // be modified when they shouldn't
    int lkind = kind;
    float llevel = level;

    int ip_new = 0;
    ConvertIp(&ip_new, &llevel, &lkind, 2);
    ips_tab[2][ip_nb[2]] = ip_new;
    ip_nb[2]++;
    if (ip_nb[2] >= Max_Ipvals) {
        Lib_Log(APP_LIBFST, APP_ERROR, "%s: ip3 table full (ip_nb=%d)\n", __func__, ip_nb[2]);
        return -1;
    }
    return ip_new;
}


// Everything past this point used to be in if_fst98.h
// It has been moved here since this is the only place where it was used


//! Position at the end of a sequential file for an append
int32_t f77name(fstapp)(
    //! [in] Unit number associated to the file
    int32_t *f_iun,
    //! [in] Kept for backward compatibility (not used)
    char *option,
    F2Cl lng
) {
    (void)lng; // Not used anymore (length of option)

    int iun = *f_iun;

    // option not used anymore by c_fstapp
    const int ier = c_fstapp(iun, option);
    return (int32_t) ier;
}


//! Checkpoint: Clear buffers and rewrite headers
int32_t f77name(fstckp)(
    //! [in] Unit number associated to the file
    int32_t *f_iun
) {
    int iun = *f_iun;
    int ier;

    ier = c_fstckp(iun);
    return (int32_t) ier;
}


/*****************************************************************************
 *                                F S T C V T                                *
 *                                                                           *
 *Object                                                                     *
 *         VARIABLE UTILISEE COMME HOLLERITH SERA TRANSFORME EN CARACTERE    *
 *         OU L'INVERSE POUR NOMVAR, TYPVAR, GRID TYPE, ET ETIKET LE MOT       *
 *         ETIKET AURA 2 LOCATIONS SUR UNE MACHINE A 32 BITS                 *
 *                                                                           *
 *ARGUMENTS                                                                  *
 *  IN OUT    NOM       HOLLERITH *2                       [NOMVAR]          *
 *  IN OUT    TYP       HOLLERITH *1                       [TYPVAR]          *
 *  IN OUT    ETIK      HOLLERITH *8   2 MOTS A4 POUR SUN  [ETIKET]          *
 *  IN OUT    GRTP      HOLLERITH *1                       [GRTYP]           *
 *  OUT IN    CNOM      CARACTERE *2                                         *
 *  OUT IN    CTYP      CARACTERE *1                                         *
 *  OUT IN    CETIK     CARACTERE *8                                         *
 *  OUT IN    CGRTP     CARACTRE *1                                          *
 *  IN        HOLACAR   LOGICAL .TRUE.  HOLLERITH A CARATERE                 *
 *                      LOGICAL .FALSE. CARACTERE A HOLLERITH                *
 *                                                                           *
 *****************************************************************************/
int32_t f77name(fstcvt)(
    int32_t *name,
    int32_t *type,
    int32_t *etik,
    int32_t *grtyp,
    char *cname,
    char *ctype,
    char *cetik,
    char *cgrtyp,
    int32_t *holocar,
    F2Cl l1,
    F2Cl l2,
    F2Cl l3,
    F2Cl l4
) {
    return (int32_t) f77name(fstcvt2)(name, type, etik, grtyp, cname, ctype, cetik, cgrtyp, holocar, l1, l2, l3, l4);
}


/*****************************************************************************
 *                      F S T _ D A T A _ L E N G T H                        *
 *                                                                           *
 *Object                                                                     *
 *   Gives information on data lenght of the elements passed to fstecr       *
 *   and fstlir (double, short integer, byte ...)                            *
 *                                                                           *
 *Arguments                                                                  *
 *                                                                           *
 *  IN  length_type     data length kind                                     *
 *                      1: byte                                              *
 *                      2: short (16 bits)                                   *
 *                      4: regular 32 bits                                   *
 *                      8: double (64 bits)                                  *
 *                                                                           *
 *****************************************************************************/
int32_t f77name(fst_data_length)(int *f_length_type)
{
    return (int32_t) c_fst_data_length(*f_length_type);
}

//! Write record to file
int32_t f77name(fstecr)(
    //! [in] Field to write to the file
    void *field,
    //! [in] Unused, but kept for backward compatibility
    int32_t *work,
    //! [in] Number of bits kept for the elements of the field
    int32_t *f_npak,
    //! [in] Unit number of the file in which to write
    int32_t *f_iun,
    //! [in] Date time stamp
    int32_t *f_date,
    //! [in] Duration of timestemps in seconds
    int32_t *f_deet,
    //! [in] Time step number
    int32_t *f_npas,
    //! [in] First dimension of the data field
    int32_t *f_ni,
    //! [in] Second dimension of the data field
    int32_t *f_nj,
    //! [in] Third dimension of the data field
    int32_t *f_nk,
    //! [in] Vertical level
    int32_t *f_ip1,
    //! [in] Forecast hour
    int32_t *f_ip2,
    //! [in] User defined identifier
    int32_t *f_ip3,
    //! [in] type of field (forecast, analysis, climatology)
    char *f_typvar,
    //! [in] Variable name
    char *f_nomvar,
    //! [in] Label
    char *f_etiket,
    //! [in] Grid type
    char *f_grtyp,
    //! [in] First grid descriptor
    int32_t *f_ig1,
    //! [in] Second grid descriptor
    int32_t *f_ig2,
    //! [in] Third grid descriptor
    int32_t *f_ig3,
    //! [in] Fourth grid descriptor
    int32_t *f_ig4,
    //! [in] Data type of the elements
    int32_t *f_datyp,
    //! [in] Rewrite existing record if true, append otherwise
    int32_t *f_rewrit,
    //! [in] Length of f_typvar
    F2Cl ll1,
    //! [in] Length of f_nomvar
    F2Cl ll2,
    //! [in] Length of f_etiket
    F2Cl ll3,
    //! [in] Length of f_grtyp
    F2Cl ll4
) {
    int iun = *f_iun, npak = *f_npak, date = *f_date, deet = *f_deet;
    int npas = *f_npas, ip1 = *f_ip1, ip2 = *f_ip2, ip3 = *f_ip3;
    int ni = *f_ni, nj = *f_nj, nk = *f_nk;
    int ig1 = *f_ig1, ig2 = *f_ig2, ig3 = *f_ig3, ig4 = *f_ig4;
    int datyp = *f_datyp, rewrit = *f_rewrit;
    int l1 = ll1, l2 = ll2, l3 = ll3, l4 = ll4;

    char etiket[13];
    char typvar[3];
    char nomvar[5];
    char grtyp[2];

    str_cp_init(typvar, 3, f_typvar, l1);
    str_cp_init(nomvar, 5, f_nomvar, l2);
    str_cp_init(etiket, 13, f_etiket, l3);
    str_cp_init(grtyp, 2, f_grtyp, l4);

    return (int32_t) c_fstecr(field, work, npak, iun, date, deet, npas,
                 ni, nj, nk, ip1, ip2, ip3,
                 typvar, nomvar, etiket, grtyp, ig1, ig2, ig3, ig4,
                 datyp, rewrit);
}


/*
 *Arguments                                                                  *
 *                                                                           *
 *  IN  string  character string to write to the file                        *
 *  IN  work    work field (kept for backward compatibility)                 *
 *  IN  npak    number of bits kept for the elements of the field (-npak)    *
 *  IN  iun     unit number associated to the file                           *
 *  IN  date    date time stamp                                              *
 *  IN  deet    length of a time step in seconds                             *
 *  IN  npas    time step number                                             *
 *  IN  ni      first dimension of the data field                            *
 *  IN  nj      second dimension of the data field                           *
 *  IN  nk      third dimension of the data field                            *
 *  IN  ip1     vertical level                                               *
 *  IN  ip2     forecast hour                                                *
 *  IN  ip3     user defined identifier                                      *
 *  IN  typvar  type of field (forecast, analysis, climatology)              *
 *  IN  nomvar  variable name                                                *
 *  IN  etiket  label                                                        *
 *  IN  grtyp   type of geographical projection                              *
 *  IN  ig1     first grid descriptor                                        *
 *  IN  ig2     second grid descriptor                                       *
 *  IN  ig3     third grid descriptor                                        *
 *  IN  ig4     fourth grid descriptor                                       *
 *  IN  datyp   data type of the elements                                    *
 *  IN  rewrit  rewrite flag (true = rewrite existing record, false = append)*
 *                                                                           *
 *****************************************************************************/
//! Write string to file
int32_t f77name(fstecr_s)(void *string, int32_t *work, int32_t *f_npak,
                        int32_t *f_iun, int32_t *f_date,
                        int32_t *f_deet, int32_t *f_npas,
                        int32_t *f_ni, int32_t *f_nj, int32_t *f_nk,
                        int32_t *f_ip1, int32_t *f_ip2, int32_t *f_ip3,
                        char *f_typvar, char *f_nomvar, char *f_etiket,
                        char *f_grtyp, int32_t *f_ig1, int32_t *f_ig2,
                        int32_t *f_ig3, int32_t *f_ig4,
                        int32_t *f_datyp, int32_t *f_rewrit,
                        int lng_string, F2Cl ll1, F2Cl ll2, F2Cl ll3, F2Cl ll4)
{
    int ninjnk = Max(1, *f_ni) * Max(1, *f_nj) * Max(1, *f_nk);
    if (ninjnk > lng_string * *f_nj) {
        Lib_Log(APP_LIBFST, APP_ERROR, "%s: ni * nj * nk (%d) > string length (%d)\n", __func__, ninjnk, lng_string);
        return ERR_BAD_DIM;
    } else {
        return f77name(fstecr)(string, work, f_npak, f_iun, f_date, f_deet, f_npas, f_ni, f_nj, f_nk, f_ip1, f_ip2, f_ip3,
                            f_typvar, f_nomvar, f_etiket, f_grtyp, f_ig1, f_ig2, f_ig3, f_ig4, f_datyp, f_rewrit,
                            ll1, ll2, ll3, ll4);
    }
}


/*****************************************************************************
 *                              F S T E C R _ H                              *
 *                                                                           *
 *Object                                                                     *
 *   Writes record to file.                                                  *
 *                                                                           *
 *Arguments                                                                  *
 *                                                                           *
 *  IN  haft_w  haft word (16 bit) array to write to the file                *
 *  IN  work    work field (kept for backward compatibility)                 *
 *  IN  npak    number of bits kept for the elements of the field (-npak)    *
 *  IN  iun     unit number associated to the file                           *
 *  IN  date    date time stamp                                              *
 *  IN  deet    length of a time step in seconds                             *
 *  IN  npas    time step number                                             *
 *  IN  ni      first dimension of the data field                            *
 *  IN  nj      second dimension of the data field                           *
 *  IN  nk      third dimension of the data field                            *
 *  IN  ip1     vertical level                                               *
 *  IN  ip2     forecast hour                                                *
 *  IN  ip3     user defined identifier                                      *
 *  IN  typvar  type of field (forecast, analysis, climatology)              *
 *  IN  nomvar  variable name                                                *
 *  IN  etiket  label                                                        *
 *  IN  grtyp   type of geographical projection                              *
 *  IN  ig1     first grid descriptor                                        *
 *  IN  ig2     second grid descriptor                                       *
 *  IN  ig3     third grid descriptor                                        *
 *  IN  ig4     fourth grid descriptor                                       *
 *  IN  datyp   data type of the elements                                    *
 *  IN  rewrit  rewrite flag (true = rewrite existing record, false = append)*
 *                                                                           *
 *****************************************************************************/
int32_t f77name(fstecr_h)(
    void *haft_w, int32_t *work, int32_t *f_npak,
    int32_t *f_iun, int32_t *f_date,
    int32_t *f_deet, int32_t *f_npas,
    int32_t *f_ni, int32_t *f_nj, int32_t *f_nk,
    int32_t *f_ip1, int32_t *f_ip2, int32_t *f_ip3,
    char *f_typvar, char *f_nomvar, char *f_etiket,
    char *f_grtyp, int32_t *f_ig1, int32_t *f_ig2,
    int32_t *f_ig3, int32_t *f_ig4,
    int32_t *f_datyp, int32_t *f_rewrit,
    F2Cl ll1, F2Cl ll2, F2Cl ll3, F2Cl ll4
) {
    xdf_short = 1;
    int32_t ier = f77name(fstecr)(
        haft_w, work, f_npak, f_iun, f_date, f_deet, f_npas, f_ni, f_nj, f_nk, f_ip1, f_ip2, f_ip3,
        f_typvar, f_nomvar, f_etiket, f_grtyp, f_ig1, f_ig2, f_ig3, f_ig4, f_datyp, f_rewrit,
        ll1, ll2, ll3, ll4);
    xdf_short = 0;
    return ier;
}


/*****************************************************************************
 *                              F S T E C R _ B                              *
 *                                                                           *
 *Object                                                                     *
 *   Writes record to file.                                                  *
 *                                                                           *
 *Arguments                                                                  *
 *                                                                           *
 *  IN  bytes   byte array to write to the file                              *
 *  IN  work    work field (kept for backward compatibility)                 *
 *  IN  npak    number of bits kept for the elements of the field (-npak)    *
 *  IN  iun     unit number associated to the file                           *
 *  IN  date    date time stamp                                              *
 *  IN  deet    length of a time step in seconds                             *
 *  IN  npas    time step number                                             *
 *  IN  ni      first dimension of the data field                            *
 *  IN  nj      second dimension of the data field                           *
 *  IN  nk      third dimension of the data field                            *
 *  IN  ip1     vertical level                                               *
 *  IN  ip2     forecast hour                                                *
 *  IN  ip3     user defined identifier                                      *
 *  IN  typvar  type of field (forecast, analysis, climatology)              *
 *  IN  nomvar  variable name                                                *
 *  IN  etiket  label                                                        *
 *  IN  grtyp   type of geographical projection                              *
 *  IN  ig1     first grid descriptor                                        *
 *  IN  ig2     second grid descriptor                                       *
 *  IN  ig3     third grid descriptor                                        *
 *  IN  ig4     fourth grid descriptor                                       *
 *  IN  datyp   data type of the elements                                    *
 *  IN  rewrit  rewrite flag (true = rewrite existing record, false = append)*
 *                                                                           *
 *****************************************************************************/
int32_t f77name(fstecr_b)(
    void *bytes, int32_t *work, int32_t *f_npak,
    int32_t *f_iun, int32_t *f_date,
    int32_t *f_deet, int32_t *f_npas,
    int32_t *f_ni, int32_t *f_nj, int32_t *f_nk,
    int32_t *f_ip1, int32_t *f_ip2, int32_t *f_ip3,
    char *f_typvar, char *f_nomvar, char *f_etiket,
    char *f_grtyp, int32_t *f_ig1, int32_t *f_ig2,
    int32_t *f_ig3, int32_t *f_ig4,
    int32_t *f_datyp, int32_t *f_rewrit,
    F2Cl ll1, F2Cl ll2, F2Cl ll3, F2Cl ll4
) {

    int32_t ier = 0;
    xdf_byte = 1;
    ier = f77name(fstecr)(
        bytes, work, f_npak, f_iun, f_date, f_deet, f_npas, f_ni, f_nj, f_nk, f_ip1, f_ip2, f_ip3,
        f_typvar, f_nomvar, f_etiket, f_grtyp, f_ig1, f_ig2, f_ig3, f_ig4, f_datyp, f_rewrit,
        ll1, ll2, ll3, ll4);
    xdf_byte = 0;
    return ier;
}


//! Edits the directory content of a RPN standard file
int32_t f77_name(fst_edit_dir_plus)(
    //! Handle of the directory entry to edit
    int32_t *f_handle,
    int32_t *f_date, int32_t *f_deet, int32_t *f_npas,
    int32_t *f_ni, int32_t *f_nj, int32_t *f_nk,
    int32_t *f_ip1, int32_t *f_ip2, int32_t *f_ip3,
    char *f_typvar, char *f_nomvar, char *f_etiket,
    char *f_grtyp, int32_t *f_ig1, int32_t *f_ig2,
    int32_t *f_ig3, int32_t *f_ig4, int32_t *f_datyp,
    F2Cl l1, F2Cl l2, F2Cl l3, F2Cl l4
) {
  int ier;
  int handle = *f_handle;
  int date = *f_date, deet = *f_deet;
  int npas = *f_npas, ip1 = *f_ip1, ip2 = *f_ip2, ip3 = *f_ip3;
  int ni = *f_ni, nj = *f_nj, nk = *f_nk;
  int ig1 = *f_ig1, ig2 = *f_ig2, ig3 = *f_ig3, ig4 = *f_ig4;
  int datyp = *f_datyp;

  char etiket[13];
  char typvar[3];
  char nomvar[5];
  char grtyp[2];

  str_cp_init(typvar, 3, f_typvar, l1);
  str_cp_init(nomvar, 5, f_nomvar, l2);
  str_cp_init(etiket, 13, f_etiket, l3);
  str_cp_init(grtyp, 2, f_grtyp, l4);

  ier = c_fst_edit_dir_plus(handle, date, deet, npas, ni, nj, nk, ip1, ip2, ip3, typvar, nomvar, etiket, grtyp,
                       ig1, ig2, ig3, ig4, datyp);
  return (int32_t) ier;
}


int32_t f77_name(fst_edit_dir)(
    int32_t *f_handle,
    int32_t *f_date, int32_t *f_deet, int32_t *f_npas,
    int32_t *f_ni, int32_t *f_nj, int32_t *f_nk,
    int32_t *f_ip1, int32_t *f_ip2, int32_t *f_ip3,
    char *f_typvar, char *f_nomvar, char *f_etiket,
    char *f_grtyp, int32_t *f_ig1, int32_t *f_ig2,
    int32_t *f_ig3, int32_t *f_ig4, int32_t *f_datyp,
    F2Cl l1, F2Cl l2, F2Cl l3, F2Cl l4
) {
  int ier;
  int handle = *f_handle;
  int date = *f_date, deet = *f_deet;
  int npas = *f_npas, ip1 = *f_ip1, ip2 = *f_ip2, ip3 = *f_ip3;
  int ni = *f_ni, nj = *f_nj, nk = *f_nk;
  int ig1 = *f_ig1, ig2 = *f_ig2, ig3 = *f_ig3, ig4 = *f_ig4;
  int datyp = *f_datyp;

  char etiket[13];
  char typvar[3];
  char nomvar[5];
  char grtyp[2];

  str_cp_init(typvar, 3, f_typvar, l1);
  str_cp_init(nomvar, 5, f_nomvar, l2);
  str_cp_init(etiket, 13, f_etiket, l3);
  str_cp_init(grtyp, 2, f_grtyp, l4);

  ier = c_fst_edit_dir(handle, date, deet, npas, ni, nj, nk, ip1, ip2, ip3, typvar, nomvar, etiket, grtyp,
                       ig1, ig2, ig3, ig4, datyp);
  return (int32_t) ier;
}


/*****************************************************************************
 *                             F S T E F F                                   *
 *                                                                           *
 *Object                                                                     *
 *   Deletes the record associated to handle.                                *
 *                                                                           *
 *Arguments                                                                  *
 *                                                                           *
 *  IN  handle  handle to the record to delete                               *
 *                                                                           *
 *****************************************************************************/
int32_t f77name(fsteff)(int32_t *f_handle)
{
  int ier, handle = *f_handle;

  ier = c_fsteff(handle);
  return (int32_t) ier;
}


/*****************************************************************************
 *                             F S T E O F                                   *
 *                                                                           *
 *Object                                                                     *
 *   Return the level of end of file for the sequential file.                *
 *                                                                           *
 *Arguments                                                                  *
 *                                                                           *
 *  IN  iun     unit number associated to the file                           *
 *                                                                           *
 *****************************************************************************/
int32_t f77name(fsteof)(int32_t *f_iun)
{
  int eof, iun = *f_iun;

  eof = c_fsteof(iun);
  return (int32_t) eof;
}


/*****************************************************************************
 *                              F S T F R M                                  *
 *                                                                           *
 *Object                                                                     *
 *   Closes a RPN standard file.                                             *
 *                                                                           *
 *Arguments                                                                  *
 *                                                                           *
 *  IN  iun     unit number associated to the file                           *
 *                                                                           *
 *****************************************************************************/
int32_t f77name(fstfrm)(int32_t *f_iun)
{
  int iun = *f_iun;
  return (int32_t) c_fstfrm(iun);
}


//! Locate the next record that matches the research keys
int32_t f77name(fstinf)(
    //! [in] Unit number associated to the file
    int32_t *f_iun,
    //! [out] Dimension 1 of the data field
    int32_t *f_ni,
    //! [out] Dimension 2 of the data field
    int32_t *f_nj,
    //! [out] Dimension 3 of the data field
    int32_t *f_nk,
    //! [in] Validity date
    int32_t *f_datev,
    //! [in] Label
    char *f_etiket,
    //! [in] Vertical level
    int32_t *f_ip1,
    //! [in] Forecast hour
    int32_t *f_ip2,
    //! [in] User defined identifier
    int32_t *f_ip3,
    //! [in] Type of field
    char *f_typvar,
    //! [in] Variable name
    char *f_nomvar,
    //! [in] Fortran hidden string length for 1st string
    F2Cl ll1,
    //! [in] Fortran hidden string length for 1st string
    F2Cl ll2,
    //! [in] Fortran hidden string length for 1st string
    F2Cl ll3
) {
    int32_t iun = *f_iun;
    int32_t datev = *f_datev;
    int32_t ip1 = *f_ip1;
    int32_t ip2 = *f_ip2;
    int32_t ip3 = *f_ip3;
    int l1 = ll1, l2 = ll2, l3 = ll3;

    char etiket[13];
    char typvar[3];
    char nomvar[5];

    str_cp_init(etiket, 13, f_etiket, l1);
    str_cp_init(typvar, 3, f_typvar, l2);
    str_cp_init(nomvar, 5, f_nomvar, l3);

    return (int32_t) c_fstinf(iun, f_ni, f_nj, f_nk, datev, etiket, ip1, ip2, ip3, typvar, nomvar);
}


/*****************************************************************************
 *                              F S T I N F X                                *
 *                                                                           *
 *Object                                                                     *
 *   Locate the next record that matches the research keys.                  *
 *   The search begins at the position given by handle.                      *
 *                                                                           *
 *Arguments                                                                  *
 *                                                                           *
 *  IN  iun     unit number associated to the file                           *
 *  OUT ni      dimension 1 of the data field                                *
 *  OUT nj      dimension 2 of the data field                                *
 *  OUT nk      dimension 3 of the data field                                *
 *  IN  datev   valid date                                                   *
 *  IN  etiket  label                                                        *
 *  IN  ip1     vertical level                                               *
 *  IN  ip2     forecast hour                                                *
 *  IN  ip3     user defined identifier                                      *
 *  IN  typvar  type of field                                                *
 *  IN  nomvar  variable name                                                *
 *                                                                           *
 *****************************************************************************/
int32_t f77name(fstinfx)(int32_t *f_handle, int32_t *f_iun,
                         int32_t *f_ni, int32_t *f_nj,
                         int32_t *f_nk, int32_t *f_datev, char *f_etiket,
                         int32_t *f_ip1, int32_t *f_ip2, int32_t *f_ip3,
                         char *f_typvar, char *f_nomvar,
                         F2Cl ll1, F2Cl ll2, F2Cl ll3)
{
  int iun = *f_iun, datev = *f_datev, ip1 = *f_ip1, ip2 = *f_ip2, ip3 = *f_ip3;
  int handle = *f_handle;
  int ier, ni, nj, nk;
  int l1 = ll1, l2 = ll2, l3 = ll3;

  char etiket[13];
  char typvar[3];
  char nomvar[5];

  str_cp_init(etiket, 13, f_etiket, l1);
  str_cp_init(typvar, 3, f_typvar, l2);
  str_cp_init(nomvar, 5, f_nomvar, l3);

  ier = c_fstinfx(handle, iun, &ni, &nj, &nk, datev, etiket,
                     ip1, ip2, ip3, typvar, nomvar);
  *f_ni = (int32_t) ni;
  *f_nj = (int32_t) nj;
  *f_nk = (int32_t) nk;
  return (int32_t) ier;
}


/*****************************************************************************
 *                              F S T I N L                                  *
 *                                                                           *
 *Object                                                                     *
 *   Locates all the records that matches the research keys.                 *
 *                                                                           *
 *Arguments                                                                  *
 *                                                                           *
 *  IN  iun     unit number associated to the file                           *
 *  OUT ni      dimension 1 of the data field                                *
 *  OUT nj      dimension 2 of the data field                                *
 *  OUT nk      dimension 3 of the data field                                *
 *  IN  datev   valid date                                                   *
 *  IN  etiket  label                                                        *
 *  IN  ip1     vertical level                                               *
 *  IN  ip2     forecast hour                                                *
 *  IN  ip3     user defined identifier                                      *
 *  IN  typvar  type of field                                                *
 *  IN  nomvar  variable name                                                *
 *  OUT liste   list of handle to the records                                *
 *  OUT infon   number of elements for the list (number of records found)    *
 *  OUT nmax    dimension of list as given by caller                         *
 *                                                                           *
 *****************************************************************************/
int32_t f77name(fstinl)(int32_t *f_iun, int32_t *f_ni, int32_t *f_nj,
                        int32_t *f_nk, int32_t *f_datev, char *f_etiket,
                        int32_t *f_ip1, int32_t *f_ip2, int32_t *f_ip3,
                        char *f_typvar, char *f_nomvar,
                        int32_t *liste, int32_t *f_infon, int32_t *f_nmax,
                        F2Cl ll1, F2Cl ll2, F2Cl ll3)
{
    char etiket[FST_ETIKET_LEN];
    char typvar[FST_TYPVAR_LEN];
    char nomvar[FST_NOMVAR_LEN];

    str_cp_init(etiket, FST_ETIKET_LEN, f_etiket, ll1);
    str_cp_init(typvar, FST_TYPVAR_LEN, f_typvar, ll2);
    str_cp_init(nomvar, FST_NOMVAR_LEN, f_nomvar, ll3);

    return (int32_t) c_fstinl(*f_iun, f_ni, f_nj, f_nk, *f_datev, etiket, *f_ip1, *f_ip2, *f_ip3, typvar, nomvar,
                              liste, f_infon, *f_nmax);
}


//! \copydoc c_fstlic
int32_t f77name(fstlic)(uint32_t *field, int32_t *f_iun,
                         int32_t *f_ni, int32_t *f_nj,
                         int32_t *f_nk, int32_t *f_date, char *f_etiket,
                         int32_t *f_ip1, int32_t *f_ip2, int32_t *f_ip3,
                         char *f_typvar, char *f_nomvar,
                         int32_t *f_ig1, int32_t *f_ig2, int32_t *f_ig3,
                         int32_t *f_ig4, char *f_grtyp,
                         F2Cl ll1, F2Cl ll2, F2Cl ll3, F2Cl ll4)
{
    int iun = *f_iun, ni = *f_ni, nj = *f_nj, nk = *f_nk;
    int date = *f_date, ip1 = *f_ip1, ip2 = *f_ip2, ip3 = *f_ip3;
    int ig1 = *f_ig1, ig2 = *f_ig2, ig3 = *f_ig3, ig4 = *f_ig4;
    int ier;
    int l1 = ll1, l2 = ll2, l3 = ll3, l4 = ll4;

    char etiket[13];
    char typvar[3];
    char nomvar[5];
    char grtyp[2];

    str_cp_init(etiket, 13, f_etiket, l1);
    str_cp_init(typvar, 3, f_typvar, l2);
    str_cp_init(nomvar, 5, f_nomvar, l3);
    str_cp_init(grtyp, 2, f_grtyp, l4);

    ier = c_fstlic(field, iun, ni, nj, nk, date, etiket, ip1, ip2, ip3,
                        typvar, nomvar, ig1, ig2, ig3, ig4, grtyp);
    return (int32_t) ier;
}


/*****************************************************************************
 *                              F S T L I R                                  *
 *                                                                           *
 *Object                                                                     *
 *   Reads the next record that matches the research keys.                   *
 *                                                                           *
 *Arguments                                                                  *
 *                                                                           *
 *  IN  iun     unit number associated to the file                           *
 *  OUT field   data field to be read                                        *
 *  OUT ni      dimension 1 of the data field                                *
 *  OUT nj      dimension 2 of the data field                                *
 *  OUT nk      dimension 3 of the data field                                *
 *  IN  datev   valid date                                                   *
 *  IN  etiket  label                                                        *
 *  IN  ip1     vertical level                                               *
 *  IN  ip2     forecast hour                                                *
 *  IN  ip3     user defined identifier                                      *
 *  IN  typvar  type of field                                                *
 *  IN  nomvar  variable name                                                *
 *                                                                           *
 *****************************************************************************/
int32_t f77name(fstlir)(int32_t *field, int32_t *f_iun,
                        int32_t *f_ni, int32_t *f_nj,
                        int32_t *f_nk, int32_t *f_datev, char *f_etiket,
                        int32_t *f_ip1, int32_t *f_ip2, int32_t *f_ip3,
                        char *f_typvar, char *f_nomvar,
                        F2Cl ll1, F2Cl ll2, F2Cl ll3)
{
  int iun = *f_iun, datev = *f_datev, ip1 = *f_ip1, ip2 = *f_ip2, ip3 = *f_ip3;
  int ier, ni, nj, nk;
  int l1 = ll1, l2 = ll2, l3 = ll3;

  char etiket[13];
  char typvar[3];
  char nomvar[5];

  str_cp_init(etiket, 13, f_etiket, l1);
  str_cp_init(typvar, 3, f_typvar, l2);
  str_cp_init(nomvar, 5, f_nomvar, l3);

  ier = c_fstlir(field, iun, &ni, &nj, &nk, datev, etiket,
                     ip1, ip2, ip3, typvar, nomvar);

  if (ier < 0) return (int32_t) ier;
  *f_ni = (int32_t) ni;
  *f_nj = (int32_t) nj;
  *f_nk = (int32_t) nk;
  return (int32_t) ier;
}


/*****************************************************************************
 *                              F S T L I R _ S                              *
 *                                                                           *
 *Object                                                                     *
 *   Reads the next record that matches the research keys.                   *
 *                                                                           *
 *Arguments                                                                  *
 *                                                                           *
 *  IN  iun     unit number associated to the file                           *
 *  OUT string  character string to be read                                  *
 *  OUT ni      dimension 1 of the data field                                *
 *  OUT nj      dimension 2 of the data field                                *
 *  OUT nk      dimension 3 of the data field                                *
 *  IN  datev   valid date                                                   *
 *  IN  etiket  label                                                        *
 *  IN  ip1     vertical level                                               *
 *  IN  ip2     forecast hour                                                *
 *  IN  ip3     user defined identifier                                      *
 *  IN  typvar  type of field                                                *
 *  IN  nomvar  variable name                                                *
 *                                                                           *
 *****************************************************************************/
int32_t f77name(fstlir_s)(void *string, int32_t *f_iun,
                        int32_t *f_ni, int32_t *f_nj,
                        int32_t *f_nk, int32_t *f_datev, char *f_etiket,
                        int32_t *f_ip1, int32_t *f_ip2, int32_t *f_ip3,
                        char *f_typvar, char *f_nomvar,
                        int lng_string, F2Cl ll1, F2Cl ll2, F2Cl ll3)
{
  int32_t ier;
  int i;
  char *ptr = (char*) string;

  for (i = 0; i < lng_string; i++)
    *ptr++ = ' ';
  ier = f77name(fstlir)(string, f_iun, f_ni, f_nj, f_nk, f_datev, f_etiket,
                     f_ip1, f_ip2, f_ip3, f_typvar, f_nomvar, ll1, ll2, ll3);
  return ier;
}


/*****************************************************************************
 *                              F S T L I R _ H                              *
 *                                                                           *
 *Object                                                                     *
 *   Reads the next record that matches the research keys.                   *
 *                                                                           *
 *Arguments                                                                  *
 *                                                                           *
 *  IN  iun     unit number associated to the file                           *
 *  OUT haft_w  haft word short integer (16 bit) array to be read            *
 *  OUT ni      dimension 1 of the data field                                *
 *  OUT nj      dimension 2 of the data field                                *
 *  OUT nk      dimension 3 of the data field                                *
 *  IN  datev   valid date                                                   *
 *  IN  etiket  label                                                        *
 *  IN  ip1     vertical level                                               *
 *  IN  ip2     forecast hour                                                *
 *  IN  ip3     user defined identifier                                      *
 *  IN  typvar  type of field                                                *
 *  IN  nomvar  variable name                                                *
 *                                                                           *
 *****************************************************************************/
int32_t f77name(fstlir_h)(void *haft_w, int32_t *f_iun,
                        int32_t *f_ni, int32_t *f_nj,
                        int32_t *f_nk, int32_t *f_datev, char *f_etiket,
                        int32_t *f_ip1, int32_t *f_ip2, int32_t *f_ip3,
                        char *f_typvar, char *f_nomvar,
                        F2Cl ll1, F2Cl ll2, F2Cl ll3)
{
  int32_t ier;

  xdf_short = 1;
  ier = f77name(fstlir)(haft_w, f_iun, f_ni, f_nj, f_nk, f_datev, f_etiket,
                     f_ip1, f_ip2, f_ip3, f_typvar, f_nomvar, ll1, ll2, ll3);
  xdf_short = 0;
  return ier;
}


/*****************************************************************************
 *                              F S T L I R _ B                              *
 *                                                                           *
 *Object                                                                     *
 *   Reads the next record that matches the research keys.                   *
 *                                                                           *
 *Arguments                                                                  *
 *                                                                           *
 *  IN  iun     unit number associated to the file                           *
 *  OUT bytes   byte array to be read                                        *
 *  OUT ni      dimension 1 of the data field                                *
 *  OUT nj      dimension 2 of the data field                                *
 *  OUT nk      dimension 3 of the data field                                *
 *  IN  datev   valid date                                                   *
 *  IN  etiket  label                                                        *
 *  IN  ip1     vertical level                                               *
 *  IN  ip2     forecast hour                                                *
 *  IN  ip3     user defined identifier                                      *
 *  IN  typvar  type of field                                                *
 *  IN  nomvar  variable name                                                *
 *                                                                           *
 *****************************************************************************/
int32_t f77name(fstlir_b)(void *bytes, int32_t *f_iun,
                        int32_t *f_ni, int32_t *f_nj,
                        int32_t *f_nk, int32_t *f_datev, char *f_etiket,
                        int32_t *f_ip1, int32_t *f_ip2, int32_t *f_ip3,
                        char *f_typvar, char *f_nomvar,
                        F2Cl ll1, F2Cl ll2, F2Cl ll3)
{
  int32_t ier;

  xdf_byte = 1;
  ier = f77name(fstlir)(bytes, f_iun, f_ni, f_nj, f_nk, f_datev, f_etiket,
                     f_ip1, f_ip2, f_ip3, f_typvar, f_nomvar, ll1, ll2, ll3);
  xdf_byte = 0;
  return ier;
}


/*****************************************************************************
 *                              F S T L I R X                                *
 *                                                                           *
 *Object                                                                     *
 *   Reads the next record that matches the research keys.                   *
 *   The search begins at the position given by handle.                      *
 *                                                                           *
 *Arguments                                                                  *
 *                                                                           *
 *  IN  iun     unit number associated to the file                           *
 *  OUT field   data field to be read                                        *
 *  OUT ni      dimension 1 of the data field                                *
 *  OUT nj      dimension 2 of the data field                                *
 *  OUT nk      dimension 3 of the data field                                *
 *  IN  datev   valid date                                                   *
 *  IN  etiket  label                                                        *
 *  IN  ip1     vertical level                                               *
 *  IN  ip2     forecast hour                                                *
 *  IN  ip3     user defined identifier                                      *
 *  IN  typvar  type of field                                                *
 *  IN  nomvar  variable name                                                *
 *                                                                           *
 *****************************************************************************/
int32_t f77name(fstlirx)(uint32_t *field, int32_t *f_handle, int32_t *f_iun,
                        int32_t *f_ni, int32_t *f_nj,
                        int32_t *f_nk, int32_t *f_datev, char *f_etiket,
                        int32_t *f_ip1, int32_t *f_ip2, int32_t *f_ip3,
                        char *f_typvar, char *f_nomvar,
                        F2Cl ll1, F2Cl ll2, F2Cl ll3)
{
  int iun = *f_iun, datev = *f_datev, ip1 = *f_ip1, ip2 = *f_ip2, ip3 = *f_ip3;
  int handle = *f_handle;
  int ier, ni, nj, nk;
  int l1 = ll1, l2 = ll2, l3 = ll3;

  char etiket[13];
  char typvar[3];
  char nomvar[5];

  str_cp_init(etiket, 13, f_etiket, l1);
  str_cp_init(typvar, 3, f_typvar, l2);
  str_cp_init(nomvar, 5, f_nomvar, l3);

  ier = c_fstlirx(field, handle, iun, &ni, &nj, &nk, datev, etiket,
                     ip1, ip2, ip3, typvar, nomvar);

  *f_ni = (int32_t) ni;
  *f_nj = (int32_t) nj;
  *f_nk = (int32_t) nk;
  return (int32_t) ier;
}


/*****************************************************************************
 *                            F S T L I S                                    *
 *                                                                           *
 *Object                                                                     *
 *   Reads the next record that matches the last search criterias            *
 *                                                                           *
 *Arguments                                                                  *
 *                                                                           *
 *  OUT field   data field to be read                                        *
 *  IN  iun     unit number associated to the file                           *
 *  OUT ni      dimension 1 of the data field                                *
 *  OUT nj      dimension 2 of the data field                                *
 *  OUT nk      dimension 3 of the data field                                *
 *                                                                           *
 *****************************************************************************/
int32_t f77name(fstlis)(uint32_t *field, int32_t *f_iun,
                        int32_t *f_ni, int32_t *f_nj, int32_t *f_nk)
{
  int iun = *f_iun;
  int ier, ni, nj, nk;

  ier = c_fstlis((uint32_t *)field, iun, &ni, &nj, &nk);

  *f_ni = (int32_t) ni;
  *f_nj = (int32_t) nj;
  *f_nk = (int32_t) nk;
  return (int32_t) ier;
}

//! Link files together for searches
int32_t c_fstlnk(
    //! [in] List of unit numbers associated to the files
    const int32_t * const liste,
    //! [in] Number of files to link
    const int32_t n
) {
    int previous_index = -1;
    first_linked_file = liste[0];
    for (int i = 0; i < n; i++) {
        const int index_fnom = get_fnom_index(liste[i]);
        if (index_fnom == -1) {
            Lib_Log(APP_LIBFST, APP_ERROR, "%s: file (unit=%d) is not connected with fnom\n", __func__, liste[i]);
            c_fstunl();
            return ERR_NO_FNOM;
        }

        if (previous_index >= 0) {
            if (fst98_open_files[previous_index].next_file >= 0) {
                Lib_Log(APP_LIBFST, APP_ERROR, "%s: file index %d is already linked with another one\n", __func__, previous_index);
                c_fstunl();
                return ERR_BAD_LINK;
            }
            Lib_Log(APP_LIBFST, APP_INFO, "%s: linking file %d to %d\n", __func__, index_fnom, previous_index);
            fst98_open_files[previous_index].next_file = index_fnom;
        }

        previous_index = index_fnom;
    }

    return 0;
}


//! Link files together for search purpose
//! \return 0 on success, error code otherwise
int32_t f77name(fstlnk)(
    //! [in] List of unit numbers associated to the files
    const int32_t * const liste,
    //! [in] Number of files to link
    const int32_t * const f_n
) {
    return c_fstlnk(liste, *f_n);
}


/*****************************************************************************
 *                              F S T L U K                                  *
 *                                                                           *
 *Object                                                                     *
 *   Read the record at position given by handle.                            *
 *                                                                           *
 *Arguments                                                                  *
 *                                                                           *
 *  OUT field   data field to be read                                        *
 *  IN  handle  positioning information to the record                        *
 *  OUT ni      dimension 1 of the data field                                *
 *  OUT nj      dimension 2 of the data field                                *
 *  OUT nk      dimension 3 of the data field                                *
 *                                                                           *
 *****************************************************************************/

int32_t f77name(fstluk)(uint32_t *field, int32_t *f_handle,
                        int32_t *f_ni, int32_t *f_nj, int32_t *f_nk)
{
    int handle = *f_handle;
    int ier, ni, nj, nk;

    ier = c_fstluk(field, handle, &ni, &nj, &nk);

    *f_ni = (int32_t) ni;
    *f_nj = (int32_t) nj;
    *f_nk = (int32_t) nk;
    return (int32_t) ier;
}


/*****************************************************************************
 *                            F S T M S Q                                    *
 *                                                                           *
 *Object                                                                     *
 *   Mask a portion of the research keys.                                    *
 *                                                                           *
 *Arguments                                                                  *
 *                                                                           *
 *   IN    iun     unit number associated to the file                        *
 * IN/OUT  mip1    mask for vertical level                                   *
 * IN/OUT  mip2    mask for forecast hour                                    *
 * IN/OUT  mip3    mask for ip3 (user defined identifier)                    *
 * IN/OUT  metiket mask for label                                            *
 *   IN    getmode logical (1: getmode 0:set mode)                           *
 *                                                                           *
 *****************************************************************************/
int32_t f77name(fstmsq)(int32_t *f_iun, int32_t *f_mip1, int32_t *f_mip2,
                        int32_t *f_mip3, char *f_metiket, int32_t *f_getmode,
                        F2Cl ll1)
{
    int err, iun = *f_iun, mip1 = *f_mip1, mip2 = *f_mip2, mip3 = *f_mip3;
    int getmode = *f_getmode;
    int l1 = ll1;

    char metiket[13];

    str_cp_init(metiket, 13, f_metiket, l1);
    err = c_fstmsq(iun, &mip1, &mip2, &mip3, metiket, getmode);

    if (getmode) {
        *f_mip1 = (int32_t) mip1;
        *f_mip2 = (int32_t) mip2;
        *f_mip3 = (int32_t) mip3;

        const int num_chars = Min(12, l1);
        strncpy(f_metiket, metiket, num_chars);
    }
    return err;
}


/*****************************************************************************
 *                              F S T N B R                                  *
 *                                                                           *
 *Object                                                                     *
 *   Returns the number of records of the file associated with unit number.  *
 *                                                                           *
 *Arguments                                                                  *
 *                                                                           *
 *  IN  iun     unit number associated to the file                           *
 *                                                                           *
 *****************************************************************************/
int32_t f77name(fstnbr)(const int32_t * const f_iun)
{
    return (int32_t) c_fstnbr(*f_iun);
}


/*****************************************************************************
 *                              F S T N B R V                                *
 *                                                                           *
 *Object                                                                     *
 *   Returns the number of valid records (excluding deleted records) of the  *
 *   file associated with unit number.                                       *
 *                                                                           *
 *Arguments                                                                  *
 *                                                                           *
 *  IN  iun     unit number associated to the file                           *
 *                                                                           *
 *****************************************************************************/
int32_t f77name(fstnbrv)(const int32_t * const f_iun)
{
    return (int32_t) c_fstnbrv(*f_iun);
}


//! \copydoc c_fstopc()
int32_t f77name(fstopc)(
    const char * const option,
    const char * const value,
    const int32_t * const getmode,
    F2Cl ll1,
    F2Cl ll2
) {
    char option2[17];
    char value2[129];
    int l1 = ll1;
    int l2 = ll2;

    l1 = (l1 > 16) ? 16 : l1;
    l2 = (l2 > 128) ? 128 : l2;
    strncpy(option2, option, l1);
    option2[l1] = '\0';
    l1--;
    while ((l1 > 0) && (option2[l1] == ' ')) {
        option2[l1] = '\0';
        l1--;
    }
    strncpy(value2, value, l2);
    value2[l2] = '\0';
    l2--;
    while ((l2 > 0) && (value2[l2] == ' ')) {
        value2[l2] = '\0';
        l2--;
    }

    return (int32_t) c_fstopc(option2, value2, *getmode);
}


//! \copydoc c_fstopi()
int32_t f77name(fstopi)(
    const char * const option,
    const int32_t * const value,
    const int32_t * const getmode,
    F2Cl ll1
) {
    char option2[7] = {' ', ' ', ' ', ' ', ' ', ' ', '\0'};

    int l1 = ll1;
    l1 = (l1 > 6) ? 6 : l1;
    strncpy(option2, option, l1);

    return (int32_t) c_fstopi(option2, *value, *getmode);
}


//! \copydoc c_fstopl()
int32_t f77name(fstopl)(
    const char * const option,
    const int32_t * const value,
    const int32_t * const getmode,
    F2Cl ll1
) {
    char option2[17];

    int l1 = ll1;
    l1 = (l1 > 16) ? 16 : l1;
    strncpy(option2, option, l1);
    option2[l1] = '\0';

    return (int32_t) c_fstopl(option2, *value, *getmode);
}


//! \copydoc c_fstopr()
int32_t f77name(fstopr)(
    const char * const option,
    const float * const value,
    const int32_t * const getmode,
    F2Cl ll1
) {
    int l1 = ll1;
    char option2[7];

    l1 = (l1 > 6) ? 6 : l1;
    strncpy(option2, option, l1);
    option2[l1] = '\0';

    return (int32_t) c_fstopr(option2, *value, *getmode);
}


//! \copydoc c_fstcheck()
int32_t f77name(fstcheck)(
    const char * const filename,
    F2Cl lng
) {
    char filename_cstr[lng + 1];
    strncpy(filename_cstr, filename, lng);
    filename_cstr[lng] = '\0';
    return (int32_t) c_fstcheck(filename_cstr);
}


/*****************************************************************************
 *                              F S T O U V                                  *
 *                                                                           *
 *Object                                                                     *
 *   Opens a RPN standard file.                                              *
 *                                                                           *
 *Arguments                                                                  *
 *                                                                           *
 *  IN  iun     unit number associated to the file                           *
 *  IN  options random or sequential access                                  *
 *                                                                           *
 *****************************************************************************/
int32_t f77name(fstouv)(
    const int32_t * const f_iun,
    const char * const options,
    F2Cl lng
) {
    // char * const options_cstr = malloc((lng + 1) * sizeof(char));
    char options_cstr[lng + 1];
    strncpy(options_cstr, options, lng);
    options_cstr[lng] = '\0';
    return c_fstouv(*f_iun, options_cstr);
}


/*****************************************************************************
 *                              F S T P R M                                  *
 *                                                                           *
 *Object                                                                     *
 *   Get all the description informations of the record.                     *
 *                                                                           *
 *Arguments                                                                  *
 *                                                                           *
 *  IN  handle  positioning information to the record                        *
 *  OUT date    date time stamp                                              *
 *  OUT deet    length of a time step in seconds                             *
 *  OUT npas    time step number                                             *
 *  OUT ni      first dimension of the data field                            *
 *  OUT nj      second dimension of the data field                           *
 *  OUT nk      third dimension of the data field                            *
 *  OUT nbits   number of bits kept for the elements of the field            *
 *  OUT datyp   data type of the elements                                    *
 *  OUT ip1     vertical level                                               *
 *  OUT ip2     forecast hour                                                *
 *  OUT ip3     user defined identifier                                      *
 *  OUT typvar  type of field (forecast, analysis, climatology)              *
 *  OUT nomvar  variable name                                                *
 *  OUT etiket  label                                                        *
 *  OUT grtyp   type of geographical projection                              *
 *  OUT ig1     first grid descriptor                                        *
 *  OUT ig2     second grid descriptor                                       *
 *  OUT ig3     third grid descriptor                                        *
 *  OUT ig4     fourth grid descriptor                                       *
 *  OUT swa     starting word address                                        *
 *  OUT lng     record length                                                *
 *  OUT dltf    delete flag                                                  *
 *  OUT ubc     unused bit count                                             *
 *  OUT extra1  extra parameter                                              *
 *  OUT extra2  extra parameter                                              *
 *  OUT extra3  extra parameter                                              *
 *                                                                           *
 *****************************************************************************/
int32_t f77name(fstprm)(int32_t *f_handle,
                        int32_t *f_dateo, int32_t *f_deet, int32_t *f_npas,
                        int32_t *f_ni, int32_t *f_nj, int32_t *f_nk,
                        int32_t *f_nbits, int32_t *f_datyp, int32_t *f_ip1,
                        int32_t *f_ip2, int32_t *f_ip3, char *f_typvar,
                        char *f_nomvar, char *f_etiket, char *f_grtyp,
                        int32_t *f_ig1, int32_t *f_ig2, int32_t *f_ig3,
                        int32_t *f_ig4, int32_t *f_swa, int32_t *f_lng,
                        int32_t *f_dltf, int32_t *f_ubc, int32_t *f_extra1,
                        int32_t *f_extra2, int32_t *f_extra3,
                        F2Cl ll1, F2Cl ll2, F2Cl ll3, F2Cl ll4)
{
  int handle = *f_handle;
  int ni, nj, nk, nbits, datyp, ip1, ip2, ip3, dateo, deet, npas;
  int ig1, ig2, ig3, ig4, swa, lng, dltf, ubc, extra1, extra2, extra3, ier;
  int l1 = ll1, l2 = ll2, l3 = ll3, l4 = ll4;
  char etiket[13] = {' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' '
                   , '\0'};
  char typvar[3] = {' ', ' ', '\0'};
  char nomvar[5] = {' ', ' ', ' ', ' ', '\0'};
  char grtyp[2] = {' ', '\0'};

  l1 = (l1 < 2) ? l1 : 2;            // typvar length
  l2 = (l2 < 4) ? l2 : 4;            // nomvar length
  l3 = (l3 < 12) ? l3 :12;           // etiket length
  l4 = (l4 < 1) ? l4 : 1;            // grtyp length

  ier = c_fstprm(handle, &dateo, &deet, &npas, &ni, &nj, &nk,
                     &nbits, &datyp, &ip1, &ip2, &ip3, typvar,
                     nomvar, etiket, grtyp, &ig1, &ig2, &ig3, &ig4, &swa, &lng,
                     &dltf, &ubc, &extra1, &extra2, &extra3);
  *f_ni = (int32_t) ni;
  *f_nj = (int32_t) nj;
  *f_nk = (int32_t) nk;
  *f_dateo = (int32_t) dateo;
  *f_deet = (int32_t) deet;
  *f_npas = (int32_t) npas;
  *f_nbits = (int32_t) nbits;
  *f_datyp = (int32_t) datyp;
  *f_ip1 = (int32_t) ip1;
  *f_ip2 = (int32_t) ip2;
  *f_ip3 = (int32_t) ip3;
  *f_ig1 = (int32_t) ig1;
  *f_ig2 = (int32_t) ig2;
  *f_ig3 = (int32_t) ig3;
  *f_ig4 = (int32_t) ig4;
  *f_swa = (int32_t) swa;
  *f_lng = (int32_t) lng;
  *f_dltf = (int32_t) dltf;
  *f_ubc = (int32_t) ubc;
  *f_extra1 = (int32_t) extra1;
  *f_extra2 = (int32_t) extra2;
  *f_extra3 = (int32_t) extra3;
  strncpy(f_typvar, typvar, l1);
  strncpy(f_nomvar, nomvar, l2);
  strncpy(f_etiket, etiket, l3);
  strncpy(f_grtyp, grtyp, l4);
  return (int32_t) ier;
}


/*****************************************************************************
 *                   F S T R E S E T _ I P _ F L A G S                       *
 *                                                                           *
 *Object                                                                     *
 *   Reset all the flags previously set by ip(1-3)_val                       *
 *                                                                           *
 *****************************************************************************/
void f77name(fstreset_ip_flags)(void) {
    init_ip_vals();
}


int32_t f77name(fstrwd)(int32_t *f_iun) {
    return (int32_t) c_fstrwd(*f_iun);
}


int32_t f77name(fstskp)(int32_t *f_iun, int32_t *f_nrec) {
    return (int32_t) c_fstskp(*f_iun, *f_nrec);
}


/*****************************************************************************
 *                            F S T S U I                                    *
 *                                                                           *
 *Object                                                                     *
 *   Finds the next record that matches the last search criterias            *
 *                                                                           *
 *Arguments                                                                  *
 *                                                                           *
 *  IN  iun     unit number associated to the file                           *
 *  OUT ni      dimension 1 of the data field                                *
 *  OUT nj      dimension 2 of the data field                                *
 *  OUT nk      dimension 3 of the data field                                *
 *                                                                           *
 *****************************************************************************/
int32_t f77name(fstsui)(
    int32_t *f_iun,
    int32_t *f_ni,
    int32_t *f_nj,
    int32_t *f_nk
) {
    return (int32_t) c_fstsui(*f_iun, f_ni, f_nj, f_nk);
}


//! Unlinks files previously linked with fstlnk
int32_t c_fstunl(void) {
    if (first_linked_file < 0) {
        Lib_Log(APP_LIBFST, APP_ERROR, "%s: No list to unlink\n", __func__);
        return ERR_BAD_LINK;
    }

    const int start_index = get_fnom_index(first_linked_file);
    if (start_index < 0) {
        Lib_Log(APP_LIBFST, APP_ERROR, "%s: File to unlink (iun %d) is not connected with fnom\n", __func__, link_list[0]);
        return ERR_NO_FNOM;
    }

    int index = start_index;
    while (index >= 0) {
        const int next_index = fst98_open_files[index].next_file;
        fst98_open_files[index].next_file = -1;
        index = next_index;
    }

    return 0;
}


int32_t f77name(fstunl)(void) {
    return c_fstunl();
}


//! Get fst version number
int32_t f77name(fst_version)(void)
{
    return (int32_t) stdf_version;
}


/*****************************************************************************
 *                              F S T V O I                                  *
 *                                                                           *
 *Object                                                                     *
 *   Opens a RPN standard file.                                              *
 *                                                                           *
 *Arguments                                                                  *
 *                                                                           *
 *  IN  iun     unit number associated to the file                           *
 *  IN  options random or sequential access                                  *
 *                                                                           *
 *****************************************************************************/
int32_t f77name(fstvoi)(int32_t *f_iun, char *f_options, F2Cl ll1)
{
    char options[80] =
    {' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ',
    ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ',
    ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ',
    ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ',
    ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ',
    ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ',
    ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ',
    ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', '\0'};

    int l1 = ll1;
    l1 = (l1 > 79) ? 79 : l1;
    strncpy(options, f_options, l1);
    options[l1] = '\0';

    return (int32_t) c_fstvoi(*f_iun, options);
}


/*****************************************************************************
 *                             F S T W E O                                   *
 *                                                                           *
 *Object                                                                     *
 *   Writes a logical end of file on a sequential file.                      *
 *                                                                           *
 *Arguments                                                                  *
 *                                                                           *
 *  IN  iun     unit number associated to the file                           *
 *  IN  level   level of logical end of file                                 *
 *                                                                           *
 *****************************************************************************/
int32_t f77name(fstweo)(int32_t *f_iun, int32_t *f_level)
{
  int ier, iun = *f_iun, level = *f_level;

  ier = c_fstweo(iun, level);
  return (int32_t) ier;
}


//! Generate a all possible coded IP1 values
int32_t f77name(ip1_all)(
    //! [in] IP1 level (float value)
    float *f_level,
    // Level kind as defined in convip_plus
    int32_t *f_kind
)
{
  int kind = *f_kind, ip1;
  float level = *f_level;

  ip1 = c_ip1_all(level, kind);
  return (int32_t) ip1;
}

int32_t f77name(ip2_all)(float *f_level, int32_t *f_kind)
{
  int kind = *f_kind, ip2;
  float level = *f_level;

  ip2 = c_ip2_all(level, kind);
  return (int32_t) ip2;
}

int32_t f77name(ip3_all)(float *f_level, int32_t *f_kind)
{
  int kind = *f_kind, ip3;
  float level = *f_level;

  ip3 = c_ip3_all(level, kind);
  return (int32_t) ip3;
}

int32_t f77name(ip1_val)(float *f_level, int32_t *f_kind)
{
  int kind = *f_kind, ip1;
  float level = *f_level;

  ip1 = c_ip1_val(level, kind);
  return (int32_t) ip1;
}

int32_t f77name(ip2_val)(float *f_level, int32_t *f_kind)
{
  int kind = *f_kind, ip2;
  float level = *f_level;

  ip2 = c_ip2_val(level, kind);
  return (int32_t) ip2;
}

int32_t f77name(ip3_val)(float *f_level, int32_t *f_kind)
{
  int kind = *f_kind, ip3;
  float level = *f_level;

  ip3 = c_ip3_val(level, kind);
  return (int32_t) ip3;
}


//! Check if name is in the translation table
int FstCanTranslateName(const char *varname) {
    FILE *fileref;
    static char filename[256];
    int result;

    // --- START critical region ---
    pthread_mutex_lock(&fst98_mutex);
    if (! read_done) {
        // First call, get do not translate table
        read_done = 1;
        char * fst_noip_name = getenv("FST_NOIP_NAME");
        ARMNLIB = getenv("ARMNLIB");
        char * basename = ARMNLIB;
        if (fst_noip_name) {
            // Environment variable contains the table
            strncpy( exception_vars , fst_noip_name , sizeof(exception_vars) );
            basename = NULL;
            // fst_noip_name contains a file name
            if (exception_vars[0] == '|') basename = exception_vars + 1;
        }
        if (basename) {
            // Get table from $ARMNLIB/data/exception_vars file if it exists
            if (basename == ARMNLIB) {
                snprintf(filename, sizeof(filename), "%s/data/exception_regex_var", ARMNLIB);
            } else {
                snprintf(filename, sizeof(filename), "%s", basename);
            }
            if ((fileref = fopen(filename, "r")) != NULL) {
                if (NULL == fgets(exception_vars, sizeof(exception_vars), fileref) ) exception_vars[0] = '\0';
                Lib_Log(APP_LIBFST, APP_DEBUG, "%s: OPENING exception file: %s\n", __func__, filename);
                fclose(fileref);
            }
        }
        if (exception_vars[0] == '~') {
            int i;
            for (i = 0; exception_vars[i] != '\0' && exception_vars[i] != '\n'; i++);
            exception_vars[i] = '\0';
            // result = regcomp(&pattern, exception_vars + 1, REG_EXTENDED | REG_NOSUB);
            regcomp(&pattern, exception_vars + 1, REG_EXTENDED | REG_NOSUB);
            Lib_Log(APP_LIBFST, APP_DEBUG, "%s: exception pattern: '%s'\n", __func__, exception_vars + 1);
        }
    }
    pthread_mutex_unlock(&fst98_mutex);
    // --- END critical region ---

    if (exception_vars[0] == '~') {
        // This is a regex pattern
        // Name not in pattern, it can be translated
        result = regexec(&pattern, varname, (size_t) 0, NULL, 0) != 0;
    } else {
        // This is a straight list of 4 char tokens
        // Name not in list, it can be translated
        result = strstr(exception_vars, varname) == NULL;
    }
    return result;
}


//! Generate a string of the field's IP1, IP2, IP3
void c_ip_string(
    //! [out] Buffer into which to write
    char * const buffer,
    //! [in] Size of the buffer
    const int size,
    //! [in] Field's IP1
    const int ip1,
    //! [in] Field's IP2
    const int ip2,
    //! [in] Field's IP3
    const int ip3
) {
    float lip1, lip2, lip3;
    int kind1, kind2, kind3;
    int StatusIP = ConvertIPtoPK(&lip1, &kind1, &lip2, &kind2, &lip3, &kind3, ip1, ip2, ip3);
    if (kind1 < 0 || kind2 < 0 || kind3 < 0 || (StatusIP & CONVERT_ERROR) ) {
        // decode error somewhere
        // integer code P = IP
        kind1 = 15; kind2 = 15; kind3 = 15;
        lip1 = ip1; lip2 = ip2; lip3 = ip3;
    }
    // force modulo 32
    kind1 &= 0x1F; kind2 &= 0x1F; kind3 &= 0x1F;
    snprintf(buffer, size, "IP1 %g (%s), IP2 %g (%s), IP3 %g (%s)", lip1, kinds(kind1), lip2, kinds(kind2), lip3, kinds(kind3));
}


//! \warning Stub; not implemented yet
int32_t f77name(fstabt)()
{
    Lib_Log(APP_LIBFST, APP_FATAL, "%s: this routine is not implemented in FSTD98\n", __func__);
    return ERR_NOT_IMPL;
}

//! \warning Stub; not implemented yet
int32_t f77name(fstsel)()
{
    Lib_Log(APP_LIBFST, APP_WARNING, "%s: this routine is not implemented in FSTD98\n \t\t fstinfx or fstlirx must be used instead\n", __func__);
    return ERR_NOT_IMPL;
}

//! \warning Stub; not implemented yet
int32_t f77name(zfstcvt)()
{
    Lib_Log(APP_LIBFST, APP_FATAL, "%s: this routine is not implemented yet in FSTD98\n", __func__);
    return ERR_NOT_IMPL;
}

//! \warning Stub; not implemented yet
int32_t f77name(fstpos)()
{
    Lib_Log(APP_LIBFST, APP_WARNING, "%s: this routine is not implemented in FSTD98\n \t\t fstinfx or fstlirx must be used instead\n", __func__);
    return ERR_NOT_IMPL;
}
