#include <stdio.h>
#include <stddef.h>
#include <stdlib.h>
#include <math.h>
#include <float.h>
#include <unistd.h>

#include <rmn.h>

//#include <png.h>

#define DATYP 5

#ifndef DATATYPE
#error "DATATYPE must be defined to float or double when invoking the compiler"
#endif

// Voodoo magic macros
#define STR(str) #str
#define BUILD_FILENAME(type) "test_" STR(type) ".fst"
#define FILENAME BUILD_FILENAME(DATATYPE)
#define BUILD_DATATYPE_STR(type) STR(type)
#define DATATYPESTR BUILD_DATATYPE_STR(DATATYPE)


int main(int argc, char** argv) {
    const int domainSz = 1024;
    DATATYPE domain[domainSz][domainSz];

    for (int i = 0; i < domainSz; i++) {
        for (int j = 0; j < domainSz; j++) {
            DATATYPE powi = pow((1.0 * i / domainSz), 2);
            DATATYPE powj = pow((1.0 * j / domainSz), 2);
            DATATYPE numerator = 1 + cos(12 * sqrt( powi + powj ));
            DATATYPE denominator = 0.5 * ( powi + powj ) + 2;
            domain[i][j] = numerator / denominator;
            // printf("domain[%d][%d] = %f\n", i, j, domain[i][j]);
        }
    }
    DATATYPE min = domain[0][0];
    int mini = 0;
    int minj = 0;

    DATATYPE max = domain[0][0];
    int maxi = 0;
    int maxj = 0;

    for (int i = 0; i < domainSz; i++) {
        for (int j = 0; j < domainSz; j++) {
            if (domain[i][j] < min) {
                min = domain[i][j];
                mini = i;
                minj = j;
            }

            if (domain[i][j] > max) {
                max = domain[i][j];
                maxi = i;
                maxj = j;
            }
        }
    }

    printf("Size of data elements = %d bits\n\n", sizeof(DATATYPE) * 8);

    printf("min(domain) = %e (%d, %d)\n", min, mini, minj);
    printf("max(domain) = %e (%d, %d)\n\n", max, maxi, maxj);

    printf("FLT_EPSILON = %e\n", FLT_EPSILON);
    printf("FLT_MIN = %e\n", FLT_MIN);
    printf("DBL_EPSILON = %e\n", DBL_EPSILON);
    printf("DBL_MIN = %e\n", DBL_MIN);

    // Delete file if it already exist
    if ( access(FILENAME, F_OK) != -1 ) {
        unlink(FILENAME);
    }

    printf("\nWriting %s into %s ...\n\n", DATATYPESTR, FILENAME);

    // Write test file
    int iun = 0;
    c_fnom(&iun, FILENAME, "RND,R/W", 0);
    c_fstouv(iun, "RND");
    c_fstecr((uint32_t *) domain, NULL, 0 - ((uint32_t)sizeof(DATATYPE) * 8), iun, 20220610, 300, 0, domainSz, domainSz,
                 1, 1, 1, 1, "P", "WAVE", DATATYPESTR, "X", 0, 0, 0, 0, DATYP, 1);
    c_fstfrm(iun);
    c_fclos(iun);

    // Read test file
    iun = 0;
    c_fnom(&iun, FILENAME, "RND,R/O", 0);
    c_fstouv(iun, "RND");

    int ni = 0;
    int nj = 0;
    int nk = 0;
    int handle = c_fstinf(iun, &ni, &nj, &nk, -1, "", -1, -1, -1, "", "");
    if (!handle) {
        fprintf(stderr, "c_fstinf failed!\n");
        exit(1);
    }

    int dateo, deet, npas, nbits, datyp, ip1, ip2, ip3;
    char typvar[TYPVAR_LEN];
    char nomvar[NOMVAR_LEN];
    char etiket[ETIKET_LEN];
    char grtyp[GTYP_LEN];
    int ig1, ig2, ig3, ig4, swa, lng, dltf, ubc, extra1, extra2, extra3;
    int res = c_fstprm(handle,
        &dateo, &deet, &npas, &ni, &nj, &nk, &nbits, &datyp, &ip1, &ip2, &ip3,
        typvar, nomvar, etiket, grtyp,
        &ig1, &ig2, &ig3, &ig4, &swa, &lng, &dltf, &ubc, &extra1, &extra2, &extra3);
    if (res < 0) {
        fprintf(stderr, "c_fstprm failed!\n");
        exit(1);
    }
    printf("Reading field nomvar = \"%s\", ni = %d, nj = %d, nk = %d, nbits = %d, dateo = %d, deet = %d, npas = %d\n",
           nomvar, ni, nj, nk, nbits, dateo, deet, npas);
    if ( ni != domainSz || nj != domainSz || nk != 1) {
        fprintf(stderr, "Dimensions of the field read do not match those of the field written (%d, %d, 1) vs (%d, %d, %d)!\n",
            domainSz, domainSz, ni, nj, nk
        );
        exit(1);
    }

    DATATYPE field[ni][nj];

    res = c_fstluk((uint32_t *)&field, handle, &ni, &nj, &nk);
    if (res < 0) {
        fprintf(stderr, "c_fstluk failed!\n");
        exit(1);
    }

    // Compare field written to the field read
    int differences = 0;
    for (int i = 0; i < ni; i++) {
        for (int j = 0; j < nj; j++) {
            if (domain[i][j] != field[i][j]) {
                differences++;
                printf("Difference at (%d, %d) = %f\n", i, j, field[i][j] - domain[i][j]);
            }
        }
    }
    printf("\n%d differences found\n", differences);

    return differences > 0 ? 1 : 0;
// 
//     FILE * fp = fopen("out.png", "wb");
//     if (!fp) return ERROR;
// 
//     png_structp png_ptr = png_create_write_struct(
//         PNG_LIBPNG_VER_STRING, (png_voidp)user_error_ptr, user_error_fn, user_warning_fn);
//     if (!png_ptr) return ERROR;
// 
//     png_infop info_ptr = png_create_info_struct(png_ptr);
//     if (!info_ptr) {
//         png_destroy_write_struct(&png_ptr, (png_infopp)NULL);
//         return ERROR;
//     }
// 
//     if (setjmp(png_jmpbuf(png_ptr))) {
//         /* If we get here, we had a problem writing the file. */
//         fclose(fp);
//         png_destroy_write_struct(&png_ptr, &info_ptr);
//         return ERROR;
//     }
// 
//     png_init_io(png_ptr, fp);
// 
//     const int bytes_per_pixel = 8;
//     png_set_IHDR(png_ptr, info_ptr, domainSz, domainSz, bytes_per_pixel,
//        PNG_COLOR_TYPE_GRAY, PNG_INTERLACE_NONE, PNG_COMPRESSION_TYPE_BASE, PNG_FILTER_TYPE_BASE);
// 
//     png_write_info(png_ptr, info_ptr);
// 
//     const int height = domainSz;
//     const int width = domainSz;
//     png_byte image[height * width * bytes_per_pixel];
}
