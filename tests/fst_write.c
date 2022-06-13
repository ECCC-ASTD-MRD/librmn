#include <stdio.h>
#include <stddef.h>
#include <math.h>
#include <rmn.h>

//#include <png.h>

#define DATATYPE float
#define DATYP 1

// #define DATATYPE double
// #define DATYP 5

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
    DATATYPE min = 0.0;
    int mini = 0;
    int minj = 0;

    DATATYPE max = 0.0;
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

    int iun = 0;
    c_fnom(&iun, "test.fst", "RND,R/W", 0);
    c_fstouv(iun, "RND");
    c_fstecr((uint32_t *) domain, NULL, sizeof(DATATYPE), iun, 20220610, 300, 0, domainSz, domainSz,
                 1, 1, 1, 1, "P", "WAV", "TEST_WRITE", "X", 0, 0, 0, 0, DATYP, 1);
    c_fstfrm(iun);

//     printf("min(domain) = %f (%d, %d)\n", min, mini, minj);
//     printf("max(domain) = %f (%d, %d)\n", max, maxi, maxj);
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
    
    return 0;
}
