/* RMNLIB - Library of useful routines for C and FORTRAN programming
 * Copyright (C) 1975-2001  Division de Recherche en Prevision Numerique
 *                          Environnement Canada
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation,
 * version 2.1 of the License.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the
 * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.
 */

#define _LARGEFILE64_SOURCE
#define _FILE_OFFSET_BITS 64

#define CMCARC_SIGN "CMCARCHS"     /* signature du debut d'un fichier cmcarc */
#define CMCARC_SIGN_V5 "CMCARCH5"  /* signature du debut d'un fichier cmcarc version 5 */

#define SIGN_STD89_RND  012525252525
#define SIGN_STD89_SEQ  025252525252

#define  TRUE  (0 == 0)
#define  FALSE (0 == 1)

#define  IDGIF87   "GIF87a"
#define  IDGIF89   "GIF89a"

#define  RAS_MAGIC 0x59a66a95

/*  Sun supported ras_type's */
#define  RT_OLD          0      /* Raw pixrect image in 68000 byte order */
#define  RT_STANDARD     1      /* Raw pixrect image in 68000 byte order */
#define  RT_BYTE_ENCODED 2      /* Run-length compression of bytes */
#define  RT_EXPERIMENTAL 0xffff /* Reserved for testing */

/*  Sun registered ras_maptype's */
#define  RMT_RAW 2

/*  Sun supported ras_maptype's */
#define  RMT_NONE      0
#define  RMT_EQUAL_RGB 1

#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <ctype.h>
#include <string.h>
#include <strings.h>
#include <assert.h>
#include <sys/types.h>

#ifdef should_never_be_true
#include <X11/Xmd.h>
#else
    #include <unistd.h>
    #define CARD32 unsigned int
    #define B32 :32
#endif

#include <App.h>
#include <rmn/fstd98.h>
#include <rmn/c_wkoffit.h>

static int endian_int = 1;
static char *little_endian = (char *)&endian_int;

typedef struct {
    int ftn1;
    int nbits;
    int nr;
    int nw;
    int ncp;
    int ftn2[2];
    char plotid[80];
    int ftn3[2];
    char framid[80];
    int ftn4;
} Rrbxfile;


//! Header for files containing raster images
struct rasterfile {
    //! Magic number
    int ras_magic;
    //! Image width (pixels)
    int ras_width;
    //! Image height (pixels)
    int ras_height;
    //! Bits per pixel (depth)
    int ras_depth;
    //! Image size in bytes
    int ras_length;
    //! File type; see RT_* below
    int ras_type;
    //! Colormap; see RMT_* below
    int ras_maptype;
    //! Size in bytes of following map
    int ras_maplength;
};

/*  XWDFile  stuff */

#define XWD_FILE_VERSION 7

#ifdef ALL64
#define sz_XWDheader 104
#else
#define sz_XWDheader 100
#endif
#define sz_XWDColor 12


typedef struct _xwd_file_header {
    CARD32 header_size B32;  /* Size of the entire file header (bytes). */
    CARD32 file_version B32;    /* XWD_FILE_VERSION */
    CARD32 pixmap_format B32;   /* Pixmap format */
    CARD32 pixmap_depth B32;    /* Pixmap depth */
    CARD32 pixmap_width B32;    /* Pixmap width */
    CARD32 pixmap_height B32;   /* Pixmap height */
    CARD32 xoffset B32;     /* Bitmap x offset */
    CARD32 byte_order B32;      /* MSBFirst, LSBFirst */
    CARD32 bitmap_unit B32;     /* Bitmap unit */
    CARD32 bitmap_bit_order B32;    /* MSBFirst, LSBFirst */
    CARD32 bitmap_pad B32;      /* Bitmap scanline pad */
    CARD32 bits_per_pixel B32;  /* Bits per pixel */
    CARD32 bytes_per_line B32;  /* Bytes per scanline */
    CARD32 visual_class B32;    /* Class of colormap */
    CARD32 red_mask B32;        /* Z red mask */
    CARD32 green_mask B32;      /* Z green mask */
    CARD32 blue_mask B32;       /* Z blue mask */
    CARD32 bits_per_rgb B32;    /* Log2 of distinct color values */
    CARD32 colormap_entries B32;    /* Number of entries in colormap */
    CARD32 ncolors B32;     /* Number of Color structures */
    CARD32 window_width B32;    /* Window width */
    CARD32 window_height B32;   /* Window height */
    CARD32 window_x B32;        /* Window upper left X coordinate */
    CARD32 window_y B32;        /* Window upper left Y coordinate */
    CARD32 window_bdrwidth B32; /* Window border width */
#ifdef ALL64
    CARD32 header_end B32;      /* Pad to fill out uint32_t */
#endif
} XWDFileHeader;

/*  PPM stuff */
/* Magic constants. */
#define PPM_MAGIC1 'P'
#define PPM_MAGIC2 '3'
#define RPPM_MAGIC2 '6'
#define PPM_FORMAT (PPM_MAGIC1 * 256 + PPM_MAGIC2)
#define RPPM_FORMAT (PPM_MAGIC1 * 256 + RPPM_MAGIC2)

/*  PCL stuff */
static   char *enter_pcl = "\033%-12345X@PJL ENTER LANGUAGE=PCL";
static   char *enter_ps = "\033%-12345X@PJL ENTER LANGUAGE=POSTSCRIPT";
#define PCL_MAX_LEN 512000

static float Get_Frac();

static int mutant_kmw = FALSE;

static int ReadFileType(char *fname);
static void Flush_Bytes(unsigned int num, FILE *fp);
static void Flush_To_Term(FILE *fp);
static int ispcl(char *path);
static int isppm(char *path);
static int issun(char *path);
static int isrrbx(char *path);
static int isrrbx(char *path);
static int iskmw(char *path);
static int isgif(char *path);
static int isxwd(char *path);
static int isps(char *path);
static int test_fichier(char *nom);


static int retour(
    FILE * pf,
    int code
) {
    fclose(pf);
    return code;
}


//! Reads nitems elements of data, each size bytes long and swap each bytes for each 4 bytes elements
static size_t fread32(
    //! Pointer to array into which to place data read
    void *ptr,
    //! Size in bytes of elements of data
    size_t size,
    //! Number of items to read
    size_t nitems,
    //! File pointer from whih to read data
    FILE *stream
) {
    size_t nr;
    int i;
    int n4 = (size * nitems) / 4;    /* number of 4 bytes */
    uint32_t *pt4 = (uint32_t *) ptr;

    if (*little_endian) {
        if ((size & 3) != 0) {
            Lib_Log(APP_LIBRMN,APP_ERROR,"%f: size=%d must be a multiple of 4\n",__func__,size);
            return -1;
        }

        nr = fread(ptr, size, nitems, stream);

        for (i = 0; i < n4; i++) {
            *pt4 = (*pt4 >> 24) | (*pt4 << 24) | ((*pt4 >> 8) & 0xFF00) | ((*pt4 & 0xFF00) << 8);
            pt4++;
        }
    } else {
        nr = fread(ptr, size, nitems, stream);
    }

    return (size_t)nr;
}


static int isftnbin(
    FILE *pf,
    int lng
) {
    int mot;
    int32_t offset;

    offset = lng + 4;
    fseek(pf, offset, 0);
    fread32(&mot, sizeof(int), 1, pf);
    if (mot == lng && lng != 0) {
        return 1;
    } else {
        return 0;
    }
}


//! Return code type of file
/*!
    @return File type code
    -3  INEXISTANT
    -2  VIDE
    -1  INCONNU
     1  STANDARD RANDOM 89
     2  STANDARD SEQUENTIEL 89
     3  STANDARD SEQUENTIEL FORTRAN 89
     4  CCRN
     5  CCRN-RPN
     6  BURP
     7  GRIB
     8  BUFR
     9  BLOK
     10 FORTRAN
     11 COMPRESS
     12 GIF89
     13 GIF87
     14 IRIS
     15 JPG
     16 KMW
     17 PBM
     18 PCL
     19 PCX
     20 PDSVICAR
     21 PM
     22 PPM
     23 PS
     24 KMW_
     25 RRBX
     26 SUNRAS
     27 TIFF
     28 UTAHRLE
     29 XBM
     30 XWD
     31 ASCII
     32 BMP
     33 STANDARD RANDOM 98
     34 STANDARD SEQUENTIEL 98
     35 NETCDF
     36 FICHIER CMCARC v4
     37 FICHIER CMCARC v5
     38 HDF5
 */
int32_t c_wkoffit(
    //! Path of the file to examine
    const char * const filePath,
    //! Fall back to legacy fnom mode for filenames if True
    const int l1
) {
    FILE *pf;
    char nom2[4096], nom3[4096], *pn2, *pn3;
    char cbuf[1024];
    int buffer[1024], *ptbuf, lowc=0;
    int32_t pos, lngf;
    int longnom;

    longnom = ( ( l1 <= 4095 ) ? l1 : 4095 );
    pos = 0;
    ptbuf = &buffer[0];
    if (filePath[0] == '+') {
        /* garder le nom de fichier tel quel */
        longnom--;
        strncpy(nom2, filePath + 1, longnom);
        nom2[longnom] = '\0';
        while (nom2[--longnom] == ' ') {
            nom2[longnom] = '\0';
        }
    } else {
        strncpy(nom2, filePath, longnom);
        nom2[longnom] = '\0';
        pn2 = &nom2[0];
        pn3 = &nom3[0];
        while ((*pn2 != ' ') && (*pn2 != '\0')) {
            if (islower(*pn2)) {
                *pn3 = *pn2;
                lowc = 1;
            } else {
                *pn3 = tolower(*pn2);
            }
            pn2++;
            pn3++;
        }
        *pn2 = '\0';
        *pn3 = '\0';
        if (lowc == 0) strcpy(nom2, nom3);
    }
    pf = fopen(nom2, "rb");
    if (pf == (FILE *) NULL) {
        return WKF_INEXISTANT;
    } else {
        /* positionnement a la fin du fichier */
        fseek(pf, pos, 2);
        lngf = ftell(pf);
        if (lngf == 0) return retour(pf, WKF_VIDE);

        /* positionnement et lecture au debut du fichier */
        fseek(pf, pos, 0);
        fread(cbuf, 1024, 1, pf);           // suite de caracteres
        fseek(pf, pos, 0);
        fread32(ptbuf, sizeof(int), 1024, pf); // suite d'entiers 32 bits

        /* CMCARC v5 */
        if( strncmp(cbuf+17, CMCARC_SIGN_V5, 8) == 0 ) {
            return retour(pf, WKF_CMCARC5);
        }

        /* CMCARC v4 */
        if( strncmp(cbuf+9, CMCARC_SIGN, 8) == 0 ) {
            return retour(pf, WKF_CMCARC4);
        }

        /* RANDOM89 */
        if (*ptbuf == SIGN_STD89_RND && *(ptbuf+1) == SIGN_STD89_RND) {
            return retour(pf, WKF_RANDOM89);
        }

        /* CCRN */
        if (*(ptbuf) == 64 && *(ptbuf + 17) == 64 && *(ptbuf + 2) == 0x20202020) {
            return retour(pf, WKF_CCRN);
        }

        /* CCRN-RPN */
        if (*(ptbuf + 2) == 0x504b3834 && isftnbin(pf, *ptbuf)) {  /* PK84 */
            return retour(pf, WKF_CCRN_RPN);
        }

        /* SEQUENTIEL89 */
        if (*(ptbuf + 28) == SIGN_STD89_SEQ && *(ptbuf + 29) == SIGN_STD89_SEQ) {
            return retour(pf, WKF_SEQUENTIEL89);
        }

        /* SEQUENTIELFORTRAN89 */
        if (*(ptbuf + 29) == SIGN_STD89_SEQ && *(ptbuf + 30) == SIGN_STD89_SEQ && isftnbin(pf, *ptbuf)) {
            return retour(pf, WKF_SEQUENTIELFORTRAN89);
        }

        /* STANDARD 98 RANDOM */
        if (*(ptbuf + 3) == 'STDR') {
            if (c_fstcheck(nom2) < 0) {
                return retour(pf, WKF_CORROMPU);
            } else {
                return retour(pf, WKF_RANDOM98);
            }
        }

        /* STANDARD with RSF backend */
        if (*(ptbuf + 4) == 'RSF0' && *(ptbuf + 5) == 'STDR') {
            return retour(pf, WKF_STDRSF);
        }

        /* STANDARD 98 SEQUENTIEL */
        if (*(ptbuf + 3) == 'STDS') {
            return retour(pf, WKF_SEQUENTIEL98);
        }

        /* BURP */
        if ((*(ptbuf + 3) == 'BRP0') || (*(ptbuf + 3) == 'bRp0')) {
            if (c_burpcheck(nom2) < 0) {
                return retour(pf, WKF_CORROMPU);
            } else {
                return retour(pf, WKF_BURP);
            }
        }

        /* GRIB */
        if (*(ptbuf) == 0x47524942) {
            return retour(pf, WKF_GRIB);
        }

        /* BUFR */
        if (*(ptbuf) == 0x42554652) {
            return retour(pf, WKF_BUFR);
        }

        /* NetCDF classic format */
        if (*(ptbuf) == 'CDF\001') {
            return retour(pf, WKF_NETCDF);
        }

        /* NetCDF 64-bit offset format */
        if (*(ptbuf) == 'CDF\002') {
            return retour(pf, WKF_NETCDF);
        }

        /* HDF5 format */
        if (*(ptbuf) == '\x89HDF' && *(ptbuf + 1) == '\x0d\x0a\x1a\x0a') {
            return retour(pf, WKF_HDF5);
        }

        /* BLOK */
        if (*(ptbuf) == 0x424c4f4b) {
            return retour(pf, WKF_BLOK);
        }

        /* FORTRAN */
        if (isftnbin(pf, *ptbuf)) {
            return retour(pf, WKF_FORTRAN);
        }

        /* INCONNU  */
        return retour(pf, test_fichier(nom2));
    }
}


static int test_fichier(
    char *nom
) {
    int id;
    int repgif;

    repgif = isgif(nom);
    if ( repgif != FALSE ) return repgif;
    if ( isrrbx(nom) ) return WKF_RRBX;
    if ( issun(nom) ) return WKF_SUNRAS;
    if ( isxwd(nom) ) return WKF_XWD;
    if ( isppm(nom) ) return WKF_PPM;
    if ( iskmw(nom) ) {
        if ( mutant_kmw ) {
            return WKF_KMW_;
        } else {
            return WKF_KMW;
        }
    }
    if ( isps(nom) ) return WKF_PS;

    /*  essais la routine de xv-3.00a */
    id = ReadFileType(nom);
    if ( id == WKF_INCONNU ) {
        /*  dernier espoir */
        if ( ispcl( nom ) ) {
            return WKF_PCL;
        } else {
            return WKF_INCONNU;
        }
    } else {
        return id;
    }
}


//! Test if the file corresponding to the path provided is PostScript
static int isps(
    char *path
) {
    FILE *fp;
    char buffer[256];
    int  i, j, ps_i;

    if ( (fp = fopen( path, "rb")) == NULL ) return FALSE;

    i = 0;
    while ( fgets( buffer, 256, fp ) != NULL ) {

    /*  first must enter Postcript using PJL */

        if ( i == 0 ) {
            for ( j = 0 ; (j < 256)&&(buffer[j]!='\0') ; j++ ) {
                buffer[j] = (char)toupper((int)buffer[j]);
            }
            if ( strncmp( buffer, enter_ps, strlen(enter_ps) ) == 0 ) {
                fclose(fp);
                return TRUE;
            }
        }

        if ( strncmp( buffer, "%%", 2 ) == 0 ) continue;
        ++i;
        if ( i > 66 ) break;
        if ( strncmp( buffer, "%!", 2 ) == 0 ) {
            while ( fgets( buffer, 256, fp ) != NULL ) {
                ++i;
                if ( i > 66 ) break;
                switch( buffer[0] ) {
                    case '%' :
                    case '/' :
                        fclose(fp);
                        return TRUE;
                        break;
                    case '\n' :
                        break;
                    default :
                        fclose(fp);
                        return FALSE;
                        break;
                }
            }
        }
        if ( strncmp( buffer, "%", 1 ) == 0 ) continue;
        break;
    }
    fclose(fp);
    return FALSE;
}


//! Test if the file corresponding to the path provided is XWDFile
static int isxwd(
    char *path
) {
   FILE *fp = fopen( path, "rb");
   if ( fp == NULL ) return FALSE;

   XWDFileHeader xwd;
   if (fread32 ((char *) &xwd, sizeof(XWDFileHeader), 1, fp) != 1) {
       fclose(fp);
       return FALSE;
   }

   if (fseek( fp, 0L, SEEK_END )) {
       fclose(fp);
       return FALSE;
   }
   int flen = ftell(fp);
   fclose(fp);

   if (xwd.file_version != XWD_FILE_VERSION ) return FALSE;

   int len = xwd.header_size + xwd.ncolors * sz_XWDColor + xwd.bytes_per_line * xwd.pixmap_height;
   if ( len != flen ) return FALSE;
   return TRUE;
}


//! Test if the file corresponding to the path provided is a Gif
static int isgif(
    char *path
) {
   FILE *fp;
   char magic[6];
   int  status = FALSE;

   if ( (fp = fopen( path, "rb")) == NULL ) return FALSE;

   if (fread(magic, 6, 1, fp) != 1) return FALSE;
   if (strncmp( magic, IDGIF87, 6 ) == 0) status = WKF_GIF87;
   if (strncmp( magic, IDGIF89, 6 ) == 0) status = WKF_GIF89;
   fclose(fp);
   return status;
}


//! Get info from a KMW file
static int get_mode(
    FILE *fp,
    int  *depth,
    int  *height,
    int  *width,
    int32_t *length
) {
     int cpt = 0;
     int kmwndx = 0 ;
     int mode = 0;
     char buf[351];
     static char sigkmw[] = "PLOT$Z";

     register char *data = buf;
     int32_t  curpos;

    // Verifie dans les 350 premiers caracteres si on trouve la chaine PLOT$Z qui indique un fichier KMW
    while ( (*data = getc(fp)) != EOF && cpt++ < 350) {
        if ( *data == sigkmw[kmwndx] ) {
            if ( kmwndx == strlen(sigkmw) - 1) {
                mode = 1;
                break;
            } else {
                kmwndx++;
            }
        } else {
            kmwndx = 0;
        }
        data++;
    }
    if (mode == 0) return mode;

    *depth = 1;
    // Lre les dimensions et verifie que la decompression est possible
    *data = '\0';
    if (cpt < 10) mode = 0 ;
    if ( mode != 0 ) {
        data = (char *)rindex(buf, '\n');
        sscanf(data - 9, "%4d%4d%1d", width, height, depth );

        if( mode == 1 && (*depth != 1 && *depth != 3) ) {
            *depth = 777;
            *width = 4224;
            *height = 6048;
        }
    }

    curpos = ftell(fp);
    if (fseek( fp, 0L, SEEK_END ))  {
        fclose(fp);
        perror("fseek");
        exit(0);
    }

    *length = ftell(fp);
    if (fseek( fp, curpos, SEEK_SET ))  {
        fclose(fp);
        perror("fseek");
        exit(0);
    }

    return mode;
}


//! Test if the file corresponding to the path provided is a KMW
static int iskmw(
    char *path
) {
    FILE *fp;
    int height, width, depth;
    int ierr = 0;
    int32_t length;

    if ( (fp = fopen( path, "rb")) == NULL ) return ierr;

    ierr = get_mode( fp, &depth, &height, &width, &length );
    if ( depth == 3 ) mutant_kmw = TRUE;

    fclose(fp) ;

    return ierr;
 }


//! Test if the file corresponding to the path provided is a RRBX
static  int isrrbx(
    char *path
) {
    FILE    *fp;
    Rrbxfile header;

    if( (fp = fopen( path, "rb")) == NULL ) return FALSE;

    // Read header
    if ( fread32( &header, sizeof(Rrbxfile), 1, fp ) == 0 ) return FALSE;

    fclose(fp);

    if ( strncmp(&header.plotid[8], "RRBXRRUX", 8) != 0 ) return FALSE;

    return TRUE;
}


//! Test if the file corresponding to the path provided is a SUNRASTER
static int issun(
    char *path
) {
    FILE   *fp;
    struct rasterfile header;

    if ( (fp = fopen( path, "rb")) == NULL ) return FALSE;

    // Read header
    if ( fread32( &header, sizeof(struct rasterfile), 1, fp ) == 0 ) return FALSE;

    if ( header.ras_magic != RAS_MAGIC ) {
        fclose(fp);
        return FALSE;
    }

    fclose(fp);

    switch ( header.ras_maptype ) {
        case RT_OLD :
        case RT_STANDARD :
        case RT_BYTE_ENCODED :
            switch ( header.ras_maptype) {
                case RMT_EQUAL_RGB :
                case RMT_NONE :
                    return TRUE;
            }
    }

    return FALSE;
}

//! Test if the file corresponding to the path provided is a PPM
static int isppm(
    char *path
) {
    FILE *fp;
    int c0, c1;
    int magic;

    if ( (fp = fopen( path, "rb")) == NULL ) return FALSE;

    c0 = getc(fp);
    if ( c0 == EOF ) {
        fclose(fp);
        return FALSE;
    }
    c1 = getc(fp);
    if ( c1 == EOF ) {
        fclose(fp);
        return FALSE;
    }

    fclose(fp);

    magic = (c0 << 8) + c1 ;
    if ( (magic == PPM_FORMAT) || (magic == RPPM_FORMAT) ) return TRUE;

    return FALSE;
}


#define ESC 27


/*
**  These variables are for the new parser.
**  The new parser handles more sequences, and also deals with combined
**  escape sequences better.
*/

static int ispcl(
    char *path
) {
    FILE    *fp;
    int     c, j;
    int     parameter;
    int     group_char;
    int     terminator;
    int     value;
    float   fvalue;                 /* fractional value */
    int     scanf_count;
    char    in_sequence = FALSE;
    char    pass_seq;
    char    plus_sign;              /* for relative values */
    char    strip_seq = FALSE;
    int32_t flen;
    char    buffer[256];

    if ( (fp = fopen( path, "rb")) == NULL ) return FALSE;

    if (fseek( fp, 0L, SEEK_END )) {
        fclose(fp);
        return FALSE;
    }
    flen = ftell(fp);
    if (fseek( fp, 0L, SEEK_SET )) {
        fclose(fp);
        return FALSE;
    }

    // first line must enter PCL if it is not a graphic PCL
    if ( fgets( buffer, 256, fp ) == NULL ) {
        fclose(fp);
        return FALSE;
    }

    for ( j = 0; (j < 256) && (buffer[j] != '\0'); j++ ) {
        buffer[j] = (char)toupper((int)buffer[j]);
    }

    if ( strncmp( buffer, enter_pcl, 13 ) == 0 ) {
        fclose(fp);
        return TRUE;
    }

    // Escape sequence for enter graphic mode must be there or it isnt a PCL
    if (fseek( fp, 0L, SEEK_SET )) {
        fclose(fp);
        return FALSE;
    }

    // PCL input parsing loop.
    while ( ( c = getc(fp) ) != EOF ) {
        // Ignore all chars until an escape char
        if ( c != ESC ) continue;

        // Now we have an escape sequence, get the parameter char.
        parameter = getc(fp);

        if ( parameter == EOF ) {
            fclose(fp);
            return FALSE;
        }

        // Check if it is a two character sequence.
        if ( parameter >= '0' && parameter <= '~' ) continue;

        // Check that the parameter character is within range.
        if ( parameter < '!' || parameter > '/' ) {
            fclose(fp);
            return FALSE;
        }

        // We are only interested in certain parameters, so pass the rest of the sequences.

        /*
        *  For the moment, we are only interested in '*' (graphics)
        *  '(' and ')' (downloads).  Although we do not do anything
        *  with downloads, we need to pass the binary data thru
        *  untouched.
        *  Oops, '&' is handled too.
        */
        if ( parameter != '*' && parameter != '(' && parameter != ')' && parameter != '&' ) {
            // Flush rest of seq.
            Flush_To_Term(fp);
            continue;
        }

        // Parameter character is in range, look for a valid group char
        group_char = getc(fp);
        if ( group_char == EOF ) {
            // Ran out of input!
            fclose(fp);
            return FALSE;
        }

        /*
        *  See if in proper range.  If it isn't, it is not an error
        *  because the group character is optional for some sequences.
        *  For the moment, we are not interested in those sequences,
        *  so pass them thru.
        */
        if ( group_char < '`' || group_char > '~' ) {
            // If the "stripper" is active, we need to suspend it till graphics are re-started.
            if ( group_char < '@' || group_char > '^' ) {
                // Pass rest of seq.
                Flush_To_Term(fp);
            }
            continue;
        }

        /*
        *  Now we have a valid group character, decide if we want
        *  to deal with this escape sequence.
        *
        *  Sequences we want do deal with include:
        *
        *    <esc>*r    ** graphics
        *    <esc>*b    ** graphics
        *    <esc>*v    ** graphics
        *
        *  Sequences we must pass thru binary data:
        *
        *    <esc>*c    ** pattern
        *    <esc>*m    ** download dither
        *    <esc>*t    ** obsolete
        *    <esc>(f    ** download char set
        *    <esc>(s    ** download char
        *    <esc>)s    ** download font
        *    <esc>&a    ** logical page
        *    <esc>&b    ** AppleTalk stuff
        *    <esc>&l    ** obsolete
        *
        */
        if (
            ( parameter == '*' && group_char != 'r' && group_char != 'b' && group_char != 'v' && group_char != 'c' && group_char != 't' && group_char != 'm' )
            || ( parameter == '&' && group_char != 'a' && group_char != 'l' && group_char != 'b' )
            || ( parameter == '(' && group_char != 'f' && group_char != 's' )
            || ( parameter == ')' && group_char != 's' )
        ) {
            // Definately not interested in the sequence.
            Flush_To_Term(fp);
            continue;
        }

        /*
        *  If the sequence is <esc>&a#H, it will have gotten past
        *  the above, but we need to suspend the "stripper" if
        *  it is active, because the CAP is getting moved.
        *
        *  The <esc>*p#X/Y sequences will have been filtered
        *  thru just above (<esc>*p is not a needed group).
        */

        // Now set up a pass thru flag so we can ignore the entire sequences of some of these.
        if ( parameter != '*' ) {
            pass_seq = TRUE;
        } else if ( group_char == 'c' || group_char == 't' || group_char == 'm' ) {
            pass_seq = TRUE;
        } else {
            pass_seq = FALSE;
        }

        /*
        *  Now we have a sequence that we are definately interested in.
        *
        *  Get the value field and terminator, and loop until final
        *  terminator is found.
        */
        // First see if the value has a plus sign
        scanf_count = fscanf(fp, " + %d", &value );
        if ( scanf_count == 1 ) {
            plus_sign = TRUE;
        } else {
            plus_sign = FALSE;
            scanf_count = fscanf(fp, " %d", &value );
            if ( scanf_count == 0 ) {
                /* by default */
                value = 0;
            }
        }

        // I wonder if I will get bitten by a trailing space character right here?
        terminator = getc(fp);

        // Check for a fractional component.
        fvalue = 0.0;
        if ( terminator == '.' ) {
            fvalue = Get_Frac(fp);
            // Now get real terminator.
            terminator = getc(fp);
        }

        if ( terminator == EOF ) {
            fclose(fp);
            return FALSE;
        }

        // If the pass_seq flag is set, then just pass it thru to stdout until a 'W' is found.
        if ( pass_seq ) {
            if ( !in_sequence ) in_sequence = TRUE;

            // See if there was a non-zero fraction.
            if ( fvalue != 0.0 ) {
                if ( value < 0 ) {
                    value = -value;
                }
                fvalue += value;

                // if binary data, pass it thru
                if ( terminator == 'W' ) {
                    /* terminates */
                    in_sequence = FALSE;
                    /* pass data */
                    Flush_Bytes ( value, fp );
                }
                continue;
            }
        }

        // If we had gone so far, a big chance that it is a graphic PCL
        fclose(fp);
        return TRUE;
    }

    fclose(fp);
    return FALSE;
}

//! Pass thru input until a valid terminator character is found.
//! This is for unwanted escape sequences.
static void Flush_To_Term(
    FILE *fp
) {
    int c;

    do {
        c = getc(fp);

        if ( c == EOF ) {
            /* this is a problem */
            return;
        }
    } while ( c < '@' || c > '^' );
}


//! Tansfer the specified number of bytes directly from input to output
//! This is used to pass thru binary data that we are not interested in so that
//! it will not confuse the parser.  I.e. downloads.
static void Flush_Bytes(
    unsigned int num,
    FILE *fp
) {
    int  bnum;
    char buf[BUFSIZ];

    while ( num > 0 ) {
        bnum = ( BUFSIZ < num ? BUFSIZ : num );
        fread( buf, 1, bnum, fp );
        num -= bnum;
    }
}


//! Get the fractional part of a value.
//! This is here because scanf() will consume a trailing 'e' or 'E', which is a problem in PCL.
static float Get_Frac(
    FILE *fp
) {
    int c;
    float result = 0.0;
    float position = 10.0;

    while ( (c = getc(fp)) != EOF ) {
        if ( !isdigit(c) ) {
            /* put it back */
            ungetc( c, fp );
            /* quit */
            break;
        }

        result += ((c - '0') / position);

        position *= 10.0;
    }

    return result;
}


static int ReadFileType(
    char *fname
) {
  /* opens fname (which *better* be an actual file by this point!) and
     reads the first couple o' bytes.  Figures out what the file's likely
     to be, and returns the appropriate *** code */


  FILE *fp;
  unsigned char magicno[8];    /* first 8 bytes of file */
  int rv;


  rv = WKF_INCONNU;
  if (!fname) return rv;   /* shouldn't happen */

  fp = fopen(fname, "rb");

  if (!fp) return rv;

  rv = fread(magicno, 8, 1, fp);
  fclose(fp);

  if (rv!=1) return WKF_INCONNU;    /* files less than 8 bytes long... */

  rv = WKF_INCONNU;
  if (strncmp((char *) magicno, "GIF87a", 6)==0) rv = WKF_GIF87;

  else if (strncmp((char *) magicno, "GIF89a", 6)==0) rv = WKF_GIF89;

  else if (strncmp((char *) magicno, "VIEW", 4)==0 ||
           strncmp((char *) magicno, "WEIV", 4)==0) rv = WKF_PM;

  else if (magicno[0] == 'P' && magicno[1]>='1' &&
           magicno[1]<='6') rv = WKF_PBM;

  else if (strncmp((char *) magicno, "#define", 7)==0) rv = WKF_XBM;

  else if (magicno[0]==0x59 && (magicno[1]&0x7f)==0x26 &&
           magicno[2]==0x6a && (magicno[3]&0x7f)==0x15) rv = WKF_SUNRAS;

  else if (magicno[0] == 'B' && magicno[1] == 'M') rv = WKF_BMP;

  else if (magicno[0]==0x52 && magicno[1]==0xcc) rv = WKF_UTAHRLE;

  else if ((magicno[0]==0x01 && magicno[1]==0xda) ||
           (magicno[0]==0xda && magicno[1]==0x01)) rv = WKF_IRIS;

  else if (magicno[0]==0x1f && magicno[1]==0x9d) rv = WKF_COMPRESS;

  else if (magicno[0]==0x0a && magicno[1] <= 5) rv = WKF_PCX;

  else if (magicno[0]==0xff && magicno[1]==0xd8 &&
           magicno[2]==0xff) rv = WKF_JPG;

  else if ((magicno[0]=='M' && magicno[1]=='M') ||
           (magicno[0]=='I' && magicno[1]=='I')) rv = WKF_TIFF;

  else if (strncmp((char *) magicno,  "NJPL1I00", 8)==0 || /* fixed-len pds */
           strncmp((char *) magicno+2, "NJPL1I",  6)==0 || /* vger+other pds */
           strncmp((char *) magicno,  "CCSD3ZF", 7)==0 || /* vikng pds browse */
           strncmp((char *) magicno+2, "CCSD3Z",  6)==0 || /* vik. huffman pds */
           strncmp((char *) magicno,  "LBLSIZE=", 8)==0)   /* vicar */
      rv = WKF_PDSVICAR;

  else if (magicno[0] == '%' && magicno[1] == '!') rv = WKF_PS;

  return rv;
}





#if defined(WITH_TEST_PROGRAM)

int main(int argc, char **argv) {
    int i, code;
    if (argc < 3) {
        printf("USAGE: %s filename type_1 .... type_n\n", argv[0]);
        return 1;
    }
    code = c_wkoffit(argv[1], strlen(argv[1]));
    // printf("type code = %d\n", code);
    for (i = 2; i < argc; i++) {
        if (atoi(argv[i]) == code) return 0;
    }
    return 1;
}

#else

// not utility, provide Fortran callable entry point
#include <rmn/rpnmacros.h>
int32_t f77name(wkoffit)(
    char *nom,
    F2Cl fl1
) {
    return c_wkoffit(nom, fl1);
}

#endif
