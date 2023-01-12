/* RMNLIB - Library of useful routines for C and FORTRAN programming
 * Copyright (C) 1975-2004  Division de Recherche en Prevision Numerique
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

//! \file c_zfstlib.c Librairie de compression des enregistrements de fichiers standards RPN

#include <math.h>
#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

#include <App.h>
#include <rmn/rpnmacros.h>
#include "zfstlib.h"
#include "armn_compress_32.h"

int c_fstzip_parallelogram(unsigned int *zfld, int *zlng, unsigned short *fld, int ni, int nj, int step, int nbits, uint32_t *header);
void calcule_entropie(float *entropie, unsigned short *bitstream, int npts, int nbits);
void packTokensSample(unsigned int z[], int *zlng, unsigned int zc[], int nicoarse, int njcoarse, int diffs[], int ni, int nj, int nbits, int step, uint32_t *header, int start, int end);
static void unpackTokensSample(unsigned int zc[], int diffs[], unsigned int z[], int nicoarse, int njcoarse,  int ni, int nj, int nbits, int step, uint32_t *header, int start);
void c_armn_compress_setlevel(int level);
int c_armn_compress_getlevel();
void c_armn_compress_setswap(int swapState);
int  c_armn_compress_getswap();
void c_fstzip(unsigned int *zfld, int *zlng, unsigned int *fld, int ni, int nj, int code_methode, int degre, int step, int nbits, int bzip);
void c_fstunzip(unsigned int *fld, unsigned int *zfld, int ni, int nj, int nbits);
void c_fstzip_minimum(unsigned int *zfld, int *zlng, unsigned short *fld, int ni, int nj, int step, int nbits, uint32_t *header);
void c_fstunzip_minimum(unsigned short *fld, unsigned int *zfld, int ni, int nj, int step, int nbits, uint32_t *header);
void c_fstunzip_parallelogram(unsigned short *fld, unsigned int *zfld, int ni, int nj, int step, int nbits, uint32_t *header);
void c_fstunzip_sample(unsigned short *fld, unsigned int *zfld, int ni, int nj, int step, int nbits, uint32_t *header);

void f77name(armn_compress_setlevel)(int32_t *level);

void f77name(fill_coarse_nodes)(int32_t *z, int32_t *ni, int32_t *nj, int32_t *zc, int32_t *nicoarse, int32_t *njcoarse, int32_t *istep);
void f77name(ibicubic_int4)(int32_t *izo, int32_t *ni, int32_t *nj, int32_t *step, int32_t *ajus_x,int32_t *ajus_y);

static void calcul_ninjcoarse(int *nicoarse, int *njcoarse, int ni, int nj, int ajus_x, int ajus_y, int istep);
static void packTokensMinimum(unsigned int z[], int *zlng, unsigned short ufld[], int ni, int nj, int nbits, int istep, uint32_t *header);
static void unpackTokensMinimum(unsigned short ufld[], unsigned int z[], int ni, int nj, int nbits, int istep, uint32_t *header);
static void calcul_ajusxy(int *ajus_x, int *ajus_y, int ni, int nj, int istep);
static void packTokensParallelogram(unsigned int z[], int *zlng, unsigned short ufld[], int ni, int nj, int nbits, int istep, uint32_t *header);
static void unpackTokensParallelogram(unsigned short ufld[], unsigned int z[], int ni, int nj, int nbits, int istep, uint32_t *header);

static int fstcompression_level = -1;
static int swapStream           =  1;
static unsigned char fastlog[256];
static int once = 0;

//! Main entry point to the compression routines
int armn_compress(
    unsigned char *fld,
    int ni,
    int nj,
    int nk,
    int nbits,
    int op_code
) {
    unsigned short *unzfld;
    unsigned int *us_fld;
    unsigned int *zfld_minimum;
    unsigned int *zfld_lle;
    int zlng_minimum;
    int zlng_lle;
    int lng_origin;
    // int debug = 0;
    // float entropie;
    float rlog2;

#if defined (Little_Endian)
    int limite;
#endif

    int initial_compression_level;

    initial_compression_level = c_armn_compress_getlevel();
    lng_origin = (1 + ni * nj * nk * 16 / 8);

    if (initial_compression_level == -1) {
        fstcompression_level = BEST;
    }


    if (once == 0) {
        rlog2 = 1.0 / log(2.0);
        for (int i = 0; i < 256; i++) {
            fastlog[i] = (int)(1 + log(i + 0.5) * rlog2);
        }
        once = 1;
    }

    switch (op_code) {
        case COMPRESS:
            if (nbits > 16 || ni == 1 || nj == 1) {
                Lib_Log(APP_LIBFST,APP_WARNING,"%f: Cannot compress if nbits>16 or ni=1 or nj=1. Returning original field\n",__func__);
                return -1;
            }

            us_fld = (unsigned int *) fld;
            zfld_minimum = (unsigned int *) malloc(sizeof(unsigned int) * ni * nj * nk);
            zfld_lle     = (unsigned int *) malloc(sizeof(unsigned int) * ni * nj * nk);

#if defined (Little_Endian)
            if (swapStream == 1) {
                limite = (1 + ni * nj) / 2;
                for (int i = 0; i < limite; i++) {
                    us_fld[i] = (us_fld[i] >> 16) | (us_fld[i] << 16);
                }
            }
#endif

            // if (debug) {
            //     calcule_entropie(&entropie, (unsigned short *) us_fld, ni * nj * nk, nbits);
            // }

            if (0 == fstcompression_level || (ni < 16) || (nj < 16) || (nbits <= 4)) {
                c_fstzip(zfld_minimum, &zlng_minimum, us_fld, ni, nj, MINIMUM, 0, 5, nbits, 0);
                if (zlng_minimum >= lng_origin) {
#if defined (Little_Endian)
                    if (swapStream == 1) {
                        for (int i = 0; i < limite; i++){
                            us_fld[i] = (us_fld[i] >> 16) | (us_fld[i] << 16);
                        }
                    }
#endif
                    Lib_Log(APP_LIBFST,APP_WARNING,"%f: Compressed field is larger than original. Returning original \n",__func__);

                    free(zfld_minimum);
                    free(zfld_lle);
                    return -1;
                } else {
                    memcpy(fld, zfld_minimum, zlng_minimum);
                    free(zfld_minimum);
                    free(zfld_lle);
                    return zlng_minimum;
                }
            }

            c_fstzip(zfld_lle, &zlng_lle, us_fld, ni, nj, PARALLELOGRAM, 1, 3, nbits, 0);

            //Lib_Log(APP_LIBFST,APP_DEBUG,"%f: Entropie theorique:%f\tbps - minimum:%f\tbps - parallele:%f\tbps - sample:%f\n",__func__, entropie, 8.0 * zlng_minimum / ((ni * nj * nk) * 1.0), 8.0 * zlng_lle / ((ni * nj * nk) * 1.0), 8.0 * zlng_sample / ((ni * nj * nk) * 1.0));

            if (zlng_lle >= lng_origin) {
#if defined (Little_Endian)
                if (swapStream == 1) {
                    for (int i = 0; i < limite; i++) {
                        us_fld[i] = (us_fld[i] >> 16) | (us_fld[i] << 16);
                    }
                }
#endif
                Lib_Log(APP_LIBFST,APP_WARNING,"%f: Compressed field is larger than original. Returning original\n",__func__);

                free(zfld_minimum);
                free(zfld_lle);
                return -1;
            }

            memcpy(fld, zfld_lle, zlng_lle);
            free(zfld_minimum);
            free(zfld_lle);

            return zlng_lle;
            break;

        case UNCOMPRESS:
            if (nbits > 16 || ni == 1 || nj == 1) {
                return 1 + ni * nj * nk * nbits / 8;
            }

            us_fld = (unsigned int *) fld;
            unzfld = (unsigned short *)malloc(sizeof(unsigned int)*ni*nj);
            c_fstunzip((unsigned int *)unzfld, (unsigned int *)fld, ni, nj, nbits);
            memcpy(fld, unzfld, (1+ni*nj/2)*sizeof(unsigned int));
#if defined (Little_Endian)
            if (swapStream == 1) {
                limite = (1 + ni * nj) / 2;
                for (int i = 0; i < limite; i++) {
                    us_fld[i] = (us_fld[i] >> 16) | (us_fld[i] << 16);
                }
            }
#endif
            free(unzfld);
            return ni*nj*sizeof(short);
            break;
    } // switch(mode)
    return 0;
}



void c_fstzip(
    unsigned int *zfld,
    int *zlng,
    unsigned int *fld,
    int ni,
    int nj,
    int code_methode,
    int degre,
    int step,
    //! \deprecated Not used
    int nbits,
    int bzip
) {
    _fstzip zfstzip;

    memset(&zfstzip, 0, sizeof(_fstzip));

    zfstzip.predictor_type = code_methode;
    zfstzip.step           = step;
    zfstzip.degree         = degre;
    zfstzip.nbits          = nbits;
    zfstzip.levels         = 0;
    zfstzip.version        = 0;

    if (code_methode == MINIMUM) {
        zfstzip.levels = 1;
    }
    if (code_methode == PARALLELOGRAM) {
        zfstzip.levels = 1;
    }
    if (code_methode == SAMPLE) {
        zfstzip.levels = 3;
    }

    switch (code_methode) {
        case MINIMUM:
            c_fstzip_minimum(zfld, zlng, (unsigned short *)fld, ni, nj, step, nbits, (uint32_t *) &zfstzip);
            break;

        case PARALLELOGRAM:
            c_fstzip_parallelogram(zfld, zlng, (unsigned short *)fld, ni, nj, step, nbits, (uint32_t *) &zfstzip);
            break;

        case SAMPLE:
            Lib_Log(APP_LIBFST,APP_ERROR,"%f: The SAMPLE option has been deactivated as of April 2006. This is an error and should never happen\n",__func__);
            exit(13);
            break;

        default:
            break;
    }
}


void c_fstunzip(
    unsigned int *fld,
    unsigned int *zfld,
    int ni,
    int nj,
    //! \deprecated Not used
    int nbits
) {
    _fstzip zfstzip;

    memset(&zfstzip, 0, sizeof(_fstzip));
    memcpy(&zfstzip, &zfld[0], sizeof(float));

    switch (zfstzip.predictor_type) {
        case MINIMUM:
            c_fstunzip_minimum((unsigned short *)fld, zfld, ni, nj, zfstzip.step, zfstzip.nbits, (uint32_t *)&zfstzip);
            break;

        case PARALLELOGRAM:
            c_fstunzip_parallelogram((unsigned short *)fld, zfld, ni, nj, zfstzip.step, zfstzip.nbits, (uint32_t *)&zfstzip);
            break;

        case SAMPLE:
            c_fstunzip_sample((unsigned short *)fld, zfld, ni, nj, zfstzip.step, zfstzip.nbits, (uint32_t *)&zfstzip);
            break;

        default:
            Lib_Log(APP_LIBFST,APP_ERROR,"%f: Unknown compression algorithm\n");
            exit(13);
            break;
    }
}


void c_fstzip_minimum(unsigned int *zfld, int *zlng, unsigned short *fld, int ni, int nj, int step, int nbits, uint32_t *header)
{
    packTokensMinimum(zfld, zlng, fld, ni, nj, nbits, step, header);
}


int c_fstzip_parallelogram(unsigned int *zfld, int *zlng, unsigned short *fld, int ni, int nj, int step, int nbits, uint32_t *header)
{
    packTokensParallelogram(zfld, zlng, fld, ni, nj, nbits, step, header);
    // This function is declared as int, but doesn't even return a value.  Return 0 instead of random garbage.
    return 0;
}



void c_fstunzip_minimum(unsigned short *fld, unsigned int *zfld, int ni, int nj, int step, int nbits, uint32_t *header)
{
    unpackTokensMinimum(fld, zfld, ni, nj, nbits, step, header);
}

void c_fstunzip_parallelogram(unsigned short *fld, unsigned int *zfld, int ni, int nj, int step, int nbits, uint32_t *header) {
    unpackTokensParallelogram(fld, zfld, ni, nj, nbits, step, header);
}


void c_fstunzip_sample(unsigned short *fld, unsigned int *zfld, int ni, int nj, int step, int nbits, uint32_t *header)
{
  unsigned int *zc, *zc1, *zc2;
  int *idiffs;
  int ajus_x, ajus_y, ninj, i, j, k;
  int *predfld, lclstep, start;
  int nic, njc, nic1, njc1, nic2, njc2;
  int ajus_x1, ajus_x2, ajus_y1, ajus_y2;
  int *predfld1, *predfld2, *idiffs1, *idiffs2;

  ninj = ni * nj;

  lclstep = step;
  calcul_ajusxy(&ajus_x, &ajus_y, ni, nj, step);
  calcul_ninjcoarse(&nic, &njc, ni, nj, ajus_x, ajus_y, step);

  calcul_ajusxy(&ajus_x1, &ajus_y1, nic, njc, step);
  calcul_ninjcoarse(&nic1, &njc1, nic, njc, ajus_x1, ajus_y1, step);

  calcul_ajusxy(&ajus_x2, &ajus_y2, nic1, njc1, step);
  calcul_ninjcoarse(&nic2, &njc2, nic1, njc1, ajus_x2, ajus_y2, step);

  zc = malloc(nic*njc*sizeof(int));
  predfld = malloc(ninj*sizeof(int));
  idiffs = malloc(ninj*sizeof(int));

  zc1 = malloc(nic1*njc1*sizeof(int));
  predfld1 = malloc(nic*njc*sizeof(int));
  idiffs1 = malloc(nic*njc*sizeof(int));

  zc2 = malloc(nic2*njc2*sizeof(int));
  predfld2 = malloc(nic1*njc1*sizeof(int));
  idiffs2 = malloc(nic1*njc1*sizeof(int));

  /*************** Niveau 3 *******************/


  start = 1;
  unpackTokensSample(zc2, idiffs2, zfld, nic2, njc2, nic1, njc1, nbits, step, header, start);

  f77name(fill_coarse_nodes)(zc1, &nic1, &njc1, zc2, &nic2, &njc2, &lclstep);
  f77name(ibicubic_int4)(zc1, &nic1, &njc1, &lclstep, &ajus_x2, &ajus_y2);
  f77name(fill_coarse_nodes)(zc1, &nic1, &njc1, zc2, &nic2, &njc2, &lclstep);

  for (j=1; j <= njc1; j++)
    {
    for (i=1; i <= nic1; i++)
      {
      k = FTN2C(i, j, nic1);
      zc1[k] += idiffs2[k];
/*      if (zc1[k] < 0) zc1[k] = 0;*/
      }
    }

    /*************** Niveau 2 *******************/


  start = 0;
  unpackTokensSample(zc1, idiffs1, zfld, nic1, njc1, nic, njc, nbits, step, header, start);

  f77name(fill_coarse_nodes)(zc, &nic, &njc, zc1, &nic1, &njc1, &lclstep);
  f77name(ibicubic_int4)(zc,&nic,&njc,&lclstep,&ajus_x1, &ajus_y1);
  f77name(fill_coarse_nodes)(zc, &nic, &njc, zc1, &nic1, &njc1, &lclstep);

  for (j=1; j <= njc; j++)
    {
    for (i=1; i <= nic; i++)
      {
      k = FTN2C(i, j, nic);
      zc[k] += idiffs1[k];
/*      if (zc[k] < 0) zc[k] = 0;*/
      }
    }


    /*************** Niveau 1 *******************/


  start = 0;
  unpackTokensSample(zc, idiffs, zfld, nic, njc, ni, nj, nbits, step, header, start);

  f77name(fill_coarse_nodes)(predfld, &ni, &nj, zc, &nic, &njc, &lclstep);
  f77name(ibicubic_int4)(predfld,&ni,&nj,&lclstep,&ajus_x, &ajus_y);

  for (j=1; j <= nj; j++)
    {
    for (i=1; i <= ni; i++)
      {
      k = FTN2C(i, j, ni);
      predfld[k] += idiffs[k];
      if (predfld[k] < 0) predfld[k] = 0;
      }
    }


  f77name(fill_coarse_nodes)(predfld, &ni, &nj, zc, &nic, &njc, &lclstep);

  for (j=1; j <= nj; j++)
    {
    for (i=1; i <= ni; i++)
      {
      k = FTN2C(i, j, ni);
      fld[k] = (unsigned short) predfld[k];
      }
    }

  free(idiffs);
  free(idiffs1);
  free(idiffs2);
  free(predfld);
  free(predfld1);
  free(predfld2);
  free(zc);
  free(zc1);
  free(zc2);
  }


/*
 Given a stream of unsigned shorts, this routine re-encodes the stream by the minimum tile method.
 The stream is decomposed into a number "step * step" tiles, for each of which the minimum value is stored, and the difference between
 each cell and the minimum for each member of the tile

 Output stream
 Token 0 contains the zfstzip structure. Starting from Token 1, we have
 |---------------|---------------|---------------------------------|
 |  nbits_needed | local_min     | local_value - local_min         |
 |---------------|---------------|---------------------------------|
 |  4 bits       |  nbits        | step * step * nbits_needed      |
 |---------------|---------------|---------------------------------|

 With the exception that if the field is invariant within the tile, nbits_needed = 0 and we only encode
 |---------------|---------------|
 |  nbits_needed | local_min     |
 |---------------|---------------|
 |  4 bits       |  nbits        |
 |---------------|---------------|

 Variables :
   z      : output compressed byte stream
   zlng   : the length of the stream, in bytes
   ufld   : the source unsigned short stream
   ni, nj : dimensions of the field
   nbits  : the number of bits used to pack the original stream - normally btn 2 and 16
   istep  : the size of the tile (normally 5)
   header : contents of the zfstzip structure for info about compression parameters
*/


void packTokensMinimum(unsigned int z[], int *zlng, unsigned short ufld[], int ni, int nj, int nbits, int istep, uint32_t *header)
{
  unsigned int i, j, k, m, n;
  unsigned int lastWordShifted, spaceInLastWord, lastSlot;
  int lcl_m, lcl_n;

  float entropie;
  unsigned int *cur, local_min, local_max, local_var;
  unsigned int local_bins[24];
  unsigned int lcl, nbits_needed, lsum;
  unsigned char debug;

  debug = 0;
  lastSlot = 0;
  cur = z;

   if (debug)
   {
   for (i=0; i <= 16; i++)
      {
      local_bins[i] = 0;
      }
   }

  lastWordShifted = 0;
  spaceInLastWord = 32;
  memcpy(cur, header, sizeof(unsigned int));
  cur++;
  *cur = 0;
  for (j=1; j <= nj; j+=istep)
    {
    lcl_n = ((j + istep - 1) >= nj ? nj - j : istep - 1);
    for (i=1; i <= ni; i+=istep)
      {
      k = FTN2C(i,j,ni);
      local_min = ufld[k];
      local_max = local_min;
      lcl_m = ((i + istep - 1) >= ni ? ni - i: istep - 1);
      for (n=0; n <= lcl_n; n++)
        {
        for (m=0; m <= lcl_m; m++)
          {
          k = FTN2C(i+m,j+n,ni);
          if (local_min > ufld[k]) local_min = ufld[k];
          if (local_max < ufld[k]) local_max = ufld[k];
          }
        }
      local_var = local_max - local_min;
      if (local_var == 0)
        {
        nbits_needed = 0;
        }
      else
        {
        if (local_var < 256)
         {
         nbits_needed = fastlog[local_var];
         }
       else
         {
         nbits_needed = 8 + fastlog[local_var>>8];
         }
        }
      if (nbits_needed == 16) nbits_needed = 15;
      stuff(nbits_needed, cur, 32, 4, lastWordShifted, spaceInLastWord);
      switch (nbits_needed)
        {
        case 0:
        stuff(local_min, cur, 32, nbits, lastWordShifted, spaceInLastWord);
        break;

        case 15:
        for (n=0; n <= lcl_n; n++)
          {
          for (m=0; m <= lcl_m; m++)
            {
            k = FTN2C(i+m,j+n,ni);
            stuff(ufld[k], cur, 32, 16, lastWordShifted, spaceInLastWord);
            }
          }
        break;

        default:
        stuff(local_min, cur, 32, nbits, lastWordShifted, spaceInLastWord);
        for (n=0; n <= lcl_n; n++)
          {
          for (m=0; m <= lcl_m; m++)
            {
            k = FTN2C(i+m,j+n,ni);
            lcl = ufld[k] - local_min;
            stuff(lcl, cur, 32, nbits_needed, lastWordShifted, spaceInLastWord);
            }
          }
        break;
        }
       }
    }


   lcl = 0;
   stuff(lcl, cur, 32, 16, lastWordShifted, spaceInLastWord);
   stuff(lcl, cur, 32, 16, lastWordShifted, spaceInLastWord);

   *zlng = 1 + (int) (cur-z) * 4;

   if (Lib_LogLevel(APP_LIBFST,NULL)==APP_EXTRA) {
      entropie = 8.0*(*zlng)/(ni*nj);
      Lib_Log(APP_LIBFST,APP_VERBATIM,"total_space:\t%d\tentropie:\t%f\n",*zlng,entropie);
      for (i=0; i <= 16; i++) {
         Lib_Log(APP_LIBFST,APP_VERBATIM,"%05d ", i);
      }
      Lib_Log(APP_LIBFST,APP_VERBATIM,"\n");
      lsum = 0;
      for (i=0; i <= 16; i++) {
         lsum+=local_bins[i];
         Lib_Log(APP_LIBFST,APP_VERBATIM,"%05d ", local_bins[i]);
      }
      Lib_Log(APP_LIBFST,APP_VERBATIM,"---- npts : %d \n", lsum);
   }
}


/* See the documentation of "packTokensMinimum" for the structure of the compressed stream */

void unpackTokensMinimum(unsigned short ufld[], unsigned int z[], int ni, int nj, int nbits, int istep, uint32_t *header)
{
    unsigned int *cur, local_min;
    unsigned int  nbits_needed, curword;
    int lcl_m, lcl_n;

    int bitPackInWord = 32;

    cur = z;
    memcpy(header, cur, sizeof(unsigned int));
    cur++;
    curword = *cur;
    for (int j = 1; j <= nj; j += istep) {
        lcl_n = ((j + istep - 1) >= nj ? nj - j : istep - 1);
        for (int i = 1; i <= ni; i += istep) {
            lcl_m = ((i + istep - 1) >= ni ? ni - i : istep - 1);
            extract(nbits_needed, cur, 32, 4, curword, bitPackInWord);
            switch (nbits_needed) {
                case 0:
                    extract(local_min, cur, 32, nbits, curword, bitPackInWord);
                    for (int n = 0; n <= lcl_n; n++) {
                        for (int m = 0; m <= lcl_m; m++) {
                            ufld[FTN2C(i + m, j + n, ni)] = local_min;
                        }
                    }
                    break;

                case 15:
                case 16:
                    for (int n = 0; n <= lcl_n; n++) {
                        for (int m = 0; m <= lcl_m; m++) {
                            extract(ufld[FTN2C(i + m, j + n, ni)], cur, 32, 16, curword, bitPackInWord);
                        }
                    }
                    break;

                default:
                    extract(local_min, cur, 32, nbits, curword, bitPackInWord);
                    for (int n = 0; n <= lcl_n; n++) {
                        for (int m = 0; m <= lcl_m; m++) {
                            int k = FTN2C(i + m, j + n, ni);
                            extract(ufld[k], cur, 32, nbits_needed, curword, bitPackInWord);
                            ufld[k] += local_min;
                        }
                    }
                break;
            }
        }
    }
}


//! See the documentation of "packTokensMinimum" for the structure of the compressed stream
static void packTokensParallelogram(
    unsigned int z[],
    int *zlng,
    unsigned short ufld[],
    int ni,
    int nj,
    int nbits,
    int istep,
    uint32_t *header
) {
    unsigned int k;
    unsigned int lastWordShifted, spaceInLastWord;
    int lcl_m, lcl_n;

    float rlog2;
    int local_max;
    unsigned int lcl, nbits_needed;
    int k22;
    unsigned int nbits2;
    unsigned int nbits_req_container, gt16, token;
    int *ufld_dst;

    unsigned int *cur = z;
    unsigned int lastSlot = 0;

    if (once == 0) {
        rlog2 = 1.0/log(2.0);
        for (int i = 0; i < 256; i++) {
            fastlog[i] = (int)(1 + log(i + 0.5) * rlog2);
        }
        once = 1;
    }

    ufld_dst = (int *) malloc(ni * nj * sizeof(int));

    for (int j = 1; j <= nj; j++) {
        k = FTN2C(1, j, ni);
        ufld_dst[k] = 0;
    }

    for (int i = 1; i <= ni; i++) {
        k = FTN2C(i, 1, ni);
        ufld_dst[k] = 0;
    }

    for (int j = 2; j <= nj; j++) {
        for (int i = 2; i <= ni; i++) {
            k22 = FTN2C(i, j, ni);
            ufld_dst[k22] = ufld[k22] - (ufld[k22 - ni] + ufld[k22 - 1] - ufld[k22 - 1 - ni]);
        }
    }

    nbits_req_container = 4;

    gt16 = 0;
    if (nbits >= 15) {
        int i = 0;
        while (i < ni * nj && gt16 == 0) {
            if (65535 < abs(ufld_dst[i])) {
                nbits_req_container = 5;
                gt16 = 1;
            }
            i++;
        }
    }

    lastWordShifted = 0;
    spaceInLastWord = 32;
    memcpy(cur, header, sizeof(unsigned int));
    cur++;
    *cur = 0;

    stuff(nbits_req_container, cur, 32, 3, lastWordShifted, spaceInLastWord);

    for (int i = 1; i <= ni; i++) {
        k = FTN2C(i, 1, ni);
        stuff(ufld[k], cur, 32, nbits, lastWordShifted, spaceInLastWord);
    }

    for (int j = 2; j <= nj; j++) {
        k = FTN2C(1, j, ni);
        stuff(ufld[k], cur, 32, nbits, lastWordShifted, spaceInLastWord);
    }

    for (int j = 2; j <= nj; j += istep) {
        lcl_n = ((j + istep - 1) >= nj ? nj - j : istep - 1);
        for (int i = 2; i <= ni; i += istep) {
            k = FTN2C(i, j, ni);
            local_max = ufld_dst[k];
            lcl_m = ((i + istep - 1) >= ni ? ni - i : istep - 1);
            for (int n = 0; n <= lcl_n; n++) {
                for (int m = 0; m <= lcl_m; m++) {
                    k = FTN2C(i + m, j + n, ni);
                    if (local_max < abs(ufld_dst[k])) local_max = abs(ufld_dst[k]);
                }
            }
            if (local_max == 0) {
                nbits_needed = 0;
            } else {
                if (local_max < 256) {
                    nbits_needed = fastlog[local_max];
                } else if (local_max > 65535) {
                    nbits_needed =(int)(1 + log(local_max + 0.5) / log(2.0));
                } else {
                    nbits_needed = 8 + fastlog[local_max >> 8];
                }
            }
            if (nbits_needed == 16) nbits_needed = 15;
            stuff(nbits_needed, cur, 32, nbits_req_container, lastWordShifted, spaceInLastWord);
            switch (nbits_needed) {
                case 0:
                    break;

                case 15:
                    for (int n = 0; n <= lcl_n; n++) {
                        for (int m = 0; m <= lcl_m; m++) {
                            k = FTN2C(i + m, j + n, ni);
                            token = (unsigned int) (ufld_dst[k] & ~((-1) << 17));
                            stuff(token, cur, 32, 17, lastWordShifted, spaceInLastWord);
                        }
                    }
                    break;

                default:
                    nbits2 = nbits_needed + 1;
                    for (int n = 0; n <= lcl_n; n++) {
                        for (int m = 0; m <= lcl_m; m++) {
                            k = FTN2C(i + m, j + n, ni);
                            token = (unsigned int) (ufld_dst[k] & ~((-1) << nbits2));
                            stuff(token, cur, 32, nbits2, lastWordShifted, spaceInLastWord);
                        }
                    }
                    break;
            }
        }
    }

    lcl = 0;
    stuff(lcl, cur, 32, 16, lastWordShifted, spaceInLastWord);
    stuff(lcl, cur, 32, 16, lastWordShifted, spaceInLastWord);

   *zlng = 1 + (int) (cur-z) * 4;
    free(ufld_dst);
}


static void unpackTokensParallelogram(unsigned short ufld[], unsigned int z[], int ni, int nj, int nbits, int istep, uint32_t *header)
{
    int bitPackInWord = 32;

    unsigned int *cur = z;
    unsigned int  nbits_needed;
    int lcl_m, lcl_n;
    int *ufld_tmp;
    int k11, k12, k21, k22;
    unsigned int nbits_req_container, token, nbits2;

    memcpy(header, cur, sizeof(unsigned int));
    cur++;
    unsigned int curword = *cur;
    ufld_tmp = (int *) malloc(ni * nj * sizeof(int));

    extract(nbits_req_container, cur, 32, 3, curword, bitPackInWord);

    for (int i = 1; i <= ni; i++) {
        extract(token, cur, 32, nbits, curword, bitPackInWord);
        ufld[FTN2C(i, 1, ni)] = token;
    }

    for (int j = 2; j <= nj; j++) {
        extract(token, cur, 32, nbits, curword, bitPackInWord);
        ufld[FTN2C(1, j, ni)] = token;
    }


    for (int j = 2; j <= nj; j += istep) {
        lcl_n = ((j + istep - 1) >= nj ? nj - j : istep - 1);
        for (int i = 2; i <= ni; i += istep) {
            lcl_m = ((i + istep - 1) >= ni ? ni - i : istep - 1);
            extract(nbits_needed, cur, 32, nbits_req_container, curword, bitPackInWord);
            switch (nbits_needed) {
                case 0:
                    for (int n = 0; n <= lcl_n; n++) {
                        for (int m = 0; m <= lcl_m; m++) {
                            ufld_tmp[FTN2C(i + m, j + n, ni)] = 0;
                        }
                    }
                    break;

                case 15:
                case 16:
                    for (int n = 0; n <= lcl_n; n++) {
                        for (int m = 0; m <= lcl_m; m++) {
                            int k = FTN2C(i + m, j + n, ni);
                            extract(token, cur, 32, 17, curword, bitPackInWord);
                            ufld_tmp[k] = token;
                            ufld_tmp[k] = (ufld_tmp[k] << 15) >> 15;
                        }
                    }
                    break;

                default:
                    nbits2 = nbits_needed + 1;
                    for (int n=0; n <= lcl_n; n++) {
                        for (int m = 0; m <= lcl_m; m++) {
                            int k = FTN2C(i + m, j + n, ni);
                            extract(token, cur, 32, nbits2, curword, bitPackInWord);
                            ufld_tmp[k] = token;
                            ufld_tmp[k] = (ufld_tmp[k] << (32-nbits2)) >> (32-nbits2);
                        }
                    }
            }
        }
    }

    for (int j = 2; j <= nj; j++) {
        for (int i = 2; i <= ni; i++) {
            k11 = FTN2C(i-1, j-1,ni);
            k12 = FTN2C(i-1, j  ,ni);
            k21 = FTN2C(i,   j-1,ni);
            k22 = FTN2C(i,   j,  ni);
            ufld[k22] =  ufld_tmp[k22] + (ufld[k21]+ufld[k12]-ufld[k11]);
        }
    }

    free(ufld_tmp);
}



/*
 Given a stream of unsigned shorts, this routine re-encodes the stream by the minimum tile method.
 The stream is decomposed into a number "step * step" tiles, for each of which the minimum value is stored, and the difference between
 each cell and the minimum for each member of the tile

 Output stream
 Token 0 contains the zfstzip structure. Starting from Token 1, we have
 |---------------|---------------|---------------------------------|
 |  nbits_needed | local_min     | local_value - local_min         |
 |---------------|---------------|---------------------------------|
 |  4 bits       |  nbits        | step * step * nbits_needed      |
 |---------------|---------------|---------------------------------|

 With the exception that if the field is invariant within the tile, nbits_needed = 0 and we only encode
 |---------------|---------------|
 |  nbits_needed | local_min     |
 |---------------|---------------|
 |  4 bits       |  nbits        |
 |---------------|---------------|

 Variables :
   z      : output compressed byte stream
   zlng   : the length of the stream, in bytes
   ufld   : the source unsigned short stream
   ni, nj : dimensions of the field
   nbits  : the number of bits used to pack the original stream - normally btn 2 and 16
   istep  : the size of the tile (normally 5)
   header : contents of the zfstzip structure for info about compression parameters
*/




/*
 Given a stream of unsigned shorts, this routine re-encodes the stream by the bi-cubic tile method.
 The stream is decomposed into a number "step * step" tiles, for each of which the the values in the tile are predicted by
 bicubic interpolation. What is stored is the prediction error, ie the difference between the predicted value the original value.

 This function is a front end to the "packTokenSample" function, which it calls 3 times

 Initially, we have

 |-------------------------------------------------------------------
 | zc                                | diffs                        |
 |-------------------------------------------------------------------

 Then zc is split in 2 parts, zc1 and diffs1

 |-------------------------------------------------------------------
 | zc1            | diffs1           | diffs                        |
 |-------------------------------------------------------------------

 and finally zc1 is split in zc2 and idiffs2

 |-------------------------------------------------------------------
 | zc2 |  diffs2  | diffs1           | diffs                        |
 |-------------------------------------------------------------------

 At the uncompression level,
 zc2 and diffs2 will be merged to reform zc1
 zc1 and diffs1 will be merged to reform zc, and
 zc and diffs will be merged to reform the original stream
 */


/*
 Given a stream of unsigned shorts, this routine re-encodes the stream by the bi-cubic tile method.
 The stream is decomposed into a number "step * step" tiles, for each of which the the values in the tile are predicted by
 bicubic interpolation. What is stored is the prediction error, ie the difference between the predicted value the original value.

 Output stream
 Token 0 contains the zfstzip structure.
 Token 1 starts with the 3 bits token"nbits_req". This token has the value 4 or 5. "nbits_req" defines how many bits are required
 to encode the differences. If the number is <=16, nbits_req = 4. Otherwise nbits_req = 5. The condition where nbits_req = 5 occurs very rarely,
 but it does can occur, for instance if we encode 16-bit tokens, if the field is noisy then we can have prediction errors demanding more than 16 bits
 then we will have nbits_req = 5
 Starting from Token 1, we have
 |-----------------------------|-----------|--------------|----------------------------------------|--------------|-----------------------------------
 |  zcoarse                    | nbits_req | nbits_needed | diffs (tile 0)                         | nbits_needed | diffs (tile 1)
 |-----------------------------|-----------|--------------|----------------------------------------|--------------|-----------------------------------
    nicoarse*njcoarse*nbits    | 3         | nbits_req    | (step* step - 1)* nbits_needed         | nbits_needed | (step* step - 1)* nbits_needed
 |-----------------------------|-----------|--------------|----------------------------------------|--------------|-----------------------------------


 Variables :
   z      : output compressed byte stream
   zlng   : the length of the stream, in bytes
   zc     : a coarse grid used to generate the interpolated values
   nicoarse, njcoarse : dimensions of the coarse grid
   diffs  : the prediction errors
   ni, nj : dimension of the source field
   nbits  : the number of bits used to pack the original stream - normally btn 2 and 16
   istep  : the size of the tile (normally 5)
   header : contents of the zfstzip structure for info about compression parameters
   start  : flag to indicate the status of the stream
            if == 1, we start a new stream
            if == 0, we continue where we left
   end    : flag to indicate the end of the stream
            if == 0, nothing is done
            if == 1, a 32-bit word containing 0 closes the stream
*/
void packTokensSample(unsigned int z[], int *zlng, unsigned int zc[], int nicoarse, int njcoarse, int diffs[], int ni, int nj, int nbits, int step, uint32_t *header, int start, int end)
{
  int i, j, k, m, n;
  static unsigned int lastWordShifted, spaceInLastWord, lastSlot;
  static unsigned int *cur;
  int lcl_m, lcl_n;

  float entropie;
  int local_max;
  unsigned int nbits_req_container, local_bins[24];
  int lcl, nbits_needed, nbits2,lsum;
  unsigned char debug;
  unsigned int token, gt16;


  if (start == 1)
    {
    lastSlot = 0;
    cur = z;
    memset(z, 0, ni*nj*sizeof(unsigned int));
    for (i=0; i <= 20; i++)
      {
      local_bins[i] = 0;
      }

    lastWordShifted = 0;
    spaceInLastWord = 32;
    memcpy(cur, header, sizeof(unsigned int));
    cur++;
    *cur = 0;
    }

  nbits_req_container = 4;
  if (start == 1)
    {
    for (j=1; j <= njcoarse; j++)
      {
      for (i=1; i <= nicoarse; i++)
        {
        k = FTN2C(i,j,nicoarse);
        stuff(zc[k], cur, 32, nbits, lastWordShifted, spaceInLastWord);
        }
      }
    }

  i = 0;
  gt16 = 0;
  while (i < ni*nj && gt16 == 0)
    {
    if (65535 < abs(diffs[i]))
      {
      nbits_req_container = 5;
      gt16 = 1;
      }
    i++;
    }


  stuff(nbits_req_container, cur, 32, 3, lastWordShifted, spaceInLastWord);

  for (j=1; j <= nj; j+=step)
    {
    lcl_n = ((j + step - 1) >= nj ? nj - j: step - 1);
    for (i=1; i <= ni; i+=step)
      {
      k = FTN2C(i,j,ni);
      local_max = abs(diffs[k]);
      lcl_m = ((i + step - 1) >= ni ? ni - i : step - 1);
      for (n=0; n <= lcl_n; n++)
        {
        for (m=0; m <= lcl_m; m++)
          {
          if (!(m == 0 && n == 0))
            {
            k = FTN2C(i+m,j+n,ni);
            if (local_max < abs(diffs[k])) local_max = abs(diffs[k]);
            }
          }
        }
      if (local_max == 0)
        {
        nbits_needed = 0;
        }
      else
        {
        nbits_needed = (int)(1+log(local_max+0.5)/log(2.0));
        }
      if (nbits_needed == 16) nbits_needed = 15;
      local_bins[nbits_needed]++;
      stuff(nbits_needed, cur, 32, nbits_req_container, lastWordShifted, spaceInLastWord);
      switch (nbits_needed)
        {
        case 0:
        break;

        case 15:
        case 16:
        for (n=0; n <= lcl_n; n++)
          {
          for (m=0; m <= lcl_m; m++)
            {
            if (!(m == 0 && n == 0))
              {
              k = FTN2C(i+m,j+n,ni);
              token = (unsigned int) (diffs[k] & ~((-1) << 17));
              stuff(token, cur, 32, 17, lastWordShifted, spaceInLastWord);
              }
            }
          }
        break;

        default:
        nbits2 = nbits_needed + 1;
        for (n=0; n <= lcl_n; n++)
          {
          for (m=0; m <= lcl_m; m++)
            {
            if (!(m == 0 && n == 0))
              {
              k = FTN2C(i+m,j+n,ni);
              token = (unsigned int) (diffs[k] & ~((-1) << nbits2));
              stuff(token, cur, 32, nbits2, lastWordShifted, spaceInLastWord);
              }
            }
          }
        break;
        }

       }
    }


  if (end == 1)
    {
    lcl = 0;
    stuff(lcl, cur, 32, 16, lastWordShifted, spaceInLastWord);
    stuff(lcl, cur, 32, 16, lastWordShifted, spaceInLastWord);
    }

    *zlng = 1 + (int) (cur-z) * 4;

   if (Lib_LogLevel(APP_LIBFST,NULL)==APP_EXTRA) {
      entropie = 8.0*(*zlng)/(ni*nj);

      Lib_Log(APP_LIBFST,APP_VERBATIM,"total_space:\t%d\tentropie:\t%f\n",*zlng,entropie);
      for (i=0; i <= 20; i++) {
         Lib_Log(APP_LIBFST,APP_VERBATIM,"%05d ", i);
      }
      Lib_Log(APP_LIBFST,APP_VERBATIM,"\n");
      lsum = 0;
      for (i=0; i <= 20; i++) {
         lsum+=local_bins[i];
         Lib_Log(APP_LIBFST,APP_VERBATIM,"%05d ", local_bins[i]);
      }
      Lib_Log(APP_LIBFST,APP_VERBATIM,"---- npts : %d \n", lsum);
   }
}


/* See documentation for packTokenSample */

void unpackTokensSample(unsigned int zc[], int diffs[], unsigned int z[], int nicoarse, int njcoarse,  int ni, int nj, int nbits, int step, uint32_t *header, int start)
{
    static unsigned int *cur, curword;
    static int bitPackInWord;

    unsigned int nbits_req_container;
    unsigned int nbits_needed, nbits2, token;

    if (start == 1) {
        bitPackInWord = 32;

        memset(zc, 0, nicoarse * njcoarse * sizeof(int));
        cur = z;
        memcpy(header, cur, sizeof(unsigned int));
        cur++;
        curword = *cur;
    }
    memset(diffs, 0, ni*nj*sizeof(int));

    int k;
    if (start == 1) {
        for (int j = 1; j <= njcoarse; j++) {
            for (int i = 1; i <= nicoarse; i++) {
                k = FTN2C(i, j, nicoarse);
                extract(zc[k], cur, 32, nbits, curword, bitPackInWord);
            }
        }
    }

    extract(nbits_req_container, cur, 32, 3, curword, bitPackInWord);

    for (int j = 1; j <= nj; j+=step) {
        int lcl_n = ((j + step - 1) >= nj ? nj - j : step - 1);
        for (int i = 1; i <= ni; i+=step) {
            int lcl_m = ((i + step - 1) >= ni ? ni - i : step - 1);
            extract(nbits_needed, cur, 32, nbits_req_container, curword, bitPackInWord);
            switch (nbits_needed) {
                case 0:
                    for (int n = 0; n <= lcl_n; n++) {
                        for (int m = 0; m <= lcl_m; m++) {
                            k = FTN2C(i+m,j+n,ni);
                            diffs[k] = 0;
                        }
                    }
                    break;

                case 15:
                case 16:
                    for (int n = 0; n <= lcl_n; n++) {
                        for (int m = 0; m <= lcl_m; m++) {
                            if (!(m == 0 && n == 0)) {
                                k = FTN2C(i+m,j+n,ni);
                                extract(token, cur, 32, 17, curword, bitPackInWord);
                                diffs[k] = token;
                                diffs[k] = (diffs[k] << 15) >> 15;
                            }
                        }
                    }
                    break;

                default:
                    nbits2 = nbits_needed + 1;
                    for (int n=0; n <= lcl_n; n++) {
                        for (int m=0; m <= lcl_m; m++) {
                            if (!(m == 0 && n == 0)) {
                                k = FTN2C(i+m,j+n,ni);
                                extract(token, cur, 32, nbits2, curword, bitPackInWord);
                                diffs[k] = token;
                                diffs[k] = (diffs[k] << (32-nbits2)) >> (32-nbits2);
                            }
                        }
                    }
                    break;
            }
        }
    }
}

//! Compute how many points are leftover on the east/north sides of the grid
void calcul_ajusxy(
    //! [out] Number of leftover points along i
    int *ajus_x,
    //! [out] Number of leftover points along j
    int *ajus_y,
    //! [in] Size of the first dimension
    int ni,
    //! [in] Size of the second dimension
    int nj,
    //! [in]
    int istep
) {
    *ajus_x = (ni - 1) % istep;
    *ajus_y = (nj - 1) % istep;
}



//! Compute the dimensions of the coarse grid
//!
//! A correction factor is used to compensate for the grid size when the last point of the source grid is not a multiple of step
void calcul_ninjcoarse(
    int *nicoarse,
    int *njcoarse,
    int ni,
    int nj,
    int ajus_x,
    int ajus_y,
    int istep
) {
    int correction_x = 0;
    int correction_y = 0;

    if (ajus_x != 0) {
        correction_x = 1;
    }

    if (ajus_y != 0) {
        correction_y = 1;
    }

    if (ni > 1 && nj > 1) {
        *nicoarse = correction_x + (ni + istep - 1) / istep;
        *njcoarse = correction_y + (nj + istep - 1) / istep;
    } else {
        if (ni == 1) {
            *nicoarse = 1;
            *njcoarse = correction_y + (nj + istep - 1) / istep;
        } else {
            *nicoarse = correction_x + (ni + istep - 1) / istep;
            *njcoarse = 1;
        }
    }
}


void calcule_entropie(
    float *entropie,
    unsigned short *bitstream,
    int npts,
    int nbits
) {
    int lbin, sizebins;
    int imin, imax, nbits_local;
    float prob, range;
    unsigned int *bins;

    *entropie = 0.0;
    imin = bitstream[0];
    imax = imin;
    for (int i = 1; i < npts; i++) {
        if (bitstream[i] < imin) imin = bitstream[i];
        if (bitstream[i] > imax) imax = bitstream[i];
    }

    range = (float)(imax - imin);
    nbits_local = 1 + (int)(log(1.0 * (imax - imin)) / log(2.0));

    sizebins = 1 << nbits_local;
    bins = (unsigned int *) calloc(sizebins,sizeof(unsigned int));

    for (int i = 0; i < npts; i++) {
        lbin = bitstream[i] - imin;
        bins[lbin]++;
    }

    for (int i = 0; i < (int)(range); i++) {
        if (bins[i] != 0) {
            prob = (float)bins[i] / (float)(npts);
            *entropie += (prob * log(prob) / log(2.0));
        }
    }

    *entropie = -1.0 * (*entropie);
    free(bins);
}


void f77name(armn_compress_setlevel)(int32_t *level)
{
    int local_level;

    local_level = *level;

    c_armn_compress_setlevel(local_level);
}


void c_armn_compress_setlevel(int level)
{
  switch(level)
    {
    case BEST:
       fstcompression_level = BEST;
       Lib_Log(APP_LIBFST,APP_INFO,"%f: Setting level to BEST (%d)\n",__func__,level);
       break;

    case FAST:
       fstcompression_level = FAST;
       Lib_Log(APP_LIBFST,APP_INFO,"%f: Setting level to FAST (%d)\n",__func__,level);
       break;

    default:
       Lib_Log(APP_LIBFST,APP_ERROR,"%f: Wrong compression level (%d), reverting to FAST\n",__func__,level);
       fstcompression_level = FAST;
    }
}


int f77name(armn_compress_getlevel)()
{
    return c_armn_compress_getlevel();
}


int c_armn_compress_getlevel()
{
    return fstcompression_level;
}


void f77name(armn_compress_setswap)(int32_t *swap)
{
    int local_swap;

    local_swap = *swap;

    c_armn_compress_setlevel(local_swap);
}


void c_armn_compress_setswap(int swapState)
{
    switch(swapState) {
        case 0:
        case 1:
            swapStream = swapState;
            break;

        default:
            Lib_Log(APP_LIBFST,APP_ERROR,"%f: Wrong swapState (%d), should be 0 (no swap) or 1 (swap)\n",__func__,swapState);
    }
}


int f77name(armn_compress_getswap)()
{
    return c_armn_compress_getswap();
}


int c_armn_compress_getswap()
{
    return swapStream;
}
