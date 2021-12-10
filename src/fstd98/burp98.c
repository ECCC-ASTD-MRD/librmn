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

#include <stdio.h>
#ifdef WIN32    /*CHC/NRC*/
#include <string.h>
#else
#include <unistd.h>
#endif
#include <stdlib.h>
#include <burp.h>
#include "qstdir.h"
#include "proto.h"

int32_t f77name(mrfbfl)(int32_t *iun);

int BurP_nele;
int BurP_ntot;

void c_buf89a0(uint32_t *buffer)
{
    buffer_interface_ptr buf = (buffer_interface_ptr) buffer;

    buf->buf78.buf8 = 0;
    buf->buf9 = 0;
}


int c_getbuf8(uint32_t *buffer)
{
    buffer_interface_ptr buf = (buffer_interface_ptr) buffer;

    return buf->buf78.buf8;
}


//! Pack burp info keys into buffer or get info keys from buffer depending on mode argument
void build_burp_info_keys(
    //! [in,out]  Buffer to contain the keys
    uint32_t *buf,
    //! [in,out] Info keys
    uint32_t *keys,
    //! [in] File index in file table
    int index,
    //! [in] Write to buffer when WMODE, otherwise get keys from buffer
    int mode
) {
    burp_dir_info *info;

    info = (burp_dir_info *) buf;

    if (mode == WMODE) {
        /* write info keys to burp record */
        if (keys[0] != -1) info->nblks = keys[0];
        if (keys[1] != -1) info->oars  = keys[1];
        if (keys[2] != -1) info->elev  = keys[2];
        if (keys[3] != -1) info->drcv  = keys[3];
        if (keys[4] != -1) info->runn  = keys[4];
    } else {
        /* read info keys from burp record */
        keys[0] = info->nblks;
        keys[1] = info->oars;
        keys[2] = info->elev;
        keys[3] = info->drcv;
        keys[4] = info->runn;
    }
}



//! Pack burp primary keys into buffer or get primary keys from buffer depending on mode argument
void build_burp_prim_keys(
    //! [in,out] Buffer to contain the keys
    burp_record *brpk,
    //! [in,out] Primary keys
    uint32_t *keys,
    //! [out] Search mask
    burp_record *mask,
    //! [in] Unpacked masks
    uint32_t *mskkeys,
    //! [in] File index in file table
    int index,
    //! [in] Write to buffer when WMODE, otherwise get keys from buffer
    int mode
) {
    int *mskptr;
    int i;

    mskptr = (int *)mask;

   if (mode == WMODE) {
        // Write keys to burp record
        // Initialize compress keys to 0 and bit masks to all ones

        for (i = 0; i < sizeof(burp_dir_keys) / sizeof(int); i++) {
            *mskptr = -1;
            mskptr++;
        }

        mask->keys.idtyp = 0;
        mask->keys.lng = 0;
        mask->keys.addr = 0;

        if (keys[0] == -1) {
            mask->keys.sti1 = 0;
        } else {
            brpk->keys.sti1 = upper_case(keys[0]);
        }
        if (keys[1] == -1) {
            mask->keys.sti2 = 0;
        } else {
            brpk->keys.sti2 = upper_case(keys[1]);
        }
        if (keys[2] == -1) {
            mask->keys.sti3 = 0;
        } else {
            brpk->keys.sti3 = upper_case(keys[2]);
        }
        if (keys[3] == -1) {
            mask->keys.sti4 = 0;
        } else {
            brpk->keys.sti4 = upper_case(keys[3]);
        }
        if (keys[4] == -1) {
            mask->keys.sti5 = 0;
        } else {
            brpk->keys.sti5 = upper_case(keys[4]);
        }
        if (keys[5] == -1) {
            mask->keys.sti6 = 0;
        } else {
            brpk->keys.sti6 = upper_case(keys[5]);
        }
        if (keys[6] == -1) {
            mask->keys.sti7 = 0;
        } else {
            brpk->keys.sti7 = upper_case(keys[6]);
        }
        if (keys[7] == -1) {
            mask->keys.sti8 = 0;
        } else {
            brpk->keys.sti8 = upper_case(keys[7]);
        }
        if (keys[8] == -1) {
            mask->keys.sti9 = 0;
        } else {
            brpk->keys.sti9 = upper_case(keys[8]);
        }
        if (keys[9] == -1) {
            mask->keys.flgs = 0;
        } else {
            brpk->keys.flgs = keys[9];
        }
        if (keys[10] == -1) {
            mask->keys.lati = 0;
        } else {
            brpk->keys.lati = keys[10];
        }
        if (keys[11] == -1) {
            mask->keys.lon = 0;
        } else {
            brpk->keys.lon = keys[11];
        }
        if (keys[12] == -1) {
            mask->keys.date = 0;
        } else {
            brpk->keys.date = keys[12];
        }
        if (keys[13] == -1) {
            mask->keys.dx = 0;
        } else {
            brpk->keys.dx = keys[13];
        }
        if (keys[14] == -1) {
            mask->keys.idtp = 0;
        } else {
            brpk->keys.idtp = keys[14];
        }
        if (keys[15] == -1) {
            mask->keys.dy = 0;
        } else {
            brpk->keys.dy = keys[15];
        }
        if (keys[16] == -1) {
            mask->keys.heur = 0;
        } else {
            brpk->keys.heur = keys[16];
        }
        if (keys[17] == -1) {
            mask->keys.min = 0;
        } else {
            brpk->keys.min = keys[17];
        }
    } else {
        // Read keys from burp record
        keys[0] = brpk->keys.sti1;
        keys[1] = brpk->keys.sti2;
        keys[2] = brpk->keys.sti3;
        keys[3] = brpk->keys.sti4;
        keys[4] = brpk->keys.sti5;
        keys[5] = brpk->keys.sti6;
        keys[6] = brpk->keys.sti7;
        keys[7] = brpk->keys.sti8;
        keys[8] = brpk->keys.sti9;
        keys[9] = brpk->keys.flgs;
        keys[10] = brpk->keys.lati;
        keys[11] = brpk->keys.lon;
        keys[12] = brpk->keys.date;
        keys[13] = brpk->keys.dx;
        keys[14] = brpk->keys.idtp;
        keys[15] = brpk->keys.dy;
        keys[16] = brpk->keys.heur;
        keys[17] = brpk->keys.min;
    }
}


//! Check burp file for corruption
//! @return 0 when valid; -1 otherwise
int c_burpcheck(
    //! Path of the file
    const char *filePath
) {
   return c_xdfcheck(filePath);
}



//! Determine the number of bits and the proper datyp needed to insert tblval into a data block
static int burp_nbit_datyp(
    //! [in,out] Number of bits
    int *nbits,
    //! [in,out] Data type of tblval
    int *datyp,
    //! [in] Tblval  array of values to be inserted subsequently
    uint32_t *tval,
    //! [in] Tblval dimension
    int tbldim,
    //! [in] Stride to use within tblval
    int stride
) {
    int i, needed;
    int inbits = *nbits;
    int32_t *tblval = (int32_t *) tval;
    int32_t temp;
    int32_t tblmax;
    int32_t tblmin;
    int v_nbits = inbits;
    int v_datyp = *datyp;

    /* if transparent mode, nomodifications */
    if (((*datyp==2) && (*nbits == 32)) || (datyp == 0)) return 0;

#if defined(NEC)
    /* check if datyp is greater than 5 --> not allowed on the NEC */
    if (*datyp > 5) {
        sprintf(errmsg, "datyp 6,7,8 or 9 is not allowed on the NEC");
        return error_msg("burp_nbit_datyp", ERR_BAD_DATYP, ERRFATAL);
    }
#endif

    /* if datyp > 5 (real, real*8, complex, complex*8) nbits is set to 32 */
    if (*datyp > 5) {
        *nbits = 32;
        return 0;
    }

    /* if datyp = character type, nbits is set to 8 */
    if ((*datyp == 3) || (*datyp == 5)) {
        *nbits = 8;
        return 0;
    }

    /* find min and max */
    tblmin = tblval[0];
    tblmax = tblval[0];
    for (i = 0; i < tbldim * stride; i += stride) {
        tblmax = Max(tblval[i], tblmax);
        tblmin = Min(tblval[i], tblmin);
    }

    if ((tblmin == tblmax) && (tblmin == -1)) return 0;

    /* check if signed integer is required and set max to ABS value */
    if (tblmin < -1) {
        tblmax = (tblmax > (-tblmin)) ? tblmax : (-tblmin);
        *datyp = 4;
    }

    /* determine the number of bits needed to represent the maximum value. */
    /* If there are missing values (-1) one additional bit is needed */
    temp = tblmax;
    needed = 0;
    while ((temp) && (needed < 32)) {
        needed++;
        temp >>=1;
    }
    if (tblmax == (~(-1 << needed))) needed++;

    *nbits = needed;
    if (*datyp == 4) {
        (*nbits)++;
        if (*nbits > 31) {
        *nbits = 32;
        *datyp = 2;
        sprintf(errmsg, "encoding values < 0 with nbit=32 and datyp=2");
        return error_msg("burp_nbit_datyp", BURP_ERR_CMPR, WARNING);
        }
    }
    *nbits = (*nbits > 32) ? 32 : *nbits;
    *nbits = (inbits > *nbits) ? inbits : *nbits;

    return 0;
}



//! Traitement de la liste d'elements pour les data de type 7, 8 et 9
/*!
    7 = real*8
    8 = complex
    9 = complex*8

    Selon le datyp, on s'assure que le nombre d'elements est bon
    et que la valeur des elements est adequate.  Si la valeur d'un
    element est zero, on lui attribut la bonne valeur.  Les elements
    verifies sont les suivants:

    055204  32 Bits inferieurs d'un reel de 8 octets
    055205  Imaginaire d'un complexe de 4 octets
    055206  32 Bits superieurs, imaginaire complexe de 8 octets
    055207  32 Bits inferieurs, imaginaire complexe de 8 octets

    Exemple:  temperature du point de rose en reel*8  (datyp = 7)
    code burp 012003  = temperature du point de rose
    055204  = 32 bits du bas d'un reel*8

    Si la liste d'elements contient 2 elements soit:
    elem1     elem2
    012003    055204           --> O.K.
    012003         0           --> remplace 0 par 055204
    012003    !=055204 | !=0   --> erreur

    Si mod(nele,2) != 0        --> erreur
*/
static int burp_valid789(
    //! [in,out] List of elements
    uint32_t *lstele,
    //! [in] Number of elements in list
    int nele,
    //! [in] Data type
    int datyp)
{
    int i, j, codval[3];

    switch (datyp) {
        case 7:
        case 8:
            if ((nele &1) != 0) {
                sprintf(errmsg, "datyp=%d, nele must be even, nele=%d", datyp, nele);
                return error_msg("burp_valid789", ERR_BAD_DATYP, ERRFATAL);
            }
            if (datyp == 7) {
                codval[0] = MRBCOV(0, 55, 204);
            } else {
                codval[0] = MRBCOV(0, 55, 205);
            }
            for (i = 1; i < nele; i += 2) {
                if (lstele[i] != codval[0]) {
                    if (lstele[i] == 0) {
                        lstele[i] = codval[0];
                    } else {
                        sprintf(errmsg, "invalid code for datyp %d", datyp);
                        return error_msg("burp_valid789", BURP_ERR_CODE, ERRFATAL);
                    }
                }
            }
            break;

        case 9:
            if ((nele & 3) != 0) {
                sprintf(errmsg, "datyp=%d, nele must be a multiple of 4, nele=%d", datyp, nele);
                return error_msg("burp_valid789", ERR_BAD_DATYP, ERRFATAL);
            }
            codval[0] = MRBCOV(0, 55, 204);
            codval[1] = MRBCOV(0, 55, 206);
            codval[2] = MRBCOV(0, 55, 207);
            for (i = 1; i < nele; i += 4) {
                for (j = 0; j < 3; j++) {
                    if (lstele[i+j] != codval[j]) {
                        if (lstele[i+j] == 0) {
                            lstele[i+j] = codval[j];
                        } else {
                            sprintf(errmsg, "invalid code for datyp %d", datyp);
                            return error_msg("burp_valid789", BURP_ERR_CODE, ERRFATAL);
                        }
                    }
                }
            }
            break;
    }
    return 0;
}


//! Add a data block at the end of the report
int c_mrbadd(
    //! [in,out] Buffer vector to contain the report
    void *buffer,
    //! [out] Number of blocks in buf
    int *bkno,
    //! [in] Number of meteorogical elements in block
    int nele,
    //! [in] Number of data per elements
    int nval,
    //! [in] Number of group of nele*nval values in block
    int nt,
    //! [in] Block family (12 bits, bdesc no more used)
    int bfam,
    //! [in] Kept for backward compatibility
    int bdesc,
    //! [in] Block type
    int btyp,
    //! [in] Number of bit to keep per values
    int nbit,
    // [out] Position of first bit of the report
    int *bit0,
    //! [in] Data type for packing
    int datyp,
    //! [in] List of nele meteorogical elements
    uint32_t *lstele,
    //! [in] Array of values to write (nele*nval*nt)
    uint32_t *tblval
) {
    burp_block_header entete;
    buffer_interface_ptr buf = (buffer_interface_ptr) buffer;
    uint32_t *pos;
    int inbit, idatyp, nombre, err, temp, left, i, bits_added, bfamho;
    int done,indx;
    uint32_t r_nele, r_nval, r_nt, r_bfam, r_bdesc;
    uint32_t r_btyp, r_nbit, r_bit0, r_datyp;

    /* initialize block header to 0 */
    pos = (uint32_t *)&entete;
    for (i = 0; i < (sizeof(entete) / sizeof(uint32_t)); i++) {
        *pos = 0;
    }

#if defined(NEC)
    if (datyp > 5) {
        sprintf(errmsg, "datyp 6,7,8 or 9 not allowed on the NEC");
        return error_msg("c_mrbadd", ERR_BAD_DATYP, ERRFATAL);
    }
#endif

    if (((datyp == 3) || (datyp == 5)) && (nbit != 8)) {
        sprintf(errmsg, "nbits must be 8 for datyp 3 or 5");
        return error_msg("c_mrbadd", ERR_BAD_DATYP, ERROR);
    }

    /* validate (possibly modify) nbit and datyp */
    inbit = nbit;
    idatyp = datyp;
    nombre = nele * nval * nt;
    err = burp_nbit_datyp(&inbit, &idatyp, tblval, nombre, xdf_stride);
    if (err < 0) return err;

    /* validates lstele for datyp 7,8 or 9 */
    if (datyp >= 7) {
        err = burp_valid789(lstele, nele, datyp);
        if (err < 0) return err;
    }

    if (bdesc != 0) {
        bfamho = (bfam >> 6) & 0x3f;
        if ((bfamho != 0) && (bfamho != bdesc)) {
            sprintf(errmsg, "illegal use of bdesc");
            return error_msg("c_mrbadd", BURP_ERR_BDESC, ERRFATAL);
        }
        entete.bfamdesc = (bfam & 0x3f) << 6;
        entete.bfamdesc |= (bdesc & 0x3f);
    } else {
        entete.bfamdesc = (bfam & 0x3f) << 6;
        entete.bfamdesc |= ((bfam >> 6) & 0x3f);
    }

    entete.btyp = btyp;
    entete.nbit = inbit-1;
    entete.datyp = idatyp;
    *bit0 = (buf->nbits - NBENTR - buf->buf9) / 64;
    *bit0 = (*bit0 < 0) ? 0 : *bit0;
    entete.bit0 = *bit0;
    if ((nele >= GROSNELE) || (nval >= GROSDIM) || (nt >= GROSDIM)) {
        entete.flag = 1;
        entete.nele = GROSNELE;
        entete.elem1 = nele;
        entete.elem2 = nval;
        entete.elem3 = nt;
        done = 0;
    } else {
        entete.flag = 0;
        entete.nt = nt;
        entete.nele = nele;
        entete.nval = nval;
        entete.elem1 = lstele[0];
        entete.elem2 = lstele[1];
        entete.elem3 = lstele[2];
        done = 3;
    }
    indx = (buf->nbits) / (8 * sizeof(uint32_t));
    pos = (uint32_t *) &(buf->data[indx]);
    *pos = 0;
    temp = 0;
    left = 8 * sizeof(uint32_t);
    bits_added = 0;

    i = done;
    while (i < nele) {
        *pos = (*pos << (32-left)) | (lstele[i] & 0xffff);
        bits_added += 16;
        left -= 16;
        if (left == 0) {
            pos++;
            *pos = 0;
            left = 8*sizeof(uint32_t);
        }
        i++;
    }
    if (left != 8 * sizeof(uint32_t)) {
        *pos <<= 16;
        bits_added += 16;
    }

    buf->nbits += (bits_added +63) / 64 * 64;
    err = c_xdfins((uint32_t *)buf, (uint32_t *)&entete, buf->buf9, DIMENT,
        8 * sizeof(uint32_t), 0);
    if (err < 0) return err;

    err = c_xdfadd((uint32_t *)buf, tblval, nombre, inbit, idatyp);
    if (err < 0) return err;

    buf->buf78.buf8++;
    buf->buf9 += NBENTB;
    *bkno = buf->buf78.buf8;

    if (msg_level <= INFORM) {
        err = c_mrbprm((uint32_t *)buf, *bkno, &r_nele, &r_nval, &r_nt, &r_bfam, &r_bdesc,
            &r_btyp, &r_nbit, &r_bit0, &r_datyp);
        fprintf(stdout, "MRBADD - write block #%5d NELE=%5d NVAL=%5d NT=%5d BFAM=%4d BTYP=%4d NBITS=%2d BIT0=%8d DATYP=%1d\n",
            *bkno, r_nele, r_nval, r_nt, r_bfam, r_btyp, r_nbit, r_bit0, r_datyp);
      }
    return 0;
}


//! Delete a particular block of the report.
int c_mrbdel(
    //! [in,out] Vector to contain the report
    void *buffer,
    //! [in] Block number to be deleted
    int number
) {
    buffer_interface_ptr buf = ( buffer_interface_ptr) buffer;
    int err, bitpos, bit0, prebit, difbit, nele, nval, nt, nbit;
    int nelements, datyp, nombre, i;
    burp_block_header entete, *block;
    burp_record *burprec;
    uint32_t *pos;

    if ((number < 1) || (number > buf->buf78.buf8)) {
        sprintf(errmsg, "invalid block number");
        return error_msg("c_mrbdel", BURP_ERR_BNUM, ERROR);
    }

   bitpos = NBENTB * (number -1);
   err = c_xdfxtr((uint32_t *)buf,(uint32_t *)&entete,bitpos,DIMENT,8*sizeof(uint32_t),0);
   if (err < 0) return err;

    if (entete.flag != 0) {
        nele = entete.elem1;
        nval = entete.elem2;
        nt = entete.elem3;
        nelements = nele;
    } else {
        nele = entete.nele;
        nval = entete.nval;
        nt = entete.nt;
        nelements = ((nele-3) > 0) ? nele-3 : 0;
    }
    nbit = entete.nbit + 1;
    datyp = entete.datyp;
    bit0 = entete.bit0;

    prebit = bit0;
    bit0 = bit0 * 64 + buf->buf9;

    /* cut the meteorogical element list */
    if (nelements > 0) err = c_xdfcut(buf, bit0, nelements, 16, 2);

    /* cut the value array */
    nombre = nele * nval * nt;
    err = c_xdfcut(buf, bit0, nombre, nbit, datyp);

    /* cut block header */
    err = c_xdfcut(buf, bitpos, DIMENT, 8 * sizeof(uint32_t), 0);

    /* update number of blocks */
    buf->buf78.buf8--;

    /* update bit position of block */
    buf->buf9 -= NBENTB;

    /* update bit0 for each block */
    burprec = (burp_record *) buf->data;
    block = (burp_block_header *) burprec->data;
    bitpos = block[number-1].bit0;
    difbit = bitpos - prebit;

    for (i = number - 1; i < buf->buf78.buf8; i++) {
        block[i].bit0 -= difbit;
    }

    return 0;
}


//! Get the description parameters of a data block
int c_mrbhdr(
    //! [in] Buffer containing the report
    uint32_t *buf,
    int *temps,
    int *flgs,
    char *stnid,
    int *idtyp,
    int *lati,
    int *lon,
    int *dx,
    int *dy,
    int *elev,
    int *drcv,
    int *date,
    int *oars,
    int *run,
    //! [in] Block number
    int *nblk,
    uint32_t *sup,
    int nsup,
    uint32_t *xaux,
    int nxaux
) {
    buffer_interface_ptr buffer = (buffer_interface_ptr)buf;
    burp_record *burprec;
    int AA;
    int MM;
    int JJ;
    int annee;
    int mois;

    if (nsup > NPRISUP) {
        sprintf(errmsg, "there is too many supplementary prim keys");
        error_msg("c_mrbhdr", BURP_ERR_CLEF, WARNING);
    }
    if (nxaux > NAUXSUP) {
        sprintf(errmsg, "there is too many supplementary aux keys");
        error_msg("c_mrbhdr", BURP_ERR_CLEF, WARNING);
    }

    burprec = (burp_record *) buffer->data;
    stnid[0] = burprec->keys.sti1;
    stnid[1] = burprec->keys.sti2;
    stnid[2] = burprec->keys.sti3;
    stnid[3] = burprec->keys.sti4;
    stnid[4] = burprec->keys.sti5;
    stnid[5] = burprec->keys.sti6;
    stnid[6] = burprec->keys.sti7;
    stnid[7] = burprec->keys.sti8;
    stnid[8] = burprec->keys.sti9;
    *temps = burprec->keys.heur * 100 + burprec->keys.min;
    *flgs = burprec->keys.flgs;
    *idtyp = burprec->keys.idtp;
    *lati = burprec->keys.lati;
    *lon = burprec->keys.lon;
    *dx = burprec->keys.dx;
    *dy = burprec->keys.dy;
    *elev = burprec->info.elev;
    *drcv = burprec->info.drcv;
    *oars = burprec->info.oars;
    *run = burprec->info.runn;
    *nblk = buffer->buf78.buf8;
    *date = burprec->keys.date;
    if ((((*date/100) % 100) > 12) || (xdf_enforc8)) {
        // Retourner la date en format AAAAMMJJ
        AA = (*date/10000) % 100;
        MM = (*date/100) % 100;
        JJ = *date % 100;
        annee = 1900 + AA + (((MM-1)/12)*100);
        mois = 1 + ((MM-1) % 12);
        *date = (annee * 10000) + (mois * 100) + JJ;
    }

    return 0;
}


//! Get the number of bits used in buf and the number of bits left
//! \return Always 0
int c_mrblen(
    // [in] Buffer containing the report
    void *buffer,
    //! [out] Number of bits used
    int *bitsUsed,
    //! [out] Number of bits left
    int *bitsLeft
) {
   buffer_interface_ptr buf = (buffer_interface_ptr) buffer;

   *bitsUsed = buf->nbits;
   *bitsLeft = ((buf->nwords - 9) * 8 * sizeof(uint32_t)) - *bitsUsed;
   return 0;
}



//! Search for a specific block in the buffer
//! \return Index of the block if found, -1 otherwise
int c_mrbloc(
    //! [in] Buffer containing the report
    void *buffer,
    //! [in] Block family (12 bits, bdesc no more used)
    int bfam,
    //! [in] Kept for backward compatibility
    int bdesc,
    //! [in] Block type
    int btyp,
    //! [in] Index of the block from which to start the search (0 to start from the beginning)
    int blkno
) {
    buffer_interface_ptr buf = (buffer_interface_ptr) buffer;
    int i, bfamho, bfamdesc, err;
    int match, mskbtyp, mskfamdesc;
    int r_nele, r_nval, r_nt, r_bfam, r_bdesc, r_btyp, r_nbit, r_bit0, r_datyp;
    int bno;
    burp_block_header *block;
    burp_record *burprec;

    if (bfam == -1) {
        if ((bdesc == 0) || (bdesc == -1)) {
            bfamdesc = -1;
        } else {
            sprintf(errmsg, "illegal use of bdesc");
            return error_msg("c_mrbloc", BURP_ERR_BDESC, ERRFATAL);
        }
    } else if ((bdesc != 0) && (bdesc != -1)) {
        bfamho = (bfam >> 6) & 0x3f;
        if ((bfamho != 0) && (bfamho != bdesc)) {
            sprintf(errmsg, "illegal use of bdesc");
            return error_msg("c_mrbloc", BURP_ERR_BDESC, ERRFATAL);
        }
        bfamdesc = (bfam & 0x3f) << 6;
        bfamdesc |= (bdesc & 0x3f);
    } else {
        bfamdesc = (bfam & 0x3f) << 6;
        bfamdesc |= ((bfam >> 6) & 0x3f);
    }

    if (bfamdesc == -1) {
        mskfamdesc = 0;
    } else {
        mskfamdesc = -1;
    }

    if (btyp == -1) {
        mskbtyp = 0;
    } else {
        mskbtyp = 0;
        mskbtyp |= (0x10000000 & btyp) ? 0 : 0x000f;
        mskbtyp |= (0x20000000 & btyp) ? 0 : 0x07f0;
        mskbtyp |= (0x40000000 & btyp) ? 0 : 0x7800;
    }

    burprec = (burp_record *) buf->data;
    block = (burp_block_header *) burprec->data;
    match = 0;
    for (i = blkno; i < buf->buf78.buf8; i++) {
        match = ((((bfamdesc ^ block[i].bfamdesc) & mskfamdesc) == 0) &&
                (((btyp ^ block[i].btyp) & mskbtyp) == 0));
        if (match) {
            if (msg_level <= INFORM) {
                bno = i + 1;
                err = c_mrbprm(buf, bno, &r_nele, &r_nval, &r_nt,
                        &r_bfam, &r_bdesc, &r_btyp, &r_nbit,
                        &r_bit0, &r_datyp);
                fprintf(stdout, "MRBLOC - find block #%5d NELE=%5d NVAL=%5d NT=%5d BFAM=%4d BTYP=%4d NBITS=%2d BIT0=%8d DATYP=%1d\n",
                    i + 1, r_nele, r_nval, r_nt, r_bfam, r_btyp, r_nbit, r_bit0, r_datyp);
            }
            return i + 1;
        }
    }
    if (msg_level <= INFORM) {
        fprintf(stdout, "MRBLOC - block not found bfam=%d, bdesc=%d, btyp=%d\n",
            bfam, bdesc, btyp);
    }
    return -1;
}



//! Get the description parameters of a data block
//! \return 0 on success, error code otherwise
int c_mrbprm(
    //! [in] Buffer containing the report
    uint32_t *buf,
    //! [in] Index of the block from which to get the parameters
    int  bkno,
    //! [out] Number of elements
    int *nele,
    //! [out] Number of values per element
    int *nval,
    //! [out] Number of nele * nval values
    int *nt,
    //! [out] Block family type (12 bits)
    int *bfam,
    //! [out] Block descriptor (set to zero)
    int *bdesc,
    //! [out] Block type
    int *btyp,
    //! [out] Number of bits kept per value
    int *nbit,
    //! [out] First bit of array values
    int *bit0,
    //! [out] Data compaction type
    int *datyp
) {
    burp_block_header header;
    int ier, bitpos;

    bitpos = (bkno - 1) * NBENTB;
    ier = c_xdfxtr(buf, (uint32_t *)&header, bitpos, DIMENT, 32, 0);

    *btyp = header.btyp;
    *nbit = header.nbit + 1;
    *bit0 = header.bit0;
    *datyp = header.datyp;
    *nele = header.nele;

    if (header.flag) {
        *nele = header.elem1;
        *nval = header.elem2;
        *nt = header.elem3;
    } else {
        *nval = header.nval;
        *nt = header.nt;
    }

    *bfam = (header.bfamdesc & 0x3f) << 6;
    *bfam |= ((header.bfamdesc >> 6) & 0x3f);
    *bdesc = 0;
    return ier;
}


//! Replace a data block by an other one with the same variables and dimensions
int c_mrbrep(
    //! [in,out] Buffer containing the report
    void *buffer,
    //! [in] Index of the block to be replaced
    int blkno,
    //! [in] Array of values to write (nele * nval * nt)
    uint32_t *tblval
) {
    burp_block_header *block;
    buffer_interface_ptr buf = (buffer_interface_ptr) buffer;
    int nele, nval, nt, nelements;
    int nbit, bit0, datyp, bitpos, err, done, i, nnbits;
    int nmots, new_nmots, diff_nmots;
    int new_nbit, new_datyp, nombre;
    burp_record *burprec;

    if ((blkno < 1) || (blkno > buf->buf78.buf8)) {
        sprintf(errmsg, "invalid block number");
        return error_msg("c_mrbrep", BURP_ERR_BNUM, ERROR);
    }

    burprec = (burp_record *) buf->data;
    block = (burp_block_header *) burprec->data;

    if (block[blkno - 1].flag != 0) {
        nele = block[blkno - 1].elem1;
        nval = block[blkno - 1].elem2;
        nt = block[blkno - 1].elem3;
        nelements = nele;
        done = 0;
    } else {
        nele = block[blkno-1].nele;
        nval = block[blkno-1].nval;
        nt = block[blkno-1].nt;
        nelements = ((nele-3) > 0) ? nele-3 : 0;
        done = (nele < 3) ? nele : 3;
    }
    nbit = block[blkno-1].nbit + 1;
    datyp = block[blkno-1].datyp;
    bit0 = block[blkno-1].bit0;

#if defined(NEC)
    if (datyp > 6) {
        sprintf(errmsg, "datyp 6,7,8 or 9 not allowed on the NEC");
        return error_msg("c_mrbrep", ERR_BAD_DATYP, ERRFATAL);
    }
#endif

    bitpos = bit0 * 64 + buf->buf9;

    nnbits = (((nele - done) * 16 + 63) / 64) * 64;
    bitpos = bitpos + nnbits;
    nombre = nele * nval * nt;
    new_nbit = nbit;
    new_datyp = datyp;

    /* check if precision should be ajusted */
    err = burp_nbit_datyp(&new_nbit, &new_datyp, tblval, nombre, xdf_stride);
    if (err < 0) return err;

    if (new_datyp != datyp)
        block[blkno - 1].datyp = new_datyp;

    if (new_nbit != nbit) {
        block[blkno - 1].nbit = new_nbit - 1;
        nmots = (nbit * nombre + 63) / 64;
        new_nmots = (new_nbit * nombre + 63) / 64;
        diff_nmots = new_nmots - nmots;
        /* insertion by 32 bit slices */
        new_nmots = diff_nmots * 2;
        if (new_nmots > 0) {
            err = c_xdfins((uint32_t *)buf, tblval, bitpos, new_nmots, 32, 0);
            for (i = blkno; i < buf->buf78.buf8; i++) {
                block[i].bit0 += diff_nmots;
            }
        }
    }

    /* replace block */
    err = c_xdfrep((uint32_t *)buf, tblval, bitpos, nombre, new_nbit, new_datyp);
    return err;
}


//! Extract the list of elements and values from a report
//! \return 0 on success, error code otherwise
int c_mrbxtr(
    //! [in] Buffer containing the report
    void *buffer,
    //! [in] Number of blocks in buf
    int bkno,
    //! [out] List of nele meteorogical elements
    uint32_t *lstele,
    //! [out] Array of values to write (nele * nval * nt)
    uint32_t *tblval
) {
    burp_block_header entete;
    buffer_interface_ptr buf = (buffer_interface_ptr) buffer;
    int nele, nval, nt, nelements;
    int nbit, bit0, datyp, bitpos, err, done, i, rmask, nnbits, nombre, allones;
    int in_header;
    uint32_t *pos;

    err = 0;
    if ((bkno < 1) || (bkno > buf->buf78.buf8)) {
        sprintf(errmsg, "invalid block number");
        return error_msg("c_mrbxtr", BURP_ERR_BNUM, ERROR);
    }

    bitpos = NBENTB * (bkno - 1);
    err = c_xdfxtr((uint32_t *)buf, (uint32_t *)&entete, bitpos, DIMENT, 8 * sizeof(uint32_t), 0);
    if (err < 0) return err;

    if (entete.flag != 0) {
        nele = entete.elem1;
        nval = entete.elem2;
        nt = entete.elem3;
        nelements = nele;
        done = 0;
    } else {
        nele = entete.nele;
        nval = entete.nval;
        nt = entete.nt;
        in_header = (nele < 3) ? nele : 3;
        nelements = ((nele-in_header) > 0) ? nele-in_header : 0;
        if (in_header > 0) lstele[0] = entete.elem1;
        if (in_header > 1) lstele[1] = entete.elem2;
        if (in_header > 2) lstele[2] = entete.elem3;
        lstele += in_header;
        done = (nele < in_header) ? nele : in_header;
    }
    BurP_nele = nele;
    BurP_ntot = nele * nval * nt;
    nbit = entete.nbit + 1;
    datyp = entete.datyp;
    bit0 = entete.bit0;

#if defined(NEC)
    if (datyp > 6) {
        sprintf(errmsg, "datyp 6,7,8 or 9 not allowed on the NEC");
        return error_msg("c_mrbxtr", ERR_BAD_DATYP, ERRFATAL);
    }
#endif

    bitpos = bit0 * 64 + buf->buf9;

    /* extract list of elements */
    if (nelements > 0) {
        err = c_xdfxtr((uint32_t *)buf, lstele, bitpos, nelements, 16, 2);
    }

    /* extract array of values */
    nnbits = (((nele - done) * 16 + 63) / 64) * 64;
    bitpos = bitpos + nnbits;
    nombre = nele * nval * nt;
    err = c_xdfxtr((uint32_t *)buf, tblval, bitpos, nombre, nbit, datyp);

    if ((datyp == 2) || (datyp == 4) || (datyp == 6)) {
        /* set all bits of missing values to 1 */
        allones = ~(0);
        rmask = (nbit >= 32) ? allones : ~(-1 << nbit);
        /*      rmask = ~(-1 << nbit); */
        for (i=0; i < nombre; i++) {
            if (tblval[i] == rmask) tblval[i] = allones;
        }
    }
    return err;
}


//! Position at the end of a sequential file for an append
int c_mrfapp(
    //! [in] Unit number associated to the file
    int iun
) {
    int index_fnom, index, width, nw, end_of_file;
    file_table_entry *fte;
    xdf_record_header *header;
    seq_dir_keys *seq_entry;

    index_fnom = fnom_index(iun);
    if (index_fnom == -1) {
        sprintf(errmsg, "file (unit=%d) is not connected with fnom", iun);
        return error_msg("c_mrfapp", ERR_NO_FNOM, ERROR);
    }

    if ((index = file_index(iun)) == ERR_NO_FILE) {
        sprintf(errmsg, "file (unit=%d) is not open", iun);
        return error_msg("c_mrfapp", ERR_NO_FILE, ERROR);
    }

    fte = file_table[index];

    if (!fte->xdf_seq) {
        sprintf(errmsg, "file (unit=%d) is not sequential", iun);
        return error_msg("c_mrfapp", ERR_BAD_FTYPE, WARNING);
    }

    end_of_file = 0;
    width = W64TOWD(fte->primary_len);

    while (!end_of_file) {
        nw = c_waread2(iun, fte->head_keys, fte->cur_addr, width);
        header = (xdf_record_header *)fte->head_keys;
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
    fte->nxtadr = fte->cur_addr;
    return 0;
}


//! Get the length of the longer report in the file
int c_mrfbfl(
    //! [in] Unit number associated of the file
    int iun
) {
    return f77name(mrfbfl)(&iun);
}


//! Read a report from a file
int c_mrfget(
    //! [in] Handle the record
    int handle,
    //! [out] Pointer to the buffer where the report will be written
    void *buffer
) {
    buffer_interface_ptr buf = (buffer_interface_ptr)buffer;
    burp_record *burprec;
    int err;

    err = c_xdfget(handle, buf);
    if (err < 0) {
        return error_msg("c_mrfget", err, ERROR);
    }
    if (msg_level <= INFORM) {
        fprintf(stdout, "RECORD READ\n");
    }

    burprec = (burp_record *)buf->data;
    buf->buf78.buf8 = burprec->info.nblks;
    buf->buf9 = burprec->info.nblks * NBENTB;

    return 0;
}


//! Write a report to a file
/*!
    If handle is not 0, the record referenced by handle is written at end of file.
    If handle is 0, a new record is written.
    If hanlde is > 0, it will be forced to be negative to write at end of file.
*/
int c_mrfput(
    //! [in] Unit number associated with the file in which to write
    int iun,
    //! [in] Handle of the record
    int handle,
    //! [in] Buffer containing the report
    void *buffer
) {
    buffer_interface_ptr buf = (buffer_interface_ptr)buffer;
    burp_record *burprec;
    int err, new_handle;
    int temps, flgs, idtyp, lat, lon, dx, dy, elev, drnd, date;
    int oars, runn, nblk, sup, nsup, xaux, nxaux;
    char stnid[10];

    burprec = (burp_record *) buf->data;
    burprec->info.nblks = buf->buf78.buf8;

    new_handle = (handle > 0) ? -handle : handle;
    err = c_xdfput(iun, new_handle, buf);
    if (msg_level <= INFORM) {
        nsup = 0;
        nxaux = 0;
        err = c_mrbhdr((uint32_t *)buf, &temps, &flgs, stnid, &idtyp, &lat, &lon,
                &dx, &dy, &elev, &drnd, &date, &oars, &runn, &nblk,
                (uint32_t *)&sup, nsup, (uint32_t *)&xaux, nxaux);
        stnid[9] = '\0';
        fprintf(stdout, "MRFPUT - WRITE - STNID=%s IDTYP=%3d LAT=%5d LON=%5d DX=%4d DY=%4d DATE=%8d TEMPS=%4d, FLGS=%8d\n",
            stnid, idtyp, lat, lon, dx, dy, date, temps, flgs);
    }
    return 0;
}


//! Rewinds a BURP sequential file.
//! \return 0 on success, error code otherwise
int c_mrfrwd(
    //! [in] Unit number associated with the file
    int iun
) {
    int index, index_fnom;
    file_table_entry *fte;

    index_fnom = fnom_index(iun);
    if (index_fnom == -1) {
        sprintf(errmsg, "file (unit=%d) is not connected with fnom", iun);
        return error_msg("c_mrfrwd", ERR_NO_FNOM, ERROR);
    }

    if ((index = file_index(iun)) == ERR_NO_FILE) {
        sprintf(errmsg, "file (unit=%d) is not open", iun);
        return error_msg("c_mrfrwd", ERR_NO_FILE, ERROR);
    }

    fte = file_table[index];

    if (! fte->cur_info->attr.burp) {
        sprintf(errmsg, "file (unit=%d) is not a BURP file", iun);
        return error_msg("c_mrfrwd", ERR_NO_FILE, ERROR);
    }

    if (! fte->xdf_seq) {
        sprintf(errmsg, "file (unit=%d) is not sequential", iun);
        return error_msg("c_mrfrwd", ERR_BAD_FTYPE, WARNING);
    }

    fte->cur_addr = fte->seq_bof;
    fte->valid_pos = 0;
    return 0;
}


// Everything past this point was included from what used to be in if_burp98.h
//! Check if a BURP file is valid
int32_t f77name(burpcheck)(
    //! IN Path of the file to be checked
    char *filePath,
    //! IN filePath strong length
    F2Cl lng
) {
    int ier;
    ier = c_burpcheck(filePath);
    return (int32_t)ier;
}


void f77name(buf89a0)(
    uint32_t *buf
) {
#if defined(NEC64)
    BUF_C;
    c_buf89a0(buf + 1);
    BUF_F;
#else
    c_buf89a0(buf);
#endif
}


int32_t f77name(getbuf8)(
    uint32_t *buf
) {
    int n;
#if defined(NEC64)
    BUF_C;
    n = c_getbuf8(buf + 1);
    BUF_F;
#else
    n = c_getbuf8(buf);
#endif
    return (int32_t)n;
}


void f77name(genvdt8)(
    int32_t *val
) {
    char *enforc8;

    enforc8 = getenv("ENFORC8");
    if (enforc8) {
        *val = 1;
        xdf_enforc8 = 1;
    } else {
        *val = 0;
        xdf_enforc8 = 0;
    }
}



int32_t f77name(mrbadd)(
    uint32_t *buf,
    int32_t *f_bkno,
    int32_t *f_nele,
    int32_t *f_nval,
    int32_t *f_nt,
    int32_t *f_bfam,
    int32_t *f_bdesc,
    int32_t *f_btyp,
    int32_t *f_nbit,
    int32_t *f_bit0,
    int32_t *f_datyp,
    int32_t *lstele,
    uint32_t *tblval
) {
    int bkno = *f_bkno;
    int nele = *f_nele;
    int nval = *f_nval;
    int nt = *f_nt;
    int bfam = *f_bfam;
    int bdesc = *f_bdesc;
    int btyp = *f_btyp;
    int nbit = *f_nbit;
    int bit0 = *f_bit0;
    int datyp = *f_datyp;
    int err;

#if defined(NEC64)
    int32_t listele[1024];
    int32_t *pliste;
    int was_allocated = 0;
    int i;

    BUF_C;
    xdf_stride = 2;
    if (nele > 1024) {
        pliste = calloc(nele,sizeof(int32_t));
        was_allocated = 1;
    } else{
        pliste = listele;
    }

    for (i = 0; i < nele; i++) {
        pliste[i] = lstele[i];
    }

    err = c_mrbadd(buf+1, &bkno, nele, nval, nt, bfam, bdesc, btyp, nbit, &bit0,
        datyp, pliste, tblval+1);
    if (was_allocated) free(pliste);
    xdf_stride = 1;
    BUF_F;
#else
    err = c_mrbadd(buf, &bkno, nele, nval, nt, bfam, bdesc, btyp, nbit, &bit0,
        datyp, (uint32_t *)lstele, tblval);
#endif
    *f_bit0 = (int32_t)bit0;
    *f_bkno = (int32_t)bkno;
    return err;
}


int32_t f77name(mrbdel)(
    uint32_t *buf,
    int32_t *f_number
) {
   int number = *f_number;
   int ier;
#if defined(NEC64)
    BUF_C;
    ier = c_mrbdel(buf + 1, number);
    BUF_F;
#else
    ier = c_mrbdel(buf, number);
#endif
    return (int32_t)ier;

}


int32_t f77name(mrbhdr)(
    uint32_t *buf,
    int32_t *f_temps,
    int32_t *f_flgs,
    char *f_stnid,
    int32_t *f_idtyp,
    int32_t *f_lati,
    int32_t *f_lon,
    int32_t *f_dx,
    int32_t *f_dy,
    int32_t *f_elev,
    int32_t *f_drcv,
    int32_t *f_date,
    int32_t *f_oars,
    int32_t *f_run,
    int32_t *f_nblk,
    int32_t *f_sup,
    int32_t *f_nsup,
    int32_t *f_xaux,
    int32_t *f_nxaux,
    int ll1
) {
    int temps, flgs, idtyp, lati, lon;
    int dx, dy,elev, drcv, date, oars, run, nblk;
    int nsup = *f_nsup, nxaux = *f_nxaux;
    char stnid[11] = {' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', '\0'};
    int ier, l1;

#if defined(NEC64)
    BUF_C;
    ier = c_mrbhdr(buf + 1, &temps, &flgs, stnid, &idtyp,
        &lati, &lon, &dx, &dy, &elev,
        &drcv, &date, &oars, &run, &nblk,
        (uint32_t *)f_sup, nsup, (uint32_t *)f_xaux, nxaux);
    BUF_F;
#else
    ier = c_mrbhdr(buf, &temps, &flgs, stnid, &idtyp,
        &lati, &lon, &dx, &dy, &elev,
        &drcv, &date, &oars, &run, &nblk,
        (uint32_t *)f_sup, nsup, (uint32_t *)f_xaux, nxaux);
#endif
    *f_temps = (int32_t) temps;
    *f_flgs = (int32_t) flgs;
    *f_idtyp = (int32_t) idtyp;
    *f_lati  = (int32_t) lati;
    *f_lon = (int32_t) lon;
    *f_dx = (int32_t) dx;
    *f_dy = (int32_t) dy;
    *f_elev = (int32_t) elev;
    *f_drcv = (int32_t) drcv;
    *f_date = (int32_t) date;
    *f_oars = (int32_t) oars;
    *f_run = (int32_t) run;
    *f_nblk = (int32_t) nblk;
    l1 = (ll1 > 11) ? 11: ll1;
    string_copy(f_stnid,stnid,l1);
    return (int32_t)ier;
}



int32_t f77name(mrblen)(
    uint32_t *buf,
    int32_t *f_lbits,
    int32_t *f_left
) {
    int lbits, left, err;
#if defined(NEC64)
    BUF_C;
    err = c_mrblen(buf + 1, &lbits, &left);
    BUF_F;
#else
   err = c_mrblen(buf, &lbits, &left);
#endif
   *f_lbits = (int32_t)lbits;
   *f_left = (int32_t)left;
   return (int32_t)err;

}


int32_t f77name(mrbloc)(
    uint32_t *buf,
    int32_t *f_bfam,
    int32_t *f_bdesc,
    int32_t *f_btyp,
    int32_t *f_blkno
) {
   int blkno = *f_blkno, bfam = *f_bfam;
   int bdesc = *f_bdesc, btyp = *f_btyp;
   int ier;

#if defined(NEC64)
    BUF_C;
    ier = c_mrbloc(buf + 1, bfam, bdesc, btyp, blkno);
    BUF_F;
#else
    ier = c_mrbloc(buf, bfam, bdesc, btyp, blkno);
#endif
    return (int32_t)ier;
}


int32_t f77name(mrbrep)(
    uint32_t *buf,
    int32_t *f_blkno,
    uint32_t *tblval
) {
    int blkno = *f_blkno;
    int ier;
#if defined(NEC64)
    xdf_stride = 2;
    BUF_C;
    ier = c_mrbrep(buf + 1, blkno, tblval + 1);
    xdf_stride = 1;
    BUF_F;
#else
   ier = c_mrbrep(buf, blkno, tblval);
#endif
   return (int32_t)ier;
}


int32_t f77name(mrbxtr)(
    uint32_t *buf,
    int32_t *f_bkno,
    int32_t *lstele,
    int32_t *tblval
) {
    int ier, i;
    int bkno = *f_bkno;
#if defined(NEC64)
    int32_t *plong;
    BUF_C;
    ier = c_mrbxtr(buf + 1, bkno, (uint32_t *)lstele, (uint32_t *)tblval);
    BUF_F;
    plong = (int32_t *) lstele;
    for (i = BurP_nele - 1; i >= 0; i--) {
        lstele[i] = plong[i];
    }
    plong = (int32_t *)tblval;
    for (i = BurP_ntot - 1; i >= 0; i--) {
        tblval[i] = plong[i];
    }
#else
    ier = c_mrbxtr(buf, bkno, (uint32_t *)lstele, (uint32_t *)tblval);
#endif
    return (int32_t)ier;
}


int32_t f77name(mrfapp)(
    int32_t *f_iun
) {
  int ier, iun = *f_iun;

  ier = c_mrfapp(iun);
  return (int32_t)ier;
}


int32_t f77name(mrfget)(
    int32_t *f_handle,
    uint32_t *buf
) {
   int handle = *f_handle, ier;
#if defined(NEC64)
    BUF_C;
    ier = c_mrfget(handle, buf + 1);
    BUF_F;
#else
    ier = c_mrfget(handle, buf);
#endif
    return (int32_t)ier;
}


int32_t f77name(mrfput)(
    int32_t *f_iun,
    int32_t *f_handle,
    uint32_t *buf
) {
    int iun = *f_iun, handle = *f_handle, ier;

#if defined(NEC64)
    BUF_C;
    ier = c_mrfput(iun, handle, buf + 1);
    BUF_F;
#else
    ier = c_mrfput(iun, handle, buf);
#endif
    return (int32_t)ier;
}


int32_t f77name(mrfrwd)(
    int32_t *f_iun
) {
    int err, iun = *f_iun;

    err = c_mrfrwd(iun);
    return (int32_t)err;
}
