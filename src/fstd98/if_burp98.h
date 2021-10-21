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
    INT_32 listele[1024];
    INT_32 *pliste;
    int was_allocated = 0;
    int i;

    BUF_C;
    xdf_stride = 2;
    if (nele > 1024) {
        pliste = calloc(nele,sizeof(INT_32));
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
    INT_32 *plong;
    BUF_C;
    ier = c_mrbxtr(buf + 1, bkno, (uint32_t *)lstele, (uint32_t *)tblval);
    BUF_F;
    plong = (INT_32 *) lstele;
    for (i = BurP_nele - 1; i >= 0; i--) {
        lstele[i] = plong[i];
    }
    plong = (INT_32 *)tblval;
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
