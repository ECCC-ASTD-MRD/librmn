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

//! C interface for burp


#include <string.h>
#include <stdint.h>

#include <rmn/rpnmacros.h>

int32_t f77name(mrblocx)(int32_t * buf, int32_t * bfam, int32_t * bdesc, int32_t * bknat, int32_t * bktyp, int32_t * bkstp, int32_t * blk0);
int32_t f77name(mrbprml)(int32_t  * buf, int32_t  * bkno, int32_t  * blprm, int32_t  * nprm, int32_t  * inblocs);

//! Convert a list of of 6 digit decimal BUFR coded element names to 16-bit
int c_mrbcol(int liste[], int cliste[], int nele)
{
    int lnele;
    lnele = nele;
    return f77name(mrbcol)(liste, cliste, &lnele);
}


//! Convert a 6-digit decimal BUFR element name to a 16-bit coded (CMC) element name
int c_mrbcov(int elem)
{
    int lelem;
    lelem = elem;
    return f77name(mrbcov)(&lelem);
}


//! Convert value from float to integer or vise-versa
//
//! Elements that are codes instead of values are not converted.
//! Missing values in tblval are encoded by settings all the bits to 1
int c_mrbcvt(
    int liste[],
    //! Integer representation of data (BUFR)
    int tblval[],
    //! Float representation of data (CMC)
    float rval[],
    //! Number of elements
    int nele,
    //! Number of values per element
    int nval,
    //! Number of groups
    int nt,
    //! When 0, convert from tblval to rval.  When 1, do the opposite.
    int mode
) {
    int lnele, lnval, lnt, lmode;
    lnele = nele; lnval = nval; lnt = nt; lmode = mode;
    return f77name(mrbcvt)(liste, tblval, rval, &lnele, &lnval, &lnt, &lmode);
}


//! Convert a list of of CMC coded element names to 6 digit decimal BUFR format
int c_mrbdcl(
    //! [in] List of CMC coded element names
    int cliste[],
    //! [out] List of 6 decimal digits BUFR element names
    int liste[],
    //! [in] Number of names in cliste
    int nele
) {
    int lnele;
    lnele = nele;
    return f77name(mrbdcl)(cliste, liste, &lnele);
}


//! Convert a 16-bit coded (CMC) element name to a 6-digit decimal BUFR element name
int c_mrbdcv(int elem)
{
    int lelem;
    lelem = elem;
    return f77name(mrbdcv)(&lelem);
}


//! Initialize a report header
//
//! This is the first function that must be called to create a report.
//! This must be done before data blocks are added to the report.
int c_mrbini(
    int iun,
    int buf[],
    int temps,
    int flgs,
    char stnid[],
    int idtp,
    int lati,
    int longi,
    int dx,
    int dy,
    int elev,
    int drcv,
    int date,
    int oars,
    int runn,
    int sup[],
    int nsup,
    int xaux[],
    int nxaux
) {
    int liun, ltemps, lflgs, lidtp, llati, llongi, lelev, ldrcv, ldate, loars;
    int ldx, ldy;
    int lrunn, lnsup, lnxaux;
    F2Cl l1 = strlen(stnid);
    ltemps = temps; lflgs = flgs; lidtp = idtp; llati = lati; llongi = longi;
    ldx = dx; ldy = dy;
    lelev = elev; ldrcv = drcv; ldate = date; loars = oars; lrunn = runn;
    lnsup = nsup; lnxaux = nxaux; liun = iun;
    return f77name(mrbini)(&liun, buf, &ltemps, &lflgs, stnid, &lidtp, &llati, &llongi,
                    &ldx, &ldy, &lelev, &ldrcv, &ldate, &loars, &lrunn, sup, &lnsup, xaux,
                    &lnxaux, l1);
}


//! Find a block in a report
int c_mrblocx(int buf[], int bfam, int bdesc, int bknat, int bktyp, int bkstp, int blk0)
{
    int lbfam,lbdesc,lbknat,lbktyp,lbkstp,lblk0;
    lbfam = bfam; lbdesc = bdesc; lblk0 = blk0;
    lbknat = bknat; lbktyp = bktyp; lbkstp = bkstp;
    return f77name(mrblocx)(buf, &lbfam, &lbdesc, &lbknat, &lbktyp, &lbkstp, &lblk0);
}


//! Extract the parameter descriptors from all the blocks
int c_mrbprml(int buf[], int bkno, int tblprm[], int nprm, int inblocs)
{
    int lbkno, lnprm, linblocs;

    lbkno = bkno; lnprm = nprm; linblocs = inblocs;

    return f77name(mrbprml)(buf, &lbkno, tblprm, &lnprm, &linblocs);
}


//! Verify if the element is repeating
int c_mrbrpt(int elem)
{
    int lelem;
    lelem = elem;
    return f77name(mrbrpt)(&lelem);
}


//! Add and initialize a user-defined array of variables for conversion to the array used by mbrcvt
//
//! The uncoded names of elements defined by the user should limit the values
//! between 63000 and 63255 inclusively and must be in coded (CMC) form before
//! calling MRBSCT.
int c_mrbsct(
    //! Array of user-defined variables
    int tablusr[],
    int neleusr
) {
    int lneleusr;
    lneleusr = neleusr;
    return f77name(mrbsct)(tablusr, &lneleusr);
}


//! Fill a table from tableburp
int c_mrbtbl(int tablusr[], int nslots, int neleusr)
{
    int lneleusr, lnslots;
    lneleusr = neleusr;
    lnslots  = nslots;
    return f77name(mrbtbl)(tablusr, &lnslots, &lneleusr);
}


//! \todo Write a short description of what this function actually does!
//! If btyp = -1, then a search key will be derived from bknat, bktyp and bkstp 
//! and placed in btyp; the function will also return the value of btyp itself.
//! If btyp > 0, then it will decode the "btyp" and return values in bknat, 
//! bktyp, bkstp; the function will return 0. 
int c_mrbtyp(int *hbknat, int *hbktyp, int *hbkstp, int hbtyp)
{
    int lbtyp;
    lbtyp = hbtyp;
    return f77name(mrbtyp)(hbknat, hbktyp, hbkstp, &lbtyp);
}


//! Update a report header
//
//! If the value of a parameter is -1 ('*' for stnid), it will not have an 
//! up-to-date value of the parameter. If nsup = 0, sup is ignored.
//! If nxaux = 0, xaux is ignored.
int c_mrbupd(
    int iun,
    int buf[],
    int temps,
    int flgs,
    char stnid[],
    int idtp,
    int lati,
    int longi,
    int dx,
    int dy,
    int elev,
    int drcv,
    int date,
    int oars,
    int runn,
    int sup[],
    int nsup,
    int xaux[],
    int nxaux
) {
    int liun,ltemps,lflgs,lidtp,llati,llongi,lelev,ldrcv,ldate,loars;
    int ldx, ldy;
    int lrunn,lnsup,lnxaux;
    F2Cl l1 = strlen(stnid);
    ltemps = temps; lflgs = flgs; lidtp = idtp; llati = lati; llongi = longi;
    ldx = dx; ldy = dy;
    lelev = elev; ldrcv = drcv; ldate = date; loars = oars; lrunn = runn;
    lnsup = nsup; lnxaux = nxaux; liun = iun;
    return f77name(mrbupd)(&liun, buf, &ltemps, &lflgs, stnid, &lidtp, &llati, &llongi,
                    &ldx, &ldy, &lelev, &ldrcv, &ldate, &loars, &lrunn, sup, &lnsup, xaux,
                    &lnxaux, l1);
}


//! Close a report file
//
//! Files nmust be closed to prevent damage that could be sufficient to render
//! them useless.
int c_mrfcls(int iun)
{
    int liun;
    liun = iun;
    return f77name(mrfcls)(&liun);
}


//! Get the value of a character option
int c_mrfgoc(char optnom[], char opvalc[9])
{
    F2Cl l1 = strlen(optnom);
    F2Cl l2 = strlen(opvalc);
    int iii = f77name(mrfgoc)(optnom, opvalc, l1, l2);
    opvalc[8] = '\0';
    return iii;
}


//! Get the value of a float option
int c_mrfgor(char optnom[], float *opvalr)
{
    F2Cl l1 = strlen(optnom);
    return f77name(mrfgor)(optnom, opvalr, l1);
}


//! Locate the handle of the report that matches the stnid, idtyp, lati, long, date, temps parameters and the contents of array sup.
//
//! The search will start at the beginning if "handle" is equal to 0 otherwise,
//! the search will start on the report that follows the report pointed to by 
//! "handle". If an element of stnid is equal to an asterik ('*'), this element
//! will be considered like a "wildcard" and will be ignored during the search.
//! It is the same for idtyp, lati, long, date, temps and sup if their values
//! are -1. Note that only the "hour" portion of the argument temps is used
//! during the search.
int c_mrfloc(
    int iun,
    int handle,
    char stnid[],
    int idtyp,
    int lat,
    int lon,
    int date,
    int temps,
    int sup[],
    int nsup
) {
    F2Cl l1 = strlen(stnid);
    return f77name(mrfloc)(&iun, &handle, stnid, &idtyp, &lat, &lon, &date,
                    &temps, sup, &nsup, l1);
}


//! Get the length of the longest report in the file corresponding to iun
int c_mrfmxl(int iun)
{
    int liun;
    liun = iun;
    return f77name(mrfmxl)(&liun);
}


//! Get the number of active reports in the file corresponding to iun
//
//! The file doesn't need to be opened before calling this function.  It will be
//! left in the same state it as it was before the call.
int c_mrfnbr(int iun)
{
    int liun;
    liun = iun;
    return f77name(mrfnbr)(&liun);
}


//! Open a report file
//
//! \return Number of active reports in the file
int c_mrfopn(int iun, char mode[])
{
    F2Cl l1 = strlen(mode);
    return f77name(mrfopn)(&iun, mode, l1);
}


//! Initialize a character option
int c_mrfopc(char optnom[], char opvalc[])
{
    F2Cl l1 = strlen(optnom);
    F2Cl l2 = strlen(opvalc);
    return f77name(mrfopc)(optnom, opvalc, l1, l2);
}


//! Initialize a float option
int c_mrfopr(char optnom[], float opvalr)
{
    F2Cl l1 = strlen(optnom);
    return f77name(mrfopr)(optnom, &opvalr, l1);
}


//! Get the main parameters from the report referenced by handle
int c_mrfprm(
    int handle,
    char stnid[10],
    int *idtyp,
    int *lat,
    int *lon,
    int *dx,
    int *dy,
    int *date,
    int *temps,
    int *flgs,
    int sup[],
    int nsup,
    int *lng
) {
    F2Cl l1 = strlen(stnid);
    int iii = f77name(mrfprm)(&handle, stnid, idtyp, lat, lon, dx, dy, date, temps, flgs,
                    sup, &nsup, lng, l1);
    stnid[9] = '\0';
    return iii;
}


//! Print the parameter descriptions from reports contained in the file corresponding to iun
int  c_mrfvoi(int iun)
{
    return f77name(mrfvoi)(&iun);
}


//! Delete the report designated by handle
int c_mrfdel(int handle)
{
    return f77name(mrfdel)(&handle);
}
