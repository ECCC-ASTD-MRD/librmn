/* RMNLIB - Library of useful routines for C and FORTRAN programming
 * Copyright (C) 1975-2021  Division de Recherche en Prevision Numerique
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
 */
     int c_fst_data_length(int length_type);
     int c_ip1_all(float level, int kind);
     int c_ip1_val(float level, int kind);
     int c_ip2_all(float level, int kind);
     int c_ip2_val(float level, int kind);
     int c_ip3_all(float level, int kind);
     int c_ip3_val(float level, int kind);
     int c_xdflnk(int *liste, int n);
     int c_xdfunl(int *liste, int n);
     int c_fstckp(int iun);
     int c_fstmsq(int iun, int *mip1, int *mip2, int *mip3, char *metiket, int getmode);
     int c_fstopi(char *option, int value, int getmode);
     int c_fstopl(char *option, int value, int getmode);
     int c_fstopr(char *option, float value, int getmode);
     int c_fstopc(char *option, char *value, int getmode);
     int c_fstcheck(const char *filePath);
     int c_fstinf(int iun, int *ni, int *nj, int *nk, int datev,char *in_etiket,
                  int ip1, int ip2, int ip3, char *in_typvar, char *in_nomvar);
     int c_fstsui(int iun, int *ni, int *nj, int *nk);
     int c_fstinl(int iun, int *ni, int *nj, int *nk, int datev, char *etiket,
                  int ip1, int ip2, int ip3, char *typvar, char *nomvar,
                  int *liste, int *infon, int nmax);
     int c_fstinfx(int handle, int iun, int *ni, int *nj, int *nk,
                   int datev,char *in_etiket,
                   int ip1, int ip2, int ip3, char *in_typvar, char *in_nomvar);
     int c_fstlir(int *field, int iun, int *ni, int *nj, int *nk,
                  int datev, char *etiket,
                  int ip1, int ip2, int ip3, char *typvar, char *nomvar);
     int c_fstlirx(int *field, int handle, int iun,
                   int *ni, int *nj, int *nk, int datev, char *etiket,
                   int ip1, int ip2, int ip3, char *typvar, char *nomvar);
     int c_fstlis(int *field, int iun, int *ni, int *nj, int *nk);
     int c_fstlic(int *field, int iun, int niin, int njin, int nkin,
                  int datein, char *etiketin, int ip1in, int ip2in, int ip3in,
                  char *typvarin, char *nomvarin,
                  int ig1in, int ig2in, int ig3in, int ig4in, char *grtypin);
     int c_fstluk(int *field, int handle, int *ni, int *nj, int *nk);
     int c_fstprm(int handle, int *dateo, int *deet, int *npas, int *ni, int *nj, int *nk,
                  int *nbits, int *datyp, int *ip1, int *ip2, int *ip3, char *typvar,
                  char *nomvar, char *etiket, char *grtyp,
                  int *ig1, int *ig2, int *ig3, int *ig4, int *swa, int *lng,
                  int *dltf, int *ubc, int *extra1, int *extra2, int *extra3);
     int c_fstecr(int *field_in, void * work, int npak, int iun, int date,
                  int deet, int npas, int ni, int nj, int nk, int ip1, int ip2, int ip3,
                  char *in_typvar, char *in_nomvar, char *in_etiket, char *in_grtyp, 
                  int ig1, int ig2, int ig3, int ig4, int in_datyp_ori, int rewrit);
     int c_fstouv(int iun, char *options);
     int c_fstvoi(int iun,char *options);
     int c_fstapp(int iun, char *option);
     int c_fsteff(int handle);
     int c_fsteof(int iun);
     int c_fstrwd(int iun);
     int c_fstskp(int iun, int nrec);
     int c_fstweo(int iun, int level);
     int c_fstnbr(int iun);
     int c_fstnbrv(int iun);
     int c_fstfrm(int iun);
     int c_fst_version();
     void c_fstreset_ip_flags();
     void c_fst_env_var(char *cle, int index, char *content);
     int c_fst_edit_dir_plus(int handle, unsigned int date, int deet, int npas,
                             int ni, int nj, int nk, int ip1, int ip2, int ip3,
                             char *in_typvar, char *in_nomvar, char *in_etiket, char *in_grtyp, 
                             int ig1, int ig2, int ig3, int ig4, int datyp);
     int c_fst_edit_dir(int handle, unsigned int date, int deet, int npas,
                        int ni, int nj, int nk, int ip1, int ip2, int ip3,
                        char *in_typvar, char *in_nomvar, char *in_etiket, char *in_grtyp, 
                        int ig1, int ig2, int ig3, int ig4, int datyp);
