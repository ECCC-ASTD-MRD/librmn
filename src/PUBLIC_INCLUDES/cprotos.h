// Prototypes automatically extracted from the C source code with ctags



// cmc_log/cmcwlog.c
int c_cmcwlog(char * cl,int msgno,char * id,char * txt);
int c_cmcwlog2(char * filen,char * cl,int msgno,char * id,char * txt);
int c_cmcwlog2B(char * filen,char * cl,int msgno,char * id,char * txt);
int c_cmcwlog3(char * filen,char * cl,int msgno,char * id,char * txt);


// compresseur/armn_compress_32.c
int compact_mask_char(unsigned int * dest,unsigned char * src,int npts);
int c_armn_compress32(unsigned char * zstream,float * fld,int ni,int nj,int nk,int znbits);
int c_armn_uncompress32(float * fld,unsigned char * zstream,int ni,int nj,int nk,int znbits);
int c_fstzip32(unsigned int * zfld,unsigned int * fld,int ni,int nj,int nk,int step,int nbits,int remaining_space);
int uncompact_mask_char(int * dest,unsigned int * src,int npts);
void pack1bitRLE(unsigned int z[],unsigned int * zlng,unsigned char ufld[],int npts);
void packTokensParallelogram32(unsigned int z[],int * zlng,unsigned int ufld[],int ni,int nj,int istep,int nbits,int remaining_space);
void packTokensParallelogram_8(unsigned int z[],unsigned int * zlng,unsigned char ufld[],int ni,int nj,int nbits,int istep);
void pack_stream_nbits_16(unsigned int z[],unsigned int * zlng,unsigned short ufld[],int npts,int nbits);
void pack_stream_nbits_32(unsigned int z[],unsigned int * zlng,unsigned int ufld[],int npts,int nbits);
void pack_stream_nbits_8(unsigned int z[],unsigned int * zlng,unsigned char ufld[],int npts,int nbits);
void unpack1bitRLE(unsigned char ufld[],unsigned int z[],unsigned int * zlng,int npts);
void unpackTokensParallelogram32(unsigned int ufld[],unsigned int z[],int ni,int nj,int nbits,int istep);
void unpackTokensParallelogram_8(unsigned char ufld[],unsigned int z[],int ni,int nj,int nbits,int istep);
void unpack_stream_nbits_16(unsigned short ufld[],unsigned int z[],int npts,int nbits);
void unpack_stream_nbits_32(unsigned int ufld[],unsigned int z[],int npts,int nbits);
void unpack_stream_nbits_8(unsigned char ufld[],unsigned int z[],int npts,int nbits);


// compresseur/c_zfstlib.c
int armn_compress(unsigned char * fld,int ni,int nj,int nk,int nbits,int op_code);
int c_armn_compress_getlevel();
int c_armn_compress_getswap();
int c_fstzip_parallelogram(unsigned int * zfld,int * zlng,unsigned short * fld,int ni,int nj,int step,int nbits,int32_t * header);
int is_on_coarse(int i,int j,int ni,int nj,int step);
void calcule_entropie(float * entropie,unsigned short * bitstream,int npts,int nbits);
void calcul_ajusxy(int * ajus_x,int * ajus_y,int ni,int nj,int istep);
void calcul_ninjcoarse(int * nicoarse,int * njcoarse,int ni,int nj,int ajus_x,int ajus_y,int istep);
void c_armn_compress_option(char * option,char * value);
void c_armn_compress_setlevel(int level);
void c_armn_compress_setswap(int swapState);
void c_fstunzip(unsigned int * fld,unsigned int * zfld,int ni,int nj,int nbits);
void c_fstunzip_minimum(unsigned short * fld,unsigned int * zfld,int ni,int nj,int step,int nbits,int32_t * header);
void c_fstunzip_parallelogram(unsigned short * fld,unsigned int * zfld,int ni,int nj,int step,int nbits,int32_t * header);
void c_fstunzip_sample(unsigned short * fld,unsigned int * zfld,int ni,int nj,int step,int nbits,int32_t * header);
void c_fstzip(unsigned int * zfld,int * zlng,unsigned int * fld,int ni,int nj,int code_methode,int degre,int step,int nbits,int bzip);
void c_fstzip_minimum(unsigned int * zfld,int * zlng,unsigned short * fld,int ni,int nj,int step,int nbits,int32_t * header);
void c_fstzip_sample(unsigned int * zfld,int * zlng,unsigned short * fld,int ni,int nj,int step,int nbits,int32_t * header);
void fixpredflds(int * predfld,int * zc,int ni,int nj,int nicoarse,int njcoarse,int step,int ajus_x,int ajus_y);
void init_comp_settings(char * comp_settings);
void packTokensMinimum(unsigned int z[],int * zlng,unsigned short ufld[],int ni,int nj,int nbits,int istep,int32_t * header);
void packTokensParallelogram(unsigned int z[],int * zlng,unsigned short ufld[],int ni,int nj,int nbits,int istep,int32_t * header);
void packTokensSample(unsigned int z[],int * zlng,unsigned int zc[],int nicoarse,int njcoarse,int diffs[],int ni,int nj,int nbits,int step,int32_t * header,int start,int end);
void unpackTokensMinimum(unsigned short ufld[],unsigned int z[],int ni,int nj,int nbits,int istep,int32_t * header);
void unpackTokensParallelogram(unsigned short ufld[],unsigned int z[],int ni,int nj,int nbits,int istep,int32_t * header);
void unpackTokensSample(unsigned int zc[],int diffs[],unsigned int z[],int nicoarse,int njcoarse,int ni,int nj,int nbits,int step,int32_t * header,int start);


// fstd98/FC_string.c
char * fstring_to_cstring(char * fstring,int nc,int rmblanks);
char ** allocate_string_array(int ns);
char ** fill_string_array(char ** string_array,char * farray,int nc,int ns,int rmblanks);
void cstring_to_fstring(char * cstring,char * fstring,int nc);
void free_cstring(char * cstring);
void free_string_array(char ** string_array);


// fstd98/burp98.c
int c_burpcheck(const char * filePath);
int c_getbuf8(int32_t * buffer);
int c_mrbadd(void * buffer,int * bkno,int nele,int nval,int nt,int bfam,int bdesc,int btyp,int nbit,int * bit0,int datyp,int32_t * lstele,int32_t * tblval);
int c_mrbdel(void * buffer,int number);
int c_mrbhdr(int32_t * buf,int * temps,int * flgs,char * stnid,int * idtyp,int * lati,int * lon,int * dx,int * dy,int * elev,int * drcv,int * date,int * oars,int * run,int * nblk,int32_t * sup,int nsup,int32_t * xaux,int nxaux);
int c_mrblen(void * buffer,int * bitsUsed,int * bitsLeft);
int c_mrbloc(void * buffer,int bfam,int bdesc,int btyp,int blkno);
int c_mrbprm(int32_t * buf,int bkno,int * nele,int * nval,int * nt,int * bfam,int * bdesc,int * btyp,int * nbit,int * bit0,int * datyp);
int c_mrbrep(void * buffer,int blkno,int32_t * tblval);
int c_mrbxtr(void * buffer,int bkno,int32_t * lstele,int32_t * tblval);
int c_mrfapp(int iun);
int c_mrfbfl(int iun);
int c_mrfget(int handle,void * buffer);
int c_mrfput(int iun,int handle,void * buffer);
int c_mrfrwd(int iun);
void build_burp_info_keys(int32_t * buf,int32_t * keys,int index,int mode);
void build_burp_prim_keys(burp_record * brpk,int32_t * keys,burp_record * mask,int32_t * mskkeys,int index,int mode);
void c_buf89a0(int32_t * buffer);


// fstd98/c_burp.c
int c_mrbcol(int liste[],int cliste[],int nele);
int c_mrbcov(int elem);
int c_mrbcvt(int liste[],int tblval[],float rval[],int nele,int nval,int nt,int mode);
int c_mrbdcl(int cliste[],int liste[],int nele);
int c_mrbdcv(int elem);
int c_mrbini(int iun,int buf[],int temps,int flgs,char stnid[],int idtp,int lati,int longi,int dx,int dy,int elev,int drcv,int date,int oars,int runn,int sup[],int nsup,int xaux[],int nxaux);
int c_mrblocx(int buf[],int bfam,int bdesc,int bknat,int bktyp,int bkstp,int blk0);
int c_mrbprml(int buf[],int bkno,int tblprm[],int nprm,int inblocs);
int c_mrbrpt(int elem);
int c_mrbsct(int tablusr[],int neleusr);
int c_mrbtbl(int tablusr[],int nslots,int neleusr);
int c_mrbtyp(int * hbknat,int * hbktyp,int * hbkstp,int hbtyp);
int c_mrbupd(int iun,int buf[],int temps,int flgs,char stnid[],int idtp,int lati,int longi,int dx,int dy,int elev,int drcv,int date,int oars,int runn,int sup[],int nsup,int xaux[],int nxaux);
int c_mrfcls(int iun);
int c_mrfdel(int handle);
int c_mrfgoc(char optnom[],char opvalc[9]);
int c_mrfgor(char optnom[],float * opvalr);
int c_mrfloc(int iun,int handle,char stnid[],int idtyp,int lat,int lon,int date,int temps,int sup[],int nsup);
int c_mrfmxl(int iun);
int c_mrfnbr(int iun);
int c_mrfopc(char optnom[],char opvalc[]);
int c_mrfopn(int iun,char mode[]);
int c_mrfopr(char optnom[],float opvalr);
int c_mrfprm(int handle,char stnid[10],int * idtyp,int * lat,int * lon,int * dx,int * dy,int * date,int * temps,int * flgs,int sup[],int nsup,int * lng);
int c_mrfvoi(int iun);


// fstd98/c_fstgrib_helper.c
void c_84bits_ig_get(int32_t * ig1,int32_t * ig2,int32_t * ig3,int32_t * ig4,int32_t * the_84bit_token);
void c_84bits_ig_put(int32_t * ig1,int32_t * ig2,int32_t * ig3,int32_t * ig4,int32_t * the_84bit_token);
void c_84bits_ip_get(int32_t * ip1,int32_t * ip2,int32_t * ip3,int32_t * the_84bit_token);
void c_84bits_ip_put(int32_t * ip1,int32_t * ip2,int32_t * ip3,int32_t * the_84bit_token);
void c_84bits_token(int32_t * the_84bit_token,unsigned char * grib_header,int32_t length_grib_header);
void c_def_84bitkey(int32_t * ip1,int32_t * ip2,int32_t * ip3,int32_t * ig1,int32_t * ig2,int32_t * ig3,int32_t * ig4,unsigned char * grib_header,int len_grib_header);
void c_igaip84(int32_t * ip1,int32_t * ip2,int32_t * ip3,int32_t * ig1,int32_t * ig2,int32_t * ig3,int32_t * ig4);
void c_ipaig84(int32_t * ig1,int32_t * ig2,int32_t * ig3,int32_t * ig4,int32_t * ip1,int32_t * ip2,int32_t * ip3);
void grb_84bits_to_ig1234(int32_t * ig1,int32_t * ig2,int32_t * ig3,int32_t * ig4,int32_t * the_84bit_token);
void grb_84bits_to_ip123(int32_t * ip1,int32_t * ip2,int32_t * ip3,int32_t * the_84bit_token);


// fstd98/excdes_new.c
int C_filtre_desire();
int C_filtre_exclure();
int C_fstmatch_parm(int handle,int datevalid,int ni,int nj,int nk,int ip1,int ip2,int ip3,char * typvar,char * nomvar,char * etiket,char * grtyp,int ig1,int ig2,int ig3,int ig4);
int C_fstmatch_req(int handle);
int C_fst_match_req(int handle);
int C_requetes_read_file(char * requetes_file);
int C_requetes_reset(int set_nb,int nomvars,int typvars,int etikets,int dates,int ip1s,int ip2s,int ip3s);
int C_select_date(int set_nb,int des_exc,int * date_list,int nelm);
int C_select_etiquette(char * etiq_list[],int nelm);
int C_select_groupset(int first_set_nb,int last_set_nb);
int C_select_ip1(void * iplist,int nelm);
int C_select_ip2(void * iplist,int nelm);
int C_select_ip3(void * iplist,int nelm);
int C_select_nomvar(char * nomv_list[],int nelm);
int C_select_suppl(int ni,int nj,int nk,int ig1,int ig2,int ig3,int ig4,char gtyp);
int C_select_typvar(char * typv_list[],int nelm);
int Directive_charvar(int argc,char ** argv,char cmd_strt,void * func (),char * Private_Data_2);
int Directive_dates(int argc,char ** argv,char cmd_strt,void * func (int set_nb,int des_exc,int * date_list,int nelm,float delta),char * Private_Data_2);
int Directive_datev(int argc,char ** argv,char cmd_strt,void * func (),char * Private_Data_2);
int Directive_desire(int argc,char ** argv,char cmd_strt,void * func (),char * Private_Data_2);
int Directive_exclure(int argc,char ** argv,char cmd_strt,void * func (),char * Private_Data_2);
int Directive_ip123(int argc,char ** argv,char cmd_strt,void * func (),char * Private_Data_2);
int fst_deactivate_filters();
int fst_reactivate_filters();
int ReadRequestTable(char * filename);
int Xc_Select_date(int set_nb,int des_exc,int * date_list,int nelm);
int Xc_Select_etiquette(int set_nb,int des_exc,char * etiq_list[],int nelm);
int Xc_Select_ip1(int set_nb,int des_exc,void * iplist,int nelm);
int Xc_Select_ip2(int set_nb,int des_exc,void * iplist,int nelm);
int Xc_Select_ip3(int set_nb,int des_exc,void * iplist,int nelm);
int Xc_Select_nomvar(int set_nb,int des_exc,char * nomv_list[],int nelm);
int Xc_Select_suppl(int set_nb,int des_exc,int ni,int nj,int nk,int ig1,int ig2,int ig3,int ig4,char gtyp);
int Xc_Select_typvar(int set_nb,int des_exc,char * typv_list[],int nelm);
int Xf_Select_etiquette(int set_nb,int des_exc,char * etiq_list,int nelm,int flng);
int Xf_Select_nomvar(int set_nb,int des_exc,char * nomv_list,int nelm,int flng);
int Xf_Select_typvar(int set_nb,int des_exc,char * typv_list,int nelm,int flng);
void C_requetes_init(char * requetes_filename,char * debug_filename);
void DumpRequestTable();
void f_requetes_init();
void RequetesInit();
void WriteRequestTable(int use_header,char * filename);


// fstd98/fst_missing.c
int EncodeMissingValue(void * field,void * field2,int nvalues,int datatype,int nbits,int is_byte,int is_short,int is_double);
int encode_missing_value(void * field,void * field2,int * nvalues,int * datatype,int * nbits,int * is_byte,int * is_short,int * is_double);
int ForceMissingValueUsage(int flag);
int force_missing_value_used(int * flag);
int get_missing_value_flags(float * f,int * i,unsigned int * ui,double * d,short * s,unsigned short * us,signed char * b,unsigned char * ub);
int missing_value_used();
void DecodeMissingValue(void * field,int nvalues,int datatype,int is_byte,int is_short,int is_double);
void decode_missing_value(void * field,int * nvalues,int * datatype,int * is_byte,int * is_short,int * is_double);
void RestoreMissingValueMapping(void);
void restore_missing_value_mapping(void);
void restore_missing_value_mapping_(void);
void restore_missing_value_mapping__(void);
void SetMissingValueMapping(int what,int datatype,void * processor_,int is_byte,int is_short,int is_double);
void set_missing_value_flags(float * f,int * i,unsigned int * ui,double * d,short * s,unsigned short * us,signed char * b,unsigned char * ub);
void set_missing_value_mapping(int * what,int * datatype,void * processor_,int * is_byte,int * is_short,int * is_double);


// fstd98/fstd98.c
int FstCanTranslateName(char * varname);
int c_fst_data_length(int length_type);
int c_fst_edit_dir(int handle,unsigned int date,int deet,int npas,int ni,int nj,int nk,int ip1,int ip2,int ip3,char * in_typvar,char * in_nomvar,char * in_etiket,char * in_grtyp,int ig1,int ig2,int ig3,int ig4,int datyp);
int c_fst_edit_dir_plus(int handle,unsigned int date,int deet,int npas,int ni,int nj,int nk,int ip1,int ip2,int ip3,char * in_typvar,char * in_nomvar,char * in_etiket,char * in_grtyp,int ig1,int ig2,int ig3,int ig4,int datyp);
int c_fst_version();
int c_fstapp(int iun,char * option);
int c_fstcheck(const char * filePath);
int c_fstckp(int iun);
int c_fstecr(uint32_t * field_in,void * work,int npak,int iun,int date,int deet,int npas,int ni,int nj,int nk,int ip1,int ip2,int ip3,char * in_typvar,char * in_nomvar,char * in_etiket,char * in_grtyp,int ig1,int ig2,int ig3,int ig4,int in_datyp_ori,int rewrit);
int c_fsteff(int handle);
int c_fsteof(int iun);
int c_fstfrm(int iun);
int c_fstinf(int iun,int * ni,int * nj,int * nk,int datev,char * in_etiket,int ip1,int ip2,int ip3,char * in_typvar,char * in_nomvar);
int c_fstinfx(int handle,int iun,int * ni,int * nj,int * nk,int datev,char * in_etiket,int ip1,int ip2,int ip3,char * in_typvar,char * in_nomvar);
int c_fstinl(int iun,int * ni,int * nj,int * nk,int datev,char * etiket,int ip1,int ip2,int ip3,char * typvar,char * nomvar,int * liste,int * infon,int nmax);
int c_fstlic(uint32_t * field,int iun,int niin,int njin,int nkin,int datein,char * etiketin,int ip1in,int ip2in,int ip3in,char * typvarin,char * nomvarin,int ig1in,int ig2in,int ig3in,int ig4in,char * grtypin);
int c_fstlir(uint32_t * field,int iun,int * ni,int * nj,int * nk,int datev,char * etiket,int ip1,int ip2,int ip3,char * typvar,char * nomvar);
int c_fstlirx(uint32_t * field,int handle,int iun,int * ni,int * nj,int * nk,int datev,char * etiket,int ip1,int ip2,int ip3,char * typvar,char * nomvar);
int c_fstlis(uint32_t * field,int iun,int * ni,int * nj,int * nk);
int c_fstluk(uint32_t * field,int handle,int * ni,int * nj,int * nk);
int c_fstmsq(int iun,int * mip1,int * mip2,int * mip3,char * metiket,int getmode);
int c_fstnbr(int iun);
int c_fstnbrv(int iun);
int c_fstopc(char * option,char * value,int getmode);
int c_fstopi(char * option,int value,int getmode);
int c_fstopl(char * option,int value,int getmode);
int c_fstopr(char * option,float value,int getmode);
int c_fstouv(int iun,char * options);
int c_fstprm(int handle,int * dateo,int * deet,int * npas,int * ni,int * nj,int * nk,int * nbits,int * datyp,int * ip1,int * ip2,int * ip3,char * typvar,char * nomvar,char * etiket,char * grtyp,int * ig1,int * ig2,int * ig3,int * ig4,int * swa,int * lng,int * dltf,int * ubc,int * extra1,int * extra2,int * extra3);
int c_fstrwd(int iun);
int c_fstskp(int iun,int nrec);
int c_fstsui(int iun,int * ni,int * nj,int * nk);
int c_fstvoi(int iun,char * options);
int c_fstweo(int iun,int level);
int c_ip1_all(float level,int kind);
int c_ip1_val(float level,int kind);
int c_ip2_all(float level,int kind);
int c_ip2_val(float level,int kind);
int c_ip3_all(float level,int kind);
int c_ip3_val(float level,int kind);
int init_ip_vals();
int ip_is_equal(int target,int ip,int ind);
void backto64(int32_t * field,int32_t * temp,int nelm);
void c_fst_env_var(char * cle,int index,char * content);
void c_fstreset_ip_flags();


// fstd98/memcpy_16_32.c
void memcpy_16_32(int * p32,short * p16,int nbits,int n);
void memcpy_32_16(short * p16,int * p32,int nbits,int n);


// fstd98/memcpy_8_16.c
void memcpy_16_8(char * p8,short * p16,int nb);
void memcpy_8_16(short * p16,char * p8,int nb);


// fstd98/xdf98.c
int c_qdfdiag(int iun);
int c_qdfmsig(int iun,char * newappl);
int c_qdfput(int32_t * buf,int elem,int derbit,int nbits);
int c_qdfrstr(int inp,int outp);
int c_secateur(char * filename,int where);
int c_xdfadd(int32_t * buffer,int32_t * donnees,int nelm,int nbits,int datyp);
int c_xdfcheck(const char * filePath);
int c_xdfcle(char * keyname,int bit1,int lkey,int tkey,int * desc1,int * desc2);
int c_xdfcls(int iun);
int c_xdfcut(void * buffer,int bitpos,int nelm,int nbits,int datyp);
int c_xdfdel(int handle);
int c_xdfget(int handle,buffer_interface_ptr buf);
int c_xdfget2(int handle,buffer_interface_ptr buf,int * aux_ptr);
int c_xdfgop(char * optname,char * optc,int * optv);
int c_xdfhdr(buffer_interface_ptr buf,int * addr,int * lng,int * idtyp,int32_t * primk,int nprim,int32_t * info,int ninfo);
int c_xdfimp(int iun,int32_t * stat,int nstat,word_2 * pri,word_2 * aux,char * vers,char * appl);
int c_xdfini(int iun,buffer_interface_ptr buf,int idtyp,int32_t * keys,int nkeys,int32_t * info,int ninfo);
int c_xdfins(int32_t * buffer,int32_t * donnees,int bitpos,int nelm,int nbits,int datyp);
int c_xdflnk(int * liste,int n);
int c_xdfloc(int iun,int handle,int32_t * primk,int nprim);
int c_xdfloc2(int iun,int handle,int32_t * primk,int nprim,int32_t * mskkeys);
int c_xdfopn(int iun,char * mode,word_2 * pri,int npri,word_2 * aux,int naux,char * appl);
int c_xdfopt(char * optname,char * optc,int optv);
int c_xdfprm(int handle,int * addr,int * lng,int * idtyp,int32_t * primk,int nprim);
int c_xdfput(int iun,int handle,buffer_interface_ptr buf);
int c_xdfrep(int32_t * buffer,int32_t * donnees,int bitpos,int nelm,int nbits,int datyp);
int c_xdfsta(int iun,int32_t * stat,int nstat,word_2 * pri,int npri,word_2 * aux,int naux,char * vers,char * appl);
int c_xdfunl(int * liste,int n);
int c_xdfupd(int iun,buffer_interface_ptr buf,int idtyp,int32_t * keys,int nkeys,int32_t * info,int ninfo);
int c_xdfuse(int src_unit,int dest_unit);
int c_xdfxtr(int32_t * buffer,int32_t * donnees,int bitpos,int nelm,int nbits,int datyp);
int error_msg(char * function_name,int errcode,int errlevel);
int file_index(int iun);
int fnom_index(int iun);
void build_fstd_info_keys(int32_t * buf,int32_t * keys,int index,int mode);
void build_fstd_prim_keys(int32_t * buf,int32_t * keys,int32_t * mask,int32_t * mskkeys,int index,int mode);


// interp/ez_addgrid.c
int c_ez_addgrid(int grid_index,_Grille * newgr);
int c_ez_refgrid(int grid_index);


// interp/ez_calc_crc.c
unsigned int ez_calc_crc(int * p,int * flen,float * ax,float * ay,int ni,int nj);


// interp/ez_calcdist.c
void c_ez_calcarea(float * area,float lats[],float lons[]);
void c_ez_calcarea2(double * area,float lats[],float lons[]);
void c_ez_calcarea_rect(float * area,float lat1,float lon1,float lat2,float lon2);
void c_ez_calcdist(float * distance,float lat1,float lon1,float lat2,float lon2);
void c_ez_calcdist2(double * distance,float lat1,float lon1,float lat2,float lon2);


// interp/ez_calclatlon.c
int32_t ez_calclatlon(int32_t gdid);


// interp/ez_calcnpolarwind.c
int32_t ez_calcnpolarwind(float * polar_uu_in,float * polar_vv_in,float * uuin,float * vvin,int32_t ni,int32_t nj,int32_t gdin);


// interp/ez_calcntncof.c
void ez_calcntncof(int32_t gdid);


// interp/ez_calcspolarwind.c
int32_t ez_calcspolarwind(float * polar_uu_in,float * polar_vv_in,float * uuin,float * vvin,int32_t ni,int32_t nj,int32_t gdin);


// interp/ez_calcxy.c
int32_t ez_calcxy(int32_t gdin,int32_t gdout);


// interp/ez_corrval.c
int32_t ez_corrval(float * zout,float * zin,int32_t gdin,int32_t gdout);


// interp/ez_corrval_aunord.c
int32_t ez_corrval_aunord(float * zout,float * zin,int32_t gdin,int32_t gdout);


// interp/ez_corrval_ausud.c
int32_t ez_corrval_ausud(float * zout,float * zin,int32_t gdin,int32_t gdout);


// interp/ez_corrvec.c
int32_t ez_corrvec(float * uuout,float * vvout,float * uuin,float * vvin,int32_t gdin,int32_t gdout);


// interp/ez_corrvec_aunord.c
int32_t ez_corrvec_aunord(float * uuout,float * vvout,float * uuin,float * vvin,int32_t gdin,int32_t gdout);


// interp/ez_corrvec_ausud.c
int32_t ez_corrvec_ausud(float * uuout,float * vvout,float * uuin,float * vvin,int32_t gdin,int32_t gdout);


// interp/ez_defaxes.c
void c_ezdefaxes(int32_t gdid,float * ax,float * ay);


// interp/ez_defxg.c
void c_ezdefxg(int32_t gdid);


// interp/ez_defzone_dehors.c
int32_t ez_defzone_dehors(int32_t gdin,float * x,float * y,int32_t npts,_zone * zone);


// interp/ez_defzone_est.c
int32_t ez_defzone_est(int32_t gdin,float * x,float * y,int32_t npts,_zone * zone);


// interp/ez_defzone_nord.c
int32_t ez_defzone_nord(int32_t gdin,float * x,float * y,int32_t npts,_zone * zone);


// interp/ez_defzone_ouest.c
int32_t ez_defzone_ouest(int32_t gdin,float * x,float * y,int32_t npts,_zone * zone);


// interp/ez_defzone_polenord.c
int32_t ez_defzone_polenord(int32_t gdin,float * x,float * y,int32_t npts,_zone * zone);


// interp/ez_defzone_polesud.c
int32_t ez_defzone_polesud(int32_t gdin,float * x,float * y,int32_t npts,_zone * zone);


// interp/ez_defzone_sud.c
int32_t ez_defzone_sud(int32_t gdin,float * x,float * y,int32_t npts,_zone * zone);


// interp/ez_defzones.c
int32_t ez_defzones(int32_t gdin,int32_t gdout);


// interp/ez_eliminer_grille.c
void EliminerGrille(int32_t gdid);


// interp/ez_find_gdin.c
int c_find_gdin(int gdin,int gdout);


// interp/ez_find_gdin_in_gset.c
int32_t ez_find_gdin_in_gset(int32_t gdin,int32_t gdout);


// interp/ez_findgrid.c
int c_ez_findgrid(int grid_index,_Grille * gr);
void dump_gr_list();


// interp/ez_freegridset.c
int32_t c_ezfreegridset(int32_t gdid,int32_t index);


// interp/ez_ftnstrclean.c
- ftnstrclean(str,lenstr);


// interp/ez_gdgxpndaxes.c
int32_t c_gdgxpndaxes(int32_t gdid,float * ax,float * ay);


// interp/ez_genpole.c
int32_t c_ezgenpole(float * vpolnor,float * vpolsud,float * fld,int32_t ni,int32_t nj,int32_t vecteur,char * grtyp,int32_t hem);


// interp/ez_gfllfxy.c
void c_ezgfllfxy(float * lonp,float * latp,float * lon,float * lat,float * r,float * ri,int32_t * npts,float * xlat1,float * xlon1,float * xlat2,float * xlon2);


// interp/ez_gfwfllw.c
void c_ezgfwfllw(float * uullout,float * vvllout,float * latin,float * lonin,float * xlatingf,float * xloningf,int32_t * ni,int32_t * nj,char * grtyp,int32_t * ig1,int32_t * ig2,int32_t * ig3,int32_t * ig4);


// interp/ez_gfxyfll.c
void c_ezgfxyfll(float * lonp,float * latp,float * lon,float * lat,float * r,float * ri,int32_t * npts,float * xlat1,float * xlon1,float * xlat2,float * xlon2);


// interp/ez_identifygrid.c
int32_t c_ezidentify_irreg_grid(int32_t ni,int32_t nj,char * grtyp,char * grref,int32_t ig1,int32_t ig2,int32_t ig3,int32_t ig4,float * ax,float * ay);
int32_t c_ezidentify_reg_grid(int32_t ni,int32_t nj,char * grtyp,int32_t ig1,int32_t ig2,int32_t ig3,int32_t ig4);
void c_ez_manageGrillesMemory();


// interp/ez_interp.c
int32_t ez_interp(float * zout,float * zin,int32_t gdin,int32_t gdout);


// interp/ez_lireEnrPositionnels.c
int32_t LireEnrPositionnels(_Grille * gr,int32_t iunit,int32_t ip1,int32_t ip2,int32_t ip3,int32_t ip4,int32_t read);
void Lire_enrTicTac(_Grille * gr,float * ax,int32_t nixnjx,float * ay,int32_t niynjy,int32_t ip3,int32_t ip4);
void Lire_enrUvercode1(_Grille * gr,float * yy,int32_t nix);
void RemplirDeBlancs(char str[],int32_t longueur);


// interp/ez_llfgr.c
void c_llfgr(float * lat,float * lon,float * x,float * y,int32_t npts,float latOrigine,float lonOrigine,float deltaLat,float deltaLon);


// interp/ez_llwfgfw.c
void c_ezllwfgfw(float * uullout,float * vvllout,float * latin,float * lonin,float * xlatingf,float * xloningf,int32_t * ni,int32_t * nj,char * grtyp,int32_t * ig1,int32_t * ig2,int32_t * ig3,int32_t * ig4);


// interp/ez_mask.c
int c_ezget_mask_zones(int * mask_out,int * mask_in);
int c_ezsint_m(float * zout,float * zin);
int c_ezsint_mask(int * mask_out,int * mask_in);
int c_ezsint_mdm(float * zout,int * mask_out,float * zin,int * mask_in);
int c_ezuvint_m(float * uuout,float * vvout,float * uuin,float * vvin);
int c_ezuvint_mdm(float * uuout,float * vvout,int * mask_out,float * uuin,float * vvin,int * mask_in);
int c_gdgetmask(int gdid,int * mask);
int c_gdsetmask(int gdid,int * mask);


// interp/ez_sincoslatlon.c
int32_t c_ezsincoslatlon(float * lat,float * lon,float * sinlat,float * sinlon,float * coslat,float * coslon,int32_t npts);


// interp/ez_util.c
- c_ezsetgdout(gdout);
int32_t c_ezgetgdin();
int32_t c_ezgetgdout();


// interp/ez_xpncof.c
int32_t ez_calcxpncof(int32_t gdid);
void ez_xpncof(int32_t * i1,int32_t * i2,int32_t * j1,int32_t * j2,int32_t * extension,int32_t ni,int32_t nj,char grtyp,char grref,int32_t ig1,int32_t ig2,int32_t ig3,int32_t ig4,int32_t sym,float * ax,float * ay);


// interp/ez_xpnsrcgd.c
void ez_xpnsrcgd(int32_t gdid,float * zout,float * zin);


// interp/ezdefset.c
int32_t c_ezdefset(int32_t gdout,int32_t gdin);
void allocate_gridset_table(int gdid);
void reallocate_gridset_table(int gdid);


// interp/ezgdef.c
int32_t c_ezgdef(int32_t ni,int32_t nj,char * grtyp,char * grref,int32_t ig1,int32_t ig2,int32_t ig3,int32_t ig4,float * ax,float * ay);


// interp/ezgdef_ffile.c
int32_t c_ezgdef_ffile(int32_t ni,int32_t nj,char * grtyp,int32_t ig1,int32_t ig2,int32_t ig3,int32_t ig4,int32_t iunit);


// interp/ezgdef_fll.c
int32_t c_ezgdef_fll(int32_t ni,int32_t nj,float * lat,float * lon);


// interp/ezgdef_fmem.c
int32_t c_ezgdef_fmem(int32_t ni,int32_t nj,char * grtyp,char * grref,int32_t ig1,int32_t ig2,int32_t ig3,int32_t ig4,float * ax,float * ay);


// interp/ezgdef_supergrid.c
int32_t c_ezgdef_supergrid(int32_t ni,int32_t nj,char * grtyp,char * grref,int32_t vercode,int32_t nsubgrids,int32_t * subgrid);


// interp/ezgdef_yymask.c
int32_t c_ezgdef_yymask(_Grille * subgd);


// interp/ezget_nsubgrids.c
int32_t c_ezget_nsubgrids(int32_t gdid);


// interp/ezget_subgridids.c
int32_t c_ezget_subgridids(int32_t gdid,int32_t * subgrid);


// interp/ezgetopt.c
int32_t c_ezgetopt(char * option,char * value);


// interp/ezgetval.c
int32_t c_ezgetival(char * option,int32_t * ivalue);
int32_t c_ezgetval(char * option,float * fvalue);


// interp/ezgfstp.c
int32_t c_ezgfstp(int32_t gdid,char * nomvarx,char * typvarx,char * etiketx,char * nomvary,char * typvary,char * etikety,int32_t * ip1,int32_t * ip2,int32_t * ip3,int32_t * dateo,int32_t * deet,int32_t * npas,int32_t * nbits);


// interp/ezgprm.c
int32_t c_ezgprm(int32_t gdid,char * grtyp,int32_t * ni,int32_t * nj,int32_t * ig1,int32_t * ig2,int32_t * ig3,int32_t * ig4);


// interp/ezgxprm.c
int32_t c_ezgxprm(int32_t gdid,int32_t * ni,int32_t * nj,char * grtyp,int32_t * ig1,int32_t * ig2,int32_t * ig3,int32_t * ig4,char * grref,int32_t * ig1ref,int32_t * ig2ref,int32_t * ig3ref,int32_t * ig4ref);


// interp/ezqkdef.c
int32_t c_ezqkdef(int32_t ni,int32_t nj,char * grtyp,int32_t ig1,int32_t ig2,int32_t ig3,int32_t ig4,int32_t iunit);


// interp/ezsetopt.c
int32_t c_ezsetopt(char * option,char * value);


// interp/ezsetval.c
int32_t c_ezsetfval(char * option,float fvalue);
int32_t c_ezsetival(char * option,int32_t ivalue);
int32_t c_ezsetval(char * option,float fvalue);
int32_t c_ezsetval2(char * option,float * fvalue);


// interp/ezsint.c
int32_t c_ezsint(float * zout,float * zin);
int32_t c_ezsint_orig(float * zout,float * zin);


// interp/ezuvint.c
int32_t c_ezuvint(float * uuout,float * vvout,float * uuin,float * vvin);
int32_t c_ezuvint_orig(float * uuout,float * vvout,float * uuin,float * vvin);


// interp/ezwdint.c
int32_t c_ezwdint(float * uuout,float * vvout,float * uuin,float * vvin);
int32_t c_ezwdint_orig(float * uuout,float * vvout,float * uuin,float * vvin);


// interp/ezyy_calcxy.c
int32_t c_ezyy_calcxy(int32_t gdout,int32_t gdin);


// interp/ezyymint.c
int32_t c_ezyymint(int32_t gdout,int32_t gdin,int32_t ni,int32_t nj,float * maskout,float * dlat,float * dlon,float * yinlat,float * yinlon,int32_t * yyincount,float * yanlat,float * yanlon,int32_t * yyancount);


// interp/ezyysint.c
int32_t c_ezyysint(float * zout,float * zin,int32_t gdout,int32_t gdin);


// interp/ezyyuvint.c
int32_t c_ezyyuvint(float * uuout,float * vvout,float * uuin,float * vvin,int32_t gdout,int32_t gdin);


// interp/ezyywdint.c
int32_t c_ezyywdint(float * uuout,float * vvout,float * uuin,float * vvin,int32_t gdout,int32_t gdin);


// interp/gdgaxes.c
int32_t c_gdgaxes(int32_t gdid,float * ax,float * ay);


// interp/gdinterp.c
int c_gdcompatible_grids(int gdin,int gdout);
int c_gd_isgridrotated(int gdid);
int32_t c_gdinterp(float * zout,float * zin,int32_t gdin,float * x,float * y,int32_t npts);


// interp/gdll.c
int32_t c_gdll(int32_t gdid,float * lat,float * lon);
int32_t c_gdll_orig(int32_t gdid,float * lat,float * lon);


// interp/gdllfxy.c
int32_t c_gdllfxy(int32_t gdid,float * lat,float * lon,float * x,float * y,int32_t n);
int32_t c_gdllfxy_new(int32_t gdid,float * lat,float * lon,float * x,float * y,int32_t n);
int32_t c_gdllfxy_orig(int32_t gdid,float * lat,float * lon,float * x,float * y,int32_t n);


// interp/gdllfxyz.c
int32_t c_gdllfxyz(int32_t gdid,float * lat,float * lon,float * x,float * y,int32_t n);


// interp/gdllsval.c
int32_t c_gdllsval(int32_t gdid,float * zout,float * zin,float * lat,float * lon,int32_t n);


// interp/gdllvval.c
int32_t c_gdllvval(int32_t gdid,float * uuout,float * vvout,float * uuin,float * vvin,float * lat,float * lon,int32_t n);


// interp/gdllwdval.c
int32_t c_gdllwdval(int32_t gdid,float * uuout,float * vvout,float * uuin,float * vvin,float * lat,float * lon,int32_t n);


// interp/gdrls.c
int32_t c_gdrls(int32_t gdin);


// interp/gduvfwd.c
int32_t c_gduvfwd(int32_t gdid,float * uugdout,float * vvgdout,float * uullin,float * vvllin,float * latin,float * lonin,int32_t npts);
int32_t c_gduvfwd_orig(int32_t gdid,float * uugdout,float * vvgdout,float * uullin,float * vvllin,float * latin,float * lonin,int32_t npts);


// interp/gdwdfuv.c
int32_t c_gdwdfuv(int32_t gdid,float * spd_out,float * wd_out,float * uuin,float * vvin,float * latin,float * lonin,int32_t npts);
int32_t c_gdwdfuv_orig(int32_t gdid,float * spd_out,float * wd_out,float * uuin,float * vvin,float * latin,float * lonin,int32_t npts);


// interp/gdxpncf.c
int32_t c_gdxpncf(int32_t gdin,int32_t * i1,int32_t * i2,int32_t * j1,int32_t * j2);


// interp/gdxyfll.c
int32_t c_gdxyfll(int32_t gdid,float * x,float * y,float * lat,float * lon,int32_t n);
int32_t c_gdxyfll_new(int32_t gdid,float * x,float * y,float * lat,float * lon,int32_t n);
int32_t c_gdxyfll_orig(int32_t gdid,float * x,float * y,float * lat,float * lon,int32_t n);


// interp/gdxysint.c
int32_t c_gdxysint(float * zout,float * zin,int32_t gdin,float * x,float * y,int32_t npts);


// interp/gdxysval.c
int32_t c_gdxysval(int32_t gdin,float * zout,float * zin,float * x,float * y,int32_t n);
int32_t c_gdxysval_orig(int32_t gdin,float * zout,float * zin,float * x,float * y,int32_t n);
void ez_freezones(_gridset * gdset);


// interp/gdxyvval.c
int32_t c_gdxyvval(int32_t gdin,float * uuout,float * vvout,float * uuin,float * vvin,float * x,float * y,int32_t n);
int32_t c_gdxyvval_orig(int32_t gdin,float * uuout,float * vvout,float * uuin,float * vvin,float * x,float * y,int32_t n);


// interp/gdxywdval.c
int32_t c_gdxywdval(int32_t gdin,float * uuout,float * vvout,float * uuin,float * vvin,float * x,float * y,int32_t n);


// interp/gdxyzfll.c
int32_t c_gdxyzfll(int32_t gdid,float * x,float * y,float * lat,float * lon,int32_t n);


// packers/compact_integer.c
int compact_char(void * unpackedArrayOfBytes,void * packedHeader,void * packedArrayOfInt,int elementCount,int bitSizeOfPackedToken,int off_set,int stride,int opCode);
int compact_integer(void * unpackedArrayOfInt,void * packedHeader,void * packedArrayOfInt,int elementCount,int bitSizeOfPackedToken,int off_set,int stride,int opCode);
int compact_short(void * unpackedArrayOfShort,void * packedHeader,void * packedArrayOfInt,int elementCount,int bitSizeOfPackedToken,int off_set,int stride,int opCode);


// packers/compact_rle.c
int compact_rle(void * unpackedArrayOfInt,void * packedHeader,void * packedArrayOfInt,int max,int min,int elementCount,int bitSizeOfPackedToken,int off_set,int stride,int opCode);


// packers/float_packer.c
int32_t c_float_packer(float * source,int32_t nbits,int32_t * header,int32_t * stream,int32_t npts);
int32_t c_float_unpacker(float * dest,int32_t * header,int32_t * stream,int32_t npts,int32_t * nbits);
int32_t float_packer_1(float * source,int32_t nbits,int32_t * header,int32_t * stream,int32_t npts);
int32_t float_unpacker_1(float * dest,int32_t * header,int32_t * stream,int32_t npts);
void c_float_packer_params(int32_t * header_size,int32_t * stream_size,int32_t * p1,int32_t * p2,int32_t npts);


// packers/unpackWrapper.c
void unpackWrapper(void * unpackedArray,void * packedHeader,void * packedArray,int stride,void * missingValueTag);


// primitives/DlInterface.c
char * DlError(void);
char * _DlError_(void);
int DlClose(void * handle);
int _DlClose_(void * handle);
void * DlOpen(const char * filename,int flag);
void * DlSym(void * handle,const char * symbol);
void * _DlSym_(void * handle,const char * symbol);
void DlRegister(void * open,void * sym,void * error,void * close);


// primitives/appl_var.c
int get_appl_var(char * varname,char * value,int ln,int lng);
void free_appl_var_table();
void init_appl_var_table();
void set_appl_var(char * name,char * value,int ln,int lv);


// primitives/arc4.c
int arc4_self_test(int verbose);
void arc4_crypt(arc4_context * ctx,unsigned char * buf,int buflen);
void arc4_setup(arc4_context * ctx,unsigned char * key,int keylen);


// primitives/c_baseio.c
int c_fclos(int iun);
int c_fnom(int * iun,char * nom,char * type,int lrec);
int c_fretour(int iun);
int c_getfdsc(int iun);
int c_sqgets(int iun,char * bufptr,int nchar);
int c_sqgetw(int iun,int32_t * bufptr,int nmots);
int c_sqputs(int iun,char * bufptr,int nchar);
int c_sqputw(int iun,int32_t * bufptr,int nmots);
int c_waclos2(int iun);
int c_waopen2(int iun);
int c_waread2(int iun,void * buf,unsigned int adr,int nmots);
int c_wawrit2(int iun,void * buf,unsigned int adr,int nmots);
int fnom_rem_connect(int ind,char * remote_host);
int32_t c_numblks(int iun);
int32_t c_wasize(int iun);
void c_checda(int iun);
void c_closda(int iun);
void c_openda(int iun);
void c_readda(int iun,int * bufptr,int ns,int is);
void c_socket_open();
void c_sqclos(int iun);
void c_sqeoi(int iun);
void c_sqopen(int iun);
void c_sqrew(int iun);
void c_waclos(int iun);
void c_waopen(int iun);
void c_waread(int iun,void * buf,unsigned int adr,int nmots);
void c_wawrit(int iun,void * buf,unsigned int adr,int nmots);
void c_writda(int iun,int * bufptr,int ns,int is);


// primitives/c_ccard.c
char ** c_jfc_positionel(char ** argv,struct c_jfc_definition les_clefs[],int n,int pos,int * npos,int debut,int * erreur);
int c_jfc_cherche_la_clef(char * nom,struct c_jfc_definition cle[],int n);
int c_jfc_majmin(char * arg);
void c_ccard(char ** argv,int argc,char ** cle,char val[][NCARMAX],char ** def,int n,int * npos);
void c_jfc_les_valeurs(struct c_jfc_definition clefs[],char ** argv,int pos,int * posc);
void c_jfc_traduire(char * cle,int cletype);
void c_jfc_tradup(char * nom,char * nommaj);
void sequence_appel(struct c_jfc_definition defo[],char * scriptnom,int n);


// primitives/c_env_var_cracker.c
int check_start_end_char(char * string,int length);
void c_env_var_cracker(char * fstoption,void (* user_function)(),char * lang);


// primitives/calc_crc.c
int calc_crc(unsigned char * buffer,int * len_p,int * seed_p,int stride);
int f_calc_crc(unsigned char * buffer,int * f_flen,int * f_fseed,int * f_stride);
int f_calc_crc_(unsigned char * buffer,int * f_flen,int * f_fseed,int * f_stride);
int f_calc_crc__(unsigned char * buffer,int * f_flen,int * f_fseed,int * f_stride);


// primitives/clib_interface.c
int mkpath(char * s,mode_t mode);


// primitives/client_timeout.c
int get_client_timeout(int fclient);
int get_timeout_value(int fclient);
void init_client_table(int channel);
void set_client_timeout(int fclient,int timeout);


// primitives/cpu_type.c
double rdtscp_seconds(void);
double rdtsc_seconds(void);
double wall_clock_seconds(uint64_t ticks);
int cpu_has_feature(int flag);
int get_cpu_cores();
int get_cpu_core_thread();
int get_cpu_hyperthreads();
int get_cpu_id();
int get_fp_status_ctl(void);
int main_cpuid(int argc,char ** argv);
int main_rdtsc(int argc,char ** argv);
uint64_t get_cpu_freq(void);
uint64_t rdtsc(void);
uint64_t rdtscp(void);
void set_fp_status_ctl(int fpstat_in);


// primitives/crc16_.c
crc16_t crc16_reflect(crc16_t data,size_t data_len);
crc16_t crc16_update(crc16_t crc,const unsigned char * data,size_t data_len);
crc16_t crc16_update_le(crc16_t crc,const unsigned char * data,size_t data_len,int mask);


// primitives/crc24_.c
crc24_t crc24_update(crc24_t crc,const unsigned char * data,size_t data_len);
crc24_t crc24_update_le(crc24_t crc,const unsigned char * data,size_t data_len,int mask);


// primitives/crc32.c
- crc32(crc,buf,len);
uLong f_crc32(uLong * crc,const Bytef * buf,uInt * len);
uLong f_crc32_(uLong * crc,const Bytef * buf,uInt * len);
uLong f_crc32__(uLong * crc,const Bytef * buf,uInt * len);


// primitives/crc32_.c
crc32_t crc32_reflect(crc32_t data,size_t data_len);
crc32_t crc32_update(crc32_t crc,const unsigned char * data,size_t data_len);
crc32_t crc32_update_le(crc32_t crc,const unsigned char * data,size_t data_len,int mask);


// primitives/crc_16_24_32.c
uint32_t f_update_crc_ne(uint32_t * old_crc,int * crclen,void * data,int * datasiz,int * datalen,int * mode);
uint32_t update_crc_ne(uint32_t old_crc,int crclen,void * data,int datasiz,int datalen,int mode);


// primitives/dmms.c
- bloc_alloc(nbytes,mode);
- bloc_check(ptbloc,msg_level);
- bloc_dealloc(ptbloc,mode);
- mem_check(mode,msg_level);
void c_memuse();


// primitives/f90_threads.c
event * c_create_event();
int c_acquire_lock(pthread_mutex_t * lockno);
int c_check_event(event * user_event);
int c_create_thread(void * (* function)(void *),void * arg);
int c_destroy_event(event * the_event);
int c_destroy_lock(pthread_mutex_t * lockno);
int c_id_thread();
int c_join_thread(int id);
int c_post_event(event * user_event,int value);
int c_release_lock(pthread_mutex_t * lockno);
int c_wait_event(event * user_event,int value);
pthread_mutex_t * c_create_lock();


// primitives/f_gettimeofday.c
int gettimeofday(struct timeval * tv,timezone * tz);


// primitives/flush_stdout.c
void flush_stdout();
void flush_stdout_();
void flush_stdout__();


// primitives/fmain2cmain.c
- c_main(int argc,char ** argv);


// primitives/fool_optimizer.c
void fool_optimizer();
void fool_optimizer_();
void fool_optimizer__();


// primitives/fread32.c
size_t fread16(void * ptr,size_t size,size_t nitems,FILE * stream);
size_t fread32(void * ptr,size_t size,size_t nitems,FILE * stream);
size_t fread64(void * ptr,size_t size,size_t nitems,FILE * stream);
size_t fwrite16(void * ptr,size_t size,size_t nitems,FILE * stream);
size_t fwrite32(void * ptr,size_t size,size_t nitems,FILE * stream);
size_t fwrite64(void * ptr,size_t size,size_t nitems,FILE * stream);


// primitives/ftn2c_helper_c.c
int ftn2c_cstra_fstra(unsigned char ** src,unsigned char * dest,int lsrc,int ldest,int nitems,unsigned char pad);
int ftn2c_fstra_cstra(unsigned char * src,unsigned char ** dest,int lsrc,int ldest,int nitems,unsigned char pad);
int ftn2c_string_copy(unsigned char * src,unsigned char * dest,int lsrc,int ldest,unsigned char pad);


// primitives/get_ccard_arg.c
char * get_ccard_arg_m(char * nom,int ord);


// primitives/get_endian.c
int is_big_endian();
int is_little_endian();


// primitives/gossip_sock.c
char * get_broker_Authorization();
char * get_gossip_dir(int display);
char * get_host_and_port(char * channel_file);
char * get_server_host(char * channel);
char * get_server_name(char * host_ip);
int accept_from_sock(int fserver);
int bind_sock_to_port(int sockfd);
int bind_to_localport(int * port,char * buf,int maxbuf);
int close_channel(int fclient,char * channel);
int cmd_open();
int connect_to_channel_by_name(char * name);
int connect_to_channel_by_name_2(char * name,char * msg);
int connect_to_hostport(char * target);
int connect_to_localport(int port);
int connect_to_server();
int connect_to_subchannel_by_name(char * channel,char * subchannel,char * mode);
int connect_with_timeout(char * ipaddress,int portno,int timeout);
int connect_with_timeout_localport(char * ipaddress,int portno,int timeout);
int GetHostName(char * name,size_t len);
int get_ack_nack(int fserver);
int get_file_size(char * file_name);
int get_ip_address(char * hostname);
int get_own_ip_address();
int get_server_alias(char * path,const char * filename,int maxlen);
int get_sock_net();
int get_status(char * reply);
int get_stream_timeout(int channel);
int get_timeout_signal(int channel);
int read_data_file(char * file_name,char * buffer,int size);
int read_ft_nonblocking_socket(int fd,char * ptr,int n);
int read_ft_nonblocking_socket_count(int fd,char * ptr,int n);
int read_stream(int fd,char * ptr,int nbytes);
int send_command(char * command);
int send_command_to_server(int fserver,char * buf);
int send_command_to_server2(int fclient,char * buffer);
int set_host_and_port(char * channel_file,char * host_and_port);
int set_sock_opt(int sockfd);
int signal_timeout(int channel);
int store_channel_data(char * buffer,int nbytes,char * file_name);
int write_ft_nonblocking_socket(int fd,char * ptr,int n);
int write_record(int fclient,void * record,int size,int tokensize);
int write_stream(int fd,char * ptr,int n);
int32_t get_int32_from_channel(int channel);
long fsize(FILE * fd);
long long time_base();
void * read_record(int fclient,void * records,int * length,int maxlength,int tokensize);
void check_data(char * record,int size);
void check_swap_records(void * record,int size,int tokensize);
void cmd_close(int fclient);
void disable_nagle(int socket);
void ip_to_host_name(char * hostname);
void pack_cmd(char * buffer,char * tmpbuf);
void put_int32_to_channel(int channel,int32_t to_send);
void send_ack_nack(int fclient,int status);
void set_broker_Authorization(int auth_token);
void set_stream_timeout(int channel,int new_time);
void set_timeout_signal(int channel,int option);


// primitives/gossip_thread.c
int exit_from_client(int fclient);
int get_client_count();
int is_exit_requested();
void decrement_client_count();
void exit_from_client_thread(extendedClientSlot * client);
void increment_client_count();
void set_exit_requested();
void start_client_module_2(void (* client_address)(),int client_uid,int client_pid,int fclient,char * command,void * data);
void start_client_thread_2(void (* client_address)(extendedClientSlot *),int client_uid,int client_pid,int fclient,char * command,void * data,void (* user_server)());


// primitives/gossip_timeout.c
int get_ping_interval();
int get_timeout_counter();
int set_timeout_counter(int timeout_value);
void decrement_timeout_counter();
void increment_timeout_counter();
void reset_timeout_counter();
void set_ping_interval(int ping_interval);


// primitives/ibm32_ieee.c
void c_ibm32_ieee(unsigned long * tab_data_IBM,int nb_data);


// primitives/logical_smt_cpus.c
int c_logical_smt_cpus();


// primitives/md5.c
int md5_file(char * path,unsigned char output[16]);
int md5_self_test(int verbose);
int md5_ssh(unsigned char output[16]);
void md5(unsigned char * input,int ilen,unsigned char output[16]);
void md5_finish(md5_context * ctx,unsigned char output[16]);
void md5_hmac(unsigned char * key,int keylen,unsigned char * input,int ilen,unsigned char output[16]);
void md5_hmac_finish(md5_context * ctx,unsigned char output[16]);
void md5_hmac_starts(md5_context * ctx,unsigned char * key,int keylen);
void md5_hmac_update(md5_context * ctx,unsigned char * input,int ilen);
void md5_starts(md5_context * ctx);
void md5_update(md5_context * ctx,unsigned char * input,int ilen);


// primitives/mgilib2.c
int mgi_get_retry_connect(int chan);
int retry_connect(int chan);


// primitives/new_second.c
float second();
float second_();
float second__();


// primitives/plugin_code.c
charptr * plugin_function_names(const plugin * p);
charptr plugin_function_name(const plugin * p,int ordinal);
int plugin_n_functions(const plugin * p);
int unload_plugin(plugin * p);
plugin * load_plugin(const char * lib,int diag);
void * plugin_function(const plugin * p,const char * name);
void set_plugin_diag(int diag);


// primitives/read_direct.c
int process_c_callback(char * filename);
int rpn_c_callback(char * VERB,void * callback,char * OPTIONS,void * Private_data,void * Private_data_2);
void rpn_c_callback_setverbose(int verbose);


// primitives/register_dl_routines.c
void register_dl_routines();


// primitives/resident_time.c
- f_get_my_resident_time(int * wu,int * wh,int * ws);
- f_get_my_resident_time_(int * wu,int * wh,int * ws);
- f_get_my_resident_time__(int * wu,int * wh,int * ws);
void c_get_my_resident_time(int * wu,int * wh,int * ws);
void get_wall_clock_used(char * stepid,char ** hostname,int * wu,int * wh,int * ws);


// primitives/vmm.c
int calc_checksum(int bkno);
int eject_block(int bkno,int save,int fait_checksum);
int eject_from_tableau(int size,int tableau_eject_index);
int fichier_vide(char * nom);
int obtient_environ();
int pack_blocks(int * biggest_free_block_index);
int pack_segment(int bkno,int * biggest_free_block_index);
int qvmindex_from_key(complete_key inkey);
int strfind(char * SousChaine,char * Chaine);
int trouve_best_fit(int size);
int trouve_best_free(int size);
int trouve_best_segment(int size,int * tableau_eject_index);
int verbar(int bkno);
int vmmerr(char * fonction,int32_t valeur);
int32_t qvmlod(complete_key inlkey[],int32_t * nkey);
void collapse_blocks(int i,int inext);
void ecrit_bloc(int bkno,int classe,int32_t * memadresse,int fileadresse,int nmots);
void ecrit_vmm_controle();
void imprime();
void imprime_structures(int mode);
void impval(int32_t * adresse);
void imp_bar(int * valeur);
void lit_bloc(int bkno,unsigned int classe,int32_t * memadresse,int fileadresse,int nmots);
void lit_vmm_controle();
void ouvre_ou_ferme_controle(int ouvre,int premiere_fois,char * fonction);
void reserve_disk_space(int bkno);
void swap_blocks(int i,int inext);
void trie_le_tableau(int * table,int longueur);


// primitives/wkoffit.c
int main(int argc,char ** argv);
int32_t c_wkoffit(char * filePath,int l1);


// whiteboard/WhiteBoard_c.c
int c_wb_check(WhiteBoard * wb,char * name,int optionMask,int nameLength,int print,LineWorker fncptr,void * extra_data);
int c_wb_checkpoint();
int c_wb_checkpoint_get_name(char * filename,int Lfilename);
int c_wb_checkpoint_name(char * filename);
int c_wb_free(WhiteBoard * wb);
int c_wb_get(WhiteBoard * wb,char * name,char type,int size,unsigned char * dest,int nbelem,int nameLength);
int c_wb_lock(WhiteBoard * wb,char * name,int nameLength);
int c_wb_put(WhiteBoard * wb,char * name,char type,int size,unsigned char * src,int nbelem,int options,int nameLength);
int c_wb_read(WhiteBoard * wb,char * filename,char * package,char * section,int options,int filenameLength,int packageLength,int sectionLength);
int c_wb_reload();
int c_wb_verbosity(int level);
WhiteBoard * c_wb_new();
