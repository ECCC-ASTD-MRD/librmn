char **allocate_string_array(int ns);
int armn_compress(unsigned char *fld, int ni, int nj, int nk, int nbits, int op_code);
void calcule_entropie(float *entropie, unsigned short *bitstream, int npts, int nbits);
void calcul_ajusxy(int *ajus_x, int *ajus_y, int ni, int nj, int istep);
void calcul_ajusxy(int *ajus_x, int *ajus_y, int ni, int nj, int istep);
void calcul_ninjcoarse(int *nicoarse, int *njcoarse, int ni, int nj, int ajus_x, int ajus_y, int istep);
extern ftnword calc_crc(unsigned char *p, ftnword *flen, ftnword *fseed, ftnword stride);
char CAT(PROJECT_NAME,_version)[] = VERSION;
char CAT(PROJECT_NAME,_ec_arch)[] = EC_ARCH;
void check_data(char *record, int size);
int check_start_end_char(char *var, int length);
extern int close_channel (int fclient, char *channel);
int compact_mask_char(unsigned int *dest, unsigned char *src, int npts);
extern int connect_to_subchannel_by_name (char *channel, char *subchannel, char *mode);
int connect_with_timeout(char *ipaddress, int portno, int timeout);
int connect_with_timeout_localport(char *ipaddress, int portno, int timeout);
int cpu_has_feature_(int flag);
int cpu_has_feature__(int flag);
void c_84bits_ig_get(ftnword *ig1, ftnword *ig2, ftnword *ig3, ftnword *ig4, ftnword *the_84bit_token);
void c_84bits_ig_put(ftnword *ig1, ftnword *ig2, ftnword *ig3, ftnword *ig4, ftnword *the_84bit_token);
void c_84bits_ip_get(ftnword *ip1, ftnword *ip2, ftnword *ip3, ftnword *the_84bit_token);
void c_84bits_ip_put(ftnword *ip1, ftnword *ip2, ftnword *ip3, ftnword *the_84bit_token);
void c_84bits_token(ftnword *the_84bit_token, unsigned char *grib_header, ftnword length_grib_header);
int c_armn_compress32(unsigned char *, float *, int, int, int, int);
int c_armn_compress_getlevel();
int c_armn_compress_getlevel();
int c_armn_compress_getswap();
void c_armn_compress_option(char *option, char *value);
void c_armn_compress_setlevel(int level);
void c_armn_compress_setswap(int swapState);
int c_armn_uncompress32(float *fld, unsigned char *zstream, int ni, int nj, int nk, int nchiffres_sign);
void c_def_84bitkey(ftnword *ip1, ftnword *ip2, ftnword *ip3, ftnword *ig1, ftnword *ig2, 
void c_fstunzip(unsigned int *fld, unsigned int *zfld, int ni, int nj, int nbits); 
void c_fstunzip(unsigned int *fld, unsigned int *zfld, int ni, int nj, int nbits); 
void c_fstunzip_minimum(unsigned short *fld, unsigned int *zfld, int ni, int nj, int step, int nbits, word *header);
void c_fstunzip_parallelogram(unsigned short *fld, unsigned int *zfld, int ni, int nj, int step, int nbits, word *header);
void c_fstunzip_sample(unsigned short *fld, unsigned int *zfld, int ni, int nj, int step, int nbits, word *header);
void c_fstzip(unsigned int *zfld, int *zlng, unsigned int *fld, int ni, int nj, int code_methode, int degre, int step, int nbits, int bzip);
int c_fstzip32(unsigned int *zfld, unsigned int *fld, int ni, int nj, int nk, int step, int nbits, int remaining_space);
int c_fstzip_getlevel(); 
void c_fstzip_minimum(unsigned int *zfld, int *zlng, unsigned short *fld, int ni, int nj, int step, int nbits, word *header);
int c_fstzip_parallelogram32(unsigned int *zfld, int *zlng, unsigned int *fld, int ni, int nj, int step, int nbits, word *header);
void c_fstzip_sample(unsigned int *zfld, int *zlng, unsigned short *fld, int ni, int nj, int step, int nbits, word *header);
void c_fstzip_setlevel(int level);
int C_fst_match_req(int set_nb, int handle);
void c_igaip84(ftnword *ip1, ftnword *ip2, ftnword *ip3, ftnword *ig1, ftnword *ig2, ftnword *ig3, ftnword *ig4);
void c_ipaig84(ftnword *ig1, ftnword *ig2, ftnword *ig3, ftnword *ig4, ftnword *ip1, ftnword *ip2, ftnword *ip3);
void C_requetes_init(char *f1,char *f2);
int c_wb_reload();
void DecodeMissingValue(void *field,int nvalues,int datatype,int is_byte,int is_short,int is_double);
void decode_missing_value_(void *field,int *nvalues,int *datatype,int *is_byte,int *is_short,int *is_double);
void decode_missing_value__(void *field,int *nvalues,int *datatype,int *is_byte,int *is_short,int *is_double);
void *DlOpen(const char *filename, int flag);
void DlRegister(void *open, void *sym, void *error, void *close);
void *DlSym(void *handle, const char *symbol);
int EncodeMissingValue(void *field,void *field2,int nvalues,int datatype,int nbits,int is_byte,int is_short,int is_double);
int encode_missing_value_(void *field,void *field2,int *nvalues,int *datatype,int *nbits,int *is_byte,int *is_short,int *is_double);
int encode_missing_value__(void *field,void *field2,int *nvalues,int *datatype,int *nbits,int *is_byte,int *is_short,int *is_double);
int f77name(armn_compress32)(unsigned char *, float *, int *, int *, int *, int *);
extern long long f77name(f_gettimeofday_micro)();
int f77name(armn_uncompress32)(float *fld, unsigned char *zstream, int *ni, int *nj, int *nk, int *nchiffres_sign);
extern void f77name(f_bits_put)(ftnword *bit_array, ftnword *bits_per_slice, ftnword *slices, ftnword *nslices);
extern void f77name(f_bits_get)(ftnword *bit_array, ftnword *bits_per_slice, ftnword *slices, ftnword *nslices);
void f77name(f_igaip84)(ftnword *ip1, ftnword *ip2, ftnword *ip3, ftnword *ig1, ftnword *ig2, ftnword *ig3, ftnword *ig4);
void f77name(f_ipaig84)(ftnword *ig1, ftnword *ig2, ftnword *ig3, ftnword *ig4, ftnword *ip1, ftnword *ip2, ftnword *ip3);
void f77name(f_def_84bitkey)(ftnword *ip1, ftnword *ip2, ftnword *ip3, ftnword *ig1, ftnword *ig2, 
f77name(qqexit) (val)
f77name(getenvc) ( name, value, len1, len2 )
ftnword f77name(open_db_file) (ftnword * );
ftnword f77name(close_db_file) (ftnword * );
ftnword f77name(read_db_file) (ftnword *, ftnword *, ftnword *);
ftnword f77name(write_db_file) (ftnword *, ftnword *, ftnword *);
ftnword f77name(rewind_db_file) (ftnword *);
void f77name(armn_compress_setlevel)(wordint *level);
f77name(csortr) ( list, index, size )
ftnword f77name(slabini)(char *f_name, ftnword dateo[2], ftnword *f_npas,
ftnword f77name(slabopt)(ftnword *f_proc, ftnword *f_numproc );
ftnword f77name (mgi_init) (char *channel_name, F2Cl lname);
ftnword f77name(slabig34)(unsigned ftnword *f_ig3, unsigned ftnword *f_ig4,
ftnword f77name (mgi_open) (ftnword *f_chan, char *mode, F2Cl lmode);
ftnword f77name (mgi_read) (ftnword *f_chan, void *data, ftnword *f_nelm, char *dtype, F2Cl ltype);
ftnword f77name (mgi_write) (ftnword *f_chan, void *data, ftnword *f_nelm, char *dtype, F2Cl ltype);
ftnword f77name(slabdsc)(ftnword *f_hand, ftnword *f_snum,char *f_gxtyp,
ftnword f77name (mgi_clos) (ftnword *f_chan);
ftnword f77name (mgi_term) ();
void f77name (mgi_set_timeout) (ftnword *chan, ftnword *timeout);
ftnword f77name(slabxtr)(ftnword *f_hand, ftnword *f_snum, ftnword *f_nx,
ftnword f77name(slabend)(ftnword *f_hand, char *f_sf_hand, F2Cl l1);
ftnword f77name (mgi_read_oob) ();
f77name(ipsort) ( index, list, size )
ftnword f77name (mgi_write_oob) ();
f77name(csortr) ( list, index, size ) ;
f77name(csortd) ( list, index, size )
f77name(ipsort8) ( index, list, size )
f77name(csortd) ( list, index, size ) ;
f77name(csorte) ( list, index, size )
f77name(ipsorti) ( index, list, size )
f77name(csorte) ( list, index, size ) ;
f77name(csortc) ( list, index, size, len )
f77name(ipsortc) ( index, list, size, len )
f77name(csortc) ( list, index, size, len ) ;
int f77name(memoirc)(msg_level)
void f77name(dmmsdbg)(dbgr)
void f77name(dmmsnabt)(abort)
f77name(hpalloc)( addr, length, errcode, abort )
f77name(tracebck)();
f77name(hpdeallc)(addr, errcode, abort)
f77name(ca_deallc)(addr, errcode, abort)
f77name(memoirh)(buf,ind,nw)
f77name(memoir)(buf,ind,nw)
f77name(bkcheck)(addr, errcode)
f77name(hpcheck)(errcode)
f77name(mcheck)(errcode)
f77name(bkcheck)(addr, errcode)
f77name(hpalloc)( addr, length, errcode, abort )
f77name(tracebck)();
f77name(hpdeallc)(addr, errcode, abort)
f77name(memoirh)(buf,ind,nw)
f77name(memoir)(buf,ind,nw)
void f77_name(f_logical_move)(void *, void *, wordint *);
void f77_name(f_logical2int)(void *, void *, wordint *);
void f77_name(f_int2logical)(void *, void *, wordint *);
char **fill_string_array(char **string_array, char *farray, int nc, int ns, int rmblanks);
void fixpredflds(int *predfld, int *zc, int ni, int nj, int nicoarse, int njcoarse, int step, int ajus_x, int ajus_y);
int force_missing_value_used_(int *flag);
int force_missing_value_used__(int *flag);
int fst_deactivate_filters_();
int fst_deactivate_filters__();
int fst_reactivate_filters_();
int fst_reactivate_filters__();
void f_requetes_init_();
void f_requetes_init__();
uint32_t f_update_crc_ne_(uint32_t *old_crc, int *crclen, void *data, int *datasiz, int *datalen, int *mode);
uint32_t f_update_crc_ne__(uint32_t *old_crc, int *crclen, void *data, int *datasiz, int *datalen, int *mode);
extern int get_ack_nack (int socket);
extern int get_client_timeout (int fclient);
int get_cpu_cores_();
int get_cpu_cores__();
uint64_t get_cpu_freq_(void);
uint64_t get_cpu_freq__(void);
int get_cpu_hyperthreads_();
int get_cpu_hyperthreads__();
int get_cpu_id_();
int get_cpu_id__();
int get_fp_status_ctl_(void);
int get_fp_status_ctl__(void);
extern char *get_gossip_dir (int display);
char *get_link_address(char *path, const char *filename);
int get_missing_value_flags_(float *f, int *i, unsigned int *ui, double *d, short *s, unsigned short *us,
int get_missing_value_flags__(float *f, int *i, unsigned int *ui, double *d, short *s, unsigned short *us,
int get_timeout_signal( int channel );
void get_wall_clock_used(char *stepid,char **hostname,int *wu,int *wh,int *ws);
void grb_84bits_to_ig1234(ftnword *ig1, ftnword *ig2, ftnword *ig3, ftnword *ig4, ftnword *the_84bit_token);
void grb_84bits_to_ip123(ftnword *ip1, ftnword *ip2, ftnword *ip3, ftnword *the_84bit_token);
int IARGC();
extern void init_client_table ();
void init_comp_settings(char *comp_settings);
int is_big_endian_();
int is_big_endian__();
int is_little_endian_();
int is_little_endian__();
int is_on_coarse(int i, int j, int ni, int nj, int step);
void KindToString(int kind, char *s1, char *s2); /* fortran routine from comvertip_123 */
void make_shorts(unsigned short *z16, unsigned int *z32, int npts);
int missing_value_used_();
int missing_value_used__();
int obtient_environ();
void ouvre_ou_ferme_controle(int , int , char *);
void pack1bitRLE(unsigned int z[], unsigned int *zlng, unsigned char ufld[], int npts);
void pack1bitRLE(unsigned int z[], unsigned int *zlng, unsigned char ufld[], int npts);
void packTokensMinimum(unsigned int z[], int *zlng, unsigned short ufld[], int ni, int nj, int nbits, int istep, word *header);
void packTokensParallelogram(unsigned int z[], unsigned int *zlng, unsigned short ufld[], int ni, int nj, int nbits, int istep, word *header);
void packTokensParallelogram(unsigned int z[], int *zlng, unsigned short ufld[], int ni, int nj, int nbits, int istep, word *header);
void packTokensParallelogram32(unsigned int z[], int *zlng, unsigned int ufld[], int ni, int nj, int nbits, int istep, int remaining_space);
void packTokensParallelogram_8(unsigned int z[], unsigned int *zlng, unsigned char ufld[], int ni, int nj, int nbits, int istep);
void packTokensSample(unsigned int z[], int *zlng, unsigned int zc[], int nicoarse, int njcoarse, int diffs[], int ni, int nj, int nbits, int step, word *header, int start, int end);
void pack_stream_nbits_16(unsigned int z[], unsigned int *zlng, unsigned short ufld[], int npts, int nbits);
void pack_stream_nbits_32(unsigned int z[], unsigned int *zlng, unsigned int ufld[], int npts, int nbits);
void pack_stream_nbits_32(unsigned int z[], unsigned int *zlng, unsigned int ufld[], int npts, int nbits);
void pack_stream_nbits_8(unsigned int z[], unsigned int *zlng, unsigned char ufld[], int npts, int nbits);
uint64_t rdtscp_(void);
double rdtscp_seconds_(void) ;
double rdtscp_seconds__(void) ;
uint64_t rdtscp__(void);
uint64_t rdtsc_(void);
double rdtsc_seconds_(void) ;
double rdtsc_seconds__(void) ;
uint64_t rdtsc__(void);
extern void *read_record (int fclient, void *buf, int *longueur, int maxlongueur, int tokensize);
void register_dl_routines_();
void register_dl_routines__();
void SetMissingValueMapping(int what, int datatype, void *processor_, int is_byte, int is_short, int is_double);
extern void set_client_timeout (int fclient, int timeout);
void set_fp_status_ctl_(int fpstat_in);
void set_fp_status_ctl__(int fpstat_in);
void set_missing_value_flags_(float *f, int *i, unsigned int *ui, double *d, short *s, unsigned short *us,
void set_missing_value_flags__(float *f, int *i, unsigned int *ui, double *d, short *s, unsigned short *us,
void set_missing_value_mapping_(int *what, int *datatype, void *processor_, int *is_byte, int *is_short, int *is_double);
void set_missing_value_mapping__(int *what, int *datatype, void *processor_, int *is_byte, int *is_short, int *is_double);
void set_timeout_signal( int channel, int option );
int signal_timeout( int channel );
int uncompact_mask_char(int *dest, unsigned int *src, int npts);
void unpack1bitRLE(unsigned char ufld[], unsigned int z[], unsigned int *zlng, int npts);
void unpackTokensMinimum(unsigned short ufld[], unsigned int z[], int ni, int nj, int nbits, int istep, word *header);
void unpackTokensParallelogram(unsigned short ufld[], unsigned int z[], int ni, int nj, int nbits, int istep, word *header);
void unpackTokensParallelogram(unsigned short ufld[], unsigned int z[], int ni, int nj, int nbits, int istep, word *header);
void unpackTokensParallelogram32(unsigned int ufld[], unsigned int z[], int ni, int nj, int nbits, int istep);
void unpackTokensParallelogram_8(unsigned char ufld[], unsigned int z[], int ni, int nj, int nbits, int istep);
void unpackTokensSample(unsigned int zc[], int diffs[], unsigned int z[], int nicoarse, int njcoarse, int ni, int nj, int nbits, int step, word *header, int start);
void unpackTokensSample(unsigned int zc[], int diffs[], unsigned int z[], int nicoarse, int njcoarse, int ni, int nj, int nbits, int step, word *header, int start);
void unpack_stream_nbits_16(unsigned short ufld[], unsigned int z[], int npts, int nbits);
void unpack_stream_nbits_32(unsigned int ufld[], unsigned int z[], int npts, int nbits);
void unpack_stream_nbits_8(unsigned char ufld[], unsigned int z[], int npts, int nbits);
void unpack_tokens(unsigned int *ufld, unsigned int *z, int ni, int nj, int nbits);
uint32_t update_crc_ne(uint32_t seed, int crclen, void *data, int datasiz, int datalen, int mode);
double wall_clock_seconds_(uint64_t ticks) ;
double wall_clock_seconds__(uint64_t ticks) ;
extern int write_record (int fclient, void *buf, int longueur, int tokensize);
