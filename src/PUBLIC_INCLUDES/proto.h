#if !defined(F2Cl)
#define F2Cl int
#endif
int fnom_index(int iun);
int error_msg(char *function_name, int errcode, int errlevel);
int file_index(int iun);
int32_t f77name(xdfopn)(int32_t *fiun, char *mode, ftnword_2 *pri, int32_t *fnpri,
             ftnword_2 *aux, int32_t *fnaux, char *appl, F2Cl l1, F2Cl l2);
int c_xdfopn(int iun, char *mode, word_2 *pri, int npri,
             word_2 *aux, int naux, char *appl);
int32_t f77name(xdfcls)(int32_t *fiun);
int c_xdfcls(int iun);
int32_t f77name(xdfsta)(int32_t *fiun, int32_t *stat, int32_t *fnstat,
            ftnword_2 *pri, int32_t *fnpri,
            ftnword_2 *aux, int32_t *fnaux,
            char *vers, char *appl, F2Cl l1, F2Cl l2);
int c_xdfsta(int iun, int32_t *stat, int nstat,
                    word_2 *pri, int npri, word_2 *aux, int naux,
                    char *vers, char *appl);
int32_t f77name(xdfimp)(int32_t *fiun, int32_t *stat, int32_t *fnstat,
                    ftnword_2 *pri, ftnword_2 *aux,
                    char *vers, char *appl, F2Cl l1, F2Cl l2);
int c_xdfimp(int iun, int32_t *stat, int nstat, word_2 *pri, word_2 *aux,
                    char *vers, char *appl);
int32_t f77name(xdfini)(int32_t *fiun, int32_t *buf, int32_t *fidtyp,
            int32_t *keys, int32_t *fnkeys,
            int32_t *info, int32_t *fninfo);
int c_xdfini(int iun, buffer_interface_ptr buf, int idtyp,
             int32_t *keys, int nkeys, int32_t *info, int ninfo);

void build_burp_prim_keys(burp_record *brpk, int32_t *keys,
                                 burp_record *mask, int32_t *mskkeys,
                 int index, int mode);
void build_burp_info_keys(int32_t *buf, int32_t *keys, int index, int mode);
void build_fstd_info_keys(int32_t *buf, int32_t *keys, int index, int mode);
void build_fstd_prim_keys(int32_t *buf, int32_t *keys, int32_t *mask, int32_t *mskkeys,
                int index, int mode);
int32_t f77name(xdfadd)(int32_t *buf, int32_t *donnees,
                        int32_t *fnelm, int32_t *fnbits, int32_t *fdatyp);
int c_xdfadd(int32_t *buffer, int32_t *donnees, int nelm, int nbits, int datyp);
int32_t f77name(xdfprm)(int32_t *fhandle, int32_t *addr, int32_t *lng,
                        int32_t *idtyp, int32_t *primk, int32_t *fnprim);
int c_xdfprm(int handle, int *addr, int *lng, int *idtyp,
         int32_t *primk, int nprim);
int32_t f77name(xdfhdr)(int32_t *buf, int32_t *addr, int32_t *lng,
                        int32_t *idtyp, int32_t *primk, int32_t *fnprim,
            int32_t *info, int32_t *fninfo);
int c_xdfhdr(buffer_interface_ptr buf , int *addr, int *lng, int *idtyp,
         int32_t *primk, int nprim, int32_t *info, int ninfo);
int32_t f77name(xdfloc)(int32_t *fiun, int32_t *fhandle, int32_t *primk,
            int32_t *fnprim);
int c_xdfloc(int iun, int handle, int32_t *primk, int nprim);
int c_xdfloc2(int iun, int handle, int32_t *primk, int nprim, int32_t *mskkeys);
int32_t f77name(xdfget)(int32_t *fhandle, int32_t *buf);
int c_xdfget(int handle, buffer_interface_ptr buf);
int32_t f77name(xdfput)(int32_t *fiun, int32_t *fhandle,
            int32_t *buf);
int c_xdfput(int iun, int handle, buffer_interface_ptr buf);
int32_t f77name(xdfopt)(char *foptname, char *foptc, int32_t *foptv,
            F2Cl l1, F2Cl l2);
int c_xdfopt(char *optname, char *optc, int optv);
int32_t f77name(xdfgop)(char *foptname, char *foptc, int32_t *foptv,
            F2Cl l1, F2Cl l2);
int c_xdfgop(char *optname, char *optc, int *optv);
int32_t f77name(xdfins)(int32_t *buf, int32_t *donnees,
                        int32_t *fbitpos, int32_t *fnelm,
            int32_t *fnbits, int32_t *fdatyp);
int c_xdfins(int32_t *buffer, int32_t *donnees, int bitpos,
             int nelm, int nbits, int datyp);
int32_t f77name(xdfxtr)(int32_t *buf, int32_t *donnees,
                        int32_t *fbitpos, int32_t *fnelm,
            int32_t *fnbits, int32_t *fdatyp);
int c_xdfxtr(int32_t *buffer, int32_t *donnees, int bitpos,
             int nelm, int nbits, int datyp);
int32_t f77name(xdfrep)(int32_t *buf, int32_t *donnees,
                        int32_t *fbitpos, int32_t *fnelm,
            int32_t *fnbits, int32_t *fdatyp);
int c_xdfrep(int32_t *buffer, int32_t *donnees, int bitpos,
         int nelm, int nbits, int datyp);
int32_t f77name(xdfcut)(int32_t *buf,
            int32_t *fbitpos, int32_t *fnelm,
            int32_t *fnbits, int32_t *fdatyp);
int c_xdfcut(void *buffer, int bitpos, int nelm, int nbits, int datyp);
int32_t f77name(xdfupd)(int32_t *fiun, int32_t *buf,
            int32_t *fidtyp,
            int32_t *keys, int32_t *fnkeys,
            int32_t *info, int32_t *fninfo);
int c_xdfupd(int iun, buffer_interface_ptr buf, int idtyp,
             int32_t *keys, int nkeys, int32_t *info, int ninfo);
int32_t f77name(xdfuse)(int32_t *fsrc_unit, int32_t *fdest_unit);
int c_xdfuse(int src_unit, int dest_unit);
int32_t f77name(xdfcle)(char *fkeyname, int32_t *fbit1, int32_t *flkey,
            int32_t *ftkey, int32_t *fdesc1, int32_t *fdesc2, F2Cl l1);
int c_xdfcle(char *keyname, int bit1, int lkey, int tkey, int *desc1, int *desc2);
int c_qdfmsig(int iun, char* newappl);
int32_t f77name(qdfmsig)(int32_t *fiun, char *appl, F2Cl l1);
int32_t f77name(mrbdel)(int32_t *buf, int32_t *f_number);
int c_mrbdel(void *buffer, int number);
int32_t f77name(xdflnk)(int32_t *liste, int32_t *fn);
int32_t f77name(fstvoi)(int32_t *f_iun, char *options, F2Cl l1);
int32_t f77name(fstouv)(int32_t *f_iun, char *options, F2Cl l1);
int32_t f77name(secateur)(char *filename, int32_t *f_where, F2Cl l1);
int c_xdfcheck(const char* filename);
