#define MAX_NAME     256
#define MAXWAFILES     128
#define MAXPAGES     10


#define new_age_rd(age) (age+256)
#define new_age_wr(age) (age+512)
#define decay(age)      (age - (age >> 2))
#if defined (NEC)
/* long long llseek(); */
#define LLSK long long
#define LSEEK llseek
#else
#define LLSK long long
#define LSEEK lseek64
#endif
#define WSEEK(fdesc,offst,posi)\
 {\
  LLSK local_off;\
  local_off = offst;\
  LSEEK(fdesc,local_off * sizeof(word),posi);\
 }

#define CMCARC_SIGN "CMCARCHS"  /* signature du debut d'un fichier cmcarc */

typedef struct {
   word *page_adr;
   int wa0;
   int walast;
   int access_count;
   int last_access;
   int touch_flag;
   int not_used_pad_for_word_alignment;
   } PAGEINFO;

typedef struct {
   int file_desc;
   int nb_page_in_use;
   PAGEINFO page[MAXPAGES];
   long offset;
   } FILEINFO;

typedef struct {
   unsigned char ntc[4];        /* nt (longueur totale du fichier) 64 bits */
   unsigned char ndc[4];        /* nd (longueur des donnees) 64 bits */
   char cmcarc_name[MAX_NAME];
   } ENTETE_CMCARC;

