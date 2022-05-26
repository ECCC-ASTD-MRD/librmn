#define MAX_NAME     256
#define MAXWAFILES  1024
#define MAXPAGES      10

#define new_age_rd(age) (age + 256)
#define new_age_wr(age) (age + 512)
//! Decay age of page
#define decay(age) (age - (age >> 2))

//! cmcarc file signature
#define CMCARC_SIGN "CMCARCHS"
//! cmcarc version 5 file signature
#define CMCARC_SIGN_V5 "CMCARCH5"

typedef struct {
    uint32_t *page_adr;
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
    long long offset;
} FILEINFO;

typedef struct {
    //! Total file size in 64 bits units
    unsigned char ntc[4];
    //! Data size in 64 bits units
    unsigned char ndc[4];
    char cmcarc_name[MAX_NAME];
} ENTETE_CMCARC;

typedef struct {
    //! Total file size in 64 bits units
    unsigned char ntc[8];
    //! Data size in 64 bits units
    unsigned char ndc[8];
    char cmcarc_name[MAX_NAME];
} ENTETE_CMCARC_V5;
