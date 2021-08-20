#include <stdint.h>

//! Maximum number of open slab files
#define MAX_SLAB_FILES 10
//! Maximum number of open slab types
#define MAX_SLAB_TYPES 50
//! Error opening file
#define ERR_NO_FILE -1
//! File table if full
#define ERR_TAB_FULL -2
//! Maximum number of char for a file name
#define MAX_LEN 257
//! Maximum number of char for etiket
#define MAX_ETIKET 13


typedef struct {
    //! Slab file name
    char file_name[MAX_LEN];
    //! Number of rows to write
    int nrows[MAX_SLAB_TYPES];
    //! Counter to check that all columns are written
    int count[MAX_SLAB_TYPES];
    //! x dimension of full output grid
    int nio[MAX_SLAB_TYPES];
    //! Index of 1st point along x
    int i1[MAX_SLAB_TYPES];
    //! x dimension of '#' grid section
    int ni[MAX_SLAB_TYPES];
    //! y dimension of full output grid
    int njo[MAX_SLAB_TYPES];
    //! Index of 1st point along y
    int j1[MAX_SLAB_TYPES];
    //! x dimension of '#' grid section for any grid type but '#', i1=j, 1=1, ni=nio, nj=njo
    int nj[MAX_SLAB_TYPES];
    //! Data buffer
    uint32_t *buffer;
    //! Current write position into buffer
    int pos;
} file_table_desc;


typedef struct {
    int32_t slb0;
    int32_t nBytes;
    int32_t deet;
    int32_t npas;
    int32_t dateo1;
    int32_t dateo2;
    float val15;
    // 4 char variable disguised as 32 bit integer
    int32_t Ietiket[3];
} Id_Block_file;


typedef struct {
    int32_t slb1;
    int32_t nBytes;
    int32_t slab_id;
    int32_t ig1;
    int32_t ig2;
    int32_t ig3;
    int32_t ig4;
    int32_t Nrows;
    int32_t Niout;
    int32_t Njout;
    int32_t nxgrid;
    int32_t nygrid;
    int32_t Nextra;
    int32_t ig1_;
    int32_t ig2_;
    int32_t ig3_;
    int32_t ig4_;
    // 4 char variable disguised as 32 bit integer
    int32_t Igrtyp
    // 4 char variable disguised as 32 bit integer
    int32_t Igrtyp_;
} Slab_Descrt_file;


typedef struct {
    int32_t slb2;
    int32_t nBytes;
    int32_t slab_id;
    int32_t nX;
    int32_t Nrows;
} Data_Block_file;


typedef struct {
    int32_t id_end;
    int32_t nBytes;
} Slab_End;

#define SWAP32(temp) ( ((temp>>24) & 0xFF) | ((temp & 0xFF)<<24) | ((temp>>8) & 0xFF00) | ((temp & 0xFF00)<<8) )
