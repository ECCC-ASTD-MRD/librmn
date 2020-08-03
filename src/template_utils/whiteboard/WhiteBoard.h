//! @todo Should these really be macros?  Wouldn't it be better to call functions that the compiler can inline?

#define WB_PUT_C(WB,name,value,strglen,options) c_wb_put(WB,(unsigned char *)name,WB_FORTRAN_CHAR,strglen,(unsigned char *)value,0,options,strlen(name))
#define WB_GET_C(WB,name,value,strglen) c_wb_get(WB,(unsigned char *)name,WB_FORTRAN_CHAR,strglen,(unsigned char *)value,0,strlen(name))
#define WB_PUT_CV(WB,name,value,strglen,size,options) c_wb_put(WB,(unsigned char *)name,WB_FORTRAN_CHAR,strglen,(unsigned char *)value,size,options,strlen(name))
#define WB_GET_CV(WB,name,value,strglen,size) c_wb_get(WB,(unsigned char *)name,WB_FORTRAN_CHAR,strglen,(unsigned char *)value,size,strlen(name))

#define WB_PUT_L1(WB,name,value,options) c_wb_put(WB,(unsigned char *)name,WB_FORTRAN_BOOL,1,(unsigned char *)value,0,options,strlen(name))
#define WB_GET_L1(WB,name,value) c_wb_get(WB,(unsigned char *)name,WB_FORTRAN_BOOL,1,(unsigned char *)value,0,strlen(name))
#define WB_PUT_L1V(WB,name,value,size,options) c_wb_put(WB,(unsigned char *)name,WB_FORTRAN_BOOL,1,(unsigned char *)value,size,options,strlen(name))
#define WB_GET_L1V(WB,name,value,size) c_wb_get(WB,(unsigned char *)name,WB_FORTRAN_BOOL,1,(unsigned char *)value,size,strlen(name))

#define WB_PUT_I4(WB,name,value,options) c_wb_put(WB,(unsigned char *)name,WB_FORTRAN_INT,4,(unsigned char *)value,0,options,strlen(name))
#define WB_GET_I4(WB,name,value) c_wb_get(WB,(unsigned char *)name,WB_FORTRAN_INT,4,(unsigned char *)value,0,strlen(name))
#define WB_PUT_I4V(WB,name,value,size,options) c_wb_put(WB,(unsigned char *)name,WB_FORTRAN_INT,4,(unsigned char *)value,size,options,strlen(name))
#define WB_GET_I4V(WB,name,value,size) c_wb_get(WB,(unsigned char *)name,WB_FORTRAN_INT,4,(unsigned char *)value,size,strlen(name))

#define WB_PUT_I8(WB,name,value,options) c_wb_put(WB,(unsigned char *)name,WB_FORTRAN_INT,8,(unsigned char *)value,0,options,strlen(name))
#define WB_GET_I8(WB,name,value) c_wb_get(WB,(unsigned char *)name,WB_FORTRAN_INT,8,(unsigned char *)value,0,strlen(name))
#define WB_PUT_I8V(WB,name,value,size,options) c_wb_put(WB,(unsigned char *)name,WB_FORTRAN_INT,8,(unsigned char *)value,size,options,strlen(name))
#define WB_GET_I8V(WB,name,value,size) c_wb_get(WB,(unsigned char *)name,WB_FORTRAN_INT,8,(unsigned char *)value,size,strlen(name))

#define WB_PUT_R4(WB,name,value,options) c_wb_put(WB,(unsigned char *)name,WB_FORTRAN_REAL,4,(unsigned char *)value,0,options,strlen(name))
#define WB_GET_R4(WB,name,value) c_wb_get(WB,(unsigned char *)name,WB_FORTRAN_REAL,4,(unsigned char *)value,0,strlen(name))
#define WB_PUT_R4V(WB,name,value,size,options) c_wb_put(WB,(unsigned char *)name,WB_FORTRAN_REAL,4,(unsigned char *)value,size,options,strlen(name))
#define WB_GET_R4V(WB,name,value,size) c_wb_get(WB,(unsigned char *)name,WB_FORTRAN_REAL,4,(unsigned char *)value,size,strlen(name))

#define WB_PUT_R8(WB,name,value,options) c_wb_put(WB,(unsigned char *)name,WB_FORTRAN_REAL,8,(unsigned char *)value,0,options,strlen(name))
#define WB_GET_R8(WB,name,value) c_wb_get(WB,(unsigned char *)name,WB_FORTRAN_REAL,8,(unsigned char *)value,0,strlen(name))
#define WB_PUT_R8V(WB,name,value,size,options) c_wb_put(WB,(unsigned char *)name,WB_FORTRAN_REAL,8,(unsigned char *)value,size,options,strlen(name))
#define WB_GET_R8V(WB,name,value,size) c_wb_get(WB,(unsigned char *)name,WB_FORTRAN_REAL,8,(unsigned char *)value,size,strlen(name))


typedef struct {
    int code;
    char *text;
} wb_symbol;

typedef struct {
    //! real/int/logical/char 0 means invalid entry
    unsigned int type:3,
    //! 1 if array
    unsigned int array:1,
    //! 1 if variable has been created by a restart
    unsigned int fromrestart:1,
    //! 1 if variable is now read-only
    unsigned int readonly:1,
    //! Size of each element 4/8 for real/int, 0-1023 for char, 4 for logical
    unsigned int elementsize:10,
    //! 1 if value is dubious due to a bad put
    unsigned int badval:1,
    //! 1 if variable is not global (same on all MPI Whiteboards
    unsigned int islocal:1,
    //! 1 if variable can be set until package/variable is locked
    unsigned int resetuntil:1,
    //! 1 if value can be set any number of times
    unsigned int resetmany:1,
    //! 1 if after restart value becomes read-only
    unsigned int noresetafterrestart:1,
    //! variable has been set
    unsigned int initialized:1,
    //! number of datalines occupied by variable. if 0 it means entire page
    unsigned int lines:10;
} wb_flags;

//! Large token, full name, integers
typedef union {
    //! Name as a character array
    unsigned char carr[WB_MAXNAMELENGTH + 1];
    //! Name as a int array
    unsigned int  iarr[(WB_MAXNAMELENGTH + 1) / sizeof(int)];
} wb_name;

//! Array metadata container
typedef struct {
    int usedElements;
    int maxElements;
} wb_arraydesc;

//! 64 bit token, chars,  ints, long long, array metadata
typedef union {
   unsigned char      carr[8];
   unsigned int       iarr[2];
   unsigned long long llarr;
   wb_arraydesc       desc;
} wb_metaarray;

//! Whiteboard data line, one line per scalar item <= 8 bytes
// THE SIZE OF THIS STRUCTURE SHOULD BE A MULTIPLE OF 8 BYTES
typedef struct {
    //! WB_MAXNAMELENGTH characters name
    wb_name name;
    //! Type, unused bytes, size of element, number of lines
    wb_flags flags;
    //! Value or array descriptor (this token is 8 byte aligned)
    wb_metaarray data;
} wb_linemeta;

// THE SIZE OF THIS STRUCTURE SHOULD BE A MULTIPLE OF 8 BYTES
typedef struct {
    unsigned char data[sizeof(wb_linemeta)];
} wb_linedata;

//! A page line is either a metadata + short data or a long data
typedef union {
    //! Metadata + data for a simple real integer logical or <=8 characters string
    wb_linemeta meta;
    //! Long data container (strings > 8 characters or arrays )
    wb_linedata data;
} wb_line;

//! Used by wb_read to check declaration/assignation consistency
typedef struct {
    wb_line *line;
    int defined;
    int assigned;
} wb_definition;

//! Whiteboard page , size is 16 or 24 bytes + space needed by line
typedef struct wb_page {
    //! Really an address only but an 8 byte item should be forced for alignment reasons
    struct wb_page *next;
    //! This garantees a multiple of 8 bytes before line (that needs 8 byte alignment
    struct wb_page *not_used;
    //! Index of the first free entry
    int firstFreeLine;
    //! Number of entries
    int nbLines;
    wb_line line[WB_MAXLINESPERPAGE];
} wb_page;
