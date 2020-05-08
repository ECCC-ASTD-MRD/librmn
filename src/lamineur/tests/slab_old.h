#define MAX_SLAB_FILES 10      /* maximum number of open slab files */
#define MAX_SLAB_TYPES 10      /* maximum number of open slab types */
#define ERR_NO_FILE -1         /* error opening file */
#define ERR_TAB_FULL -2        /* file table if full */
#define MAX_LEN 257            /* maximum number of char for a file name */



typedef struct {
                char file_name[MAX_LEN];     /* file name */
                int nrows[MAX_SLAB_TYPES];
                int nio_njo[MAX_SLAB_TYPES];
                int count[MAX_SLAB_TYPES];
                int *buffer;
                int pos;
                } file_table_desc;

typedef struct{
               int slb0, nBytes, deet, npas, dateo1,dateo2;
               float val15;
               char etiket[12];

               }Id_Block_file;

typedef struct{
               int  slb1, nBytes, slab_id,
	            ig1, ig2, ig3, ig4, Nrows,
                    Niout, Njout, nxgrid, nygrid, 
	            Nextra, ig1_, ig2_, ig3_, ig4_;
               char grtyp[4] ,grtyp_[4];

               }Slab_Descrt_file;

typedef struct{
               int slb2,nBytes,slab_id, nX, Nrows;

               }Data_Block_file;

typedef struct{
               int id_end;
	       }Slab_End;


