#ifndef _App_h
#define _App_h

#include <stdio.h>
#include <sys/time.h>
#include "App_Timer.h"

#ifdef HAVE_OPENMP
#   include <omp.h>
#endif

#ifdef HAVE_MPI
#   include <mpi.h>
#endif

#define TRUE   1
#define FALSE  0

#define APP_COLOR_BLINK      "\x1b[5m"
#define APP_COLOR_BLACK      "\x1b[0;30m"
#define APP_COLOR_RED        "\x1b[0;31m"
#define APP_COLOR_GREEN      "\x1b[0;32m"
#define APP_COLOR_LIGHTGREEN "\x1b[1;32m"
#define APP_COLOR_ORANGE     "\x1b[33m" 
#define APP_COLOR_YELLOW     "\x1b[1m\x1b[33m"
#define APP_COLOR_BLUE       "\x1b[0;34m"
#define APP_COLOR_MAGENTA    "\x1b[0;35m"
#define APP_COLOR_CYAN       "\x1b[0;36m"
#define APP_COLOR_LIGHTCYAN  "\x1b[1m\x1b[36m"
#define APP_COLOR_GRAY       "\x1b[0;37m"
#define APP_COLOR_RESET      "\x1b[0m"
#define APP_MASTER    0
#define APP_THREAD    1

#define APP_ERRORSIZE 2048
#define APP_BUFMAX    32768               ///< Maximum input buffer length
#define APP_LISTMAX   4096                ///< Maximum number of items in a flag list
#define APP_SEED      1049731793          ///< Initial FIXED seed
#define APP_FTNSTRMAX 1024                ///< Maximum fortran message length
#define APP_LIBSMAX   64                  ///< Maximum number of libraries

#define APP_NOARGSFLAG 0x00               ///< No flag specified
#define APP_NOARGSFAIL 0x01               ///< Fail if no arguments are specified
#define APP_ARGSLOG    0x02               ///< Use log flag
#define APP_ARGSLANG   0x04               ///< Multilingual app
#define APP_ARGSSEED   0x08               ///< Use seed flag
#define APP_ARGSTHREAD 0x10               ///< Use thread flag
#define APP_ARGSTMPDIR 0x20               ///< Use tmp dir

#ifdef __xlC__
#   define APP_ONCE    ((1)<<3)
#else
#   define APP_ONCE    ((__COUNTER__+1)<<3)
#endif
#define APP_MAXONCE 1024

typedef enum { APP_MAIN=0,APP_LIBRMN=1,APP_LIBFST=2,APP_LIBVGRID=3,APP_LIBINTERPV=4,APP_LIBGEOREF=5,APP_LIBRPNMPI=6,APP_LIBIRIS=7  } TApp_Lib;
typedef enum { APP_MUST=-1,APP_FATAL=0,APP_SYSTEM=1,APP_ERROR=2,APP_WARNING=3,APP_INFO=4,APP_DEBUG=5,APP_EXTRA=6,APP_QUIET=7 } TApp_LogLevel;
typedef enum { APP_NODATE=0,APP_DATETIME=1,APP_TIME=2,APP_SECOND=3,APP_MSECOND=4 } TApp_LogTime;
typedef enum { APP_STOP,APP_RUN,APP_DONE } TApp_State;
typedef enum { APP_NIL=0x0,APP_FLAG=0x01,APP_CHAR=0x02,APP_UINT32=0x04,APP_INT32=0x06,APP_UINT64=0x08,APP_INT64=0x0A,APP_FLOAT32=0x0C,APP_FLOAT64=0x0E } TApp_Type;
typedef enum { APP_FR=0x0,APP_EN=0x01 } TApp_Lang;
typedef enum { APP_OK=1,APP_ERR=0 } TApp_RetCode;
typedef enum { APP_AFFINITY_NONE=0,APP_AFFINITY_COMPACT=1,APP_AFFINITY_SCATTER=2,APP_AFFINITY_SOCKET=3 } TApp_Affinity;

#define APP_ASRT_OK(x) if( (x)!=APP_OK ) return(APP_ERR)
#define APP_ASRT_OK_M(Fct, ...) \
   if( (Fct)!=APP_OK ) { \
      Lib_Log(APP_ERROR,APP_MAIN, __VA_ARGS__); \
      return(APP_ERR); \
   }

// Check FST function and return the specified value if an error was encountered
#define APP_FST_ASRT_H(Fct, ...) \
   if( (Fct) < 0 ) { \
      Lib_Log(APP_ERROR,APP_MAIN, __VA_ARGS__); \
      return(APP_ERR); \
   }
#define APP_FST_ASRT(Fct, ...) \
   if( (Fct) != 0 ) { \
      Lib_Log(APP_ERROR,APP_MAIN, __VA_ARGS__); \
      return(APP_ERR); \
   }
// Memory helpers
#define APP_MEM_ASRT(Buf,Fct) \
   if( !(Buf=(Fct)) ) { \
      Lib_Log(APP_ERROR,APP_MAIN,"(%s) Could not allocate memory for field %s at line %d.\n",__func__,#Buf,__LINE__); \
      return(APP_ERR); \
   }
#define APP_FREE(Ptr) if(Ptr) { free(Ptr); Ptr=NULL; }
// MPI helpers
#ifdef HAVE_MPI
#define APP_MPI_ASRT(Fct) { \
   int err = (Fct); \
   if( err!=MPI_SUCCESS ) { \
      Lib_Log(APP_ERROR,APP_MAIN,"(%s) MPI call %s at line %d failed with code %d for MPI node %d\n",__func__,#Fct,__LINE__,err,App->RankMPI); \
      return(APP_ERR); \
   } \
}
#define APP_MPI_CHK(Fct) { \
   int err = (Fct); \
   if( err!=MPI_SUCCESS ) { \
      Lib_Log(APP_ERROR,APP_MAIN,"(%s) MPI call %s at line %d failed with code %d for MPI node %d\n",__func__,#Fct,__LINE__,err,App->RankMPI); \
   } \
}
#endif //HAVE_MPI

// Argument definitions
typedef struct TApp_Arg {
   TApp_Type    Type;                    // Argument type
   void         *Var;                    // Where to put the argument value(s)
   int          Multi;                   // Multiplicity of the argument (Maximum number of values for a list)
   char         *Short,*Long,*Info;      // Argument flags and description
} TApp_Arg;

// Application controller definition
typedef struct TApp {
   char*          Name;                  ///< Name of applicaton
   char*          Version;               ///< Version of application
   char*          Desc;                  ///< Description of application
   char*          TimeStamp;             ///< Compilation timestamp
   char*          LogFile;               ///< Log file
   int            LogSplit;              ///< Split the log file per MPI rank path
//   char*          TmpDir;               ///< Tmp directory
   char*          Tag;                   ///< Identificateur
   FILE*          LogStream;             ///< Log file associated stream
   int            LogWarning;            ///< Number of warnings
   int            LogError;              ///< Number of errors
   int            LogColor;              ///< Use coloring in the logs
   TApp_LogTime   LogTime;               ///< Display time in the logs
   TApp_LogLevel  LogLevel[APP_LIBSMAX]; ///< Level of log
   TApp_LogLevel  Tolerance;             ///< Abort level
   TApp_State     State;                 ///< State of application
   TApp_Lang      Language;              ///< Language (default: $CMCLNG or APP_EN)
   double         Percent;               ///< Percentage of execution done (0=not started, 100=finished)
   struct timeval Time;                  ///< Timer for execution time
   int            Type;                  ///< App object type (APP_MASTER,APP_THREAD)
   int            Step;                  ///< Model step

   char*          LibsVersion[APP_LIBSMAX];

   int            Seed,*OMPSeed;         ///< Random number generator seed
   int           *TotalsMPI;             ///< MPI total number of items arrays
   int           *CountsMPI;             ///< MPI count gathering arrays
   int           *DisplsMPI;             ///< MPI displacement gathering arrays
   int            NbMPI,RankMPI;         ///< Number of MPI process
   int            NbThread;              ///< Number of OpenMP threads
   int            Signal;                ///< Trapped signal
   TApp_Affinity  Affinity;              ///< Thread placement affinity
   int            NbNodeMPI,NodeRankMPI; ///< Number of MPI process on the current node
#ifdef HAVE_MPI
   MPI_Comm       Comm;
   MPI_Comm       NodeComm,NodeHeadComm;///< Communicator for the current node and the head nodes
#endif //HAVE_MPI

   TApp_Timer     *TimerLog;             ///< Time spent on log printing
} TApp;

#ifndef APP_BUILD
extern __thread TApp *App;               ///< Per thread App pointer
#endif

typedef int (TApp_InputParseProc) (void *Def,char *Token,char *Value,int Index);

#define App_Log(LEVEL, ...) Lib_Log(APP_MAIN,LEVEL,__VA_ARGS__)

TApp *App_Init(int Type,char* Name,char* Version,char* Desc,char* Stamp);
void  App_LibRegister(TApp_Lib Lib,char *Version);
void  App_Free(void);
void  App_Start(void);
int   App_End(int Status);
void  Lib_Log(TApp_Lib Lib,TApp_LogLevel Level,const char *Format,...);
int   Lib_LogLevel(TApp_Lib Lib,char *Val);
int   App_LogLevel(char *Val);
void  App_LogOpen(void);
void  App_LogClose(void);
int   App_LogTime(char *Val);
void  App_Progress(float Percent,const char *Format,...);
int   App_ParseArgs(TApp_Arg *AArgs,int argc,char *argv[],int Flags);
int   App_ParseInput(void *Def,char *File,TApp_InputParseProc *ParseProc);
int   App_ParseBool(char *Param,char *Value,char *Var);
int   App_ParseDate(char *Param,char *Value,time_t *Var);
int   App_ParseDateSplit(char *Param,char *Value,int *Year,int *Month,int *Day,int *Hour,int *Min);
int   App_ParseCoords(char *Param,char *Value,double *Lat,double *Lon,int Index);
void  App_SeedInit(void);
char* App_ErrorGet(void);
int   App_ThreadPlace(void);
void  App_Trap(int Signal);
int   App_IsDone(void); 
int   App_IsMPI(void);
int   App_IsOMP(void);
int   App_IsSingleNode(void);
int   App_IsAloneNode(void);
int   App_NodeGroup();

#endif
