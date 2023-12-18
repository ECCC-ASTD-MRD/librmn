#ifndef _Meta
#define _Meta

#include <time.h>
#include <stdbool.h>
#include <json-c/json.h>
#ifdef HAVE_RMN
   #include <rmn.h>
#endif
#define META_TOKEN_MAXLEN 1024
#define META_PATH_MAXLEN 2048

// Generic json manipulation calls
int32_t      Meta_Free(json_object *Obj);
char*        Meta_Stringify(json_object *Obj);
json_object* Meta_Parse(char *MetaString);
int32_t      Meta_ArrayLength(json_object *Obj);
json_object* Meta_ArrayGetObject(json_object *Obj,int32_t Idx);
json_object* Meta_ArrayFind(json_object *Obj,char *Token);
json_object* Meta_GetObject(json_object *Obj,char *Path);
char*        Meta_GetObjectString(json_object *Obj);
json_object* Meta_Copy(json_object *Obj);
int32_t      Meta_Equivalent(json_object *Obj1,json_object *Obj2);

json_object* Meta_ResolveRef(json_object *Obj);

// Metadata specific functions
int32_t      Meta_Init();
json_object* Meta_LoadProfile(char *Name,char *Version);

json_object* Meta_DefVar(json_object *Obj,char *StandardName,char* RPNName,char *LongName,char *Description);
json_object* Meta_GetVar(json_object *Obj,char **StandardName,char **RPNName,char **LongName,char **Description);

json_object* Meta_DefSize(json_object *Obj,int32_t NI,int32_t NJ,int32_t NK);
json_object* Meta_GetSize(json_object *Obj,int32_t *NI,int32_t *NJ,int32_t *NK);

json_object* Meta_DefBound(json_object *Obj,double Min,double Max,const char* Unit);
json_object* Meta_GetBound(json_object *Obj,double *Min,double *Max,char **Unit);

json_object* Meta_DefForecastTime(json_object *Obj,time_t T0,int32_t Step,double Duration,char *Unit);
json_object* Meta_GetForecastTime(json_object *Obj,time_t *T0,int32_t *Step,double *Duration,char **Unit);

json_object* Meta_DefVerticalRef(json_object *Obj,char *Identifier,double *Value,int32_t Nb,bool Copy);
json_object* Meta_GetVerticalRef(json_object *Obj,int32_t Index,char **Identifier,double *Value);

json_object* Meta_DefHorizontalRef(json_object *Obj,char *Identifier,bool Copy);
json_object* Meta_GetHorizontalRef(json_object *Obj,char **Identifier);

json_object* Meta_DefData(json_object *Node,char *Type,char *Compression,int32_t Pack,int32_t Size);
json_object* Meta_GetData(json_object *Node,char **Type,char **Compression,int32_t *Pack,int32_t *Size);

json_object *Meta_AddVerticalRef(json_object *Obj,char *Identifier,bool Copy);
json_object *Meta_AddHorizontalRef(json_object *Obj,char *Identifier,bool Copy);
json_object *Meta_AddCellMethod(json_object *Obj,char *Method);
json_object *Meta_AddQualifier(json_object *Obj,char *Qualifier);

json_object *Meta_AddMissingValue(json_object *Obj,char *Reason,double Value);
json_object *Meta_GetMissingValue(json_object *Obj,int32_t Index,char **Reason,double *Value);

json_object *Meta_ClearCellMethods(json_object *Obj);
json_object *Meta_ClearQualifiers(json_object *Obj);
json_object *Meta_ClearMissingValues(json_object *Obj);

json_object *Meta_FindVerticalObj(char* Identifier);
json_object *Meta_FindHorizontalObj(char* Identifier);

json_object *Meta_DefFile(json_object *Obj,char *Institution,char* Discipline,char *Title,char *Source,char *Description);

#ifdef HAVE_RMN

void    Meta_StampDecode(int32_t Stamp,int32_t *YYYY,int32_t *MM,int32_t *DD,int32_t *H,int32_t *M,int32_t *S);
void    Meta_StampEncode(int32_t *Stamp,int32_t YYYY,int32_t MM,int32_t DD,int32_t H,int32_t M,int32_t S);
time_t  Meta_Stamp2Seconds(int32_t Stamp);
int32_t Meta_Seconds2Stamp(time_t Sec);
int32_t      Meta_To89(json_object *Obj,fst_record *Rec);
int32_t      Meta_From89(json_object *Obj,fst_record *Rec);

#endif

#endif