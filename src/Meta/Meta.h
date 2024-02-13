#ifndef _Meta
#define _Meta

#include <time.h>
#include <json-c/json.h>
#ifdef HAVE_RMN
   #include <rmn.h>
#endif
#define META_TOKEN_MAXLEN 1024
#define META_TYPE_FIELD   1
#define META_TYPE_FILE    2

// Generic json manipulation calls
char*        Meta_Stringify(json_object *Obj);
json_object* Meta_Parse(const char *MetaString);
int32_t      Meta_ArrayLength(json_object *Obj);
json_object* Meta_ArrayGetObject(json_object *Obj,int32_t Idx);
json_object* Meta_ArrayFind(json_object *Obj,char *Token);
json_object* Meta_GetObject(json_object *Obj,char *Path);
char*        Meta_GetObjectString(json_object *Obj);
json_object* Meta_Copy(json_object *Obj);
int32_t      Meta_Match(json_object *Obj1,json_object *Obj2,int RegExp);
json_object* Meta_Resolve(json_object *Obj,json_object *ObjMaster);
json_object* Meta_NewObject();

// Metadata specific functions
int32_t      Meta_Init();
int32_t      Meta_Free(json_object *Obj);
int32_t      Meta_Is(json_object *Obj);
json_object* Meta_New(int Type,char *Version);
json_object* Meta_Load(char *Path);
int32_t      Meta_LoadProfile(char *Version);

json_object* Meta_DefVarFromDict(json_object *Obj,char* RPNName);
json_object* Meta_DefVar(json_object *Obj,char *StandardName,char* RPNName,char *LongName,char *Description,char* Unit);
json_object* Meta_GetVar(json_object *Obj,char **StandardName,char **RPNName,char **LongName,char **Description,char **Unit);

json_object* Meta_DefForecastTime(json_object *Obj,time_t T0,int32_t Step,double Duration,char *Unit);
json_object* Meta_GetForecastTime(json_object *Obj,time_t *T0,int32_t *Step,double *Duration,char **Unit);

json_object* Meta_AddVerticalRef(json_object *Obj,char *Identifier,int Copy);
json_object* Meta_DefVerticalRef(json_object *Obj,char *Identifier,double *Value,int32_t Nb,int Copy);
json_object* Meta_GetVerticalRef(json_object *Obj,int32_t Index,char **Identifier,double *Value);
json_object* Meta_FindVerticalObj(char* Identifier,json_object *ObjMaster);

json_object* Meta_AddHorizontalRef(json_object *Obj,char *Identifier,int Copy);
json_object* Meta_DefHorizontalRef(json_object *Obj,char *Identifier,int Copy);
json_object* Meta_GetHorizontalRef(json_object *Obj,char **Identifier);
json_object* Meta_FindHorizontalObj(char* Identifier,json_object *ObjMaster);

json_object* Meta_DefData(json_object *Node,int32_t NI,int32_t NJ,int32_t NK,char *Type,char *Compression,int32_t Pack,int32_t Bit,double Min,double Max);
json_object* Meta_GetData(json_object *Node,int32_t *NI,int32_t *NJ,int32_t *NK,char **Type,char **Compression,int32_t *Pack,int32_t *Bit,double *Min,double *Max);

json_object* Meta_ClearCellMethods(json_object *Obj);
json_object* Meta_AddCellMethod(json_object *Obj,char *Method);
json_object* Meta_SetCellMethods(json_object *Obj,char *Methods[]);

json_object* Meta_ClearQualifiers(json_object *Obj);
json_object* Meta_AddQualifier(json_object *Obj,char *Qualifier);
json_object* Meta_SetQualifiers(json_object *Obj,char **Qualifiers);

json_object* Meta_ClearMissingValues(json_object *Obj);
json_object* Meta_AddMissingValue(json_object *Obj,char *Reason,double Value);
json_object* Meta_GetMissingValue(json_object *Obj,int32_t Index,char **Reason,double *Value);

json_object* Meta_DefFile(json_object *Obj,char *Institution,char* Discipline,char *Title,char *Source,char *Description,char *State);

#ifdef HAVE_RMN

time_t  Meta_DateTime2Seconds(int YYYY,int MM,int DD,int hh,int mm,int ss,int GMT);
void    Meta_StampDecode(int32_t Stamp,int32_t *YYYY,int32_t *MM,int32_t *DD,int32_t *H,int32_t *M,int32_t *S);
void    Meta_StampEncode(int32_t *Stamp,int32_t YYYY,int32_t MM,int32_t DD,int32_t H,int32_t M,int32_t S);
time_t  Meta_Stamp2Seconds(int32_t Stamp);
int32_t Meta_Seconds2Stamp(time_t Sec);
int32_t Meta_To89(json_object *Obj,fst_record *Rec);
int32_t Meta_From89(json_object *Obj,const fst_record* const Rec);

#endif

#endif