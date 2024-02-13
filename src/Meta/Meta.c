#include <App.h>
#include "rmn.h"
#include "Meta.h"
#include "Dict.h"
#include "fstd98/fstd98_internal.h"
#include <str.h>

#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <errno.h>
#include <glob.h>
#include </usr/include/regex.h>

#ifdef HAVE_UDUNITS2
#include <udunits2.h>
static ut_system   *MetaProfileUnit=NULL;   ///< UDUnits references
#endif

static char        *ARMNLIB;

typedef struct {
   char        *Version;
   json_object *Field,*File;        ///< Field and File profiles
   json_object *Z;                  ///< Vertical references
   json_object *XY;                 ///< Horizontal references (grid)
   json_object *Defs;               ///< Valid token definitions
   json_object *Types;              ///< 
   json_object *Compressions;       ///< 
   json_object *Qualifiers;         ///< 
   json_object *CellMethods;        ///< 
   json_object *CellProcesses;      ///< 
   json_object *Reasons;            ///< 
} TMetaProfile;

static TMetaProfile MetaProfiles[64];
static int MetaProfileNb;


static char *MetaVersion=NULL;       ///< Metadata version
static char  MetaValidate=TRUE;      ///< Enable token validation

static char* MetaTimeUnits[] = { "millisecond","second","minute","hour","day","month","year","decade","centenary","millenia" };

//TODO: static cf_system   *MetaProfileVar=NULL;   ///< CF convention variable references

/**----------------------------------------------------------------------------
 * @brief  Get the length of the token without extension 
 *         Token delimiter is anything not a [0-1],[a-z],[A-Z]

 * @date   July 2023
 *    @param[in]  Token   Token name
 *    @param[out] IsSplit Token name
 *
 *    @return              Length of token part or 0 if not a split token
*/
inline static int32_t Meta_TokenEnd(char* Token,int32_t *IsSplit) {

   char *e = Token;

   while(*e) {
      // Check if it is not a number, a lowercase, an uppercase or an underscore
      if (!((*e>=48 && *e<=57) || (*e>=65 && *e<=90) || (*e>=97 && *e<=122) || (*e==95) || (*e<=32))) {
         break;
      }
      e++;
    }
    *IsSplit=(*e)?TRUE:FALSE;
    
    return ((int)(e-Token));
}

/**----------------------------------------------------------------------------
 * @brief  Create new Meta object

 * @date   July 2023
 *
 *    @return              Metadata object pointer
*/
json_object *Meta_NewObject() {

   return(json_object_new_object());
}

/**----------------------------------------------------------------------------
 * @brief  Check if object is a valid JSON metadata object 

 * @date   July 2023
 *    @param[in]   Obj     Profile json object
 *
 *    @return              Object validity (TRUE,1 of FALSE,0)
*/
int32_t Meta_Is(json_object *Obj) {

   json_object *obj=NULL;

   return(json_pointer_get(Obj,"/version",&obj)==0);
}

inline static char* Meta_Version(json_object *Obj) {

   json_object *obj=NULL;

   json_pointer_get(Obj,"/version",&obj);
   return((char*)json_object_get_string(obj));
}

/**----------------------------------------------------------------------------
 * @brief  Find a profile from the availabel list by its version number

 * @date   February 2024
 *    @param[in]  Version  Profile version (if NULL, use env defined)
 *
 *    @return              json profile object
*/
static inline TMetaProfile *Meta_GetProfile(char *Version) {

   int          m;
   char        *version=NULL;

   version=Version&&Version[0]!='\0'?Version:MetaVersion;

   // Look for profile version
   for(m=0;m<MetaProfileNb;m++) {
      if (!strcmp(MetaProfiles[m].Version,version)) {
         break;
      }
   }

   // If not found, try to load it
   if (m==MetaProfileNb && !Meta_LoadProfile(version))
      return(NULL);

   return(&MetaProfiles[m]);
}

/**----------------------------------------------------------------------------
 * @brief  Initialise Meta package environment

 * @date   June 2023
 *    @return              Error code (1=ok)
*/
int32_t Meta_Init(){

   char   path[APP_BUFMAX];
   char  *c;
   glob_t globs;
   int32_t    j;
   json_object *obj=NULL,*obja=NULL;

   MetaProfileNb=0;
   MetaValidate=FALSE;

   // Check the log parameters in the environment 
   if ((c=getenv("META_VALIDATE"))) {
      MetaValidate=TRUE;
   }

   // Get metadata version to use
   if (!(MetaVersion=getenv("META_VERSION"))) {
      MetaVersion="0.1.0";
   }
   Lib_Log(APP_LIBMETA,APP_DEBUG,"%s: Meta data profile version: %s\n",__func__,MetaVersion);

#ifdef HAVE_UDUNITS2
   // Load units data, should use UDUNITS2_XML_PATH var
   if (!(MetaProfileUnit=ut_read_xml(NULL))) {
      Lib_Log(APP_LIBMETA,APP_ERROR,"%s: Unable to load udunits profile\n",__func__);
      return(FALSE);
   }
#endif

   // Will not load again if already loaded
   Dict_Load(DICT_UTF8);

   return(TRUE);
}

/**----------------------------------------------------------------------------
 * @brief  Load a profile from file

 * @date   February 2024
 *    @param[in]  Version  Profile version
 *
 *    @return              Found or not (TRUE or FALSE)
*/
int32_t Meta_LoadProfile(char *Version) {

   TMetaProfile *prof;
   json_object  *obj=NULL,*obja=NULL;
   glob_t        globs;
   char          path[APP_BUFMAX];
   int32_t       j;

   if (!Version) {
      return(FALSE);
   }

   prof=&MetaProfiles[MetaProfileNb];

   // Load metadata references 
   // TODO: move to CMCCONST
   if ((ARMNLIB=getenv("ARMNLIB"))) {

      snprintf(path,APP_BUFMAX,"%s/json/%s/field.json",ARMNLIB,Version);
      if (!(prof->Field=json_object_from_file(path))) {
         Lib_Log(APP_LIBMETA,APP_ERROR,"%s: Unabled to load field profile %s\n",__func__,path);
         return(FALSE);
      }
      snprintf(path,APP_BUFMAX,"%s/json/%s/file.json",ARMNLIB,Version);
      if (!(prof->File=json_object_from_file(path))) {
         Lib_Log(APP_LIBMETA,APP_ERROR,"%s: Unabled to load file profile %s\n",__func__,path);
         return(FALSE);
      } 

      // Load vertical reference definitions
      snprintf(path,APP_BUFMAX,"%s/json/%s/vertical/*.json",ARMNLIB,Version);
      glob(path,0x0,NULL,&globs);
      if (globs.gl_pathc) {
         prof->Z=json_object_new_object();
         obja=json_object_new_array();
         json_object_object_add(prof->Z,"vertical_references",obja);

         for(j=0;j<globs.gl_pathc;j++) {
            if (!(obj=json_object_from_file(globs.gl_pathv[j]))) {
               Lib_Log(APP_LIBMETA,APP_ERROR,"%s: Unable to load vertical profile: %s\n",__func__,globs.gl_pathv[j]);
            }
            Lib_Log(APP_LIBMETA,APP_DEBUG,"%s: Reading vertical profile: %s\n",__func__,globs.gl_pathv[j]);
            json_object_array_add(obja,obj);
         }
      }
      globfree(&globs);

      // Load horizontal definitions
      snprintf(path,APP_BUFMAX,"%s/json/%s/horizontal/*.json",ARMNLIB,Version);
      glob(path,0x0,NULL,&globs);
      if (globs.gl_pathc) {
         prof->XY=json_object_new_object();
         obja=json_object_new_array();
         json_object_object_add(prof->XY,"horizontal_references",obja);

         for(j=0;j<globs.gl_pathc;j++) {
            if (!(obj=json_object_from_file(globs.gl_pathv[j]))) {
               Lib_Log(APP_LIBMETA,APP_ERROR,"%s: Unable to load horizontal profile: %s\n",__func__,globs.gl_pathv[j]);
            }
            Lib_Log(APP_LIBMETA,APP_DEBUG,"%s: Reading horizontal profile: %s\n",__func__,globs.gl_pathv[j]);
            json_object_array_add(obja,obj);
         }
      }
      globfree(&globs);

      // Load token definitions
      snprintf(path,APP_BUFMAX,"%s/json/%s/definitions.json",ARMNLIB,MetaVersion);
      if (!(obja=json_object_from_file(path))) {
         Lib_Log(APP_LIBMETA,APP_ERROR,"%s: Unable to load token definitions: %s\n",__func__,path);
         return(0);
      }
      json_pointer_get(obja,"/types",&prof->Types);
      json_pointer_get(obja,"/compressions",&prof->Compressions);
      json_pointer_get(obja,"/qualifiers",&prof->Qualifiers);
      json_pointer_get(obja,"/cell_methods",&prof->CellMethods);
      json_pointer_get(obja,"/cell_processes",&prof->CellProcesses);
      json_pointer_get(obja,"/reasons",&prof->Reasons);
   } else {
      Lib_Log(APP_LIBMETA,APP_ERROR,"%s: Unable to initialize, ARMNLIB variable not defined\n",__func__);
      return(FALSE);
   }

   prof->Version=strdup(Version);
   MetaProfileNb++;

   return(TRUE);
}

/**----------------------------------------------------------------------------
 * @brief  Decrement json object reference count and free memory if no more reference
 * @date   July 2023
 *    @param[in]   Obj           Profile json object
 *
 *    @return                    Has memory been freed
*/
int32_t Meta_Free(json_object *Obj) {

   return(json_object_put(Obj));
}

/**----------------------------------------------------------------------------
 * @brief  Load a standard json profile from the environment

 * @date   July 2023
 *    @param[in]  Name     Profile name
 *    @param[in]  Version  Profile version (if NULL, use env defined)
 *
 *    @return              json object
*/
json_object *Meta_New(int Type,char *Version) {
   
   TMetaProfile *prof=NULL;;
   json_object  *obj=NULL;

   if (!(prof=Meta_GetProfile(Version))) {
      return(NULL);
   }
   
   obj=(Type==META_TYPE_FIELD?prof->Field:prof->File);
   return(Meta_Copy(obj));
}

/**----------------------------------------------------------------------------
 * @brief  Load a user json file

 * @date   July 2023
 *    @param[in]  Name     Profile name
 *    @param[in]  Version  Profile version (if NULL, use env defined)
 *
 *    @return              json object
*/
json_object *Meta_Load(char *Path) {
   
   json_object *obj=NULL;

   if (!(obj=json_object_from_file(Path))) {
      Lib_Log(APP_LIBMETA,APP_ERROR,"%s: Unabled to load json file %s\n",__func__,Path);
      return(NULL);
   }
  
   return(obj);
}

/**----------------------------------------------------------------------------
 * @brief  Validate a token from a token list

 * @date   July 2023
 *    @param[in]  TokenList Valid token name list
 *    @param[in]  Token     Token name
 *
 *    @return               Pointer on last token char, or NULL if non valid token
*/
inline static char* Meta_ValidateToken(json_object *TokenList,char *Token) { 

   int32_t          n=0,m=0,t=0;
   json_object *obj=NULL;
   const char  *string=NULL;

   if (Token && TokenList) {
      n=Meta_TokenEnd(Token,&t);

      for(int32_t i=0;i<json_object_array_length(TokenList);i++) {
         obj=json_object_array_get_idx(TokenList,i);
         string=json_object_get_string(obj);
         if (!t) {

         }
         m=t?n:strlen(string);
         if (!t && n!=m) {
            continue;
         }
         if (strncmp(Token,string,m)==0) {
            return(&Token[n]);
         }
      }
   } else {
      Lib_Log(APP_LIBMETA,APP_ERROR,"%s: Invalid Token or TokenList\n",__func__);
   }
   return(NULL);
}

/**----------------------------------------------------------------------------
 * @brief  Define variable information

 * @date   July 2023
 *    @param[in]  Obj           Profile json object
 *    @param[in]  StandardName  Standard name (CF table)
 *    @param[in]  RPNName       RPN nomvar
 *    @param[in]  LongName      Long name
 *    @param[in]  Description   Varibale description
 *    @param[in]  Unit          Data unit (udunits)
 *
 *    @return                   json_object pointer (NULL if error)
*/
json_object *Meta_DefVar(json_object *Obj,char *StandardName,char* RPNName,char *LongName,char *Description,char *Unit) {

   json_object *objval=NULL;
#ifdef HAVE_UDUNITS2
   ut_unit     *unit=NULL;
#endif

   if (StandardName) {
      json_pointer_get(Obj,"/standard_name",&objval);
      json_object_set_string(objval,StandardName);
   }
   if (RPNName) {
      json_pointer_get(Obj,"/rpn_name",&objval);
      json_object_set_string(objval,RPNName);
   }
   if (LongName) {
      json_pointer_get(Obj,"/long_name",&objval);
      json_object_set_string(objval,LongName);
   }
   if (Description) {
      json_pointer_get(Obj,"/description",&objval);
      json_object_set_string(objval,Description);
   }

   if (Unit) {
      json_pointer_get(Obj,"/unit",&objval);
#ifdef HAVE_UDUNITS2
      if (MetaValidate && !(unit=ut_get_unit_by_name(MetaProfileUnit,Unit))) {
         Lib_Log(APP_LIBMETA,APP_WARNING,"%s: Specified unit not defined in udunits: %s",__func__,Unit);
         return(NULL);
      } else {
         json_object_set_string(objval,Unit);
      }
#else
      json_object_set_string(objval,Unit);
#endif
   }

   return(Obj);
}

/**----------------------------------------------------------------------------
 * @brief  Define variable information based on $CMCCONST dictionnary to extract
 *         the StandardName, LongName and Unit
 *
 * @date   July 2023
 *    @param[in]  Obj           Profile json object
 *    @param[in]  RPNName       RPN nomvar
 *
 *    @return                   json_object pointer (NULL if error)
*/
json_object *Meta_DefVarFromDict(json_object *Obj,char* RPNName) {

   TDictVar *var;
   json_object *objval=NULL;
#ifdef HAVE_UDUNITS2
   ut_unit     *unit=NULL;
#endif

   if (!RPNName) {
      return(NULL);
   }
   json_pointer_get(Obj,"/rpn_name",&objval);
   json_object_set_string(objval,RPNName);
   
   if ((var=Dict_GetVar(RPNName))) {
      json_pointer_get(Obj,"/standard_name",&objval);
      json_object_set_string(objval,var->Short[0]);
   
      json_pointer_get(Obj,"/long_name",&objval);
      json_object_set_string(objval,var->Long[0]);
   
//      json_pointer_get(Obj,"/description",&objval);
//      json_object_set_string(objval,Description);

      if (var->Units) {
         json_pointer_get(Obj,"/unit",&objval);
#ifdef HAVE_UDUNITS2
         if (MetaValidate && !(unit=ut_get_unit_by_name(MetaProfileUnit,var->Units))) {
            Lib_Log(APP_LIBMETA,APP_WARNING,"%s: Specified unit not defined in udunits: %s",__func__,var->Units);
            return(NULL);
         } else {
            json_object_set_string(objval,var->Units);
         }
#else
         json_object_set_string(objval,var->Units);
      }
#endif
   }

   return(Obj);
}

/**----------------------------------------------------------------------------
 * @brief  Extract variable information

 * @date   July 2023
 *    @param[in]   Obj           Profile json object
 *    @param[out]  StandardName  Standard name (CF table)
 *    @param[out]  RPNName       RPN nomvar
 *    @param[out]  LongName      Long name
 *    @param[out]  Description   Varibale description
 *    @param[out]  Unit          Data unit (udunits)
 *
 *    @return                    json_object pointer (NULL if error)
*/
json_object *Meta_GetVar(json_object *Obj,char **StandardName,char **RPNName,char **LongName,char **Description,char **Unit) {

   json_object *objval=NULL;

   if (StandardName) {
      json_pointer_get(Obj,"/standard_name",&objval);
      *StandardName=(char*)json_object_get_string(objval);
   }
   if (RPNName) {
      json_pointer_get(Obj,"/rpn_name",&objval);
      *RPNName=(char*)json_object_get_string(objval);
   }
   if (LongName) {
      json_pointer_get(Obj,"/long_name",&objval);
      *LongName=(char*)json_object_get_string(objval);
   }
   if (Description) {
      json_pointer_get(Obj,"/description",&objval);
      *Description=(char*)json_object_get_string(objval);
   }
   if (Unit) {
      json_pointer_get(Obj,"/unit",&objval);
      *Unit=(char*)json_object_get_string(objval);
   }

   return(Obj);
}

double Meta_DurationToSeconds(char *Unit) {

   double sec;

   if (!strncmp(MetaTimeUnits[0],Unit,11)) {
      sec=0.001;
   } else if (!strncmp(MetaTimeUnits[1],Unit,6)) {
      sec=1;
   } else if (!strncmp(MetaTimeUnits[2],Unit,6)) {
      sec=60;
   } else if (!strncmp(MetaTimeUnits[3],Unit,4)) {
      sec=3600;
   } else if (!strncmp(MetaTimeUnits[4],Unit,3)) {
      sec=21600;
   } else if (!strncmp(MetaTimeUnits[5],Unit,5)) {
//      t0.tm_mon+=Duration*Step;
   } else if (!strncmp(MetaTimeUnits[6],Unit,4)) {
//      t0.tm_year+=Duration*Step;
   } else if (!strncmp(MetaTimeUnits[7],Unit,6)) {
//      t0.tm_year+=Duration*Step*10;
   } else if (!strncmp(MetaTimeUnits[8],Unit,10)) {
//      t0.tm_year+=Duration*Step*100;
   } else {
      Lib_Log(APP_LIBMETA,APP_ERROR,"%s: Invalid time unit: %s\n",__func__,Unit);
      return(0.0);
   }

   return(sec);
}

/**----------------------------------------------------------------------------
 * @brief  Define temporal information

 * @date   July 2023
 *    @param[in]  Obj           Profile json object
 *    @param[in]  T0            Forecast reference time
 *    @param[in]  Step          Timestep number
 *    @param[in]  Duration      Timestep duration
 *    @param[in]  Unit          Timestep step unit
 *
 *    @return                   json_object pointer (NULL if error)
*/
json_object *Meta_DefForecastTime(json_object *Obj,time_t T0,int32_t Step,double Duration,char *Unit) {
 
   json_object *obj=NULL,*objval=NULL;
   struct tm t0;
   uint32_t milli=0,sec=0;
   char timestr[32],timemil[32];

   gmtime_r(&T0,&t0);
   strftime(timestr,32,"%FT%TZ",&t0);
   json_pointer_get(Obj,"/forecast_reference_datetime",&objval);
   json_object_set_string(objval,timestr);

   if (!strncmp(MetaTimeUnits[0],Unit,11)) {
      milli=Duration*Step;
      sec=(int)milli/1000;
      t0.tm_sec+=sec;
      milli=milli-(sec*1000);
   } else if (!strncmp(MetaTimeUnits[1],Unit,6)) {
      t0.tm_sec+=Duration*Step;
   } else if (!strncmp(MetaTimeUnits[2],Unit,6)) {
      t0.tm_min+=Duration*Step;
   } else if (!strncmp(MetaTimeUnits[3],Unit,4)) {
      t0.tm_hour+=Duration*Step;
   } else if (!strncmp(MetaTimeUnits[4],Unit,3)) {
      t0.tm_mday+=Duration*Step;
   } else if (!strncmp(MetaTimeUnits[5],Unit,5)) {
      t0.tm_mon+=Duration*Step;
   } else if (!strncmp(MetaTimeUnits[6],Unit,4)) {
      t0.tm_year+=Duration*Step;
   } else if (!strncmp(MetaTimeUnits[7],Unit,6)) {
      t0.tm_year+=Duration*Step*10;
   } else if (!strncmp(MetaTimeUnits[8],Unit,10)) {
      t0.tm_year+=Duration*Step*100;
   } else {
      Lib_Log(APP_LIBMETA,APP_ERROR,"%s: Invalid time unit: %s\n",__func__,Unit);
      return(NULL);
   }

   json_pointer_get(Obj,"/forecast_period/unit",&objval);
   json_object_set_string(objval,Unit);
   json_pointer_get(Obj,"/forecast_period/step",&objval);
   json_object_set_int(objval,Step);
   json_pointer_get(Obj,"/forecast_period/value",&objval);
   json_object_set_double(objval,Duration);

   json_pointer_get(Obj,"/forecast_datetime",&objval);

   T0=timegm(&t0);
   strftime(timestr,32,"%FT%T",&t0);
   if (milli) {
      snprintf(timemil,32,"%s.%03iZ",timestr,milli);
   } else {
      snprintf(timemil,32,"%sZ",timestr);
   }
   json_object_set_string(objval,timemil);
   return(Obj);
}

/**----------------------------------------------------------------------------
 * @brief  Extract temporal information

 * @date   July 2023
 *    @param[in]   Obj           Profile json object
 *    @param[out]  T0            Forecast reference time
 *    @param[out]  Step          Timestep number
 *    @param[out]  Duration      Timestep duration
 *    @param[out]  Unit          Timestep step unit
 *
 *    @return                    json_object pointer (NULL if error)
*/
json_object *Meta_GetForecastTime(json_object *Obj,time_t *T0,int32_t *Step,double *Duration,char **Unit) {
 
   json_object *obj=NULL,*objval=NULL;
   struct tm t0;
   char *timestr;

   if (Step) {
      json_pointer_get(Obj,"/forecast_period/step",&objval);
      *Step=json_object_get_int(objval);
   }
   if (Duration) {
      json_pointer_get(Obj,"/forecast_period/value",&objval);
      *Duration=json_object_get_double(objval);
   }
   if (Unit) {
      json_pointer_get(Obj,"/forecast_period/unit",&objval);
      *Unit=(char*)json_object_get_string(objval);
   }
   if (T0) {
      json_pointer_get(Obj,"/forecast_reference_datetime",&objval);
      timestr=(char*)json_object_get_string(objval);
      strptime(timestr,"%FT%T",&t0);
      *T0=timegm(&t0);
   }

   return(Obj);
}

/**----------------------------------------------------------------------------
 * @brief  Resolve references of identifiers by string definitions in place

 * @date   Novembre 2023
 *    @param[in]  Obj           Profile json object
 *    @param[in]  ObjMaster     Reference json object
 *
 *    @return                   json_object pointer (NULL if error)
*/
json_object *Meta_Resolve(json_object *Obj,json_object *ObjMaster) {

   char *id=NULL;
   json_object *obj=NULL,*objref=NULL;

   json_pointer_get(Obj,"/horizontal_reference",&obj);
   if (obj) {
      id=(char*)json_object_get_string(obj);
      if (!(objref=Meta_FindHorizontalObj(id,ObjMaster))) {
         Lib_Log(APP_LIBMETA,APP_WARNING,"%s: Could not resolve horizontal reference %s",__func__,id);
      }
      json_object_object_add(Obj,"horizontal_reference",json_object_get(objref));
   }

   json_pointer_get(Obj,"/vertical_level/vertical_reference",&obj);
   if (obj) {
      id=(char*)json_object_get_string(obj);
      if (!(objref=Meta_FindVerticalObj(id,ObjMaster))) {
         Lib_Log(APP_LIBMETA,APP_WARNING,"%s: Could not find horizontal reference %s",__func__,id);
      }
      json_pointer_get(Obj,"/vertical_level",&obj);
      json_object_object_add(obj,"vertical_reference",json_object_get(objref));
   }
   
   return(Obj);
}

json_object *Meta_FindVerticalObj(char* Identifier,json_object *ObjMaster) {

   TMetaProfile *prof=NULL;
   json_object  *obj=NULL,*obj_id=NULL,*obj_it=NULL;
   int32_t          nb=0,i=0;

   if (ObjMaster) {
      json_pointer_get(ObjMaster,"/vertical_references",&obj);
   } else {
      if (!(prof=Meta_GetProfile(NULL))) {
         return(NULL);
      }
      json_pointer_get(prof->Z,"/vertical_references",&obj);
   }

   if (obj) {
      nb=json_object_array_length(obj);
      for(i=0;i<nb;i++) {
         obj_it=json_object_array_get_idx(obj,i);
         json_pointer_get(obj_it,"/identifier",&obj_id);
         if (strncmp(Identifier,json_object_get_string(obj_id),64)==0) {
            break;
         }
      }
   }
   return(i<nb?obj_it:NULL);
}

json_object *Meta_FindHorizontalObj(char* Identifier,json_object *ObjMaster) {

   TMetaProfile *prof=NULL;
   json_object  *obj=NULL,*obj_id=NULL,*obj_it=NULL;
   int32_t          nb=0,i=0;

   if (ObjMaster) {
      json_pointer_get(ObjMaster,"/horizontal_references",&obj);
   } else {
      if (!(prof=Meta_GetProfile(NULL))) {
         return(NULL);
      }
      json_pointer_get(prof->XY,"/horizontal_references",&obj);
   }

   if (obj) {
      nb=json_object_array_length(obj);
      for(i=0;i<nb;i++) {
         obj_it=json_object_array_get_idx(obj,i);
         json_pointer_get(obj_it,"/identifier",&obj_id);
         if (strncmp(Identifier,json_object_get_string(obj_id),64)==0) {
            break;
         }
      }
   }
   return(i<nb?obj_it:NULL);
}

/**----------------------------------------------------------------------------
 * @brief  Add vertical reference

 * @date   July 2023
 *    @param[in]  Obj           Profile json object
 *    @param[in]  Identifier    Identifier of the vertical reference
 *    @param[in]  Copy          Add complete definition or only identifier
 *
 *    @return                    json_object pointer (NULL if error)
*/
json_object *Meta_AddVerticalRef(json_object *Obj,char* Identifier,int Copy) {

   json_object *obj=NULL,*objval=NULL,*objref=NULL;

   if (json_pointer_get(Obj,"/vertical_references",&objval)!=0) {
      Lib_Log(APP_LIBMETA,APP_ERROR,"%s: Could not find object: %s\n",__func__,"/vertical_references");
      return(NULL);
   }

   // Do we copy the reference or just define the Identifier
   if (Copy) {
      // Find the vertical reference
      if (!(objref=Meta_FindVerticalObj(Identifier,NULL))) {
         Lib_Log(APP_LIBMETA,APP_ERROR,"%s: Could not find vertical reference %s\n",__func__,Identifier);
         return(NULL);
      }

      json_object_array_add(objval,objref);
   } else {
      json_object_array_add(objval,json_object_new_string(Identifier));
   }
   return(Obj);
}

/**----------------------------------------------------------------------------
 * @brief  Define vertical reference

 * @date   July 2023
 *    @param[in]  Obj           Profile json object
 *    @param[in]  Identifier    Identifier of the vertical reference
 *    @param[in]  Value         Vertical level value
 *    @param[in]  Copy          Add complete definition or only identifier
 *
 *    @return                    json_object pointer (NULL if error)
*/
json_object *Meta_DefVerticalRef(json_object *Obj,char* Identifier,double *Value,int32_t Nb,int Copy) {

   json_object *obj=NULL,*objval=NULL,*objref=NULL;
   int32_t l;

   json_pointer_get(Obj,"/vertical_level",&obj);

   // Do we copy the reference or just define the Identifier
   if (Copy) {
      // Find the vertical reference
      if (!(objref=Meta_FindVerticalObj(Identifier,NULL))) {
         Lib_Log(APP_LIBMETA,APP_ERROR,"%s: Could not find vertical reference %s\n",__func__,Identifier);
         return(NULL);
      }

      json_object_object_add(obj,"vertical_reference",objref);
   } else  {
      json_pointer_get(obj,"/vertical_reference",&objval);
      json_object_set_string(objval,Identifier);
   }
   json_pointer_get(obj,"/value",&objval);
   for(l=0;l<Nb;l++){
       json_object_array_add(objval,json_object_new_double(Value[l]));
   }

   return(Obj);
}

/**----------------------------------------------------------------------------
 * @brief  Extract vertical reference

 * @date   July 2023
 *    @param[in]   Obj           Profile json object
 *    @param[in]   Index         Level index (data cube)
 *    @param[out]  Identifier    Identifier of the vertical reference
 *    @param[out]  Value         Vertical level value
 *
 *    @return                    json_object pointer (NULL if error)
*/
//TODO: muli level
json_object *Meta_GetVerticalRef(json_object *Obj,int32_t Index,char **Identifier,double *Value) {

   json_object *objval=NULL;

   if (Identifier) {
      json_pointer_get(Obj,"/vertical_level/vertical_reference",&objval);
      *Identifier=(char*)json_object_get_string(objval);
   }
   if (Value) {
      json_pointer_get(Obj,"/vertical_level/value",&objval);
      *Value=json_object_get_double(json_object_array_get_idx(objval,Index));
   }
   return(Obj);
}

/**----------------------------------------------------------------------------
 * @brief  Add horizontal reference

 * @date   July 2023
 *    @param[in]  Obj           Profile json object
 *    @param[in]  Identifier    Identifier of the horizontal reference
 *    @param[in]  Copy          Add complete definition or only identifier
 *
 *    @return                    json_object pointer (NULL if error)
*/
json_object *Meta_AddHorizontalRef(json_object *Obj,char* Identifier,int Copy) {

   json_object *obj=NULL,*objval=NULL,*objref=NULL;

   if (json_pointer_get(Obj,"/horizontal_references",&objval)!=0) {
      Lib_Log(APP_LIBMETA,APP_ERROR,"%s: Could not find object: %s\n",__func__,"/horizontal_references");
      return(NULL);
   }

   // Do we copy the reference or just define the Identifier
   if (Copy) {
      // Find the vertical reference
      if (!(objref=Meta_FindHorizontalObj(Identifier,NULL))) {
         Lib_Log(APP_LIBMETA,APP_ERROR,"%s: Could not find horizontal reference %s\n",__func__,Identifier);
         return(NULL);
      }

      json_object_array_add(objval,objref);
   } else {
      json_object_array_add(objval,json_object_new_string(Identifier));
   }
   return(Obj);
}

/**----------------------------------------------------------------------------
 * @brief  Define horizontal reference

 * @date   July 2023
 *    @param[in]  Obj           Profile json object
 *    @param[in]  Identifier    Identifier of the horizontal reference
 *    @param[in]  Copy          Add complete definition or only identifier
 *
 *    @return                   json_object pointer (NULL if error)
*/
json_object *Meta_DefHorizontalRef(json_object *Obj,char* Identifier,int Copy) {

   json_object *obj=NULL,*objval=NULL,*objref=NULL;

   // Do we copy the reference or just define the Identifier
   if (Copy) {
      // Find the vertical reference
      if (!(objref=Meta_FindHorizontalObj(Identifier,NULL))) {
         Lib_Log(APP_LIBMETA,APP_ERROR,"%s: Could not find horizontal reference %s\n",__func__,Identifier);
         return(NULL);
      }

      json_object_object_add(Obj,"horizontal_reference",objref);
   } else {
      json_pointer_get(Obj,"/horizontal_reference",&objval);
      json_object_set_string(objval,Identifier);
   }
   return(Obj);
}

/**----------------------------------------------------------------------------
 * @brief  Extract horizontal reference

 * @date   July 2023
 *    @param[in]   Obj           Profile json object
 *    @param[out]  Identifier    Identifier of the horizontal reference
 *
 *    @return                    json_object pointer (NULL if error)
*/
json_object *Meta_GetHorizontalRef(json_object *Obj,char **Identifier) {

   json_object *objval=NULL;

   if (Identifier) {
      json_pointer_get(Obj,"/horizontal_reference",&objval);
      *Identifier=(char*)json_object_get_string(objval);
   }
   return(Obj);
}

/**----------------------------------------------------------------------------
 * @brief  Add a single cell method

 * @date   July 2023
 *    @param[in]  Obj           Profile json object
 *    @param[in]  Method        Cell method to add
 *
 *    @return                    json_object pointer (NULL if error)
*/
json_object *Meta_AddCellMethod(json_object *Obj,char *Method) {

   TMetaProfile *prof=NULL;
   json_object *objval=NULL;
   char *c;

   if (!(prof=Meta_GetProfile(Meta_Version(Obj)))) {
      return(NULL);
   }

   if (MetaValidate) {
      if (!(c=Meta_ValidateToken(prof->CellMethods,Method))) {
         Lib_Log(APP_LIBMETA,APP_ERROR,"%s: Invalid cell method: %s\n",__func__,Method);
         return(NULL);
      }
      if (!(c=Meta_ValidateToken(prof->CellProcesses,++c))) {
         Lib_Log(APP_LIBMETA,APP_ERROR,"%s: Invalid cell process: %s\n",__func__,Method);
         return(NULL);
      }
   }

   json_pointer_get(Obj,"/cell_methods",&objval);
   if (!objval) {
      Lib_Log(APP_LIBMETA,APP_ERROR,"%s: Undefined objet array: cell_methods\n",__func__);
      return(NULL);
   }
   json_object_array_add(objval,json_object_new_string(Method));
  
   return(Obj);
}

/**----------------------------------------------------------------------------
 * @brief  Set methods from a list 

 * @date   July 2023
 *    @param[in]  Obj           Profile json object
 *    @param[in]  Qualifiers    Qualifiers (NULL terminated list)
 *
 *    @return                   json_object pointer (NULL if error)
*/
json_object *Meta_SetCellMethods(json_object *Obj,char *Methods[]) {

   TMetaProfile *prof=NULL;
   json_object *objval=NULL;
   int   nb=0;
   char *c,*method=NULL;

   if (!(prof=Meta_GetProfile(Meta_Version(Obj)))) {
      return(NULL);
   }

   objval=json_object_new_array();
   json_object_object_add(Obj,"cell_methods",objval);

   while((method=Methods[nb++])) {
      if (MetaValidate) {
         if (!(c=Meta_ValidateToken(prof->CellMethods,method))) {
            Lib_Log(APP_LIBMETA,APP_ERROR,"%s: Invalid cell method: %s\n",__func__,method);
            return(NULL);
         }
         if (!(c=Meta_ValidateToken(prof->CellProcesses,++c))) {
            Lib_Log(APP_LIBMETA,APP_ERROR,"%s: Invalid cell process: %s\n",__func__,method);
            return(NULL);
         }
      }

      json_object_array_add(objval,json_object_new_string(method));
   }
  
   return(Obj);
}

/**----------------------------------------------------------------------------
 * @brief  Clear cell method list

 * @date   July 2023
 *    @param[in]  Obj           Profile json object
 *
 *    @return                   json_object pointer (NULL if error)
*/
json_object *Meta_ClearCellMethods(json_object *Obj) {

   json_object_object_add(Obj,"cell_methods",json_object_new_array());
  
   return(Obj);
}

/**----------------------------------------------------------------------------
 * @brief  Add a single data qualifier

 * @date   July 2023
 *    @param[in]  Obj           Profile json object
 *    @param[in]  Qualifier     Qualifier
 *
 *    @return                   json_object pointer (NULL if error)
*/
json_object *Meta_AddQualifier(json_object *Obj,char *Qualifier) {

   TMetaProfile *prof=NULL;
   json_object *objval=NULL;
   
   if (!(prof=Meta_GetProfile(Meta_Version(Obj)))) {
      return(NULL);
   }

   if (MetaValidate && !Meta_ValidateToken(prof->Qualifiers,Qualifier)) {
      Lib_Log(APP_LIBMETA,APP_ERROR,"%s: Invalid qualifier: %s\n",__func__,Qualifier);
      return(NULL);
   }

   json_pointer_get(Obj,"/qualifiers",&objval);
   if (!objval) {
      Lib_Log(APP_LIBMETA,APP_ERROR,"%s: Undefined objet array: qualifiers\n",__func__);
      return(NULL);
   }
   json_object_array_add(objval,json_object_new_string(Qualifier));
  
   return(Obj);
}

/**----------------------------------------------------------------------------
 * @brief  Set qualifiers from a list

 * @date   July 2023
 *    @param[in]  Obj           Profile json object
 *    @param[in]  Qualifiers    Qualifiers (NULL terminated list)
 *
 *    @return                   json_object pointer (NULL if error)
*/
json_object *Meta_SetQualifiers(json_object *Obj,char **Qualifiers) {

   TMetaProfile *prof=NULL;
   json_object *objval=NULL;
   int   nb=0;
   char *qualifier=NULL;

   if (!(prof=Meta_GetProfile(Meta_Version(Obj)))) {
      return(NULL);
   }

   objval=json_object_new_array();
   json_object_object_add(Obj,"qualifiers",objval);

   while((qualifier=Qualifiers[nb++])) {
      if (MetaValidate && !Meta_ValidateToken(prof->Qualifiers,qualifier)) {
         Lib_Log(APP_LIBMETA,APP_ERROR,"%s: Invalid qualifier: %s\n",__func__,qualifier);
         return(NULL);
      }

      json_object_array_add(objval,json_object_new_string(qualifier));
   }
  
   return(Obj);
}

/**----------------------------------------------------------------------------
 * @brief  Clear data qualifier list

 * @date   July 2023
 *    @param[in]  Obj           Profile json object
 *
 *    @return                   json_object pointer (NULL if error)
*/
json_object *Meta_ClearQualifiers(json_object *Obj) {

   json_object_object_add(Obj,"qualifiers",json_object_new_array());
  
   return(Obj);
}

/**----------------------------------------------------------------------------
 * @brief  Add a missing value

 * @date   July 2023
 *    @param[in]  Obj           Profile json object
 *    @param[in]  Reason        Reason for/Definition of the mising value
 *    @param[in]  Value         Value representing missing data
 *
 *    @return                   json_object pointer (NULL if error)
*/
json_object *Meta_AddMissingValue(json_object *Obj,char *Reason,double Value) {

   TMetaProfile *prof=NULL;
   json_object *objval=NULL,*objmis=NULL;

   if (!(prof=Meta_GetProfile(Meta_Version(Obj)))) {
      return(NULL);
   }

   if (MetaValidate && !(Meta_ValidateToken(prof->Reasons,Reason))) {
      Lib_Log(APP_LIBMETA,APP_ERROR,"%s: Invalid reason: %s\n",__func__,Reason);
      return(NULL);
   }
   
   json_pointer_get(Obj,"/missing_values",&objval);
   if (!objval) {
      Lib_Log(APP_LIBMETA,APP_ERROR,"%s: Undefined objet array: missing_values\n",__func__);
      return(NULL);
   }
   objmis=json_object_new_object();
   json_object_object_add(objmis,"reason",json_object_new_string(Reason));
   json_object_object_add(objmis,"value",json_object_new_double(Value));
   json_object_array_add(objval,objmis);
  
   return(Obj);
}

/**----------------------------------------------------------------------------
 * @brief  Add a missing value

 * @date   July 2023
 *    @param[in]   Obj           Profile json object
 *    @param[in]   Idx           Index of the missing value
 *    @param[out]  Reason        Reason for/Definition of the mising value
 *    @param[out]  Value         Value representing missing data
 *
 *    @return                   json_object pointer (NULL if error)
*/
json_object *Meta_GetMissingValue(json_object *Obj,int32_t Idx,char **Reason,double *Value) {

   json_object *objval=NULL,*objmis=NULL,*obj=NULL;

   json_pointer_get(Obj,"/missing_values",&obj);
   objmis=json_object_array_get_idx(obj,Idx);

   if (Reason) {
      json_pointer_get(objmis,"/reason",&objval);
      *Reason=(char*)json_object_get_string(objval);
   }
   if (Value) {
      json_pointer_get(objmis,"/value",&objval);
      *Value=json_object_get_double(objval);
   }
  
   return(Obj);
}

/**----------------------------------------------------------------------------
 * @brief  Clear missing value list

 * @date   July 2023
 *    @param[in]  Obj           Profile json object
 *
 *    @return                   json_object pointer (NULL if error)
*/
json_object *Meta_ClearMissingValues(json_object *Obj) {

   json_object_object_add(Obj,"missing_values",json_object_new_array());
  
   return(Obj);
}

/**----------------------------------------------------------------------------
 * @brief  Define data specification

 * @date   July 2023
 *    @param[in]  Obj           Profile json object
 *    @param[in]   NI            I dimension
 *    @param[in]   NJ            J dimension
 *    @param[in]   NK            K dimension
 *    @param[in]  Type          Data type
 *    @param[in]  Compression   Compression type
 *    @param[in]  Pack          Number of stored bits of precision (packing)
 *    @param[in]  Bits          Number of in memory bits
 *    @param[in]  Min           Minimum value
 *    @param[in]  Max           Maximum value
 *
 *    @return                   json_object pointer (NULL if error)
*/
json_object *Meta_DefData(json_object *Obj,int32_t NI,int32_t NJ,int32_t NK,char *Type,char *Compression,int32_t Pack,int32_t Bit,double Min,double Max) {

   TMetaProfile *prof=NULL;
   json_object *obj=NULL,*objval=NULL;
   int32_t i;

   if (!(prof=Meta_GetProfile(Meta_Version(Obj)))) {
      return(NULL);
   }

   if (MetaValidate && !(Meta_ValidateToken(prof->Compressions,Compression))) {
      Lib_Log(APP_LIBMETA,APP_ERROR,"%s: Invalid compression: %s\n",__func__,Compression);
      return(NULL);
   }
   if (MetaValidate && !(Meta_ValidateToken(prof->Types,Type))) {
      Lib_Log(APP_LIBMETA,APP_ERROR,"%s: Invalid data type: %s\n",__func__,Type);
      return(NULL);
   }

   json_pointer_get(Obj,"/data",&obj);

   json_pointer_get(obj,"/type",&objval);
   json_object_set_string(objval,Type);
   json_pointer_get(obj,"/compression",&objval);
   json_object_set_string(objval,Compression);
   json_pointer_get(obj,"/pack",&objval);
   json_object_set_int(objval,Pack);
   json_pointer_get(obj,"/bits",&objval);
   json_object_set_int(objval,Bit);

   json_object_object_add(Obj,"size",json_object_new_array());
   json_pointer_get(Obj,"/size",&objval);
   json_object_array_add(objval,json_object_new_int(NI));
   json_object_array_add(objval,json_object_new_int(NJ));
   json_object_array_add(objval,json_object_new_int(NK));

   json_pointer_get(obj,"/bounds/min",&objval);
   json_object_set_double(objval,Min);
   json_pointer_get(obj,"/bounds/max",&objval);
   json_object_set_double(objval,Max);

   return(Obj);
}

/**----------------------------------------------------------------------------
 * @brief  Extract data specification

 * @date   July 2023
 *    @param[in]   Obj           Profile json object
 *    @param[out]  NI            I dimension
 *    @param[out]  NJ            J dimension
 *    @param[out]  NK            K dimension
 *    @param[out]  Type          Data type
 *    @param[out]  Compression   Compression type
 *    @param[in]   Pack          Number of stored bits of precision (packing)
 *    @param[in]   Bits          Number of in memory bits
 *    @param[out]  Min           Minimum value
 *    @param[out]  Max           Maximum value *
 *    @return                    json_object pointer (NULL if error)
*/
json_object *Meta_GetData(json_object *Obj,int32_t *NI,int32_t *NJ,int32_t *NK,char **Type,char **Compression,int32_t *Pack,int32_t *Bit,double *Min,double *Max) {

   json_object *obj=NULL,*objval=NULL;

   json_pointer_get(Obj,"/size",&objval);
   if (NI) *NI=json_object_get_int(json_object_array_get_idx(objval,0));
   if (NJ) *NJ=json_object_get_int(json_object_array_get_idx(objval,1));
   if (NK) *NK=json_object_get_int(json_object_array_get_idx(objval,2));

   json_pointer_get(Obj,"/data",&obj);

   if (Type) {
      json_pointer_get(obj,"/type",&objval);
      *Type=(char*)json_object_get_string(objval);
   }
   if (Compression) {
      json_pointer_get(obj,"/compression",&objval);
      *Compression=(char*)json_object_get_string(objval);
   }
   if (Pack) {
      json_pointer_get(obj,"/pack",&objval);
      *Pack=json_object_get_int(objval);
   }
   if (Bit) {
      json_pointer_get(obj,"/bits",&objval);
      *Bit=json_object_get_int(objval);
   }
   if (Min) {
      json_pointer_get(Obj,"/bounds/min",&objval);
      *Min=json_object_get_double(objval);
   }
   if (Max) {
      json_pointer_get(Obj,"/bounds/max",&objval);
      *Max=json_object_get_double(objval);
   }
   return(Obj);
}
 
/**----------------------------------------------------------------------------
 * @brief  Define File level metadata
 * @date   July 2023
 *    @param[in]   Obj           Array json object
 *    @param[in]   Institution   ex:CMC
 *    @param[in]   Discipline    ex:Meteorology
 *    @param[in]   Title         ex: G100
 *    @param[in]   Source        ex: GDPS-5.2.0
 *    @param[in]   Description   ex: Global system ...
 *    @param[in]   State         ex: Experimental,Parallel,Operational, ...
 *
 *    @return                    object reference
*/
json_object *Meta_DefFile(json_object *Obj,char *Institution,char* Discipline,char *Title,char *Source,char *Description,char *State) {

   json_object *objval=NULL;

   if (Institution) {
      json_pointer_get(Obj,"/institution",&objval);
      json_object_set_string(objval,Institution);
   }
   if (Discipline) {
      json_pointer_get(Obj,"/discipline",&objval);
      json_object_set_string(objval,Discipline);
   }
   if (Title) {
      json_pointer_get(Obj,"/title",&objval);
      json_object_set_string(objval,Title);
   }
   if (Source) {
      json_pointer_get(Obj,"/source",&objval);
      json_object_set_string(objval,Source);
   }
   if (Description) {
      json_pointer_get(Obj,"/description",&objval);
      json_object_set_string(objval,Description);
   }

   return(Obj);
}

/**----------------------------------------------------------------------------
 * @brief  Format an object for output
 * @date   July 2023
 *    @param[in]   Obj           json object
 *
 *    @return                    formatted string
*/
char *Meta_Stringify(json_object *Obj) {

   return((char*)json_object_to_json_string_ext(Obj,JSON_C_TO_STRING_PRETTY));
}

/**----------------------------------------------------------------------------
 * @brief  Parse a string into a metadata object
 * @date   July 2023
 *    @param[in]   MetaString    metadat json string
 *
 *    @return                    json object
*/
json_object* Meta_Parse(const char *MetaString) {

   struct json_object *obj=NULL;
   enum json_tokener_error jerr;

   if (MetaString) {
	   obj = json_tokener_parse_verbose(MetaString,&jerr);
   }

   if (!obj) {
      Lib_Log(APP_LIBMETA,APP_ERROR,"%s: %s\n",__func__,json_tokener_error_desc(jerr));
      return(NULL);
   }
   return(obj);
}

/**----------------------------------------------------------------------------
 * @brief  Return an array length
 * @date   July 2023
 *    @param[in]   Obj           Array json object
 *
 *    @return                    Array length
*/
int32_t Meta_ArrayLength(json_object *Obj) {

   return(json_object_array_length(Obj));
}

/**----------------------------------------------------------------------------
 * @brief  Find an object reference in an array using a string token.
 *         The search will only compare on the token length so partial
 *         token are valid
 * @date   July 2023
 *    @param[in]   Obj           Array json object
 *    @param[in]   Token         String token to search for
 *
 *    @return                    object reference
*/
json_object* Meta_ArrayFind(json_object *Obj,char *Token) {

   json_object *obj;
   const char  *string;
   int32_t          n;

   n=strlen(Token);
   n=n>META_TOKEN_MAXLEN?META_TOKEN_MAXLEN:n;

   for(int32_t i=0;i<Meta_ArrayLength(Obj);i++) {
      obj=json_object_array_get_idx(Obj,i);
      string=json_object_get_string(obj);
      if (strncmp(Token,string,n)==0) {
         return(obj);
      }
   }
   return(NULL);
}

/**----------------------------------------------------------------------------
 * @brief  Get an object reference from an array
 * @date   July 2023
 *    @param[in]   Obj           Array json object
 *    @param[in]   Idx           Index within array
 *
 *    @return                    object reference
*/
json_object* Meta_ArrayGetObject(json_object *Obj,int32_t Idx) {

   return(json_object_array_get_idx(Obj,Idx));
}

/**----------------------------------------------------------------------------
 * @brief  Get an object reference from a json path
 * @date   July 2023
 *    @param[in]   Obj           Profile json object
 *    @param[in]   Path          Search path (ex: /bounds/lower)
 *
 *    @return                    object reference
*/
json_object* Meta_GetObject(json_object *Obj,char *Path) {

   json_object *obj=NULL;

   json_pointer_get(Obj,Path,&obj);

   return(obj);
}

/**----------------------------------------------------------------------------
 * @brief  Get a string representation of an object
 * @date   July 2023
 *    @param[in]   Obj           Profile json object
 *
 *    @return                    String representation
*/
char* Meta_GetObjectString(json_object *Obj) {

   return((char*)json_object_get_string(Obj));
}

/**----------------------------------------------------------------------------
 * @brief  Copy an object (and all it's children)
 * @date   July 2023
 *    @param[in]   Obj           Profile json object
 *
 *    @return                    New object (NULL if error)
*/
json_object *Meta_Copy(json_object *Obj) {

   json_object *dst=NULL;

   if (json_object_deep_copy(Obj,&dst,NULL)==0) {
      return(dst);
   } else {
      return(NULL);
   }
}

/**----------------------------------------------------------------------------
 * @brief  Check if objects match
 * @date   July 2023
 *    @param[in]   Obj1          Profile json object
 *    @param[in]   Obj2          Profile json object
 *    @param[in]   RegExp        Use regular expression for string comparisons
 *
 *    @return                    True:match or False
*/
int32_t Meta_Match(json_object *Obj1,json_object *Obj2,int RegExp) {

   double       val1,val2;
   const char  *str1,*str2;
   json_object *obj2=NULL,*objval1=NULL,*objval2=NULL;
   regex_t      re;
   int32_t      l1,l2,found,regi=FALSE;

   json_object_object_foreach(Obj1, key, obj1) { 
      if (!(obj2=json_object_object_get(Obj2,key))) {
         return(FALSE);
      }

      switch (json_object_get_type(obj1)) {
         // For numbers, we compare value to value
         case json_type_boolean: 
         case json_type_double: 
         case json_type_int: 
            val1=json_object_get_double(obj1);
            val2=json_object_get_double(obj2);
            if (val1!=-1 && val1==val1 && val1!=val2) {
               return(FALSE);
            }
            break; 

         // For strings, do a string compare and if not, try a regexp
         case json_type_string: 
            str1=json_object_get_string(obj1);
            str2=json_object_get_string(obj2);

            if (str1 && str2) {
               if (strcmp(str1,str2)!=0)  {
                  if (RegExp) {
                     if (regcomp(&re,str1,REG_EXTENDED|REG_NOSUB|REG_ICASE)!=0)  {
                        Lib_Log(APP_LIBMETA,APP_ERROR,"%s: Invalid comparison token: %s\n",__func__,str1);
                        return(FALSE);
                     }
                     if (regexec(&re,str2,(size_t)0,NULL,0)!=0) {
                        regfree(&re);
                        return(FALSE);
                     }
                  }
               }
            }
            break; 

         // Recurse on objects
         case json_type_object: 
            if (!Meta_Match(obj1,obj2,RegExp)) {
               return(FALSE);
            }
            break;

         // For array, we make sure each of array1 values is included within array2 values
         case json_type_array: 
            found=0;
            regi=FALSE;
            l1=json_object_array_length(obj1);
            for(int32_t i1=0;i1<l1;i1++) {
               objval1=json_object_array_get_idx(obj1,i1);
               if (json_object_get_type(objval1)==json_type_string) {
                   if (str1=json_object_get_string(objval1)) {
                     if (str1[0]=='\0') {
                        found++;
                        break;
                     }
                     if (RegExp) {
                        if (regcomp(&re,str1,REG_EXTENDED|REG_NOSUB|REG_ICASE)!=0)  {
                           Lib_Log(APP_LIBMETA,APP_ERROR,"%s: Invalid comparison token: %s\n",__func__,str1);
                           return(FALSE);
                        }
                        regi=TRUE;
                     }
                  }
               } else {
                  val1=json_object_get_double(objval1);
                  if (val1==-1 || val1!=val1) {
                    found++;
                    break;
                  }
               }

               l2=json_object_array_length(obj2);
               for(int32_t i2=0;i2<l2;i2++) {
                  objval2=json_object_array_get_idx(obj2,i2);
                   
                  if (json_object_get_type(objval2)==json_type_string) {      
                     if (str2=json_object_get_string(objval2)) {
                        if (RegExp) {
                           if (regexec(&re,str2,(size_t)0,NULL,0)==0) {
                              found++;
                              break;  
                           }
                        } else {
                           if (strncmp(str1,str2,strlen(str1))==0) {
                              found++;
                              break;  
                           }
                        }
                     }
                  } else {
                     val2=json_object_get_double(objval2);
                     if (val1==val2) {
                        found++;
                        break;
                     }
                  }
               }
            }
            if (regi) regfree(&re);
            if (found<l1) {
               return(FALSE);
            }
            break;
      }
   }
   return(TRUE);
}

#ifdef HAVE_RMN
/**----------------------------------------------------------------------------
 * @brief  Decode a CMC datetime stamp into components

 * @date   July 2023
 *    @param[in]   Stamp         CMCDate Stamp
 *    @param[out]  YYYY          Year
 *    @param[out]  MM            Month
 *    @param[out]  DD            Day
 *    @param[out]  H             Hour
 *    @param[out]  M             Minute
 *    @param[out]  S             Second
 *
*/
void Meta_StampDecode(int32_t Stamp,int32_t *YYYY,int32_t *MM,int32_t *DD,int32_t *H,int32_t *M,int32_t *S) {

   int32_t op=-3,date,time;

   f77name(newdate)(&Stamp,&date,&time,&op);

   *YYYY=date/10000;
   *DD=date-((*YYYY)*10000);
   *MM=(*DD)/100;
   *DD-=((*MM)*100);

   *H=time/1000000;
   *S=time-(*H)*1000000;
   *M=(*S)/10000;
   *S-=(*M)*10000;
   *S/=100;
}

void Meta_StampEncode(int32_t *Stamp,int32_t YYYY,int32_t MM,int32_t DD,int32_t H,int32_t M,int32_t S) {

   int32_t op=3,date,time;

   date=YYYY*10000+MM*100+DD;
   time=H*1000000+M*10000+S*100;
   f77name(newdate)(Stamp,&date,&time,&op);
}


/**----------------------------------------------------------------------------
 * @brief  Decode a CMC datetime stamp into system seconds

 * @date   July 2023
 *    @param[in]   Stamp         CMCDate Stamp
  *
 *    @return                    system seconts (time_t)
*/
time_t Meta_Stamp2Seconds(int32_t Stamp) {

   int32_t           yyyy,mm,dd,hh,nn,ss;
   struct tm     tdate;

   extern long timezone;

   if (!Stamp) {
      return(0);
   }
   
   Meta_StampDecode(Stamp,&yyyy,&mm,&dd,&hh,&nn,&ss);

   tdate.tm_sec=ss;           /*seconds apres la minute [0,61]*/
   tdate.tm_min=nn;           /*minutes apres l'heure [0,59]*/
   tdate.tm_hour=hh;          /*heures depuis minuit [0,23]*/
   tdate.tm_mday=dd;          /*jour du mois [1,31]*/
   tdate.tm_mon=mm-1;         /*mois apres Janvier [0,11]*/
   tdate.tm_year=yyyy-1900;   /*annee depuis 1900*/
   tdate.tm_isdst=0;          /*Flag de l'heure avancee*/

   /* Force GMT and set back to original TZ after*/
   return(timegm(&tdate)-timezone);
}

int32_t Meta_Seconds2Stamp(time_t Sec) {

   int32_t         stamp=0,date,time,op=3;
   struct tm  *tsec;

   if (!Sec) {
      return(0);
   }
   tsec=gmtime(&Sec);
   date=(tsec->tm_year+1900)*10000+(tsec->tm_mon+1)*100+tsec->tm_mday;
   time=tsec->tm_hour*1000000+tsec->tm_min*10000+tsec->tm_sec*100;

   f77name(newdate)(&stamp,&date,&time,&op);
   return(stamp);
}

/**----------------------------------------------------------------------------
 * @brief  Convert a date/time to seconds
 * @date   July 2023
 *    @param[in]   YYYY         Year
 *    @param[in]   MM           Month
 *    @param[in]   DD           Day
 *    @param[in]   hh           Hour
 *    @param[in]   mm           Minute
 *    @param[in]   ss           Second
 *    @param[in]   GMT          GMT (GMT or local time)
 *   
 *    @return                   System seconds
*/
time_t Meta_DateTime2Seconds(int YYYY,int MM,int DD,int hh,int mm,int ss,int GMT) {

   struct tm date;

   extern long timezone;

   date.tm_sec=ss;         /*seconds apres la minute [0,61]*/
   date.tm_min=mm;         /*minutes apres l'heure [0,59]*/
   date.tm_hour=hh;        /*heures depuis minuit [0,23]*/

   date.tm_mday=DD;        /*jour du mois [1,31]*/
   date.tm_mon=MM-1;       /*mois apres Janvier [0,11]*/
   date.tm_year=YYYY-1900; /*annee depuis 1900*/
   date.tm_isdst=0;        /*Flag de l'heure avancee*/

   /* Force GMT and set back to original TZ after*/
   if (GMT) {
      return(mktime(&date)-timezone);
   } else {
      return(mktime(&date));
   }
}

/**----------------------------------------------------------------------------
 * @brief  Encode new JSON metadata from old metadata format
 * @date   July 2023
 *    @param[in]   Obj           Profile json object
 *    @param[in]   Var           Variable name
 *    
 *    @return                    Status (TRUE or FALSE)
*/
int32_t Meta_From89(json_object *Obj,const fst_record* const Rec)	{

   char tmp[FST_ETIKET_LEN+4],*c=NULL;
   char typvar[FST_TYPVAR_LEN];
   char nomvar[FST_NOMVAR_LEN];
   char etiket[FST_ETIKET_LEN];

   strncpy(typvar,Rec->typvar,FST_TYPVAR_LEN);
   strncpy(nomvar,Rec->nomvar,FST_NOMVAR_LEN);
   strncpy(etiket,Rec->etiket,FST_ETIKET_LEN);
  
   strtrim(nomvar,' ');
   strtrim(typvar,' ');
   strtrim(etiket,' ');

   // NOMVAR
   Meta_DefVarFromDict(Obj,nomvar);

   // DATEO,DEET,NPAS
   Meta_DefForecastTime(Obj,Meta_Stamp2Seconds(Rec->dateo),Rec->npas,Rec->deet,"second");

   // TYPVAR
   switch(typvar[0]) {
      case 'C': Meta_AddQualifier(Obj,"climatology");                                                
      case 'D': Meta_AddQualifier(Obj,"station"); break;  //   Données brutes aux stations                                
          case 'A': Meta_AddQualifier(Obj,"analysis");                                                   
  case 'E': Meta_AddQualifier(Obj,"error"); break;  //    Erreur mensuelle                                            
      case 'K': Meta_AddQualifier(Obj,"constant"); break;   // Constantes variées                                         
      case 'M': Meta_AddQualifier(Obj,"verification"); break;  //    Matrice de vérification (table contingente)                
//      case 'N': Meta_AddCellMethod(Obj,"member:"); break;  //  N@ Nombre de membres utilisés pour le calcul du champ         
      case 'O': Meta_AddQualifier(Obj,"observation");  break;                                                
      case 'P': Meta_AddQualifier(Obj,"prognosis");  break;                                                  
      case 'Q': Meta_AddQualifier(Obj,"diagnostic");  break;                                            
      case 'R': Meta_AddQualifier(Obj,"increment"); break;                                        
      case 'S': Meta_AddQualifier(Obj,"score"); break;                                            
      case 'T': Meta_AddQualifier(Obj,"timeserie"); break;                                          
//      case 'X': Meta_AddQualifier(Obj,""); break;    // Divers  
   };

   switch(typvar[1]) {                                                    
      case 'B': Meta_AddCellMethod(Obj,"clamped"); break; //  Borné                                                      
      case 'F': Meta_AddCellMethod(Obj,"filter:"); break; //  Filtré                                                     
//      case 'H': Meta_AddQualifier(Obj,""); break; //   Données manquantes                                         
      case 'I': Meta_AddCellMethod(Obj,"interpolation:"); break; //Interpolé                                                 
//      case 'M': Meta_AddCellMethod(Obj,""); break; //   Modifications multiples                                     
//      case 'U': Meta_AddCellMethod(Obj,""); break; //   Unités converties                                          
//      case 'Z': Meta_AddCellMethod(Obj,""); break; //   Zappé                                                      
   }

   // NPACK,DATYP,DASIZ will be done internally at write
//   Meta_DefData(Obj,Rec->ni,Rec->nj,Rec->nk,FST_TYPE_NAMES[Rec->datyp],"",Rec->npak,Rec->dasiz,0,0);
 
   // ETIKET
   snprintf(tmp,FST_ETIKET_LEN+4,"tag:%s",etiket);
   Meta_AddQualifier(Obj,tmp);

//TODO:
   // IP1,IP2,IP3
//   Meta_DefVerticalRef(prof_fld,"LEVEL_PRESSURE",1000.0,false);

   //GRTYP,IG1,IG2,IG3,IG4
//   Meta_DefHorizontalRef(prof_fld,"GRID_CYLINDRIC",false);
   return(TRUE);
}

int32_t Meta_To89(json_object *Obj,fst_record *Rec)	{

   char   *c1;
   double  d1;
   int32_t     i,i1,kind;
   time_t  t0;
   json_object *obj=NULL,*objval=NULL;

   // NOMVAR
   Rec->nomvar[0]='\0';
   if (Meta_GetVar(Obj,NULL,&c1,NULL,NULL,NULL)) {
      strncpy(Rec->nomvar,c1,FST_NOMVAR_LEN-1);
      strblank2end(Rec->nomvar,FST_NOMVAR_LEN);
   }
   // DATEO,DEET,NPAS
   if (Meta_GetForecastTime(Obj,&t0,&i1,&d1,&c1)) {
      Rec->dateo=Meta_Seconds2Stamp(t0);
      Rec->deet=Meta_DurationToSeconds(c1);
      Rec->ip2=Rec->npas=i1;
      Rec->ip3=0;
   }

   // TYPVAR, ETIKET
   Rec->typvar[0]='X';
   Rec->typvar[1]=' ';
   Rec->etiket[0]='\0';
   if (obj=Meta_GetObject(Obj,"/qualifiers")) {
      for(i=0;i<Meta_ArrayLength(obj);i++) {

         c1=Meta_GetObjectString(Meta_ArrayGetObject(obj,i));

         if (!strncmp(c1,"analysis",8)) {
            Rec->typvar[0]='A';
         } else if (!strncmp(c1,"climatology",11)) {
            Rec->typvar[0]='C';
         } else if (!strncmp(c1,"station",6)) {
            Rec->typvar[0]='D';
         } else if (!strncmp(c1,"error",5)) {
            Rec->typvar[0]='E';
         } else if (!strncmp(c1,"constant",7)) {
            Rec->typvar[0]='K';
         } else if (!strncmp(c1,"verification",12)) {
            Rec->typvar[0]='M';
   //      } else if (!strncmp(c1,"member",6)) {
   //         Rec->typvar[0]='N';
         } else if (!strncmp(c1,"observation",11)) {
            Rec->typvar[0]='O';
         } else if (!strncmp(c1,"prognosis",9)) {
            Rec->typvar[0]='P';
         } else if (!strncmp(c1,"diagnostic",10)) {
            Rec->typvar[0]='Q';
         } else if (!strncmp(c1,"increment",9)) {
            Rec->typvar[0]='R';
         } else if (!strncmp(c1,"score",5)) {
            Rec->typvar[0]='S';
         } else if (!strncmp(c1,"timeserie",9)) {
            Rec->typvar[0]='T';
         } else if (!strncmp(c1,"filter:",7)) {
            Rec->typvar[1]='F';
         } else if (!strncmp(c1,"clamped",7)) {
            Rec->typvar[1]='B';
         } else if (!strncmp(c1,"interpolation:",14)) {
            Rec->typvar[1]='I';
         } else if (!strncmp(c1,"tag:",4)) {
            strncpy(Rec->etiket,&c1[4],strlen(c1)-4);
            strblank2end(Rec->etiket,FST_ETIKET_LEN);
         }
      };

   //   switch(TYPVAR[1]) {                                                    
   //      case 'H': Meta_AddQualifier(Obj,""); break; //   Données manquantes                                         
   //      case 'M': Meta_AddCellMethod(Obj,""); break; //   Modifications multiples                                     
   //      case 'U': Meta_AddCellMethod(Obj,""); break; //   Unités converties                                          
   //      case 'Z': Meta_AddCellMethod(Obj,""); break; //   Zappé                                                      
   //      case '!@  Masque du champ dont la variable est modifiée par son 'ETIKET'
   //   }
   }

   //  NI,NJ,NK,NPACK,DATYP,DASIZ 
   //Meta_GetData(Obj,&Rec->ni,&Rec->nj,&Rec->nk,&c1,NULL,&Rec->npak,&Rec->dasiz,NULL,NULL);

   // TODO: define datyp
   Rec->npak=-Rec->npak;
   switch(c1[0]) {
      case 'b': Rec->datyp=FST_TYPE_BINARY; break;
      case 's': Rec->datyp=FST_TYPE_SIGNED; break;
      case 'u': Rec->datyp=FST_TYPE_UNSIGNED; break;
      case 'r': Rec->datyp=FST_TYPE_REAL; break;
      case 'c': Rec->datyp=FST_TYPE_COMPLEX; break;
   }

   // IP1
   Rec->ip1=-1;
   if (Meta_GetVerticalRef(Obj,0,&c1,&d1) && (obj=Meta_FindVerticalObj(c1,NULL))) {
      json_pointer_get(obj,"/rpn_kind",&objval);
      kind=json_object_get_int(objval);

      int32_t flag=0,mode=2;
      char fmt;
      float lvl;
      lvl=d1;

      f77name(convip_plus)(&Rec->ip1,&lvl,&kind,&mode,&fmt,&flag,1);
   } else {
      Lib_Log(APP_LIBMETA,APP_ERROR,"%s: Unable to find vertical reference: %s\n",__func__,c1);
   }

   if (Meta_GetHorizontalRef(Obj,&c1) && (obj=Meta_FindHorizontalObj(c1,NULL))) {
      json_pointer_get(obj,"/rpn_name",&objval);
      if ((c1=(char*)json_object_get_string(objval))) {
         strncpy(Rec->grtyp,c1,FST_GTYP_LEN);
      }
      json_pointer_get(obj,"/IG1",&objval);
      Rec->ig1=json_object_get_int(objval);
      json_pointer_get(obj,"/IG2",&objval);
      Rec->ig2=json_object_get_int(objval);
      json_pointer_get(obj,"/IG3",&objval);
      Rec->ig3=json_object_get_int(objval);
      json_pointer_get(obj,"/IG4",&objval);
      Rec->ig4=json_object_get_int(objval);
   } else {
      Lib_Log(APP_LIBMETA,APP_ERROR,"%s: Unable to find horizontal reference: %s\n",__func__,c1);
   }

   return(TRUE);
}

#endif