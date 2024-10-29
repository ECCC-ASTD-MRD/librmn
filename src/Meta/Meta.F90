module rmn_meta
    use App
    use f_c_strings_mod
    use rmn_common
    implicit none

#include "rmn/dlfcn.inc"

    integer, parameter :: META_TYPE_RECORD=1
    integer, parameter :: META_TYPE_FILE=2
    integer, parameter :: META_BUF_MAX=32768
    integer, parameter :: JSON_C_TO_STRING_PLAIN=0
    integer, parameter :: JSON_C_TO_STRING_SPACED=1
    integer, parameter :: JSON_C_TO_STRING_PRETTY=2

    interface

        !  void Meta_Init();
        SUBROUTINE meta_init() BIND(C, name="Meta_Init")
        end SUBROUTINE

        !  char *Meta_Stringify(json_object *Obj);
        type(C_PTR) FUNCTION meta_stringify(obj,format) BIND(C, name="Meta_Stringify")
            import :: C_PTR, C_INT32_T

<<<<<<< HEAD
            type(C_PTR), intent(in), value  :: obj
            integer(kind=C_INT32_T), intent(in), value  :: format
        end FUNCTION

        !  int32_t Meta_Is(json_object *Obj) {
        integer(C_INT32_T) FUNCTION meta_is(obj) BIND(C, name="Meta_Is")
            import :: C_PTR, C_INT32_T

            type(C_PTR), intent(in), value  :: obj
        end FUNCTION

        !  json_object *Meta_NewObject();
        type(C_PTR) FUNCTION meta_newobject() BIND(C, name="Meta_NewObject")
            import :: C_PTR
        end FUNCTION

        !  int Meta_Free(json_object *Obj);
        integer(C_INT32_T) FUNCTION meta_free(obj) BIND(C, name="Meta_Free")
            import :: C_PTR, C_INT32_T

            type(C_PTR), intent(in), value  :: obj
        end FUNCTION

        !  json_object* Meta_Parse(char *MetaString);
        type(C_PTR) FUNCTION meta_parse(str) BIND(C, name="Meta_Parse")
            import :: C_PTR, C_CHAR

            character(C_CHAR), dimension(*) :: str
        end FUNCTION

        !  json_object* Meta_Resolve(json_object *Obj,json_object *ObjMaster);
        type(C_PTR) FUNCTION meta_resolve(obj,objmaster) BIND(C, name="Meta_Resolve")
            import :: C_PTR

            type(C_PTR), intent(in), value :: obj,objmaster
        end FUNCTION

        !  json_object *Meta_Load(char *Path );
        type(C_PTR) FUNCTION meta_load(path) BIND(C, name="Meta_Load")
            import :: C_PTR, C_CHAR

            character(C_CHAR), dimension(*), intent(in) :: path
        end FUNCTION

        !  json_object *Meta_New(int Type,char *Version);
        type(C_PTR) FUNCTION meta_new(type,version) BIND(C, name="Meta_New")
            import :: C_PTR, C_CHAR, C_INT

            integer(C_INT),  value :: type
            character(C_CHAR), dimension(*), intent(in) :: version
        end FUNCTION

        !  json_object *Meta_DefFile(json_object *Obj,char *Institution,char* Discipline,char *Title,char *Source,char *Description,char *State);
        type(C_PTR) FUNCTION meta_deffile(obj,institution,discipline,title,source,description,state) BIND(C, name="Meta_DefVar")
            import :: C_PTR, C_CHAR

            type(C_PTR), intent(in), value :: obj
            character(C_CHAR), dimension(*), intent(in) :: institution,discipline,title,source,description,state
        end FUNCTION

        !  int Meta_WriteFile(fst_file *file,json_object *Obj) ;
        integer(C_INT32_T) FUNCTION meta_writefile(file,obj) BIND(C, name="Meta_WriteFile")
            import :: C_PTR, C_INT32_T

            type(C_PTR), intent(in), value  :: file,obj
        end FUNCTION


        !  json_object *Meta_DefVar(json_object *Obj,char *StandardName,char* RPNName,char *LongName,char *Description,const char* Unit);
        type(C_PTR) FUNCTION meta_defvar(obj,standardname,rpnname,longname,description,unit) BIND(C, name="Meta_DefVar")
            import :: C_PTR, C_CHAR

            type(C_PTR), intent(in), value :: obj
            character(C_CHAR), dimension(*), intent(in) :: standardname,rpnname,longname,description,unit
        end FUNCTION

        !  json_object *Meta_DefVarFromDict(json_object *Obj,char* RPNName);
        type(C_PTR) FUNCTION meta_defvarfromdict(obj,rpnname) BIND(C, name="Meta_DefVarFromDict")
            import :: C_PTR, C_CHAR

            type(C_PTR), intent(in), value :: obj
            character(C_CHAR), dimension(*), intent(in) :: rpnname
        end FUNCTION

        !  json_object *Meta_DefForecastTime(json_object *Obj,time_t T0,int Step,double Duration,char *Unit);
        type(C_PTR) FUNCTION meta_defforecasttime(obj,t0,step,duration,unit) BIND(C, name="Meta_DefForecastTime")
            import :: C_PTR, C_CHAR, C_LONG, C_INT, C_DOUBLE

            type(C_PTR), intent(in), value :: obj
            integer(C_LONG), value :: t0
            integer(C_INT),  value :: step
            real(C_DOUBLE),  value :: duration
            character(C_CHAR), dimension(*) :: unit
        end FUNCTION

        !  json_object *Meta_AddVerticalRef(json_object *Obj,char* Identifier,bool Copy);
        type(C_PTR) FUNCTION meta_addverticalref(obj,identifier,copy) BIND(C, name="Meta_AddVerticalRef")
            import :: C_PTR, C_CHAR, C_INT

            type(C_PTR), intent(in), value :: obj
            character(C_CHAR), dimension(*), intent(in) :: identifier
            integer(kind=C_INT),    value :: copy
        end FUNCTION

        !  json_object *Meta_AddHorizontalRef(json_object *Obj,char* Identifier,bool Copy);
        type(C_PTR) FUNCTION meta_addhorizontalref(obj,identifier,copy) BIND(C, name="Meta_AddHorizontalRef")
            import :: C_PTR, C_CHAR, C_INT

            type(C_PTR), intent(in), value :: obj
            character(C_CHAR), dimension(*), intent(in) :: identifier
            integer(kind=C_INT),    value :: copy
        end FUNCTION

        !  json_object *Meta_DefVerticalRef(json_object *Obj,char* Identifier,double* Values,int32_t Nb,bool Copy);
        type(C_PTR) FUNCTION meta_defverticalref(obj,identifier,values,nb,copy) BIND(C, name="Meta_DefVerticalRef")
            import :: C_PTR, C_CHAR, C_INT, C_INT32_T, C_DOUBLE

            type(C_PTR), intent(in), value :: obj
            character(C_CHAR), dimension(*), intent(in) :: identifier
            real(C_DOUBLE), dimension(*), intent(in) :: values
            integer(kind=C_INT32_T), value :: nb
            integer(kind=C_INT),     value :: copy
        end FUNCTION

        !  json_object *Meta_DefHorizontalRef(json_object *Obj,char* Identifier,bool Copy);
        type(C_PTR) FUNCTION meta_defhorizontalref(obj,identifier,copy) BIND(C, name="Meta_DefHorizontalRef")
            import :: C_PTR, C_CHAR, C_INT

            type(C_PTR), intent(in), value :: obj
            character(C_CHAR), dimension(*) :: identifier
            integer(kind=C_INT), value :: copy
        end FUNCTION

        !  json_object *Meta_DefData(json_object *Obj,int32_t NI,int32_t NJ,int32_t NK,char *Type,char *Compression,int32_t Pack,int32_t Bit,double Min,double Max)
        type(C_PTR) FUNCTION meta_defdata(obj,ni,nj,nk,type,compression,pack,bit,min,max) BIND(C, name="Meta_DefData")
            import :: C_PTR, C_CHAR, C_INT32_T, C_DOUBLE

            type(C_PTR), intent(in), value :: obj
            character(C_CHAR), dimension(*) :: type,compression
            integer(C_INT32_T),  value :: ni,nj,nk,pack,bit
            real(C_DOUBLE), value :: min, max
        end FUNCTION

        !  json_object *Meta_AddCellMethod(json_object *Obj,char *Method);
        type(C_PTR) FUNCTION meta_addcellmethod(obj,method) BIND(C, name="Meta_AddCellMethod")
            import :: C_PTR, C_CHAR

            type(C_PTR), intent(in), value :: obj
            character(C_CHAR), dimension(*) :: method
        end FUNCTION

        !  json_object *Meta_SetCellMethods(json_object *Obj,char **Methods);
        type(C_PTR) FUNCTION meta_setcellmethods(obj,methods) BIND(C, name="Meta_SetCellMethods")
            import :: C_PTR

            type(C_PTR), intent(in), value :: obj, methods
        end FUNCTION

        !  json_object *Meta_SetQualifiers(json_object *Obj,char **Qualifiers);
        type(C_PTR) FUNCTION meta_setqualifiers(obj,qualifiers) BIND(C, name="Meta_SetQualifiers")
            import :: C_PTR

            type(C_PTR), intent(in), value :: obj
            type(C_PTR), intent(in), dimension(*) :: qualifiers
        end FUNCTION

        !  json_object *Meta_ClearCellMethods(json_object *Obj);
        type(C_PTR) FUNCTION meta_clearcellmethods(obj) BIND(C, name="Meta_ClearCellMethods")
            import :: C_PTR

            type(C_PTR), intent(in), value :: obj
        end FUNCTION

        !  json_object *Meta_AddQualifier(json_object *Obj,char *Qualifier);
        type(C_PTR) FUNCTION meta_addqualifier(obj,qualifier) BIND(C, name="Meta_AddQualifier")
            import :: C_PTR, C_CHAR

            type(C_PTR), intent(in), value :: obj
            character(C_CHAR), dimension(*) :: qualifier
        end FUNCTION

        !  json_object *Meta_ClearQualifiers(json_object *Obj);
        type(C_PTR) FUNCTION meta_clearqualifiers(obj) BIND(C, name="Meta_ClearQualifiers")
            import :: C_PTR

            type(C_PTR), intent(in), value :: obj
        end FUNCTION

        !  json_object *Meta_AddMissingValue(json_object *Obj,char *Reason,double Value);
        type(C_PTR) FUNCTION meta_addmissingvalue(obj,reason,value) BIND(C, name="Meta_AddMissingValue")
            import :: C_PTR, C_CHAR, C_DOUBLE

            type(C_PTR), intent(in), value :: obj
            character(C_CHAR), dimension(*) :: reason
            real(C_DOUBLE), value :: value
        end FUNCTION

        !  json_object *Meta_ClearMissingValues(json_object *Obj);
        type(C_PTR) FUNCTION meta_clearmissingvalues(obj) BIND(C, name="Meta_ClearMissingValues")
            import :: C_PTR

            type(C_PTR), intent(in), value :: obj
        end FUNCTION

        !  int Meta_ArrayLength(json_object *Obj);
        integer(kind=C_INT) FUNCTION meta_arraylength(obj) BIND(C, name="Meta_ArrayLength")
            import :: C_PTR, C_INT

            type(C_PTR), intent(in), value :: obj
        end FUNCTION

        !  json_object* Meta_ArrayGetObject(json_object *Obj,int Idx);
        type(C_PTR) FUNCTION meta_arraygetobject(obj,idx) BIND(C, name="Meta_ArrayGetObject")
            import :: C_PTR, C_INT

            type(C_PTR), intent(in), value :: obj
            integer(kind=C_INT), intent(in), value :: idx
        end FUNCTION

        !  json_object* Meta_ArrayFind(json_object *Obj,char *Token);
        type(C_PTR) FUNCTION meta_arrayfind(obj,token) BIND(C, name="Meta_ArrayFind")
            import :: C_PTR, C_CHAR

            type(C_PTR), intent(in), value :: obj
            character(C_CHAR), dimension(*) :: token
        end FUNCTION

        !  json_object* Meta_GetObject(json_object *Obj,char *Path);
        type(C_PTR) FUNCTION meta_getobject(obj,path) BIND(C, name="Meta_GetObject")
            import :: C_PTR, C_CHAR

            type(C_PTR), intent(in), value :: obj
            character(C_CHAR), dimension(*) :: path
        end FUNCTION

        !  char* Meta_GetObjectString(json_object *Obj);
        type(C_PTR) FUNCTION meta_getobjectstring4fortran(obj) BIND(C, name="Meta_GetObjectString")
            import :: C_PTR

            type(C_PTR), intent(in), value :: obj
        end FUNCTION

        !  json_object* Meta_Copy(json_object *Obj);
        type(C_PTR) FUNCTION meta_copy(obj) BIND(C, name="Meta_Copy")
            import :: C_PTR

            type(C_PTR), intent(in), value :: obj
        end FUNCTION

        !  int Meta_Match(json_object *Obj1,json_object *Obj2,int RegExp);
        integer(kind=C_INT) FUNCTION meta_match(obj1,obj2,regexp) BIND(C, name="Meta_Match")
            import :: C_PTR, C_INT

            type(C_PTR), intent(in), value :: obj1, obj2
            integer(kind=C_INT), intent(in), value :: regexp
        end FUNCTION

        !TODO: fortran rec call
        !  int Meta_To89(json_object *Obj,fst_record *Rec);
        integer(kind=C_INT) FUNCTION meta_to89(obj,rec) BIND(C, name="Meta_To89")
            import :: C_PTR, C_INT

            type(C_PTR), intent(in), value :: obj, rec
        end FUNCTION

        !  int Meta_From89(json_object *Obj,fst_record *Rec);
        integer(kind=C_INT) FUNCTION meta_from89(obj,rec) BIND(C, name="Meta_From89")
            import :: C_PTR, C_INT

            type(C_PTR), intent(in), value :: obj, rec
        end FUNCTION

        !  json_object* Meta_DefFromTypVar(json_object *Obj,const char* TypVar)
        type(C_PTR) FUNCTION meta_deffromtypvar(obj,typvar) BIND(C, name="Meta_DefFromTypVar")
            import :: C_PTR, C_CHAR

            type(C_PTR), intent(in), value :: obj
            character(C_CHAR), dimension(*) :: typvar
        end FUNCTION

        !  json_object* Meta_DefFromEtiket(json_object *Obj,const char* Etiket)
        type(C_PTR) FUNCTION meta_deffrometiket(obj,etiket) BIND(C, name="Meta_DefFromEtiket")
            import :: C_PTR, C_CHAR

            type(C_PTR), intent(in), value :: obj
            character(C_CHAR), dimension(*) :: etiket
        end FUNCTION

    end interface

    type, public :: meta
        type(C_PTR) :: json_obj = c_null_ptr
    contains
        procedure, pass :: is => tmeta_is
        procedure, pass :: load => tmeta_load
        procedure, pass :: init => tmeta_init
        procedure, pass :: resolve => tmeta_resolve
        procedure, pass :: parse => tmeta_parse
        procedure, pass :: newobject => tmeta_newobject
        procedure, pass :: free => tmeta_free
        procedure, pass :: addmissingvalue => tmeta_addmissingvalue
        procedure, pass :: clearmissingvalues => tmeta_clearmissingvalues
        procedure, pass :: addqualifier => tmeta_addqualifier
        procedure, pass :: setqualifiers => tmeta_setqualifiers
        procedure, pass :: clearqualifiers => tmeta_clearqualifiers
        procedure, pass :: addcellmethod => tmeta_addcellmethod
        procedure, pass :: clearcellmethods => tmeta_clearcellmethods
        procedure, pass :: stringify => tmeta_stringify
        procedure, pass :: deffile => tmeta_deffile
        procedure, pass :: writefile => tmeta_writefile
        procedure, pass :: defvar => tmeta_defvar
        procedure, pass :: defvarfromdict => tmeta_defvarfromdict
        procedure, pass :: deffromtypvar => tmeta_deffromtypvarref
        procedure, pass :: deffrometiket => tmeta_deffrometiket
        procedure, pass :: defforecasttime => tmeta_defforecasttime
        procedure, pass :: addverticalref => tmeta_addverticalref
        procedure, pass :: addhorizontalref => tmeta_addhorizontalref
        procedure, pass :: defverticalref => tmeta_defverticalref
        procedure, pass :: defhorizontalref => tmeta_defhorizontalref
        procedure, pass :: defdata => tmeta_defdata
        procedure, pass :: getobject => tmeta_getobject
        procedure, pass :: copy => tmeta_copy
        procedure, pass :: match => tmeta_match
        final :: meta_final
    end type meta
=======
      type(C_PTR), intent(in), value  :: obj
   end FUNCTION

!  json_object *Meta_NewObject();
   type(C_PTR) FUNCTION meta_newobject() BIND(C, name="Meta_NewObject")
      import :: C_PTR
   end FUNCTION

!  int Meta_Free(json_object *Obj);
   integer(C_INT32_T) FUNCTION meta_free(obj) BIND(C, name="Meta_Free")
      import :: C_PTR, C_INT32_T

      type(C_PTR), intent(in), value  :: obj
   end FUNCTION

!  json_object* Meta_Parse(char *MetaString);
   type(C_PTR) FUNCTION meta_parse(str) BIND(C, name="Meta_Parse")
      import :: C_PTR, C_CHAR

      character(C_CHAR), dimension(*) :: str
   end FUNCTION

!  json_object* Meta_Resolve(json_object *Obj,json_object *ObjMaster);
   type(C_PTR) FUNCTION meta_resolve(obj,objmaster) BIND(C, name="Meta_Resolve")
      import :: C_PTR

      type(C_PTR), intent(in), value :: obj,objmaster
   end FUNCTION
   
!  json_object *Meta_Load(char *Path );
   type(C_PTR) FUNCTION meta_load(path) BIND(C, name="Meta_Load")
      import :: C_PTR, C_CHAR

      character(C_CHAR), dimension(*), intent(in) :: path
   end FUNCTION

!  json_object *Meta_New(int Type,char *Version);
   type(C_PTR) FUNCTION meta_new(type,version) BIND(C, name="Meta_New")
      import :: C_PTR, C_CHAR, C_INT

      integer(C_INT),  value :: type
      character(C_CHAR), dimension(*), intent(in) :: version
   end FUNCTION

!  json_object *Meta_DefFile(json_object *Obj,char *Institution,char* Discipline,char *Title,char *Source,char *Description,char *State);
   type(C_PTR) FUNCTION meta_deffile(obj,institution,discipline,title,source,description,state) BIND(C, name="Meta_DefVar")
      import :: C_PTR, C_CHAR

      type(C_PTR), intent(in), value :: obj
      character(C_CHAR), dimension(*), intent(in) :: institution,discipline,title,source,description,state        
   end FUNCTION

!  int Meta_WriteFile(fst_file *file,json_object *Obj) ;
   integer(C_INT32_T) FUNCTION meta_writefile(file,obj) BIND(C, name="Meta_WriteFile")
      import :: C_PTR, C_INT32_T

      type(C_PTR), intent(in), value  :: file,obj
   end FUNCTION


!  json_object *Meta_DefVar(json_object *Obj,char *StandardName,char* RPNName,char *LongName,char *Description,const char* Unit);
   type(C_PTR) FUNCTION meta_defvar(obj,standardname,rpnname,longname,description,unit) BIND(C, name="Meta_DefVar")
      import :: C_PTR, C_CHAR

      type(C_PTR), intent(in), value :: obj
      character(C_CHAR), dimension(*), intent(in) :: standardname,rpnname,longname,description,unit
   end FUNCTION

!  json_object *Meta_DefVarFromDict(json_object *Obj,char* RPNName);
   type(C_PTR) FUNCTION meta_defvarfromdict(obj,rpnname) BIND(C, name="Meta_DefVarFromDict")
      import :: C_PTR, C_CHAR

      type(C_PTR), intent(in), value :: obj
      character(C_CHAR), dimension(*), intent(in) :: rpnname
   end FUNCTION

!  json_object *Meta_DefForecastTime(json_object *Obj,time_t T0,int Step,double Duration,char *Unit);
   type(C_PTR) FUNCTION meta_defforecasttime(obj,t0,step,duration,unit) BIND(C, name="Meta_DefForecastTime")
      import :: C_PTR, C_CHAR, C_LONG, C_INT, C_DOUBLE

      type(C_PTR), intent(in), value :: obj
      integer(C_LONG), value :: t0
      integer(C_INT),  value :: step
      real(C_DOUBLE),  value :: duration
      character(C_CHAR), dimension(*) :: unit
   end FUNCTION

!  json_object *Meta_AddVerticalRef(json_object *Obj,char* Identifier,bool Copy);
   type(C_PTR) FUNCTION meta_addverticalref(obj,identifier,copy) BIND(C, name="Meta_AddVerticalRef")
      import :: C_PTR, C_CHAR, C_INT

      type(C_PTR), intent(in), value :: obj
      character(C_CHAR), dimension(*), intent(in) :: identifier
      integer(kind=C_INT),    value :: copy
   end FUNCTION

!  json_object *Meta_AddHorizontalRef(json_object *Obj,char* Identifier,bool Copy);
   type(C_PTR) FUNCTION meta_addhorizontalref(obj,identifier,copy) BIND(C, name="Meta_AddHorizontalRef")
      import :: C_PTR, C_CHAR, C_INT

      type(C_PTR), intent(in), value :: obj
      character(C_CHAR), dimension(*), intent(in) :: identifier
      integer(kind=C_INT),    value :: copy
   end FUNCTION

!  json_object *Meta_DefVerticalRef(json_object *Obj,char* Identifier,double* Values,int32_t Nb,bool Copy);
   type(C_PTR) FUNCTION meta_defverticalref(obj,identifier,values,nb,copy) BIND(C, name="Meta_DefVerticalRef")
      import :: C_PTR, C_CHAR, C_INT, C_INT32_T, C_DOUBLE

      type(C_PTR), intent(in), value :: obj
      character(C_CHAR), dimension(*), intent(in) :: identifier
      real(C_DOUBLE), dimension(*), intent(in) :: values
      integer(kind=C_INT32_T), value :: nb
      integer(kind=C_INT),     value :: copy
   end FUNCTION

!   json_object *Meta_CreateVerticalRef(char *Identifier,char *StandardName,char* RPNName,char *LongName,char* Description,char* Positive,char *Unit,char* FormulaTerms,int32_t Kind, int32_t VCode) {
   type(C_PTR) FUNCTION meta_createverticalref(identifier,standardname,rpnname,longname,description,positive,unit,formulaterms,kind,vcode) BIND(C, name="Meta_CreateVerticalRef")
      import :: C_PTR, C_CHAR, C_INT32_T

      character(C_CHAR), dimension(*), intent(in) :: identifier,standardname,rpnname,longname,description,positive,formulaterms
      integer(kind=C_INT32_T), value :: kind,vcode
   end FUNCTION
!   json_object *Meta_CreateHorizontalRef(char *Identifier,char *StandardName,char* RPNName,char *LongName,int32_t IG1,int32_t IG2,int32_t IG3,int32_t IG4,int32_t IG1REF,int32_t IG2REF,int32_t IG3REF,int32_t IG4REF) {
   type(C_PTR) FUNCTION meta_createhorizontalref(identifier,standardname,rpnname,longname,IG1,IG2,IG3,IG4,IG1REF,IG2REF,IG3REF,IG4REF) BIND(C, name="Meta_CreateHorizontalRef")
      import :: C_PTR, C_CHAR, C_INT32_T

      character(C_CHAR), dimension(*), intent(in) :: identifier,standardname,rpnname,longname
      integer(kind=C_INT32_T), value :: IG1,IG2,IG3,IG4,IG1REF,IG2REF,IG3REF,IG4REF
   end FUNCTION

!  json_object *Meta_DefHorizontalRef(json_object *Obj,char* Identifier,bool Copy);
   type(C_PTR) FUNCTION meta_defhorizontalref(obj,identifier,copy) BIND(C, name="Meta_DefHorizontalRef")
      import :: C_PTR, C_CHAR, C_INT

      type(C_PTR), intent(in), value :: obj
      character(C_CHAR), dimension(*) :: identifier
      integer(kind=C_INT), value :: copy
   end FUNCTION

!  json_object *Meta_DefData(json_object *Obj,int32_t NI,int32_t NJ,int32_t NK,char *Type,char *Compression,int32_t Pack,int32_t Bit,double Min,double Max)
   type(C_PTR) FUNCTION meta_defdata(obj,ni,nj,nk,type,compression,pack,bit,min,max) BIND(C, name="Meta_DefData")
      import :: C_PTR, C_CHAR, C_INT32_T, C_DOUBLE

      type(C_PTR), intent(in), value :: obj
      character(C_CHAR), dimension(*) :: type,compression
      integer(C_INT32_T),  value :: ni,nj,nk,pack,bit
      real(C_DOUBLE), value :: min, max
   end FUNCTION
       
!  json_object *Meta_AddCellMethod(json_object *Obj,char *Method);
   type(C_PTR) FUNCTION meta_addcellmethod(obj,method) BIND(C, name="Meta_AddCellMethod")
      import :: C_PTR, C_CHAR

      type(C_PTR), intent(in), value :: obj
      character(C_CHAR), dimension(*) :: method
   end FUNCTION

!  json_object *Meta_SetCellMethods(json_object *Obj,char **Methods);
   type(C_PTR) FUNCTION meta_setcellmethods(obj,methods) BIND(C, name="Meta_SetCellMethods")
      import :: C_PTR

      type(C_PTR), intent(in), value :: obj, methods
   end FUNCTION

!  json_object *Meta_SetQualifiers(json_object *Obj,char **Qualifiers);
   type(C_PTR) FUNCTION meta_setqualifiers(obj,qualifiers) BIND(C, name="Meta_SetQualifiers")
      import :: C_PTR

      type(C_PTR), intent(in), value :: obj
      type(C_PTR), intent(in), dimension(*) :: qualifiers
   end FUNCTION

!  json_object *Meta_ClearCellMethods(json_object *Obj);
   type(C_PTR) FUNCTION meta_clearcellmethods(obj) BIND(C, name="Meta_ClearCellMethods")
      import :: C_PTR

      type(C_PTR), intent(in), value :: obj
   end FUNCTION

!  json_object *Meta_AddQualifier(json_object *Obj,char *Qualifier);
   type(C_PTR) FUNCTION meta_addqualifier(obj,qualifier) BIND(C, name="Meta_AddQualifier")
      import :: C_PTR, C_CHAR

      type(C_PTR), intent(in), value :: obj
      character(C_CHAR), dimension(*) :: qualifier
   end FUNCTION

!  json_object *Meta_ClearQualifiers(json_object *Obj);
   type(C_PTR) FUNCTION meta_clearqualifiers(obj) BIND(C, name="Meta_ClearQualifiers")
      import :: C_PTR

      type(C_PTR), intent(in), value :: obj
   end FUNCTION

!  json_object *Meta_AddMissingValue(json_object *Obj,char *Reason,double Value);
   type(C_PTR) FUNCTION meta_addmissingvalue(obj,reason,value) BIND(C, name="Meta_AddMissingValue")
      import :: C_PTR, C_CHAR, C_DOUBLE

      type(C_PTR), intent(in), value :: obj
      character(C_CHAR), dimension(*) :: reason
      real(C_DOUBLE), value :: value
   end FUNCTION

!  json_object *Meta_ClearMissingValues(json_object *Obj);
   type(C_PTR) FUNCTION meta_clearmissingvalues(obj) BIND(C, name="Meta_ClearMissingValues")
      import :: C_PTR

      type(C_PTR), intent(in), value :: obj
   end FUNCTION

!  int Meta_ArrayLength(json_object *Obj);
   integer(kind=C_INT) FUNCTION meta_arraylength(obj) BIND(C, name="Meta_ArrayLength")
      import :: C_PTR, C_INT

      type(C_PTR), intent(in), value :: obj
   end FUNCTION

!  json_object* Meta_ArrayGetObject(json_object *Obj,int Idx);
   type(C_PTR) FUNCTION meta_arraygetobject(obj,idx) BIND(C, name="Meta_ArrayGetObject")
      import :: C_PTR, C_INT

      type(C_PTR), intent(in), value :: obj
      integer(kind=C_INT), intent(in), value :: idx
   end FUNCTION

!  json_object* Meta_ArrayFind(json_object *Obj,char *Token);
   type(C_PTR) FUNCTION meta_arrayfind(obj,token) BIND(C, name="Meta_ArrayFind")
      import :: C_PTR, C_CHAR

      type(C_PTR), intent(in), value :: obj
      character(C_CHAR), dimension(*) :: token
   end FUNCTION

!  json_object* Meta_GetObject(json_object *Obj,char *Path);
   type(C_PTR) FUNCTION meta_getobject(obj,path) BIND(C, name="Meta_GetObject")
      import :: C_PTR, C_CHAR

      type(C_PTR), intent(in), value :: obj
      character(C_CHAR), dimension(*) :: path
   end FUNCTION

!  char* Meta_GetObjectString(json_object *Obj);
   type(C_PTR) FUNCTION meta_getobjectstring4fortran(obj) BIND(C, name="Meta_GetObjectString")
      import :: C_PTR

      type(C_PTR), intent(in), value :: obj
   end FUNCTION

!  json_object* Meta_Copy(json_object *Obj);
   type(C_PTR) FUNCTION meta_copy(obj) BIND(C, name="Meta_Copy")
      import :: C_PTR

      type(C_PTR), intent(in), value :: obj
   end FUNCTION

!  int Meta_Match(json_object *Obj1,json_object *Obj2,int RegExp);
   integer(kind=C_INT) FUNCTION meta_match(obj1,obj2,regexp) BIND(C, name="Meta_Match")
      import :: C_PTR, C_INT

      type(C_PTR), intent(in), value :: obj1, obj2
      integer(kind=C_INT), intent(in), value :: regexp
   end FUNCTION

!TODO: fortran rec call
!  int Meta_To89(json_object *Obj,fst_record *Rec);
   integer(kind=C_INT) FUNCTION meta_to89(obj,rec) BIND(C, name="Meta_To89")
      import :: C_PTR, C_INT

      type(C_PTR), intent(in), value :: obj, rec
   end FUNCTION

!  int Meta_From89(json_object *Obj,fst_record *Rec);
   integer(kind=C_INT) FUNCTION meta_from89(obj,rec) BIND(C, name="Meta_From89")
      import :: C_PTR, C_INT

      type(C_PTR), intent(in), value :: obj, rec
   end FUNCTION

!  json_object* Meta_DefFromTypVar(json_object *Obj,const char* TypVar)
   type(C_PTR) FUNCTION meta_deffromtypvar(obj,typvar) BIND(C, name="Meta_DefFromTypVar")
      import :: C_PTR, C_CHAR

      type(C_PTR), intent(in), value :: obj
      character(C_CHAR), dimension(*) :: typvar
   end FUNCTION

!  json_object* Meta_DefFromEtiket(json_object *Obj,const char* Etiket)
   type(C_PTR) FUNCTION meta_deffrometiket(obj,etiket) BIND(C, name="Meta_DefFromEtiket")
      import :: C_PTR, C_CHAR

      type(C_PTR), intent(in), value :: obj
      character(C_CHAR), dimension(*) :: etiket
   end FUNCTION

end interface

   type, public :: meta
      type(C_PTR) :: json_obj = c_null_ptr
   contains
      procedure, pass :: is => tmeta_is
      procedure, pass :: load => tmeta_load
      procedure, pass :: init => tmeta_init
      procedure, pass :: resolve => tmeta_resolve
      procedure, pass :: parse => tmeta_parse
      procedure, pass :: newobject => tmeta_newobject
      procedure, pass :: free => tmeta_free
      procedure, pass :: addmissingvalue => tmeta_addmissingvalue
      procedure, pass :: clearmissingvalues => tmeta_clearmissingvalues
      procedure, pass :: addqualifier => tmeta_addqualifier
      procedure, pass :: setqualifiers => tmeta_setqualifiers
      procedure, pass :: clearqualifiers => tmeta_clearqualifiers
      procedure, pass :: addcellmethod => tmeta_addcellmethod
      procedure, pass :: clearcellmethods => tmeta_clearcellmethods
      procedure, pass :: stringify => tmeta_stringify
      procedure, pass :: deffile => tmeta_deffile
      procedure, pass :: writefile => tmeta_writefile
      procedure, pass :: defvar => tmeta_defvar
      procedure, pass :: defvarfromdict => tmeta_defvarfromdict
      procedure, pass :: deffromtypvar => tmeta_deffromtypvarref
      procedure, pass :: deffrometiket => tmeta_deffrometiket
      procedure, pass :: defforecasttime => tmeta_defforecasttime
      procedure, pass :: addverticalref => tmeta_addverticalref
      procedure, pass :: addhorizontalref => tmeta_addhorizontalref
      procedure, pass :: defverticalref => tmeta_defverticalref
      procedure, pass :: defhorizontalref => tmeta_defhorizontalref
      procedure, pass :: defdata => tmeta_defdata
      procedure, pass :: getobject => tmeta_getobject
      procedure, pass :: copy => tmeta_copy
      procedure, pass :: match => tmeta_match
      final :: meta_final
   end type meta
>>>>>>> be65ac5 ([META] Added horizontal and vertical reference creation functions)

contains

    SUBROUTINE meta_final(this)
        type(meta), intent(inout) :: this
        !TODO: Freed when should not      status = meta_free(this%json_obj)
    end SUBROUTINE meta_final

    FUNCTION tmeta_is(this) result(status)
        class(meta), intent(inout) :: this
        logical                    :: status

        integer(C_INT32_T)         :: c_valid

        c_valid = meta_is(this%json_obj)

        status=.false.
        if (c_valid>0) then
            status=.true.
        endif
    end FUNCTION

    FUNCTION tmeta_load(this,path) result(status)
        class(meta), intent(inout)   :: this
        integer(kind=INT32)          :: status
        character(len=*), intent(in) :: path
    !
        this%json_obj = meta_load(path//C_NULL_CHAR)

        status=0
        if (c_associated(this%json_obj)) then
            status=1
        endif
    end FUNCTION

    FUNCTION tmeta_init(this,type,version) result(status)
        class(meta), intent(inout)   :: this
        integer(kind=INT32)          :: status
        integer(kind=INT32)          :: type
        character(len=*), intent(in) :: version
    !
        this%json_obj = meta_new(type,trim(version)//C_NULL_CHAR)

        status=0
        if (c_associated(this%json_obj)) then
            status=1
        endif
    end FUNCTION

    FUNCTION tmeta_parse(this,str) result(status)
        class(meta), intent(inout) :: this
        integer(kind=INT32) :: status
        character(len=*) :: str

        this%json_obj = meta_parse(str//C_NULL_CHAR)
        status=0
        if (c_associated(this%json_obj)) then
            status=1
        endif
    end FUNCTION tmeta_parse

    FUNCTION tmeta_resolve(this,objmaster) result(status)
        class(meta), intent(inout) :: this, objmaster
        integer(kind=INT32) :: status
        type(C_PTR) :: objr

        objr = meta_resolve(this%json_obj,objmaster%json_obj)
        status=1
        if (.not. c_associated(objr)) then
            status=0
        endif
    end FUNCTION tmeta_resolve

    FUNCTION tmeta_newobject(this) result(status)
        class(meta), intent(inout)   :: this
        type(C_PTR) :: status
    !
        this%json_obj = meta_newobject()
        status = this%json_obj
    end FUNCTION

    FUNCTION tmeta_free(this) result(status)
        class(meta), intent(inout) :: this
        integer(kind=INT32) :: status

        status = meta_free(this%json_obj)
    end FUNCTION tmeta_free

    FUNCTION tmeta_copy(this) result(new)
        class(meta), intent(inout) :: this
        type(meta) :: new

        new%json_obj=meta_copy(this%json_obj)
    end FUNCTION

    FUNCTION tmeta_addmissingvalue(this,reason,value) result(status)
        class(meta), intent(inout) :: this
        type(C_PTR) :: status
        character(len=*) :: reason
        real(kind=REAL64), value :: value

        status = meta_addmissingvalue(this%json_obj,trim(reason)//C_NULL_CHAR,value)
    end FUNCTION tmeta_addmissingvalue

    FUNCTION tmeta_clearmissingvalues(this) result(status)
        class(meta), intent(inout) :: this
        type(C_PTR) :: status

        status = meta_clearmissingvalues(this%json_obj)
    end FUNCTION tmeta_clearmissingvalues

    FUNCTION tmeta_addcellmethod(this,method) result(status)
        class(meta), intent(inout) :: this
        type(C_PTR) :: status
        character(len=*) :: method

        status = meta_addcellmethod(this%json_obj,trim(method)//C_NULL_CHAR)
    end FUNCTION tmeta_addcellmethod

    FUNCTION tmeta_clearcellmethods(this) result(status)
        class(meta), intent(inout) :: this
        type(C_PTR) :: status

        status = meta_clearcellmethods(this%json_obj)
    end FUNCTION tmeta_clearcellmethods

    FUNCTION tmeta_addqualifier(this,qualifier) result(status)
        class(meta), intent(inout) :: this
        type(C_PTR) :: status
        character(len=*) :: qualifier

        status = meta_addqualifier(this%json_obj,trim(qualifier)//C_NULL_CHAR)
    end FUNCTION tmeta_addqualifier

    FUNCTION tmeta_setqualifiers(this,qualifiers) result(status)
        class(meta), intent(inout) :: this
        type(C_PTR) :: status
        type(C_PTR), dimension(:), allocatable :: qptr
        character(len=*), dimension(:) :: qualifiers
        character(len=64,kind=C_CHAR), allocatable, target :: farray(:)
        integer :: q

        allocate(qptr(SIZE(qualifiers)))
        allocate(farray(SIZE(qualifiers)+1))
        do q=1,SIZE(qualifiers)
            farray(q)=qualifiers(q)//C_NULL_CHAR
            qptr(q)=C_LOC(farray(q))
        end do
        qptr(q)=C_NULL_PTR

        deallocate(qptr)
        deallocate(farray)
        status = meta_setqualifiers(this%json_obj,qptr)
    end FUNCTION tmeta_setqualifiers

    FUNCTION tmeta_clearqualifiers(this) result(status)
        class(meta), intent(inout) :: this
        type(C_PTR) :: status

        status = meta_clearqualifiers(this%json_obj)
    end FUNCTION tmeta_clearqualifiers

    FUNCTION tmeta_stringify(this,format) result(fstring)
        class(meta), intent(in) :: this
        integer(kind=C_INT32_T), intent(in), optional :: format
        type(C_PTR) :: cstring
        character(len=:), pointer :: fstring
        integer(kind=C_INT32_T) :: f

        f=JSON_C_TO_STRING_PRETTY
        if (present(format)) then
            f=format
        endif

        cstring = meta_stringify(this%json_obj,f)
        call c_f_strpointer(cstring,fstring,META_BUF_MAX)
    end FUNCTION tmeta_stringify

    FUNCTION tmeta_deffile(this,institution,discipline,title,source,description,state) result(status)
        class(meta), intent(inout) :: this
        type(C_PTR) :: status
        character(len=*) :: institution,discipline,title,source,description,state

        status = meta_deffile(this%json_obj,trim(institution)//C_NULL_CHAR,trim(discipline)//C_NULL_CHAR,trim(title)//C_NULL_CHAR,trim(source)//C_NULL_CHAR,trim(description)//C_NULL_CHAR,trim(state)//C_NULL_CHAR)
    end FUNCTION

    FUNCTION tmeta_writefile(this,file) result(status)
        class(meta), intent(inout) :: this
        integer(kind=INT32) :: status
        type(C_PTR), intent(in), value :: file

        status = meta_writefile(this%json_obj,file)
    end FUNCTION

    FUNCTION tmeta_defvar(this,standardname,rpnname,longname,description,unit) result(status)
        class(meta), intent(inout) :: this
        type(C_PTR) :: status
        character(len=*) :: standardname, rpnname, longname, description, unit

        status = meta_defvar(this%json_obj,trim(standardname)//C_NULL_CHAR,trim(rpnname)//C_NULL_CHAR,trim(longname)//C_NULL_CHAR,trim(description)//C_NULL_CHAR,trim(unit)//C_NULL_CHAR)
    end FUNCTION

    FUNCTION tmeta_defvarfromdict(this,rpnname) result(status)
        class(meta), intent(inout) :: this
        type(C_PTR) :: status
        character(len=*) :: rpnname

        status = meta_defvarfromdict(this%json_obj,trim(rpnname)//C_NULL_CHAR)
    end FUNCTION

    FUNCTION tmeta_defforecasttime(this,t0,step,duration,unit) result(status)
        class(meta), intent(inout) :: this
        type(C_PTR) :: status
        integer(kind=INT64), value, optional :: t0
        integer(kind=INT32),  value, optional :: step
        real(kind=REAL64),  value, optional :: duration
        character(len=*), optional :: unit

        integer(kind=INT64) :: tt0
        integer(kind=INT32) :: tstep
        real(kind=REAL64)   :: tduration

        tt0=-1
        if (present(t0)) then
            tt0=t0
        endif
        tstep=-1
        if (present(step)) then
            tstep=step
        endif
        tduration=-1
        if (present(duration)) then
            tduration=duration
        endif
    !
        status = meta_defforecasttime(this%json_obj,tt0,tstep,tduration,trim(unit)//C_NULL_CHAR)
    end FUNCTION

    FUNCTION tmeta_addverticalref(this,identifier,copy) result(status)
        class(meta), intent(inout) :: this
        type(C_PTR) :: status
        character(len=*) :: identifier
        integer(kind=C_INT) :: icopy = 0
        logical :: copy

        if (copy) then
            icopy=1
        endif
        status = meta_addverticalref(this%json_obj,trim(identifier)//C_NULL_CHAR,icopy)
    end FUNCTION

    FUNCTION tmeta_addhorizontalref(this,identifier,copy) result(status)
        class(meta), intent(inout) :: this
        type(C_PTR) :: status
        character(len=*) :: identifier
        integer(kind=C_INT) :: icopy = 0
        logical :: copy

        if (copy) then
            icopy=1
        endif
        status = meta_addhorizontalref(this%json_obj,trim(identifier)//C_NULL_CHAR,icopy)
    end FUNCTION

    FUNCTION tmeta_defverticalref(this,identifier,values,nb,copy) result(status)
        class(meta), intent(inout) :: this
        type(C_PTR) :: status
        character(len=*) :: identifier
        real(kind=REAL64), dimension(:) :: values
        integer(kind=INT32), value :: nb
        integer(kind=C_INT) :: icopy = 0
        logical :: copy

        if (copy) then
            icopy=1
        endif
        status = meta_defverticalref(this%json_obj,trim(identifier)//C_NULL_CHAR,values,nb,icopy)
    end FUNCTION

    FUNCTION tmeta_defhorizontalref(this,identifier,copy) result(status)
        class(meta), intent(inout) :: this
        type(C_PTR) :: status
        character(len=*) :: identifier
        integer(kind=C_INT) :: icopy = 0
        logical :: copy    !

        if (copy) then
            icopy=1
        endif
        status = meta_defhorizontalref(this%json_obj,trim(identifier)//C_NULL_CHAR,icopy)
    end FUNCTION

    FUNCTION tmeta_deffromtypvarref(this,typvar) result(status)
        class(meta), intent(inout) :: this
        type(C_PTR) :: status
        character(len=*) :: typvar

        status = meta_deffromtypvar(this%json_obj,trim(typvar)//C_NULL_CHAR)
    end FUNCTION

    FUNCTION tmeta_deffrometiket(this,etiket) result(status)
        class(meta), intent(inout) :: this
        type(C_PTR) :: status
        character(len=*) :: etiket

        status = meta_deffrometiket(this%json_obj,trim(etiket)//C_NULL_CHAR)
    end FUNCTION

    FUNCTION tmeta_defdata(this,ni,nj,nk,type,compression,pack,bit,min,max) result(status)
        class(meta), intent(inout) :: this
        type(C_PTR) :: status
        character(len=*) :: type,compression
        integer(kind=INT32),  value :: ni,nj,nk,pack,bit
        real(kind=REAL64), value :: min,max

        status = meta_defdata(this%json_obj,ni,nj,nk,trim(type)//C_NULL_CHAR,trim(compression)//C_NULL_CHAR,pack,bit,min,max)
    end FUNCTION

    FUNCTION tmeta_match(this,match,regexp) result(status)
        class(meta), intent(inout) :: this
        type(meta), intent(inout) :: match
        integer(kind=INT32) :: status
        integer,  value :: regexp

        status=meta_match(match%json_obj,this%json_obj,regexp)
    end FUNCTION

    FUNCTION tmeta_getobject(this,path) result(obj)
        class(meta), intent(inout) :: this
        type(C_PTR) :: obj
        character(len=*) :: path

        obj=meta_getobject(this%json_obj,path)
    end FUNCTION

    FUNCTION meta_getobjectstring(obj) result(fstring)
        type(C_PTR), intent(in), value :: obj
        character(kind=C_CHAR), dimension(:), allocatable :: fstring

        fstring = C_F_STRING_CONVERT(meta_getobjectstring4fortran(obj))
    end FUNCTION

end module