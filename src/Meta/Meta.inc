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

    !  json_object *Meta_CreateVerticalRef(char *Identifier,char *StandardName,char* RPNName,char *LongName,char* Description,char* Positive,char *Unit,char* FormulaTerms,int32_t Kind, int32_t VCode);
    type(C_PTR) FUNCTION meta_createverticalref(identifier,standardname,rpnname,longname,description,positive,unit,formulaterms,vcode) BIND(C, name="Meta_CreateVerticalRef")
        import :: C_PTR, C_CHAR, C_INT

        character(C_CHAR), dimension(*), intent(in) :: identifier,standardname,rpnname,longname,description,positive,unit,formulaterms
        integer(kind=C_INT),    value :: vcode
    end FUNCTION

    !  json_object *Meta_AddVerticalRef(json_object *Obj,char* Identifier,bool Copy);
    type(C_PTR) FUNCTION meta_addverticalref(obj,identifier,copy) BIND(C, name="Meta_AddVerticalRef")
        import :: C_PTR, C_CHAR, C_INT

        type(C_PTR), intent(in), value :: obj
        character(C_CHAR), dimension(*), intent(in) :: identifier
        integer(kind=C_INT),    value :: copy
    end FUNCTION

    ! json_object *Meta_CreateHorizontalRef(char *Identifier,char *StandardName,char* RPNName,char *LongName,int32_t IG1,int32_t IG2,int32_t IG3,int32_t IG4,int32_t IG1REF,int32_t IG2REF,int32_t IG3REF,int32_t IG4REF);
    type(C_PTR) FUNCTION meta_createhorizontalref(identifier,standardname,rpnname,longname,ig1,ig2,ig3,ig4,ig1ref,ig2ref,ig3ref,ig4ref) BIND(C, name="Meta_CreateHorizontalRef")
        import :: C_PTR, C_CHAR, C_INT

        character(C_CHAR), dimension(*), intent(in) :: identifier, standardname, rpnname, longname
        integer(kind=C_INT),    value :: ig1,ig2,ig3,ig4,ig1ref,ig2ref,ig3ref,ig4ref
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