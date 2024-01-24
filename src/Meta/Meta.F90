module rmn_meta
   use App
   use rmn_common
   implicit none

#include "rmn/dlfcn.inc"

interface

!  void Meta_Init();
   SUBROUTINE meta_init() BIND(C, name="Meta_Init")
   end SUBROUTINE
    
!  char *Meta_Stringify(json_object *Obj);
   type(C_PTR) FUNCTION meta_stringify(obj) BIND(C, name="Meta_Stringify")
      import :: C_PTR

      type(C_PTR), intent(in) :: obj
   end FUNCTION

!  int Meta_Free(json_object *Obj);
   integer(C_INT32_T) FUNCTION meta_free(obj) BIND(C, name="Meta_Free")
      import :: C_PTR, C_INT32_T

      type(C_PTR), intent(in) :: obj
   end FUNCTION

!  json_object* Meta_Parse(char *MetaString);
   type(C_PTR) FUNCTION meta_parse(str) BIND(C, name="Meta_Parse")
      import :: C_PTR, C_CHAR

      character(C_CHAR), dimension(*) :: str
   end FUNCTION

!  int          Meta_ArrayLength(json_object *Obj);
!  json_object* Meta_ArrayGetObject(json_object *Obj,int Idx);
!  json_object* Meta_ArrayFind(json_object *Obj,char *Token);
!  json_object* Meta_GetObject(json_object *Obj,char *Path);
!  char*        Meta_GetObjectString(json_object *Obj);
!  json_object* Meta_Copy(json_object *Obj);
!  int          Meta_Equivalent(json_object *Obj1,json_object *Obj2);
!  int          Meta_To89(json_object *Obj,fst_record *Rec);
!  int          Meta_From89(json_object *Obj,fst_record *Rec);
!  json_object *Meta_AddVerticalRef(json_object *Obj,char* Identifier,bool Copy);
!  json_object *Meta_AddHorizontalRef(json_object *Obj,char* Identifier,bool Copy);

!  json_object* Meta_Resolve(json_object *Obj);
   type(C_PTR) FUNCTION meta_resolve(obj) BIND(C, name="Meta_Resolve")
      import :: C_PTR

      type(C_PTR), intent(in) :: obj
   end FUNCTION
   
!  json_object *Meta_LoadProfile(char *Name,char *Version);
   type(C_PTR) FUNCTION meta_loadprofile(name,version) BIND(C, name="Meta_LoadProfile")
      import :: C_PTR, C_CHAR

      character(C_CHAR), dimension(*) :: name
      character(C_CHAR), dimension(*) :: version
   end FUNCTION

!  json_object *Meta_DefVar(json_object *Obj,char *StandardName,char* RPNName,char *LongName,char *Description);
   type(C_PTR) FUNCTION meta_defvar(obj,standardname,rpnname,longname,description) BIND(C, name="Meta_DefVar")
      import :: C_PTR, C_CHAR

      type(C_PTR) :: obj
      character(C_CHAR), dimension(*) :: standardname
      character(C_CHAR), dimension(*) :: rpnname
      character(C_CHAR), dimension(*) :: longname
      character(C_CHAR), dimension(*) :: description
   end FUNCTION

!  json_object *Meta_DefBound(json_object *Obj,double Min,double Max,const char* Unit);
   type(C_PTR) FUNCTION meta_defbound4fortran(obj,min,max,unit) BIND(C, name="Meta_DefBound")
      import :: C_PTR, C_CHAR, C_DOUBLE

      type(C_PTR) :: obj
      real(C_DOUBLE), value :: min
      real(C_DOUBLE), value :: max
      character(C_CHAR), dimension(*) :: unit
   end FUNCTION

!  json_object *Meta_DefForecastTime(json_object *Obj,time_t T0,int Step,double Duration,char *Unit);
   type(C_PTR) FUNCTION meta_defforecasttime4fortran(obj,t0,step,duration,unit) BIND(C, name="Meta_DefForecastTime")
      import :: C_PTR, C_CHAR, C_LONG, C_INT, C_DOUBLE

      type(C_PTR) :: obj
      integer(C_LONG), value :: t0
      integer(C_INT),  value :: step
      real(C_DOUBLE),  value :: duration
      character(C_CHAR), dimension(*) :: unit
   end FUNCTION

!  json_object *Meta_DefVerticalRef(json_object *Obj,char* Identifier,double Value,bool Copy);
   type(C_PTR) FUNCTION meta_defverticalref(obj,identifier,value,copy) BIND(C, name="Meta_DefVerticalRef")
      import :: C_PTR, C_CHAR, C_BOOL, C_DOUBLE

      type(C_PTR) :: obj
      character(C_CHAR), dimension(*) :: identifier
      real(C_DOUBLE),       value :: value
      logical(kind=C_BOOL), value :: copy
   end FUNCTION

!  json_object *Meta_DefHorizontalRef(json_object *Obj,char* Identifier,bool Copy);
   type(C_PTR) FUNCTION meta_defhorizontalref(obj,identifier,copy) BIND(C, name="Meta_DefHorizontalRef")
      import :: C_PTR, C_CHAR, C_BOOL

      type(C_PTR) :: obj
      character(C_CHAR), dimension(*) :: identifier
      logical(kind=C_BOOL), value :: copy
   end FUNCTION

!  json_object *Meta_DefData(json_object *Obj,char *Type,char *Compression,int32_t Pack,int32_t Size) {
   type(C_PTR) FUNCTION meta_defdata(obj,type,compression,pack,size) BIND(C, name="Meta_DefData")
      import :: C_PTR, C_CHAR, C_INT32_T

      type(C_PTR) :: obj
      character(C_CHAR), dimension(*) :: type,compression
      integer(C_INT32_T),  value :: pack,size
   end FUNCTION
       
!  json_object *Meta_AddCellMethod(json_object *Obj,char *Method);
   type(C_PTR) FUNCTION meta_addcellmethod(obj,method) BIND(C, name="Meta_AddCellMethod")
      import :: C_PTR,C_CHAR

      type(C_PTR), intent(in) :: obj
      character(C_CHAR), dimension(*) :: method
   end FUNCTION

!  json_object *Meta_ClearCellMethods(json_object *Obj);
   type(C_PTR) FUNCTION meta_clearcellmethods(obj) BIND(C, name="Meta_ClearCellMethods")
      import :: C_PTR

      type(C_PTR), intent(in) :: obj
   end FUNCTION

!  json_object *Meta_AddQualifier(json_object *Obj,char *Qualifier);
   type(C_PTR) FUNCTION meta_addqualifier(obj,qualifier) BIND(C, name="Meta_AddQualifier")
      import :: C_PTR, C_CHAR

      type(C_PTR), intent(in) :: obj
      character(C_CHAR), dimension(*) :: qualifier
   end FUNCTION

!  json_object *Meta_ClearQualifiers(json_object *Obj);
   type(C_PTR) FUNCTION meta_clearqualifiers(obj) BIND(C, name="Meta_ClearQualifiers")
      import :: C_PTR

      type(C_PTR), intent(in) :: obj
   end FUNCTION

!  json_object *Meta_AddMissingValue(json_object *Obj,char *Reason,double Value);
   type(C_PTR) FUNCTION meta_addmissingvalue(obj,reason,value) BIND(C, name="Meta_AddMissingValue")
      import :: C_PTR, C_CHAR, C_DOUBLE

      type(C_PTR), intent(in) :: obj
      character(C_CHAR), dimension(*) :: reason
      real(C_DOUBLE), value :: value
   end FUNCTION

!  json_object *Meta_ClearMissingValues(json_object *Obj);
   type(C_PTR) FUNCTION meta_clearmissingvalues(obj) BIND(C, name="Meta_ClearMissingValues")
      import :: C_PTR

      type(C_PTR), intent(in) :: obj
   end FUNCTION

end interface

   type :: meta
      private
      type(C_PTR) :: json_obj = c_null_ptr ! Pointer to C json_obj
   contains
      procedure, pass :: loadprofile => tmeta_loadprofile
      procedure, pass :: resolve => tmeta_resolve
      procedure, pass :: parse => tmeta_parse
      procedure, pass :: free => tmeta_free
      procedure, pass :: addmissingvalue => tmeta_addmissingvalue
      procedure, pass :: clearmissingvalues => tmeta_clearmissingvalues
      procedure, pass :: addqualifier => tmeta_addqualifier
      procedure, pass :: clearqualifiers => tmeta_clearqualifiers
      procedure, pass :: addcellmethod => tmeta_addcellmethod
      procedure, pass :: clearcellmethods => tmeta_clearcellmethods
      procedure, pass :: stringify => tmeta_stringify
      procedure, pass :: defvar => tmeta_defvar
      procedure, pass :: defbound => tmeta_defbound
      procedure, pass :: defforecasttime => tmeta_defforecasttime
      procedure, pass :: defverticalref => tmeta_defverticalref
      procedure, pass :: defhorizontalref => tmeta_defhorizontalref
      procedure, pass :: defdata => tmeta_defdata
 !      final :: fstd23_final
   end type meta

contains

   function tmeta_loadprofile(this,name,version) result(status)
      class(meta), intent(inout) :: this
      integer(kind=INT32) :: status
      character(len=*) :: name
      character(len=*) :: version
 
      this%json_obj = meta_loadprofile(name//C_NULL_CHAR,version//C_NULL_CHAR)

      status=1
      if (.not. c_associated(this%json_obj)) then
         status=0
      endif
   end function

   function tmeta_parse(this,str) result(status)
      class(meta), intent(inout) :: this
      integer(kind=INT32) :: status
      character(len=*) :: str

      this%json_obj = meta_parse(str//C_NULL_CHAR)
      status=1
      if (.not. c_associated(this%json_obj)) then
         status=0
      endif
   end function tmeta_parse

   function tmeta_resolve(this,obj) result(status)
      class(meta), intent(inout) :: this
      integer(kind=INT32) :: status
      type(C_PTR), intent(in) :: obj
      type(C_PTR) :: objr

      objr = meta_resolve(this%json_obj)
      status=1
      if (.not. c_associated(objr)) then
         status=0
      endif
   end function tmeta_resolve

   function tmeta_free(this) result(status)
      class(meta), intent(inout) :: this
      integer(kind=INT32) :: status

      status = meta_free(this%json_obj)
   end function tmeta_free

   function tmeta_addmissingvalue(this,reason,value) result(status)
      class(meta), intent(inout) :: this
      type(C_PTR) :: status
      character(len=*) :: reason
      real(kind=REAL64), value :: value

      status = meta_addmissingvalue(this%json_obj,reason//C_NULL_CHAR,value)
   end function tmeta_addmissingvalue

   function tmeta_clearmissingvalues(this) result(status)
      class(meta), intent(inout) :: this
      type(C_PTR) :: status

      status = meta_clearmissingvalues(this%json_obj)
   end function tmeta_clearmissingvalues

   function tmeta_addcellmethod(this,method) result(status)
      class(meta), intent(inout) :: this
      type(C_PTR) :: status
      character(len=*) :: method

      status = meta_addcellmethod(this%json_obj,method//C_NULL_CHAR)
   end function tmeta_addcellmethod

   function tmeta_clearcellmethods(this) result(status)
      class(meta), intent(inout) :: this
      type(C_PTR) :: status

      status = meta_clearcellmethods(this%json_obj)
   end function tmeta_clearcellmethods

   function tmeta_addqualifier(this,qualifier) result(status)
      class(meta), intent(inout) :: this
      type(C_PTR) :: status
      character(len=*) :: qualifier

      status = meta_addqualifier(this%json_obj,qualifier//C_NULL_CHAR)
   end function tmeta_addqualifier

   function tmeta_clearqualifiers(this) result(status)
      class(meta), intent(inout) :: this
      type(C_PTR) :: status

      status = meta_clearqualifiers(this%json_obj)
   end function tmeta_clearqualifiers

   function tmeta_stringify(this) result(fstring)
      class(meta), intent(inout) :: this
      character(kind=C_CHAR), dimension(:), pointer :: fstring
!      cstring = meta_stringify(this%json_obj)
      fstring = C_F_STRING_CONVERT(meta_stringify(this%json_obj))
   end function tmeta_stringify

   function tmeta_defvar(this,standardname,rpnname,longname,description) result(status)
      class(meta), intent(inout) :: this
      type(C_PTR) :: status
      character(len=*) :: standardname, rpnname, longname, description

      status = meta_defvar(this%json_obj,standardname//C_NULL_CHAR,rpnname//C_NULL_CHAR,longname//C_NULL_CHAR,description//C_NULL_CHAR)
   end function

   function tmeta_defbound(this,min,max,unit) result(status)
      class(meta), intent(inout) :: this
      type(C_PTR) :: status
      real(kind=REAL64), value :: min, max
      character(len=*) :: unit

      status = meta_defbound4fortran(this%json_obj,min,max,unit//C_NULL_CHAR)
   end function

   function tmeta_defforecasttime(this,t0,step,duration,unit) result(status)
      class(meta), intent(inout) :: this
      type(C_PTR) :: status
      integer(kind=INT64), value :: t0
      integer(kind=INT32),  value :: step
      real(kind=REAL64),  value :: duration
      character(len=*) :: unit

      status = meta_defforecasttime4fortran(this%json_obj,t0,step,duration,unit//C_NULL_CHAR)
   end function

   function tmeta_defverticalref(this,identifier,value,copy) result(status)
      class(meta), intent(inout) :: this
      type(C_PTR) :: status
      character(len=*) :: identifier
      real(kind=REAL64), value :: value
      logical :: copy

      status = meta_defverticalref(this%json_obj,identifier//C_NULL_CHAR,value,copy)
   end function

   function tmeta_defhorizontalref(this,identifier,copy) result(status)
      class(meta), intent(inout) :: this
      type(C_PTR) :: status
      character(len=*) :: identifier
!TODO: convert to int
      logical :: copy 

      status = meta_defhorizontalref(this%json_obj,identifier//C_NULL_CHAR,copy)
   end function

   function tmeta_defdata(this,type,compression,pack,size) result(status)
      class(meta), intent(inout) :: this
      type(C_PTR) :: status
      character(len=*) :: type,compression
      integer(kind=INT32),  value :: pack,size

      status = meta_defdata(this%json_obj,type//C_NULL_CHAR,compression//C_NULL_CHAR,pack,size)
   end function
end module