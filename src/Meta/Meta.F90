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

      type(C_PTR), intent(in), value  :: obj
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

!  json_object* Meta_Resolve(json_object *Obj);
   type(C_PTR) FUNCTION meta_resolve(obj) BIND(C, name="Meta_Resolve")
      import :: C_PTR

      type(C_PTR), intent(in), value :: obj
   end FUNCTION
   
!  json_object *Meta_LoadProfile(char *Name,char *Version);
   type(C_PTR) FUNCTION meta_loadprofile(name,version) BIND(C, name="Meta_LoadProfile")
      import :: C_PTR, C_CHAR

      character(C_CHAR), dimension(*), intent(in) :: name
      character(C_CHAR), dimension(*), intent(in) :: version
   end FUNCTION

!  json_object *Meta_DefVar(json_object *Obj,char *StandardName,char* RPNName,char *LongName,char *Description);
   type(C_PTR) FUNCTION meta_defvar(obj,standardname,rpnname,longname,description) BIND(C, name="Meta_DefVar")
      import :: C_PTR, C_CHAR

      type(C_PTR), intent(in), value :: obj
      character(C_CHAR), dimension(*), intent(in) :: standardname
      character(C_CHAR), dimension(*), intent(in):: rpnname
      character(C_CHAR), dimension(*), intent(in) :: longname
      character(C_CHAR), dimension(*), intent(in) :: description
   end FUNCTION

!  json_object *Meta_DefBound(json_object *Obj,double Min,double Max,const char* Unit);
   type(C_PTR) FUNCTION meta_defbound(obj,min,max,unit) BIND(C, name="Meta_DefBound")
      import :: C_PTR, C_CHAR, C_DOUBLE

      type(C_PTR), intent(in), value :: obj
      real(C_DOUBLE), value :: min
      real(C_DOUBLE), value :: max
      character(C_CHAR), dimension(*), intent(in) :: unit
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
      import :: C_PTR, C_CHAR, C_BOOL

      type(C_PTR), intent(in), value :: obj
      character(C_CHAR), dimension(*), intent(in) :: identifier
      logical(kind=C_BOOL),    value :: copy
   end FUNCTION

!  json_object *Meta_AddHorizontalRef(json_object *Obj,char* Identifier,bool Copy);
   type(C_PTR) FUNCTION meta_addhorizontalref(obj,identifier,copy) BIND(C, name="Meta_AddHorizontalRef")
      import :: C_PTR, C_CHAR, C_BOOL

      type(C_PTR), intent(in), value :: obj
      character(C_CHAR), dimension(*), intent(in) :: identifier
      logical(kind=C_BOOL),    value :: copy
   end FUNCTION

!  json_object *Meta_DefVerticalRef(json_object *Obj,char* Identifier,double* Values,int32_t Nb,bool Copy);
   type(C_PTR) FUNCTION meta_defverticalref(obj,identifier,values,nb,copy) BIND(C, name="Meta_DefVerticalRef")
      import :: C_PTR, C_CHAR, C_BOOL, C_INT32_T, C_DOUBLE

      type(C_PTR), intent(in), value :: obj
      character(C_CHAR), dimension(*), intent(in) :: identifier
      real(C_DOUBLE), dimension(*), intent(in) :: values
      integer(kind=C_INT32_T), value :: nb
      logical(kind=C_BOOL),    value :: copy
   end FUNCTION

!  json_object *Meta_DefHorizontalRef(json_object *Obj,char* Identifier,bool Copy);
   type(C_PTR) FUNCTION meta_defhorizontalref(obj,identifier,copy) BIND(C, name="Meta_DefHorizontalRef")
      import :: C_PTR, C_CHAR, C_BOOL

      type(C_PTR), intent(in), value :: obj
      character(C_CHAR), dimension(*) :: identifier
      logical(kind=C_BOOL), value :: copy
   end FUNCTION

!  json_object *Meta_DefData(json_object *Obj,int32_t NI,int32_t NJ,int32_t NK,char *Type,char *Compression,int32_t Pack,int32_t Size) {
   type(C_PTR) FUNCTION meta_defdata(obj,ni,nj,nk,type,compression,pack,size) BIND(C, name="Meta_DefData")
      import :: C_PTR, C_CHAR, C_INT32_T

      type(C_PTR), intent(in), value :: obj
      character(C_CHAR), dimension(*) :: type,compression
      integer(C_INT32_T),  value :: ni,nj,nk,pack,size
   end FUNCTION
       
!  json_object *Meta_AddCellMethod(json_object *Obj,char *Method);
   type(C_PTR) FUNCTION meta_addcellmethod(obj,method) BIND(C, name="Meta_AddCellMethod")
      import :: C_PTR,C_CHAR

      type(C_PTR), intent(in), value :: obj
      character(C_CHAR), dimension(*) :: method
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

!TODO: objectify
!  json_object* Meta_Copy(json_object *Obj);
   type(C_PTR) FUNCTION meta_copy(obj) BIND(C, name="Meta_Copy")
      import :: C_PTR

      type(C_PTR), intent(in), value :: obj
   end FUNCTION

!  int Meta_Equivalent(json_object *Obj1,json_object *Obj2);
   integer(kind=C_INT) FUNCTION meta_equivalent(obj1,obj2) BIND(C, name="Meta_Equivalent")
      import :: C_PTR, C_INT

      type(C_PTR), intent(in), value :: obj1, obj2
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

end interface

   type :: meta
      private
      type(C_PTR) :: json_obj = c_null_ptr
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
      procedure, pass :: getobject => tmeta_getobject
      procedure, pass :: copy => tmeta_copy
      final :: meta_final
   end type meta

contains

   SUBROUTINE meta_final(this)
      type(meta), intent(inout) :: this
      integer(kind=INT32) :: status

      status = meta_free(this%json_obj)
   end SUBROUTINE meta_final

   FUNCTION tmeta_loadprofile(this,name,version) result(status)
      class(meta), intent(inout)   :: this
      integer(kind=INT32)          :: status
      character(len=*), intent(in) :: name
      character(len=*), intent(in) :: version
 
      this%json_obj = meta_loadprofile(name//C_NULL_CHAR,version//C_NULL_CHAR)

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

   FUNCTION tmeta_resolve(this,obj) result(status)
      class(meta), intent(inout) :: this
      integer(kind=INT32) :: status
      type(C_PTR), intent(in) :: obj
      type(C_PTR) :: objr

      objr = meta_resolve(this%json_obj)
      status=1
      if (.not. c_associated(objr)) then
         status=0
      endif
   end FUNCTION tmeta_resolve

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

      status = meta_addmissingvalue(this%json_obj,reason//C_NULL_CHAR,value)
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

      status = meta_addcellmethod(this%json_obj,method//C_NULL_CHAR)
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

      status = meta_addqualifier(this%json_obj,qualifier//C_NULL_CHAR)
   end FUNCTION tmeta_addqualifier

   FUNCTION tmeta_clearqualifiers(this) result(status)
      class(meta), intent(inout) :: this
      type(C_PTR) :: status

      status = meta_clearqualifiers(this%json_obj)
   end FUNCTION tmeta_clearqualifiers

   FUNCTION tmeta_stringify(this) result(fstring)
      class(meta), intent(in) :: this
      character(kind=C_CHAR), dimension(:), allocatable :: fstring

      fstring = C_F_STRING_CONVERT(meta_stringify(this%json_obj))
   end FUNCTION tmeta_stringify

   FUNCTION tmeta_defvar(this,standardname,rpnname,longname,description) result(status)
      class(meta), intent(inout) :: this
      type(C_PTR) :: status
      character(len=*) :: standardname, rpnname, longname, description

      status = meta_defvar(this%json_obj,standardname//C_NULL_CHAR,rpnname//C_NULL_CHAR,longname//C_NULL_CHAR,description//C_NULL_CHAR)
   end FUNCTION

   FUNCTION tmeta_defbound(this,min,max,unit) result(status)
      class(meta), intent(inout) :: this
      type(C_PTR) :: status
      real(kind=REAL64), value :: min, max
      character(len=*) :: unit

      status = meta_defbound(this%json_obj,min,max,unit//C_NULL_CHAR)
   end FUNCTION

   FUNCTION tmeta_defforecasttime(this,t0,step,duration,unit) result(status)
      class(meta), intent(inout) :: this
      type(C_PTR) :: status
      integer(kind=INT64), value :: t0
      integer(kind=INT32),  value :: step
      real(kind=REAL64),  value :: duration
      character(len=*) :: unit

      status = meta_defforecasttime(this%json_obj,t0,step,duration,unit//C_NULL_CHAR)
   end FUNCTION

   FUNCTION tmeta_defverticalref(this,identifier,values,nb,copy) result(status)
      class(meta), intent(inout) :: this
      type(C_PTR) :: status
      character(len=*) :: identifier
      real(kind=REAL64), dimension(:) :: values
      integer(kind=INT32), value :: nb
      logical :: copy

      status = meta_defverticalref(this%json_obj,identifier//C_NULL_CHAR,values,nb,copy)
   end FUNCTION

   FUNCTION tmeta_defhorizontalref(this,identifier,copy) result(status)
      class(meta), intent(inout) :: this
      type(C_PTR) :: status
      character(len=*) :: identifier
!TODO: convert to int
      logical :: copy 

      status = meta_defhorizontalref(this%json_obj,identifier//C_NULL_CHAR,copy)
   end FUNCTION

   FUNCTION tmeta_defdata(this,ni,nj,nk,type,compression,pack,size) result(status)
      class(meta), intent(inout) :: this
      type(C_PTR) :: status
      character(len=*) :: type,compression
      integer(kind=INT32),  value :: ni,nj,nk,pack,size

      status = meta_defdata(this%json_obj,ni,nj,nk,type//C_NULL_CHAR,compression//C_NULL_CHAR,pack,size)
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