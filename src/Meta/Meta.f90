module meta
   use, intrinsic :: iso_c_binding
   implicit none

   interface

!  void         Meta_Init();
   SUBROUTINE meta_init() BIND(C, name="Meta_Init")
      use, intrinsic :: iso_c_binding
   end SUBROUTINE
    
!  char *Meta_Stringify(json_object *Obj);
   type(C_PTR) FUNCTION meta_stringify(obj) BIND(C, name="Meta_Stringify")
      use, intrinsic :: iso_c_binding
      implicit none

      type(C_PTR), intent(in) :: obj
   end FUNCTION

!  int          Meta_Free(json_object *Obj);
!  char*        Meta_Stringify(json_object *Obj);
!  json_object* Meta_Parse(char *MetaString);
!  int          Meta_ArrayLength(json_object *Obj);
!  json_object* Meta_ArrayGetObject(json_object *Obj,int Idx);
!  json_object* Meta_ArrayFind(json_object *Obj,char *Token);
!  json_object* Meta_GetObject(json_object *Obj,char *Path);
!  char*        Meta_GetObjectString(json_object *Obj);
!  json_object* Meta_Copy(json_object *Obj);
!  int          Meta_Equivalent(json_object *Obj1,json_object *Obj2);
!  int          Meta_To89(json_object *Obj,fst_record *Rec);
!  int          Meta_From89(json_object *Obj,fst_record *Rec);
!  json_object* Meta_ResolveRef(json_object *Obj);
   
!  json_object *Meta_LoadProfile(char *Name,char *Version);
   type(C_PTR) FUNCTION meta_loadprofile4fortran(name,version) BIND(C, name="Meta_LoadProfile")
      use, intrinsic :: iso_c_binding
      implicit none

      character(C_CHAR), dimension(*) :: name
      character(C_CHAR), dimension(*) :: version
   end FUNCTION

!  json_object *Meta_DefVar(json_object *Obj,char *StandardName,char* RPNName,char *LongName,char *Description);
   type(C_PTR) FUNCTION meta_defvar4fortran(obj,standardname,rpnname,longname,description) BIND(C, name="Meta_DefVar")
      use, intrinsic :: iso_c_binding
      implicit none

      type(C_PTR) :: obj
      character(C_CHAR), dimension(*) :: standardname
      character(C_CHAR), dimension(*) :: rpnname
      character(C_CHAR), dimension(*) :: longname
      character(C_CHAR), dimension(*) :: description
   end FUNCTION

!  json_object *Meta_DefBound(json_object *Obj,double Min,double Max,const char* Unit);
   type(C_PTR) FUNCTION meta_defbound4fortran(obj,min,max,unit) BIND(C, name="Meta_DefBound")
      use, intrinsic :: iso_c_binding
      implicit none

      type(C_PTR) :: obj
      real(C_DOUBLE), value :: min
      real(C_DOUBLE), value :: max
      character(C_CHAR), dimension(*) :: unit
   end FUNCTION

!  json_object *Meta_DefForecastTime(json_object *Obj,time_t T0,int Step,double Duration,char *Unit);
   type(C_PTR) FUNCTION meta_defforecasttime4fortran(obj,t0,step,duration,unit) BIND(C, name="Meta_DefForecastTime")
      use, intrinsic :: iso_c_binding
      implicit none

      type(C_PTR) :: obj
      integer(C_LONG), value :: t0
      integer(C_INT),  value :: step
      real(C_DOUBLE),  value :: duration
      character(C_CHAR), dimension(*) :: unit
   end FUNCTION

!  json_object *Meta_DefVerticalRef(json_object *Obj,char* Identifier,double Value,bool Copy);
!  json_object *Meta_DefHorizontalRef(json_object *Obj,char* Identifier,bool Copy);
!  json_object *Meta_DefData(json_object *Node,TMeta_Compressions CompType,int Prec);
   
!  json_object *Meta_AddVerticalRef(json_object *Obj,char* Identifier,bool Copy);
!  json_object *Meta_AddHorizontalRef(json_object *Obj,char* Identifier,bool Copy);
!  json_object *Meta_AddCellMethod(json_object *Obj,char *Method);
!  json_object *Meta_AddQualifier(json_object *Obj,char *Qualifier);
!  json_object *Meta_AddMissingValue(json_object *Obj,char *Reason,double Value);
    
!  json_object *Meta_ClearCellMethods(json_object *Obj);
!  json_object *Meta_ClearQualifiers(json_object *Obj);

!  json_object *Meta_ClearMissingValues(json_object *Obj);
   type(C_PTR) FUNCTION meta_clearmissingvalues(obj) BIND(C, name="Meta_ClearMissingValues")
      use, intrinsic :: iso_c_binding
      implicit none
      type(C_PTR), intent(in) :: obj
   end FUNCTION

   end interface

   contains

   type(C_PTR) FUNCTION meta_loadprofile(name,version)
      use, intrinsic :: iso_c_binding
      implicit none

      character(len=*) :: name
      character(len=*) :: version

      meta_loadprofile = meta_loadprofile4fortran(name//C_NULL_CHAR,version//C_NULL_CHAR)
   end FUNCTION

   type(C_PTR) FUNCTION meta_defvar(obj,standardname,rpnname,longname,description)
      use, intrinsic :: iso_c_binding
      implicit none

      type(C_PTR) :: obj
      character(len=*) :: standardname
      character(len=*) :: rpnname
      character(len=*) :: longname
      character(len=*) :: description

      meta_defvar = meta_defvar4fortran(obj,standardname//C_NULL_CHAR,rpnname//C_NULL_CHAR,longname//C_NULL_CHAR,description//C_NULL_CHAR)
   end FUNCTION

   type(C_PTR) FUNCTION meta_defbound(obj,min,max,unit)
      use, intrinsic :: iso_c_binding
      implicit none

      type(C_PTR) :: obj
      real(C_DOUBLE), value :: min
      real(C_DOUBLE), value :: max
      character(len=*) :: unit
      meta_defbound=meta_defbound4fortran(obj,min,max,unit//C_NULL_CHAR)
   end FUNCTION

   type(C_PTR) FUNCTION meta_defforecasttime(obj,t0,step,duration,unit)
      use, intrinsic :: iso_c_binding
      implicit none

      type(C_PTR) :: obj
      integer(C_LONG), value :: t0
      integer(C_INT),  value :: step
      real(C_DOUBLE),  value :: duration
      character(len=*) :: unit
      meta_defforecasttime=meta_defforecasttime4fortran(obj,t0,step,duration,unit//C_NULL_CHAR)
   end FUNCTION
end module