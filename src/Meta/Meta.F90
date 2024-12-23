module rmn_meta
    use App
    use f_c_strings_mod
    use rmn_common
    implicit none

#include "rmn/dlfcn.inc"

    include 'Meta.inc'

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