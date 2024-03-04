program test_meta_fortran

    use rmn_fst24
    use rmn_meta
    use rmn_common

    type(meta) :: meta_fld, meta_file, meta_tmp
    type(fst_record) :: record
    type(C_PTR) obj
    real(kind=REAL64), dimension(1) :: levels = [ 1.0 ]
    integer(kind=INT32) :: ok
 
!   Load metadata template
    ok=meta_fld%init(META_TYPE_FIELD,C_NULL_CHAR)
    ok=meta_file%init(META_TYPE_FILE,C_NULL_CHAR)

    obj=meta_file%DefFile("CMC","Weather","G100","GDPS-5.2.0","Global forecast at 15km","Operational")
    obj=meta_file%AddHorizontalRef("RPN_GDPS_2020_25KM",.true.)
    obj=meta_file%AddVerticalRef("PRESSURE",.true.)

    write(6,*) 'JSON:',meta_file%Stringify()
 
!    obj=meta_fld%DefVar("air_temperature","TT","air temperature","Air temperature is the bulk temperature of the air, not the surface (skin) temperature","celsius")
    obj=meta_fld%DefVarFromDict("TT")
    obj=meta_fld%DefForecastTime(1672556400_C_LONG,2,60.0d0,"minutes")
    obj=meta_fld%DefHorizontalRef("RPN_GDPS_2020_25KM",.false.)
    obj=meta_fld%DefVerticalRef("PRESSURE",levels,1,.false.)
    obj=meta_fld%DefData(100,100,1,"float","lorenzo",16,32,-60.0d0,50.0d0)

    obj=meta_fld%AddCellMethod("interpolation:linear")
    obj=meta_fld%AddCellMethod("filter:gaussian")
    obj=meta_fld%AddCellMethod("time:mean(interval 5 minute)")

    obj=meta_fld%AddQualifier("prognosis")
    obj=meta_fld%AddQualifier("operational")
    obj=meta_fld%AddQualifier("member:12")
    obj=meta_fld%AddQualifier("centile>75")

!   This should cause an error
    obj=meta_fld%AddQualifier("fdsscentile>75")
    obj=meta_fld%AddMissingValue("out of domain",-999.0d0)
    obj=meta_fld%AddMissingValue("bad value",-998.0d0)
 
!    obj=meta_fld%ClearCellMethods()
!    obj=meta_fld%ClearQualifiers()
!    obj=meta_fld%ClearMissingValues()
 
!    Output formatted
     ok=meta_fld%Resolve(meta_file);

!     write(6,*) 'JSON:',meta_fld%Stringify()

     obj=record%set_metadata(meta_fld)
     meta_tmp=record%get_metadata()
     write(6,*) 'JSON:',meta_tmp%Stringify()
     
end
