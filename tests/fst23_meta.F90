program test_meta_fortran

    use rmn_meta
    use rmn_common

    type(meta) :: meta_fld, meta_file
    type(C_PTR) obj
    integer(kind=INT32) :: ok
    character(kind=C_CHAR), dimension(:), pointer :: output
    call Meta_Init()
 
!   Load metadata template
    ok=meta_fld%LoadProfile("field","")
    ok=meta_file%LoadProfile("file","")

!    obj=meta_fld%DefVar("air_temperature","TT","air temperature","Air temperature is the bulk temperature of the air, not the surface (skin) temperature")
!    obj=meta_fld%DefBound(-60.0_C_DOUBLE,50.0_C_DOUBLE,"celsius");
!    obj=meta_fld%DefForecastTime(1672556400_C_LONG,2,60.0_C_DOUBLE,"minutes");
!    obj=meta_fld%DefHorizontalRef("RPN_GDPS_2020_25KM",.false.);
!    obj=meta_fld%DefVerticalRef("PRESSURE",1000.0_C_DOUBLE,.false.);
!    obj=meta_fld%DefData("float","lorenzo",16,32);
    obj=meta_fld%AddCellMethod("interpolation:linear");
!    obj=meta_fld%AddCellMethod("filter:gaussian");
!    obj=meta_fld%AddCellMethod("time:mean(interval 5 minute)");
!    obj=meta_fld%AddQualifier("prognosis");
!    obj=meta_fld%AddQualifier("operational");
!    obj=meta_fld%AddQualifier("member:12");
!    obj=meta_fld%AddQualifier("centile>75");

!   This should cause an error
!    obj=meta_fld%AddQualifier("fdsscentile>75");  
!    obj=meta_fld%AddMissingValue("out of domain",-999.0_C_DOUBLE);
!    obj=meta_fld%AddMissingValue("bad value",-998.0_C_DOUBLE);
 
!    obj=meta_fld%ClearCellMethods();
!    obj=meta_fld%ClearQualifiers();
!    obj=meta_fld%ClearMissingValues();
 
!    Output formatted
     output=meta_fld%Stringify();
     write(6,*) output
  
end
