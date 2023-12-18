program test_meta_fortran

    use, intrinsic :: iso_c_binding
    use meta

    implicit none

    type(C_PTR) prof_fld,prof_file,obj
    character(C_CHAR) output
    call Meta_Init()
 
!   Load metadata template
    prof_fld=Meta_LoadProfile("field","0.1.0");
    prof_file=Meta_LoadProfile("file","0.1.0");

    obj=Meta_DefVar(prof_fld,"air_temperature","TT","air temperature","Air temperature is the bulk temperature of the air, not the surface (skin) temperature")
    obj=Meta_DefBound(prof_fld,-60.0_C_DOUBLE,50.0_C_DOUBLE,"celsius");
    obj=Meta_DefForecastTime(prof_fld,1672556400_C_LONG,2,60.0_C_DOUBLE,"minutes");
!    Meta_DefHorizontalRef(prof_fld,"GRID_CYLINDRIC",false);
!    Meta_DefVerticalRef(prof_fld,"LEVEL_PRESSURE",1000.0,false);
!    Meta_DefData(prof_fld,"lorenzo",32);
!    Meta_AddCellMethod(prof_fld,"interpolation:linear");
!    Meta_AddCellMethod(prof_fld,"filter:gaussian");
!    Meta_AddCellMethod(prof_fld,"time:mean(interval 5 minute)");
!    Meta_AddQualifier(prof_fld,"prognosis");
!    Meta_AddQualifier(prof_fld,"operational");
!    Meta_AddQualifier(prof_fld,"member:12");
!    Meta_AddQualifier(prof_fld,"centile>75");
!    Meta_AddQualifier(prof_fld,"fdsscentile>75");  // This should cause an error
!    Meta_AddMissingValue(prof_fld,"out of domain",-999);
!    Meta_AddMissingValue(prof_fld,"bad value",-998);
 
!    Meta_ClearCellMethods(prof_fld);
!    Meta_ClearQualifiers(prof_fld);
!    Meta_ClearMissingValues(prof_fld);
 
!    Output formatted
!     output=Meta_Stringify(prof_fld);
!     write(6,*) output
  
end
