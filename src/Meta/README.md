# Table of Contents
1. [Introduction](#introduction)
2. [Grammar](#grammar)
   1. [Field](#field-level-metadata-and-fst98-rpn-correspondance)
   2. [File](#file-level)
3. [Environment variables](#environment_variables)
4. [Code Example](#code_example)
   1. [C](#C)
   2. [Fortran](#fortran)

# Introduction
- The new metadata now available in fst24 allows the storage of much more information while being flexible, extensible and future proof.
- This metadata is in addition to the previous indexable metadata already available (NOMVAR,TYPVAR,IP,...) and will keep backward compatibility with previous FST89 and FST98.
- It uses a grammar for token/value pair definitions implemented as JSON, and versionned through JSON profiles. 
- Management of the profiles and accepted value pair shall be managed by the [Data Governance Commitee (DGC/CGD)](https://wiki.cmc.ec.gc.ca/wiki/Comit%C3%A9_de_gouvernance_des_donn%C3%A9es_du_CMC)

# Grammar
## Field level metadata and FST98 RPN correspondance

| RPN          | Extended metadata                                                         |                            
|--------------|--------------------------------------------------------------|
|              |version = [semantic version]                                  |
|              |standard_name = [cf convention]                               |
|NOMVAR        |rpn_name = [dict]                                             |
|              |long_name = "…“                                               |
|              |description ="…"                                              |
|              |units = [udunits]                                             |
|NI,NJ,NK      |size=[ni,nj,nk]                                               |
|GRTYP         |horizontal_reference= [horizontal_reference id]               |
|IP2-3         |cell_methods = [cell_method]*                                 |
|              |missing_values = [reason value]*                              |
|              |mask = "…"                                                    |
|IP1           |vertical_level = value [vertical_reference id]                |
|ETIKET+TYPVAR |qualifiers = [qualifier]*                                     |
|DATEO         |forecast_reference_datetime = [datetime]                      |
|DATEV         |forecast_datetime  = [datetime]                               |
|DEET+NPAS     |forecast_period = [size time_unit step]                       |
|NPACK         |type = [datatype]                                             |
|              |bits = [nunmber of bits]                                      |
|              |pack = [number of packed bits]                                |
|              |compression = [compression type]                              |
|              |bounds= [lower, higher]                                       |
  
- axis          = [time, level, latitude, longitude, x, y, z]
- process       = [average, sum, minimum, maximum, median, standard_deviation, error, …]
- time_unit     = [seconds, minutes, hours, days, weeks, years, ...]
- datetime      = [ISO8601](https://en.wikipedia.org/wiki/ISO_8601) formatted
- qualifier     = [analysis, climatology, raw_station, error, constants, verification_matrix, observation, prognostic, diagnostic, analysis_increment, scores, timeseries, operational, parallel, experimental, member:n, centile[<,<=,=,>,>=]value, …]
- unit          = [udunit] | [time_unit] 
- filter        = [gaussian, …] 
- interpolator  = [nearest, linear, cubic, …]
- cell_method   = interpolation:[interpolator] | filter:[filter] | [axis]:[process]([interval [n]|[value_0 value_1] [unit])*

(For all accepted and up to date values of these parameters see ```${CMCCONST}/json/[version]/definitions.json```)

### json template 0.1.0
``` json
{  "version" : "",
   "standard_name" : "",
   "rpn_name" : "",
   "long_name" : "",
   "description" : "",
   "unit" : "",
   "size" : [],
   "cell_methods" : [],
   "qualifiers" : [],
   "missing_values" : [],
   "mask"  : "",
   "vertical_level" : {
      "value" : [],
      "vertical_reference" : ""
   },
   "horizontal_reference" : "",
   "forecast_reference_datetime" : "",
   "forecast_datetime" : "",
   "forecast_period" : {
      "step"  : -1,
      "value" : NaN,
      "unit"  : ""
   },
   "data" : {
      "type" : "",
      "bits" : 0,
      "pack" : 0,
      "compression" : "",
      "bounds" : {
         "min" : NaN,
         "max" : NaN
      }
   }
}
```

## File level

| Extended metadata                                |
|--------------------------------------------------|
| version : [semantic version]                     |
| institution : "CMC"                              |
| discipline" : [climatology,meteorology,…]        |
| title" : "run id"                                |
| source" : [GDPS, RDPS, HRDPS, …][version]        |
| description" : ""                                |
| state : [Operational, parallel, experimenta, ...]|
| vertical_references" : []                        |
| horizontal_references" : []                      |

### json template 0.1.0
```json
{
   "version" : "",
   "institution" : "CMC",
   "discipline" : "",
   "title" : "",
   "source" : "",
   "state" : "",
   "description" : "",
   "vertical_references" : [],
   "horizontal_references" : []
}
```

# Environment variables
- **CMCCONST**      : Location of standard metadata templates and definitions (```${CMCCONST}/json/${META_VERSION}```)
- **META_PROFPATH** : Location of user defined metadata templates and definitions (```${META_PROFPATH}/json/${META_VERSION}```)
- **META_VERSION**  : Version of metadata to be used (Default: **latest**)
- **META_VALIDATE** : Enable validation of json values (**TRUE** or **FALSE**, Default: **FALSE**)
- **META_MATCH**    : Define matching mode when comparing two metadata set (**EQUAL** or **REGEXP**, Default: **EQUAL**)

# Code example
## C
```C
#include <rmn/Meta.h>
#include <stdio.h>
#include <stdlib.h>

int main(int argc, char **argv) {
    json_object *prof_file = NULL;
    json_object *prof_fld = NULL;
    double levels[1] = { 1000.0 };

    // Create metadata object from template
    prof_fld = Meta_New(META_TYPE_RECORD, NULL);
    prof_file = Meta_New(META_TYPE_FILE, NULL);

    // Define file level metadata
    Meta_DefFile(prof_file, "CMC", "Weather", "G100", "GDPS-5.2.0", "Global forecast at 15km", "Operational");
    Meta_AddHorizontalRef(prof_file, "RPN_GDPS_2020_25KM", TRUE);
    Meta_AddVerticalRef(prof_file, "PRESSURE", TRUE);

    fprintf(stderr, "File JSON: %s\n", Meta_Stringify(prof_file));

    // Define field level metadata
    //   Meta_DefVar(prof_fld,"air_temperature","TT","air temperature","Air temperature is the bulk temperature of the air, not the surface (skin) temperature","celsius");
    Meta_DefVarFromDict(prof_fld, "TT");
    Meta_DefForecastTime(prof_fld, 1672556400, 2, 60, "second");
    Meta_DefHorizontalRef(prof_fld, "RPN_GDPS_2020_25KM", FALSE); // RPN_GDPS_2020_25KM has to be defined in profile of file metadata
    Meta_DefVerticalRef(prof_fld, "PRESSURE", levels, 1, FALSE);    // PRESSURE has to be defined in profile of file metadata
    Meta_AddCellMethod(prof_fld, "interpolation:linear");
    Meta_AddCellMethod(prof_fld, "filter:gaussian");
    Meta_AddCellMethod(prof_fld, "time:mean(interval 5 minute)");
    Meta_AddQualifier(prof_fld, "prognosis");
    Meta_AddQualifier(prof_fld, "tag:ETKGG22");
    Meta_AddQualifier(prof_fld, "member:12");
    Meta_AddQualifier(prof_fld, "centile>15");

    Meta_AddMissingValue(prof_fld, "out_of_domain", -999);

    fprintf(stderr, "Field JSON: %s\n", Meta_Stringify(prof_file));

	exit(EXIT_SUCCESS);
}
```

## Fortran
```Fortran
program meta_fortran
    use rmn_meta
    use rmn_common

    type(meta) :: meta_fld, meta_file
    type(C_PTR) obj
    real(kind = REAL64), dimension(1) :: levels = [ 1.0 ]
    integer(kind = INT32) :: ok
 
    ! Create metadata object from template
    ok = meta_fld%Init(META_TYPE_RECORD, "")
    ok = meta_file%Init(META_TYPE_FILE, "")

    ! Define file level metadata
    obj = meta_file%DefFile("CMC", "Weather", "G100", "GDPS-5.2.0", "Global forecast at 15km", "Operational")
    obj = meta_file%AddHorizontalRef("RPN_GDPS_2020_25KM", .true.)
    obj = meta_file%AddVerticalRef("PRESSURE", .true.)

    write(6,*) 'File JSON:',meta_file%Stringify()
 
    ! Define file level metadata
    ! obj = meta_fld%DefVar("air_temperature", "TT", "air temperature", "Air temperature is the bulk temperature of the air, not the surface (skin) temperature", "celsius")
    obj = meta_fld%DefVarFromDict("TT")
    obj = meta_fld%DefForecastTime(1672556400_C_LONG, 2, 60.0d0, "seconds") 
    obj = meta_fld%DefHorizontalRef("RPN_GDPS_2020_25KM", .false.)        ! RPN_GDPS_2020_25KM has to be defined in profile of file metadata
    obj = meta_fld%DefVerticalRef("PRESSURE", levels, 1, .false.)           ! PRESSURE has to be defined in profile of file metadata
    obj = meta_fld%AddCellMethod("interpolation:linear")
    obj = meta_fld%AddCellMethod("filter:gaussian")
    obj = meta_fld%AddCellMethod("time:mean(interval 5 minute)")
    obj = meta_fld%AddQualifier("prognosis")
    obj = meta_fld%AddQualifier("tag:ETKGG22");
    obj = meta_fld%AddQualifier("member:12")
    obj = meta_fld%AddQualifier("centile>75")

    obj = meta_fld%AddMissingValue("out of domain", -999.0d0)

    ! Resolve references
    ok = meta_fld%Resolve(meta_file);

    ! Output formatted
    write(6,*) 'Field JSON:', meta_fld%Stringify()
end
```