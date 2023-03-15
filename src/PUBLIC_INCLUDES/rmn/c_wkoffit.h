#ifndef C_WKOFFIT_H
#define C_WKOFFIT_H

#define WKF_CORROMPU              -4
#define WKF_INEXISTANT            -3
#define WKF_VIDE                  -2
#define WKF_INCONNU               -1
#define WKF_RANDOM89               1
#define WKF_SEQUENTIEL89           2
#define WKF_SEQUENTIELFORTRAN89    3
#define WKF_CCRN                   4
#define WKF_CCRN_RPN               5
#define WKF_BURP                   6
#define WKF_GRIB                   7
#define WKF_BUFR                   8
#define WKF_BLOK                   9
#define WKF_FORTRAN               10
#define WKF_COMPRESS              11
#define WKF_GIF89                 12
#define WKF_GIF87                 13
#define WKF_IRIS                  14
#define WKF_JPG                   15
#define WKF_KMW                   16
#define WKF_PBM                   17
#define WKF_PCL                   18
#define WKF_PCX                   19
#define WKF_PDSVICAR              20
#define WKF_PM                    21
#define WKF_PPM                   22
#define WKF_PS                    23
#define WKF_KMW_                  24
#define WKF_RRBX                  25
#define WKF_SUNRAS                26
#define WKF_TIFF                  27
#define WKF_UTAHRLE               28
#define WKF_XBM                   29
#define WKF_XWD                   30
#define WKF_ASCII                 31
#define WKF_BMP                   32
#define WKF_RANDOM98              33
#define WKF_SEQUENTIEL98          34
#define WKF_NETCDF                35
#define WKF_CMCARC4               36
#define WKF_CMCARC5               37
#define WKF_HDF5                  38
#define WKF_STDRSF                39

int32_t c_wkoffit(
    const char * const filePath,
    const int l1
);

#endif
