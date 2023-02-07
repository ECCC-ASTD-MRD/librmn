#ifndef CONVERT_IP_DEFS
#define CONVERT_IP_DEFS

#define TO_IP 1
#define TO_RP -1
#define CONVERT_OK 0
#define CONVERT_GUESS 14
#define CONVERT_GOOD_GUESS 2
#define CONVERT_BAD_GUESS 4
#define CONVERT_TERRIBLE_GUESS 8
#define CONVERT_WARNING 32
#define CONVERT_ERROR 64

#define KIND_ABOVE_SEA 0
#define KIND_SIGMA 1
#define KIND_PRESSURE 2
#define KIND_ARBITRARY 3
#define KIND_ABOVE_GND 4
#define KIND_HYBRID 5
#define KIND_THETA 6
#define KIND_BELOW_SEA 7
#define KIND_HOURS 10
#define KIND_SAMPLES 15
#define KIND_MTX_IND 17
#define KIND_M_PRES 21

//! IP level info
/*!
    If v1 == v2, it is not a range but a single value.
 */
typedef struct { /*  */
    //! First value of range
    float v1;
    //! Second value of range
    float v2;
    //! Kind of value
    /*!
        | Kind | Description                               | Range          |
        | ---: | :---------------------------------------- | -------------: |
        |    0 | Height in reference to mean sea level (m) | -20000, 100000 |
        |    1 | Sigma                                     |       0.0, 1.0 |
        |    2 | Pressure (mb)                             |        0, 1100 |
        |    3 | Arbitrary code                            | -4.8e8, 1.0e10 |
        |    4 | Height above ground level (m)             | -20000, 100000 |
        |    5 | Hybrid coordinate                         |       0.0, 1.0 |
        |    6 | Theta coordinate                          |      1, 200000 |
        |    7 | Height below sea level                    |      0, 20000  |
        |   10 | Time (h)                                  |  0.0, 200000.0 |
        |   15 | Reserved (integer)                        |                |
        |   17 | X index of the conversion matrix          |    1.0, 1.0e10 |
        |   21 | Pressure-metre                            |     0, 1000000 |
     */
    int kind;
} ip_info;


/* see fortran module convert_ip123.f90 for quick documentation of arguments */

#ifdef __cplusplus
extern "C" {
#endif
    void ConvertIp(int * const ip, float * const p, int * const kind, const int mode);

    int EncodeIp( int *ip1, int *ip2, int *ip3, ip_info *p1, ip_info *p2, ip_info *p3);
    int DecodeIp(ip_info *p1, ip_info *p2, ip_info *p3, int ip1, int ip2, int ip3);

    int EncodeIp_v(int ip[3],ip_info p[3]);
    int DecodeIp_v(ip_info p[3],int ip[3]);

    int ConvertPKtoIP(int *ip1, int *ip2, int *ip3, float p1, int kind1, float p2, int kind2, float p3, int kind3);
    int ConvertIPtoPK(float *p1, int *kind1, float *p2, int *kind2, float *p3, int *kind3, int ip1, int ip2, int ip3);

    int ConvertPKtoIP_v(int ip[3],float p[3],int kind[3]);
    int ConvertIPtoPK_v(float p[3],int kind[3],int ip[3]);

    void KindToString(int code, char *s1, char *s2) ;
#ifdef __cplusplus
}
#endif

#endif
