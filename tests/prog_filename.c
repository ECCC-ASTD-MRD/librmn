#include <stdio.h>

#include <rmn/rpnmacros.h>

int32_t f77name(prog_filename)(
    char *f_nom,
    //! Prefix 2 alphabetic characters (A-Z,a-z)
    unsigned char *f_prefix,
    //! 8 digit number from newdate, ie:20010120
    int32_t *f_date,
    //! Hour as a number between 0 and 24
    int32_t *f_hour,
    //! Minutes; 00 - 59, if -1, omitted
    int32_t *f_min,
    //! Seconds; 00 - 59, if -1, omitted
    int32_t *f_sec,
    //! PE row;  00 - 99, if -1, omitted
    int32_t *f_npex,
    //! PE col;  00 - 99, if -1, omitted
    int32_t *f_npey,
    //! Multiple of unit;  >= 0
    int32_t *f_num,
    //! Number of digits for num, optional
    int32_t *f_numlen,
    //! 1 character to represent type of units, optional
    char *f_unit,
    //! Fortran string length of f_nom
    F2Cl lf_nom,
    //! Fortran string length of f_prefix
    F2Cl lf_prefix,
    //! Fortran string length of f_unit
    F2Cl lf_unit
);

int main() {
    const int fileNameLen = 50;
    char fileName[fileNameLen + 1];
    fileName[0] = '\0';
    int date = 20220922;
    int hour = 0;
    int min = -1;
    int sec = -1;
    int npex = -1;
    int npey = -1;
    int num = 1;
    int numlen = 0;

    int res = f77name(prog_filename)(fileName, "pp", &date, &hour, &min, &sec, &npex, &npey, &num, &numlen, "", fileNameLen, 2, 0);
    int i = 0;
    while (fileName[i] != ' ') i++;
    fileName[i] = '\0';
    printf("%s\n", fileName);
    // pp2022092200_001

    date = 20220922;
    hour = 16;
    min = 36;
    sec = 42;
    npex = 24;
    npey = 25;
    num = 1;
    numlen = 3;

    res = f77name(prog_filename)(fileName, "si", &date, &hour, &min, &sec, &npex, &npey, &num, &numlen, "", fileNameLen, 2, 0);
    i = 0;
    while (fileName[i] != ' ') i++;
    fileName[i] = '\0';
    printf("%s\n", fileName);
    // si20220922163642-24-25_001

    return 0;
}
