#include <stdint.h>
#include <string.h>

#include <App.h>
#include <rmn/rpnmacros.h>

//! \file c_fstgrib_helper.c Routines commodes pour ecrire des records GRIB encodes dans des fichiers standars

extern int32_t calc_crc(unsigned char *p, int32_t *flen, int32_t *fseed, int32_t stride);
extern long long f77name(f_gettimeofday_micro)();
extern void f77name(f_bits_put)(int32_t *bit_array, int32_t *bits_per_slice, int32_t *slices, int32_t *nslices);
extern void f77name(f_bits_get)(int32_t *bit_array, int32_t *bits_per_slice, int32_t *slices, int32_t *nslices);
void grb_84bits_to_ig1234(int32_t *ig1, int32_t *ig2, int32_t *ig3, int32_t *ig4, int32_t *the_84bit_token);
void grb_84bits_to_ip123(int32_t *ip1, int32_t *ip2, int32_t *ip3, int32_t *the_84bit_token);
void c_84bits_ig_get(int32_t *ig1, int32_t *ig2, int32_t *ig3, int32_t *ig4, int32_t *the_84bit_token);
void c_84bits_ig_put(int32_t *ig1, int32_t *ig2, int32_t *ig3, int32_t *ig4, int32_t *the_84bit_token);
void c_84bits_ip_get(int32_t *ip1, int32_t *ip2, int32_t *ip3, int32_t *the_84bit_token);
void c_84bits_ip_put(int32_t *ip1, int32_t *ip2, int32_t *ip3, int32_t *the_84bit_token);
void c_igaip84(int32_t *ip1, int32_t *ip2, int32_t *ip3, int32_t *ig1, int32_t *ig2, int32_t *ig3, int32_t *ig4);
void c_ipaig84(int32_t *ig1, int32_t *ig2, int32_t *ig3, int32_t *ig4, int32_t *ip1, int32_t *ip2, int32_t *ip3);
void f77name(f_igaip84)(int32_t *ip1, int32_t *ip2, int32_t *ip3, int32_t *ig1, int32_t *ig2, int32_t *ig3, int32_t *ig4);
void f77name(f_ipaig84)(int32_t *ig1, int32_t *ig2, int32_t *ig3, int32_t *ig4, int32_t *ip1, int32_t *ip2, int32_t *ip3);
void c_84bits_token(int32_t *the_84bit_token, unsigned char *grib_header, int32_t length_grib_header);
void f77name(f_def_84bitkey)(int32_t *ip1, int32_t *ip2, int32_t *ip3, int32_t *ig1, int32_t *ig2,
  int32_t *ig3, int32_t *ig4, unsigned char *grib_header, int *len_grib_header);
void c_def_84bitkey(int32_t *ip1, int32_t *ip2, int32_t *ip3, int32_t *ig1, int32_t *ig2,
  int32_t *ig3, int32_t *ig4, unsigned char *grib_header, int len_grib_header);


void f77name(f_def_84bitkey)(int32_t *ip1, int32_t *ip2, int32_t *ip3, int32_t *ig1, int32_t *ig2,
  int32_t *ig3, int32_t *ig4, unsigned char *grib_header, int *len_grib_header)
{
    c_def_84bitkey(ip1, ip2, ip3, ip1, ig2, ig3, ig4, grib_header, *len_grib_header);
}


void c_def_84bitkey(int32_t *ip1, int32_t *ip2, int32_t *ip3, int32_t *ig1, int32_t *ig2,
  int32_t *ig3, int32_t *ig4, unsigned char *grib_header, int len_grib_header)
{
  int32_t the_84bit_token[3];
  c_84bits_token(the_84bit_token, grib_header, len_grib_header);
  c_84bits_ip_get(ip1, ip2, ip3, the_84bit_token);
  c_84bits_ig_get(ig1, ig2, ig3, ig4, the_84bit_token);
}


void c_84bits_token(int32_t *the_84bit_token, unsigned char *grib_header, int32_t length_grib_header)
{
  long long time_of_day_micro;
  uint32_t time_of_day, micro_secs;
  int32_t slices[3];
  int32_t nslices = 3;
  int32_t fseed = 0;
  int32_t bits_per_slice[] = {32, 32, 20};
  int32_t header_crc;

  time_of_day_micro = f77name(f_gettimeofday_micro)();

  header_crc = calc_crc(grib_header, &length_grib_header, &fseed, 1);
  Lib_Log(APP_LIBFST,APP_EXTRA,"%s: %d crc: %d timeofday: %lld\n",__func__,strlen(grib_header), header_crc, time_of_day_micro);

  micro_secs = time_of_day_micro % 1000000;
  time_of_day = (time_of_day_micro - micro_secs) / 1000000;
  Lib_Log(APP_LIBFST,APP_EXTRA,"%s: %d %d %lld %lld\\n",__func__,time_of_day, micro_secs, time_of_day_micro, (long long)(time_of_day * 1000000 + micro_secs));

  slices[0] = header_crc;
  slices[1] = time_of_day;
  slices[2] = micro_secs;
  f77name(f_bits_put)((int32_t *)the_84bit_token, (int32_t *)bits_per_slice, (int32_t *)slices, (int32_t *)&nslices);
}


void grb_84bits_to_ip123(int32_t *ip1, int32_t *ip2, int32_t *ip3, int32_t *the_84bit_token)
{
  int32_t ip_bits_per_slice[] = {28, 28, 28};
  int32_t ip_slices[3];
  int32_t nslices = 3;

  f77name(f_bits_get)((int32_t *)the_84bit_token, (int32_t *)ip_bits_per_slice, (int32_t *)ip_slices, (int32_t *)&nslices);
  *ip1 = ip_slices[0];
  *ip2 = ip_slices[1];
  *ip3 = ip_slices[2];
}


void grb_84bits_to_ig1234(int32_t *ig1, int32_t *ig2, int32_t *ig3, int32_t *ig4, int32_t *the_84bit_token)
{
  int32_t ig_bits_per_slice[] = {21, 21, 21, 21};
  int32_t ig_slices[4];
  int32_t nslices = 4;

  f77name(f_bits_get)((int32_t *)the_84bit_token, (int32_t *)ig_bits_per_slice, (int32_t *)ig_slices, (int32_t *)&nslices);
  *ig1 = ig_slices[0];
  *ig2 = ig_slices[1];
  *ig3 = ig_slices[2];
  *ig4 = ig_slices[3];
}


void c_84bits_ig_get(int32_t *ig1, int32_t *ig2, int32_t *ig3, int32_t *ig4, int32_t *the_84bit_token)
{
  int32_t ig_bits_per_slice[] = {21, 21, 21, 21};
  int32_t ig_slices[4];
  int32_t nslices = 4;

  f77name(f_bits_get)((int32_t *)the_84bit_token, (int32_t *)ig_bits_per_slice, (int32_t *)ig_slices, (int32_t *)&nslices);
  *ig1 = ig_slices[0];
  *ig2 = ig_slices[1];
  *ig3 = ig_slices[2];
  *ig4 = ig_slices[3];
}


void c_84bits_ig_put(int32_t *ig1, int32_t *ig2, int32_t *ig3, int32_t *ig4, int32_t *the_84bit_token)
{
  int32_t ig_bits_per_slice[] = {21, 21, 21, 21};
  int32_t ig_slices[4];
  int32_t nslices = 4;

  memset(the_84bit_token, 0, 3*sizeof(int32_t));
  ig_slices[0] = *ig1;
  ig_slices[1] = *ig2;
  ig_slices[2] = *ig3;
  ig_slices[3] = *ig4;
  f77name(f_bits_put)((int32_t *)the_84bit_token, (int32_t *)ig_bits_per_slice, (int32_t *)ig_slices, (int32_t *)&nslices);
}


void c_84bits_ip_get(int32_t *ip1, int32_t *ip2, int32_t *ip3, int32_t *the_84bit_token)
{
  int32_t ip_bits_per_slice[] = {28, 28, 28};
  int32_t ip_slices[3];
  int32_t nslices = 3;

  f77name(f_bits_get)((int32_t *)the_84bit_token, (int32_t *)ip_bits_per_slice, (int32_t *)ip_slices, (int32_t *)&nslices);
  *ip1 = ip_slices[0];
  *ip2 = ip_slices[1];
  *ip3 = ip_slices[2];
}


void c_84bits_ip_put(int32_t *ip1, int32_t *ip2, int32_t *ip3, int32_t *the_84bit_token)
{
  int32_t ip_bits_per_slice[] = {28, 28, 28};
  int32_t ip_slices[3];
  int32_t nslices = 3;

  memset(the_84bit_token, 0, 3*sizeof(int32_t));
  ip_slices[0] = *ip1;
  ip_slices[1] = *ip2;
  ip_slices[2] = *ip3;
  f77name(f_bits_put)((int32_t *)the_84bit_token, (int32_t *)ip_bits_per_slice, (int32_t *)ip_slices, (int32_t *)&nslices);
}


void c_igaip84(int32_t *ip1, int32_t *ip2, int32_t *ip3, int32_t *ig1, int32_t *ig2, int32_t *ig3, int32_t *ig4)
{
  int32_t the_84bit_token[3];

  c_84bits_ig_put(ig1, ig2, ig3, ig4, the_84bit_token);
  c_84bits_ip_get(ip1, ip2, ip3, the_84bit_token);
}


void c_ipaig84(int32_t *ig1, int32_t *ig2, int32_t *ig3, int32_t *ig4, int32_t *ip1, int32_t *ip2, int32_t *ip3)
{
  int32_t the_84bit_token[3];

  c_84bits_ip_put(ip1, ip2, ip3, the_84bit_token);
  c_84bits_ig_get(ig1, ig2, ig3, ig4, the_84bit_token);
}


void f77name(f_igaip84)(int32_t *ip1, int32_t *ip2, int32_t *ip3, int32_t *ig1, int32_t *ig2, int32_t *ig3, int32_t *ig4)
{
  c_igaip84(ip1,ip2, ip3, ig1, ig2, ig3, ig4);
}


void f77name(f_ipaig84)(int32_t *ig1, int32_t *ig2, int32_t *ig3, int32_t *ig4, int32_t *ip1, int32_t *ip2, int32_t *ip3)
{
  c_ipaig84(ig1, ig2, ig3, ig4, ip1, ip2, ip3);
}
