
int main(int argc, char **argv)
{

  union {
    int i;
    float f;
  }i_or_f;
  int i, j;
  float heures;
  int ip1s_i[] = { 400, 500, 600, 750 };
  float ip1s_r[] = { .3840, .4440, 0.6110 };
  float ip1s_r_range[] = { .3280, 0.8000 };
  int ip1s_range[] = {400, READLX_RANGE, 750};
  int ip1s_range2[] = {READLX_RANGE, 750};
  int ip1s_range3[] = { 750, READLX_RANGE};
  int ip2s[] = {0, 12, 24};
  int ip3s[] = {0, 80, 90};
  int dates[] = {313290800, 313301600};
  int dates_range[] = {313280000, READLX_RANGE, 313290800};
  int dates_range1[] = {313280000, READLX_RANGE, 313290800, READLX_DELTA, 12};
  int dates_range2[] = {313280000, READLX_RANGE};
  int dates_range3[] = {READLX_RANGE, 313290800};
  int dates_range4[] = {313280000, READLX_RANGE, 313290800, READLX_DELTA, 12};

  char *testeti[] = { "Etiquette #1", "R2428V4N", "Etiquet #3" };
  char *testnom[2] = { "TT", "GZ" };
  char *testtyp[4] = { "P", "V1", "V2", "V3" };
  int narg = 2;

  i_or_f.f = 5399.6;
  dates_range1[4] = i_or_f.i;

  dbprint(stddebug, "Debug debut \n");
  dbprint(stddebug, "Debug testeti=%s %s %s \n", testeti[0], testeti[1], testeti[2]);
  dbprint(stddebug, "Debug testeti=%s %s %s \n", testeti[0], testeti[1], testeti[2]);
  dbprint(stddebug, "Debug testnom=%s %s \n", testnom[0], testnom[1]);
  dbprint(stddebug, "Debug testtyp=%s %s %s %s \n", testtyp[0], testtyp[1], testtyp[2], testtyp[3]);
  RequetesInit();
fprintf(stderr, "DEBUG: %d\n", 1);
  i = Xc_Select_etiquette(5, 1, testeti, 3);
  i = Xc_Select_nomvar(5, 1, testnom, 2);
  i = Xc_Select_typvar(5, 1, testtyp, 4);
  i = Xc_Select_ip1(5, 1, ip1s_range, 3);
//  i = Xc_Select_ip1(1, 1, ip1s_i, 4);
  i = Xc_Select_ip2(5, 1, ip2s, 3);
  i = Xc_Select_date(5, 1, dates_range4, 5);

  i = Xc_Select_etiquette(2, 1, testeti, 3);
  i = Xc_Select_nomvar(2, 1, testnom, 2);
  i = Xc_Select_typvar(2, 1, testtyp, 4);
  i = Xc_Select_ip1(2, 1, ip1s_range, 3);
//  i = Xc_Select_ip1(2, 1, ip1s_i, 4);
  i = Xc_Select_ip2(2, 1, ip2s, 3);
  i = Xc_Select_date(2, 1, dates_range1, 5);

  i = Xc_Select_ip1(3, 1, ip1s_range2, 2);
  i = Xc_Select_ip3(3, 1, ip3s, 3);
  i = Xc_Select_ip2(3, 1, ip1s_range3, 2);
  i = Xc_Select_date(3, 1, dates_range2, 2);

  i = Xc_Select_ip1(4, -1, ip1s_range3, 2);
  i = Xc_Select_ip2(4, -1, ip1s_range2, 2);
  i = Xc_Select_date(4, -1, dates_range3, 2);
  i = C_select_groupset(2, 5);
  if(argc > 1)
    WriteRequestTable(atoi(argv[1]), NULL);
  else
    WriteRequestTable(0, NULL);

//i=C_fstmatch_parm(handle,  datevalid, ni, nj, nk,       ip1,       ip2,       ip3, typvar, nomvar,      etiket, grtyp, ig1, ig2, ig3, ig4)
  i=C_fstmatch_parm(-1    ,  313280000, 50, 51, 52,       500,        12,         0,   "P",  "TT",  "R2428V4N", "X"  ,  0,  0,  0,  0);
  fprintf(stderr, "match result = %d \n", i);
  i=C_fstmatch_parm(-1    ,  313280000, 50, 51, 52,  41394464,        12,         0,   "P",  "TT",  "R2428V4N", "X"  ,  0,  0,  0,  0);
  fprintf(stderr, "match result = %d \n", i);
  i=C_fstmatch_parm(-1    ,  313290800, 50, 51, 52,       750,       750,     77777,   "P",  "TT",  "R2428V4N", "X"  ,  0,  0,  0,  0);
  fprintf(stderr, "match result = %d \n", i);
  if(argc>narg){
    WriteRequestTable(0, argv[narg]);
    RequetesInit();
    ReadRequestTable(argv[narg]);
    fprintf(stdout, "=========== Reading Back table===========\n");
    WriteRequestTable(1, NULL);
  }
  narg++ ;
  fprintf(stdout, "\n=========================================\n\n");

  RequetesInit();
  i = Xc_Select_ip1(1, 1, ip1s_i, 4);
//  i = Xc_Select_suppl(1, 1, -1, -1, -1, -1, -1, -1, -1, 'Z');
  i = Xc_Select_date(1, 1, dates_range, 3);
  i = C_select_groupset(0, 1);
  if(argc > 1)
    WriteRequestTable(atoi(argv[1]), NULL);
  else
    WriteRequestTable(0, NULL);
  if(argc>narg){
    WriteRequestTable(0, argv[narg]);
    RequetesInit();
    ReadRequestTable(argv[narg]);
    fprintf(stdout, "=========== Reading Back table===========\n");
    WriteRequestTable(1, NULL);
  }
  narg++ ;
//  DumpRequestTable(0, NULL);
//  i = Xc_Select_ip1(0, 1, ip1s_r, 3);
/*  i = Xc_Select_ip1(1, 1, ip1s_range, -2);*/
//  i = Xc_Select_ip1(1, 1, ip1s_r_range, -2);
//  heures=12.0;
//  i = Xc_Select_date(1, 1, dates_range, -2);
/*  heures=0.0;
  i = Xc_Select_date(1, 1, dates_range, -2, heures);*/
 /* j = C_fstmatch_req(1, *handle);*/
//  j = C_fstmatch_req(*handle);

}
