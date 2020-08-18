#include "../WhiteBoard_c.c"

//! @todo Should these really be macros?  Wouldn't it be better to call functions that the compiler can inline?

#define WB_PUT_C(WB,name,value,strglen,options) c_wb_put(WB,(unsigned char *)name,WB_FORTRAN_CHAR,strglen,(unsigned char *)value,0,options,strlen(name))
#define WB_GET_C(WB,name,value,strglen) c_wb_get(WB,(unsigned char *)name,WB_FORTRAN_CHAR,strglen,(unsigned char *)value,0,strlen(name))
#define WB_PUT_CV(WB,name,value,strglen,size,options) c_wb_put(WB,(unsigned char *)name,WB_FORTRAN_CHAR,strglen,(unsigned char *)value,size,options,strlen(name))
#define WB_GET_CV(WB,name,value,strglen,size) c_wb_get(WB,(unsigned char *)name,WB_FORTRAN_CHAR,strglen,(unsigned char *)value,size,strlen(name))

#define WB_PUT_L1(WB,name,value,options) c_wb_put(WB,(unsigned char *)name,WB_FORTRAN_BOOL,1,(unsigned char *)value,0,options,strlen(name))
#define WB_GET_L1(WB,name,value) c_wb_get(WB,(unsigned char *)name,WB_FORTRAN_BOOL,1,(unsigned char *)value,0,strlen(name))
#define WB_PUT_L1V(WB,name,value,size,options) c_wb_put(WB,(unsigned char *)name,WB_FORTRAN_BOOL,1,(unsigned char *)value,size,options,strlen(name))
#define WB_GET_L1V(WB,name,value,size) c_wb_get(WB,(unsigned char *)name,WB_FORTRAN_BOOL,1,(unsigned char *)value,size,strlen(name))

#define WB_PUT_I4(WB,name,value,options) c_wb_put(WB,(unsigned char *)name,WB_FORTRAN_INT,4,(unsigned char *)value,0,options,strlen(name))
#define WB_GET_I4(WB,name,value) c_wb_get(WB,(unsigned char *)name,WB_FORTRAN_INT,4,(unsigned char *)value,0,strlen(name))
#define WB_PUT_I4V(WB,name,value,size,options) c_wb_put(WB,(unsigned char *)name,WB_FORTRAN_INT,4,(unsigned char *)value,size,options,strlen(name))
#define WB_GET_I4V(WB,name,value,size) c_wb_get(WB,(unsigned char *)name,WB_FORTRAN_INT,4,(unsigned char *)value,size,strlen(name))

#define WB_PUT_I8(WB,name,value,options) c_wb_put(WB,(unsigned char *)name,WB_FORTRAN_INT,8,(unsigned char *)value,0,options,strlen(name))
#define WB_GET_I8(WB,name,value) c_wb_get(WB,(unsigned char *)name,WB_FORTRAN_INT,8,(unsigned char *)value,0,strlen(name))
#define WB_PUT_I8V(WB,name,value,size,options) c_wb_put(WB,(unsigned char *)name,WB_FORTRAN_INT,8,(unsigned char *)value,size,options,strlen(name))
#define WB_GET_I8V(WB,name,value,size) c_wb_get(WB,(unsigned char *)name,WB_FORTRAN_INT,8,(unsigned char *)value,size,strlen(name))

#define WB_PUT_R4(WB,name,value,options) c_wb_put(WB,(unsigned char *)name,WB_FORTRAN_REAL,4,(unsigned char *)value,0,options,strlen(name))
#define WB_GET_R4(WB,name,value) c_wb_get(WB,(unsigned char *)name,WB_FORTRAN_REAL,4,(unsigned char *)value,0,strlen(name))
#define WB_PUT_R4V(WB,name,value,size,options) c_wb_put(WB,(unsigned char *)name,WB_FORTRAN_REAL,4,(unsigned char *)value,size,options,strlen(name))
#define WB_GET_R4V(WB,name,value,size) c_wb_get(WB,(unsigned char *)name,WB_FORTRAN_REAL,4,(unsigned char *)value,size,strlen(name))

#define WB_PUT_R8(WB,name,value,options) c_wb_put(WB,(unsigned char *)name,WB_FORTRAN_REAL,8,(unsigned char *)value,0,options,strlen(name))
#define WB_GET_R8(WB,name,value) c_wb_get(WB,(unsigned char *)name,WB_FORTRAN_REAL,8,(unsigned char *)value,0,strlen(name))
#define WB_PUT_R8V(WB,name,value,size,options) c_wb_put(WB,(unsigned char *)name,WB_FORTRAN_REAL,8,(unsigned char *)value,size,options,strlen(name))
#define WB_GET_R8V(WB,name,value,size) c_wb_get(WB,(unsigned char *)name,WB_FORTRAN_REAL,8,(unsigned char *)value,size,strlen(name))



//! Dummy callback function for c_wb_check
static int Action1(wb_line *line, void *blinddata) {
    fprintf(stderr, "Action1 has been called, key=%s\n", &(line->meta.name.carr));
    return 0;
}


void f77_name(c_wb_test)() {
   int status, myint, myint2;
   long long myll, myll2;
   float myreal, myreal2;
   double mydouble, mydouble2;
   int integer_array[]={1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25};
   int integer_array2[25];
   char *string1="this is string 1";
   char string2[32];
   WhiteBoard *WB=NULL;

   message_level = WB_MSG_INFO;
   status = WB_PUT_C(WB, "string1", string1, strlen(string1), 0 + WB_REWRITE_UNTIL);
   printf("status=%d\n", status);
   myint = 12; myll=1212;
   myreal=12.12; mydouble=2424.68;
   status=WB_PUT_I4(WB,"valeur1",&myint,WB_CREATE_ONLY);
   printf("status=%d\n",status);
   status=WB_PUT_I8(WB,"ll1",&myll,0+WB_READ_ONLY_ON_RESTART);
   printf("status=%d\n",status);
   status=WB_PUT_R4(WB,"reel1",&myreal,0+WB_READ_ONLY_ON_RESTART);
   printf("status=%d\n",status);
   status=WB_PUT_R8(WB,"dble1",&mydouble,0+WB_REWRITE_UNTIL);
   printf("status=%d\n",status);
   myint = -134;myll=-134431;
   myreal=-13.45; mydouble=-12345.6789;
   status=WB_PUT_I4(WB,"valeur2",&myint,0+WB_REWRITE_UNTIL);
   printf("status=%d\n",status);
   status=WB_PUT_I8(WB,"ll2",&myll,0);
   printf("status=%d\n",status);
   status=WB_PUT_R4(WB,"reel2",&myreal,0);
   printf("status=%d\n",status);
   status=WB_PUT_R8(WB,"dble2",&mydouble,0);
   printf("status=%d\n",status);
printf("========\n");
   status=c_wb_check(WB,(unsigned char *)"", -1, 0, 1, NULL,NULL);
   printf("c_wb_check printed %d entries\n",status);
printf("========\n");
   printf("before c_wb_lock\n");
   c_wb_lock(WB,(unsigned char *)"D",1);

   c_wb_checkpoint();

   BaseWhiteboardPtr->firstpage=NULL;
   c_wb_reload();

   status=WB_GET_C(WB,"string1",string2,32);
   string2[31]=0;
   printf("status=%d, string2='%s'\n",status,string2);
   myint2 = -1; myll2=-1;
   myreal2=-1.0; mydouble2=-1.1111111111;
   status=WB_GET_I4(WB,"valeur1",&myint2);
   printf("status=%d, myint2=%d\n",status,myint2);
   status=WB_GET_I8(WB,"ll1",&myll2);
   printf("status=%d, myll2=%lld\n",status,myll2);
   status=WB_GET_R4(WB,"reel1",&myreal2);
   printf("status=%d, myreal2=%lf\n",status,myreal2);
   status=WB_GET_R8(WB,"dble1",&mydouble2);
   printf("status=%d, mydouble2=%f\n",status,mydouble2);
   myint2 = -1; myll2=-1;
   myreal2=-1.0; mydouble2=-1.1111111111;
   status=WB_GET_I4(WB,"valeur2",&myint2);
   printf("status=%d, myint2=%d\n",status,myint2);
   status=WB_GET_I8(WB,"ll2",&myll2);
   printf("status=%d, myll2=%lld\n",status,myll2);
   status=WB_GET_R4(WB,"reel2",&myreal2);
   printf("status=%d, myreal2=%f\n",status,myreal2);
   status=WB_GET_R8(WB,"dble2",&mydouble2);
   printf("status=%d, mydouble2=%lf\n",status,mydouble2);
printf("========\n");
   status=WB_PUT_I4V(WB,"intarray1",integer_array,25,0);
   printf("status=%d\n",status);
printf("========\n");
   status=WB_GET_I4V(WB,"intarray1",integer_array2,5);
   printf("status=%d, integer_array2[4]=%d\n",status,integer_array2[4]);

   c_wb_checkpoint();

   BaseWhiteboardPtr->firstpage=NULL;
   c_wb_reload();

printf("========\n");
   status=WB_PUT_I4V(WB,"intarray1",integer_array,31,0);
   printf("status=%d\n",status);
printf("========\n");
   status=WB_GET_I4V(WB,"intarray1",integer_array2,5);
   printf("status=%d, integer_array2[4]=%d\n",status,integer_array2[4]);
   status=c_wb_check(WB,(unsigned char *)"", -1, 0, 1, NULL,NULL);
   printf("c_wb_check printed %d entries\n",status);
printf("========\n");
   status=WB_PUT_I4V(WB,"intarray1",integer_array,10,0);
   printf("status=%d\n",status);
printf("========\n");
   status=WB_GET_I4V(WB,"intarray1",integer_array2,5);
   printf("status=%d, integer_array2[4]=%d\n",status,integer_array2[4]);
printf("========\n");
   status=WB_GET_I4V(WB,"intarray1",integer_array2,15);
   printf("status=%d, integer_array2[14]=%d\n",status,integer_array2[14]);
printf("==== Locking Test ====\n");
   status=c_wb_check(WB,(unsigned char *)"V", -1, 1, 1, Action1,NULL);
   printf("c_wb_check printed %d entries\n",status);
   c_wb_lock(WB,"V",1);
   status=c_wb_check(WB,(unsigned char *)"V", -1, 1, 1, Action1,NULL);
   printf("c_wb_check printed %d entries\n",status);
printf("==== END of Locking Test ====\n");
}
