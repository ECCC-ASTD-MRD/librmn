#if defined(NEVER_TO_BE_TRUE_OR_ELSE)
!****P* FMAIN/f_main_to_c_main
! AUTHOR
!   M.Valin Recherche en Prevision Numerique 2015/2016
! SYNOPSIS
  fake fortran main program for use with C programs that call fortran routines that
  might require fortran runtime library initialization

  said main program may also be included in a static (.a) library
  this has been tested with gfortran, Intel fortran, Portland group fortran
 
================================ simple example ================================
cat >mydemo.c <<EOT
#include <stdio.h>
//int UseFmain();
int main(int argc, char**argv)
{
 int i;
// int dummy = UseFmain();
 for(i=0;i<argc;i++) printf("arg %d = '%s'\n",i,argv[i]);
 return(0);
}
EOT

# straight
s.cc -c -Dmain=MY_C_MAIN mydemo.c
s.f90 f_main_to_cmain.F90 mydemo.o
# or with library version of f_main_to_cmain
s.f90 -c f_main_to_cmain.F90
ar rcv libfmain.a f_main_to_cmain.o
s.cc -c -Dmain=MY_C_MAIN mydemo.c
s.f90 -o mydemo mydemo.o -L. -lfmain
./mydemo 1 2 3

================================ example with tcl interpreter ================================
cat >mytcl.c <<EOT
#include <tcl.h>
//int UseFmain();
int main(
    int argc,                   /* Number of command-line arguments. */
    char **argv)                /* Values of command-line arguments. */
{
//    int dummy = UseFmain();
    Tcl_Main(argc, argv, Tcl_AppInit);
    return 0;                   /* Needed only to prevent compiler warning. */
}
int Tcl_AppInit(interp)
    Tcl_Interp *interp;         /* Interpreter for application. */
{
    if (Tcl_Init(interp) == TCL_ERROR) {
        return TCL_ERROR;
    }
    Tcl_SetVar(interp, "tcl_rcFileName", "~/tclshrc.tcl", TCL_GLOBAL_ONLY);
    return TCL_OK;
}
EOT
# Ubuntu 14.04 for the following lines
s.cc -c -Dmain=MY_C_MAIN mytcl.c -I/usr/include/tcl8.5
# then
s.f90  -o mytcl f_main_to_cmain.F90 mytcl.o -L/usr/lib/x86_64-linux-gnu -ltcl8.5
# or
s.f90 -c f_main_to_cmain.F90
ar rcv libfmain.a f_main_to_cmain.o
s.f90 -o mytcl mytcl.o -L. -lfmain -L/usr/lib/x86_64-linux-gnu -ltcl8.5

================================ example for wish interpreter ================================
cat >mywish.c <<EOT
#include <tcl.h>
#include <tk.h>
//int UseFmain();
int main(
    int argc,                   /* Number of command-line arguments. */
    char **argv)                /* Values of command-line arguments. */
{
//    int dummy = UseFmain();
    Tk_Main(argc, argv, Tcl_AppInit);
    return 0;                   /* Needed only to prevent compiler warning. */
}

int Tcl_AppInit(interp)
    Tcl_Interp *interp;         /* Interpreter for application. */
{
    if (Tcl_Init(interp) == TCL_ERROR) {
        return TCL_ERROR;
    }
    if (Tk_Init(interp) == TCL_ERROR) {
        return TCL_ERROR;
    }
    Tcl_StaticPackage(interp, "Tk", Tk_Init, Tk_SafeInit);
    Tcl_SetVar(interp, "tcl_rcFileName", "~/.wishrc", TCL_GLOBAL_ONLY);
    return TCL_OK;
}
EOT
# Ubuntu 14.04 for the following lines
s.cc -c -Dmain=MY_C_MAIN mywish.c -I/usr/include/tcl8.5
# then
s.f90  -o mywish f_main_to_cmain.F90 mywish.o -L/usr/lib/x86_64-linux-gnu -ltcl8.5
# or
s.f90 -c f_main_to_cmain.F90
ar rcv libfmain.a f_main_to_cmain.o
s.f90 -o mywish mywish.o -L. -lfmain -L/usr/lib/x86_64-linux-gnu -ltcl8.5 -ltk8.5

================================ example for python interpreter ================================
cat >mypython.c <<EOT
//int UseFmain();
main(int argc, char** argv)
{
//  int dummy = UseFmain();
  Py_Initialize();
  Py_Main(argc, argv);
  Py_Finalize();
}
EOT
s.cc -c -Dmain=MY_C_MAIN mypython.c
#
# Ubuntu 14.04 for the following lines
s.f90 -o mypython  f_main_to_cmain.F90 mypython.o -lpython2.7
# or 
s.f90 -c f_main_to_cmain.F90
ar rcv libfmain.a f_main_to_cmain.o
s.f90 -o mypython  mypython.o -L. -lfmain -lpython2.7
==================================================================================
!******
#endif
program fcmain
  use ISO_C_BINDING
  implicit none

  integer(C_INT) :: nargs
  integer :: i, length, status
  character(len=4096) :: argument
  character(len=1), dimension(:), pointer :: arg1
  type(C_PTR), dimension(:), pointer :: argv
  type(C_PTR) :: argtab
  interface
    function c_main(nargs,argv) result(status) BIND(C,name='MY_C_MAIN')
    import
    implicit none
    integer, intent(IN), value :: nargs
    type(C_PTR), intent(IN), value :: argv
    integer :: status
    end function c_main
  end interface

  nargs = command_argument_count()
  allocate(argv(0:nargs+1))
  argv = C_NULL_PTR
  do i=0,nargs
    call get_command_argument(i,argument,length,status)
    allocate(arg1(length+1))
    arg1 = transfer(trim(argument)//achar(0),arg1,length+1)
    argv(i) = C_LOC(arg1(1))
  enddo
  argv(nargs+1) = C_NULL_PTR
  argtab = C_LOC(argv(0))
  status = c_main(nargs+1,argtab)
  stop
end
! function from_c() result(dummy) bind(C,name='UseFmain')
!   use ISO_C_BINDING
!   implicit none
!   integer(C_INT) :: dummy
!   dummy = 0
!   return
! end function from_c
