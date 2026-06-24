#include <dlfcn.h>
#include <rmn/DlInterface.h>

#pragma weak register_dl_routines_ = register_dl_routines
#pragma weak register_dl_routines__ = register_dl_routines
void register_dl_routines_(void);
void register_dl_routines__(void);
void register_dl_routines(void) {
    DlRegister(dlopen, dlsym, dlerror, dlclose);
}
