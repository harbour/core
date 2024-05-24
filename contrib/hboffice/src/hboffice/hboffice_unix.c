/* HBOffice internal tools for Windows */

#if defined(__UNIX__)

#include "hboffice.h"

#if defined(__LINUX__)
static const char_t *i_LIB_PREFIX = "lib";
static const char_t *i_LIB_SUFIX = ".so";
#elif defined(__MACOS__)
static const char_t *i_LIB_PREFIX = "lib";
static const char_t *i_LIB_SUFIX = ".so";
#else
#error Unknown platform
#endif

#include <dlfcn.h>

/*---------------------------------------------------------------------------*/

DLib *hbdlib_open(const char_t *path, const char_t *libname)
{
    char_t name[512];
    void *lib = NULL;

    if (path != NULL && path[0] != '\0')
    {
        uint32_t n = strlen(path);
        strcpy_s(name, sizeof(name), path);
        if (path[n - 1] != '/')
            strcat_s(name, sizeof(name), "/");
        strcat_s(name, sizeof(name), i_LIB_PREFIX);
        strcat_s(name, sizeof(name), libname);
        strcat_s(name, sizeof(name), i_LIB_SUFIX);
    }
    else
    {
        strcpy_s(name, sizeof(name), i_LIB_PREFIX);
        strcat_s(name, sizeof(name), libname);
        strcat_s(name, sizeof(name), i_LIB_SUFIX);
    }

    lib = dlopen(name, RTLD_LAZY | RTLD_LOCAL);
    return (DLib *)lib;
}

/*---------------------------------------------------------------------------*/

void hbdlib_close(DLib **dlib)
{
    dlclose((void *)*dlib);
    *dlib = NULL;
}

/*---------------------------------------------------------------------------*/

FPtr_libproc hbdlib_proc_imp(DLib *dlib, const char_t *procname)
{
    void *func = NULL;
    FPtr_libproc *proc = NULL;
    func = dlsym((void *)dlib, procname);
    proc = (FPtr_libproc *)(&func);
    return *proc;
}

/*---------------------------------------------------------------------------*/

void *hbdlib_var_imp(DLib *dlib, const char_t *varname)
{
    return (void *)dlsym((void *)dlib, varname);
}

#endif
