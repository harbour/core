/* HBOffice internal tools for Windows */

#if defined(__WINDOWS__)
#include "hboffice.inl"
#include <sewer/nowarn.hxx>
#include <Windows.h>
#include <sewer/warn.hxx>

/*---------------------------------------------------------------------------*/

DLib *hbdlib_open(const char_t *path, const char_t *libname)
{
    HMODULE lib = NULL;
    char_t pathname[MAX_PATH + 1];
    pathname[0] = '\0';

    if (path != NULL)
    {
        strcat_s(pathname, sizeof(pathname), path);
        strcat_s(pathname, sizeof(pathname), "\\");
    }

    strcat_s(pathname, sizeof(pathname), libname);

    lib = LoadLibraryA(pathname);
    return (DLib *)lib;
}

/*---------------------------------------------------------------------------*/

void hbdlib_close(DLib **dlib)
{
    FreeLibrary((HMODULE)*dlib);
    *dlib = NULL;
}

/*---------------------------------------------------------------------------*/

FPtr_libproc hbdlib_proc_imp(DLib *dlib, const char_t *procname)
{
    FARPROC func = GetProcAddress((HMODULE)dlib, procname);
    return cast_func_ptr(func, FPtr_libproc);
}

/*---------------------------------------------------------------------------*/

void *hbdlib_var_imp(DLib *dlib, const char_t *varname)
{
#if defined(_MSC_VER)
#pragma warning(push, 0)
#pragma warning(disable : 4064)
#endif
#if defined(__GNUC__)
#pragma GCC diagnostic ignored "-Wpedantic"
#endif
    return (void *)GetProcAddress((HMODULE)dlib, varname);
#if defined(_MSC_VER)
#pragma warning(pop)
#endif
#if defined(__GNUC__)
#pragma GCC diagnostic warning "-Wpedantic"
#endif
}

#endif
