/*
 * LibreOffice Harbour Wrapper
 * 2024 - Francisco Garcia Collado
 */

#include <core/core.hxx>

__EXTERN_C

DLib *hbdlib_open(const char_t *path, const char_t *libname);

void hbdlib_close(DLib **dlib);

FPtr_libproc hbdlib_proc_imp(DLib *dlib, const char_t *procname);

void *hbdlib_var_imp(DLib *dlib, const char_t *varname);

__END_C

#define hbdlib_proc(dlib, procname, type) \
    cast_func_ptr(hbdlib_proc_imp(dlib, procname), type)

#define hbdlib_var(dlib, varname, type) \
    (const type *)hbdlib_var_imp(dlib, varname)
