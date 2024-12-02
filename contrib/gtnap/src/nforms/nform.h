/* NAppGUI form */

#include "nforms.hxx"

__EXTERN_C

_nforms_api NForm *nform_create(const byte_t *data, const uint32_t size);

_nforms_api NForm *nform_from_file(const char_t *pathname, ferror_t *error);

_nforms_api void nform_destroy(NForm **form);

_nforms_api Window *nform_window(const NForm *form);

__END_C
