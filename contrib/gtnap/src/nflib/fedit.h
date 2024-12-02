/* Form editbox */

#include "nflib.hxx"

__EXTERN_C

_nflib_api FEdit *fedit_create(void);

_nflib_api void fedit_passmode(FEdit *edit, const bool_t passmode);

_nflib_api void fedit_autosel(FEdit *edit, const bool_t autosel);

//_nflib_api void fedit_text_align(FEdit *edit, const halign_t text_align);

__END_C
