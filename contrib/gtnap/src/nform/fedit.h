/* Form editbox */

#include "nform.hxx"

__EXTERN_C

_nform_api FEdit *fedit_create(void);

_nform_api void fedit_passmode(FEdit *edit, const bool_t passmode);

_nform_api void fedit_autosel(FEdit *edit, const bool_t autosel);

//_nform_api void fedit_text_align(FEdit *edit, const halign_t text_align);

__END_C
