/* Design editbox */

#include "designer.hxx"

DEdit *dedit_create(void);

void dedit_passmode(DEdit *edit, const bool_t passmode);

void dedit_autosel(DEdit *edit, const bool_t autosel);

void dedit_text_align(DEdit *edit, const halign_t text_align);
