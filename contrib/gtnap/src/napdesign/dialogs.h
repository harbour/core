/* Dialog boxes */

#include "designer.hxx"

void dialog_dbind(void);

String *dialog_form_name(Window *parent, const char_t *name);

FLabel *dialog_new_label(Window *parent, const DSelect *sel);

FButton *dialog_new_button(Window *parent, const DSelect *sel);

FCheck *dialog_new_check(Window *parent, const DSelect *sel);

FEdit *dialog_new_edit(Window *parent, const DSelect *sel);

FLayout *dialog_new_layout(Window *parent, const DSelect *sel);

uint8_t dialog_unsaved_changes(Window *parent);
