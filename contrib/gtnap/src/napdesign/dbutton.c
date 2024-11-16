/* Design button */

#include "dbutton.h"
#include <core/dbind.h>
#include <core/strings.h>
#include <sewer/cassert.h>

/*---------------------------------------------------------------------------*/

DButton *dbutton_create(void)
{
    return dbind_create(DButton);
}

/*---------------------------------------------------------------------------*/

void dbutton_text(DButton *button, const char_t *text)
{
    cassert_no_null(button);
    str_upd(&button->text, text);
}
