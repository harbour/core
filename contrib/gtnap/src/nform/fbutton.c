/* Form button */

#include "fbutton.h"
#include <core/dbind.h>
#include <core/strings.h>
#include <sewer/cassert.h>

/*---------------------------------------------------------------------------*/

FButton *fbutton_create(void)
{
    return dbind_create(FButton);
}

/*---------------------------------------------------------------------------*/

void fbutton_text(FButton *button, const char_t *text)
{
    cassert_no_null(button);
    str_upd(&button->text, text);
}
