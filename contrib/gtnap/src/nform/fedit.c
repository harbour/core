/* Form editbox */

#include "fedit.h"
#include <core/dbind.h>
#include <sewer/cassert.h>

/*---------------------------------------------------------------------------*/

FEdit *fedit_create(void)
{
    return dbind_create(FEdit);
}

/*---------------------------------------------------------------------------*/

void fedit_passmode(FEdit *edit, const bool_t passmode)
{
    cassert_no_null(edit);
    edit->passmode = passmode;
}

/*---------------------------------------------------------------------------*/

void fedit_autosel(FEdit *edit, const bool_t autosel)
{
    cassert_no_null(edit);
    edit->autosel = autosel;
}

/*---------------------------------------------------------------------------*/

void fedit_text_align(FEdit *edit, const halign_t text_align)
{
    cassert_no_null(edit);
    edit->text_align = text_align;
}
