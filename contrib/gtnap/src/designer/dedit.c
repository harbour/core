/* Design editbox */

#include "dedit.h"
#include <core/dbind.h>
#include <sewer/cassert.h>

/*---------------------------------------------------------------------------*/

DEdit *dedit_create(void)
{
    return dbind_create(DEdit);
}

/*---------------------------------------------------------------------------*/

void dedit_passmode(DEdit *edit, const bool_t passmode)
{
    cassert_no_null(edit);
    edit->passmode = passmode;
}

/*---------------------------------------------------------------------------*/

void dedit_autosel(DEdit *edit, const bool_t autosel)
{
    cassert_no_null(edit);
    edit->autosel = autosel;
}

/*---------------------------------------------------------------------------*/

void dedit_text_align(DEdit *edit, const halign_t text_align)
{
    cassert_no_null(edit);
    edit->text_align = text_align;
}
