/* Design label */

#include "dlabel.h"
#include <core/dbind.h>
#include <core/strings.h>
#include <sewer/cassert.h>

/*---------------------------------------------------------------------------*/

DLabel *dlabel_create(void)
{
    return dbind_create(DLabel);
}

/*---------------------------------------------------------------------------*/

/*void dlabel_destroy(DLabel **label);*/

/*---------------------------------------------------------------------------*/

void dlabel_name(DLabel *label, const char_t *name)
{
    cassert_no_null(label);
    str_upd(&label->name, name);
}

/*---------------------------------------------------------------------------*/

void dlabel_text(DLabel *label, const char_t *text)
{
    cassert_no_null(label);
    str_upd(&label->text, text);
}
