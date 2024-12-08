/* Form label */

#include "flabel.h"
#include <core/dbind.h>
#include <core/strings.h>
#include <sewer/cassert.h>

/*---------------------------------------------------------------------------*/

FLabel *flabel_create(void)
{
    return dbind_create(FLabel);
}

/*---------------------------------------------------------------------------*/

//void flabel_text(FLabel *label, const char_t *text)
//{
//    cassert_no_null(label);
//    str_upd(&label->text, text);
//}
