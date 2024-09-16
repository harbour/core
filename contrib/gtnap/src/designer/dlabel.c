/* Design label */

#include "dlabel.h"
#include <core/dbind.h>

/*---------------------------------------------------------------------------*/

DLabel *dlabel_create(void)
{
    return dbind_create(DLabel);
}

void dlabel_destroy(DLabel **label);

void dlabel_text(DLabel *label, const char_t *text);
