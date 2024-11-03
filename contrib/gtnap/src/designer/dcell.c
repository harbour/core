/* Design cell (editable parameters) */

#include "dcell.h"
#include <core/strings.h>
#include <sewer/cassert.h>

/*---------------------------------------------------------------------------*/

void dcell_name(DCell *cell, const char_t *name)
{
    cassert_no_null(cell);
    str_upd(&cell->name, name);
}
