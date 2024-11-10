/* Design checkbox */

#include "dcheck.h"
#include <core/dbind.h>
#include <core/strings.h>
#include <sewer/cassert.h>

/*---------------------------------------------------------------------------*/

DCheck *dcheck_create(void)
{
    return dbind_create(DCheck);
}

/*---------------------------------------------------------------------------*/

void dcheck_text(DCheck *check, const char_t *text)
{
    cassert_no_null(check);
    str_upd(&check->text, text);
}
