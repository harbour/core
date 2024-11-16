/* Form checkbox */

#include "fcheck.h"
#include <core/dbind.h>
#include <core/strings.h>
#include <sewer/cassert.h>

/*---------------------------------------------------------------------------*/

FCheck *fcheck_create(void)
{
    return dbind_create(FCheck);
}

/*---------------------------------------------------------------------------*/

void fcheck_text(FCheck *check, const char_t *text)
{
    cassert_no_null(check);
    str_upd(&check->text, text);
}
