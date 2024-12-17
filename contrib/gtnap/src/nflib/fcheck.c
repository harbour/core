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
