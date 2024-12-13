/* Form editbox */

#include "fedit.h"
#include <core/dbind.h>
#include <sewer/cassert.h>

/*---------------------------------------------------------------------------*/

FEdit *fedit_create(void)
{
    return dbind_create(FEdit);
}
