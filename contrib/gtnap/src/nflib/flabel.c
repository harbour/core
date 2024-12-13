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
