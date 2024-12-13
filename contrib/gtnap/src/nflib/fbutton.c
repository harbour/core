/* Form button */

#include "fbutton.h"
#include <core/dbind.h>
#include <core/strings.h>
#include <sewer/cassert.h>

/*---------------------------------------------------------------------------*/

FButton *fbutton_create(void)
{
    return dbind_create(FButton);
}
