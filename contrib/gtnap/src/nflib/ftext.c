/* Form textview */

#include "ftext.h"
#include <core/dbind.h>
#include <sewer/cassert.h>

/*---------------------------------------------------------------------------*/

FText *ftext_create(void)
{
    return dbind_create(FText);
}
