/*
 * AWS Harbour Wrapper
 * 2024 - Francisco Garcia Collado
 */

#include "hbaws.h"

/*---------------------------------------------------------------------------*/

HB_FUNC(HBAWS_INIT)
{
    uint32_t ok = hb_office_init();
    hb_retl(ok);
}
