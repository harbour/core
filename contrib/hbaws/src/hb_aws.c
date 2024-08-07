/*
 * AWS Harbour Wrapper
 * 2024 - Francisco Garcia Collado
 */

#include "hbaws.h"

/*---------------------------------------------------------------------------*/

HB_FUNC(HBAWS_INIT)
{
    HB_ITEM *access_key_block = hb_param(1, HB_IT_STRING | HB_IT_BLOCK);
    HB_ITEM *secret_block = hb_param(2, HB_IT_STRING | HB_IT_BLOCK);
    int ret = hb_aws_init(access_key_block, secret_block);
    hb_retl(ret);
}

/*---------------------------------------------------------------------------*/

HB_FUNC(HBAWS_FINISH)
{
    hb_aws_finish();
}

/*---------------------------------------------------------------------------*/

HB_FUNC(HBAWS_LAST_ERROR)
{
    const char *err = hb_aws_last_error();
    hb_retc(err);
}
