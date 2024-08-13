/*
 * AWS Harbour Wrapper
 * 2024 - Francisco Garcia Collado
 */

#include "hbaws.h"
#include "hbapiitm.h"

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

/*---------------------------------------------------------------------------*/

HB_FUNC(HBAWS_S3_LIST_ALL)
{
    HB_ITEM *bucket_block = hb_param(2, HB_IT_STRING | HB_IT_BLOCK);
    HB_ITEM *prefix_block = hb_param(3, HB_IT_STRING | HB_IT_BLOCK);
    const S3Objs *objects = hb_aws_s3_list(bucket_block, prefix_block);
    PHB_ITEM ret = NULL;

    if (objects != NULL)
    {
        int size = hb_aws_s3_size(objects);
        ret = hb_itemArrayNew(size);
        for (int i = 0; i < size; ++i)
        {
            PHB_ITEM item = hb_itemArrayNew(13);
            hb_arraySetC(item, 1, hb_aws_s3_key(objects, i));
            hb_arraySetNI(item, 2, 2);
            hb_arraySetNI(item, 3, 3);
            hb_arraySetNI(item, 4, 4);
            hb_arraySetNI(item, 5, 5);
            hb_arraySetNI(item, 6, 6);
            hb_arraySetNI(item, 7, 7);
            hb_arraySetNI(item, 8, 8);
            hb_arraySetNI(item, 9, 9);
            hb_arraySetNI(item, 10, 10);
            hb_arraySetNI(item, 11, 11);
            hb_arraySetNI(item, 12, 12);
            hb_arraySetNI(item, 13, 13);
            hb_arraySet(ret, i + 1, item);
        }

        // No error
        hb_storc("", 1);
    }
    else
    {
        ret = hb_itemArrayNew(0);
        hb_storc(hb_aws_last_error(), 1);
    }

    hb_itemReturnRelease(ret);
}