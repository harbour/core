/*
 * AWS Harbour Wrapper
 * 2024 - Francisco Garcia Collado
 */

#include "hbaws.h"
#include "hbaws.ch"
#include "hbapiitm.h"
#include "hbset.h"

/*---------------------------------------------------------------------------*/

HB_FUNC(HBAWS_INIT)
{
    HB_ITEM *access_key_block = hb_param(2, HB_IT_STRING | HB_IT_BLOCK);
    HB_ITEM *secret_block = hb_param(3, HB_IT_STRING | HB_IT_BLOCK);
    HB_BOOL ret = hb_aws_init(access_key_block, secret_block);

    if (ret == HB_TRUE)
        hb_storc("", 1);
    else
        hb_storc(hb_aws_last_error(), 1);

    hb_retl(ret);
}

/*---------------------------------------------------------------------------*/

HB_FUNC(HBAWS_FINISH)
{
    hb_aws_finish();
}

/*---------------------------------------------------------------------------*/

HB_FUNC(HBAWS_S3_LIST_ALL)
{
    HB_ITEM *bucket_block = hb_param(2, HB_IT_STRING | HB_IT_BLOCK);
    HB_ITEM *prefix_block = hb_param(3, HB_IT_STRING | HB_IT_BLOCK);
    const S3Objs *objects = hb_aws_s3_list_all(bucket_block, prefix_block);
    PHB_ITEM ret = NULL;

    if (objects != NULL)
    {
        int size = hb_aws_s3_size(objects);
        ret = hb_itemArrayNew(size);
        for (int i = 0; i < size; ++i)
        {
            PHB_ITEM item = hb_itemArrayNew(13);
            hb_arraySetC(item, OBJ_S3KEY, hb_aws_s3_key(objects, i));
            hb_arraySetNL(item, OBJ_CONTENT_SIZE, hb_aws_s3_content_size(objects, i));
            hb_arraySetC(item, OBJ_CONTENT_TYPE, hb_aws_s3_content_type(objects, i));
            hb_arraySetDS(item, OBJ_DATE, hb_aws_s3_date(objects, i));
            hb_arraySetC(item, OBJ_TIME, hb_aws_s3_time(objects, i));
            hb_arraySetC(item, OBJ_TIMEZONE, hb_aws_s3_timezone(objects, i));
            hb_arraySetC(item, OBJ_STORAGE_CLASS, hb_aws_s3_storage_class(objects, i));
            hb_arraySetL(item, OBJ_IS_RESTORE, hb_aws_s3_is_restore(objects, i));
            hb_arraySetDS(item, OBJ_RESTORE_DATE, hb_aws_s3_restore_date(objects, i));
            hb_arraySetC(item, OBJ_RESTORE_TIME, hb_aws_s3_restore_time(objects, i));
            hb_arraySetC(item, OBJ_RESTORE_TIMEZONE, hb_aws_s3_restore_timezone(objects, i));
            hb_arraySetC(item, OBJ_CHECKSUM_ALGORITHM, hb_aws_s3_checksum_algorithm(objects, i));
            hb_arraySetC(item, OBJ_ETAG, hb_aws_s3_etag(objects, i));
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