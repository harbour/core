/*
 * AWS Harbour Wrapper
 * 2024 - Francisco Garcia Collado
 */

#include "hbaws.h"
#include <aws/core/Aws.h>
#include <aws/core/auth/AWSCredentials.h>
#include <aws/s3/S3Client.h>
#include <aws/s3/model/ListBucketsResult.h>

#include "hbapiitm.h"
#include "hbapistr.h"

struct HBAWS
{
    bool init;
    Aws::SDKOptions *aws_options;
    Aws::S3::S3Client *s3_client;
    Aws::String aws_last_error;

    HBAWS()
        : init(false), aws_options(nullptr), s3_client(nullptr)
    {
    }
};

/*---------------------------------------------------------------------------*/

static HBAWS HBAWS_GLOBAL;

/*---------------------------------------------------------------------------*/

static uint32_t i_remove_utf8_CR(char *utf8)
{
    /* Remove the Carriage Return (CR) character (NAppGUI doesn't like) */
    uint32_t i = 0, j = 0;
    for (; utf8[i] != 0;)
    {
        if (utf8[i] != 13)
        {
            utf8[j] = utf8[i];
            j += 1;
        }

        i += 1;
    }

    utf8[j] = 0;
    return j;
}

/*---------------------------------------------------------------------------*/

static char *i_item_to_utf8_string(HB_ITEM *item)
{
    HB_SIZE s1 = 0, s2 = 0;
    char *str = NULL;
    s1 = hb_itemCopyStrUTF8(item, NULL, (HB_SIZE)UINT32_MAX);
    str = (char *)malloc(s1 + 1);
    s2 = hb_itemCopyStrUTF8(item, str, s1 + 1);
    i_remove_utf8_CR(str);
    (void)(s2);
    return str;
}

/*---------------------------------------------------------------------------*/

static char *i_block_to_utf8(HB_ITEM *item)
{
    char *str = NULL;

    if (HB_ITEM_TYPE(item) == HB_IT_STRING)
    {
        str = i_item_to_utf8_string(item);
    }
    else if (HB_ITEM_TYPE(item) == HB_IT_BLOCK)
    {
        PHB_ITEM ritem = hb_itemDo(item, 0);
        str = i_item_to_utf8_string(ritem);
        hb_itemRelease(ritem);
    }
    else
    {
        str = (char *)malloc(1);
        str[0] = 0;
    }

    return str;
}

/*---------------------------------------------------------------------------*/

static void i_destroy_utf8(char **str)
{
    free((void *)(*str));
    *str = NULL;
}

/*---------------------------------------------------------------------------*/

int hb_aws_init(HB_ITEM *access_key_block, HB_ITEM *secret_block)
{
    int ok = 1;

    if (HBAWS_GLOBAL.init == false)
    {
        HBAWS_GLOBAL.aws_options = new Aws::SDKOptions;
        Aws::InitAPI(*HBAWS_GLOBAL.aws_options);

        // Create the s3 client with the credentials
        if (ok)
        {
            char *access_key = i_block_to_utf8(access_key_block);
            char *secret = i_block_to_utf8(secret_block);
            Aws::String str_access(access_key);
            Aws::String str_secret(secret);
            Aws::Auth::AWSCredentials credentials(str_access, str_secret);
            HBAWS_GLOBAL.s3_client = new Aws::S3::S3Client(credentials);
            i_destroy_utf8(&access_key);
            i_destroy_utf8(&secret);

            if (HBAWS_GLOBAL.s3_client == nullptr)
            {
                HBAWS_GLOBAL.aws_last_error = Aws::String("Error creating Aws::S3Client");
                ok = 0;
            }
        }

        // Just check if connection is done
        if (ok)
        {
            Aws::S3::Model::ListBucketsOutcome res = HBAWS_GLOBAL.s3_client->ListBuckets();
            if (!res.IsSuccess())
            {
                HBAWS_GLOBAL.aws_last_error = res.GetError().GetMessage();
                ok = 0;
            }
        }

        if (ok)
        {
            HBAWS_GLOBAL.init = true;
            HBAWS_GLOBAL.aws_last_error = "";
        }
    }

    return ok;
}

/*---------------------------------------------------------------------------*/

void hb_aws_finish(void)
{
    if (HBAWS_GLOBAL.init == 1)
    {
        if (HBAWS_GLOBAL.s3_client != nullptr)
        {
            delete HBAWS_GLOBAL.s3_client;
            HBAWS_GLOBAL.s3_client = nullptr;
        }

        if (HBAWS_GLOBAL.aws_options != nullptr)
        {
            Aws::ShutdownAPI(*HBAWS_GLOBAL.aws_options);
            delete HBAWS_GLOBAL.aws_options;
            HBAWS_GLOBAL.aws_options = nullptr;
        }

        HBAWS_GLOBAL.init = 0;
    }
}

/*---------------------------------------------------------------------------*/

const char *hb_aws_last_error(void)
{
    return HBAWS_GLOBAL.aws_last_error.c_str();
}
