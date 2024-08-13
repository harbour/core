/*
 * AWS Harbour Wrapper
 * 2024 - Francisco Garcia Collado
 */

#include "hbaws.h"
#include <aws/core/Aws.h>
#include <aws/core/auth/AWSCredentials.h>
#include <aws/s3/S3Client.h>
#include <aws/s3/model/ListBucketsResult.h>
#include <aws/s3/model/ListObjectsV2Request.h>

#include "hbapiitm.h"
#include "hbapistr.h"

struct HBAWS
{
    bool init;
    Aws::SDKOptions *aws_options;
    Aws::S3::S3Client *s3_client;
    Aws::String aws_last_error;
    Aws::Vector<Aws::S3::Model::Object> aws_objects;

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

static Aws::String i_AwsString(HB_ITEM *block)
{
    char *str = i_block_to_utf8(block);
    Aws::String aws_str(str);
    i_destroy_utf8(&str);
    return aws_str;
}

/*---------------------------------------------------------------------------*/

int hb_aws_init(HB_ITEM *access_key_block, HB_ITEM *secret_block)
{
    if (HBAWS_GLOBAL.init == false)
    {
        bool ok = true;
        HBAWS_GLOBAL.aws_options = new Aws::SDKOptions;
        Aws::InitAPI(*HBAWS_GLOBAL.aws_options);

        // Create the s3 client with the credentials
        if (ok == true)
        {
            Aws::String str_access = i_AwsString(access_key_block);
            Aws::String str_secret = i_AwsString(secret_block);
            Aws::Auth::AWSCredentials credentials(str_access, str_secret);
            HBAWS_GLOBAL.s3_client = new Aws::S3::S3Client(credentials);

            if (HBAWS_GLOBAL.s3_client == nullptr)
            {
                HBAWS_GLOBAL.aws_last_error = Aws::String("Error creating Aws::S3Client");
                ok = false;
            }
        }

        // Just check if connection is done
        if (ok == true)
        {
            Aws::S3::Model::ListBucketsOutcome res = HBAWS_GLOBAL.s3_client->ListBuckets();
            if (!res.IsSuccess())
            {
                HBAWS_GLOBAL.aws_last_error = res.GetError().GetMessage();
                ok = false;
            }
        }

        if (ok == true)
        {
            HBAWS_GLOBAL.init = true;
            HBAWS_GLOBAL.aws_last_error = "";
        }
    }

    return HBAWS_GLOBAL.init ? 1 : 0;
}

/*---------------------------------------------------------------------------*/

void hb_aws_finish(void)
{
    if (HBAWS_GLOBAL.init == true)
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

        HBAWS_GLOBAL.init = false;
    }
}

/*---------------------------------------------------------------------------*/

const char *hb_aws_last_error(void)
{
    return HBAWS_GLOBAL.aws_last_error.c_str();
}

/*---------------------------------------------------------------------------*/

const S3Objs *hb_aws_s3_list(HB_ITEM *bucket_block, HB_ITEM *prefix_block)
{
    bool ok = true;
    if (HBAWS_GLOBAL.init == true)
    {
        Aws::String str_bucket = i_AwsString(bucket_block);
        Aws::String str_prefix = i_AwsString(prefix_block);
        Aws::S3::Model::ListObjectsV2Outcome res;

        {
            Aws::S3::Model::ListObjectsV2Request *request = new Aws::S3::Model::ListObjectsV2Request;
            request->SetBucket(str_bucket);
            // TODO: Use prefix
            res = HBAWS_GLOBAL.s3_client->ListObjectsV2(*request);
            delete request;
        }

        if (res.IsSuccess())
        {
            const Aws::S3::Model::ListObjectsV2Result &result = res.GetResult();
            HBAWS_GLOBAL.aws_objects = result.GetContents();
            ok = true;
        }
        else
        {
            HBAWS_GLOBAL.aws_last_error = res.GetError().GetMessage();
            ok = false;
        }
    }
    else
    {
        HBAWS_GLOBAL.aws_last_error = "HBAWS not initialized";
        ok = false;
    }

    if (ok)
        return reinterpret_cast<S3Objs *>(&HBAWS_GLOBAL.aws_objects);
    else
        return NULL;
}

/*---------------------------------------------------------------------------*/

int hb_aws_s3_size(const S3Objs *objs)
{
    const Aws::Vector<Aws::S3::Model::Object> *awsObjs = reinterpret_cast<const Aws::Vector<Aws::S3::Model::Object> *>(objs);
    return static_cast<int>(awsObjs->size());
}

/*---------------------------------------------------------------------------*/

const char *hb_aws_s3_key(const S3Objs *objs, int i)
{
    const Aws::Vector<Aws::S3::Model::Object> *awsObjs = reinterpret_cast<const Aws::Vector<Aws::S3::Model::Object> *>(objs);
    return (*awsObjs)[i].GetKey().c_str();
}
