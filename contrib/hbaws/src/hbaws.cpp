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
#include <aws/s3/model/GetObjectRequest.h>
#include <fstream>
#include <ios>

#include "hbapiitm.h"
#include "hbapistr.h"
#include "hbdate.h"
#include "hbset.h"

struct HBAWS
{
    bool init;
    Aws::SDKOptions *aws_options;
    Aws::S3::S3Client *s3_client;
    Aws::String aws_last_error;
    Aws::String aws_temp_conv;
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
    if (item && HB_ITEM_TYPE(item) == HB_IT_STRING)
    {
        str = i_item_to_utf8_string(item);
    }
    else if (item && HB_ITEM_TYPE(item) == HB_IT_BLOCK)
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

HB_BOOL hb_aws_init(HB_ITEM *access_key_block, HB_ITEM *secret_block)
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

    return static_cast<HB_BOOL>(HBAWS_GLOBAL.init);
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

static Aws::S3::Model::ListObjectsV2Outcome i_list_request(const Aws::String &bucket, const Aws::String &prefix, const Aws::String &continuation_token, const Aws::String &start_after, int max_keys)
{
    Aws::S3::Model::ListObjectsV2Request *request = new Aws::S3::Model::ListObjectsV2Request;
    request->SetBucket(bucket);
    request->SetPrefix(prefix);

    if (!continuation_token.empty())
        request->SetContinuationToken(continuation_token);

    if (!start_after.empty())
        request->SetStartAfter(start_after);

    request->SetMaxKeys(std::min(max_keys, 1000));

    Aws::S3::Model::ListObjectsV2Outcome res = HBAWS_GLOBAL.s3_client->ListObjectsV2(*request);
    delete request;
    return res;
}

/*---------------------------------------------------------------------------*/

const S3Objs *hb_aws_s3_list_all(HB_ITEM *bucket_block, HB_ITEM *prefix_block)
{
    bool ok = true;
    if (HBAWS_GLOBAL.init == true)
    {
        Aws::String bucket = i_AwsString(bucket_block);
        Aws::String prefix = i_AwsString(prefix_block);
        Aws::String continuation_token = "";
        bool is_truncated = true;
        HBAWS_GLOBAL.aws_objects.clear();

        while (is_truncated && ok)
        {
            Aws::S3::Model::ListObjectsV2Outcome res = i_list_request(bucket, prefix, continuation_token, "", 1000);

            if (res.IsSuccess())
            {
                const Aws::S3::Model::ListObjectsV2Result &result = res.GetResult();
                const Aws::Vector<Aws::S3::Model::Object> &objs = result.GetContents();
                HBAWS_GLOBAL.aws_objects.insert(HBAWS_GLOBAL.aws_objects.end(), objs.begin(), objs.end());
                continuation_token = result.GetNextContinuationToken();
                is_truncated = result.GetIsTruncated();
            }
            else
            {
                HBAWS_GLOBAL.aws_last_error = res.GetError().GetMessage();
                ok = false;
            }
        }
    }
    else
    {
        HBAWS_GLOBAL.aws_last_error = "HBAWS not initialized";
        ok = false;
    }

    if (ok)
    {
        return reinterpret_cast<S3Objs *>(&HBAWS_GLOBAL.aws_objects);
    }
    else
    {
        HBAWS_GLOBAL.aws_objects.clear();
        return NULL;
    }
}

/*---------------------------------------------------------------------------*/

// static std::ofstream i_open_log()
// {
//     std::ofstream file;
//     file.open("C:\\Users\\Fran\\Desktop\\log.txt", std::ios::app);
//     return file;
// }

/*---------------------------------------------------------------------------*/

const S3Objs *hb_aws_s3_list_page(HB_ITEM *bucket_block, HB_ITEM *prefix_block, HB_ITEM *start_after_block, HB_ITEM *continuation_token_block, int max_keys, const char **next_continuation_token)
{
    bool ok = true;
    if (HBAWS_GLOBAL.init == true)
    {
        Aws::String bucket = i_AwsString(bucket_block);
        Aws::String prefix = i_AwsString(prefix_block);
        Aws::String start_after = i_AwsString(start_after_block);
        Aws::String continuation_token = i_AwsString(continuation_token_block);
        HBAWS_GLOBAL.aws_objects.clear();

        Aws::S3::Model::ListObjectsV2Outcome res = i_list_request(bucket, prefix, continuation_token, start_after, max_keys);

        if (res.IsSuccess())
        {
            const Aws::S3::Model::ListObjectsV2Result &result = res.GetResult();
            const Aws::Vector<Aws::S3::Model::Object> &objs = result.GetContents();
            HBAWS_GLOBAL.aws_objects.insert(HBAWS_GLOBAL.aws_objects.end(), objs.begin(), objs.end());
            if (result.GetIsTruncated())
                HBAWS_GLOBAL.aws_temp_conv = result.GetNextContinuationToken();
            else
                HBAWS_GLOBAL.aws_temp_conv = "";
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
    {
        *next_continuation_token = HBAWS_GLOBAL.aws_temp_conv.c_str();
        return reinterpret_cast<S3Objs *>(&HBAWS_GLOBAL.aws_objects);
    }
    else
    {
        *next_continuation_token = "";
        HBAWS_GLOBAL.aws_objects.clear();
        return NULL;
    }
}

/*---------------------------------------------------------------------------*/

HB_BOOL hb_aws_s3_download(HB_ITEM *bucket_block, HB_ITEM *key_block, HB_ITEM *local_file_block)
{
    bool ok = true;
    if (HBAWS_GLOBAL.init == true)
    {
        Aws::String bucket = i_AwsString(bucket_block);
        Aws::String key = i_AwsString(key_block);
        Aws::String local_file = i_AwsString(local_file_block);
        Aws::S3::Model::GetObjectOutcome res;

        {
            Aws::S3::Model::GetObjectRequest *request = new Aws::S3::Model::GetObjectRequest;
            request->SetBucket(bucket);
            request->SetKey(key);
            res = HBAWS_GLOBAL.s3_client->GetObject(*request);
            delete request;
        }

        if (res.IsSuccess())
        {
            Aws::OFStream file;
            file.open(local_file.c_str(), std::ios::out | std::ios::binary);
            if (file)
            {
                const Aws::S3::Model::GetObjectResult &objRes = res.GetResult();
                file << objRes.GetBody().rdbuf();
                file.close();
            }
            else
            {
                HBAWS_GLOBAL.aws_last_error = "Error creating '" + local_file + "' file.";
                ok = false;
            }
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

    return static_cast<HB_BOOL>(ok);
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

/*---------------------------------------------------------------------------*/

long long hb_aws_s3_content_size(const S3Objs *objs, int i)
{
    const Aws::Vector<Aws::S3::Model::Object> *awsObjs = reinterpret_cast<const Aws::Vector<Aws::S3::Model::Object> *>(objs);
    return (*awsObjs)[i].GetSize();
}

/*---------------------------------------------------------------------------*/

const char *hb_aws_s3_content_type(const S3Objs *objs, int i)
{
    const Aws::Vector<Aws::S3::Model::Object> *awsObjs = reinterpret_cast<const Aws::Vector<Aws::S3::Model::Object> *>(objs);
    const Aws::String &key = (*awsObjs)[i].GetKey();
    size_t p = key.rfind('.', key.length());
    if (p != Aws::String::npos)
        return key.c_str() + p + 1;
    else
        return "";
}

/*---------------------------------------------------------------------------*/

static Aws::String i_date(const Aws::Utils::DateTime &dateTime)
{
    char date[32];
    int y = dateTime.GetYear(true);
    int m = static_cast<int>(dateTime.GetMonth(true)) + 1;
    int d = dateTime.GetDay(true);
    hb_dateStrPut(date, y, m, d);
    return Aws::String(date);
}

/*---------------------------------------------------------------------------*/

static Aws::String i_time(const Aws::Utils::DateTime &dateTime)
{
    return dateTime.ToLocalTimeString("%H:%M:%S");
}

/*---------------------------------------------------------------------------*/

static Aws::String i_timezone(const Aws::Utils::DateTime &dateTime)
{
    return dateTime.ToLocalTimeString("%Z");
}

/*---------------------------------------------------------------------------*/

const char *
hb_aws_s3_date(const S3Objs *objs, int i)
{
    const Aws::Vector<Aws::S3::Model::Object> *awsObjs = reinterpret_cast<const Aws::Vector<Aws::S3::Model::Object> *>(objs);
    const Aws::Utils::DateTime &dateTime = (*awsObjs)[i].GetLastModified();
    HBAWS_GLOBAL.aws_temp_conv = i_date(dateTime);
    return HBAWS_GLOBAL.aws_temp_conv.c_str();
}

/*---------------------------------------------------------------------------*/

const char *hb_aws_s3_time(const S3Objs *objs, int i)
{
    const Aws::Vector<Aws::S3::Model::Object> *awsObjs = reinterpret_cast<const Aws::Vector<Aws::S3::Model::Object> *>(objs);
    const Aws::Utils::DateTime &dateTime = (*awsObjs)[i].GetLastModified();
    HBAWS_GLOBAL.aws_temp_conv = i_time(dateTime);
    return HBAWS_GLOBAL.aws_temp_conv.c_str();
}

/*---------------------------------------------------------------------------*/

const char *hb_aws_s3_timezone(const S3Objs *objs, int i)
{
    const Aws::Vector<Aws::S3::Model::Object> *awsObjs = reinterpret_cast<const Aws::Vector<Aws::S3::Model::Object> *>(objs);
    const Aws::Utils::DateTime &dateTime = (*awsObjs)[i].GetLastModified();
    HBAWS_GLOBAL.aws_temp_conv = i_timezone(dateTime);
    return HBAWS_GLOBAL.aws_temp_conv.c_str();
}

/*---------------------------------------------------------------------------*/

const char *hb_aws_s3_storage_class(const S3Objs *objs, int i)
{
    const Aws::Vector<Aws::S3::Model::Object> *awsObjs = reinterpret_cast<const Aws::Vector<Aws::S3::Model::Object> *>(objs);
    const Aws::S3::Model::ObjectStorageClass &storage = (*awsObjs)[i].GetStorageClass();
    HBAWS_GLOBAL.aws_temp_conv = Aws::S3::Model::ObjectStorageClassMapper::GetNameForObjectStorageClass(storage);
    return HBAWS_GLOBAL.aws_temp_conv.c_str();
}

/*---------------------------------------------------------------------------*/

HB_BOOL hb_aws_s3_is_restore(const S3Objs *objs, int i)
{
    const Aws::Vector<Aws::S3::Model::Object> *awsObjs = reinterpret_cast<const Aws::Vector<Aws::S3::Model::Object> *>(objs);
    const Aws::S3::Model::RestoreStatus &restore = (*awsObjs)[i].GetRestoreStatus();
    return static_cast<HB_BOOL>(restore.GetIsRestoreInProgress());
}

/*---------------------------------------------------------------------------*/

const char *hb_aws_s3_restore_date(const S3Objs *objs, int i)
{
    const Aws::Vector<Aws::S3::Model::Object> *awsObjs = reinterpret_cast<const Aws::Vector<Aws::S3::Model::Object> *>(objs);
    const Aws::S3::Model::RestoreStatus &restore = (*awsObjs)[i].GetRestoreStatus();
    if (restore.RestoreExpiryDateHasBeenSet())
    {
        const Aws::Utils::DateTime &dateTime = restore.GetRestoreExpiryDate();
        HBAWS_GLOBAL.aws_temp_conv = i_date(dateTime);
    }
    else
    {
        HBAWS_GLOBAL.aws_temp_conv = "";
    }

    return HBAWS_GLOBAL.aws_temp_conv.c_str();
}

/*---------------------------------------------------------------------------*/

const char *hb_aws_s3_restore_time(const S3Objs *objs, int i)
{
    const Aws::Vector<Aws::S3::Model::Object> *awsObjs = reinterpret_cast<const Aws::Vector<Aws::S3::Model::Object> *>(objs);
    const Aws::S3::Model::RestoreStatus &restore = (*awsObjs)[i].GetRestoreStatus();
    if (restore.RestoreExpiryDateHasBeenSet())
    {
        const Aws::Utils::DateTime &dateTime = restore.GetRestoreExpiryDate();
        HBAWS_GLOBAL.aws_temp_conv = i_time(dateTime);
    }
    else
    {
        HBAWS_GLOBAL.aws_temp_conv = "";
    }

    return HBAWS_GLOBAL.aws_temp_conv.c_str();
}

/*---------------------------------------------------------------------------*/

const char *hb_aws_s3_restore_timezone(const S3Objs *objs, int i)
{
    const Aws::Vector<Aws::S3::Model::Object> *awsObjs = reinterpret_cast<const Aws::Vector<Aws::S3::Model::Object> *>(objs);
    const Aws::S3::Model::RestoreStatus &restore = (*awsObjs)[i].GetRestoreStatus();
    if (restore.RestoreExpiryDateHasBeenSet())
    {
        const Aws::Utils::DateTime &dateTime = restore.GetRestoreExpiryDate();
        HBAWS_GLOBAL.aws_temp_conv = i_timezone(dateTime);
    }
    else
    {
        HBAWS_GLOBAL.aws_temp_conv = "";
    }

    return HBAWS_GLOBAL.aws_temp_conv.c_str();
}

/*---------------------------------------------------------------------------*/

const char *hb_aws_s3_checksum_algorithm(const S3Objs *objs, int i)
{
    const Aws::Vector<Aws::S3::Model::Object> *awsObjs = reinterpret_cast<const Aws::Vector<Aws::S3::Model::Object> *>(objs);
    const Aws::Vector<Aws::S3::Model::ChecksumAlgorithm> &algs = (*awsObjs)[i].GetChecksumAlgorithm();
    Aws::S3::Model::ChecksumAlgorithm alg = Aws::S3::Model::ChecksumAlgorithm::NOT_SET;
    if (algs.size() > 0)
        alg = algs[0];
    HBAWS_GLOBAL.aws_temp_conv = Aws::S3::Model::ChecksumAlgorithmMapper::GetNameForChecksumAlgorithm(alg);
    return HBAWS_GLOBAL.aws_temp_conv.c_str();
}

/*---------------------------------------------------------------------------*/

const char *hb_aws_s3_etag(const S3Objs *objs, int i)
{
    const Aws::Vector<Aws::S3::Model::Object> *awsObjs = reinterpret_cast<const Aws::Vector<Aws::S3::Model::Object> *>(objs);
    HBAWS_GLOBAL.aws_temp_conv = (*awsObjs)[i].GetETag();
    return HBAWS_GLOBAL.aws_temp_conv.c_str();
}

#if defined(_MSC_VER)

#define stdin (__acrt_iob_func(0))
#define stdout (__acrt_iob_func(1))
#define stderr (__acrt_iob_func(2))
FILE _iob[3];
extern "C" FILE *__cdecl __iob_func(void)
{
    _iob[0] = *stdin;
    _iob[1] = *stdout;
    _iob[2] = *stderr;
    return _iob;
}

#endif
