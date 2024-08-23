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
#include <aws/s3/model/PutObjectRequest.h>
#include <aws/s3/model/CompletedPart.h>
#include <aws/s3/model/CreateMultipartUploadRequest.h>
#include <aws/s3/model/AbortMultipartUploadRequest.h>
#include <aws/s3/model/CompleteMultipartUploadRequest.h>
#include <aws/s3/model/CopyObjectRequest.h>
#include <aws/s3/model/HeadObjectRequest.h>
#include <aws/s3/model/UploadPartRequest.h>
#include <aws/s3/model/UploadPartCopyRequest.h>
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

#define MIN_AWS_S3_MULTIPART_SIZE 5 * 1024 * 1024
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

static std::ofstream i_open_log()
{
    std::ofstream file;
    file.open("C:\\Users\\Fran\\Desktop\\log.txt", std::ios::app);
    return file;
}

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

static Aws::S3::Model::StorageClass i_storage_class(const s3_storage_class_t storage)
{
    switch (storage)
    {
    case ekSTORAGE_STANDARD:
        return Aws::S3::Model::StorageClass::STANDARD;
    case ekSTORAGE_REDUCED_REDUNDANCY:
        return Aws::S3::Model::StorageClass::REDUCED_REDUNDANCY;
    case ekSTORAGE_STANDARD_IA:
        return Aws::S3::Model::StorageClass::STANDARD_IA;
    case ekSTORAGE_ONEZONE_IA:
        return Aws::S3::Model::StorageClass::ONEZONE_IA;
    case ekSTORAGE_INTELLIGENT_TIERING:
        return Aws::S3::Model::StorageClass::INTELLIGENT_TIERING;
    case ekSTORAGE_GLACIER:
        return Aws::S3::Model::StorageClass::GLACIER;
    case ekSTORAGE_DEEP_ARCHIVE:
        return Aws::S3::Model::StorageClass::DEEP_ARCHIVE;
    case ekSTORAGE_OUTPOSTS:
        return Aws::S3::Model::StorageClass::OUTPOSTS;
    case ekSTORAGE_GLACIER_IR:
        return Aws::S3::Model::StorageClass::GLACIER_IR;
    case ekSTORAGE_SNOW:
        return Aws::S3::Model::StorageClass::SNOW;
    case ekSTORAGE_EXPRESS_ONEZONE:
        return Aws::S3::Model::StorageClass::EXPRESS_ONEZONE;
    }

    return Aws::S3::Model::StorageClass::STANDARD;
}

/*---------------------------------------------------------------------------*/

HB_BOOL hb_aws_s3_upload_simple(HB_ITEM *bucket_block, HB_ITEM *local_file_block, HB_ITEM *remote_key_block, HB_ITEM *content_type_block, const s3_storage_class_t storage)
{
    bool ok = true;
    if (HBAWS_GLOBAL.init == true)
    {
        Aws::String bucket = i_AwsString(bucket_block);
        Aws::String local_file = i_AwsString(local_file_block);
        Aws::String remote_key = i_AwsString(remote_key_block);
        Aws::String content_type = i_AwsString(content_type_block);

        std::shared_ptr<Aws::IOStream> data = Aws::MakeShared<Aws::FStream>("hb_aws_s3_upload_simple", local_file.c_str(), std::ios_base::in | std::ios_base::binary);
        if (!data->fail())
        {
            Aws::S3::Model::PutObjectOutcome res;

            {
                Aws::S3::Model::PutObjectRequest *request = new Aws::S3::Model::PutObjectRequest;
                request->SetBucket(bucket);
                request->SetKey(remote_key);
                request->SetContentType(content_type);
                request->SetStorageClass(i_storage_class(storage));
                request->SetBody(data);
                res = HBAWS_GLOBAL.s3_client->PutObject(*request);
                delete request;
            }

            if (!res.IsSuccess())
            {
                HBAWS_GLOBAL.aws_last_error = res.GetError().GetMessage();
                ok = false;
            }
        }
        else
        {
            HBAWS_GLOBAL.aws_last_error = "Error trying to read '" + local_file + "'";
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

HB_BOOL hb_aws_s3_upload_multipart(HB_ITEM *bucket_block, HB_ITEM *local_file_block, HB_ITEM *remote_key_block, HB_ITEM *content_type_block, const s3_storage_class_t storage, uint32_t chunk_size, uint32_t num_retries)
{
    bool ok = true;
    // std::ofstream log = i_open_log();
    if (HBAWS_GLOBAL.init == true)
    {
        Aws::String bucket = i_AwsString(bucket_block);
        Aws::String local_file = i_AwsString(local_file_block);
        Aws::String remote_key = i_AwsString(remote_key_block);
        Aws::String content_type = i_AwsString(content_type_block);
        std::vector<Aws::S3::Model::CompletedPart> completed_parts;
        Aws::String upload_id;
        bool abort_upload = false;

        if (chunk_size < MIN_AWS_S3_MULTIPART_SIZE)
            chunk_size = MIN_AWS_S3_MULTIPART_SIZE;

        if (num_retries == 0)
            num_retries = 1;

        // Try open the file for read
        std::ifstream file(local_file.c_str(), std::ios_base::in | std::ios_base::binary);
        if (!file.is_open())
        {
            HBAWS_GLOBAL.aws_last_error = "Error trying to read '" + local_file + "'";
            ok = false;
        }

        // Create multipart upload request
        if (ok)
        {
            Aws::S3::Model::CreateMultipartUploadOutcome upload_res;

            {
                Aws::S3::Model::CreateMultipartUploadRequest *upload_request = new Aws::S3::Model::CreateMultipartUploadRequest;
                upload_request->SetBucket(bucket);
                upload_request->SetKey(remote_key);
                upload_request->SetContentType(content_type);
                upload_request->SetStorageClass(i_storage_class(storage));
                upload_res = HBAWS_GLOBAL.s3_client->CreateMultipartUpload(*upload_request);
                delete upload_request;
            }

            if (upload_res.IsSuccess())
            {
                upload_id = upload_res.GetResult().GetUploadId();
            }
            else
            {
                // log << "Fail in 'Aws::S3::Model::CreateMultipartUploadOutcome'" << std::endl;
                HBAWS_GLOBAL.aws_last_error = upload_res.GetError().GetMessage();
                ok = false;
            }
        }

        // We are going to split the file buffer into chunks and upload them
        if (ok)
        {
            int part_number = 1;
            char *file_buffer = new char[chunk_size];

            while (file.good() && ok)
            {
                file.read(file_buffer, chunk_size);
                std::streamsize bytes_read = file.gcount();
                if (bytes_read > 0)
                {
                    Aws::S3::Model::UploadPartOutcome res_part;

                    // Create an uploadable part
                    {
                        Aws::S3::Model::UploadPartRequest *upload_part_request = new Aws::S3::Model::UploadPartRequest;
                        uint32_t retries = 0;
                        upload_part_request->SetBucket(bucket);
                        upload_part_request->SetKey(remote_key);
                        upload_part_request->SetUploadId(upload_id);
                        upload_part_request->SetPartNumber(part_number);

                        std::shared_ptr<Aws::StringStream> stream = Aws::MakeShared<Aws::StringStream>("hb_aws_s3_upload_multipart");
                        stream->write(file_buffer, bytes_read);
                        upload_part_request->SetBody(stream);
                        upload_part_request->SetContentLength(bytes_read);

                        // Number of retries
                        while (retries < num_retries)
                        {
                            res_part = HBAWS_GLOBAL.s3_client->UploadPart(*upload_part_request);
                            if (res_part.IsSuccess())
                                break;
                            else
                                retries += 1;
                        }

                        delete upload_part_request;
                    }

                    if (res_part.IsSuccess())
                    {
                        Aws::S3::Model::CompletedPart completed_part;
                        completed_part.SetPartNumber(part_number);
                        completed_part.SetETag(res_part.GetResult().GetETag());
                        completed_parts.emplace_back(completed_part);
                        // log << "Ok uploaded part '" << part_number << "' of '" << bytes_read << "' bytes" << std::endl;
                    }
                    else
                    {
                        // log << "Fail in 'Aws::S3::Model::UploadPartOutcome'" << std::endl;
                        HBAWS_GLOBAL.aws_last_error = res_part.GetError().GetMessage();
                        ok = false;
                        abort_upload = true;
                    }

                    part_number++;
                }
            }

            delete[] file_buffer;
        }

        // And finally complete the multipart upload operation
        if (ok)
        {
            Aws::S3::Model::CompleteMultipartUploadOutcome complete_res;

            {
                Aws::S3::Model::CompleteMultipartUploadRequest *complete_request = new Aws::S3::Model::CompleteMultipartUploadRequest;
                complete_request->SetBucket(bucket);
                complete_request->SetKey(remote_key);
                complete_request->SetUploadId(upload_id);

                Aws::S3::Model::CompletedMultipartUpload completed_upload;
                completed_upload.SetParts(completed_parts);
                complete_request->SetMultipartUpload(completed_upload);

                complete_res = HBAWS_GLOBAL.s3_client->CompleteMultipartUpload(*complete_request);
                delete complete_request;
            }

            if (!complete_res.IsSuccess())
            {
                // log << "Fail in 'Aws::S3::Model::CompleteMultipartUploadOutcome'" << std::endl;
                HBAWS_GLOBAL.aws_last_error = complete_res.GetError().GetMessage();
                ok = false;
            }
        }

        // At least one part has failed to upload.
        // We abort the operation and free resources in AWS.
        if (abort_upload)
        {
            Aws::S3::Model::AbortMultipartUploadOutcome abort_res;
            // log << "Aborting upload" << std::endl;

            {
                Aws::S3::Model::AbortMultipartUploadRequest *abort_request = new Aws::S3::Model::AbortMultipartUploadRequest;
                abort_request->SetBucket(bucket);
                abort_request->SetKey(remote_key);
                abort_request->SetUploadId(upload_id);
                abort_res = HBAWS_GLOBAL.s3_client->AbortMultipartUpload(*abort_request);
                delete abort_request;
            }

            // At the moment, we don't inform about abort result.
            // In last error we return the cause of upload part fail.
            if (abort_res.IsSuccess())
            {
            }
            else
            {
            }
        }
    }
    else
    {
        HBAWS_GLOBAL.aws_last_error = "HBAWS not initialized";
        ok = false;
    }

    // log.close();
    return static_cast<HB_BOOL>(ok);
}

/*---------------------------------------------------------------------------*/

HB_BOOL hb_aws_s3_copy_simple(HB_ITEM *src_bucket_block, HB_ITEM *src_key_block, HB_ITEM *dest_bucket_block, HB_ITEM *dest_key_block, HB_ITEM *dest_content_type_block, const s3_storage_class_t dest_storage)
{
    bool ok = true;
    if (HBAWS_GLOBAL.init == true)
    {
        Aws::String src_bucket = i_AwsString(src_bucket_block);
        Aws::String src_key = i_AwsString(src_key_block);
        Aws::String dest_bucket = i_AwsString(dest_bucket_block);
        Aws::String dest_key = i_AwsString(dest_key_block);
        Aws::String dest_content_type = i_AwsString(dest_content_type_block);
        Aws::S3::Model::CopyObjectOutcome res;

        {
            Aws::S3::Model::CopyObjectRequest *request = new Aws::S3::Model::CopyObjectRequest;
            request->SetCopySource(src_bucket + "/" + src_key);
            request->SetBucket(dest_bucket);
            request->SetKey(dest_key);
            request->SetContentType(dest_content_type);
            request->SetStorageClass(i_storage_class(dest_storage));
            res = HBAWS_GLOBAL.s3_client->CopyObject(*request);
            delete request;
        }

        if (!res.IsSuccess())
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

HB_BOOL hb_aws_s3_copy_multipart(HB_ITEM *src_bucket_block, HB_ITEM *src_key_block, HB_ITEM *dest_bucket_block, HB_ITEM *dest_key_block, HB_ITEM *dest_content_type_block, const s3_storage_class_t dest_storage, uint32_t chunk_size, uint32_t num_retries)
{
    bool ok = true;
    std::ofstream log = i_open_log();
    if (HBAWS_GLOBAL.init == true)
    {
        Aws::String src_bucket = i_AwsString(src_bucket_block);
        Aws::String src_key = i_AwsString(src_key_block);
        Aws::String dest_bucket = i_AwsString(dest_bucket_block);
        Aws::String dest_key = i_AwsString(dest_key_block);
        Aws::String dest_content_type = i_AwsString(dest_content_type_block);
        std::vector<Aws::S3::Model::CompletedPart> completed_parts;
        Aws::String upload_id;
        uint64_t object_size = 0;
        bool abort_upload = false;

        if (chunk_size < MIN_AWS_S3_MULTIPART_SIZE)
            chunk_size = MIN_AWS_S3_MULTIPART_SIZE;

        if (num_retries == 0)
            num_retries = 1;

        // Get the size of source file
        if (ok)
        {
            Aws::S3::Model::HeadObjectOutcome head_res;

            {
                Aws::S3::Model::HeadObjectRequest *head_request = new Aws::S3::Model::HeadObjectRequest;
                head_request->SetBucket(src_bucket);
                head_request->SetKey(src_key);
                head_res = HBAWS_GLOBAL.s3_client->HeadObject(*head_request);
                delete head_request;
            }

            if (head_res.IsSuccess())
            {
                object_size = head_res.GetResult().GetContentLength();
                log << "Source object size: " << object_size << std::endl;
            }
            else
            {
                log << "Fail in 'Aws::S3::Model::HeadObjectRequest'" << std::endl;
                HBAWS_GLOBAL.aws_last_error = head_res.GetError().GetMessage();
                ok = false;
            }
        }

        // Create multipart upload request (same as UPLOAD_MULTIPART)
        if (ok)
        {
            Aws::S3::Model::CreateMultipartUploadOutcome upload_res;

            {
                Aws::S3::Model::CreateMultipartUploadRequest *upload_request = new Aws::S3::Model::CreateMultipartUploadRequest;
                upload_request->SetBucket(dest_bucket);
                upload_request->SetKey(dest_key);
                upload_request->SetContentType(dest_content_type);
                upload_request->SetStorageClass(i_storage_class(dest_storage));
                upload_res = HBAWS_GLOBAL.s3_client->CreateMultipartUpload(*upload_request);
                delete upload_request;
            }

            if (upload_res.IsSuccess())
            {
                upload_id = upload_res.GetResult().GetUploadId();
            }
            else
            {
                log << "Fail in 'Aws::S3::Model::CreateMultipartUploadOutcome'" << std::endl;
                HBAWS_GLOBAL.aws_last_error = upload_res.GetError().GetMessage();
                ok = false;
            }
        }

        if (ok)
        {
            int part_number = 1;
            uint64_t file_position = 0;

            while (ok && file_position < object_size)
            {
                Aws::S3::Model::UploadPartCopyOutcome res_part;
                uint64_t file_chunk_end = file_position + chunk_size;
                if (file_chunk_end > object_size)
                    file_chunk_end = object_size;

                {
                    Aws::S3::Model::UploadPartCopyRequest *upload_part_request = new Aws::S3::Model::UploadPartCopyRequest;
                    uint32_t retries = 0;
                    upload_part_request->SetBucket(dest_bucket);
                    upload_part_request->SetKey(dest_key);
                    upload_part_request->SetUploadId(upload_id);
                    upload_part_request->SetPartNumber(part_number);
                    upload_part_request->SetCopySource(src_bucket + "/" + src_key);
                    upload_part_request->SetCopySourceRange("bytes=" + std::to_string(file_position) + "-" + std::to_string(file_chunk_end - 1));

                    // Number of retries
                    while (retries < num_retries)
                    {
                        res_part = HBAWS_GLOBAL.s3_client->UploadPartCopy(*upload_part_request);
                        if (res_part.IsSuccess())
                            break;
                        else
                            retries += 1;
                    }

                    delete upload_part_request;
                }

                if (res_part.IsSuccess())
                {
                    Aws::S3::Model::CompletedPart completed_part;
                    completed_part.SetPartNumber(part_number);
                    completed_part.SetETag(res_part.GetResult().GetCopyPartResult().GetETag());
                    completed_parts.emplace_back(completed_part);
                    log << "Ok uploaded part '" << part_number << "' from byte '" << file_position << "' to byte '" << file_chunk_end - 1 << "'" << std::endl;
                }
                else
                {
                    log << "Fail in 'Aws::S3::Model::UploadPartCopyOutcome'" << std::endl;
                    HBAWS_GLOBAL.aws_last_error = res_part.GetError().GetMessage();
                    ok = false;
                    abort_upload = true;
                }

                file_position = file_chunk_end;
                part_number++;
            }
        }

        // And finally complete the multipart upload operation (same as UPLOAD_MULTIPART)
        if (ok)
        {
            Aws::S3::Model::CompleteMultipartUploadOutcome complete_res;

            {
                Aws::S3::Model::CompleteMultipartUploadRequest *complete_request = new Aws::S3::Model::CompleteMultipartUploadRequest;
                complete_request->SetBucket(dest_bucket);
                complete_request->SetKey(dest_key);
                complete_request->SetUploadId(upload_id);

                Aws::S3::Model::CompletedMultipartUpload completed_upload;
                completed_upload.SetParts(completed_parts);
                complete_request->SetMultipartUpload(completed_upload);

                complete_res = HBAWS_GLOBAL.s3_client->CompleteMultipartUpload(*complete_request);
                delete complete_request;
            }

            if (!complete_res.IsSuccess())
            {
                log << "Fail in 'Aws::S3::Model::CompleteMultipartUploadOutcome'" << std::endl;
                HBAWS_GLOBAL.aws_last_error = complete_res.GetError().GetMessage();
                ok = false;
            }
        }

        // At least one part has failed to upload.
        // We abort the operation and free resources in AWS.
        if (abort_upload)
        {
            Aws::S3::Model::AbortMultipartUploadOutcome abort_res;
            log << "Aborting upload" << std::endl;

            {
                Aws::S3::Model::AbortMultipartUploadRequest *abort_request = new Aws::S3::Model::AbortMultipartUploadRequest;
                abort_request->SetBucket(dest_bucket);
                abort_request->SetKey(dest_key);
                abort_request->SetUploadId(upload_id);
                abort_res = HBAWS_GLOBAL.s3_client->AbortMultipartUpload(*abort_request);
                delete abort_request;
            }

            // At the moment, we don't inform about abort result.
            // In last error we return the cause of upload part fail.
            if (abort_res.IsSuccess())
            {
            }
            else
            {
            }
        }
    }
    else
    {
        HBAWS_GLOBAL.aws_last_error = "HBAWS not initialized";
        ok = false;
    }

    log.close();
    return static_cast<HB_BOOL>(ok);
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
