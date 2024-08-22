/*
 * AWS Harbour Wrapper
 * 2024 - Francisco Garcia Collado
 */

#ifndef HB_AWS_H_
#define HB_AWS_H_

#include "hbvmint.h"
#include "hbgtcore.h"

typedef struct _s3_objs_t S3Objs;

typedef enum _S3StorageClass
{
    ekSTORAGE_STANDARD,
    ekSTORAGE_REDUCED_REDUNDANCY,
    ekSTORAGE_STANDARD_IA,
    ekSTORAGE_ONEZONE_IA,
    ekSTORAGE_INTELLIGENT_TIERING,
    ekSTORAGE_GLACIER,
    ekSTORAGE_DEEP_ARCHIVE,
    ekSTORAGE_OUTPOSTS,
    ekSTORAGE_GLACIER_IR,
    ekSTORAGE_SNOW,
    ekSTORAGE_EXPRESS_ONEZONE
} S3StorageClass;

HB_EXTERN_BEGIN

extern HB_BOOL hb_aws_init(HB_ITEM *access_key_block, HB_ITEM *secret_block);

extern void hb_aws_finish(void);

extern const char *hb_aws_last_error(void);

extern const S3Objs *hb_aws_s3_list_all(HB_ITEM *bucket_block, HB_ITEM *prefix_block);

extern const S3Objs *hb_aws_s3_list_page(HB_ITEM *bucket_block, HB_ITEM *prefix_block, HB_ITEM *start_after_block, HB_ITEM *continuation_token_block, int max_keys, const char **next_continuation_token);

extern HB_BOOL hb_aws_s3_upload_simple(HB_ITEM *bucket_block, HB_ITEM *local_file_block, HB_ITEM *remote_key_block, HB_ITEM *content_type_block, const S3StorageClass storage);

extern HB_BOOL hb_aws_s3_download(HB_ITEM *bucket_block, HB_ITEM *key_block, HB_ITEM *local_file_block);

extern int hb_aws_s3_size(const S3Objs *objs);

extern const char *hb_aws_s3_key(const S3Objs *objs, int i);

extern long long hb_aws_s3_content_size(const S3Objs *objs, int i);

extern const char *hb_aws_s3_content_type(const S3Objs *objs, int i);

extern const char *hb_aws_s3_date(const S3Objs *objs, int i);

extern const char *hb_aws_s3_time(const S3Objs *objs, int i);

extern const char *hb_aws_s3_timezone(const S3Objs *objs, int i);

extern const char *hb_aws_s3_storage_class(const S3Objs *objs, int i);

extern HB_BOOL hb_aws_s3_is_restore(const S3Objs *objs, int i);

extern const char *hb_aws_s3_restore_date(const S3Objs *objs, int i);

extern const char *hb_aws_s3_restore_time(const S3Objs *objs, int i);

extern const char *hb_aws_s3_restore_timezone(const S3Objs *objs, int i);

extern const char *hb_aws_s3_checksum_algorithm(const S3Objs *objs, int i);

extern const char *hb_aws_s3_etag(const S3Objs *objs, int i);

HB_EXTERN_END

#endif
