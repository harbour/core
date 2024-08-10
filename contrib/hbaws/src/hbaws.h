/*
 * AWS Harbour Wrapper
 * 2024 - Francisco Garcia Collado
 */

#ifndef HB_AWS_H_
#define HB_AWS_H_

#include "hbvmint.h"
#include "hbgtcore.h"

typedef struct _s3_obj_t S3Obj;

HB_EXTERN_BEGIN

extern int hb_aws_init(HB_ITEM *access_key_block, HB_ITEM *secret_block);

extern void hb_aws_finish(void);

extern const char *hb_aws_last_error(void);

extern int hb_aws_s3_list(HB_ITEM *bucket_block, HB_ITEM *prefix_block, const S3Obj **objects, int *size);

extern const char *hb_aws_s3_key(const S3Obj *object);

HB_EXTERN_END

#endif
