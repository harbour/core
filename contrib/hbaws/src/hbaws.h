/*
 * AWS Harbour Wrapper
 * 2024 - Francisco Garcia Collado
 */

#ifndef HB_AWS_H_
#define HB_AWS_H_

#include "hbvmint.h"
#include "hbgtcore.h"

HB_EXTERN_BEGIN

extern int hb_aws_init(HB_ITEM *access_key_block, HB_ITEM *credentials_block);

extern void hb_aws_finish(void);

extern int hb_aws_last_error(void);

extern const char *hb_aws_error_str(const int errcode);

HB_EXTERN_END

#endif
