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

typedef struct _hbaws_t HBAWS;

struct _hbaws_t
{
    int init;
    Aws::SDKOptions *aws_options;
    // textspace_t text_space;
    // sdkres_t last_error;
    // char_t last_cell_ref[i_CELL_REF_SIZE];
};

/*---------------------------------------------------------------------------*/

static HBAWS HBAWS_GLOBAL = {0, nullptr};

/*---------------------------------------------------------------------------*/

int hb_aws_init(HB_ITEM *access_key_block, HB_ITEM *credentials_block)
{
    (void)(access_key_block);
    (void)(credentials_block);
    if (HBAWS_GLOBAL.init == 0)
    {
        HBAWS_GLOBAL.aws_options = new Aws::SDKOptions;
        Aws::InitAPI(*HBAWS_GLOBAL.aws_options);
        HBAWS_GLOBAL.init = 1;
    }

    return HBAWS_GLOBAL.init;
}

/*---------------------------------------------------------------------------*/

void hb_aws_finish(void)
{
    if (HBAWS_GLOBAL.init == 1)
    {
        if (HBAWS_GLOBAL.aws_options != nullptr)
        {
            Aws::ShutdownAPI(*HBAWS_GLOBAL.aws_options);
            delete HBAWS_GLOBAL.aws_options;
            HBAWS_GLOBAL.aws_options = nullptr;
        }

        HBAWS_GLOBAL.init = 0;
    }
}
