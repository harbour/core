/* LibreOffice-SDK C-wrapper */

#include <core/core.hxx>

typedef enum _sdkres_t
{
    ekSDKRES_OK     = 1,
    ekSDKRES_NOENV
} sdkres_t;


__EXTERN_C

sdkres_t officesdk_text_to_pdf(const char_t *src_pathname, const char_t *dest_pathname);

const char_t* officesdk_error(const sdkres_t code);

__END_C
