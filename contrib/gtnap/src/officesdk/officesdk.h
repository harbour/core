/* LibreOffice-SDK C-wrapper */
/* Global functions */

#include "officesdk.hxx"

__EXTERN_C

void officesdk_finish(void);

sdkres_t officesdk_text_to_pdf(const char_t *src_pathname, const char_t *dest_pathname);

const char_t *officesdk_error(const sdkres_t code);

void officesdk_browse_doc(const char_t *pathname, sdkres_t *err);

uint32_t officesdk_rgb(const uint8_t red, const uint8_t green, const uint8_t blue);

__END_C
