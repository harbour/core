/*
 * LibreOffice C-Wrapper
 * 2024 - Francisco Garcia Collado
 */

#include "officesdk.hxx"

__EXTERN_C

_office_api void officesdk_finish(void);

_office_api sdkres_t officesdk_text_to_pdf(const char_t *src_pathname, const char_t *dest_pathname);

_office_api const char_t *officesdk_error(const sdkres_t code);

_office_api void officesdk_browse_doc(const char_t *pathname, sdkres_t *err);

_office_api uint32_t officesdk_rgb(const uint8_t red, const uint8_t green, const uint8_t blue);

__END_C
