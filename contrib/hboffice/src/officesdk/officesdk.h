/*
 * LibreOffice C-Wrapper
 * 2024 - Francisco Garcia Collado
 */

#include "officesdk.hxx"

__EXTERN_C

_office_api void officesdk_finish(void);

_office_api const char_t *officesdk_error_str(const sdkres_t code);

_office_api void officesdk_browse_doc(const char_t *pathname, sdkres_t *err);

_office_api uint32_t officesdk_rgb(const uint8_t red, const uint8_t green, const uint8_t blue);

__END_C
