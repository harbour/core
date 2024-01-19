/* LibreOffice-SDK C-wrapper */
/* Text document functions */

#include "officesdk.hxx"

__EXTERN_C

Writer *officesdk_writer_open(const char_t *pathname, sdkres_t *err);

Writer *officesdk_writer_create(sdkres_t *err);

void officesdk_writer_save(Writer *writer, const char_t *pathname, sdkres_t *err);

void officesdk_writer_pdf(Writer *writer, const char_t *pathname, sdkres_t *err);

void officesdk_writer_print(Writer *writer, const char_t *filename, const char_t *printer, const paperorient_t orient, const paperformat_t format, const uint32_t paper_width, const uint32_t paper_height, const uint32_t num_copies, const bool_t collate_copies, const char_t *pages, sdkres_t *err);

void officesdk_writer_close(Writer *writer, sdkres_t *err);

void officesdk_writer_insert(Writer *writer, const char_t *text, sdkres_t *err);

__END_C
