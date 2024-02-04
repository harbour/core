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

void officesdk_writer_page_header_show(Writer *writer, const bool_t show, sdkres_t *err);

void officesdk_writer_page_header_margins(Writer *writer, const uint32_t left, const uint32_t right, const uint32_t spacing, const uint32_t height, const bool_t dynamic_spacing, const bool_t dynamic_height, sdkres_t *err);

void officesdk_writer_page_footer_show(Writer *writer, const bool_t show, sdkres_t *err);

void officesdk_writer_page_footer_margins(Writer *writer, const uint32_t left, const uint32_t right, const uint32_t spacing, const uint32_t height, const bool_t dynamic_spacing, const bool_t dynamic_height, sdkres_t *err);

void officesdk_writer_page_margins(Writer *writer, const uint32_t left, const uint32_t right, const uint32_t top, const uint32_t bottom, const uint32_t gutter, sdkres_t *err);

void officesdk_writer_font_family(Writer *writer, const textspace_t space, const char_t *font_family, sdkres_t *err);

void officesdk_writer_font_size(Writer *writer, const textspace_t space, const real32_t font_size, sdkres_t *err);

void officesdk_writer_bold(Writer *writer, const textspace_t space, const bool_t bold, sdkres_t *err);

void officesdk_writer_italic(Writer *writer, const textspace_t space, const bool_t italic, sdkres_t *err);

void officesdk_writer_halign(Writer *writer, const textspace_t space, const halign_t align, sdkres_t *err);

void officesdk_writer_lspacing(Writer *writer, const textspace_t space, const uint32_t height, sdkres_t *err);

void officesdk_writer_insert_text(Writer *writer, const textspace_t space, const char_t *text, sdkres_t *err);

void officesdk_writer_insert_image(Writer *writer, const textspace_t space, const char_t *image_path, sdkres_t *err);

void officesdk_writer_new_line(Writer *writer, const textspace_t space, sdkres_t *err);

void officesdk_writer_page_break(Writer *writer, sdkres_t *err);

__END_C
