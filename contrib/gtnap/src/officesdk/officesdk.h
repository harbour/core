/* LibreOffice-SDK C-wrapper */

#include "officesdk.hxx"

__EXTERN_C

void officesdk_finish(void);

sdkres_t officesdk_text_to_pdf(const char_t *src_pathname, const char_t *dest_pathname);

const char_t* officesdk_error(const sdkres_t code);


SheetDoc *officesdk_sheetdoc_open(const char_t *pathname, sdkres_t *err);

SheetDoc *officesdk_sheetdoc_new(sdkres_t *err);

void officesdk_sheetdoc_save(SheetDoc *doc, const char_t *pathname, sdkres_t *err);

void officesdk_sheetdoc_close(SheetDoc *doc, sdkres_t *err);

void officesdk_sheetdoc_cell_text(SheetDoc *doc, const uint32_t sheet_id, const uint32_t col, const uint32_t row, const char_t *text, const char_t *font_family, const real32_t font_size, const bool_t bold, const bool_t italic, sdkres_t *err);




//Sheet *officesdk_sheet(SheetDoc *doc, const uint32_t index, sdkres_t *err);
//
//SheetCell *officesdk_sheet_cell(Sheet *sheet, const uint32_t col, const uint32_t row, sdkres_t *err);
//
//void officesdk_sheet_cell_text(SheetCell *cell, const char_t *text, const char_t *font_family, const real32_t font_size, const bool_t bold, const bool_t italic, sdkres_t *err);

__END_C
