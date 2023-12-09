/* LibreOffice-SDK C-wrapper */

#include "officesdk.hxx"

__EXTERN_C

void officesdk_finish(void);

sdkres_t officesdk_text_to_pdf(const char_t *src_pathname, const char_t *dest_pathname);

const char_t* officesdk_error(const sdkres_t code);

void officesdk_browse_doc(const char_t *pathname, sdkres_t *err);

Sheet *officesdk_sheet_open(const char_t *pathname, sdkres_t *err);

Sheet *officesdk_sheet_create(sdkres_t *err);

void officesdk_sheet_save(Sheet *sheet, const char_t *pathname, sdkres_t *err);

void officesdk_sheet_close(Sheet *sheet, sdkres_t *err);

void officesdk_sheet_cell_text(Sheet *sheet, const uint32_t page, const uint32_t col, const uint32_t row, const char_t *text, sdkres_t *err);

void officesdk_sheet_cell_font_family(Sheet *sheet, const uint32_t page, const uint32_t col, const uint32_t row, const char_t *font_family, sdkres_t *err);

void officesdk_sheet_cell_font_size(Sheet *sheet, const uint32_t page, const uint32_t col, const uint32_t row, const real32_t font_size, sdkres_t *err);

void officesdk_sheet_cell_bold(Sheet *sheet, const uint32_t page, const uint32_t col, const uint32_t row, const bool_t bold, sdkres_t *err);

void officesdk_sheet_cell_italic(Sheet *sheet, const uint32_t page, const uint32_t col, const uint32_t row, const bool_t italic, sdkres_t *err);

void officesdk_sheet_cell_halign(Sheet *sheet, const uint32_t page, const uint32_t col, const uint32_t row, const uint32_t align, sdkres_t *err);

void officesdk_sheet_cell_merge(Sheet *sheet, const uint32_t page, const uint32_t st_col, const uint32_t st_row, const uint32_t ed_col, const uint32_t ed_row, sdkres_t *err);

void officesdk_sheet_column_visible(Sheet *sheet, const uint32_t page, const uint32_t col, const bool_t visible, sdkres_t *err);

void officesdk_sheet_column_optimal_width(Sheet *sheet, const uint32_t page, const uint32_t col, const bool_t optimal_width, sdkres_t *err);

void officesdk_sheet_column_width(Sheet *sheet, const uint32_t page, const uint32_t col, const uint32_t width, sdkres_t *err);

//Sheet *officesdk_sheet(SheetDoc *doc, const uint32_t index, sdkres_t *err);
//
//SheetCell *officesdk_sheet_cell(Sheet *sheet, const uint32_t col, const uint32_t row, sdkres_t *err);
//
//void officesdk_sheet_cell_text(SheetCell *cell, const char_t *text, const char_t *font_family, const real32_t font_size, const bool_t bold, const bool_t italic, sdkres_t *err);

__END_C
