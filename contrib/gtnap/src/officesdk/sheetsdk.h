/* LibreOffice-SDK C-wrapper */
/* Spreadsheet functions */

#include "officesdk.hxx"

__EXTERN_C

Sheet *officesdk_sheet_open(const char_t *pathname, sdkres_t *err);

Sheet *officesdk_sheet_create(sdkres_t *err);

void officesdk_sheet_save(Sheet *sheet, const char_t *pathname, sdkres_t *err);

void officesdk_sheet_pdf(Sheet *sheet, const char_t *pathname, sdkres_t *err);

void officesdk_sheet_print(Sheet *sheet, const char_t *filename, const char_t *printer, const paperorient_t orient, const paperformat_t format, const uint32_t paper_width, const uint32_t paper_height, const uint32_t num_copies, const bool_t collate_copies, const char_t *pages, sdkres_t *err);

void officesdk_sheet_close(Sheet *sheet, sdkres_t *err);

uint32_t officesdk_sheet_add(Sheet *sheet, sdkres_t *err);

void officesdk_sheet_name(Sheet *sheet, const uint32_t page, const char_t *name, sdkres_t *err);

void officesdk_sheet_protect(Sheet *sheet, const uint32_t page, const bool_t protect, const char_t *pass, sdkres_t *err);

void officesdk_sheet_freeze(Sheet *sheet, const uint32_t page, const uint32_t ncols, const uint32_t nrows, sdkres_t *err);

String *officesdk_sheet_cell_ref(Sheet *sheet, const uint32_t page, const uint32_t col, const uint32_t row, sdkres_t *err);

void officesdk_sheet_cell_text(Sheet *sheet, const uint32_t page, const uint32_t col, const uint32_t row, const char_t *text, sdkres_t *err);

void officesdk_sheet_cell_value(Sheet *sheet, const uint32_t page, const uint32_t col, const uint32_t row, const real64_t value, sdkres_t *err);

void officesdk_sheet_cell_date(Sheet *sheet, const uint32_t page, const uint32_t col, const uint32_t row, const uint8_t day, const uint8_t month, const int16_t year, sdkres_t *err);

void officesdk_sheet_cell_formula(Sheet *sheet, const uint32_t page, const uint32_t col, const uint32_t row, const char_t *formula, sdkres_t *err);

void officesdk_sheet_cell_numformat(Sheet *sheet, const uint32_t page, const uint32_t col, const uint32_t row, const numformat_t format, sdkres_t *err);

void officesdk_sheet_cell_font_family(Sheet *sheet, const uint32_t page, const uint32_t col, const uint32_t row, const char_t *font_family, sdkres_t *err);

void officesdk_sheet_cell_font_size(Sheet *sheet, const uint32_t page, const uint32_t col, const uint32_t row, const real32_t font_size, sdkres_t *err);

void officesdk_sheet_cell_bold(Sheet *sheet, const uint32_t page, const uint32_t col, const uint32_t row, const bool_t bold, sdkres_t *err);

void officesdk_sheet_cell_italic(Sheet *sheet, const uint32_t page, const uint32_t col, const uint32_t row, const bool_t italic, sdkres_t *err);

void officesdk_sheet_cell_halign(Sheet *sheet, const uint32_t page, const uint32_t col, const uint32_t row, const halign_t align, sdkres_t *err);

void officesdk_sheet_cell_valign(Sheet *sheet, const uint32_t page, const uint32_t col, const uint32_t row, const valign_t align, sdkres_t *err);

void officesdk_sheet_cell_wrap(Sheet *sheet, const uint32_t page, const uint32_t col, const uint32_t row, const bool_t wrapped, sdkres_t *err);

void officesdk_sheet_cell_color(Sheet *sheet, const uint32_t page, const uint32_t col, const uint32_t row, const uint32_t rgb, sdkres_t *err);

void officesdk_sheet_cell_backcolor(Sheet *sheet, const uint32_t page, const uint32_t col, const uint32_t row, const uint32_t rgb, sdkres_t *err);

void officesdk_sheet_cells_backcolor(Sheet *sheet, const uint32_t page, const uint32_t st_col, const uint32_t st_row, const uint32_t ed_col, const uint32_t ed_row, const uint32_t rgb, sdkres_t *err);

void officesdk_sheet_cell_image(Sheet *sheet, const uint32_t page, const uint32_t col, const uint32_t row, const char_t *image_path, sdkres_t *err);

void officesdk_sheet_cell_border(Sheet *sheet, const uint32_t page, const uint32_t col, const uint32_t row, const linestyle_t style, const uint32_t thickness, const uint32_t rgb, sdkres_t *err);

void officesdk_sheet_cells_border(Sheet *sheet, const uint32_t page, const uint32_t st_col, const uint32_t st_row, const uint32_t ed_col, const uint32_t ed_row, const linestyle_t style, const uint32_t thickness, const uint32_t rgb, sdkres_t *err);

void officesdk_sheet_cells_merge(Sheet *sheet, const uint32_t page, const uint32_t st_col, const uint32_t st_row, const uint32_t ed_col, const uint32_t ed_row, sdkres_t *err);

void officesdk_sheet_column_visible(Sheet *sheet, const uint32_t page, const uint32_t col, const bool_t visible, sdkres_t *err);

void officesdk_sheet_column_optimal_width(Sheet *sheet, const uint32_t page, const uint32_t col, const bool_t optimal_width, sdkres_t *err);

void officesdk_sheet_column_width(Sheet *sheet, const uint32_t page, const uint32_t col, const uint32_t width, sdkres_t *err);

void officesdk_sheet_row_visible(Sheet *sheet, const uint32_t page, const uint32_t row, const bool_t visible, sdkres_t *err);

void officesdk_sheet_row_visible(Sheet *sheet, const uint32_t page, const uint32_t row, const bool_t visible, sdkres_t *err);

void officesdk_sheet_row_optimal_height(Sheet *sheet, const uint32_t page, const uint32_t row, const bool_t optimal_height, sdkres_t *err);

void officesdk_sheet_row_height(Sheet *sheet, const uint32_t page, const uint32_t row, const uint32_t height, sdkres_t *err);

__END_C
