/*
 * LibreOffice Harbour Wrapper
 * 2024 - Francisco Garcia Collado
 */

#ifndef HB_OFFICE_H_
#define HB_OFFICE_H_

#include "hbvmint.h"
#include "hbgtcore.h"
#include <officesdk/officesdk.hxx>

HB_EXTERN_BEGIN

extern bool_t hb_office_init(void);

extern void hb_office_finish(void);

extern uint32_t hb_office_last_error(void);

extern const char_t *hb_office_error_str(const uint32_t errcode);

extern void hb_office_browse_doc(HB_ITEM *pathname_block);

extern uint32_t hb_office_rgb(const uint8_t red, const uint8_t green, const uint8_t blue);

extern Sheet *hb_office_sheet_open(HB_ITEM *pathname_block);

extern Sheet *hb_office_sheet_create(void);

extern void hb_office_sheet_save(Sheet *sheet, HB_ITEM *pathname_block);

extern void hb_office_sheet_pdf(Sheet *sheet, HB_ITEM *pathname_block);

extern void hb_office_sheet_print(Sheet *sheet, HB_ITEM *filename_block, HB_ITEM *printer_block, const paperorient_t orient, const paperformat_t format, const uint32_t paper_width, const uint32_t paper_height, const uint32_t num_copies, const bool_t collate_copies, HB_ITEM *pages_block);

extern void hb_office_sheet_close(Sheet *sheet);

extern uint32_t hb_office_sheet_add(Sheet *sheet);

extern void hb_office_sheet_name(Sheet *sheet, const uint32_t page, HB_ITEM *name_block);

extern void hb_office_sheet_protect(Sheet *sheet, const uint32_t page, const bool_t protect, HB_ITEM *pass_block);

extern void hb_office_sheet_freeze(Sheet *sheet, const uint32_t page, const uint32_t ncols, const uint32_t nrows);

extern const char_t *hb_office_cell_ref(Sheet *sheet, const uint32_t page, const uint32_t col, const uint32_t row);

extern void hb_office_sheet_cell_text(Sheet *sheet, const uint32_t page, const uint32_t col, const uint32_t row, HB_ITEM *text_block);

extern void hb_office_sheet_cell_value(Sheet *sheet, const uint32_t page, const uint32_t col, const uint32_t row, const real64_t value);

extern void hb_office_sheet_cell_date(Sheet *sheet, const uint32_t page, const uint32_t col, const uint32_t row, const uint8_t day, const uint8_t month, const int16_t year);

extern void hb_office_sheet_cell_formula(Sheet *sheet, const uint32_t page, const uint32_t col, const uint32_t row, HB_ITEM *formula_block);

extern void hb_office_sheet_cell_numformat(Sheet *sheet, const uint32_t page, const uint32_t col, const uint32_t row, const numformat_t format);

extern void hb_office_sheet_cell_font_family(Sheet *sheet, const uint32_t page, const uint32_t col, const uint32_t row, HB_ITEM *ffamily_block);

extern void hb_office_sheet_cell_font_size(Sheet *sheet, const uint32_t page, const uint32_t col, const uint32_t row, const real32_t fsize);

extern void hb_office_sheet_cell_bold(Sheet *sheet, const uint32_t page, const uint32_t col, const uint32_t row, const bool_t bold);

extern void hb_office_sheet_cell_italic(Sheet *sheet, const uint32_t page, const uint32_t col, const uint32_t row, const bool_t italic);

extern void hb_office_sheet_cell_halign(Sheet *sheet, const uint32_t page, const uint32_t col, const uint32_t row, const halign_t align);

extern void hb_office_sheet_cell_valign(Sheet *sheet, const uint32_t page, const uint32_t col, const uint32_t row, const valign_t align);

extern void hb_office_sheet_cell_wrap(Sheet *sheet, const uint32_t page, const uint32_t col, const uint32_t row, const bool_t wrapped);

extern void hb_office_sheet_cell_color(Sheet *sheet, const uint32_t page, const uint32_t col, const uint32_t row, const uint32_t rgb);

extern void hb_office_sheet_cell_backcolor(Sheet *sheet, const uint32_t page, const uint32_t col, const uint32_t row, const uint32_t rgb);

extern void hb_office_sheet_cells_backcolor(Sheet *sheet, const uint32_t page, const uint32_t st_col, const uint32_t st_row, const uint32_t ed_col, const uint32_t ed_row, const uint32_t rgb);

extern void hb_office_sheet_cell_image(Sheet *sheet, const uint32_t page, const uint32_t col, const uint32_t row, HB_ITEM *image_path_block);

extern void hb_office_sheet_cell_border(Sheet *sheet, const uint32_t page, const uint32_t col, const uint32_t row, const linestyle_t style, const uint32_t thickness, const uint32_t rgb);

extern void hb_office_sheet_cells_border(Sheet *sheet, const uint32_t page, const uint32_t st_col, const uint32_t st_row, const uint32_t ed_col, const uint32_t ed_row, const linestyle_t style, const uint32_t thickness, const uint32_t rgb);

extern void hb_office_sheet_cells_merge(Sheet *sheet, const uint32_t page, const uint32_t st_col, const uint32_t st_row, const uint32_t ed_col, const uint32_t ed_row);

extern void hb_office_sheet_column_visible(Sheet *sheet, const uint32_t page, const uint32_t col, const bool_t visible);

extern void hb_office_sheet_column_optimal_width(Sheet *sheet, const uint32_t page, const uint32_t col, const bool_t optimal_width);

extern void hb_office_sheet_column_width(Sheet *sheet, const uint32_t page, const uint32_t col, const uint32_t width);

extern void hb_office_sheet_row_visible(Sheet *sheet, const uint32_t page, const uint32_t row, const bool_t visible);

extern void hb_office_sheet_row_optimal_height(Sheet *sheet, const uint32_t page, const uint32_t row, const bool_t optimal_height);

extern void hb_office_sheet_row_height(Sheet *sheet, const uint32_t page, const uint32_t row, const uint32_t height);

extern Writer *hb_office_writer_open(HB_ITEM *pathname_block);

extern Writer *hb_office_writer_create(void);

extern void hb_office_writer_save(Writer *writer, HB_ITEM *pathname_block);

extern void hb_office_writer_pdf(Writer *writer, HB_ITEM *pathname_block);

extern void hb_office_writer_print(Writer *writer, HB_ITEM *filename_block, HB_ITEM *printer_block, const paperorient_t orient, const paperformat_t format, const uint32_t paper_width, const uint32_t paper_height, const uint32_t num_copies, const bool_t collate_copies, HB_ITEM *pages_block);

extern void hb_office_writer_close(Writer *writer);

extern void hb_office_writer_page_header_show(Writer *writer, const bool_t show);

extern void hb_office_writer_page_header_margins(Writer *writer, const uint32_t left, const uint32_t right, const uint32_t spacing, const uint32_t height, const bool_t dynamic_spacing, const bool_t dynamic_height);

extern void hb_office_writer_page_footer_show(Writer *writer, const bool_t show);

extern void hb_office_writer_page_footer_margins(Writer *writer, const uint32_t left, const uint32_t right, const uint32_t spacing, const uint32_t height, const bool_t dynamic_spacing, const bool_t dynamic_height);

extern void hb_office_writer_page_margins(Writer *writer, const uint32_t left, const uint32_t right, const uint32_t top, const uint32_t bottom, const uint32_t gutter);

extern void hb_office_writer_text_space(Writer *writer, const textspace_t space);

extern void hb_office_writer_font_family(Writer *writer, HB_ITEM *ffamily_block);

extern void hb_office_writer_font_size(Writer *writer, const real32_t fsize);

extern void hb_office_writer_bold(Writer *writer, const bool_t bold);

extern void hb_office_writer_italic(Writer *writer, const bool_t italic);

extern void hb_office_writer_paragraph_halign(Writer *writer, const halign_t align);

extern void hb_office_writer_paragraph_lspacing(Writer *writer, const uint32_t height);

extern void hb_office_writer_insert_text(Writer *writer, HB_ITEM *text_block);

extern void hb_office_writer_insert_dash(Writer *writer, const uint32_t n);

extern void hb_office_writer_insert_image(Writer *writer, const anchortype_t anchor, const uint32_t width, const uint32_t height, const halign_t halign, const valign_t valign, HB_ITEM *image_path_block);

extern void hb_office_writer_insert_page_number(Writer *writer);

extern void hb_office_writer_insert_new_line(Writer *writer);

extern void hb_office_writer_insert_paragraph(Writer *writer);

extern void hb_office_writer_insert_page_break(Writer *writer);

HB_EXTERN_END

#endif
