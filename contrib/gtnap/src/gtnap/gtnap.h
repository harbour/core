/*
    This is part of gtnap
    TODO: More info
*/

#ifndef HB_GTNAP_H_
#define HB_GTNAP_H_

#define HB_GT_NAME NAP

#include "hbvmint.h"
#include "hbgtcore.h"
#include <officesdk/officesdk.hxx>
#include <gui/gui.hxx>

HB_EXTERN_BEGIN

extern void hb_gtnap_init(const char_t *title, const uint32_t rows, const uint32_t cols, HB_ITEM *begin_block);

extern void hb_gtnap_log(const char_t *text);

extern uint32_t hb_gtnap_width(void);

extern uint32_t hb_gtnap_height(void);

extern void hb_gtnap_terminal(void);

extern int32_t hb_gtnap_inkey(const vkey_t vkey);

extern const char_t *hb_gtnap_working_path(void);

extern uint32_t hb_gtnap_window(const int32_t top, const int32_t left, const int32_t bottom, const int32_t right, const char_t *title, const bool_t close_return, const bool_t close_esc, const bool_t minimize_button, const bool_t buttons_navigation);

extern uint32_t hb_gtnap_window_embedded(const uint32_t wid, const int32_t top, const int32_t left, const int32_t bottom, const int32_t right, const bool_t border);

extern void hb_gtnap_window_destroy(const uint32_t wid);

extern void hb_gtnap_window_hotkey(const uint32_t wid, const int32_t key, HB_ITEM *block, const bool_t autoclose);

extern void hb_gtnap_window_editable(const uint32_t wid, HB_ITEM *is_editable_block);

extern void hb_gtnap_window_confirm(const uint32_t wid, HB_ITEM *confirm_block);

extern void hb_gtnap_window_desist(const uint32_t wid, HB_ITEM *desist_block);

extern void hb_gtnap_window_errdate(const uint32_t wid, HB_ITEM *error_date_block);

extern void hb_gtnap_window_scroll(const uint32_t wid, const int32_t top, const int32_t left, const int32_t bottom, const int32_t right);

extern void hb_gtnap_window_copy(const uint32_t wid);

extern void hb_gtnap_window_paste(const uint32_t wid);

extern void hb_gtnap_window_cut(const uint32_t wid);

extern void hb_gtnap_window_undo(const uint32_t wid);

extern uint32_t hb_gtnap_window_modal(const uint32_t wid, const uint32_t pwid, const uint32_t delay_seconds);

extern void hb_gtnap_window_stop_modal(const uint32_t wid, const uint32_t result);

extern uint32_t hb_gtnap_label(const uint32_t wid, const int32_t top, const int32_t left, HB_ITEM *text_block, const bool_t in_scroll);

extern uint32_t hb_gtnap_label_message(const uint32_t wid, const int32_t top, const int32_t left, const bool_t in_scroll);

extern void hb_gtnap_label_update(const uint32_t wid, const uint32_t id, const int32_t top, const int32_t left, HB_ITEM *text_block);

extern void hb_gtnap_label_fgcolor(const uint32_t wid, const uint32_t id, const color_t color);

extern void hb_gtnap_label_bgcolor(const uint32_t wid, const uint32_t id, const color_t color);

extern void hb_gtnap_label_color(const uint32_t wid, const uint32_t id, const char_t *hb_color);

extern uint32_t hb_gtnap_button(const uint32_t wid, const int32_t top, const int32_t left, const int32_t bottom, const int32_t right, HB_ITEM *text_block, HB_ITEM *click_block, const bool_t autoclose, const bool_t in_scroll);

extern uint32_t hb_gtnap_image(const uint32_t wid, const int32_t top, const int32_t left, const int32_t bottom, const int32_t right, const char_t *pathname, HB_ITEM *click_block, const bool_t autoclose, const bool_t in_scroll);

extern uint32_t hb_gtnap_edit(const uint32_t wid, const int32_t top, const int32_t left, const uint32_t width, const char_t type, HB_ITEM *get_set_block, HB_ITEM *is_editable_block, HB_ITEM *when_block, HB_ITEM *valida_block, HB_ITEM *message_block, HB_ITEM *keyfilter_block, const bool_t in_scroll);

extern void hb_gtnap_edit_color(const uint32_t wid, const uint32_t id, const char_t *hb_color);

/* TODO: Make more generic */
extern void hb_gtnap_edit_wizard(const uint32_t wid, const uint32_t id, const uint32_t bid, int32_t key, HB_ITEM *auto_block, HB_ITEM *wizard_block);
/*------------------------ */

extern uint32_t hb_gtnap_textview(const uint32_t wid, const int32_t top, const int32_t left, const int32_t bottom, const int32_t right, HB_ITEM *get_set_block, HB_ITEM *valida_block, HB_ITEM *keyfilter_block, const bool_t in_scroll);

extern void hb_gtnap_textview_scroll(const uint32_t wid, const uint32_t id, const bool_t horizontal, const bool_t vertical);

extern void hb_gtnap_textview_caret(const uint32_t wid, const uint32_t id, const int64_t pos);

extern void hb_gtnap_textview_button(const uint32_t wid, const uint32_t id, const uint32_t bid);

extern void hb_gtnap_textview_hotkey(uint32_t wid, uint32_t id, int32_t key);

extern uint32_t hb_gtnap_menu(const uint32_t wid, const int32_t top, const int32_t left, const int32_t bottom, const int32_t right, const bool_t autoclose, const bool_t in_scroll);

extern void hb_gtnap_menu_add(const uint32_t wid, uint32_t id, HB_ITEM *text_block, HB_ITEM *click_block, uint32_t kpos);

extern uint32_t hb_gtnap_menu_selected(const uint32_t wid, uint32_t id);

extern uint32_t hb_gtnap_tableview(const uint32_t wid, const int32_t top, const int32_t left, const int32_t bottom, const int32_t right, const bool_t multisel, const bool_t autoclose, const bool_t in_scroll);

extern void hb_gtnap_tableview_column(const uint32_t wid, const uint32_t id, const uint32_t width, HB_ITEM *head_block, HB_ITEM *eval_block);

extern void hb_gtnap_tableview_scroll(const uint32_t wid, const uint32_t id, const bool_t horizontal, const bool_t vertical);

extern void hb_gtnap_tableview_grid(const uint32_t wid, const uint32_t id, const bool_t hlines, const bool_t vlines);

extern void hb_gtnap_tableview_header(const uint32_t wid, const uint32_t id, const bool_t visible);

extern void hb_gtnap_tableview_freeze(const uint32_t wid, const uint32_t id, const uint32_t col_id);

extern void hb_gtnap_tableview_bind_area(const uint32_t wid, const uint32_t id, HB_ITEM *while_block);

extern void hb_gtnap_tableview_bind_data(const uint32_t wid, const uint32_t id, const uint32_t num_rows);

extern void hb_gtnap_tableview_deselect_all(const uint32_t wid, const uint32_t id);

extern void hb_gtnap_tableview_select_row(const uint32_t wid, const uint32_t id, const uint32_t row_id);

extern void hb_gtnap_tableview_toggle_row(const uint32_t wid, const uint32_t id, const uint32_t row_id);

extern const ArrSt(uint32_t) *hb_gtnap_tableview_selected_rows(const uint32_t wid, const uint32_t id);

extern uint32_t hb_gtnap_tableview_focus_row(const uint32_t wid, const uint32_t id);

extern uint32_t hb_gtnap_tableview_recno_from_row(const uint32_t wid, const uint32_t id, const uint32_t row_id);

extern uint32_t hb_gtnap_tableview_row_from_recno(const uint32_t wid, const uint32_t id, const uint32_t recno);

extern void hb_gtnap_tableview_refresh_current(const uint32_t wid, const uint32_t id);

extern void hb_gtnap_tableview_refresh_all(const uint32_t wid, const uint32_t id);

extern void hb_gtnap_toolbar(const uint32_t wid, const uint32_t image_pixels);

extern void hb_gtnap_toolbar_button(const uint32_t wid, const char_t *pathname, const char_t *tooltip, HB_ITEM *click_block);

extern void hb_gtnap_toolbar_separator(const uint32_t wid);

/* LibreOffice interface */

extern uint32_t hb_gtnap_office_text_to_pdf(HB_ITEM *src_block, HB_ITEM *dest_block);

extern uint32_t hb_gtnap_office_last_error(void);

extern const char_t *hb_gtnap_office_error(const uint32_t errcode);

extern void hb_gtnap_office_browse_doc(HB_ITEM *pathname_block);

extern uint32_t hb_gtnap_office_rgb(const uint8_t red, const uint8_t green, const uint8_t blue);

/* LibreOffice spreadsheets (SCalc) */

extern Sheet *hb_gtnap_office_sheet_open(HB_ITEM *pathname_block);

extern Sheet *hb_gtnap_office_sheet_create(void);

extern void hb_gtnap_office_sheet_save(Sheet *sheet, HB_ITEM *pathname_block);

extern void hb_gtnap_office_sheet_pdf(Sheet *sheet, HB_ITEM *pathname_block);

extern void hb_gtnap_office_sheet_print(Sheet *sheet, HB_ITEM *filename_block, HB_ITEM *printer_block, const paperorient_t orient, const paperformat_t format, const uint32_t paper_width, const uint32_t paper_height, const uint32_t num_copies, const bool_t collate_copies, HB_ITEM *pages_block);

extern void hb_gtnap_office_sheet_close(Sheet *sheet);

extern uint32_t hb_gtnap_office_sheet_add(Sheet *sheet);

extern void hb_gtnap_office_sheet_name(Sheet *sheet, const uint32_t page, HB_ITEM *name_block);

extern void hb_gtnap_office_sheet_protect(Sheet *sheet, const uint32_t page, const bool_t protect, HB_ITEM *pass_block);

extern void hb_gtnap_office_sheet_freeze(Sheet *sheet, const uint32_t page, const uint32_t ncols, const uint32_t nrows);

extern const char_t *hb_gtnap_office_cell_ref(Sheet *sheet, const uint32_t page, const uint32_t col, const uint32_t row);

extern void hb_gtnap_office_sheet_cell_text(Sheet *sheet, const uint32_t page, const uint32_t col, const uint32_t row, HB_ITEM *text_block);

extern void hb_gtnap_office_sheet_cell_value(Sheet *sheet, const uint32_t page, const uint32_t col, const uint32_t row, const real64_t value);

extern void hb_gtnap_office_sheet_cell_date(Sheet *sheet, const uint32_t page, const uint32_t col, const uint32_t row, const uint8_t day, const uint8_t month, const int16_t year);

extern void hb_gtnap_office_sheet_cell_formula(Sheet *sheet, const uint32_t page, const uint32_t col, const uint32_t row, HB_ITEM *formula_block);

extern void hb_gtnap_office_sheet_cell_numformat(Sheet *sheet, const uint32_t page, const uint32_t col, const uint32_t row, const numformat_t format);

extern void hb_gtnap_office_sheet_cell_font_family(Sheet *sheet, const uint32_t page, const uint32_t col, const uint32_t row, HB_ITEM *ffamily_block);

extern void hb_gtnap_office_sheet_cell_font_size(Sheet *sheet, const uint32_t page, const uint32_t col, const uint32_t row, const real32_t fsize);

extern void hb_gtnap_office_sheet_cell_bold(Sheet *sheet, const uint32_t page, const uint32_t col, const uint32_t row, const bool_t bold);

extern void hb_gtnap_office_sheet_cell_italic(Sheet *sheet, const uint32_t page, const uint32_t col, const uint32_t row, const bool_t italic);

extern void hb_gtnap_office_sheet_cell_halign(Sheet *sheet, const uint32_t page, const uint32_t col, const uint32_t row, const halign_t align);

extern void hb_gtnap_office_sheet_cell_valign(Sheet *sheet, const uint32_t page, const uint32_t col, const uint32_t row, const valign_t align);

extern void hb_gtnap_office_sheet_cell_wrap(Sheet *sheet, const uint32_t page, const uint32_t col, const uint32_t row, const bool_t wrapped);

extern void hb_gtnap_office_sheet_cell_color(Sheet *sheet, const uint32_t page, const uint32_t col, const uint32_t row, const uint32_t rgb);

extern void hb_gtnap_office_sheet_cell_backcolor(Sheet *sheet, const uint32_t page, const uint32_t col, const uint32_t row, const uint32_t rgb);

extern void hb_gtnap_office_sheet_cells_backcolor(Sheet *sheet, const uint32_t page, const uint32_t st_col, const uint32_t st_row, const uint32_t ed_col, const uint32_t ed_row, const uint32_t rgb);

extern void hb_gtnap_office_sheet_cell_image(Sheet *sheet, const uint32_t page, const uint32_t col, const uint32_t row, HB_ITEM *image_path_block);

extern void hb_gtnap_office_sheet_cell_border(Sheet *sheet, const uint32_t page, const uint32_t col, const uint32_t row, const linestyle_t style, const uint32_t thickness, const uint32_t rgb);

extern void hb_gtnap_office_sheet_cells_border(Sheet *sheet, const uint32_t page, const uint32_t st_col, const uint32_t st_row, const uint32_t ed_col, const uint32_t ed_row, const linestyle_t style, const uint32_t thickness, const uint32_t rgb);

extern void hb_gtnap_office_sheet_cells_merge(Sheet *sheet, const uint32_t page, const uint32_t st_col, const uint32_t st_row, const uint32_t ed_col, const uint32_t ed_row);

extern void hb_gtnap_office_sheet_column_visible(Sheet *sheet, const uint32_t page, const uint32_t col, const bool_t visible);

extern void hb_gtnap_office_sheet_column_optimal_width(Sheet *sheet, const uint32_t page, const uint32_t col, const bool_t optimal_width);

extern void hb_gtnap_office_sheet_column_width(Sheet *sheet, const uint32_t page, const uint32_t col, const uint32_t width);

extern void hb_gtnap_office_sheet_row_visible(Sheet *sheet, const uint32_t page, const uint32_t row, const bool_t visible);

extern void hb_gtnap_office_sheet_row_optimal_height(Sheet *sheet, const uint32_t page, const uint32_t row, const bool_t optimal_height);

extern void hb_gtnap_office_sheet_row_height(Sheet *sheet, const uint32_t page, const uint32_t row, const uint32_t height);

/* LibreOffice text documents (Writer) */

extern Writer *hb_gtnap_office_writer_open(HB_ITEM *pathname_block);

extern Writer *hb_gtnap_office_writer_create(void);

extern void hb_gtnap_office_writer_save(Writer *writer, HB_ITEM *pathname_block);

extern void hb_gtnap_office_writer_pdf(Writer *writer, HB_ITEM *pathname_block);

extern void hb_gtnap_office_writer_print(Writer *writer, HB_ITEM *filename_block, HB_ITEM *printer_block, const paperorient_t orient, const paperformat_t format, const uint32_t paper_width, const uint32_t paper_height, const uint32_t num_copies, const bool_t collate_copies, HB_ITEM *pages_block);

extern void hb_gtnap_office_writer_close(Writer *writer);

extern void hb_gtnap_office_writer_page_header_show(Writer *writer, const bool_t show);

extern void hb_gtnap_office_writer_page_header_margins(Writer *writer, const uint32_t left, const uint32_t right, const uint32_t spacing, const uint32_t height, const bool_t dynamic_spacing, const bool_t dynamic_height);

extern void hb_gtnap_office_writer_page_footer_show(Writer *writer, const bool_t show);

extern void hb_gtnap_office_writer_page_footer_margins(Writer *writer, const uint32_t left, const uint32_t right, const uint32_t spacing, const uint32_t height, const bool_t dynamic_spacing, const bool_t dynamic_height);

extern void hb_gtnap_office_writer_page_margins(Writer *writer, const uint32_t left, const uint32_t right, const uint32_t top, const uint32_t bottom, const uint32_t gutter);

extern void hb_gtnap_office_writer_text_space(Writer *writer, const textspace_t space);

extern void hb_gtnap_office_writer_font_family(Writer *writer, HB_ITEM *ffamily_block);

extern void hb_gtnap_office_writer_font_size(Writer *writer, const real32_t fsize);

extern void hb_gtnap_office_writer_bold(Writer *writer, const bool_t bold);

extern void hb_gtnap_office_writer_italic(Writer *writer, const bool_t italic);

extern void hb_gtnap_office_writer_halign(Writer *writer, const halign_t align);

extern void hb_gtnap_office_writer_lspacing(Writer *writer, const uint32_t height);

extern void hb_gtnap_office_writer_insert_text(Writer *writer, HB_ITEM *text_block);

extern void hb_gtnap_office_writer_insert_dash(Writer *writer, const uint32_t n);

extern void hb_gtnap_office_writer_insert_image(Writer *writer, const anchortype_t anchor, const uint32_t width, const uint32_t height, HB_ITEM *image_path_block);

extern void hb_gtnap_office_writer_new_line(Writer *writer);

extern void hb_gtnap_office_writer_page_break(Writer *writer);

/*
 * Fran TODO: Pending refactoring
 *
 */
extern void hb_gtnap_cualib_init_log(void);

extern void hb_gtnap_cualib_window_f4_lista(void);

extern uint32_t hb_gtnap_cualib_window_current_edit(void);

extern void hb_gtnap_cualib_default_button(const uint32_t nDefault);


HB_EXTERN_END

#endif
