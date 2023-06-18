/*
    This is part of gtnap
    TODO: More info
*/

#ifndef HB_GTNAP_H_
#define HB_GTNAP_H_

#define HB_GT_NAME NAP

#include "hbvmint.h"
#include "hbgtcore.h"
#include "gui.hxx"

HB_EXTERN_BEGIN

extern void hb_gtnap_init(const char_t *title, const uint32_t rows, const uint32_t cols, HB_ITEM *begin_block);

extern void hb_gtnap_log(const char_t *text);

extern uint32_t hb_gtnap_width(void);

extern uint32_t hb_gtnap_height(void);

extern void hb_gtnap_terminal(void);

extern int32_t hb_gtnap_inkey(const vkey_t vkey);

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

extern uint32_t hb_gtnap_window_modal(const uint32_t wid, const uint32_t delay_seconds);

extern void hb_gtnap_window_stop_modal(const uint32_t result);

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
