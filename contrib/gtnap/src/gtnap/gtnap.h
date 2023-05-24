/*
    This is part of gtnap
    TODO: More info
*/

#ifndef HB_GTNAP_H_
#define HB_GTNAP_H_

#define HB_GT_NAME NAP

#include "hbvmint.h"
#include "hbgtcore.h"

// TODO: "gui.hxx" is not required here
#include "gui.hxx"

#include "gtnap.ch"

typedef struct _gtnap_area_t GtNapArea;
typedef struct _gtnap_vector_t GtNapVector;

HB_EXTERN_BEGIN

/* Public functions (with Harbour wrapper) */

extern void hb_gtnap_init(const char_t *title, const uint32_t rows, const uint32_t cols, PHB_ITEM begin_block);

extern void hb_gtnap_terminal(void);

extern int32_t hb_gtnap_inkey(const vkey_t vkey);

extern uint32_t hb_gtnap_window(const int32_t top, const int32_t left, const int32_t bottom, const int32_t right, const char_t *title, const bool_t close_return, const bool_t close_esc, const bool_t minimize_button, const bool_t buttons_navigation);

extern void hb_gtnap_window_hotkey(const uint32_t wid, const int32_t key, HB_ITEM *block, const bool_t autoclose);

extern void hb_gtnap_window_editable(const uint32_t wid, HB_ITEM *is_editable_block);

extern void hb_gtnap_window_confirm(const uint32_t wid, HB_ITEM *confirm_block);

extern void hb_gtnap_window_desist(const uint32_t wid, HB_ITEM *desist_block);

extern void hb_gtnap_window_errdate(const uint32_t wid, HB_ITEM *error_date_block);

extern void hb_gtnap_window_scroll(const uint32_t wid, const int32_t top, const int32_t left, const int32_t bottom, const int32_t right);

extern uint32_t hb_gtnap_window_modal(const uint32_t wid);

extern uint32_t hb_gtnap_label(const uint32_t wid, const int32_t top, const int32_t left, HB_ITEM *text_block, const bool_t in_scroll);

extern uint32_t hb_gtnap_label_message(const uint32_t wid, const int32_t top, const int32_t left, const bool_t in_scroll);

extern void hb_gtnap_label_fgcolor(const uint32_t wid, const uint32_t id, const color_t color);

extern void hb_gtnap_label_bgcolor(const uint32_t wid, const uint32_t id, const color_t color);

extern uint32_t hb_gtnap_button(const uint32_t wid, const int32_t top, const int32_t left, const int32_t bottom, const int32_t right, HB_ITEM *text_block, HB_ITEM *click_block, const bool_t autoclose, const bool_t in_scroll);

extern void hb_gtnap_button_click(const uint32_t wid, const uint32_t id, HB_ITEM *click_block);

extern uint32_t hb_gtnap_image(const uint32_t wid, const int32_t top, const int32_t left, const int32_t bottom, const int32_t right, const char_t *pathname, HB_ITEM *click_block, const bool_t autoclose, const bool_t in_scroll);

extern uint32_t hb_gtnap_edit(const uint32_t wid, const int32_t top, const int32_t left, const uint32_t width, const char_t type, HB_ITEM *get_set_block, HB_ITEM *is_editable_block, HB_ITEM *when_block, HB_ITEM *valida_block, HB_ITEM *message_block, HB_ITEM *keyfilter_block, HB_ITEM *auto_block, HB_ITEM *lista_block, const bool_t in_scroll);

extern void hb_gtnap_edit_wizard(const uint32_t wid, const uint32_t id, const uint32_t bid, int32_t key, HB_ITEM *auto_block, HB_ITEM *wizard_block);

extern uint32_t hb_gtnap_textview(const uint32_t wid, const int32_t top, const int32_t left, const int32_t bottom, const int32_t right, HB_ITEM *get_set_block, HB_ITEM *valida_block, const bool_t in_scroll);

extern void hb_gtnap_textview_scroll(const uint32_t wid, const uint32_t id, const bool_t horizontal, const bool_t vertical);

extern void hb_gtnap_textview_caret(const uint32_t wid, const uint32_t id, const int64_t pos);

extern void hb_gtnap_textview_button(const uint32_t wid, const uint32_t id, const uint32_t bid);

extern void hb_gtnap_textview_hotkey(uint32_t wid, uint32_t id, int32_t key);

extern uint32_t hb_gtnap_menu(const uint32_t wid, const int32_t top, const int32_t left, const int32_t bottom, const int32_t right, const bool_t autoclose, const bool_t in_scroll);

extern void hb_gtnap_menu_add(const uint32_t wid, uint32_t id, HB_ITEM *text_block, HB_ITEM *click_block, uint32_t kpos);

extern uint32_t hb_gtnap_menu_selected(const uint32_t wid, uint32_t id);

extern uint32_t hb_gtnap_tableview(const uint32_t wid, const bool_t multisel, const int32_t top, const int32_t left, const int32_t bottom, const int32_t right, const bool_t in_scroll);

extern void hb_gtnap_tableview_column(const uint32_t wid, const uint32_t id, const uint32_t width, HB_ITEM *head_block, HB_ITEM *eval_block);

extern void hb_gtnap_tableview_scroll(const uint32_t wid, const uint32_t id, const bool_t horizontal, const bool_t vertical);

extern void hb_gtnap_tableview_grid(const uint32_t wid, const uint32_t id, const bool_t hlines, const bool_t vlines);

extern void hb_gtnap_tableview_freeze(const uint32_t wid, const uint32_t id, const uint32_t col_id);

extern void hb_gtnap_tableview_bind_area(const uint32_t wid, const uint32_t id, HB_ITEM *while_block);

extern void hb_gtnap_tableview_select(const uint32_t wid, const uint32_t id, HB_ITEM *selection);

extern void hb_gtnap_tableview_toggle(const uint32_t wid, const uint32_t id, HB_ITEM *selection);

extern const ArrSt(uint32_t) *hb_gtnap_tableview_selected(const uint32_t wid, const uint32_t id);

extern uint32_t hb_gtnap_tableview_focus_row(const uint32_t wid, const uint32_t id);

extern void hb_gtnap_tableview_refresh_all(const uint32_t wid, const uint32_t id);

//
// Future improvements for more generalist GTNAP
//
// hb_gtnap_edit_wizard not in gtnap (but in cualib)
// MenuVert option autoclose decision not in gtnap (but in cualib) (.T., .F., .NIL., No return, Selecaov, etc)
//


/*
 * Private functions
 *
 */



/*
*
*
*
*/






//extern void hb_gtnap_area_add_column(GtNapArea *area, const char_t *title, const real32_t width, const align_t align, PHB_ITEM codeBlock);

extern uint32_t hb_gtnap_area_row_count(GtNapArea *area);

extern const char_t *hb_gtnap_area_eval_field(GtNapArea *area, const uint32_t field_id, const uint32_t row_id, align_t *align);

extern const char_t *hb_gtnap_vector_eval_field(GtNapVector *vector, const uint32_t field_id, const uint32_t row_id);

extern uint32_t hb_gtnap_vector_items_count(GtNapVector *vector);

//extern void hb_gtnap_vector_add_item()


extern char_t* hb_gtnap_area_temp(GtNapArea *area, uint32_t *size);

extern void* hb_gtnap_area(GtNapArea *area);





//
// GTNAP-CUALIB Specific function
//


extern void hb_gtnap_cualib_init_log(void);





extern void hb_gtnap_cualib_window_f4_lista(void);

extern uint32_t hb_gtnap_cualib_window_current_edit(void);

extern void hb_gtnap_cualib_add_message_label(const int32_t N_LinIni, const int32_t N_ColIni);


// extern void hb_gtnap_cualib_menuvert(Panel *panel, const int32_t nTop, const int32_t nLeft, const int32_t nBottom, const int32_t nRight);

extern void hb_gtnap_cualib_tableview(TableView *view, const int32_t nTop, const int32_t nLeft, const int32_t nBottom, const int32_t nRight);

// extern void hb_gtnap_cualib_textview(
//     TextView *view,
//     const uint32_t editaBlockParamId,
//     const int32_t nTop,
//     const int32_t nLeft,
//     const int32_t nBottom,
//     const int32_t nRight);

extern GtNapArea *hb_gtnap_cualib_tableview_area(TableView *view, const uint32_t whileBlockParamId);

//extern void hb_gtnap_cualib_area_refresh(GtNapArea *area, const bool_t set_deleted);

extern GtNapVector *hb_gtnap_cualib_tableview_vector(TableView *view);

//extern GtNapArea *hb_gtnap_cualib_tableview_get_area(TableView *view);

extern void hb_gtnap_cualib_tableview_area_add_column(TableView *view, const char_t *title, const bool_t freeze, const uint32_t width, PHB_ITEM codeBlock);

extern void hb_gtnap_cualib_tableview_vector_add_column(TableView *view/*, const char_t *title, const bool_t freeze*/, const uint32_t width, PHB_ITEM codeBlock);

extern void hb_gtnap_cualib_tableview_vector_add_item(TableView *view, String *text, PHB_ITEM codeBlock, const uint32_t hotkey_pos);  // /*, const char_t *title, const bool_t freeze*/, const uint32_t width, PHB_ITEM codeBlock);

extern void hb_gtnap_cualib_vector_selection(const ArrSt(uint32_t) *sel);

extern void hb_gtnap_cualib_tableview_refresh_all(void);

extern void hb_gtnap_cualib_tableview_refresh_current(void);

extern void hb_gtnap_cualib_column_width(GtNapArea *area, const uint32_t col, const char_t *text);

extern void hb_gtnap_area_cache_cur_db_row(GtNapArea *area);

extern void hb_gtnap_area_restore_cur_db_row(GtNapArea *area);

extern uint32_t hb_gtnap_cualib_tableview_select_single_row(void);

extern ArrSt(uint32_t) *hb_gtnap_cualib_tableview_select_multiple_row(void);


//extern void hb_gtnap_cualib_button(const char_t *text, const uint32_t codeBlockParamId, const uint32_t nTag, const int32_t nTop, const int32_t nLeft, const int32_t nBottom, const int32_t nRight, const bool_t autoclose);

// extern void hb_gtnap_cualib_text_confirma_button(const uint32_t button_id, const uint32_t confirmaBlockParamId, const uint32_t validBlockParamId, const bool_t autoclose);

extern void hb_gtnap_cualib_default_button(const uint32_t nDefault);

//extern void hb_gtnap_cualib_edit(
//                    const uint32_t editaBlockParamId,
//                    const uint32_t editableGlobalParamId,
//                    const uint32_t editableLocalParamId,
//                    const uint32_t mensParamId,
//                    const uint32_t listaParamId,
//                    const uint32_t autoParamId,
//                    const uint32_t validaParamId,
//                    const uint32_t nLin,
//                    const uint32_t nCol,
//                    const uint32_t nSize,
//                    const char_t *type,
//                    PHB_ITEM getobj,
//                    const bool_t in_scroll,
//                    const uint32_t filtroTecParamId,
//                    const uint32_t whenParamId);

extern void hb_gtnap_cualib_toolbar(const uint32_t nPixelsImage);

extern void hb_gtnap_cualib_toolbar_button(const char_t *pathname, const char_t *tooltip);

extern void hb_gtnap_cualib_toolbar_separator(void);

extern void hb_gtnap_cualib_hotkey(const int32_t key, const uint32_t codeBlockParamId, const bool_t autoclose);

// extern void hb_gtnap_cualib_text_confirma_hotkey(const int32_t key, const uint32_t confirmaBlockParamId, const uint32_t validBlockParamId, const bool_t autoclose);

extern void hb_gtnap_cualib_error_data(const uint32_t errorDataBlockParamId);

extern uint32_t hb_gtnap_cualib_launch_modal(const uint32_t confirmaBlockParamId, const uint32_t cancelBlockParamId);

extern void hb_gtnap_cualib_destroy_window(void);

extern TableView *hb_gtnap_cualib_current_tableview(void);

// extern Panel *hb_gtnap_cualib_current_menuvert(void);

extern void hb_gtnap_cualib_tableview_On_Single_Select_Change(void);

//extern void hb_gtnap_cualib_tableview_OnSelect(const uint32_t codeBlockParamId);

extern bool_t hb_gtnap_cualib_current_row_selected(void);

extern void hb_gtnap_cualib_multisel(void);

extern void hb_gtnap_cualib_select_current(void);

extern void hb_gtnap_cualib_select_current_vector(void);


/*
 *   Deprecated/Unused
 *
 */
//extern Listener *hb_gtnap_comp_listener(const uint32_t codeBlockParamId, GuiComponent *component, FPtr_gtnap_callback func_callback);

//extern Listener *hb_gtnap_wind_listener(const uint32_t codeBlockParamId, Window *window, FPtr_gtnap_callback func_callback);

//extern void hb_gtnap_cualib_label(const char_t *text, const uint32_t nLin, const uint32_t nCol, const bool_t background, const bool_t in_scroll, const uint32_t updateBlockParamId);

//extern void hb_gtnap_cualib_image(const char_t *pathname, const uint32_t codeBlockParamId, const uint32_t nTop, const uint32_t nLeft, const uint32_t nBottom, const uint32_t nRight, const bool_t autoclose);

extern Image *hb_gtnap_parImage(int iParam);

extern Font *hb_gtnap_parFont(int iParam);

extern Window *hb_gtnap_parWindow(int iParam);

extern void hb_gtnap_retImageGC(Image *image);

extern void hb_gtnap_retFontGC(Font *font);


HB_EXTERN_END

#endif
