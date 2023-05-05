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

typedef struct _gui_component_t GuiComponent;
typedef struct _gtnap_callback_t GtNapCallback;
typedef struct _gtnap_area_t GtNapArea;
typedef struct _gtnap_vector_t GtNapVector;

typedef void(*FPtr_gtnap_callback)(GtNapCallback *callback, Event *event);

HB_EXTERN_BEGIN

extern void hb_gtnap_setup(const char_t *title, const uint32_t rows, const uint32_t cols, PHB_ITEM begin_block);

extern Font *hb_gtnap_font(void);

extern uint32_t hb_gtnap_window(const int32_t top, const int32_t left, const int32_t bottom, const int32_t right, const char_t *title, const bool_t close_return, const bool_t close_esc, const bool_t minimize_button, const bool_t buttons_navigation);








/*
*
*
*
*/





extern void hb_gtnap_area_add_column(GtNapArea *area, const char_t *title, const real32_t width, const align_t align, PHB_ITEM codeBlock);

extern uint32_t hb_gtnap_area_row_count(GtNapArea *area);

extern const char_t *hb_gtnap_area_eval_field(GtNapArea *area, const uint32_t field_id, const uint32_t row_id, align_t *align);

extern const char_t *hb_gtnap_vector_eval_field(GtNapVector *vector, const uint32_t field_id, const uint32_t row_id);

extern uint32_t hb_gtnap_vector_items_count(GtNapVector *vector);

//extern void hb_gtnap_vector_add_item()


extern char_t* hb_gtnap_area_temp(GtNapArea *area, uint32_t *size);

extern void* hb_gtnap_area(GtNapArea *area);

const char_t *hb_gtnap_parText(const uint32_t iParam);

extern Image *hb_gtnap_parImage(int iParam);

extern Font *hb_gtnap_parFont(int iParam);

extern Window *hb_gtnap_parWindow(int iParam);

extern void hb_gtnap_retImageGC(Image *image);

extern void hb_gtnap_retFontGC(Font *font);

extern void hb_gtnap_callback(GtNapCallback *callback, Event *e);

extern bool_t hb_gtnap_callback_bool(GtNapCallback *callback, Event *e);

//
// GTNAP-CUALIB Specific function
//

String *hb_gtnap_cualib_parText(const uint32_t iParam);

extern void hb_gtnap_cualib_init_log(void);


extern void hb_gtnap_cualib_window_enter_tabstop(void);

extern void hb_gtnap_cualib_window_arrows_tabstop(void);

extern void hb_gtnap_cualib_window_stops_last_edit(void);

extern void hb_gtnap_cualib_window_f4_lista(void);

extern uint32_t hb_gtnap_cualib_window_current_edit(void);

extern void hb_gtnap_cualib_add_message_label(const int32_t N_LinIni, const int32_t N_ColIni);

extern void hb_gtnap_cualib_window_scroll_panel(const int32_t N_LinIni, const int32_t N_ColIni, const int32_t N_LinFin, const int32_t N_ColFin);

extern void hb_gtnap_cualib_menuvert(Panel *panel, const int32_t nTop, const int32_t nLeft, const int32_t nBottom, const int32_t nRight);

extern void hb_gtnap_cualib_tableview(TableView *view, const int32_t nTop, const int32_t nLeft, const int32_t nBottom, const int32_t nRight);

extern void hb_gtnap_cualib_textview(
    TextView *view,
    const uint32_t editaBlockParamId,
    const int32_t nTop,
    const int32_t nLeft,
    const int32_t nBottom,
    const int32_t nRight);

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

extern void hb_gtnap_cualib_image(const char_t *pathname, const uint32_t codeBlockParamId, const uint32_t nTop, const uint32_t nLeft, const uint32_t nBottom, const uint32_t nRight, const bool_t autoclose);

extern void hb_gtnap_cualib_button(const char_t *text, const uint32_t codeBlockParamId, const uint32_t nTag, const int32_t nTop, const int32_t nLeft, const int32_t nBottom, const int32_t nRight, const bool_t autoclose);

extern void hb_gtnap_cualib_text_confirma_button(const uint32_t button_id, const uint32_t confirmaBlockParamId, const uint32_t validBlockParamId, const bool_t autoclose);

extern void hb_gtnap_cualib_default_button(const uint32_t nDefault);

extern void hb_gtnap_cualib_label(const char_t *text, const uint32_t nLin, const uint32_t nCol, const bool_t background, const bool_t in_scroll_panel, const uint32_t updateBlockParamId);

extern void hb_gtnap_cualib_edit(
                    const uint32_t editaBlockParamId,
                    const uint32_t editableGlobalParamId,
                    const uint32_t editableLocalParamId,
                    const uint32_t mensParamId,
                    const uint32_t listaParamId,
                    const uint32_t autoParamId,
                    const uint32_t validaParamId,
                    const uint32_t nLin,
                    const uint32_t nCol,
                    const uint32_t nSize,
                    const char_t *type,
                    PHB_ITEM getobj,
                    const bool_t in_scroll_panel,
                    const uint32_t filtroTecParamId,
                    const uint32_t whenParamId);

extern void hb_gtnap_cualib_toolbar(const uint32_t nPixelsImage);

extern void hb_gtnap_cualib_toolbar_button(const char_t *pathname, const char_t *tooltip);

extern void hb_gtnap_cualib_toolbar_separator(void);

extern void hb_gtnap_cualib_hotkey(const int32_t key, const uint32_t codeBlockParamId, const bool_t autoclose);

extern void hb_gtnap_cualib_text_confirma_hotkey(const int32_t key, const uint32_t confirmaBlockParamId, const uint32_t validBlockParamId, const bool_t autoclose);

extern void hb_gtnap_cualib_error_data(const uint32_t errorDataBlockParamId);

extern uint32_t hb_gtnap_cualib_launch_modal(const uint32_t confirmaBlockParamId, const uint32_t cancelBlockParamId);

extern void hb_gtnap_cualib_destroy_window(void);

extern Window *hb_gtnap_cualib_current_window(void);

extern TableView *hb_gtnap_cualib_current_tableview(void);

extern Panel *hb_gtnap_cualib_current_menuvert(void);

extern uint32_t hb_gtnap_cell_height(void);

extern void hb_gtnap_cualib_tableview_On_Single_Select_Change(void);

extern void hb_gtnap_cualib_tableview_OnSelect(const uint32_t codeBlockParamId);

extern bool_t hb_gtnap_cualib_current_row_selected(void);

extern void hb_gtnap_cualib_multisel(void);

extern void hb_gtnap_cualib_select_current(void);

extern void hb_gtnap_cualib_select_current_vector(void);


/*
 *   Deprecated/Unused
 *
 */
extern Listener *hb_gtnap_comp_listener(const uint32_t codeBlockParamId, GuiComponent *component, FPtr_gtnap_callback func_callback);

extern Listener *hb_gtnap_wind_listener(const uint32_t codeBlockParamId, Window *window, FPtr_gtnap_callback func_callback);

HB_EXTERN_END

#endif
