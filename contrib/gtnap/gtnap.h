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

typedef void(*FPtr_gtnap_callback)(GtNapCallback *callback, Event *event);

HB_EXTERN_BEGIN

extern void hb_gtnap_runloop(void);

extern void hb_gtnap_set_global_font(Font *font);

extern Font *hb_gtnap_global_font(void);

extern Window *hb_gtnap_main_window(void);

extern Window *hb_gtnap_current_modal(void);

extern void hb_gtnap_set_modal_window(Window *window);

extern void hb_gtnap_destroy_modal(void);

extern GtNapArea *hb_gtnap_new_area(TableView *view);

extern GtNapArea *hb_gtnap_get_area(TableView *view);

extern void hb_gtnap_area_add_column(GtNapArea *area, const char_t *title, const real32_t width, const align_t align, PHB_ITEM codeBlock);

extern void hb_gtnap_area_set_row(GtNapArea *area, const uint32_t row);

extern uint32_t hb_gtnap_area_row_count(GtNapArea *area);

extern const char_t *hb_gtnap_area_eval_field(GtNapArea *area, const uint32_t field_id, const uint32_t row_id, align_t *align);

extern char_t* hb_gtnap_area_temp(GtNapArea *area, uint32_t *size);

extern void* hb_gtnap_area(GtNapArea *area);

const char_t *hb_gtnap_parText(const uint32_t iParam);

extern Image *hb_gtnap_parImage(int iParam);

extern Font *hb_gtnap_parFont(int iParam);

extern Window *hb_gtnap_parWindow(int iParam);

extern void hb_gtnap_retImageGC(Image *image);

extern void hb_gtnap_retFontGC(Font *font);

extern void hb_gtnap_retWindowGC(Window *window);

extern Listener *hb_gtnap_comp_listener(const uint32_t codeBlockParamId, GuiComponent *component, FPtr_gtnap_callback func_callback);

extern Listener *hb_gtnap_wind_listener(const uint32_t codeBlockParamId, Window *window, FPtr_gtnap_callback func_callback);

extern void hb_gtnap_callback(GtNapCallback *callback, Event *e);

//
// GTNAP-CUALIB Specific function
//

String *hb_gtnap_cualib_parText(const uint32_t iParam);

extern void hb_gtnap_cualib_setup(const char_t *title, const uint32_t rows, const uint32_t cols, PHB_ITEM codeBlock_begin);

extern uint32_t hb_gtnap_cualib_linespacing(void);

extern void hb_gtnap_cualib_set_linespacing(const uint32_t spacing);

// N_LinIni --> Top
// N_ColIni --> Left
// N_LinFin --> Bottom
// N_ColFin --> Right
// C_Cabec --> Title
extern uint32_t hb_gtnap_cualib_window(const uint32_t N_LinIni, const uint32_t N_ColIni, const uint32_t N_LinFin, const uint32_t N_ColFin, const char_t *C_Cabec, const bool_t close_return, const bool_t close_esc, const bool_t minimize_button);

extern void hb_gtnap_cualib_menuvert(Panel *panel, const uint32_t nTop, const uint32_t nLeft, const uint32_t nBottom, const uint32_t nRight);

extern void hb_gtnap_cualib_tableview(TableView *view, const uint32_t nTop, const uint32_t nLeft, const uint32_t nBottom, const uint32_t nRight);

extern GtNapArea *hb_gtnap_cualib_tableview_area(TableView *view);

//extern GtNapArea *hb_gtnap_cualib_tableview_get_area(TableView *view);

extern void hb_gtnap_cualib_tableview_area_add_column(const char_t *title, const bool_t freeze, const uint32_t width, PHB_ITEM codeBlock);

extern void hb_gtnap_cualib_image(const char_t *pathname, const uint32_t codeBlockParamId, const uint32_t nTop, const uint32_t nLeft, const uint32_t nBottom, const uint32_t nRight, const bool_t autoclose);

extern void hb_gtnap_cualib_button(const char_t *text, const uint32_t codeBlockParamId, const uint32_t nTop, const uint32_t nLeft, const uint32_t nBottom, const uint32_t nRight, const bool_t autoclose);

extern void hb_gtnap_cualib_label(const char_t *text, const uint32_t nLin, const uint32_t nCol);

extern void hb_gtnap_cualib_toolbar(const uint32_t nPixelsImage);

extern void hb_gtnap_cualib_toolbar_button(const char_t *pathname, const char_t *tooltip);

extern void hb_gtnap_cualib_toolbar_separator(void);

extern void hb_gtnap_cualib_hotkey(const int32_t key, const uint32_t codeBlockParamId, const bool_t autoclose);

extern uint32_t hb_gtnap_cualib_launch_modal(void);

extern void hb_gtnap_cualib_destroy_window(void);

extern Window *hb_gtnap_cualib_current_window(void);

HB_EXTERN_END

#endif
