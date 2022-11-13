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

const char_t *hb_gtnap_cualib_parText(const uint32_t iParam);

extern void hb_gtnap_cualib_setup(const char_t *title, const uint32_t rows, const uint32_t cols);

extern uint32_t hb_gtnap_cualib_linespacing(void);

extern void hb_gtnap_cualib_set_linespacing(const uint32_t spacing);

extern void hb_gtnap_cualib_modal_window(void);

HB_EXTERN_END

#endif
