/*
    This is part of gtnap
    TODO: More info
*/

#define HB_GT_NAME            NAP

/*---------------------------------------------------------------------------*/

















#ifndef HB_NAP_H_
#define HB_NAP_H_


/* NOTE: User programs should never call this layer directly! */

/* This definition has to be placed before #include "hbapigt.h" */



#include "hbset.h"
#include "hbgtcore.h"
#include "hbinit.h"
#include "hbapigt.h"
#include "hbapierr.h"
#include "hbapiitm.h"
#include "inkey.ch"
#include "error.ch"
#include "hbvm.h"
#include "hbstack.h"
#include "hbwinuni.h"

//#include "hbole.h"

#include <windows.h>
#include <stdlib.h>
#include <commctrl.h>

#include <math.h>       /* fmod */
#include <winuser.h>
#include <commctrl.h>
#include <commdlg.h>

#if defined( __MINGW32__ ) || defined( __WATCOMC__ ) || defined( _MSC_VER ) || defined( __DMC__ )
   #include <unknwn.h>
   #include <ole2.h>
   #include <ocidl.h>
   #include <olectl.h>

   #if defined( _MSC_VER ) || defined( __DMC__ )
      #include <conio.h>

   #endif
#else
   #include <olectl.h>
#endif




// NAPPGUI
#include "gui.hxx"

// For database...
#include "hbapirdd.h"






HB_EXTERN_BEGIN


// Global GTNap data
typedef struct _gtnap_t GtNap;
typedef struct _gui_component_t GuiComponent;
typedef struct _gtnap_callback_t GtNapCallback;
typedef struct _gtnap_area_t GtNapArea;

/*---------------------------------------------------------------------------*/

void _component_set_tag(GuiComponent *component, const uint32_t tag);
uint32_t _component_get_tag(const GuiComponent *component);
Window *_component_window(const GuiComponent *component);

struct _gtnap_callback_t
{
    GuiComponent *cb_component;
    Window *cb_window;
    PHB_ITEM codeBlock;
};

struct _gtnap_area_t
{
    AREA *area;
    uint32_t currow;
    TableView *view;
    char_t temp[512];   // Temporal buffer between RDD and TableView
};

DeclPt(GtNapCallback);
DeclPt(GtNapArea);
DeclPt(Window);

struct _gtnap_t
{
    Font *global_font;
    ArrPt(Window) *modals;
    ArrPt(Window) *windows;
    ArrPt(GtNapCallback) *callbacks;
    ArrPt(GtNapArea) *areas;
};







/* Get functions for internal Data */

HB_EXTERN_END















extern void hb_gtnap_runloop(void);

extern void hb_gtnap_set_global_font(Font *font);

extern Font *hb_gtnap_global_font(void);

extern Window *hb_gtnap_main_window(void);

extern Window *hb_gtnap_current_modal(void);

extern void hb_gtnap_set_modal_window(Window *window);

extern void hb_gtnap_destroy_modal(void);

extern GtNapArea *hb_gtnap_new_area(void);

const char_t *hb_gtnap_parText(const uint32_t iParam);

extern Image *hb_gtnap_parImage(int iParam);

extern Font *hb_gtnap_parFont(int iParam);

extern Window *hb_gtnap_parWindow(int iParam);

extern void hb_gtnap_retImageGC(Image *image);

extern void hb_gtnap_retFontGC(Font *font);

extern void hb_gtnap_retWindowGC(Window *window);

extern Listener *hb_gtnap_comp_listener(const uint32_t codeBlockParamId, GuiComponent *component, void(*FPtr_CallBack)(void*, Event*));

extern Listener *hb_gtnap_wind_listener(const uint32_t codeBlockParamId, Window *window, void(*FPtr_CallBack)(void*, Event*));

extern void hb_gtnap_callback(GtNapCallback *callback, Event *e);

#endif
