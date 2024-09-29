/*
    This is part of gtnap
    TODO: More info
    Commit - 2
*/

#include "gtnap.h"
#include "gtnap.inl"
#include "gtnap.ch"
#include "nap_menu.inl"
#include "nap_debugger.inl"
#include "nappgui.h"
#include <osapp/osmain.h>
#include <gui/drawctrl.inl>
#include <deblib/deblib.h>
#include <sewer/blib.h>
#include <sewer/cassert.h>

#include "hbapiitm.h"
#include "hbapirdd.h"
#include "hbapistr.h"
#include "hbdate.h"
#include "hbset.h"

typedef struct _gui_component_t GuiComponent;
typedef struct _gtnap_callback_t GtNapCallback;
typedef struct _gtnap_key_t GtNapKey;
typedef struct _gtnap_column_t GtNapColumn;
typedef struct _gtnap_toolbar_t GtNapToolbar;
typedef struct _gtnap_area_t GtNapArea;
typedef struct _gtnap_object_t GtNapObject;
typedef struct _gtnap_geom_t GtNapGeom;
typedef struct _gtnap_window_t GtNapWindow;
typedef struct _gtnap_t GtNap;

typedef void (*FPtr_gtnap_callback)(GtNapCallback *callback, Event *event);

typedef enum _objtype_t
{
    ekOBJ_LABEL,
    ekOBJ_EDIT,
    ekOBJ_BUTTON,
    ekOBJ_MENU,
    ekOBJ_TABLEVIEW,
    ekOBJ_TEXTVIEW,
    ekOBJ_IMAGE
} objtype_t;

typedef enum _datatype_t
{
    ekTYPE_CHARACTER,
    ekTYPE_DATE
} datatype_t;

struct _gtnap_callback_t
{
    GtNapWindow *gtwin;
    HB_ITEM *block;
    int32_t key;
    uint32_t autoclose_id;
};

struct _gtnap_key_t
{
    int32_t key; /* inkey.ch */
    vkey_t vkey;
    uint32_t modifiers;
};

struct _gtnap_column_t
{
    uint32_t fixed_chars;
    uint32_t header_lines;
    real32_t widthf;
    align_t align;
    String *title;
    HB_ITEM *block;
};

struct _gtnap_toolbar_t
{
    real32_t button_widthf;
    real32_t heightf;
    ArrPt(GuiComponent) *items;
};

struct _gtnap_area_t
{
    AREA *area;
    HB_ULONG cache_recno; /* Store the DB recno while table drawing */
    GtNapObject *gtobj;
    ArrSt(uint32_t) *records; /* Records visible in table (index, deleted, filters) */
    HB_ITEM *while_block;
};

struct _gtnap_object_t
{
    objtype_t type;
    datatype_t dtype;
    int32_t top;
    int32_t left;
    V2Df pos;
    S2Df size;
    bool_t multisel;
    bool_t autoclose;
    bool_t is_last_edit;
    bool_t in_scroll;
    bool_t can_auto_lista;
    bool_t has_focus;
    uint32_t max_chars;
    uint32_t editBoxIndexForButton;
    String *text;
    PHB_ITEM text_block;
    PHB_ITEM get_set_block;
    PHB_ITEM is_editable_block;
    PHB_ITEM when_block;
    PHB_ITEM valida_block;
    PHB_ITEM message_block;
    PHB_ITEM keyfilter_block;
    PHB_ITEM auto_block;
    PHB_ITEM wizard_block;
    ArrSt(GtNapColumn) *columns;
    GuiComponent *component;
    GtNapWindow *gtwin;
};

struct _gtnap_window_t
{
    uint32_t id;
    uint32_t parent_id;
    int32_t top;
    int32_t left;
    int32_t bottom;
    int32_t right;
    int32_t cursor_row;
    int32_t cursor_col;
    int32_t scroll_top;
    int32_t scroll_left;
    int32_t scroll_bottom;
    int32_t scroll_right;
    HB_ITEM *is_editable_block;
    HB_ITEM *confirm_block;
    HB_ITEM *desist_block;
    HB_ITEM *error_date_block;
    bool_t is_configured;
    bool_t is_closed_by_esc;
    bool_t modal_window_alive;
    bool_t buttons_navigation;
    bool_t border;
    uint32_t message_label_id;
    uint32_t default_button;
    GtNapObject *wizard_obj;
    Window *window;
    S2Df panel_size;
    Panel *panel;
    Panel *scrolled_panel;
    GtNapToolbar *toolbar;
    GtNapArea *gtarea;
    uint32_t num_rows;
    ArrPt(GuiComponent) *tabstops;
    ArrPt(GtNapObject) *objects;
    ArrPt(GtNapCallback) *callbacks;
};

struct _gtnap_modal_t
{
    uint64_t timestamp;
    uint32_t close_seconds;
    GtNapWindow *gtwin;
};

struct _gtnap_t
{
    Font *global_font;
    Font *button_font;
    Font *edit_font;
    uint8_t date_digits;
    uint8_t date_chars;
    String *title;
    String *working_path;
    uint32_t rows;
    uint32_t cols;
    real32_t cell_x_sizef;
    real32_t cell_y_sizef;
    real32_t label_y_sizef;
    real32_t button_y_sizef;
    real32_t edit_y_sizef;
    uint64_t modal_timestamp;
    uint32_t modal_delay_seconds;
    GtNapWindow *modal_time_window;
    ArrPt(GtNapWindow) *windows;
    String *debugger_path;
    bool_t debugger_visible;
    GtNapDebugger *debugger;
};

/*---------------------------------------------------------------------------*/

DeclPt(GtNapCallback);
DeclSt(GtNapColumn);
DeclPt(GtNapArea);
DeclPt(GtNapObject);
DeclPt(GtNapWindow);
DeclPt(GuiComponent);

/*---------------------------------------------------------------------------*/

static const GtNapKey KEYMAPS[] = {
    {K_F1, ekKEY_F1, 0},
    {K_F2, ekKEY_F2, 0},
    {K_F3, ekKEY_F3, 0},
    {K_F4, ekKEY_F4, 0},
    {K_F5, ekKEY_F5, 0},
    {K_F6, ekKEY_F6, 0},
    {K_F7, ekKEY_F7, 0},
    {K_F8, ekKEY_F8, 0},
    {K_F9, ekKEY_F9, 0},
    {K_F10, ekKEY_F10, 0},
    {K_F11, ekKEY_F11, 0},
    {K_F12, ekKEY_F12, 0},

    {K_ENTER, ekKEY_RETURN, 0},
    {K_SPACE, ekKEY_SPACE, 0},

    {'a', ekKEY_A, 0},
    {'b', ekKEY_B, 0},
    {'c', ekKEY_C, 0},
    {'d', ekKEY_D, 0},
    {'e', ekKEY_E, 0},
    {'f', ekKEY_F, 0},
    {'g', ekKEY_G, 0},
    {'h', ekKEY_H, 0},
    {'i', ekKEY_I, 0},
    {'j', ekKEY_J, 0},
    {'k', ekKEY_K, 0},
    {'l', ekKEY_L, 0},
    {'m', ekKEY_M, 0},
    {'n', ekKEY_N, 0},
    {'o', ekKEY_O, 0},
    {'p', ekKEY_P, 0},
    {'q', ekKEY_Q, 0},
    {'r', ekKEY_R, 0},
    {'s', ekKEY_S, 0},
    {'t', ekKEY_T, 0},
    {'u', ekKEY_U, 0},
    {'v', ekKEY_V, 0},
    {'w', ekKEY_W, 0},
    {'x', ekKEY_X, 0},
    {'y', ekKEY_Y, 0},
    {'z', ekKEY_Z, 0},

    {'A', ekKEY_A, 0},
    {'B', ekKEY_B, 0},
    {'C', ekKEY_C, 0},
    {'D', ekKEY_D, 0},
    {'E', ekKEY_E, 0},
    {'F', ekKEY_F, 0},
    {'G', ekKEY_G, 0},
    {'H', ekKEY_H, 0},
    {'I', ekKEY_I, 0},
    {'J', ekKEY_J, 0},
    {'K', ekKEY_K, 0},
    {'L', ekKEY_L, 0},
    {'M', ekKEY_M, 0},
    {'N', ekKEY_N, 0},
    {'O', ekKEY_O, 0},
    {'P', ekKEY_P, 0},
    {'Q', ekKEY_Q, 0},
    {'R', ekKEY_R, 0},
    {'S', ekKEY_S, 0},
    {'T', ekKEY_T, 0},
    {'U', ekKEY_U, 0},
    {'V', ekKEY_V, 0},
    {'W', ekKEY_W, 0},
    {'X', ekKEY_X, 0},
    {'Y', ekKEY_Y, 0},
    {'Z', ekKEY_Z, 0},

    {'0', ekKEY_0, 0},
    {'1', ekKEY_1, 0},
    {'2', ekKEY_2, 0},
    {'3', ekKEY_3, 0},
    {'4', ekKEY_4, 0},
    {'5', ekKEY_5, 0},
    {'6', ekKEY_6, 0},
    {'7', ekKEY_7, 0},
    {'8', ekKEY_8, 0},
    {'9', ekKEY_9, 0}};

/*---------------------------------------------------------------------------*/

/* Identify the main window in a debugging process */
static const uint32_t i_MAIN_WINDOW_HASH = 1234567;
static const uint32_t i_DEBUGGER_WINDOW_HASH = 7654321;
static const bool_t i_LOG_HBFUNCS = FALSE;
static const bool_t i_FULL_HBFUNCS = FALSE;

/* Harbour colors sensible to light/dark themes */
static color_t i_COLORS[16];

static const real32_t i_FONT_SIZE_DIFF = 0.7f;

/*---------------------------------------------------------------------------*/

__EXTERN_C

/*
 * These are internal, non-documented functions of NAppGUI.
 * They are used for direct handling of widgets, avoiding the 'layout' layer.
 */
void _component_detach_from_panel(GuiComponent *panel_component, GuiComponent *child_component);
void _component_set_frame(GuiComponent *component, const V2Df *origin, const S2Df *size);
void _component_visible(GuiComponent *component, const bool_t visible);
void _component_destroy(GuiComponent **component);
void _component_taborder(GuiComponent *component, Window *window);
const char_t *_component_type(const GuiComponent *component);
void *_component_ositem(const GuiComponent *component);
void _panel_attach_component(Panel *panel, GuiComponent *component);
void _panel_dettach_component(Panel *panel, GuiComponent *component);
void _panel_destroy_component(Panel *panel, GuiComponent *component);
void _panel_compose(Panel *panel, const S2Df *required_size, S2Df *final_size);
void _panel_locate(Panel *panel);
void _window_taborder(Window *window, void *ositem);
void _window_focus(Window *window, GuiComponent *component);
void _panel_content_size(Panel *panel, const real32_t width, const real32_t height);
View *_view_create(const uint32_t flags);

__END_C

/*---------------------------------------------------------------------------*/

static GtNap *GTNAP_GLOBAL = NULL;
static char_t INIT_TITLE[128];
static PHB_ITEM INIT_CODEBLOCK = NULL;
static uint32_t INIT_ROWS = 0;
static uint32_t INIT_COLS = 0;

#define STATIC_TEXT_SIZE 1024
char_t TEMP_BUFFER[STATIC_TEXT_SIZE];

/*---------------------------------------------------------------------------*/

static void i_destroy_callback(GtNapCallback **callback)
{
    cassert_no_null(callback);
    cassert_no_null(*callback);
    if ((*callback)->block != NULL)
        hb_itemRelease((*callback)->block);
    heap_delete(callback, GtNapCallback);
}

/*---------------------------------------------------------------------------*/

static void i_remove_column(GtNapColumn *column)
{
    cassert_no_null(column);
    str_destopt(&column->title);
    if (column->block != NULL)
        hb_itemRelease(column->block);
}

/*---------------------------------------------------------------------------*/

static void i_remove_toolbar(GtNapToolbar *toolbar, Panel *panel, const bool_t is_configured)
{
    cassert_no_null(toolbar);
    arrpt_foreach(item, toolbar->items, GuiComponent)
        if (item != NULL)
        {
            GuiComponent *ditem = item;

            if (is_configured == TRUE)
                _panel_destroy_component(panel, ditem);
            else
                _component_destroy(&ditem);
        }
    arrpt_end();
    arrpt_destroy(&toolbar->items, NULL, GuiComponent);
}

/*---------------------------------------------------------------------------*/

static void i_destroy_area(GtNapArea **area)
{
    cassert_no_null(area);
    cassert_no_null(*area);
    arrst_destroy(&(*area)->records, NULL, uint32_t);

    if ((*area)->while_block)
    {
        hb_itemRelease((*area)->while_block);
        (*area)->while_block = NULL;
    }

    heap_delete(area, GtNapArea);
}

/*---------------------------------------------------------------------------*/

static void i_destroy_gtobject(GtNapWindow *gtwin, const uint32_t index)
{
    GtNapObject *gtobj = NULL;
    cassert_no_null(gtwin);
    gtobj = arrpt_get(gtwin->objects, index, GtNapObject);

    _component_visible(gtobj->component, FALSE);

    if (gtwin->is_configured == TRUE)
    {
        if (gtobj->in_scroll == TRUE)
            _panel_destroy_component(gtwin->scrolled_panel, gtobj->component);
        else
            _panel_destroy_component(gtwin->panel, gtobj->component);
    }
    else
    {
        _component_destroy(&gtobj->component);
    }

    str_destopt(&gtobj->text);

    if (gtobj->text_block != NULL)
        hb_itemRelease(gtobj->text_block);

    if (gtobj->get_set_block != NULL)
        hb_itemRelease(gtobj->get_set_block);

    if (gtobj->is_editable_block != NULL)
        hb_itemRelease(gtobj->is_editable_block);

    if (gtobj->when_block != NULL)
        hb_itemRelease(gtobj->when_block);

    if (gtobj->valida_block != NULL)
        hb_itemRelease(gtobj->valida_block);

    if (gtobj->message_block != NULL)
        hb_itemRelease(gtobj->message_block);

    if (gtobj->keyfilter_block != NULL)
        hb_itemRelease(gtobj->keyfilter_block);

    if (gtobj->auto_block != NULL)
        hb_itemRelease(gtobj->auto_block);

    if (gtobj->wizard_block != NULL)
        hb_itemRelease(gtobj->wizard_block);

    arrst_destopt(&gtobj->columns, i_remove_column, GtNapColumn);

    heap_delete(&gtobj, GtNapObject);
    arrpt_delete(gtwin->objects, index, NULL, GtNapObject);
}

/*---------------------------------------------------------------------------*/

static void i_destroy_gtwin(GtNapWindow **dgtwin)
{
    GtNapWindow *gtwin = NULL;
    cassert_no_null(dgtwin);
    cassert_no_null(*dgtwin);
    gtwin = *dgtwin;

    {
        uint32_t i, n = arrpt_size(gtwin->objects, GtNapObject);
        for (i = 0; i < n; ++i)
            i_destroy_gtobject(gtwin, 0);
    }

    if (gtwin->scrolled_panel != NULL)
    {
        _component_visible((GuiComponent *)gtwin->scrolled_panel, FALSE);

        if (gtwin->is_configured == TRUE)
            _panel_destroy_component(gtwin->panel, (GuiComponent *)gtwin->scrolled_panel);
        else
            _component_destroy((GuiComponent **)&gtwin->scrolled_panel);
    }

    if (gtwin->toolbar != NULL)
    {
        i_remove_toolbar(gtwin->toolbar, gtwin->panel, gtwin->is_configured);
        heap_delete(&gtwin->toolbar, GtNapToolbar);
    }

    if (gtwin->gtarea != NULL)
        i_destroy_area(&gtwin->gtarea);

    if (gtwin->is_editable_block != NULL)
        hb_itemRelease(gtwin->is_editable_block);

    if (gtwin->confirm_block != NULL)
        hb_itemRelease(gtwin->confirm_block);

    if (gtwin->desist_block != NULL)
        hb_itemRelease(gtwin->desist_block);

    if (gtwin->error_date_block != NULL)
        hb_itemRelease(gtwin->error_date_block);

    cassert(arrpt_size(gtwin->objects, GtNapObject) == 0);
    arrpt_destroy(&gtwin->tabstops, NULL, GuiComponent);
    arrpt_destroy(&gtwin->objects, NULL, GtNapObject);
    arrpt_destroy(&gtwin->callbacks, i_destroy_callback, GtNapCallback);

    if (gtwin->parent_id == UINT32_MAX)
    {
        window_destroy(&gtwin->window);
    }
    else
    {
        cassert(gtwin->window == NULL);
        if (gtwin->panel != NULL)
            _component_destroy((GuiComponent **)&gtwin->panel);
    }

    heap_delete(dgtwin, GtNapWindow);
}

/*---------------------------------------------------------------------------*/

static void i_gtnap_destroy(GtNap **gtnap)
{
    cassert_no_null(gtnap);
    cassert_no_null(*gtnap);
    cassert(*gtnap == GTNAP_GLOBAL);
    arrpt_destroy(&(*gtnap)->windows, i_destroy_gtwin, GtNapWindow);
    font_destroy(&(*gtnap)->global_font);
    font_destroy(&(*gtnap)->button_font);
    font_destroy(&(*gtnap)->edit_font);
    str_destroy(&(*gtnap)->title);
    str_destroy(&(*gtnap)->working_path);
    str_destroy(&(*gtnap)->debugger_path);
    if ((*gtnap)->debugger != NULL)
        nap_debugger_destroy(&(*gtnap)->debugger);
    heap_delete(&(*gtnap), GtNap);
}

/*---------------------------------------------------------------------------*/

static GtNapWindow *i_gtwin(GtNap *gtnap, const uint32_t wid)
{
    cassert_no_null(gtnap);
    arrpt_foreach(gtwin, gtnap->windows, GtNapWindow)
        if (gtwin->id == wid)
            return gtwin;
    arrpt_end()
    cassert_msg(FALSE, "Invalid window id");
    return NULL;
}

/*---------------------------------------------------------------------------*/

static uint32_t i_gtwin_index(GtNap *gtnap, const uint32_t wid)
{
    cassert_no_null(gtnap);
    arrpt_foreach(gtwin, gtnap->windows, GtNapWindow)
        if (gtwin->id == wid)
            return gtwin_i;
    arrpt_end()
    cassert(FALSE);
    return UINT32_MAX;
}

/*---------------------------------------------------------------------------*/

static Window *i_effective_window(GtNapWindow *gtwin, GtNap *gtnap)
{
    cassert_no_null(gtwin);
    cassert_no_null(gtnap);
    if (gtwin->parent_id == UINT32_MAX)
    {
        cassert_no_null(gtwin->window);
        return gtwin->window;
    }
    else
    {
        GtNapWindow *gtparent = i_gtwin(gtnap, gtwin->parent_id);
        cassert_no_null(gtparent->window);
        return gtparent->window;
    }
}

/*---------------------------------------------------------------------------*/

bool_t i_gtwin_alive(GtNapWindow *gtwin, GtNap *gtnap)
{
    cassert_no_null(gtnap);
    arrpt_foreach(win, gtnap->windows, GtNapWindow)
        if (win == gtwin)
            return TRUE;
    arrpt_end();
    return FALSE;
}

/*---------------------------------------------------------------------------*/

/* TODO: TO BE REMOVED */
static GtNapWindow *i_current_gtwin(GtNap *gtnap)
{
    uint32_t id = 0;
    cassert_no_null(gtnap);
    id = arrpt_size(gtnap->windows, GtNapWindow);
    if (id >= 1)
        return arrpt_get(gtnap->windows, id - 1, GtNapWindow);
    return NULL;
}

/*---------------------------------------------------------------------------*/

/* TODO: TO BE REMOVED */
static GtNapWindow *i_current_main_gtwin(GtNap *gtnap)
{
    cassert_no_null(gtnap);
    arrpt_forback(gtwin, gtnap->windows, GtNapWindow)
        if (gtwin->parent_id == UINT32_MAX)
            return gtwin;
    arrpt_end()
    return NULL;
}

/*---------------------------------------------------------------------------*/

static GtNapObject *i_gtobj(GtNap *gtnap, const uint32_t wid, const uint32_t id)
{
    GtNapWindow *gtwin = i_gtwin(gtnap, wid);
    cassert_no_null(gtwin);
    return arrpt_get(gtwin->objects, id, GtNapObject);
}

/*---------------------------------------------------------------------------*/

static GtNapObject *i_get_button(GtNapWindow *gtwin, const uint32_t index)
{
    uint32_t i = 0;
    cassert_no_null(gtwin);
    arrpt_foreach(obj, gtwin->objects, GtNapObject)
        if (obj->type == ekOBJ_BUTTON)
        {
            if (i == index)
                return obj;
            i += 1;
        }
    arrpt_end();
    cassert(FALSE);
    return NULL;
}

/*---------------------------------------------------------------------------*/

static uint32_t i_num_buttons(const GtNapWindow *gtwin)
{
    uint32_t n = 0;
    cassert_no_null(gtwin);
    arrpt_foreach_const(obj, gtwin->objects, GtNapObject)
        if (obj->type == ekOBJ_BUTTON)
            n += 1;
    arrpt_end();
    return n;
}

/*---------------------------------------------------------------------------*/

static uint32_t i_num_images(const GtNapWindow *gtwin)
{
    uint32_t n = 0;
    cassert_no_null(gtwin);
    arrpt_foreach_const(obj, gtwin->objects, GtNapObject)
        if (obj->type == ekOBJ_IMAGE)
            n += 1;
    arrpt_end();
    return n;
}

/*---------------------------------------------------------------------------*/

static uint32_t i_num_edits(const GtNapWindow *gtwin)
{
    uint32_t n = 0;
    cassert_no_null(gtwin);
    arrpt_foreach_const(obj, gtwin->objects, GtNapObject)
        if (obj->type == ekOBJ_EDIT)
            n += 1;
    arrpt_end();
    return n;
}

/*---------------------------------------------------------------------------*/

static uint32_t i_num_texts(const GtNapWindow *gtwin)
{
    uint32_t n = 0;
    cassert_no_null(gtwin);
    arrpt_foreach_const(obj, gtwin->objects, GtNapObject)
        if (obj->type == ekOBJ_TEXTVIEW)
            n += 1;
    arrpt_end();
    return n;
}

/*---------------------------------------------------------------------------*/

static void i_set_defbutton(GtNapWindow *gtwin)
{
    GtNapObject *button = i_get_button(gtwin, gtwin->default_button);
    if (button != NULL)
        window_defbutton(gtwin->window, (Button *)button->component);
}

/*---------------------------------------------------------------------------*/

static Listener *i_gtnap_listener(HB_ITEM *block, const int32_t key, const uint32_t autoclose_id, GtNapWindow *gtwin, FPtr_gtnap_callback func_callback)
{
    GtNapCallback *callback = heap_new0(GtNapCallback);
    cassert_no_null(gtwin);
    callback->block = block ? hb_itemNew(block) : NULL;
    callback->gtwin = gtwin;
    callback->key = key;
    callback->autoclose_id = autoclose_id;
    arrpt_append(gtwin->callbacks, callback, GtNapCallback);
    return listener(callback, func_callback, GtNapCallback);
}

/*---------------------------------------------------------------------------*/

/* Change this value to make buttons higher */
static real32_t i_button_vpadding(void)
{
    return 0;
}

/*---------------------------------------------------------------------------*/

/* Change this value to make edits higher */
static real32_t i_edit_vpadding(void)
{
    return 0;
}

/*---------------------------------------------------------------------------*/

static real32_t i_button_size(const Font *font)
{
    /* Create an impostor window, only for measure the real height of buttons */
    real32_t bh = 0;
    Panel *panel = panel_create();
    Button *button = button_push();
    Window *window = window_create(ekWINDOW_STD | ekWINDOW_OFFSCREEN);
    Layout *layout = layout_create(1, 1);
    button_text(button, "DEMO");
    button_font(button, font);
    button_vpadding(button, i_button_vpadding());
    layout_button(layout, button, 0, 0);
    panel_layout(panel, layout);
    window_panel(window, panel);
    window_show(window);
    bh = button_get_height(button);
    window_destroy(&window);
    return bh;
}

/*---------------------------------------------------------------------------*/

static real32_t i_edit_size(const Font *font)
{
    /* Create an impostor window, only for measure the real height of editbox */
    real32_t eh = 0;
    Panel *panel = panel_create();
    Edit *edit = edit_create();
    Window *window = window_create(ekWINDOW_STD | ekWINDOW_OFFSCREEN);
    Layout *layout = layout_create(1, 1);
    edit_font(edit, font);
    edit_vpadding(edit, i_edit_vpadding());
    layout_edit(layout, edit, 0, 0);
    panel_layout(panel, layout);
    window_panel(window, panel);
    window_show(window);
    eh = edit_get_height(edit);
    window_destroy(&window);
    return eh;
}

/*---------------------------------------------------------------------------*/

static void i_compute_cell_size(GtNap *gtnap, const real32_t size, const real32_t width)
{
    real32_t fw = 0, fh = 0;
    real32_t bs = 0, es = 0;
    real32_t bh = 0, eh = 0;

    cassert_no_null(gtnap);
    ptr_destopt(font_destroy, &gtnap->global_font, Font);
    gtnap->global_font = font_monospace(size, ekFCELL);
    font_set_width(gtnap->global_font, width);

    /* Compute the real size of a cell, based on font */
    {
        real32_t w, h;
        const char_t *reftext = "exibicao/edicao de texto em memoria";
        font_extents(gtnap->global_font, reftext, -1, &w, &h);
        fw = w / str_len_c(reftext);
        fh = h;
    }

    /*
     * We need to set a font size for buttons, where the button
     * height doesn't exceed the cell height
     */
    bs = size;
    bh = 1e8f;
    while (bh - fh > i_FONT_SIZE_DIFF)
    {
        bs -= 1;
        ptr_destopt(font_destroy, &gtnap->button_font, Font);
        gtnap->button_font = font_monospace(bs, ekFCELL);
        bh = i_button_size(gtnap->button_font);
    }

    /*
     * We need to set a font size for edits, where the edit
     * height doesn't exceed the cell height
     */
    es = size;
    eh = 1e8f;
    while (eh - fh > i_FONT_SIZE_DIFF)
    {
        es -= 1;
        ptr_destopt(font_destroy, &gtnap->edit_font, Font);
        gtnap->edit_font = font_monospace(es, ekFCELL);
        eh = i_edit_size(gtnap->edit_font);
    }

    /* Final cell sizes */
    gtnap->cell_x_sizef = fw;
    gtnap->cell_y_sizef = fh;
    gtnap->label_y_sizef = fh;
    gtnap->button_y_sizef = bh;
    gtnap->edit_y_sizef = eh;
    cassert(gtnap->label_y_sizef <= gtnap->cell_y_sizef);
    cassert(gtnap->button_y_sizef <= gtnap->cell_y_sizef);
    cassert(gtnap->edit_y_sizef <= gtnap->cell_y_sizef);
}

/*---------------------------------------------------------------------------*/

/*
 * This function to select the font size has been cloned from original
 * 'cuademo::outros.prg', working under GTWVW.
 */
static void i_compute_font_size(const real32_t screen_width, const real32_t screen_height, GtNap *gtnap)
{
    real32_t N_HeightMin = 0; /* altura  mínima que, com certeza, cabe na menor resolução */
    real32_t N_WidthMin = 0;  /* largura mínima que, com certeza, cabe na menor resolução */
    real32_t N_HeightMax = 0; /* altura  máxima que ultrapassa a maior resolução */
    real32_t N_WidthMax = 0;  /* largura máxima que ultrapassa a maior resolução */
    real32_t N_HeightTemp = 0;
    real32_t N_WidthTemp = 0;
    const char_t *C_Fonte = NULL;

    /*
     * Resoluções mais comuns:
     * N_ScreenHeight   N_ScreenWidth
     *    >= 1024         >= 1680
     *    >= 1024         >= 1440
     *    >= 1024         >= 1280
     *    >=  960         >= 1280
     *    >=  864         >= 1152
     *    >=  768         >= 1024
     *    >=  600         >= 1024   (NetBooks)
     *    >=  600         >=  800   (SuperVGA)
     *
     */
    if (screen_height <= 600)
    {
        /*
         * Testes práticos mostraram que, em resolução vertical baixa (altura),
         * o fonte "Courier New" apresenta um serrilhado que torna a leitura das letras
         * difícil. Nesta situação, o "Lucida Console" tem aparência bem melhor.
         */

        /*
         * Lucida Console não está presente no Linux ou macOS. Nestes casos,
         * deixamos a fonte monoespaçada como padrão.
         */
#if defined(__WINDOWS__)
        C_Fonte = "Lucida Console";
#else
        C_Fonte = NULL;
#endif
    }
    else
    {
        /*
         * Já em resolução vertical alta, o "Courier New" não apresenta mais
         * o serrilhado, e tem aparência melhor.
         * A desvantagem do "Lucida Console", neste caso, é que fica muito largo,
         * com aparência pesada e feia.
         */

        /*
         * "Courier New" está presente em praticamente todos os sistemas, incluindo
         * Linux e macOS. Se não estiver presente, a fonte monoespaçada padrão será
         * selecionada.
         */
        C_Fonte = "Courier New";
    }

    if (C_Fonte != NULL)
        font_preferred_monospace(C_Fonte);

    /*
     * - Os valores abaixo foram obtidos por via prática, num computador com
     *   Windows 7 e resolução 600 x 800 (SuperVGA)
     * - Isto é importante, porque a SetMode() só consegue funcionar se o
     *   fonte escolhido efetivamente couber na resolução e tamanhos especificados
     *
     * Usar o valores mínimos para permitir que a SetMode() mude a quantidade de
     * linhas para 35 e colunas para 110, até mesmo na resolução 600 X 800
     */
    N_HeightMin = 6.f;
    N_WidthMin = 5.f;

    /*
     * Usar os valores máximos para ir reduzindo o tamanho até que
     * caiba na resolução da tela (a maior tela até agora foi a 1024 x 1860)
     */
    N_HeightMax = 24.f + 1.f; /* em teste real, só coube 23 */
    N_WidthMax = 15.f + 1.f;  /* em teste real, só coube 15 */

    /*
     * Setar a maior altura de fonte possível, que ainda caiba 35 linhas.
     * Se WVW_MAXMAXROW() continuar com o mesmo valor, é porque não coube
     * na tela, sendo necessário reduzir ainda mais a altura do fonte.
     */
    i_compute_cell_size(gtnap, N_HeightMax, -1);
    N_HeightTemp = N_HeightMax;
    for (;;)
    {
        /* The font computed by the system has not the desired height */
        if (gtnap->cell_y_sizef - N_HeightMax > i_FONT_SIZE_DIFF)
        {
            N_HeightTemp -= 1;
            i_compute_cell_size(gtnap, N_HeightTemp, -1);
        }
        /* The total height exceeds the screen limits */
        else if (gtnap->cell_y_sizef * gtnap->rows > screen_height && N_HeightMax > N_HeightMin)
        {
            N_HeightMax -= 1.f;
            N_HeightTemp -= N_HeightMax;
            i_compute_cell_size(gtnap, N_HeightMax, -1);
        }
        else
        {
            break;
        }
    }
    
    if (N_HeightTemp < N_HeightMax)
        N_HeightMax = N_HeightTemp;
    
    /*
     * Setar a maior largura de fonte possível, que ainda caiba 110 colunas.
     * Se WVW_MAXMAXCOL() continuar com o mesmo valor, é porque não coube
     * na tela, sendo necessário reduzir ainda mais a largura do fonte.
     */
    i_compute_cell_size(gtnap, N_HeightMax, N_WidthMax);
    N_WidthTemp = N_WidthMax;
    for (;;)
    {
        /* The font computed by the system has not the desired width */
        if (gtnap->cell_x_sizef - N_WidthMax > i_FONT_SIZE_DIFF)
        {
            N_WidthTemp -= 1;
            i_compute_cell_size(gtnap, N_HeightMax, N_WidthTemp);
        }
        /* The total width exceeds the screen limits */
        else if (gtnap->cell_x_sizef * gtnap->cols > screen_width && N_WidthMax > N_WidthMin)
        {
            N_WidthMax -= 1.f;
            N_WidthTemp = N_WidthMax;
            i_compute_cell_size(gtnap, N_HeightMax, N_WidthMax);
        }
        else
        {
            break;
        }
    }
}

/*---------------------------------------------------------------------------*/

static uint32_t i_remove_utf8_CR(char_t *utf8)
{
    /* Remove the Carriage Return (CR) character (NAppGUI doesn't like) */
    uint32_t i = 0, j = 0;
    for (; utf8[i] != 0;)
    {
        if (utf8[i] != 13)
        {
            utf8[j] = utf8[i];
            j += 1;
        }

        i += 1;
    }

    utf8[j] = 0;
    return j;
}

/*---------------------------------------------------------------------------*/

static uint32_t i_item_to_utf8(HB_ITEM *item, char_t *utf8, const uint32_t size)
{
    cassert(HB_ITEM_TYPE(item) == HB_IT_STRING);
    hb_itemCopyStrUTF8(item, (char *)utf8, (HB_SIZE)size);
    return i_remove_utf8_CR(utf8);
}

/*---------------------------------------------------------------------------*/

static String *i_item_to_utf8_string(HB_ITEM *item)
{
    HB_SIZE s1 = 0, s2 = 0;
    String *str = NULL;
    cassert(HB_ITEM_TYPE(item) == HB_IT_STRING);
    s1 = hb_itemCopyStrUTF8(item, NULL, (HB_SIZE)UINT32_MAX);
    str = str_reserve((uint32_t)s1);
    s2 = hb_itemCopyStrUTF8(item, tcc(str), s1 + 1);
    cassert_unref(s1 == s2, s2);
    i_remove_utf8_CR(tcc(str));
    return str;
}

/*---------------------------------------------------------------------------*/

static uint32_t i_cp_to_utf8(const char_t *cp_str, char_t *utf8, const uint32_t size)
{
    HB_CODEPAGE *cp = hb_vmCDP();
    cassert_no_null(cp_str);
    cassert_no_null(utf8);
    hb_cdpStrToUTF8(cp, (const char *)cp_str, (HB_SIZE)str_len_c(cp_str), (char *)utf8, (HB_SIZE)size);
    return i_remove_utf8_CR(utf8);
}

/*---------------------------------------------------------------------------*/

static String *i_cp_to_utf8_string(const char_t *cp_str)
{
    /* TODO: Improve, make dynamic */
    char_t utf8[STATIC_TEXT_SIZE];
    String *str = NULL;
    i_cp_to_utf8(cp_str, utf8, sizeof32(utf8));
    str = str_c(utf8);
    return str;
}

/*---------------------------------------------------------------------------*/

static String *i_utf8_to_cp_string(const char_t *utf8)
{
    HB_CODEPAGE *cp = hb_vmCDP();
    HB_SIZE n = (HB_SIZE)str_len_c(utf8);
    HB_SIZE s1 = hb_cdpUTF8AsStrLen(cp, utf8, n, 0);
    String *str = str_reserve((uint32_t)s1);
    HB_SIZE s2 = hb_cdpUTF8ToStr(cp, utf8, n, tcc(str), s1 + 1);
    cassert_unref(s1 == s2, s2);
    return str;
}

/*---------------------------------------------------------------------------*/

static void i_utf8_to_cp(const char_t *utf8, char_t *buffer, const uint32_t size)
{
    HB_CODEPAGE *cp = hb_vmCDP();
    HB_SIZE n = (HB_SIZE)str_len_c(utf8);
    HB_SIZE s1 = hb_cdpUTF8AsStrLen(cp, utf8, n, 0);
    HB_SIZE s2 = hb_cdpUTF8ToStr(cp, utf8, n, buffer, size);
    cassert_unref(s1 == s2, s1);
    cassert_unref(s1 == s2, s2);
}

/*---------------------------------------------------------------------------*/

static uint8_t i_utf8_to_cp_char(const uint32_t codepoint)
{
    char_t utf8[16];
    char_t cpstr[16];
    HB_SIZE s1, s2;
    HB_CODEPAGE *cp = hb_vmCDP();
    uint32_t nb = unicode_to_char(codepoint, utf8, ekUTF8);
    utf8[nb] = 0;
    s1 = hb_cdpUTF8AsStrLen(cp, utf8, (HB_SIZE)nb, sizeof(utf8));
    s2 = hb_cdpUTF8ToStr(cp, utf8, (HB_SIZE)nb, cpstr, sizeof(cpstr));
    cassert_unref(s1 == s2, s1);
    cassert_unref(s1 == s2, s2);
    return (uint8_t)cpstr[0];
}

/*---------------------------------------------------------------------------*/

static GtNap *i_gtnap_create(void)
{
    S2Df screen;
    const char_t *build_cfg = NULL;
    GTNAP_GLOBAL = heap_new0(GtNap);
    GTNAP_GLOBAL->title = i_cp_to_utf8_string(INIT_TITLE);
    GTNAP_GLOBAL->rows = INIT_ROWS;
    GTNAP_GLOBAL->cols = INIT_COLS;
    GTNAP_GLOBAL->windows = arrpt_create(GtNapWindow);
    GTNAP_GLOBAL->date_digits = (hb_setGetCentury() == (HB_BOOL)HB_TRUE) ? 8 : 6;
    GTNAP_GLOBAL->date_chars = GTNAP_GLOBAL->date_digits + 2;

    {
        char_t path[512];
        bfile_dir_work(path, sizeof(path));
        GTNAP_GLOBAL->working_path = str_c(path);
    }

#if defined(__DEBUG__)
    build_cfg = "Debug";
#else
    build_cfg = "Release";
#endif

    {
        const char_t *debpath = deblib_path();
#if defined(__MACOS__)
        GTNAP_GLOBAL->debugger_path = str_cpath("%s/%s/bin/gtnapdeb.app/Contents/MacOS/gtnapdeb", debpath, build_cfg);
#else
        GTNAP_GLOBAL->debugger_path = str_cpath("%s/%s/bin/gtnapdeb", debpath, build_cfg);
#endif
        GTNAP_GLOBAL->debugger_visible = FALSE;
        GTNAP_GLOBAL->debugger = NULL;
    }

    globals_resolution(&screen);
    i_compute_font_size(screen.width, screen.height, GTNAP_GLOBAL);
    deblib_init_colors(i_COLORS);

    {
        PHB_ITEM ritem = hb_itemDo(INIT_CODEBLOCK, 0);
        hb_itemRelease(ritem);
        hb_itemRelease(INIT_CODEBLOCK);
    }

    INIT_TITLE[0] = 0;
    INIT_CODEBLOCK = NULL;

    return GTNAP_GLOBAL;
}

/*---------------------------------------------------------------------------*/

static bool_t i_with_scroll_panel(const GtNapWindow *gtwin)
{
    cassert_no_null(gtwin);
    if (gtwin->scroll_top >= 0)
        return TRUE;

    cassert(gtwin->scroll_top == INT32_MIN);
    cassert(gtwin->scroll_left == INT32_MIN);
    cassert(gtwin->scroll_bottom == INT32_MIN);
    cassert(gtwin->scroll_right == INT32_MIN);
    return FALSE;
}

/*---------------------------------------------------------------------------*/

static S2Df i_scroll_content_size(const ArrPt(GtNapObject) *objects)
{
    real32_t min_x = 1e10f;
    real32_t min_y = 1e10f;
    real32_t max_x = -1e10f;
    real32_t max_y = -1e10f;

    arrpt_foreach_const(object, objects, GtNapObject)
        if (object->in_scroll == TRUE)
        {
            real32_t x1 = object->pos.x;
            real32_t x2 = object->pos.x + object->size.width;
            real32_t y1 = object->pos.y;
            real32_t y2 = object->pos.y + object->size.height;
            if (x1 < min_x)
                min_x = x1;
            if (x2 > max_x)
                max_x = x2;
            if (y1 < min_y)
                min_y = y1;
            if (y2 > max_y)
                max_y = y2;
        }
    arrpt_end();

    return s2df((max_x - min_x) + GTNAP_GLOBAL->cell_x_sizef, (max_y - min_y) + GTNAP_GLOBAL->cell_y_sizef);
}

/*---------------------------------------------------------------------------*/

static void i_attach_to_panel(ArrPt(GtNapObject) *objects, Panel *main_panel, Panel *scroll_panel, const V2Df *scroll_offset, const objtype_t type, const GtNapToolbar *toolbar)
{
    cassert_no_null(scroll_offset);
    arrpt_foreach(object, objects, GtNapObject)
        if (object->type == type)
        {
            V2Df pos = object->pos;

            if (object->in_scroll == TRUE)
                _panel_attach_component(scroll_panel, object->component);
            else
                _panel_attach_component(main_panel, object->component);

            _component_visible(object->component, FALSE);

            if (toolbar != NULL)
            {
                switch (type)
                {
                case ekOBJ_LABEL:
                    pos.y += toolbar->heightf;
                    if (GTNAP_GLOBAL->cell_y_sizef > GTNAP_GLOBAL->label_y_sizef)
                        pos.y += (GTNAP_GLOBAL->cell_y_sizef - GTNAP_GLOBAL->label_y_sizef) / 2.f;
                    break;

                case ekOBJ_EDIT:
                    pos.y += toolbar->heightf;
                    if (GTNAP_GLOBAL->cell_y_sizef > GTNAP_GLOBAL->edit_y_sizef)
                        pos.y += (GTNAP_GLOBAL->cell_y_sizef - GTNAP_GLOBAL->edit_y_sizef) / 2.f;

                    if (object->in_scroll == TRUE)
                    {
                        object->size.width -= GTNAP_GLOBAL->cell_x_sizef;
                    }
                    break;

                case ekOBJ_IMAGE:
                case ekOBJ_MENU:
                    pos.y += toolbar->heightf;
                    break;
                case ekOBJ_TABLEVIEW:
                case ekOBJ_TEXTVIEW:
                    break;
                case ekOBJ_BUTTON:
                    if (object->editBoxIndexForButton != UINT32_MAX)
                    {
                        /* The same as related editbox */
                        pos.y += toolbar->heightf;
                    }

                    if (GTNAP_GLOBAL->cell_y_sizef > GTNAP_GLOBAL->button_y_sizef)
                        pos.y += (GTNAP_GLOBAL->cell_y_sizef - GTNAP_GLOBAL->button_y_sizef) / 2.f;
                    break;
                    cassert_default();
                }
            }

            if (object->in_scroll == TRUE)
            {
                pos.x += scroll_offset->x;
                pos.y += scroll_offset->y;
            }

            object->pos = pos;
            _component_set_frame(object->component, &pos, &object->size);
        }
    arrpt_end();
}

/*---------------------------------------------------------------------------*/

static void i_attach_toolbar_to_panel(const GtNapToolbar *toolbar, Panel *panel)
{
    if (toolbar != NULL)
    {
        V2Df p1, p2;
        S2Df s1, s2;
        real32_t bsize = toolbar->button_widthf + 4.f;
        p1.x = 0;
        p1.y = (toolbar->heightf - bsize) / 2.f;
        p2.x = 0;
        p2.y = p1.y;
        s1.width = bsize;
        s1.height = bsize;
        s2.width = 1;
        s2.height = bsize;

        arrpt_foreach(item, toolbar->items, GuiComponent)
            if (item != NULL)
            {
                const char_t *type = _component_type(item);
                _panel_attach_component(panel, item);
                _component_visible(item, FALSE);
                if (str_equ_c(type, "Button") == TRUE)
                {
                    _component_set_frame(item, &p1, &s1);
                    p1.x += s1.width;
                    p2.x = p1.x;
                }
                else
                {
                    cassert(str_equ_c(type, "View") == TRUE);
                    p2.x += 2;
                    _component_set_frame(item, &p2, &s2);
                    p2.x += 3;
                    p1.x = p2.x;
                }
            }
        arrpt_end();
    }
}

/*---------------------------------------------------------------------------*/

static void i_component_tabstop(ArrPt(GtNapObject) *objects, Window *window, ArrPt(GuiComponent) *tabstops, const objtype_t type)
{
    arrpt_foreach(object, objects, GtNapObject)
        if (object->type == type)
        {
            _component_visible(object->component, TRUE);

            switch (object->type)
            {
            case ekOBJ_LABEL:
            case ekOBJ_IMAGE:
                break;
            case ekOBJ_BUTTON:
                /* Buttons don't have tabstop
                   _component_taborder(object->component, window); */
                break;
            case ekOBJ_MENU:
            {
                View *view = nap_menuvert_view((Panel *)object->component);
                nap_menuvert_window((Panel *)object->component, window);
                arrpt_append(tabstops, (GuiComponent *)view, GuiComponent);
                break;
            }
            case ekOBJ_TABLEVIEW:
            case ekOBJ_TEXTVIEW:
            case ekOBJ_EDIT:
                arrpt_append(tabstops, object->component, GuiComponent);
                break;
                cassert_default();
            }
        }
    arrpt_end();
}

/*---------------------------------------------------------------------------*/

static void i_toolbar_tabstop(GtNapToolbar *toolbar, ArrPt(GuiComponent) *tabstops)
{
    if (toolbar != NULL)
    {
        /* At the moment, toolbar buttons not have tabstop */
        unref(tabstops);
        arrpt_foreach(item, toolbar->items, GuiComponent)
            if (item != NULL)
                _component_visible(item, TRUE);
        arrpt_end();
    }
}

/*---------------------------------------------------------------------------*/

static void i_OnPreviousEdit(GtNapWindow *gtwin, Event *e)
{
    unref(e);
    window_previous_tabstop(gtwin->window);
}

/*---------------------------------------------------------------------------*/

static void i_OnNextTabstop(GtNapWindow *gtwin, Event *e)
{
    unref(e);
    cassert_no_null(gtwin);

    if (i_gtwin_alive(gtwin, GTNAP_GLOBAL) == FALSE)
        return;

    window_next_tabstop(gtwin->window);
}

/*---------------------------------------------------------------------------*/

static void i_OnLeftButton(GtNapWindow *gtwin, Event *e)
{
    unref(e);
    cassert_no_null(gtwin);
    if (gtwin->default_button > 0)
    {
        gtwin->default_button -= 1;
        i_set_defbutton(gtwin);
    }
}

/*---------------------------------------------------------------------------*/

static void i_OnRightButton(GtNapWindow *gtwin, Event *e)
{
    unref(e);
    cassert_no_null(gtwin);
    if (gtwin->default_button < i_num_buttons(gtwin) - 1)
    {
        gtwin->default_button += 1;
        i_set_defbutton(gtwin);
    }
}

/*---------------------------------------------------------------------------*/

/* Run the codeBlock that updates after a text entry in EditBox */
static void i_update_harbour_from_edit_text(const GtNapObject *gtobj)
{
    cassert_no_null(gtobj);
    if (gtobj->get_set_block != NULL)
    {
        PHB_ITEM pItem = NULL;
        const char_t *text = edit_get_text((Edit *)gtobj->component);

        if (gtobj->dtype == ekTYPE_CHARACTER)
        {
            char_t cp[STATIC_TEXT_SIZE];
            uint32_t i, len = 0;
            i_utf8_to_cp(text, cp, sizeof(cp));
            len = str_len_c(cp);
            for (i = len; i < gtobj->max_chars; ++i)
                cp[i] = ' ';
            cp[gtobj->max_chars] = 0;
            pItem = hb_itemPutC(NULL, cp);
        }
        else if (gtobj->dtype == ekTYPE_DATE)
        {
            pItem = hb_itemPutDS(NULL, text);
        }
        else
        {
            cassert_msg(FALSE, "Unknown data type in i_update_harbour_from_edit_text");
        }

        if (pItem != NULL)
        {
            PHB_ITEM ritem = hb_itemDo(gtobj->get_set_block, 1, pItem);
            hb_itemRelease(pItem);
            hb_itemRelease(ritem);
        }
    }
}

/*---------------------------------------------------------------------------*/

static void i_set_label_text(GtNapObject *obj, const char_t *utf8_text)
{
    uint32_t nchars = UINT32_MAX;
    cassert_no_null(obj);
    cassert(obj->type == ekOBJ_LABEL);
    if (utf8_text != NULL)
    {
        nchars = unicode_nchars(utf8_text, ekUTF8);
        label_text((Label *)obj->component, utf8_text);
    }
    else if (obj->text != NULL)
    {
        nchars = str_nchars(obj->text);
        label_text((Label *)obj->component, tc(obj->text));
    }
    else if (obj->text_block != NULL)
    {
        PHB_ITEM ritem = hb_itemDo(obj->text_block, 0);
        HB_TYPE type = HB_ITEM_TYPE(ritem);

        if (type == HB_IT_STRING)
        {
            char_t buffer[STATIC_TEXT_SIZE];
            i_item_to_utf8(ritem, buffer, sizeof32(buffer));
            nchars = unicode_nchars(buffer, ekUTF8);
            label_text((Label *)obj->component, buffer);
        }

        hb_itemRelease(ritem);
    }

    /* Text has been updated */
    if (nchars != UINT32_MAX)
    {
        obj->size.width = (real32_t)nchars * GTNAP_GLOBAL->cell_x_sizef;
        if (obj->gtwin->is_configured == TRUE)
            _component_set_frame(obj->component, &obj->pos, &obj->size);
    }
}

/*---------------------------------------------------------------------------*/

static void i_stop_modal(GtNap *gtnap, GtNapWindow *gtwin, const uint32_t retcode)
{
    unref(gtnap);
    cassert_no_null(gtwin);
    gtwin->modal_window_alive = FALSE;
    window_stop_modal(gtwin->window, retcode);
}

/*---------------------------------------------------------------------------*/

static void i_OnEditChange(GtNapObject *gtobj, Event *e)
{
    const EvText *p = event_params(e, EvText);
    GtNapWindow *gtwin = NULL;
    FocusInfo finfo;

    cassert_no_null(gtobj);
    cassert(gtobj->type == ekOBJ_EDIT);
    gtwin = gtobj->gtwin;
    cassert_no_null(gtwin);
    window_focus_info(gtwin->window, &finfo);

    /* Current window is in close process by 'window_stop_modal'. Validations should not be performed */
    if (gtwin->modal_window_alive == FALSE)
        return;

    /* Update Harbour with the content of the EditBox */
    i_update_harbour_from_edit_text(gtobj);

    /* The editbox has a validation code block */
    if (gtobj->valida_block != NULL)
    {
        bool_t valid = TRUE;
        PHB_ITEM ritem = hb_itemDo(gtobj->valida_block, 0);
        HB_TYPE type = HB_ITEM_TYPE(ritem);
        cassert_unref(type == HB_IT_LOGICAL, type);
        valid = (bool_t)hb_itemGetL(ritem);
        hb_itemRelease(ritem);

        /* If the input is not valid --> The editbox keep the focus and event finish here */
        if (valid == FALSE)
        {
            bool_t *r = event_result(e, bool_t);
            *r = FALSE;
            return;
        }
    }

    /* The window has a global function to process invalid date */
    if (gtobj->dtype == ekTYPE_DATE)
    {
        if (gtwin->error_date_block != NULL)
        {
            long date_ok = hb_dateUnformat(p->text, hb_setGetDateFormat());

            /* Date invalid --> The editbox keep the focus and event finish here */
            if (date_ok == 0)
            {
                PHB_ITEM ritem = NULL;
                bool_t *r = event_result(e, bool_t);
                ritem = hb_itemDo(gtwin->error_date_block, 0);
                hb_itemRelease(ritem);
                *r = FALSE;
                return;
            }
        }
    }

    /* Update possible labels associated with this input */
    arrpt_foreach(obj, gtwin->objects, GtNapObject)
        if (obj->type == ekOBJ_LABEL)
            i_set_label_text(obj, NULL);
    arrpt_end();

    /* If user have pressed the [ESC] key, we leave the stop for that event */
    if (gtwin->is_closed_by_esc == FALSE)
    {
        /* The last editbox has lost the focus --> Close the window */
        if (gtobj->is_last_edit == TRUE)
        {
            /* The user has explicity intro the editbox value */
            if (finfo.action == ekGUI_TAB_KEY || finfo.action == ekGUI_TAB_NEXT)
            {
                bool_t close = TRUE;

                /* We have asociated a confirmation block */
                if (gtwin->confirm_block != NULL)
                {
                    PHB_ITEM ritem = hb_itemDo(gtwin->confirm_block, 0);
                    HB_TYPE type = HB_ITEM_TYPE(ritem);
                    cassert_unref(type == HB_IT_LOGICAL, type);
                    close = (bool_t)hb_itemGetL(ritem);
                    hb_itemRelease(ritem);
                }

                if (close == TRUE)
                    i_stop_modal(GTNAP_GLOBAL, gtwin, NAP_MODAL_LAST_INPUT);
            }
        }
    }
}

/*---------------------------------------------------------------------------*/

static bool_t i_is_editable(GtNapWindow *gtwin, GtNapObject *gtobj)
{
    bool_t editable = TRUE;
    cassert_no_null(gtwin);
    cassert_no_null(gtobj);
    cassert(gtobj->type == ekOBJ_EDIT || gtobj->type == ekOBJ_TEXTVIEW);
    if (editable == TRUE && gtwin->is_editable_block != NULL)
    {
        PHB_ITEM ritem = hb_itemDo(gtwin->is_editable_block, 0);
        cassert(HB_ITEM_TYPE(ritem) == HB_IT_LOGICAL);
        editable = (bool_t)hb_itemGetL(ritem);
        hb_itemRelease(ritem);
    }

    if (editable == TRUE && gtobj->is_editable_block != NULL)
    {
        PHB_ITEM ritem = hb_itemDo(gtobj->is_editable_block, 0);
        cassert(HB_ITEM_TYPE(ritem) == HB_IT_LOGICAL);
        editable = (bool_t)hb_itemGetL(ritem);
        hb_itemRelease(ritem);
    }

    return editable;
}

/*---------------------------------------------------------------------------*/

static void i_get_edit_text(const GtNapObject *obj, char_t *utf8, const uint32_t size)
{
    cassert_no_null(obj);
    cassert(obj->type == ekOBJ_EDIT);
    if (obj->get_set_block != NULL)
    {
        PHB_ITEM ritem = hb_itemDo(obj->get_set_block, 0);
        HB_TYPE type = HB_ITEM_TYPE(ritem);
        switch (type)
        {
        case HB_IT_STRING:
            cassert(obj->dtype == ekTYPE_CHARACTER);
            hb_itemCopyStrUTF8(ritem, (char *)utf8, (HB_SIZE)size);
            break;

        case HB_IT_DATE:
        {
            char date[16];
            char temp[16];
            cassert(obj->dtype == ekTYPE_DATE);
            hb_itemGetDS(ritem, date);
            hb_dateFormat(date, temp, hb_setGetDateFormat());
            str_copy_c(utf8, size, temp);
            break;
        }

        default:
            str_copy_c(utf8, size, "");
        }

        hb_itemRelease(ritem);
    }
    else
    {
        str_copy_c(utf8, size, "");
    }
}

/*---------------------------------------------------------------------------*/

static void i_jump_nchars(const char_t **src, const uint32_t nchars)
{
    uint32_t i = 0;
    cassert_no_null(src);
    cassert_no_null(*src);
    for (i = 0; i < nchars; ++i)
    {
        uint32_t nb;
        uint32_t c = unicode_to_u32b(*src, ekUTF8, &nb);
        if (c != 0)
        {
            *src += nb;
        }
        else
        {
            break;
        }
    }
}

/*---------------------------------------------------------------------------*/

static int32_t i_filter_number(const EvText *text, EvTextFilter *filter)
{
    int32_t len = text->len;

    if (len > 0)
    {
        const char_t *src2 = text->text;
        int32_t i = 0;
        i_jump_nchars(&src2, text->cpos - text->len);
        for (i = 0; i < text->len; ++i)
        {
            uint32_t nb;
            uint32_t c = unicode_to_u32b(src2, ekUTF8, &nb);
            if (c != 0)
            {
                if (c >= '0' && c <= '9')
                {
                }
                else
                {
                    cassert(len > 0);
                    len -= 1;
                }
            }
        }
    }

    {
        const char_t *src = text->text;
        char_t *dest = filter->text;
        uint32_t dsize = sizeof(filter->text);
        uint32_t i = 0, cpos = text->cpos;
        uint32_t back = 0;
        for (;;)
        {
            uint32_t nb;
            uint32_t c = unicode_to_u32b(src, ekUTF8, &nb);
            if (c != 0)
            {
                if (c >= '0' && c <= '9')
                {
                    if (dsize > nb)
                    {
                        unicode_to_char(c, dest, ekUTF8);
                        dest += nb;
                        dsize -= nb;
                    }
                }
                else
                {
                    if (cpos > i)
                        back += 1;
                }

                i += 1;
                src += nb;
            }
            /* End of input string */
            else
            {
                break;
            }
        }

        cassert(dsize > 0);
        *dest = '\0';
        filter->cpos = cpos - back;
        filter->apply = TRUE;
    }

    return len;
}

/*---------------------------------------------------------------------------*/

static void i_copy_nchars(const char_t **src, char_t **dest, uint32_t *dsize, const uint32_t nchars)
{
    uint32_t i = 0;
    cassert_no_null(src);
    cassert_no_null(*src);
    cassert_no_null(dest);
    cassert_no_null(*dest);
    cassert_no_null(dsize);
    for (i = 0; i < nchars; ++i)
    {
        uint32_t nb = 0;
        uint32_t c = unicode_to_u32b(*src, ekUTF8, &nb);
        if (c != 0 && *dsize > nb)
        {
            /* There is space in dest */
            unicode_to_char(c, *dest, ekUTF8);
            *src += nb;
            *dest += nb;
            *dsize -= nb;
        }
        else
        {
            break;
        }
    }
}

/*---------------------------------------------------------------------------*/

static void i_filter_overwrite(const EvText *text, EvTextFilter *filter, const uint32_t max_chars)
{
    bool_t updated = FALSE;
    /* Text has been inserted */
    cassert_no_null(text);
    cassert_no_null(filter);
    if (text->len > 0)
    {
        const char_t *src = text->text;
        char_t *dest = filter->text;
        uint32_t dsize = sizeof(filter->text);

        /* Copy all characters from init to caret position */
        i_copy_nchars(&src, &dest, &dsize, text->cpos);

        /* Jump 'len' chars in src */
        i_jump_nchars(&src, text->len);

        /* Copy the rest of chars */
        i_copy_nchars(&src, &dest, &dsize, UINT32_MAX);
        cassert(dsize > 0);
        *dest = '\0';
        updated = TRUE;
    }

    if (updated == FALSE)
        str_copy_c(filter->text, sizeof(filter->text), text->text);

    filter->cpos = text->cpos;
    filter->apply = TRUE;

    /* Trim to size*/
    {
        uint32_t nc = unicode_nchars(filter->text, ekUTF8);
        if (nc > max_chars)
        {
            const char_t *d = filter->text;
            i_jump_nchars(&d, max_chars);
            *((char_t *)d) = '\0';

            if (filter->cpos > max_chars)
                filter->cpos = max_chars;
        }
    }
}

/*---------------------------------------------------------------------------*/

static void i_filter_date(const EvText *text, EvTextFilter *filter, const char_t *format, bool_t insert)
{
    const char_t *src = text->text;
    char_t *dest = filter->text;
    uint32_t dsize = sizeof(filter->text);
    uint32_t i = 0;
    uint32_t cpos = text->cpos;
    for (;;)
    {
        uint32_t nbf;
        bool_t sep = FALSE;

        /* Current character of format string */
        uint32_t f = unicode_to_u32b(format, ekUTF8, &nbf);

        /* End of format string --> bye */
        if (f == 0)
            break;

        /* Digit position */
        if (f == 'd' || f == 'D' || f == 'm' || f == 'M' || f == 'y' || f == 'Y')
        {
            uint32_t nb;
            uint32_t d = unicode_to_u32b(src, ekUTF8, &nb);

            /* We have a digit into input text */
            if (d != 0)
            {
                /* Write the digit into dest */
                if (dsize > nb)
                {
                    unicode_to_char(d, dest, ekUTF8);
                    dest += nb;
                    dsize -= nb;
                }
                src = unicode_next(src, ekUTF8);
            }
            /* No more digits --> Write an space in dest */
            else
            {
                if (dsize > 1)
                {
                    unicode_to_char(' ', dest, ekUTF8);
                    dest += 1;
                    dsize -= 1;
                }
            }
        }
        /* We have a format separator character, just write it into dest */
        else
        {
            sep = TRUE;
            if (dsize > nbf)
            {
                unicode_to_char(f, dest, ekUTF8);
                dest += nbf;
                dsize -= nbf;
            }
        }

        /* Advance to next character of format string */
        format = unicode_next(format, ekUTF8);
        i += 1;

        /* Compute the new caret position */
        if (sep == TRUE)
        {
            if (insert == TRUE)
            {
                if (cpos >= i - 1)
                    cpos = cpos + 1;
            }
            else
            {
                if (cpos == i - 1)
                    cpos -= 1;
            }
        }
    }

    cassert(dsize > 0);
    *dest = '\0';
    filter->apply = TRUE;
    filter->cpos = cpos;
}

/*---------------------------------------------------------------------------*/

static void i_filter_tecla(const GtNapObject *gtobj, const EvText *text, EvTextFilter *filter)
{
    bool_t updated = FALSE;
    cassert_no_null(gtobj);
    cassert_no_null(text);
    cassert_no_null(filter);
    cassert(gtobj->type == ekOBJ_EDIT);
    /* Some text has been inserted */
    if (text->len > 0)
    {
        /* We have a filter */
        if (gtobj->keyfilter_block != NULL)
        {
            const char_t *src = text->text;
            char_t *dest = filter->text;
            uint32_t dsize = sizeof(filter->text);
            int32_t i, n = (int32_t)text->cpos - text->len;
            cassert(n >= 0);

            /* Copy the string prefix (old string init until new insertions) */
            i_copy_nchars(&src, &dest, &dsize, (uint32_t)n);

            /* Filter all characters inserted */
            for (i = 0; i < text->len; ++i)
            {
                uint32_t nb;
                uint32_t c = unicode_to_u32b(src, ekUTF8, &nb);
                if (c != 0)
                {
                    /* From Unicode (NappGUI) to code page */
                    uint8_t cp2 = i_utf8_to_cp_char(c);
                    uint32_t nb2, ncp;

                    /* Set character as lastKey */
                    hb_inkeySetLast(cp2);

                    /* Call to filter */
                    {
                        PHB_ITEM ritem = hb_itemDo(gtobj->keyfilter_block, 0);
                        HB_TYPE type = HB_ITEM_TYPE(ritem);
                        if (type == HB_IT_NIL)
                        {
                            ncp = c;
                            nb2 = nb;
                        }
                        else
                        {
                            char_t temp[32];
                            cassert(type == HB_IT_STRING);
                            hb_itemCopyStrUTF8(ritem, temp, sizeof(temp));
                            cassert(unicode_nchars(temp, ekUTF8) == 1);
                            ncp = unicode_to_u32b(temp, ekUTF8, &nb2);
                        }

                        hb_itemRelease(ritem);
                    }

                    /* There is space in dest */
                    if (dsize > nb2)
                    {
                        unicode_to_char(ncp, dest, ekUTF8);
                        dest += nb2;
                        dsize -= nb2;
                    }

                    src += nb;
                }
                else
                {
                    break;
                }
            }

            /* Copy the rest of the string */
            i_copy_nchars(&src, &dest, &dsize, UINT32_MAX);
            cassert(dsize > 0);

            *dest = '\0';
            updated = TRUE;
        }
    }

    /* No filter applied, just copy the input string */
    if (updated == FALSE)
        str_copy_c(filter->text, sizeof(filter->text), text->text);

    filter->apply = TRUE;
    filter->cpos = text->cpos;
}

/*---------------------------------------------------------------------------*/

static void i_filter_tecla_textview(const GtNapObject *gtobj, const EvText *text, EvTextFilter *filter)
{
    bool_t updated = FALSE;
    cassert_no_null(gtobj);
    cassert_no_null(text);
    cassert_no_null(filter);
    cassert(gtobj->type == ekOBJ_TEXTVIEW);
    /* Some text has been inserted */
    if (text->len > 0)
    {
        /* We have a filter */
        if (gtobj->keyfilter_block != NULL)
        {
            const char_t *src = text->text;
            char_t *dest = filter->text;
            uint32_t dsize = sizeof(filter->text);
            int32_t i;

            /* Filter all characters inserted */
            for (i = 0; i < text->len; ++i)
            {
                uint32_t nb;
                uint32_t c = unicode_to_u32b(src, ekUTF8, &nb);
                if (c != 0)
                {
                    /* From Unicode (NappGUI) to code page */
                    uint8_t cp2 = i_utf8_to_cp_char(c);
                    uint32_t nb2, ncp;

                    /* Set character as lastKey */
                    hb_inkeySetLast(cp2);

                    /* Call to filter */
                    {
                        PHB_ITEM ritem = hb_itemDo(gtobj->keyfilter_block, 0);
                        HB_TYPE type = HB_ITEM_TYPE(ritem);
                        if (type == HB_IT_NIL)
                        {
                            ncp = c;
                            nb2 = nb;
                        }
                        else
                        {
                            char_t temp[32];
                            cassert(type == HB_IT_STRING);
                            hb_itemCopyStrUTF8(ritem, temp, sizeof(temp));
                            cassert(unicode_nchars(temp, ekUTF8) == 1);
                            ncp = unicode_to_u32b(temp, ekUTF8, &nb2);
                        }

                        hb_itemRelease(ritem);
                    }

                    /* There is space in dest */
                    if (dsize > nb2)
                    {
                        unicode_to_char(ncp, dest, ekUTF8);
                        dest += nb2;
                        dsize -= nb2;
                    }

                    src += nb;
                }
                else
                {
                    break;
                }
            }

            *dest = '\0';
            updated = TRUE;
        }
    }

    filter->apply = updated;
}

/*---------------------------------------------------------------------------*/

static void i_OnEditFilter(GtNapObject *gtobj, Event *e)
{
    const EvText *p = event_params(e, EvText);
    EvTextFilter *res = event_result(e, EvTextFilter);
    GtNapWindow *gtwin = NULL;
    cassert_no_null(gtobj);
    cassert(gtobj->type == ekOBJ_EDIT);
    gtwin = gtobj->gtwin;
    cassert_no_null(gtwin);

    if (i_is_editable(gtwin, gtobj) == FALSE)
    {
        /* If editBox is not editable --> Restore the original text */
        i_get_edit_text(gtobj, res->text, sizeof(res->text));
        if (p->cpos > 0)
        {
            if (p->len > 0)
                res->cpos = p->cpos - p->len;
            else
                res->cpos = p->cpos;
        }
        else
        {
            res->cpos = 0;
        }

        res->apply = TRUE;
    }
    else
    {
        if (gtobj->dtype == ekTYPE_DATE)
        {
            EvTextFilter fil1;
            EvTextFilter fil2;
            uint32_t len;
            fil1.apply = FALSE;
            fil2.apply = FALSE;

            len = i_filter_number(p, &fil1);
            cassert(fil1.apply == TRUE);

            {
                EvText tf;
                tf.text = fil1.text;
                tf.cpos = fil1.cpos;
                tf.len = len;
                i_filter_overwrite(&tf, &fil2, GTNAP_GLOBAL->date_digits);
            }

            cassert(fil2.apply == TRUE);

            {
                EvText tf;
                tf.text = fil2.text;
                tf.cpos = fil2.cpos;
                tf.len = 0;
                i_filter_date(&tf, res, hb_setGetDateFormat(), p->len >= 0);
            }

            cassert(res->apply == TRUE);

            if (res->cpos == GTNAP_GLOBAL->date_chars)
                gui_OnIdle(listener(gtwin, i_OnNextTabstop, GtNapWindow));
        }
        else
        {
            EvTextFilter filTec;
            filTec.apply = FALSE;
            i_filter_tecla(gtobj, p, &filTec);
            cassert(filTec.apply == TRUE);

            {
                EvText tf;
                cassert(filTec.cpos == p->cpos);
                tf.text = filTec.text;
                tf.cpos = filTec.cpos;
                tf.len = p->len;
                i_filter_overwrite(&tf, res, gtobj->max_chars);
            }

            cassert(res->apply == TRUE);

            /* End of editable string reached. */
            if (res->cpos >= gtobj->max_chars)
                gui_OnIdle(listener(gtwin, i_OnNextTabstop, GtNapWindow));
        }
    }
}

/*---------------------------------------------------------------------------*/

static void i_set_edit_message(GtNapObject *obj, GtNapObject *mes_obj)
{
    cassert_no_null(obj);
    cassert_no_null(mes_obj);
    cassert(obj->type == ekOBJ_EDIT);
    cassert(mes_obj->type == ekOBJ_LABEL);
    if (obj->message_block != NULL)
    {
        PHB_ITEM ritem = hb_itemDo(obj->message_block, 0);
        HB_TYPE type = HB_ITEM_TYPE(ritem);

        if (type == HB_IT_STRING)
        {
            char_t buffer[1024];
            uint32_t len;
            hb_itemCopyStrUTF8(ritem, (char *)buffer, (HB_SIZE)sizeof(buffer));
            len = unicode_nchars(buffer, ekUTF8);
            mes_obj->size.width = (real32_t)len * GTNAP_GLOBAL->cell_x_sizef;
            _component_set_frame(mes_obj->component, &mes_obj->pos, &mes_obj->size);
            label_text((Label *)mes_obj->component, buffer);
        }
        else
        {
            cassert_msg(FALSE, "Unkown type in i_set_edit_message");
        }

        hb_itemRelease(ritem);
    }
}

/*---------------------------------------------------------------------------*/

static void i_set_edit_text(const GtNapObject *obj)
{
    char_t buffer[STATIC_TEXT_SIZE];
    cassert_no_null(obj);
    i_get_edit_text(obj, buffer, sizeof(buffer));
    edit_text((Edit *)obj->component, buffer);
}

/*---------------------------------------------------------------------------*/

static void i_set_view_text(const GtNapObject *obj)
{
    cassert_no_null(obj);
    cassert(obj->type == ekOBJ_TEXTVIEW);
    textview_clear((TextView *)obj->component);
    if (obj->get_set_block != NULL)
    {
        PHB_ITEM ritem = hb_itemDo(obj->get_set_block, 0);
        String *str = i_item_to_utf8_string(ritem);
        textview_writef((TextView *)obj->component, tc(str));
        hb_itemRelease(ritem);
        str_destroy(&str);
    }
}

/*---------------------------------------------------------------------------*/

static void i_launch_wizard(GtNapWindow *gtwin, GtNapObject *obj)
{
    char_t temp[1024];
    PHB_ITEM ritem = NULL;
    HB_TYPE type = HB_IT_NIL;
    cassert_no_null(gtwin);
    cassert_no_null(obj);
    cassert(obj->type == ekOBJ_EDIT);
    cassert_no_null(obj->wizard_block);
    ritem = hb_itemDo(obj->wizard_block, 0);
    type = HB_ITEM_TYPE(ritem);

    if (type != HB_IT_NIL)
    {
        cassert(type == HB_IT_STRING);
        hb_itemCopyStrUTF8(ritem, temp, sizeof(temp));
        edit_text((Edit *)obj->component, temp);
    }

    hb_itemRelease(ritem);

    if (type != HB_IT_NIL)
        gui_OnIdle(listener(gtwin, i_OnNextTabstop, GtNapWindow));
}

/*---------------------------------------------------------------------------*/

static void i_OnAutoWizard(GtNapWindow *gtwin, Event *e)
{
    GtNapObject *gtobj = NULL;
    cassert_no_null(gtwin);

    if (i_gtwin_alive(gtwin, GTNAP_GLOBAL) == FALSE)
        return;

    gtobj = gtwin->wizard_obj;
    cassert_no_null(gtobj);
    unref(e);

    if (gtobj->can_auto_lista == TRUE && gtobj->auto_block != NULL && gtobj->wizard_block != NULL)
    {
        bool_t lista = FALSE;

        {
            PHB_ITEM ritem = hb_itemDo(gtobj->auto_block, 0);
            HB_TYPE type = HB_ITEM_TYPE(ritem);
            cassert_unref(type == HB_IT_LOGICAL, type);
            lista = (bool_t)hb_itemGetL(ritem);
            hb_itemRelease(ritem);
        }

        if (lista == TRUE)
        {
            gtobj->can_auto_lista = FALSE;
            i_launch_wizard(gtwin, gtobj);
        }
    }
}

/*---------------------------------------------------------------------------*/

static void i_OnEditFocus(GtNapObject *gtobj, Event *e)
{
    const bool_t *p = event_params(e, bool_t);
    GtNapWindow *gtwin = NULL;
    cassert_no_null(gtobj);
    cassert(gtobj->type == ekOBJ_EDIT);
    gtwin = gtobj->gtwin;
    cassert_no_null(gtwin);

    if (*p == TRUE)
    {
        gtobj->has_focus = TRUE;

        /* We sure only one control has the focus */
        arrpt_foreach(obj, gtwin->objects, GtNapObject)
            if (obj != gtobj)
            {
                obj->has_focus = FALSE;
                obj->can_auto_lista = TRUE;
            }
        arrpt_end();

        if (gtwin->message_label_id != UINT32_MAX)
        {
            GtNapObject *mes_obj = arrpt_get(gtwin->objects, gtwin->message_label_id, GtNapObject);
            i_set_edit_message(gtobj, mes_obj);
        }

        if (gtobj->when_block != NULL)
        {
            PHB_ITEM ritem = hb_itemDo(gtobj->when_block, 0);
            HB_TYPE type = HB_ITEM_TYPE(ritem);
            bool_t updated = FALSE;
            cassert_unref(type == HB_IT_LOGICAL, type);
            updated = (bool_t)hb_itemGetL(ritem);
            hb_itemRelease(ritem);

            if (updated == TRUE)
                i_set_edit_text(gtobj);
        }

        edit_select((Edit *)gtobj->component, 0, 0);

        if (gtobj->can_auto_lista == TRUE && gtobj->auto_block != NULL && gtobj->wizard_block != NULL)
        {
            gtwin->wizard_obj = gtobj;
            gui_OnIdle(listener(gtwin, i_OnAutoWizard, GtNapWindow));
        }
    }
}

/*---------------------------------------------------------------------------*/

static void i_OnTextFocus(GtNapObject *gtobj, Event *e)
{
    const bool_t *p = event_params(e, bool_t);
    GtNapWindow *gtwin = NULL;
    cassert_no_null(gtobj);
    cassert(gtobj->type == ekOBJ_TEXTVIEW);
    gtwin = gtobj->gtwin;
    cassert_no_null(gtwin);

    if (*p == TRUE)
    {
        gtobj->has_focus = TRUE;

        /* We sure only one control has the focus */
        arrpt_foreach(obj, gtwin->objects, GtNapObject)
            if (obj != gtobj)
            {
                obj->has_focus = FALSE;
                obj->can_auto_lista = TRUE;
            }
        arrpt_end();
    }
}

/*---------------------------------------------------------------------------*/

static void i_OnTextFilter(GtNapObject *gtobj, Event *e)
{
    const EvText *p = event_params(e, EvText);
    EvTextFilter *r = event_result(e, EvTextFilter);
    cassert_no_null(gtobj);
    cassert(gtobj->type == ekOBJ_TEXTVIEW);
    i_filter_tecla_textview(gtobj, p, r);
}

/*---------------------------------------------------------------------------*/

static void i_gtwin_configure(GtNap *gtnap, GtNapWindow *gtwin, GtNapWindow *main_gtwin)
{
    Panel *scroll_panel = NULL;
    Layout *layout = layout_create(1, 1);
    V2Df offset = kV2D_ZEROf;
    cassert_no_null(gtnap);
    cassert_no_null(gtwin);
    cassert(gtwin->is_configured == FALSE);
    cassert(gtwin->panel == NULL);
    cassert_no_null(main_gtwin);
    cassert_no_null(main_gtwin->window);

    gtwin->panel = panel_custom(FALSE, FALSE, gtwin->border);

    /*if (gtwin->toolbar != NULL)
          gtwin->panel_size.height += (real32_t)GTNAP_GLOBAL->cell_y_size; */

    panel_size(gtwin->panel, gtwin->panel_size);
    panel_layout(gtwin->panel, layout);

    if (i_with_scroll_panel(gtwin) == TRUE)
    {
        /* We add a subpanel to window main panel to implement the scroll area */
        Panel *panel = panel_scroll(FALSE, TRUE);
        S2Df csize = i_scroll_content_size(gtwin->objects);
        int32_t cell_x = gtwin->scroll_left - gtwin->left;
        int32_t cell_y = gtwin->scroll_top - gtwin->top;
        real32_t pos_x = (real32_t)cell_x * GTNAP_GLOBAL->cell_x_sizef;
        real32_t pos_y = (real32_t)cell_y * GTNAP_GLOBAL->cell_y_sizef;
        real32_t width = (real32_t)(gtwin->scroll_right - gtwin->scroll_left + 3) * GTNAP_GLOBAL->cell_x_sizef;
        real32_t height = (real32_t)(gtwin->scroll_bottom - gtwin->scroll_top + 1) * GTNAP_GLOBAL->cell_y_sizef;
        V2Df pos = v2df(pos_x, pos_y);
        S2Df size = s2df(width, height);
        _panel_attach_component(gtwin->panel, (GuiComponent *)panel);
        _component_set_frame((GuiComponent *)panel, &pos, &size);
        _component_visible((GuiComponent *)panel, FALSE);
        _panel_content_size(panel, csize.width, csize.height);
        offset.x = -pos.x;
        offset.y = -pos.y;
        scroll_panel = panel;
        gtwin->scrolled_panel = panel;
    }

    /* Attach gui objects in certain Z-Order (from back to front) */
    i_attach_to_panel(gtwin->objects, gtwin->panel, scroll_panel, &offset, ekOBJ_MENU, gtwin->toolbar);
    i_attach_to_panel(gtwin->objects, gtwin->panel, scroll_panel, &offset, ekOBJ_TABLEVIEW, gtwin->toolbar);
    i_attach_to_panel(gtwin->objects, gtwin->panel, scroll_panel, &offset, ekOBJ_TEXTVIEW, gtwin->toolbar);
    i_attach_to_panel(gtwin->objects, gtwin->panel, scroll_panel, &offset, ekOBJ_LABEL, gtwin->toolbar);
    i_attach_to_panel(gtwin->objects, gtwin->panel, scroll_panel, &offset, ekOBJ_BUTTON, gtwin->toolbar);
    i_attach_to_panel(gtwin->objects, gtwin->panel, scroll_panel, &offset, ekOBJ_EDIT, gtwin->toolbar);
    i_attach_to_panel(gtwin->objects, gtwin->panel, scroll_panel, &offset, ekOBJ_IMAGE, gtwin->toolbar);
    i_attach_toolbar_to_panel(gtwin->toolbar, gtwin->panel);

    /* We are in a main (not embedded) window */
    if (gtwin->window != NULL)
    {
        cassert(gtwin == main_gtwin);
        cassert(gtwin->parent_id == UINT32_MAX);
        /* Add the window main panel */
        window_panel(gtwin->window, gtwin->panel);
        /* Clear the tabstop list */
        arrpt_clear(gtwin->tabstops, NULL, GuiComponent);
    }

    i_component_tabstop(gtwin->objects, main_gtwin->window, main_gtwin->tabstops, ekOBJ_MENU);
    i_component_tabstop(gtwin->objects, main_gtwin->window, main_gtwin->tabstops, ekOBJ_TABLEVIEW);
    i_component_tabstop(gtwin->objects, main_gtwin->window, main_gtwin->tabstops, ekOBJ_TEXTVIEW);
    i_component_tabstop(gtwin->objects, main_gtwin->window, main_gtwin->tabstops, ekOBJ_EDIT);
    i_component_tabstop(gtwin->objects, main_gtwin->window, main_gtwin->tabstops, ekOBJ_BUTTON);
    i_component_tabstop(gtwin->objects, main_gtwin->window, main_gtwin->tabstops, ekOBJ_LABEL);
    i_component_tabstop(gtwin->objects, main_gtwin->window, main_gtwin->tabstops, ekOBJ_IMAGE);

    if (scroll_panel != NULL)
        _component_visible((GuiComponent *)scroll_panel, TRUE);

    if (gtwin->window != NULL)
    {
        i_toolbar_tabstop(gtwin->toolbar, main_gtwin->tabstops);
    }
    else
    {
        /* Toolbar is not allowed in embedded windows */
        cassert(gtwin->toolbar == NULL);
    }

    /* We are in a main window */
    if (gtwin->parent_id == UINT32_MAX)
    {
        cassert(gtwin == main_gtwin);

        /* Configure the child (embedded) windows as subpanels */
        arrpt_forback(embgtwin, gtnap->windows, GtNapWindow)
            if (embgtwin->parent_id == gtwin->id)
            {
                V2Df pos;
                int32_t cell_x = embgtwin->left - gtwin->left;
                int32_t cell_y = embgtwin->top - gtwin->top;
                pos.x = (real32_t)cell_x * GTNAP_GLOBAL->cell_x_sizef;
                pos.y = (real32_t)cell_y * GTNAP_GLOBAL->cell_y_sizef;
                i_gtwin_configure(gtnap, embgtwin, gtwin);
                _panel_attach_component(gtwin->panel, (GuiComponent *)embgtwin->panel);
                _component_set_frame((GuiComponent *)embgtwin->panel, &pos, &embgtwin->panel_size);
                _component_visible((GuiComponent *)embgtwin->panel, TRUE);
            }
        arrpt_end();

        /* At this point the main window (with embedded windows) is complete.
         * We begin the tabstop configuration */
        _window_taborder(gtwin->window, NULL);
        arrpt_foreach(component, gtwin->tabstops, GuiComponent)
            _component_taborder(component, gtwin->window);
        arrpt_end()
    }

    /* Allow navigation between edit controls with arrows and return */
    if (i_num_edits(gtwin) > 0)
    {
        /* At the moment, embedded windows with edits is not allowed */
        GtNapObject *last_edit = NULL;

        arrpt_foreach(obj, gtwin->objects, GtNapObject)
            if (obj->type == ekOBJ_EDIT)
            {
                edit_OnChange((Edit *)obj->component, listener(obj, i_OnEditChange, GtNapObject));
                edit_OnFilter((Edit *)obj->component, listener(obj, i_OnEditFilter, GtNapObject));
                edit_OnFocus((Edit *)obj->component, listener(obj, i_OnEditFocus, GtNapObject));
                obj->is_last_edit = FALSE;
                last_edit = obj;
            }
        arrpt_end();

        cassert_no_null(last_edit);
        last_edit->is_last_edit = TRUE;
    }

    /* Allow TextView listeners */
    arrpt_foreach(obj, gtwin->objects, GtNapObject)
        if (obj->type == ekOBJ_TEXTVIEW)
        {
            textview_OnFocus((TextView *)obj->component, listener(obj, i_OnTextFocus, GtNapObject));
            if (obj->keyfilter_block != NULL)
                textview_OnFilter((TextView *)obj->component, listener(obj, i_OnTextFilter, GtNapObject));
        }
    arrpt_end();

    if (gtwin->buttons_navigation == TRUE)
    {
        if (i_num_buttons(gtwin) > 1)
        {
            /* At the moment, embedded windows with button navigation is not allowed */
            cassert(gtwin->window != NULL);
            window_hotkey(gtwin->window, ekKEY_LEFT, 0, listener(gtwin, i_OnLeftButton, GtNapWindow));
            window_hotkey(gtwin->window, ekKEY_RIGHT, 0, listener(gtwin, i_OnRightButton, GtNapWindow));
        }
    }

    gtwin->is_configured = TRUE;
}

/*---------------------------------------------------------------------------*/

static void i_gtnap_update(GtNap *gtnap, const real64_t prtime, const real64_t ctime)
{
    cassert(gtnap == NULL || gtnap == GTNAP_GLOBAL);
    gtnap = GTNAP_GLOBAL;
    cassert_no_null(gtnap);
    unref(prtime);
    unref(ctime);
    if (gtnap->modal_time_window != NULL)
    {
        GtNapWindow *gtwin = gtnap->modal_time_window;
        if (arrpt_find(gtnap->windows, gtwin, GtNapWindow) != UINT32_MAX)
        {
            if (gtnap->modal_delay_seconds > 0)
            {
                uint64_t now = btime_now();
                if ((now - gtnap->modal_timestamp) / 1000000 >= gtnap->modal_delay_seconds)
                {
                    gtnap->modal_timestamp = 0;
                    gtnap->modal_delay_seconds = 0;
                    gtnap->modal_time_window = NULL;
                    i_stop_modal(gtnap, gtwin, NAP_MODAL_TIMESTAMP);
                }
            }
        }
        else
        {
            gtnap->modal_timestamp = 0;
            gtnap->modal_delay_seconds = 0;
            gtnap->modal_time_window = NULL;
        }
    }
}

/*---------------------------------------------------------------------------*/

void hb_gtnap_init(const char_t *title, const uint32_t rows, const uint32_t cols, PHB_ITEM begin_block)
{
    void *hInstance = NULL;

#if defined(HB_OS_WIN)
    hb_winmainArgGet(&hInstance, NULL, NULL);
#endif

    str_copy_c(INIT_TITLE, sizeof32(INIT_TITLE), title);
    INIT_CODEBLOCK = hb_itemNew(begin_block);
    INIT_ROWS = rows;
    INIT_COLS = cols;

    osmain_imp(
        0, NULL, hInstance, 0.5f,
        (FPtr_app_create)i_gtnap_create,
        (FPtr_app_update)i_gtnap_update,
        (FPtr_destroy)i_gtnap_destroy,
        (char_t *)"");
}

/*---------------------------------------------------------------------------*/

void hb_gtnap_log(const char_t *text)
{
    String *str = i_cp_to_utf8_string(text);
    log_printf("%s", tc(str));
    str_destroy(&str);
}

/*---------------------------------------------------------------------------*/

static void i_OnTerminalClose(GtNapWindow *gtwin, Event *e)
{
    unref(gtwin);
    unref(e);
    osapp_finish();
}

/*---------------------------------------------------------------------------*/

uint32_t hb_gtnap_width(void)
{
    cassert_no_null(GTNAP_GLOBAL);
    return (uint32_t)(GTNAP_GLOBAL->cell_x_sizef * GTNAP_GLOBAL->cols);
}

/*---------------------------------------------------------------------------*/

uint32_t hb_gtnap_height(void)
{
    cassert_no_null(GTNAP_GLOBAL);
    return (uint32_t)(GTNAP_GLOBAL->cell_y_sizef * GTNAP_GLOBAL->rows);
}

/*---------------------------------------------------------------------------*/

void hb_gtnap_terminal(void)
{
    GtNap *gtnap = GTNAP_GLOBAL;
    GtNapWindow *gtwin = NULL;
    cassert(arrpt_size(gtnap->windows, GtNapWindow) == 0);
    hb_gtnap_window(0, 0, gtnap->rows - 1, gtnap->cols - 1, tc(gtnap->title), FALSE, TRUE, TRUE, FALSE);
    gtwin = i_current_gtwin(gtnap);
    i_gtwin_configure(gtnap, gtwin, gtwin);
    window_OnClose(gtwin->window, listener(gtwin, i_OnTerminalClose, GtNapWindow));
    window_show(gtwin->window);
}

/*---------------------------------------------------------------------------*/

int32_t hb_gtnap_inkey(const vkey_t vkey)
{
    uint32_t i, n = sizeof(KEYMAPS) / sizeof(GtNapKey);
    for (i = 0; i < n; ++i)
    {
        if (KEYMAPS[i].vkey == vkey)
            return KEYMAPS[i].key;
    }

    return INT32_MAX;
}

/*---------------------------------------------------------------------------*/

static ___INLINE uint32_t i_window_flags(const bool_t close_return, const bool_t close_esc, const bool_t minimize_button)
{
    uint32_t flags = ekWINDOW_TITLE | ekWINDOW_CLOSE | ekWINDOW_MODAL_NOHIDE;

    if (close_return == TRUE)
        flags |= ekWINDOW_RETURN;

    if (close_esc == TRUE)
        flags |= ekWINDOW_ESC;

    if (minimize_button == TRUE)
        flags |= ekWINDOW_MIN;

    return flags;
}

/*---------------------------------------------------------------------------*/

static void i_OnWindowClose(GtNapWindow *gtwin, Event *e)
{
    const EvWinClose *p = event_params(e, EvWinClose);
    bool_t *res = event_result(e, bool_t);
    cassert(*res == TRUE);
    if (gtwin->desist_block != NULL)
    {
        PHB_ITEM ritem = hb_itemDo(gtwin->desist_block, 0);
        cassert(HB_ITEM_TYPE(ritem) == HB_IT_LOGICAL);
        *res = (bool_t)hb_itemGetL(ritem);
        hb_itemRelease(ritem);
    }

    if (*res == TRUE && p->origin == ekGUI_CLOSE_ESC)
        gtwin->is_closed_by_esc = TRUE;
}

/*---------------------------------------------------------------------------*/

static uint32_t i_get_window_id(GtNap *gtnap)
{
    uint32_t id = NAP_WINDOW_FIST_ID;
    bool_t found = FALSE;
    while (!found)
    {
        bool_t valid_id = TRUE;
        arrpt_foreach_const(gtwin, gtnap->windows, GtNapWindow)
            if (gtwin->id == id)
            {
                id += 1;
                valid_id = FALSE;
                break;
            }
        arrpt_end();

        if (valid_id == TRUE)
            found = TRUE;
    }

    return id;
}

/*---------------------------------------------------------------------------*/

static GtNapWindow *i_new_window(GtNap *gtnap, uint32_t parent_id, const int32_t top, const int32_t left, const int32_t bottom, const int32_t right, const bool_t border)
{
    GtNapWindow *gtwin = NULL;
    cassert_no_null(gtnap);
    gtwin = heap_new0(GtNapWindow);
    gtwin->id = i_get_window_id(gtnap);
    gtwin->parent_id = parent_id;
    gtwin->top = top;
    gtwin->left = left;
    gtwin->bottom = bottom;
    gtwin->right = right;
    gtwin->border = border;
    gtwin->scroll_top = INT32_MIN;
    gtwin->scroll_left = INT32_MIN;
    gtwin->scroll_bottom = INT32_MIN;
    gtwin->scroll_right = INT32_MIN;
    gtwin->message_label_id = UINT32_MAX;
    gtwin->default_button = UINT32_MAX;
    gtwin->tabstops = arrpt_create(GuiComponent);
    gtwin->objects = arrpt_create(GtNapObject);
    gtwin->callbacks = arrpt_create(GtNapCallback);
    gtwin->panel_size.width = gtnap->cell_x_sizef * (real32_t)(gtwin->right - gtwin->left + 1);
    gtwin->panel_size.height = gtnap->cell_y_sizef * (real32_t)(gtwin->bottom - gtwin->top + 1);
    arrpt_append(gtnap->windows, gtwin, GtNapWindow);
    return gtwin;
}

/*---------------------------------------------------------------------------*/

const char_t *hb_gtnap_working_path(void)
{
    return tc(GTNAP_GLOBAL->working_path);
}

/*---------------------------------------------------------------------------*/

#define COL_GREEN 2
#define COL_CYAN 3
#define COL_RED 4
#define COL_MAGENTA 5
#define COL_BROWN 6
#define COL_WHITE 7
#define COL_LIGHT_GRAY 8
#define COL_BRIGHT_BLUE 9
#define COL_BRIGHT_GREEN 10
#define COL_BRIGHT_CYAN 11
#define COL_BRIGHT_RED 12
#define COL_BRIGHT_MAGENTA 13
#define COL_YELLOW 14
#define COL_BRIGHT_WHITE 15

/*---------------------------------------------------------------------------*/

color_t hb_gtnap_color_black(void)
{
    return i_COLORS[COL_BLACK];
}

/*---------------------------------------------------------------------------*/

color_t hb_gtnap_color_blue(void)
{
    return i_COLORS[COL_BLUE];
}

/*---------------------------------------------------------------------------*/

color_t hb_gtnap_color_green(void)
{
    return i_COLORS[COL_GREEN];
}

/*---------------------------------------------------------------------------*/

color_t hb_gtnap_color_cyan(void)
{
    return i_COLORS[COL_CYAN];
}

/*---------------------------------------------------------------------------*/

color_t hb_gtnap_color_red(void)
{
    return i_COLORS[COL_RED];
}

/*---------------------------------------------------------------------------*/

color_t hb_gtnap_color_magenta(void)
{
    return i_COLORS[COL_MAGENTA];
}

/*---------------------------------------------------------------------------*/

color_t hb_gtnap_color_brown(void)
{
    return i_COLORS[COL_BROWN];
}

/*---------------------------------------------------------------------------*/

color_t hb_gtnap_color_white(void)
{
    return i_COLORS[COL_WHITE];
}

/*---------------------------------------------------------------------------*/

color_t hb_gtnap_color_light_gray(void)
{
    return i_COLORS[COL_LIGHT_GRAY];
}

/*---------------------------------------------------------------------------*/

color_t hb_gtnap_color_bright_blue(void)
{
    return i_COLORS[COL_BRIGHT_BLUE];
}

/*---------------------------------------------------------------------------*/

color_t hb_gtnap_color_bright_green(void)
{
    return i_COLORS[COL_BRIGHT_GREEN];
}

/*---------------------------------------------------------------------------*/

color_t hb_gtnap_color_bright_cyan(void)
{
    return i_COLORS[COL_BRIGHT_CYAN];
}

/*---------------------------------------------------------------------------*/

color_t hb_gtnap_color_bright_red(void)
{
    return i_COLORS[COL_BRIGHT_RED];
}

/*---------------------------------------------------------------------------*/

color_t hb_gtnap_color_bright_magenta(void)
{
    return i_COLORS[COL_BRIGHT_MAGENTA];
}

/*---------------------------------------------------------------------------*/

color_t hb_gtnap_color_yellow(void)
{
    return i_COLORS[COL_YELLOW];
}

/*---------------------------------------------------------------------------*/

color_t hb_gtnap_color_bright_white(void)
{
    return i_COLORS[COL_BRIGHT_WHITE];
}

/*---------------------------------------------------------------------------*/

uint32_t hb_gtnap_window(const int32_t top, const int32_t left, const int32_t bottom, const int32_t right, const char_t *title, const bool_t close_return, const bool_t close_esc, const bool_t minimize_button, const bool_t buttons_navigation)
{
    GtNapWindow *gtwin = i_new_window(GTNAP_GLOBAL, UINT32_MAX, top, left, bottom, right, FALSE);
    uint32_t flags = i_window_flags(close_return, close_esc, minimize_button);
    gtwin->window = window_create(flags);
    gtwin->buttons_navigation = buttons_navigation;

    if (str_empty_c(title) == FALSE)
    {
        char_t utf8[STATIC_TEXT_SIZE];
        i_cp_to_utf8(title, utf8, sizeof(utf8));
        window_title(gtwin->window, utf8);
    }
    else
    {
        window_title(gtwin->window, tc(GTNAP_GLOBAL->title));
    }

    window_cycle_tabstop(gtwin->window, FALSE);
    window_OnClose(gtwin->window, listener(gtwin, i_OnWindowClose, GtNapWindow));
    return gtwin->id;
}

/*---------------------------------------------------------------------------*/

uint32_t hb_gtnap_window_embedded(const uint32_t wid, const int32_t top, const int32_t left, const int32_t bottom, const int32_t right, const bool_t border)
{
    GtNapWindow *gtwin = i_new_window(GTNAP_GLOBAL, wid, top, left, bottom, right, border);
    return gtwin->id;
}

/*---------------------------------------------------------------------------*/

static void i_dettach_embedded(GtNap *gtnap, GtNapWindow *gtwin)
{
    cassert_no_null(gtnap);
    cassert_no_null(gtwin);
    if (gtwin->is_configured == TRUE)
    {
        /* We are in a main window --> Dettach all possible embedded windows */
        if (gtwin->parent_id == UINT32_MAX)
        {
            arrpt_foreach(embgtwin, GTNAP_GLOBAL->windows, GtNapWindow)
                if (embgtwin->parent_id == gtwin->id)
                {
                    cassert(embgtwin->is_configured == TRUE);
                    _component_visible((GuiComponent *)embgtwin->panel, FALSE);
                    _component_detach_from_panel((GuiComponent *)gtwin->panel, (GuiComponent *)embgtwin->panel);
                }
            arrpt_end()
        }
        /* We are in an embedded window --> Dettach from ONLY one parent */
        else
        {
            arrpt_foreach(maingtwin, GTNAP_GLOBAL->windows, GtNapWindow)
                if (gtwin->parent_id == maingtwin->id)
                {
                    cassert(maingtwin->is_configured == TRUE);
                    _panel_dettach_component(maingtwin->panel, (GuiComponent *)gtwin->panel);
                    break;
                }
            arrpt_end()
        }
    }
}

/*---------------------------------------------------------------------------*/

void hb_gtnap_window_destroy(const uint32_t wid)
{
    uint32_t id = i_gtwin_index(GTNAP_GLOBAL, wid);
    GtNapWindow *gtwin = arrpt_get(GTNAP_GLOBAL->windows, id, GtNapWindow);
    /* Before destroy we have to dettach the possible parent-embedded connections */
    i_dettach_embedded(GTNAP_GLOBAL, gtwin);
    arrpt_delete(GTNAP_GLOBAL->windows, id, i_destroy_gtwin, GtNapWindow);
}

/*---------------------------------------------------------------------------*/

static const GtNapKey *i_convert_key(const int32_t key)
{
    uint32_t i, n = sizeof(KEYMAPS) / sizeof(GtNapKey);
    for (i = 0; i < n; ++i)
    {
        if (KEYMAPS[i].key == key)
            return &KEYMAPS[i];
    }

    return NULL;
}

/*---------------------------------------------------------------------------*/

static void i_OnWindowHotKey(GtNapCallback *callback, Event *e)
{
    cassert_no_null(callback);
    cassert_no_null(callback->gtwin);
    unref(e);

    if (callback->block != NULL)
    {
        PHB_ITEM ritem = hb_itemDo(callback->block, 0);
        hb_itemRelease(ritem);
    }

    if (callback->autoclose_id != UINT32_MAX)
        i_stop_modal(GTNAP_GLOBAL, callback->gtwin, NAP_MODAL_HOTKEY_AUTOCLOSE + callback->autoclose_id);
}

/*---------------------------------------------------------------------------*/

void hb_gtnap_window_hotkey(const uint32_t wid, const int32_t key, HB_ITEM *block, const bool_t autoclose)
{
    GtNapWindow *gtwin = i_gtwin(GTNAP_GLOBAL, wid);
    const GtNapKey *nkey = i_convert_key(key);
    cassert_no_null(gtwin);

    /* Exists a Harbour/NAppGUI key convertion */
    if (nkey != NULL)
    {
        {
            uint32_t pos = UINT32_MAX;

            /* Delete a previous callback on this hotkey */
            arrpt_foreach(callback, gtwin->callbacks, GtNapCallback)
                if (callback->key == key)
                {
                    pos = callback_i;
                    break;
                }
            arrpt_end();

            if (pos != UINT32_MAX)
                arrpt_delete(gtwin->callbacks, pos, i_destroy_callback, GtNapCallback);
        }

        {
            uint32_t autoclose_id = autoclose ? (uint32_t)nkey->vkey : UINT32_MAX;
            Listener *listener = i_gtnap_listener(block, key, autoclose_id, gtwin, i_OnWindowHotKey);
            window_hotkey(gtwin->window, nkey->vkey, nkey->modifiers, listener);
        }
    }
}

/*---------------------------------------------------------------------------*/

void hb_gtnap_window_editable(const uint32_t wid, HB_ITEM *is_editable_block)
{
    GtNapWindow *gtwin = i_gtwin(GTNAP_GLOBAL, wid);
    cassert_no_null(gtwin);
    cassert(gtwin->is_editable_block == NULL);
    gtwin->is_editable_block = hb_itemNew(is_editable_block);
}

/*---------------------------------------------------------------------------*/

void hb_gtnap_window_confirm(const uint32_t wid, HB_ITEM *confirm_block)
{
    GtNapWindow *gtwin = i_gtwin(GTNAP_GLOBAL, wid);
    cassert_no_null(gtwin);
    cassert(gtwin->confirm_block == NULL);
    gtwin->confirm_block = hb_itemNew(confirm_block);
}

/*---------------------------------------------------------------------------*/

void hb_gtnap_window_desist(const uint32_t wid, HB_ITEM *desist_block)
{
    GtNapWindow *gtwin = i_gtwin(GTNAP_GLOBAL, wid);
    cassert_no_null(gtwin);
    cassert(gtwin->desist_block == NULL);
    gtwin->desist_block = hb_itemNew(desist_block);
}

/*---------------------------------------------------------------------------*/

void hb_gtnap_window_errdate(const uint32_t wid, HB_ITEM *error_date_block)
{
    GtNapWindow *gtwin = i_gtwin(GTNAP_GLOBAL, wid);
    cassert_no_null(gtwin);
    cassert(gtwin->error_date_block == NULL);
    gtwin->error_date_block = hb_itemNew(error_date_block);
}

/*---------------------------------------------------------------------------*/

void hb_gtnap_window_scroll(const uint32_t wid, const int32_t top, const int32_t left, const int32_t bottom, const int32_t right)
{
    GtNapWindow *gtwin = i_gtwin(GTNAP_GLOBAL, wid);
    cassert(gtwin->scroll_top == INT32_MIN);
    cassert(gtwin->is_configured == FALSE);
    gtwin->scroll_top = top;
    gtwin->scroll_left = left;
    gtwin->scroll_bottom = bottom;
    gtwin->scroll_right = right;
}

/*---------------------------------------------------------------------------*/

static GtNapObject *i_focus_obj(GtNapWindow *gtwin)
{
    GtNapObject *focus = NULL;
    arrpt_foreach(gtobj, gtwin->objects, GtNapObject)
        if (gtobj->has_focus == TRUE)
        {
            cassert(focus == NULL);
            focus = gtobj;
        }
    arrpt_end();
    return focus;
}

/*---------------------------------------------------------------------------*/

void hb_gtnap_window_copy(const uint32_t wid)
{
    GtNapWindow *gtwin = i_gtwin(GTNAP_GLOBAL, wid);
    GtNapObject *gtobj = i_focus_obj(gtwin);
    if (gtobj != NULL)
    {
        if (gtobj->type == ekOBJ_EDIT)
            edit_copy((Edit *)gtobj->component);
        else if (gtobj->type == ekOBJ_TEXTVIEW)
            textview_copy((TextView *)gtobj->component);
    }
}

/*---------------------------------------------------------------------------*/

void hb_gtnap_window_paste(const uint32_t wid)
{
    GtNapWindow *gtwin = i_gtwin(GTNAP_GLOBAL, wid);
    GtNapObject *gtobj = i_focus_obj(gtwin);
    if (gtobj != NULL)
    {
        if (gtobj->type == ekOBJ_EDIT)
            edit_paste((Edit *)gtobj->component);
        else if (gtobj->type == ekOBJ_TEXTVIEW)
            textview_paste((TextView *)gtobj->component);
    }
}

/*---------------------------------------------------------------------------*/

void hb_gtnap_window_cut(const uint32_t wid)
{
    GtNapWindow *gtwin = i_gtwin(GTNAP_GLOBAL, wid);
    GtNapObject *gtobj = i_focus_obj(gtwin);
    if (gtobj != NULL)
    {
        if (gtobj->type == ekOBJ_EDIT)
            edit_cut((Edit *)gtobj->component);
        else if (gtobj->type == ekOBJ_TEXTVIEW)
            textview_cut((TextView *)gtobj->component);
    }
}

/*---------------------------------------------------------------------------*/

void hb_gtnap_window_undo(const uint32_t wid)
{
    GtNapWindow *gtwin = i_gtwin(GTNAP_GLOBAL, wid);
    GtNapObject *gtobj = i_focus_obj(gtwin);
    if (gtobj != NULL)
    {
        if (gtobj->type == ekOBJ_EDIT)
            i_set_edit_text(gtobj);
        else if (gtobj->type == ekOBJ_TEXTVIEW)
            i_set_view_text(gtobj);
    }
}

/*---------------------------------------------------------------------------*/

uint32_t hb_gtnap_window_modal(const uint32_t wid, const uint32_t pwid, const uint32_t delay_seconds)
{
    GtNapWindow *gtwin = i_gtwin(GTNAP_GLOBAL, wid);
    GtNapWindow *pgtwin = pwid > 0 ? i_gtwin(GTNAP_GLOBAL, pwid) : NULL;
    GtNapWindow *embgtwin = NULL;
    cassert_no_null(gtwin);

    /* An embedded window can't be launched as modal. We must launch the parent */
    if (gtwin->parent_id != UINT32_MAX)
    {
        embgtwin = gtwin;
        gtwin = i_gtwin(GTNAP_GLOBAL, gtwin->parent_id);
        cassert_no_null(gtwin);
    }

    {
        V2Df pos;
        uint32_t ret = 0;

        if (gtwin->is_configured == FALSE)
            i_gtwin_configure(GTNAP_GLOBAL, gtwin, gtwin);

        if (gtwin->buttons_navigation == TRUE)
        {
            uint32_t n = i_num_buttons(gtwin);
            if (n > 0)
            {
                if (gtwin->default_button == UINT32_MAX)
                    gtwin->default_button = 0;
                cassert(gtwin->default_button < n);
                i_set_defbutton(gtwin);
            }
        }

        pos.x = (real32_t)gtwin->left * GTNAP_GLOBAL->cell_x_sizef;
        pos.y = (real32_t)gtwin->top * GTNAP_GLOBAL->cell_y_sizef;

        if (arrpt_size(GTNAP_GLOBAL->windows, GtNapWindow) > 0)
        {
            GtNapWindow *base = arrpt_first(GTNAP_GLOBAL->windows, GtNapWindow);
            V2Df ppos;
            cassert_no_null(base);
            ppos = window_get_origin(base->window);
            pos.x += ppos.x;
            pos.y += ppos.y;
        }

        cassert(gtwin->window != NULL);
        /* Allow arrows/intro in TextView */
        if (embgtwin != NULL && i_num_texts(embgtwin) > 0)
        {
            window_hotkey(gtwin->window, ekKEY_UP, 0, NULL);
            window_hotkey(gtwin->window, ekKEY_DOWN, 0, NULL);
            window_hotkey(gtwin->window, ekKEY_RETURN, 0, NULL);
        }
        /* Allow arrows/intro navigation between editboxes */
        else if (i_num_edits(gtwin) > 0)
        {
            window_hotkey(gtwin->window, ekKEY_UP, 0, listener(gtwin, i_OnPreviousEdit, GtNapWindow));
            window_hotkey(gtwin->window, ekKEY_DOWN, 0, listener(gtwin, i_OnNextTabstop, GtNapWindow));
            window_hotkey(gtwin->window, ekKEY_RETURN, 0, listener(gtwin, i_OnNextTabstop, GtNapWindow));
            window_hotkey(gtwin->window, ekKEY_NUMRET, 0, listener(gtwin, i_OnNextTabstop, GtNapWindow));
        }

        window_origin(gtwin->window, pos);
        gtwin->is_closed_by_esc = FALSE;
        gtwin->modal_window_alive = TRUE;

        if (delay_seconds > 0)
        {
            GTNAP_GLOBAL->modal_timestamp = btime_now();
            GTNAP_GLOBAL->modal_delay_seconds = delay_seconds;
            GTNAP_GLOBAL->modal_time_window = gtwin;
        }
        else
        {
            GTNAP_GLOBAL->modal_timestamp = 0;
            GTNAP_GLOBAL->modal_delay_seconds = 0;
            GTNAP_GLOBAL->modal_time_window = NULL;
        }

        ret = window_modal(gtwin->window, pgtwin ? pgtwin->window : NULL);
        return ret;
    }
}

/*---------------------------------------------------------------------------*/

void hb_gtnap_window_stop_modal(const uint32_t wid, const uint32_t result)
{
    GtNapWindow *gtwin = i_gtwin(GTNAP_GLOBAL, wid);
    i_stop_modal(GTNAP_GLOBAL, gtwin, result);
}

/*---------------------------------------------------------------------------*/

static uint32_t i_add_object(const objtype_t type, const int32_t top, const int32_t left, const real32_t cell_x_sizef, const real32_t cell_y_sizef, const S2Df *size, const bool_t in_scroll, GuiComponent *component, GtNapWindow *gtwin)
{
    uint32_t id;
    GtNapObject *obj = NULL;
    cassert_no_null(gtwin);
    cassert_no_null(size);
    id = arrpt_size(gtwin->objects, GtNapObject);
    obj = heap_new0(GtNapObject);
    arrpt_append(gtwin->objects, obj, GtNapObject);
    obj->type = type;
    obj->top = top;
    obj->left = left;
    obj->component = component;
    obj->pos.x = (real32_t)left * cell_x_sizef;
    obj->pos.y = (real32_t)top * cell_y_sizef;
    obj->size = *size;
    obj->is_last_edit = FALSE;
    obj->in_scroll = in_scroll;
    obj->can_auto_lista = TRUE;
    obj->has_focus = FALSE;
    obj->editBoxIndexForButton = UINT32_MAX;
    obj->gtwin = gtwin;
    return id;
}

/*---------------------------------------------------------------------------*/

static uint32_t i_add_label(const int32_t top, const int32_t left, const bool_t in_scroll, GtNapWindow *gtwin, GtNap *gtnap)
{
    Label *label = label_create();
    S2Df size;
    cassert_no_null(gtnap);
    label_font(label, gtnap->global_font);
    size.width = gtnap->cell_x_sizef;
    size.height = gtnap->label_y_sizef;
    return i_add_object(ekOBJ_LABEL, top, left, gtnap->cell_x_sizef, gtnap->cell_y_sizef, &size, in_scroll, (GuiComponent *)label, gtwin);
}

/*---------------------------------------------------------------------------*/

uint32_t hb_gtnap_label(const uint32_t wid, const int32_t top, const int32_t left, HB_ITEM *text_block, const bool_t in_scroll)
{
    GtNapWindow *gtwin = i_gtwin(GTNAP_GLOBAL, wid);
    uint32_t id;
    GtNapObject *obj;
    cassert(gtwin->is_configured == FALSE);
    id = i_add_label(top - gtwin->top, left - gtwin->left, in_scroll, gtwin, GTNAP_GLOBAL);
    obj = arrpt_last(gtwin->objects, GtNapObject);
    cassert_no_null(obj);
    cassert(obj->type == ekOBJ_LABEL);

    if (text_block != NULL)
    {
        if (HB_ITEM_TYPE(text_block) == HB_IT_STRING)
        {
            obj->text = i_item_to_utf8_string(text_block);
        }
        else
        {
            cassert(HB_ITEM_TYPE(text_block) == HB_IT_BLOCK);
            obj->text_block = hb_itemNew(text_block);
        }
    }

    i_set_label_text(obj, NULL);
    return id;
}

/*---------------------------------------------------------------------------*/

uint32_t hb_gtnap_label_message(const uint32_t wid, const int32_t top, const int32_t left, const bool_t in_scroll)
{
    GtNapWindow *gtwin = i_gtwin(GTNAP_GLOBAL, wid);
    cassert(gtwin->is_configured == FALSE);
    cassert(gtwin->message_label_id == UINT32_MAX);
    gtwin->message_label_id = i_add_label(top - gtwin->top, left - gtwin->left, in_scroll, gtwin, GTNAP_GLOBAL);
    return gtwin->message_label_id;
}

/*---------------------------------------------------------------------------*/

void hb_gtnap_label_update(const uint32_t wid, const uint32_t id, const int32_t top, const int32_t left, HB_ITEM *text_block)
{
    GtNapObject *gtobj = i_gtobj(GTNAP_GLOBAL, wid, id);
    GtNapWindow *gtwin = NULL;
    cassert_no_null(gtobj);
    cassert(gtobj->type == ekOBJ_LABEL);
    gtwin = gtobj->gtwin;
    cassert_no_null(gtwin);

    str_destopt(&gtobj->text);

    if (gtobj->text_block != NULL)
    {
        hb_itemRelease(gtobj->text_block);
        gtobj->text_block = NULL;
    }

    if (text_block != NULL)
    {
        if (HB_ITEM_TYPE(text_block) == HB_IT_STRING)
        {
            gtobj->text = i_item_to_utf8_string(text_block);
        }
        else
        {
            cassert(HB_ITEM_TYPE(text_block) == HB_IT_BLOCK);
            gtobj->text_block = hb_itemNew(text_block);
        }
    }

    gtobj->pos.x = (real32_t)(left - gtwin->left) * GTNAP_GLOBAL->cell_x_sizef;
    gtobj->pos.y = (real32_t)(top - gtwin->top) * GTNAP_GLOBAL->cell_y_sizef;

    i_set_label_text(gtobj, NULL);
}

/*---------------------------------------------------------------------------*/

void hb_gtnap_label_fgcolor(const uint32_t wid, const uint32_t id, const color_t color)
{
    GtNapObject *obj = i_gtobj(GTNAP_GLOBAL, wid, id);
    cassert_no_null(obj);
    cassert(obj->type == ekOBJ_LABEL);
    label_color((Label *)obj->component, color);
}

/*---------------------------------------------------------------------------*/

void hb_gtnap_label_bgcolor(const uint32_t wid, const uint32_t id, const color_t color)
{
    GtNapObject *obj = i_gtobj(GTNAP_GLOBAL, wid, id);
    cassert_no_null(obj);
    cassert(obj->type == ekOBJ_LABEL);
    label_bgcolor((Label *)obj->component, color);
}

/*---------------------------------------------------------------------------*/

void hb_gtnap_label_color(const uint32_t wid, const uint32_t id, const char_t *hb_color)
{
    int attr = hb_gtColorToN(hb_color);
    int fore = attr & 0x000F;
    int back = (attr & 0x00F0) >> 4;
    GtNapObject *obj = i_gtobj(GTNAP_GLOBAL, wid, id);
    cassert_no_null(obj);
    cassert(obj->type == ekOBJ_LABEL);
    cassert(fore < 16);
    cassert(back < 16);
    label_color((Label *)obj->component, (fore != COL_BLACK) ? i_COLORS[fore] : kCOLOR_DEFAULT);
    label_bgcolor((Label *)obj->component, (back != COL_WHITE) ? i_COLORS[back] : kCOLOR_DEFAULT);
}

/*---------------------------------------------------------------------------*/

static void i_OnButtonClick(GtNapCallback *callback, Event *e)
{
    cassert_no_null(callback);
    cassert_no_null(callback->gtwin);
    unref(e);
    if (callback->block != NULL)
    {
        PHB_ITEM ritem = hb_itemDo(callback->block, 0);
        hb_itemRelease(ritem);
    }

    if (callback->autoclose_id != UINT32_MAX)
        i_stop_modal(GTNAP_GLOBAL, callback->gtwin, NAP_MODAL_BUTTON_AUTOCLOSE + callback->autoclose_id);
}

/*---------------------------------------------------------------------------*/

static void i_set_button_text(GtNapObject *obj)
{
    cassert_no_null(obj);
    cassert(obj->type == ekOBJ_BUTTON);
    if (obj->text_block != NULL)
    {
        PHB_ITEM ritem = hb_itemDo(obj->text_block, 0);
        char_t utf8[STATIC_TEXT_SIZE];
        cassert(HB_ITEM_TYPE(ritem) == HB_IT_STRING);
        i_item_to_utf8(ritem, utf8, sizeof32(utf8));
        button_text((Button *)obj->component, utf8);
        hb_itemRelease(ritem);
    }
}

/*---------------------------------------------------------------------------*/

uint32_t hb_gtnap_button(const uint32_t wid, const int32_t top, const int32_t left, const int32_t bottom, const int32_t right, HB_ITEM *text_block, HB_ITEM *click_block, const bool_t autoclose, const bool_t in_scroll)
{
    GtNapWindow *gtwin = i_gtwin(GTNAP_GLOBAL, wid);
    uint32_t autoclose_id = autoclose ? i_num_buttons(gtwin) + 1 : UINT32_MAX;
    Button *button = NULL;
    Listener *listener = NULL;
    S2Df size;
    uint32_t id = UINT32_MAX;
    cassert_no_null(gtwin);
    button = button_push();
    listener = i_gtnap_listener(click_block, INT32_MAX, autoclose_id, gtwin, i_OnButtonClick);
    button_OnClick(button, listener);
    button_font(button, GTNAP_GLOBAL->button_font);
    button_vpadding(button, i_button_vpadding());
    cassert(bottom == top);
    size.width = (real32_t)(right - left + 1) * GTNAP_GLOBAL->cell_x_sizef;
    size.height = (real32_t)(bottom - top + 1) * GTNAP_GLOBAL->button_y_sizef;
    id = i_add_object(ekOBJ_BUTTON, top - gtwin->top, left - gtwin->left, GTNAP_GLOBAL->cell_x_sizef, GTNAP_GLOBAL->cell_y_sizef, &size, in_scroll, (GuiComponent *)button, gtwin);

    if (text_block != NULL)
    {
        GtNapObject *obj = arrpt_last(gtwin->objects, GtNapObject);
        cassert_no_null(obj);
        cassert(obj->type == ekOBJ_BUTTON);
        obj->text_block = hb_itemNew(text_block);
        i_set_button_text(obj);
    }

    return id;
}

/*---------------------------------------------------------------------------*/

static void i_OnImageClick(GtNapCallback *callback, Event *e)
{
    cassert_no_null(callback);
    cassert_no_null(callback->gtwin);
    unref(e);
    if (callback->block != NULL)
    {
        PHB_ITEM ritem = hb_itemDo(callback->block, 0);
        hb_itemRelease(ritem);
    }

    if (callback->autoclose_id != UINT32_MAX)
        i_stop_modal(GTNAP_GLOBAL, callback->gtwin, NAP_MODAL_IMAGE_AUTOCLOSE + callback->autoclose_id);
}

/*---------------------------------------------------------------------------*/

uint32_t hb_gtnap_image(const uint32_t wid, const int32_t top, const int32_t left, const int32_t bottom, const int32_t right, const char_t *pathname, HB_ITEM *click_block, const bool_t autoclose, const bool_t in_scroll)
{
    Image *image = NULL;
    char_t utf8[STATIC_TEXT_SIZE];
    i_cp_to_utf8(pathname, utf8, sizeof32(utf8));
    image = image_from_file(utf8, NULL);

    if (image != NULL)
    {
        GtNapWindow *gtwin = i_gtwin(GTNAP_GLOBAL, wid);
        uint32_t autoclose_id = autoclose ? i_num_images(gtwin) + 1 : UINT32_MAX;
        ImageView *view;
        Listener *listener;
        S2Df size;
        cassert_no_null(gtwin);
        view = imageview_create();
        listener = i_gtnap_listener(click_block, INT32_MAX, autoclose_id, gtwin, i_OnImageClick);
        view_OnClick((View *)view, listener);
        imageview_scale(view, ekGUI_SCALE_AUTO);
        size.width = (real32_t)(right - left + 1) * GTNAP_GLOBAL->cell_x_sizef;
        size.height = (real32_t)(bottom - top + 1) * GTNAP_GLOBAL->cell_y_sizef;
        imageview_image(view, image);
        image_destroy(&image);
        return i_add_object(ekOBJ_IMAGE, top - gtwin->top, left - gtwin->left, GTNAP_GLOBAL->cell_x_sizef, GTNAP_GLOBAL->cell_y_sizef, &size, in_scroll, (GuiComponent *)view, gtwin);
    }

    return UINT32_MAX;
}

/*---------------------------------------------------------------------------*/

uint32_t hb_gtnap_edit(const uint32_t wid, const int32_t top, const int32_t left, const uint32_t width, const char_t type, HB_ITEM *get_set_block, HB_ITEM *is_editable_block, HB_ITEM *when_block, HB_ITEM *valida_block, HB_ITEM *message_block, HB_ITEM *keyfilter_block, const bool_t in_scroll)
{
    GtNapWindow *gtwin = i_gtwin(GTNAP_GLOBAL, wid);
    Edit *edit = edit_create();
    S2Df size;
    uint32_t id = UINT32_MAX;
    GtNapObject *obj = NULL;
    cassert_no_null(gtwin);
    edit_font(edit, GTNAP_GLOBAL->edit_font);
    edit_vpadding(edit, i_edit_vpadding());
    size.width = (real32_t)(width + 1) * GTNAP_GLOBAL->cell_x_sizef;
    size.height = GTNAP_GLOBAL->edit_y_sizef;
    id = i_add_object(ekOBJ_EDIT, top - gtwin->top, left - gtwin->left, GTNAP_GLOBAL->cell_x_sizef, GTNAP_GLOBAL->cell_y_sizef, &size, in_scroll, (GuiComponent *)edit, gtwin);
    obj = arrpt_last(gtwin->objects, GtNapObject);
    cassert_no_null(obj);
    cassert(obj->type = ekOBJ_EDIT);

    obj->max_chars = width;

    if (type == 'C')
    {
        obj->dtype = ekTYPE_CHARACTER;
    }
    else if (type == 'D')
    {
        obj->dtype = ekTYPE_DATE;
    }
    else
    {
        obj->dtype = ENUM_MAX(datatype_t);
        cassert(FALSE);
    }

    if (get_set_block != NULL)
        obj->get_set_block = hb_itemNew(get_set_block);

    if (is_editable_block != NULL)
        obj->is_editable_block = hb_itemNew(is_editable_block);

    if (when_block != NULL)
        obj->when_block = hb_itemNew(when_block);

    if (valida_block != NULL)
        obj->valida_block = hb_itemNew(valida_block);

    if (message_block != NULL)
        obj->message_block = hb_itemNew(message_block);

    if (keyfilter_block != NULL)
        obj->keyfilter_block = hb_itemNew(keyfilter_block);

    i_set_edit_text(obj);

    return id;
}

/*---------------------------------------------------------------------------*/

void hb_gtnap_edit_color(const uint32_t wid, const uint32_t id, const char_t *hb_color)
{
    ArrPt(String) *hbcols = str_splits(hb_color, ",", TRUE, FALSE);
    GtNapObject *obj = i_gtobj(GTNAP_GLOBAL, wid, id);
    cassert_no_null(obj);
    cassert(obj->type == ekOBJ_EDIT);

    if (arrpt_size(hbcols, String) > 0)
    {
        const String *c = arrpt_get_const(hbcols, 0, String);
        int attr = hb_gtColorToN(tc(c));
        int fore = attr & 0x000F;
        int back = (attr & 0x00F0) >> 4;
        cassert(fore < 16);
        cassert(back < 16);
        edit_color((Edit *)obj->component, (fore != COL_BLACK) ? i_COLORS[fore] : kCOLOR_DEFAULT);
        edit_bgcolor((Edit *)obj->component, (back != COL_BRIGHT_WHITE) ? i_COLORS[back] : kCOLOR_DEFAULT);
    }

    if (arrpt_size(hbcols, String) > 1)
    {
        const String *c = arrpt_get_const(hbcols, 1, String);
        int attr = hb_gtColorToN(tc(c));
        int fore = attr & 0x000F;
        int back = (attr & 0x00F0) >> 4;
        cassert(fore < 16);
        cassert(back < 16);
        edit_color_focus((Edit *)obj->component, (fore != COL_BLACK) ? i_COLORS[fore] : kCOLOR_DEFAULT);
        edit_bgcolor_focus((Edit *)obj->component, (back != COL_BRIGHT_WHITE) ? i_COLORS[back] : kCOLOR_DEFAULT);
    }

    arrpt_destroy(&hbcols, str_destroy, String);
}

/*---------------------------------------------------------------------------*/

static void i_OnWizardButton(GtNapCallback *callback, Event *e)
{
    GtNapObject *obj = NULL;
    cassert_no_null(callback);
    cassert_no_null(callback->gtwin);
    cassert(callback->autoclose_id == UINT32_MAX);
    unref(e);
    obj = arrpt_get(callback->gtwin->objects, callback->key, GtNapObject);
    i_launch_wizard(callback->gtwin, obj);
}

/*---------------------------------------------------------------------------*/

static void i_OnKeyWizard(GtNapWindow *gtwin, Event *e)
{
    cassert_no_null(gtwin);
    unref(e);
    arrpt_foreach(obj, gtwin->objects, GtNapObject)
        if (obj->type == ekOBJ_EDIT)
        {
            if (obj->has_focus == TRUE)
            {
                if (obj->wizard_block != NULL)
                    i_launch_wizard(gtwin, obj);
                return;
            }
        }
    arrpt_end();
}

/*---------------------------------------------------------------------------*/

void hb_gtnap_edit_wizard(const uint32_t wid, const uint32_t id, const uint32_t bid, int32_t key, HB_ITEM *auto_block, HB_ITEM *wizard_block)
{
    GtNapObject *obj = i_gtobj(GTNAP_GLOBAL, wid, id);
    const GtNapKey *nkey = i_convert_key(key);
    cassert_no_null(obj);
    cassert(obj->type == ekOBJ_EDIT);
    cassert(obj->auto_block == NULL);
    cassert(obj->wizard_block == NULL);
    unref(key);
    if (auto_block != NULL)
        obj->auto_block = hb_itemNew(auto_block);

    if (wizard_block != NULL)
        obj->wizard_block = hb_itemNew(wizard_block);

    if (bid != UINT32_MAX)
    {
        GtNapObject *bobj = i_gtobj(GTNAP_GLOBAL, wid, bid);
        Listener *listener = NULL;
        cassert_no_null(bobj);
        cassert(bobj->type == ekOBJ_BUTTON);
        listener = i_gtnap_listener(NULL, id, UINT32_MAX, bobj->gtwin, i_OnWizardButton);
        bobj->editBoxIndexForButton = id;
        button_OnClick((Button *)bobj->component, listener);

        {
            char_t text[8];
            uint32_t b = unicode_to_char(0x25BE, text, ekUTF8);
            text[b] = '\0';
            button_text((Button *)bobj->component, text);
        }
    }

    if (nkey != NULL)
        window_hotkey(obj->gtwin->window, nkey->vkey, 0, listener(obj->gtwin, i_OnKeyWizard, GtNapWindow));
}

/*---------------------------------------------------------------------------*/

uint32_t hb_gtnap_textview(const uint32_t wid, const int32_t top, const int32_t left, const int32_t bottom, const int32_t right, HB_ITEM *get_set_block, HB_ITEM *valida_block, HB_ITEM *keyfilter_block, const bool_t in_scroll)
{
    GtNapWindow *gtwin = i_gtwin(GTNAP_GLOBAL, wid);
    TextView *view = textview_create();
    S2Df size;
    uint32_t id = UINT32_MAX;
    GtNapObject *obj = NULL;
    bool_t is_editable = FALSE;
    cassert_no_null(gtwin);
    textview_family(view, font_family(GTNAP_GLOBAL->global_font));
    textview_fsize(view, font_size(GTNAP_GLOBAL->global_font));
    size.width = (real32_t)(right - left + 1) * GTNAP_GLOBAL->cell_x_sizef;
    size.height = (real32_t)(bottom - top + 1) * GTNAP_GLOBAL->cell_y_sizef;
    id = i_add_object(ekOBJ_TEXTVIEW, top - gtwin->top, left - gtwin->left, GTNAP_GLOBAL->cell_x_sizef, GTNAP_GLOBAL->cell_y_sizef, &size, in_scroll, (GuiComponent *)view, gtwin);
    obj = arrpt_last(gtwin->objects, GtNapObject);
    cassert_no_null(obj);
    cassert(obj->type = ekOBJ_TEXTVIEW);

    if (get_set_block != NULL)
        obj->get_set_block = hb_itemNew(get_set_block);

    if (valida_block != NULL)
        obj->valida_block = hb_itemNew(valida_block);

    if (keyfilter_block != NULL)
        obj->keyfilter_block = hb_itemNew(keyfilter_block);

    i_set_view_text(obj);
    is_editable = i_is_editable(gtwin, obj);
    textview_editable(view, is_editable);
    return id;
}

/*---------------------------------------------------------------------------*/

void hb_gtnap_textview_scroll(const uint32_t wid, const uint32_t id, const bool_t horizontal, const bool_t vertical)
{
    GtNapObject *obj = i_gtobj(GTNAP_GLOBAL, wid, id);
    cassert_no_null(obj);
    cassert(obj->type == ekOBJ_TEXTVIEW);
    textview_scroll_visible((TextView *)obj->component, horizontal, vertical);
}

/*---------------------------------------------------------------------------*/

void hb_gtnap_textview_caret(const uint32_t wid, const uint32_t id, const int64_t pos)
{
    GtNapObject *obj = i_gtobj(GTNAP_GLOBAL, wid, id);
    cassert_no_null(obj);
    cassert(obj->type == ekOBJ_TEXTVIEW);
    textview_select((TextView *)obj->component, (int32_t)pos, (int32_t)pos);
    textview_scroll_caret((TextView *)obj->component);
}

/*---------------------------------------------------------------------------*/

static void i_OnTextConfirm(GtNapObject *gtobj, Event *e)
{
    cassert_no_null(gtobj);
    cassert(gtobj->type == ekOBJ_TEXTVIEW);
    cassert_no_null(gtobj->gtwin);
    unref(e);
    if (gtobj->get_set_block != NULL)
    {
        bool_t valid = TRUE;
        bool_t confirm = FALSE;
        HB_ITEM *citem = hb_itemDo(gtobj->get_set_block, 0);
        cassert_no_null(citem);
        cassert(HB_ITEM_TYPE(citem) == HB_IT_STRING);

        /* Overwrite the Harbour variable with the current TextView text (for validation) */
        {
            const char_t *utf8 = textview_get_text((const TextView *)gtobj->component);
            String *cpstr = i_utf8_to_cp_string(utf8);
            HB_ITEM *item = hb_itemPutC(NULL, tc(cpstr));

            if (item != NULL)
            {
                HB_ITEM *ritem = hb_itemDo(gtobj->get_set_block, 1, item);
                hb_itemRelease(item);
                hb_itemRelease(ritem);
            }

            str_destroy(&cpstr);
        }

        /* Validate the text */
        if (gtobj->valida_block != NULL)
        {
            HB_ITEM *ritem = hb_itemDo(gtobj->valida_block, 0);
            cassert(HB_ITEM_TYPE(ritem) == HB_IT_LOGICAL);
            valid = (bool_t)hb_itemGetL(ritem);
            hb_itemRelease(ritem);
        }

        /* Confirma the text */
        if (valid == TRUE)
        {
            if (gtobj->gtwin->confirm_block != NULL)
            {
                HB_ITEM *ritem = hb_itemDo(gtobj->gtwin->confirm_block, 0);
                cassert(HB_ITEM_TYPE(ritem) == HB_IT_LOGICAL);
                confirm = (bool_t)hb_itemGetL(ritem);
                hb_itemRelease(ritem);
            }
        }

        /* Restore the Harbour variable with the original text */
        if (confirm == FALSE)
        {
            HB_ITEM *ritem = hb_itemDo(gtobj->get_set_block, 1, citem);
            hb_itemRelease(ritem);
        }

        hb_itemRelease(citem);

        if (confirm == TRUE)
        {
            GtNapWindow *gtwin = gtobj->gtwin;
            if (gtwin->parent_id != UINT32_MAX)
                gtwin = i_gtwin(GTNAP_GLOBAL, gtwin->parent_id);
            i_stop_modal(GTNAP_GLOBAL, gtwin, NAP_MODAL_TEXT_CONFIRM);
        }
    }
}

/*---------------------------------------------------------------------------*/

void hb_gtnap_textview_button(const uint32_t wid, const uint32_t id, const uint32_t bid)
{
    GtNapObject *obj = i_gtobj(GTNAP_GLOBAL, wid, id);
    GtNapObject *bobj = i_gtobj(GTNAP_GLOBAL, wid, bid);
    cassert_no_null(obj);
    cassert_no_null(bobj);
    cassert(obj->type == ekOBJ_TEXTVIEW);
    cassert(bobj->type == ekOBJ_BUTTON);
    button_OnClick((Button *)bobj->component, listener(obj, i_OnTextConfirm, GtNapObject));
}

/*---------------------------------------------------------------------------*/

void hb_gtnap_textview_hotkey(uint32_t wid, uint32_t id, int32_t key)
{
    GtNapObject *obj = i_gtobj(GTNAP_GLOBAL, wid, id);
    const GtNapKey *nkey = i_convert_key(key);
    cassert_no_null(obj);
    cassert(obj->type == ekOBJ_TEXTVIEW);
    if (nkey != NULL)
    {
        Window *window = i_effective_window(obj->gtwin, GTNAP_GLOBAL);
        window_hotkey(window, nkey->vkey, nkey->modifiers, listener(obj, i_OnTextConfirm, GtNapObject));
    }
}

/*---------------------------------------------------------------------------*/

uint32_t hb_gtnap_menu(const uint32_t wid, const int32_t top, const int32_t left, const int32_t bottom, const int32_t right, const bool_t autoclose, const bool_t in_scroll)
{
    GtNapWindow *gtwin = i_gtwin(GTNAP_GLOBAL, wid);
    Panel *panel = nap_menu_create(autoclose, GTNAP_GLOBAL->global_font, GTNAP_GLOBAL->cell_x_sizef, GTNAP_GLOBAL->cell_y_sizef);
    S2Df size, final_size;
    uint32_t id = UINT32_MAX;
    cassert_no_null(gtwin);
    size.width = (real32_t)(right - left + 1) * GTNAP_GLOBAL->cell_x_sizef;
    size.height = (real32_t)(bottom - top + 1) * GTNAP_GLOBAL->cell_y_sizef;
    _panel_compose(panel, &size, &final_size);
    _panel_locate(panel);
    id = i_add_object(ekOBJ_MENU, top - gtwin->top, left - gtwin->left, GTNAP_GLOBAL->cell_x_sizef, GTNAP_GLOBAL->cell_y_sizef, &size, in_scroll, (GuiComponent *)panel, gtwin);
    return id;
}

/*---------------------------------------------------------------------------*/

void hb_gtnap_menu_add(const uint32_t wid, uint32_t id, HB_ITEM *text_block, HB_ITEM *click_block, uint32_t kpos)
{
    GtNapObject *gtobj = i_gtobj(GTNAP_GLOBAL, wid, id);
    cassert_no_null(gtobj);
    cassert_no_null(gtobj->gtwin);
    cassert(gtobj->type == ekOBJ_MENU);
    nap_menu_add((Panel *)gtobj->component, gtobj->gtwin->window, text_block, click_block, kpos);
}

/*---------------------------------------------------------------------------*/

uint32_t hb_gtnap_menu_selected(const uint32_t wid, uint32_t id)
{
    GtNapObject *gtobj = i_gtobj(GTNAP_GLOBAL, wid, id);
    cassert_no_null(gtobj);
    cassert(gtobj->type == ekOBJ_MENU);
    return nap_menu_selected((Panel *)gtobj->component);
}

/*---------------------------------------------------------------------------*/

static bool_t i_in_vect(const ArrSt(uint32_t) *sel, const uint32_t i)
{
    arrst_foreach_const(id, sel, uint32_t)
        if (*id == i)
            return TRUE;
    arrst_end();
    return FALSE;
}

/*---------------------------------------------------------------------------*/

static void i_toogle_sel(TableView *view, const ArrSt(uint32_t) *sel, const uint32_t row)
{
    if (i_in_vect(sel, row) == TRUE)
        tableview_deselect(view, &row, 1);
    else
        tableview_select(view, &row, 1);
}

/*---------------------------------------------------------------------------*/

static void i_OnTableRowClick(GtNapObject *gtobj, Event *e)
{
    const EvTbRow *p = event_params(e, EvTbRow);
    cassert_no_null(gtobj);
    cassert(gtobj->type == ekOBJ_TABLEVIEW);
    if (gtobj->multisel == TRUE)
    {
        /* The row has not been selected in click --> We force the selection */
        if (p->sel == FALSE)
        {
            const ArrSt(uint32_t) *sel = tableview_selected((TableView *)gtobj->component);
            i_toogle_sel((TableView *)gtobj->component, sel, p->row);
            tableview_update((TableView *)gtobj->component);
        }
    }
    else if (gtobj->autoclose == TRUE)
    {
        uint32_t ret_value = NAP_MODAL_ROW_CLICK;
        GtNapWindow *gtwin = gtobj->gtwin;
        GtNapArea *gtarea = NULL;
        cassert_no_null(gtwin);
        gtarea = gtwin->gtarea;
        if (gtarea != NULL)
        {
            uint32_t *recno = arrst_get(gtarea->records, p->row, uint32_t);
            cassert_no_null(recno);
            ret_value += *recno;
        }
        else
        {
            ret_value += p->row + 1;
        }

        i_stop_modal(GTNAP_GLOBAL, gtwin, ret_value);
    }
}

/*---------------------------------------------------------------------------*/

uint32_t hb_gtnap_tableview(const uint32_t wid, const int32_t top, const int32_t left, const int32_t bottom, const int32_t right, const bool_t multisel, const bool_t autoclose, const bool_t in_scroll)
{
    GtNapWindow *gtwin = i_gtwin(GTNAP_GLOBAL, wid);
    TableView *view = tableview_create();
    S2Df size;
    uint32_t id = UINT32_MAX;
    GtNapObject *obj = NULL;
    cassert_no_null(gtwin);
    cassert(gtwin->gtarea == NULL);
    tableview_font(view, GTNAP_GLOBAL->global_font);
    tableview_multisel(view, multisel, multisel);
    size.width = (real32_t)(right - left + 1) * GTNAP_GLOBAL->cell_x_sizef;
    size.height = (real32_t)(bottom - top + 1) * GTNAP_GLOBAL->cell_y_sizef;
    id = i_add_object(ekOBJ_TABLEVIEW, top - gtwin->top, left - gtwin->left, GTNAP_GLOBAL->cell_x_sizef, GTNAP_GLOBAL->cell_y_sizef, &size, in_scroll, (GuiComponent *)view, gtwin);
    obj = arrpt_last(gtwin->objects, GtNapObject);
    cassert_no_null(obj);
    cassert(obj->type = ekOBJ_TABLEVIEW);
    obj->multisel = multisel;
    obj->autoclose = autoclose;
    tableview_OnRowClick(view, listener(obj, i_OnTableRowClick, GtNapObject));
    tableview_row_height(view, GTNAP_GLOBAL->cell_y_sizef);
    tableview_hkey_scroll(view, TRUE, 0.f);
    return id;
}

/*---------------------------------------------------------------------------*/

static uint32_t i_header_char_width(const char_t *title, uint32_t *nlines)
{
    uint32_t nchars = 0;
    ArrPt(String) *strs = str_splits(title, "\n", TRUE, FALSE);
    cassert_no_null(nlines);
    *nlines = arrpt_size(strs, String);
    arrpt_foreach_const(str, strs, String)
        uint32_t n = unicode_nchars(tc(str), ekUTF8);
        if (n > nchars)
            nchars = n;
    arrpt_end();

    arrpt_destroy(&strs, str_destroy, String);
    return nchars;
}

/*---------------------------------------------------------------------------*/

static real32_t i_col_widthf(const uint32_t fixed_chars, const uint32_t str_chars, const GtNap *gtnap)
{
    uint32_t cwidth = fixed_chars;
    cassert_no_null(gtnap);

    if (cwidth == 0)
        cwidth = str_chars;

    return (real32_t)(cwidth + 1) * gtnap->cell_x_sizef;
}

/*---------------------------------------------------------------------------*/

void hb_gtnap_tableview_column(const uint32_t wid, const uint32_t id, const uint32_t width, HB_ITEM *head_block, HB_ITEM *eval_block)
{
    GtNapObject *obj = i_gtobj(GTNAP_GLOBAL, wid, id);
    GtNapColumn *col = NULL;
    uint32_t cid = UINT32_MAX;
    uint32_t hnchars = 0;
    uint32_t nlines = 0;
    cassert_no_null(obj);
    cassert(obj->type == ekOBJ_TABLEVIEW);

    if (obj->columns == NULL)
        obj->columns = arrst_create(GtNapColumn);

    cid = tableview_new_column_text((TableView *)obj->component);
    cassert(cid == arrst_size(obj->columns, GtNapColumn));

    col = arrst_new(obj->columns, GtNapColumn);

    if (head_block != NULL)
    {
        PHB_ITEM ritem = hb_itemDo(head_block, 0);
        col->title = i_item_to_utf8_string(ritem);
        hb_itemRelease(ritem);
    }
    else
    {
        col->title = str_c("");
    }

    str_repl_c(tcc(col->title), ";", "\n");
    hnchars = i_header_char_width(tc(col->title), &col->header_lines);
    col->fixed_chars = width;
    col->widthf = i_col_widthf(col->fixed_chars, hnchars, GTNAP_GLOBAL);
    col->align = ekLEFT;
    col->block = hb_itemNew(eval_block);

    arrst_foreach(c, obj->columns, GtNapColumn)
        if (c->header_lines > nlines)
            nlines = c->header_lines;
    arrst_end();

    tableview_header_title((TableView *)obj->component, cid, tc(col->title));
    tableview_column_width((TableView *)obj->component, cid, col->widthf);
    tableview_header_align((TableView *)obj->component, cid, col->align);
    tableview_header_height((TableView *)obj->component, (real32_t)nlines * GTNAP_GLOBAL->cell_y_sizef);
}

/*---------------------------------------------------------------------------*/

void hb_gtnap_tableview_scroll(const uint32_t wid, const uint32_t id, const bool_t horizontal, const bool_t vertical)
{
    GtNapObject *obj = i_gtobj(GTNAP_GLOBAL, wid, id);
    cassert_no_null(obj);
    cassert(obj->type == ekOBJ_TABLEVIEW);
    tableview_scroll_visible((TableView *)obj->component, horizontal, vertical);
}

/*---------------------------------------------------------------------------*/

void hb_gtnap_tableview_grid(const uint32_t wid, const uint32_t id, const bool_t hlines, const bool_t vlines)
{
    GtNapObject *obj = i_gtobj(GTNAP_GLOBAL, wid, id);
    cassert_no_null(obj);
    cassert(obj->type == ekOBJ_TABLEVIEW);
    tableview_grid((TableView *)obj->component, hlines, vlines);
}

/*---------------------------------------------------------------------------*/

void hb_gtnap_tableview_header(const uint32_t wid, const uint32_t id, const bool_t visible)
{
    GtNapObject *obj = i_gtobj(GTNAP_GLOBAL, wid, id);
    cassert_no_null(obj);
    cassert(obj->type == ekOBJ_TABLEVIEW);
    tableview_header_visible((TableView *)obj->component, visible);
}

/*---------------------------------------------------------------------------*/

void hb_gtnap_tableview_freeze(const uint32_t wid, const uint32_t id, const uint32_t col_id)
{
    GtNapObject *obj = i_gtobj(GTNAP_GLOBAL, wid, id);
    cassert_no_null(obj);
    cassert(obj->type == ekOBJ_TABLEVIEW);
    cassert(col_id > 0);
    tableview_column_freeze((TableView *)obj->component, col_id - 1);
}

/*---------------------------------------------------------------------------*/

static GtNapArea *i_create_area(void)
{
    GtNapArea *area = heap_new0(GtNapArea);
    area->records = arrst_create(uint32_t);
    area->cache_recno = UINT32_MAX;
    area->while_block = NULL;
    return area;
}

/*---------------------------------------------------------------------------*/

static void i_hbitem_to_char(HB_ITEM *item, char_t *buffer, const uint32_t size)
{
    HB_TYPE type = HB_ITEM_TYPE(item);
    buffer[0] = '\0';

    switch (type)
    {
    case HB_IT_STRING:
        hb_itemCopyStrUTF8(item, buffer, size);
        break;

    case HB_IT_DATE:
    {
        char date[16];
        hb_itemGetDS(item, date);
        hb_dateFormat(date, buffer, hb_setGetDateFormat());
        break;
    }

    case HB_IT_DOUBLE:
    {
        double value = hb_itemGetND(item);
        bstd_sprintf(buffer, size, "%12.4f", value);
        break;
    }

    case HB_IT_LONG:
    case HB_IT_INTEGER:
    {
        HB_MAXINT value = hb_itemGetNInt(item);
        bstd_sprintf(buffer, size, "%d", (int)value);
        break;
    }

    case HB_IT_LOGICAL:
    {
        HB_BOOL value = hb_itemGetL(item);
        bstd_sprintf(buffer, size, "%s", value ? "true" : "false");
        break;
    }

    default:
        buffer[0] = '\0';
    }
}

/*---------------------------------------------------------------------------*/

static const char_t *i_area_eval_field(GtNapArea *gtarea, const uint32_t field_id, const uint32_t row_id, align_t *align)
{
    uint32_t recno = 0;
    const GtNapColumn *column = NULL;
    HB_ITEM *ritem = NULL;

    cassert_no_null(gtarea);
    cassert_no_null(gtarea->gtobj);
    cassert(gtarea->gtobj->type == ekOBJ_TABLEVIEW);
    cassert(field_id > 0);

    /* Go to DB record */
    recno = *arrst_get_const(gtarea->records, row_id, uint32_t);
    SELF_GOTO(gtarea->area, recno);

    /* Get the table column */
    column = arrst_get_const(gtarea->gtobj->columns, field_id - 1, GtNapColumn);

    /* CodeBlock that computes the cell content */
    ritem = hb_itemDo(column->block, 0);

    /* Fill the temporal cell buffer with cell result */
    i_hbitem_to_char(ritem, TEMP_BUFFER, sizeof(TEMP_BUFFER));

    hb_itemRelease(ritem);

    if (align != NULL)
        *align = column->align;

    return TEMP_BUFFER;
}

/*---------------------------------------------------------------------------*/

static void i_area_column_width(GtNapArea *gtarea, const uint32_t col, const char_t *text)
{
    GtNapColumn *column = NULL;
    uint32_t nchars = 0;
    real32_t width = 0;
    cassert_no_null(gtarea);
    cassert_no_null(gtarea->gtobj);
    cassert(gtarea->gtobj->type == ekOBJ_TABLEVIEW);
    column = arrst_get(gtarea->gtobj->columns, col, GtNapColumn);
    nchars = unicode_nchars(text, ekUTF8);
    width = i_col_widthf(column->fixed_chars, nchars, GTNAP_GLOBAL);

    if (width > column->widthf)
    {
        column->widthf = width;
        tableview_column_width((TableView *)gtarea->gtobj->component, col, column->widthf);
    }
}

/*---------------------------------------------------------------------------*/

static void i_OnTableAreaData(GtNapArea *gtarea, Event *e)
{
    uint32_t etype = event_type(e);
    cassert_no_null(gtarea);

    switch (etype)
    {
    case ekGUI_EVENT_TBL_BEGIN:
        SELF_RECNO(gtarea->area, &gtarea->cache_recno);
        break;

    case ekGUI_EVENT_TBL_END:
        SELF_GOTO(gtarea->area, gtarea->cache_recno);
        gtarea->cache_recno = UINT32_MAX;
        break;

    case ekGUI_EVENT_TBL_NROWS:
    {
        uint32_t *n = event_result(e, uint32_t);
        *n = arrst_size(gtarea->records, uint32_t);
        break;
    }

    case ekGUI_EVENT_TBL_CELL:
    {
        EvTbCell *cell = event_result(e, EvTbCell);
        const EvTbPos *pos = event_params(e, EvTbPos);
        cell->text = i_area_eval_field(gtarea, pos->col + 1, pos->row, &cell->align);
        /* Table column automatic width based on cell content */
        i_area_column_width(gtarea, pos->col, cell->text);
        break;
    }

    default:
        break;
    }
}

/*---------------------------------------------------------------------------*/

static void i_OnTableAreaRowSelect(GtNapArea *gtarea, Event *e)
{
    cassert_no_null(gtarea);
    cassert_no_null(gtarea->gtobj);
    cassert(gtarea->gtobj->type == ekOBJ_TABLEVIEW);
    if (gtarea->gtobj->multisel == FALSE)
    {
        const EvTbSel *sel = event_params(e, EvTbSel);
        uint32_t first = 0;
        uint32_t recno = 0;
        cassert(arrst_size(sel->sel, uint32_t) == 1);
        /* The row selected in table */
        first = *arrst_first_const(sel->sel, uint32_t);
        /* The DB RECNO in this row selected in table */
        recno = *arrst_get_const(gtarea->records, first, uint32_t);
        /* Just GOTO */
        SELF_GOTO(gtarea->area, recno);
    }
}

/*---------------------------------------------------------------------------*/

void hb_gtnap_tableview_bind_area(const uint32_t wid, const uint32_t id, HB_ITEM *while_block)
{
    GtNapObject *obj = i_gtobj(GTNAP_GLOBAL, wid, id);
    GtNapWindow *gtwin = NULL;
    cassert_no_null(obj);
    cassert(obj->type == ekOBJ_TABLEVIEW);
    gtwin = obj->gtwin;
    cassert_no_null(gtwin);

    if (gtwin->gtarea != NULL)
        i_destroy_area(&gtwin->gtarea);

    gtwin->gtarea = i_create_area();
    gtwin->gtarea->area = (AREA *)hb_rddGetCurrentWorkAreaPointer();
    gtwin->gtarea->gtobj = obj;

    if (while_block != NULL)
        gtwin->gtarea->while_block = hb_itemNew(while_block);
    else
        gtwin->gtarea->while_block = NULL;

    tableview_OnData((TableView *)obj->component, listener(gtwin->gtarea, i_OnTableAreaData, GtNapArea));
    tableview_OnSelect((TableView *)obj->component, listener(gtwin->gtarea, i_OnTableAreaRowSelect, GtNapArea));
}

/*---------------------------------------------------------------------------*/

static const char_t *i_data_eval_field(GtNapObject *gtobj, const uint32_t col_id, const uint32_t row_id, align_t *align)
{
    const GtNapColumn *column = NULL;
    HB_ITEM *pitem = NULL;
    HB_ITEM *ritem = NULL;

    cassert_no_null(gtobj);
    cassert(gtobj->type == ekOBJ_TABLEVIEW);

    /* Get the table column */
    column = arrst_get_const(gtobj->columns, col_id, GtNapColumn);

    /* TODO: CACHE THIS PARAM */
    pitem = hb_itemPutNI(NULL, row_id + 1);

    /* CodeBlock that computes the cell content */
    ritem = hb_itemDo(column->block, 1, pitem);

    /* Fill the temporal cell buffer with cell result */
    i_hbitem_to_char(ritem, TEMP_BUFFER, sizeof(TEMP_BUFFER));

    hb_itemRelease(pitem);
    hb_itemRelease(ritem);

    if (align != NULL)
        *align = column->align;

    return TEMP_BUFFER;
}

/*---------------------------------------------------------------------------*/

static void i_OnTableData(GtNapObject *gtobj, Event *e)
{
    uint32_t etype = event_type(e);
    cassert_no_null(gtobj);
    cassert(gtobj->type == ekOBJ_TABLEVIEW);
    cassert_no_null(gtobj->gtwin);

    switch (etype)
    {
    case ekGUI_EVENT_TBL_BEGIN:
        break;

    case ekGUI_EVENT_TBL_END:
        break;

    case ekGUI_EVENT_TBL_NROWS:
    {
        uint32_t *n = event_result(e, uint32_t);
        *n = gtobj->gtwin->num_rows;
        break;
    }

    case ekGUI_EVENT_TBL_CELL:
    {
        EvTbCell *cell = event_result(e, EvTbCell);
        const EvTbPos *pos = event_params(e, EvTbPos);
        cell->text = i_data_eval_field(gtobj, pos->col, pos->row, &cell->align);
        break;
    }

    default:
        break;
    }
}

/*---------------------------------------------------------------------------*/

void hb_gtnap_tableview_bind_data(const uint32_t wid, const uint32_t id, const uint32_t num_rows)
{
    GtNapObject *obj = i_gtobj(GTNAP_GLOBAL, wid, id);
    GtNapWindow *gtwin = NULL;
    cassert_no_null(obj);
    cassert(obj->type == ekOBJ_TABLEVIEW);
    gtwin = obj->gtwin;
    cassert_no_null(gtwin);
    gtwin->num_rows = num_rows;
    tableview_OnData((TableView *)obj->component, listener(obj, i_OnTableData, GtNapObject));
    tableview_OnSelect((TableView *)obj->component, NULL);
}

/*---------------------------------------------------------------------------*/

void hb_gtnap_tableview_deselect_all(const uint32_t wid, const uint32_t id)
{
    GtNapObject *obj = i_gtobj(GTNAP_GLOBAL, wid, id);
    cassert_no_null(obj);
    cassert(obj->type == ekOBJ_TABLEVIEW);
    tableview_deselect_all((TableView *)obj->component);
}

/*---------------------------------------------------------------------------*/

void hb_gtnap_tableview_select_row(const uint32_t wid, const uint32_t id, const uint32_t row_id)
{
    GtNapObject *obj = i_gtobj(GTNAP_GLOBAL, wid, id);
    cassert_no_null(obj);
    cassert(obj->type == ekOBJ_TABLEVIEW);
    tableview_select((TableView *)obj->component, &row_id, 1);
    tableview_update((TableView *)obj->component);
}

/*---------------------------------------------------------------------------*/

void hb_gtnap_tableview_toggle_row(const uint32_t wid, const uint32_t id, const uint32_t row_id)
{
    GtNapObject *obj = i_gtobj(GTNAP_GLOBAL, wid, id);
    const ArrSt(uint32_t) *sel = NULL;
    cassert_no_null(obj);
    cassert(obj->type == ekOBJ_TABLEVIEW);
    sel = tableview_selected((TableView *)obj->component);
    i_toogle_sel((TableView *)obj->component, sel, row_id);
    tableview_update((TableView *)obj->component);
}

/*---------------------------------------------------------------------------*/

const ArrSt(uint32_t) *hb_gtnap_tableview_selected_rows(const uint32_t wid, const uint32_t id)
{
    GtNapObject *obj = i_gtobj(GTNAP_GLOBAL, wid, id);
    cassert_no_null(obj);
    cassert(obj->type == ekOBJ_TABLEVIEW);
    return tableview_selected((TableView *)obj->component);
}

/*---------------------------------------------------------------------------*/

uint32_t hb_gtnap_tableview_focus_row(const uint32_t wid, const uint32_t id)
{
    GtNapObject *obj = i_gtobj(GTNAP_GLOBAL, wid, id);
    uint32_t focused;
    cassert_no_null(obj);
    cassert(obj->type == ekOBJ_TABLEVIEW);
    focused = tableview_get_focus_row((TableView *)obj->component);
    return focused;
}

/*---------------------------------------------------------------------------*/

uint32_t hb_gtnap_tableview_recno_from_row(const uint32_t wid, const uint32_t id, const uint32_t row_id)
{
    GtNapObject *obj = i_gtobj(GTNAP_GLOBAL, wid, id);
    GtNapWindow *gtwin = NULL;
    cassert_no_null(obj);
    cassert(obj->type == ekOBJ_TABLEVIEW);
    gtwin = obj->gtwin;
    cassert_no_null(gtwin);
    if (gtwin->gtarea != NULL)
    {
        uint32_t recno = *arrst_get_const(gtwin->gtarea->records, row_id, uint32_t);
        return recno;
    }

    return UINT32_MAX;
}

/*---------------------------------------------------------------------------*/

static uint32_t i_row_from_recno(GtNapArea *area, const uint32_t recno)
{
    cassert_no_null(area);
    cassert(recno > 0);
    arrst_foreach_const(rec, area->records, uint32_t)
        if (*rec == recno)
            return rec_i;
    arrst_end();
    return UINT32_MAX;
}

/*---------------------------------------------------------------------------*/

uint32_t hb_gtnap_tableview_row_from_recno(const uint32_t wid, const uint32_t id, const uint32_t recno)
{
    GtNapObject *obj = i_gtobj(GTNAP_GLOBAL, wid, id);
    GtNapWindow *gtwin = NULL;
    cassert_no_null(obj);
    cassert(obj->type == ekOBJ_TABLEVIEW);
    gtwin = obj->gtwin;
    cassert_no_null(gtwin);
    if (gtwin->gtarea != NULL)
    {
        uint32_t row = i_row_from_recno(gtwin->gtarea, recno);
        return row;
    }

    return UINT32_MAX;
}

/*---------------------------------------------------------------------------*/

void hb_gtnap_tableview_refresh_current(const uint32_t wid, const uint32_t id)
{
    GtNapObject *obj = i_gtobj(GTNAP_GLOBAL, wid, id);
    cassert_no_null(obj);
    cassert(obj->type == ekOBJ_TABLEVIEW);
    tableview_update((TableView *)obj->component);
}

/*---------------------------------------------------------------------------*/

static void i_area_refresh(GtNapArea *area)
{
    HB_ULONG ulCurRec;

    cassert_no_null(area);

    /* Database current RECNO() */
    SELF_RECNO(area->area, &ulCurRec);

    /* Clear the current record index */
    arrst_clear(area->records, NULL, uint32_t);

    /* Generate the record index for TableView */
    if (area->while_block == NULL)
    {
        HB_BOOL fEof;
        SELF_GOTOP(area->area);
        SELF_EOF(area->area, &fEof);
        while (fEof == HB_FALSE)
        {
            HB_ULONG uiRecNo = 0;
            SELF_RECNO(area->area, &uiRecNo);
            arrst_append(area->records, (uint32_t)uiRecNo, uint32_t);
            SELF_SKIP(area->area, 1);
            SELF_EOF(area->area, &fEof);
        }
    }
    else
    {
        HB_BOOL fEof;
        SELF_GOTOP(area->area);
        SELF_EOF(area->area, &fEof);
        while (fEof == HB_FALSE)
        {
            HB_ULONG uiRecNo = 0;
            SELF_RECNO(area->area, &uiRecNo);

            {
                PHB_ITEM ritem = hb_itemDo(area->while_block, 0);
                HB_TYPE type = HB_ITEM_TYPE(ritem);
                bool_t add = FALSE;
                cassert_unref(type == HB_IT_LOGICAL, type);
                add = (bool_t)hb_itemGetL(ritem);
                hb_itemRelease(ritem);

                if (add == TRUE)
                    arrst_append(area->records, (uint32_t)uiRecNo, uint32_t);
            }

            SELF_SKIP(area->area, 1);
            SELF_EOF(area->area, &fEof);
        }
    }

    /* Restore database RECNO() */
    SELF_GOTO(area->area, ulCurRec);
}

/*---------------------------------------------------------------------------*/

static void i_area_select_row(GtNapArea *gtarea)
{
    HB_ULONG ulCurRec;
    uint32_t sel_row;
    TableView *view;

    cassert_no_null(gtarea);
    cassert_no_null(gtarea->gtobj);
    cassert(gtarea->gtobj->type == ekOBJ_TABLEVIEW);
    view = (TableView *)gtarea->gtobj->component;

    /* Current selected */
    SELF_RECNO(gtarea->area, &ulCurRec);

    sel_row = i_row_from_recno(gtarea, (uint32_t)ulCurRec);

    /* In multisel table, the selected rows comes from  VN_Selecio */
    if (gtarea->gtobj->multisel == TRUE)
    {
        if (tableview_get_focus_row(view) == UINT32_MAX)
        {
            /* We use RECNO for focused row */
            if (sel_row != UINT32_MAX)
            {
                tableview_focus_row(view, sel_row, ekTOP);
            }
            else
            {
                uint32_t nrecs = arrst_size(gtarea->records, uint32_t);
                sel_row = tableview_get_focus_row(view);
                /* We move recno to current focused row */
                if (sel_row >= nrecs)
                {
                    sel_row = 0;
                }

                if (sel_row < nrecs)
                {
                    uint32_t recno = *arrst_get_const(gtarea->records, sel_row, uint32_t);
                    tableview_select(view, &sel_row, 1);
                    SELF_GOTO(gtarea->area, recno);
                }
            }
        }
    }
    else
    {
        tableview_deselect_all(view);

        if (sel_row != UINT32_MAX)
        {
            tableview_select(view, &sel_row, 1);
            tableview_focus_row(view, sel_row, ekTOP);
        }
        /* RECNO() doesn't exists in view (perhaps is deleted) */
        else
        {
            uint32_t nrecs = arrst_size(gtarea->records, uint32_t);
            sel_row = tableview_get_focus_row(view);
            /* We move recno to current focused row */
            if (sel_row < nrecs)
            {
                uint32_t recno = *arrst_get_const(gtarea->records, sel_row, uint32_t);
                tableview_select(view, &sel_row, 1);
                SELF_GOTO(gtarea->area, recno);
            }
        }
    }
}

/*---------------------------------------------------------------------------*/

void hb_gtnap_tableview_refresh_all(const uint32_t wid, const uint32_t id)
{
    GtNapObject *obj = i_gtobj(GTNAP_GLOBAL, wid, id);
    GtNapWindow *gtwin = NULL;
    TableView *view = NULL;
    cassert_no_null(obj);
    cassert(obj->type == ekOBJ_TABLEVIEW);
    gtwin = obj->gtwin;
    view = (TableView *)obj->component;
    cassert_no_null(gtwin);
    if (gtwin->gtarea != NULL && view != NULL)
    {
        i_area_refresh(gtwin->gtarea);
        tableview_update(view);
        i_area_select_row(gtwin->gtarea);
    }
    else
    {
        tableview_update(view);
    }
}

/*---------------------------------------------------------------------------*/

void hb_gtnap_toolbar(const uint32_t wid, const uint32_t image_pixels)
{
    GtNapWindow *gtwin = i_gtwin(GTNAP_GLOBAL, wid);
    cassert_no_null(gtwin);
    cassert(gtwin->toolbar == NULL);
    gtwin->toolbar = heap_new0(GtNapToolbar);
    gtwin->toolbar->items = arrpt_create(GuiComponent);
    gtwin->toolbar->button_widthf = (real32_t)image_pixels;
    gtwin->toolbar->heightf = GTNAP_GLOBAL->cell_y_sizef * 2.f;
}

/*---------------------------------------------------------------------------*/

void hb_gtnap_toolbar_button(const uint32_t wid, const char_t *pathname, const char_t *tooltip, HB_ITEM *click_block)
{
    Image *image = NULL;
    char_t utf8[STATIC_TEXT_SIZE];
    i_cp_to_utf8(pathname, utf8, sizeof(utf8));
    image = image_from_file(utf8, NULL);

    if (image != NULL)
    {
        GtNapWindow *gtwin = i_gtwin(GTNAP_GLOBAL, wid);
        Button *button = button_flat();
        Listener *listener = i_gtnap_listener(click_block, INT32_MAX, UINT32_MAX, gtwin, i_OnButtonClick);
        uint32_t image_size = UINT32_MAX;
        cassert_no_null(gtwin);
        cassert_no_null(gtwin->toolbar);
        image_size = (uint32_t)gtwin->toolbar->button_widthf;
        if (image_width(image) != image_size || image_height(image) != image_size)
        {
            Image *scaled = image_scale(image, image_size, image_size);
            image_destroy(&image);
            image = scaled;
        }

        button_image(button, image);
        button_OnClick(button, listener);
        i_cp_to_utf8(tooltip, utf8, sizeof(utf8));
        button_tooltip(button, utf8);
        arrpt_append(gtwin->toolbar->items, (GuiComponent *)button, GuiComponent);
        image_destroy(&image);
    }
}

/*---------------------------------------------------------------------------*/

void hb_gtnap_toolbar_separator(const uint32_t wid)
{
    GtNapWindow *gtwin = i_gtwin(GTNAP_GLOBAL, wid);
    View *separator = _view_create(ekVIEW_BORDER | ekVIEW_CONTROL);
    cassert_no_null(gtwin);
    cassert_no_null(gtwin->toolbar);
    arrpt_append(gtwin->toolbar->items, (GuiComponent *)separator, GuiComponent);
}

/*---------------------------------------------------------------------------*/

void hb_gtnap_cualib_init_log(void)
{
    osbs_start();
}

/*---------------------------------------------------------------------------*/

void hb_gtnap_cualib_default_button(const uint32_t nDefault)
{
    GtNapWindow *gtwin = i_current_gtwin(GTNAP_GLOBAL);
    cassert(gtwin->default_button == UINT32_MAX);
    cassert(nDefault > 0);
    gtwin->default_button = nDefault - 1;
}

/*---------------------------------------------------------------------------*/

void hb_gtnap_cualib_window_f4_lista(void)
{
    GtNapWindow *gtwin = i_current_gtwin(GTNAP_GLOBAL);
    window_hotkey(gtwin->window, ekKEY_F4, 0, listener(gtwin, i_OnKeyWizard, GtNapWindow));
}

/*---------------------------------------------------------------------------*/

uint32_t hb_gtnap_cualib_window_current_edit(void)
{
    uint32_t id = 0;
    GtNapWindow *gtwin = i_current_main_gtwin(GTNAP_GLOBAL);
    arrpt_foreach(obj, gtwin->objects, GtNapObject)
        if (obj->type == ekOBJ_EDIT)
        {
            if (obj->has_focus == TRUE)
                return id;
            id += 1;
        }
    arrpt_end();
    return UINT32_MAX;
}

/*---------------------------------------------------------------------------*/

String *hb_block_to_utf8(HB_ITEM *item)
{
    String *str = NULL;

    if (HB_ITEM_TYPE(item) == HB_IT_STRING)
    {
        str = i_item_to_utf8_string(item);
    }
    else if (HB_ITEM_TYPE(item) == HB_IT_BLOCK)
    {
        PHB_ITEM ritem = hb_itemDo(item, 0);
        cassert(HB_ITEM_TYPE(item) == HB_IT_BLOCK);
        str = i_item_to_utf8_string(ritem);
        hb_itemRelease(ritem);
    }
    else
    {
        cassert_msg(FALSE, "Unknown block type");
        str = str_c("");
    }

    return str;
}

/*---------------------------------------------------------------------------*/

static int s_GtId;
static HB_GT_FUNCS SuperTable;
#define HB_GTSUPER (&SuperTable)
#define HB_GTID_PTR (&s_GtId)

/*---------------------------------------------------------------------------*/

static HB_BOOL hb_gtnap_Lock(PHB_GT pGT)
{
    HB_SYMBOL_UNUSED(pGT);
    if (i_LOG_HBFUNCS == TRUE)
        log_printf("hb_gtnap_Lock");
    return TRUE;
}

/*---------------------------------------------------------------------------*/

static void hb_gtnap_Unlock(PHB_GT pGT)
{
    HB_SYMBOL_UNUSED(pGT);
    if (i_LOG_HBFUNCS == TRUE)
        log_printf("hb_gtnap_Unlock");
}

/*---------------------------------------------------------------------------*/

static void hb_gtnap_Init(PHB_GT pGT, HB_FHANDLE hFilenoStdin, HB_FHANDLE hFilenoStdout, HB_FHANDLE hFilenoStderr)
{
    HB_SYMBOL_UNUSED(pGT);
    HB_SYMBOL_UNUSED(hFilenoStdin);
    HB_SYMBOL_UNUSED(hFilenoStdout);
    HB_SYMBOL_UNUSED(hFilenoStderr);
    if (i_LOG_HBFUNCS == TRUE)
        log_printf("hb_gtnap_Init");

    /*
   //HB_GTSUPER_INIT( pGT, hFilenoStdin, hFilenoStdout, hFilenoStderr );
   //HB_GTSELF_RESIZE( pGT, 35, 110);
   //HB_GTSELF_SETFLAG( pGT, HB_GTI_REDRAWMAX, 1 );
   //HB_GTSELF_SEMICOLD( pGT );
   */
}

/*---------------------------------------------------------------------------*/

static void hb_gtnap_Exit(PHB_GT pGT)
{
    HB_SYMBOL_UNUSED(pGT);
    if (i_LOG_HBFUNCS == TRUE)
        log_printf("hb_gtnap_Exit");
}

/*---------------------------------------------------------------------------*/

static void *hb_gtnap_New(PHB_GT pGT)
{
    HB_SYMBOL_UNUSED(pGT);
    if (i_LOG_HBFUNCS == TRUE)
        log_printf("hb_gtnap_New");
    return HB_GTSUPER_NEW(pGT);
}

/*---------------------------------------------------------------------------*/

static void hb_gtnap_Free(PHB_GT pGT)
{
    HB_SYMBOL_UNUSED(pGT);
    if (i_LOG_HBFUNCS == TRUE)
        log_printf("hb_gtnap_Free");
    HB_GTSUPER_FREE(pGT);
}

/*---------------------------------------------------------------------------*/

static void hb_gtnap_Mark(PHB_GT pGT)
{
    HB_SYMBOL_UNUSED(pGT);
    if (i_LOG_HBFUNCS == TRUE)
        log_printf("hb_gtnap_Mark");
    HB_GTSUPER_MARK(pGT);
}

/*---------------------------------------------------------------------------*/

static HB_BOOL hb_gtnap_Resize(PHB_GT pGT, int iRow, int iCol)
{
    HB_SYMBOL_UNUSED(pGT);
    if (i_LOG_HBFUNCS == TRUE)
        log_printf("hb_gtnap_Resize: %d, %d", iRow, iCol);
    return TRUE;
}

/*---------------------------------------------------------------------------*/

static HB_BOOL hb_gtnap_SetMode(PHB_GT pGT, int iRow, int iCol)
{
    HB_SYMBOL_UNUSED(pGT);
    if (i_LOG_HBFUNCS == TRUE)
        log_printf("hb_gtnap_SetMode: %d, %d", iRow, iCol);
    return TRUE;
}

/*---------------------------------------------------------------------------*/

static void hb_gtnap_GetSize(PHB_GT pGT, int *piRows, int *piCols)
{
    HB_SYMBOL_UNUSED(pGT);
    if (GTNAP_GLOBAL != NULL)
    {
        *piRows = (int)GTNAP_GLOBAL->rows;
        *piCols = (int)GTNAP_GLOBAL->cols;
    }
    else
    {
        *piRows = 0;
        *piCols = 0;
    }

    if (i_LOG_HBFUNCS == TRUE)
        log_printf("hb_gtnap_GetSize: %d, %d", *piRows, *piCols);
}

/*---------------------------------------------------------------------------*/

static void hb_gtnap_SemiCold(PHB_GT pGT)
{
    HB_SYMBOL_UNUSED(pGT);
    if (i_LOG_HBFUNCS == TRUE)
        log_printf("hb_gtnap_SemiCold");
}

/*---------------------------------------------------------------------------*/

static void hb_gtnap_ColdArea(PHB_GT pGT, int iTop, int iLeft, int iBottom, int iRight)
{
    HB_SYMBOL_UNUSED(pGT);
    if (i_LOG_HBFUNCS == TRUE)
        log_printf("hb_gtnap_ColdArea %d, %d, %d, %d", iTop, iLeft, iBottom, iRight);
}

/*---------------------------------------------------------------------------*/

static void hb_gtnap_ExposeArea(PHB_GT pGT, int iTop, int iLeft, int iBottom, int iRight)
{
    HB_SYMBOL_UNUSED(pGT);
    if (i_LOG_HBFUNCS == TRUE)
        log_printf("hb_gtnap_ExposeArea %d, %d, %d, %d", iTop, iLeft, iBottom, iRight);
}

/*---------------------------------------------------------------------------*/

static void hb_gtnap_ScrollArea(PHB_GT pGT, int iTop, int iLeft, int iBottom, int iRight, int iColor, HB_USHORT usChar, int iRows, int iCols)
{
    HB_SYMBOL_UNUSED(pGT);
    if (i_LOG_HBFUNCS == TRUE)
        log_printf("hb_gtnap_ScrollArea %d, %d, %d, %d, %d, %d, %d, %d", iTop, iLeft, iBottom, iRight, iColor, usChar, iRows, iCols);
}

/*---------------------------------------------------------------------------*/

static void hb_gtnap_TouchLine(PHB_GT pGT, int iRow)
{
    HB_SYMBOL_UNUSED(pGT);
    if (i_LOG_HBFUNCS == TRUE)
        log_printf("hb_gtnap_TouchLine %d", iRow);
}

/*---------------------------------------------------------------------------*/

static void hb_gtnap_TouchCell(PHB_GT pGT, int iRow, int iCol)
{
    HB_SYMBOL_UNUSED(pGT);
    if (i_LOG_HBFUNCS == TRUE)
        log_printf("hb_gtnap_TouchCell %d %d", iRow, iCol);
}

/*---------------------------------------------------------------------------*/

static void hb_gtnap_Redraw(PHB_GT pGT, int iRow, int iCol, int iSize)
{
    HB_SYMBOL_UNUSED(pGT);
    if (i_LOG_HBFUNCS == TRUE)
        log_printf("hb_gtnap_Redraw %d %d %d", iRow, iCol, iSize);
}

/*---------------------------------------------------------------------------*/

static void hb_gtnap_RedrawDiff(PHB_GT pGT)
{
    HB_SYMBOL_UNUSED(pGT);
    if (i_LOG_HBFUNCS == TRUE)
        log_printf("hb_gtnap_RedrawDiff");
}

/*---------------------------------------------------------------------------*/

static void hb_gtnap_Refresh(PHB_GT pGT)
{
    HB_SYMBOL_UNUSED(pGT);
    if (i_LOG_HBFUNCS == TRUE)
        log_printf("hb_gtnap_Refresh");
}

/*---------------------------------------------------------------------------*/

static void hb_gtnap_Flush(PHB_GT pGT)
{
    HB_SYMBOL_UNUSED(pGT);
    if (i_LOG_HBFUNCS == TRUE)
        log_printf("hb_gtnap_Flush");
}

/*---------------------------------------------------------------------------*/

static int hb_gtnap_MaxCol(PHB_GT pGT)
{
    int maxcol = 0;
    HB_SYMBOL_UNUSED(pGT);
    if (GTNAP_GLOBAL != NULL)
        maxcol = GTNAP_GLOBAL->cols - 1;

    if (i_LOG_HBFUNCS == TRUE)
        log_printf("hb_gtnap_MaxCol %d", maxcol);

    return maxcol;
}

/*---------------------------------------------------------------------------*/

static int hb_gtnap_MaxRow(PHB_GT pGT)
{
    int maxrow = 0;
    HB_SYMBOL_UNUSED(pGT);
    if (GTNAP_GLOBAL != NULL)
        maxrow = GTNAP_GLOBAL->rows - 1;

    if (i_LOG_HBFUNCS == TRUE)
        log_printf("hb_gtnap_MaxRow %d", maxrow);

    return maxrow;
}

/*---------------------------------------------------------------------------*/

static HB_BOOL hb_gtnap_CheckPos(PHB_GT pGT, int iRow, int iCol, long *plIndex)
{
    HB_SYMBOL_UNUSED(pGT);
    *plIndex = 0;
    if (i_LOG_HBFUNCS == TRUE)
        log_printf("hb_gtnap_CheckPos %d %d %d", iRow, iCol, (int)*plIndex);
    return TRUE;
}

/*---------------------------------------------------------------------------*/

static void hb_gtnap_SetPos(PHB_GT pGT, int iRow, int iCol)
{
    HB_SYMBOL_UNUSED(pGT);
    if (i_LOG_HBFUNCS == TRUE)
        log_printf("hb_gtnap_SetPos %d %d", iRow, iCol);

    if (GTNAP_GLOBAL != NULL)
    {
        if (GTNAP_GLOBAL->debugger != NULL)
        {
            nap_debugger_set_pos(GTNAP_GLOBAL->debugger, (uint32_t)iRow, (uint32_t)iCol);
        }
        else
        {
            GtNapWindow *gtwin = i_current_gtwin(GTNAP_GLOBAL);
            if (gtwin != NULL)
            {
                gtwin->cursor_row = (int32_t)iRow;
                gtwin->cursor_col = (int32_t)iCol;
            }
        }
    }
}

/*---------------------------------------------------------------------------*/

static void hb_gtnap_GetPos(PHB_GT pGT, int *piRow, int *piCol)
{
    HB_SYMBOL_UNUSED(pGT);
    if (GTNAP_GLOBAL != NULL)
    {
        if (GTNAP_GLOBAL->debugger != NULL)
        {
            uint32_t row, col;
            nap_debugger_get_pos(GTNAP_GLOBAL->debugger, &row, &col);
            *piRow = (int)row;
            *piCol = (int)col;
        }
        else
        {
            GtNapWindow *gtwin = i_current_gtwin(GTNAP_GLOBAL);
            if (gtwin != NULL)
            {
                *piRow = (int)gtwin->cursor_row;
                *piCol = (int)gtwin->cursor_col;
            }
            else
            {
                *piRow = 0;
                *piCol = 0;
            }
        }
    }

    if (i_LOG_HBFUNCS == TRUE)
        log_printf("hb_gtnap_GetPos %d %d", *piRow, *piCol);
}

/*---------------------------------------------------------------------------*/

static HB_BOOL hb_gtnap_IsColor(PHB_GT pGT)
{
    HB_SYMBOL_UNUSED(pGT);
    if (i_LOG_HBFUNCS == TRUE)
        log_printf("hb_gtnap_IsColor");
    return TRUE;
}

/*---------------------------------------------------------------------------*/

static void hb_gtnap_GetColorStr(PHB_GT pGT, char *pszColorString)
{
    HB_SYMBOL_UNUSED(pGT);
    HB_SYMBOL_UNUSED(pszColorString);
    if (i_LOG_HBFUNCS == TRUE)
        log_printf("hb_gtnap_GetColorStr");
}

/*---------------------------------------------------------------------------*/

static void hb_gtnap_SetColorStr(PHB_GT pGT, const char *szColorString)
{
    HB_SYMBOL_UNUSED(pGT);
    if (i_LOG_HBFUNCS == TRUE)
        log_printf("hb_gtnap_SetColorStr %s", szColorString);
}

/*---------------------------------------------------------------------------*/

static void hb_gtnap_ColorSelect(PHB_GT pGT, int iColorIndex)
{
    HB_SYMBOL_UNUSED(pGT);
    if (i_LOG_HBFUNCS == TRUE)
        log_printf("hb_gtnap_ColorSelect %d", iColorIndex);
}

/*---------------------------------------------------------------------------*/

static int hb_gtnap_GetColor(PHB_GT pGT)
{
    HB_SYMBOL_UNUSED(pGT);
    if (i_LOG_HBFUNCS == TRUE)
        log_printf("hb_gtnap_GetColor");
    return 0;
}

/*---------------------------------------------------------------------------*/

static int hb_gtnap_ColorNum(PHB_GT pGT, const char *szColorString)
{
    HB_SYMBOL_UNUSED(pGT);
    if (i_LOG_HBFUNCS == TRUE)
        log_printf("hb_gtnap_ColorNum %s", szColorString);
    return 0;
}

/*---------------------------------------------------------------------------*/

static void hb_gtnap_ColorsToString(PHB_GT pGT, int *pColors, int iColorCount, char *pszColorString, int iBufSize)
{
    HB_SYMBOL_UNUSED(pGT);
    HB_SYMBOL_UNUSED(pColors);
    HB_SYMBOL_UNUSED(iColorCount);
    HB_SYMBOL_UNUSED(pszColorString);
    HB_SYMBOL_UNUSED(iBufSize);
    if (i_LOG_HBFUNCS == TRUE)
        log_printf("hb_gtnap_ColorsToString");
}

/*---------------------------------------------------------------------------*/

static void hb_gtnap_StringToColors(PHB_GT pGT, const char *szColorString, int **pColorsPtr, int *piColorCount)
{
    HB_SYMBOL_UNUSED(pGT);
    HB_SYMBOL_UNUSED(szColorString);
    HB_SYMBOL_UNUSED(pColorsPtr);
    HB_SYMBOL_UNUSED(piColorCount);
    if (i_LOG_HBFUNCS == TRUE)
        log_printf("hb_gtnap_StringToColors");
}

/*---------------------------------------------------------------------------*/

static void hb_gtnap_GetColorData(PHB_GT pGT, int **pColorsPtr, int *piColorCount, int *piColorIndex)
{
    HB_SYMBOL_UNUSED(pGT);
    HB_SYMBOL_UNUSED(pColorsPtr);
    HB_SYMBOL_UNUSED(piColorCount);
    HB_SYMBOL_UNUSED(piColorIndex);
    if (i_LOG_HBFUNCS == TRUE)
        log_printf("hb_gtnap_GetColorData");
}

/*---------------------------------------------------------------------------*/

static int hb_gtnap_GetClearColor(PHB_GT pGT)
{
    HB_SYMBOL_UNUSED(pGT);
    if (i_LOG_HBFUNCS == TRUE)
        log_printf("hb_gtnap_GetClearColor");
    return 0;
}

/*---------------------------------------------------------------------------*/

static void hb_gtnap_SetClearColor(PHB_GT pGT, int iColor)
{
    HB_SYMBOL_UNUSED(pGT);
    if (i_LOG_HBFUNCS == TRUE)
        log_printf("hb_gtnap_SetClearColor %d", iColor);
}

/*---------------------------------------------------------------------------*/

static HB_USHORT hb_gtnap_GetClearChar(PHB_GT pGT)
{
    HB_SYMBOL_UNUSED(pGT);
    if (i_LOG_HBFUNCS == TRUE)
        log_printf("hb_gtnap_GetClearChar");
    return 0;
}

/*---------------------------------------------------------------------------*/

static void hb_gtnap_SetClearChar(PHB_GT pGT, HB_USHORT usChar)
{
    HB_SYMBOL_UNUSED(pGT);
    if (i_LOG_HBFUNCS == TRUE)
        log_printf("hb_gtnap_SetClearChar %d", usChar);
}

/*---------------------------------------------------------------------------*/

static int hb_gtnap_GetCursorStyle(PHB_GT pGT)
{
    HB_SYMBOL_UNUSED(pGT);
    if (i_LOG_HBFUNCS == TRUE)
        log_printf("hb_gtnap_GetCursorStyle");
    return SC_NONE;
}

/*---------------------------------------------------------------------------*/

static void hb_gtnap_SetCursorStyle(PHB_GT pGT, int iStyle)
{
    HB_SYMBOL_UNUSED(pGT);
    if (i_LOG_HBFUNCS == TRUE)
        log_printf("hb_gtnap_SetCursorStyle %d", iStyle);

    if (GTNAP_GLOBAL != NULL && GTNAP_GLOBAL->debugger != NULL)
        nap_debugger_cursor(GTNAP_GLOBAL->debugger, (uint32_t)iStyle);
}

/*---------------------------------------------------------------------------*/

static void hb_gtnap_GetScrCursor(PHB_GT pGT, int *piRow, int *piCol, int *piStyle)
{
    HB_SYMBOL_UNUSED(pGT);
    HB_SYMBOL_UNUSED(piRow);
    HB_SYMBOL_UNUSED(piCol);
    HB_SYMBOL_UNUSED(piStyle);
    if (i_LOG_HBFUNCS == TRUE)
        log_printf("hb_gtnap_GetScrCursor");
}

/*---------------------------------------------------------------------------*/

static HB_BOOL hb_gtnap_GetScrChar(PHB_GT pGT, int iRow, int iCol, int *piColor, HB_BYTE *pbAttr, HB_USHORT *pusChar)
{
    HB_SYMBOL_UNUSED(pGT);
    HB_SYMBOL_UNUSED(piColor);
    HB_SYMBOL_UNUSED(pbAttr);
    HB_SYMBOL_UNUSED(pusChar);
    if (i_LOG_HBFUNCS == TRUE)
        log_printf("hb_gtnap_GetScrChar %d, %d", iRow, iCol);
    return HB_TRUE;
}

/*---------------------------------------------------------------------------*/

static HB_BOOL hb_gtnap_PutScrChar(PHB_GT pGT, int iRow, int iCol, int iColor, HB_BYTE bAttr, HB_USHORT usChar)
{
    HB_SYMBOL_UNUSED(pGT);
    if (i_LOG_HBFUNCS == TRUE)
        log_printf("hb_gtnap_PutScrChar %d, %d, %d, %d, %d", iRow, iCol, iColor, bAttr, usChar);
    return HB_TRUE;
}

/*---------------------------------------------------------------------------*/

static HB_BOOL hb_gtnap_GetScrUC(PHB_GT pGT, int iRow, int iCol, int *piColor, HB_BYTE *pbAttr, HB_UCHAR *puChar, HB_BOOL fTerm)
{
    HB_SYMBOL_UNUSED(pGT);
    HB_SYMBOL_UNUSED(piColor);
    HB_SYMBOL_UNUSED(pbAttr);
    HB_SYMBOL_UNUSED(puChar);
    HB_SYMBOL_UNUSED(fTerm);
    if (i_LOG_HBFUNCS == TRUE)
        log_printf("hb_gtnap_GetScrUC %d, %d", iRow, iCol);
    return HB_TRUE;
}

/*---------------------------------------------------------------------------*/

static void hb_gtnap_DispBegin(PHB_GT pGT)
{
    HB_SYMBOL_UNUSED(pGT);
    if (i_LOG_HBFUNCS == TRUE)
        log_printf("hb_gtnap_DispBegin");
}

/*---------------------------------------------------------------------------*/

static void hb_gtnap_DispEnd(PHB_GT pGT)
{
    HB_SYMBOL_UNUSED(pGT);
    if (i_LOG_HBFUNCS == TRUE)
        log_printf("hb_gtnap_DispEnd");
}

/*---------------------------------------------------------------------------*/

static int hb_gtnap_DispCount(PHB_GT pGT)
{
    HB_SYMBOL_UNUSED(pGT);
    if (i_LOG_HBFUNCS == TRUE)
        log_printf("hb_gtnap_DispCount");
    return 0;
}

/*---------------------------------------------------------------------------*/

static HB_BOOL hb_gtnap_GetChar(PHB_GT pGT, int iRow, int iCol, int *pbColor, HB_BYTE *pbAttr, HB_USHORT *pusChar)
{
    HB_SYMBOL_UNUSED(pGT);
    HB_SYMBOL_UNUSED(iCol);
    HB_SYMBOL_UNUSED(iRow);
    *pbColor = 0;
    *pbAttr = 0;
    *pusChar = 65;
    if (i_LOG_HBFUNCS == TRUE)
        log_printf("hb_gtnap_GetChar");
    return TRUE;
}

/*---------------------------------------------------------------------------*/

static uint32_t i_hb_codepoint(HB_USHORT usChar)
{
    return (uint32_t)usChar;
}

/*---------------------------------------------------------------------------*/

static HB_BOOL hb_gtnap_PutChar(PHB_GT pGT, int iRow, int iCol, int bColor, HB_BYTE bAttr, HB_USHORT usChar)
{
    HB_SYMBOL_UNUSED(pGT);
    if (GTNAP_GLOBAL != NULL && GTNAP_GLOBAL->debugger != NULL)
    {
        uint32_t codepoint = i_hb_codepoint(usChar);
        nap_debugger_putchar(GTNAP_GLOBAL->debugger, (uint32_t)iRow, (uint32_t)iCol, codepoint, (byte_t)bColor, (byte_t)bAttr);
    }

    if (i_LOG_HBFUNCS == TRUE)
        log_printf("hb_gtnap_PutChar %d %d %d %d %d", iRow, iCol, bColor, bAttr, usChar);

    return TRUE;
}

/*---------------------------------------------------------------------------*/

static long hb_gtnap_RectSize(PHB_GT pGT, int iTop, int iLeft, int iBottom, int iRight)
{
    HB_SYMBOL_UNUSED(pGT);
    if (i_LOG_HBFUNCS == TRUE)
        log_printf("hb_gtnap_RectSize %d %d %d %d", iTop, iLeft, iBottom, iRight);
    return 0;
}

/*---------------------------------------------------------------------------*/

static void hb_gtnap_Save(PHB_GT pGT, int iTop, int iLeft, int iBottom, int iRight, void *pBuffer)
{
    HB_SYMBOL_UNUSED(pGT);
    if (i_LOG_HBFUNCS == TRUE)
        log_printf("hb_gtnap_Save %d %d %d %d", iTop, iLeft, iBottom, iRight);

    if (GTNAP_GLOBAL != NULL && GTNAP_GLOBAL->debugger != NULL)
        nap_debugger_save(GTNAP_GLOBAL->debugger, (uint32_t)iTop, (uint32_t)iLeft, (uint32_t)iBottom, (uint32_t)iRight, (byte_t *)pBuffer);
}

/*---------------------------------------------------------------------------*/

static void hb_gtnap_Rest(PHB_GT pGT, int iTop, int iLeft, int iBottom, int iRight, const void *pBuffer)
{
    HB_SYMBOL_UNUSED(pGT);
    if (i_LOG_HBFUNCS == TRUE)
        log_printf("hb_gtnap_Rest %d %d %d %d", iTop, iLeft, iBottom, iRight);

    if (GTNAP_GLOBAL != NULL && GTNAP_GLOBAL->debugger != NULL)
        nap_debugger_rest(GTNAP_GLOBAL->debugger, (uint32_t)iTop, (uint32_t)iLeft, (uint32_t)iBottom, (uint32_t)iRight, (const byte_t *)pBuffer);
}

/*---------------------------------------------------------------------------*/

static int hb_gtnap_PutText(PHB_GT pGT, int iRow, int iCol, int bColor, const char *pText, HB_SIZE ulLen)
{
    char_t utf8[STATIC_TEXT_SIZE];
    HB_SYMBOL_UNUSED(pGT);
    HB_SYMBOL_UNUSED(iRow);
    HB_SYMBOL_UNUSED(iCol);
    HB_SYMBOL_UNUSED(bColor);
    HB_SYMBOL_UNUSED(pText);
    HB_SYMBOL_UNUSED(ulLen);
    i_cp_to_utf8(pText, utf8, sizeof32(utf8));

    if (GTNAP_GLOBAL != NULL && GTNAP_GLOBAL->debugger != NULL)
        nap_debugger_puttext(GTNAP_GLOBAL->debugger, (uint32_t)iRow, (uint32_t)iCol, (byte_t)bColor, utf8);

    if (i_LOG_HBFUNCS == TRUE)
        log_printf("hb_gtnap_PutText %d %d", iRow, iCol);

    return 0;
}

/*---------------------------------------------------------------------------*/

static int hb_gtnap_PutTextW(PHB_GT pGT, int iRow, int iCol, int bColor, const HB_WCHAR *pText, HB_SIZE ulLen)
{
    HB_SYMBOL_UNUSED(pGT);
    HB_SYMBOL_UNUSED(bColor);
    HB_SYMBOL_UNUSED(pText);
    HB_SYMBOL_UNUSED(ulLen);
    if (i_LOG_HBFUNCS == TRUE)
        log_printf("hb_gtnap_PutTextW %d %d", iRow, iCol);
    return 0;
}

/*---------------------------------------------------------------------------*/

static void hb_gtnap_Replicate(PHB_GT pGT, int iRow, int iCol, int bColor, HB_BYTE bAttr, HB_USHORT usChar, HB_SIZE ulLen)
{
    HB_SYMBOL_UNUSED(pGT);
    HB_SYMBOL_UNUSED(bColor);
    HB_SYMBOL_UNUSED(bAttr);
    HB_SYMBOL_UNUSED(usChar);
    HB_SYMBOL_UNUSED(ulLen);
    if (i_LOG_HBFUNCS == TRUE)
        log_printf("hb_gtnap_Replicate %d %d", iRow, iCol);
}

/*---------------------------------------------------------------------------*/

static void hb_gtnap_WriteAt(PHB_GT pGT, int iRow, int iCol, const char *pText, HB_SIZE ulLength)
{
    HB_SYMBOL_UNUSED(pGT);
    HB_SYMBOL_UNUSED(ulLength);

    if (i_LOG_HBFUNCS == TRUE)
        log_printf("hb_gtnap_WriteAt %d %d", iRow, iCol);

    if (GTNAP_GLOBAL != NULL)
    {
        GtNapWindow *gtwin = i_current_gtwin(GTNAP_GLOBAL);
        if (gtwin != NULL)
        {
            i_add_label(iRow - gtwin->top, iCol - gtwin->left, FALSE, gtwin, GTNAP_GLOBAL);
            if (pText != NULL)
            {
                GtNapObject *obj = arrpt_last(gtwin->objects, GtNapObject);
                char_t utf8[STATIC_TEXT_SIZE];
                i_cp_to_utf8(pText, utf8, sizeof32(utf8));
                i_set_label_text(obj, utf8);
            }
        }
    }
}

/*---------------------------------------------------------------------------*/

static void hb_gtnap_WriteAtW(PHB_GT pGT, int iRow, int iCol, const HB_WCHAR *szText, HB_SIZE nLength)
{
    HB_SYMBOL_UNUSED(pGT);
    HB_SYMBOL_UNUSED(szText);
    HB_SYMBOL_UNUSED(nLength);
    if (i_LOG_HBFUNCS == TRUE)
        log_printf("hb_gtnap_WriteAtW %d %d", iRow, iCol);
}

/*---------------------------------------------------------------------------*/

static void hb_gtnap_Write(PHB_GT pGT, const char *szText, HB_SIZE nLength)
{
    HB_SYMBOL_UNUSED(pGT);
    HB_SYMBOL_UNUSED(szText);
    HB_SYMBOL_UNUSED(nLength);
    if (i_LOG_HBFUNCS == TRUE)
        log_printf("hb_gtnap_Write");
}

/*---------------------------------------------------------------------------*/

static void hb_gtnap_WriteW(PHB_GT pGT, const HB_WCHAR *szText, HB_SIZE nLength)
{
    HB_SYMBOL_UNUSED(pGT);
    HB_SYMBOL_UNUSED(szText);
    HB_SYMBOL_UNUSED(nLength);
    if (i_LOG_HBFUNCS == TRUE)
        log_printf("hb_gtnap_WriteW");
}

/*---------------------------------------------------------------------------*/

static void hb_gtnap_WriteCon(PHB_GT pGT, const char *szText, HB_SIZE nLength)
{
    HB_SYMBOL_UNUSED(pGT);
    HB_SYMBOL_UNUSED(szText);
    HB_SYMBOL_UNUSED(nLength);
    if (i_LOG_HBFUNCS == TRUE)
        log_printf("hb_gtnap_WriteCon");
}

/*---------------------------------------------------------------------------*/

static void hb_gtnap_WriteConW(PHB_GT pGT, const HB_WCHAR *szText, HB_SIZE nLength)
{
    HB_SYMBOL_UNUSED(pGT);
    HB_SYMBOL_UNUSED(szText);
    HB_SYMBOL_UNUSED(nLength);
    if (i_LOG_HBFUNCS == TRUE)
        log_printf("hb_gtnap_WriteConW");
}

/*---------------------------------------------------------------------------*/

static void hb_gtnap_SetAttribute(PHB_GT pGT, int iTop, int iLeft, int iBottom, int iRight, int bColor)
{
    HB_SYMBOL_UNUSED(pGT);
    HB_SYMBOL_UNUSED(bColor);
    if (i_LOG_HBFUNCS == TRUE)
        log_printf("hb_gtnap_SetAttribute %d %d %d %d", iTop, iLeft, iBottom, iRight);
}

/*---------------------------------------------------------------------------*/

static void hb_gtnap_DrawShadow(PHB_GT pGT, int iTop, int iLeft, int iBottom, int iRight, int iColor)
{
    HB_SYMBOL_UNUSED(pGT);
    HB_SYMBOL_UNUSED(iColor);
    if (i_LOG_HBFUNCS == TRUE)
        log_printf("hb_gtnap_DrawShadow %d %d %d %d", iTop, iLeft, iBottom, iRight);
}

/*---------------------------------------------------------------------------*/

static void hb_gtnap_Scroll(PHB_GT pGT, int iTop, int iLeft, int iBottom, int iRight, int bColor, HB_USHORT bChar, int iRows, int iCols)
{
    HB_SYMBOL_UNUSED(pGT);
    if (i_LOG_HBFUNCS == TRUE)
        log_printf("hb_gtnap_Scroll %d %d %d %d", iTop, iLeft, iBottom, iRight);

    if (GTNAP_GLOBAL != NULL)
    {
        if (GTNAP_GLOBAL->debugger != NULL)
        {
            uint32_t codepoint = i_hb_codepoint(bChar);
            nap_debugger_scroll(GTNAP_GLOBAL->debugger, (uint32_t)iTop, (uint32_t)iLeft, (uint32_t)iBottom, (uint32_t)iRight, (uint32_t)iRows, (uint32_t)iCols, codepoint, (byte_t)bColor);
        }
        else
        {
            GtNapWindow *gtwin = i_current_gtwin(GTNAP_GLOBAL);

            if (gtwin != NULL)
            {
                uint32_t i, n;
                /*
                   FRAN: The scroll, at the moment, delete all texts
                   Improve taking into account the input rectangle
                   Take into account if a real scroll exists (iRows > 0 || iCols > 0)
                */
                n = arrpt_size(gtwin->objects, GtNapObject);
                for (i = 0; i < n;)
                {
                    GtNapObject *object = arrpt_get(gtwin->objects, i, GtNapObject);
                    const char_t *type = _component_type(object->component);
                    if (str_equ_c(type, "Label") == TRUE)
                    {
                        i_destroy_gtobject(gtwin, i);
                        n -= 1;
                    }
                    else
                    {
                        i += 1;
                    }
                }
            }
        }
    }
}

/*---------------------------------------------------------------------------*/

static void hb_gtnap_ScrollUp(PHB_GT pGT, int iRows, int iColor, HB_USHORT usChar)
{
    HB_SYMBOL_UNUSED(pGT);
    HB_SYMBOL_UNUSED(iColor);
    HB_SYMBOL_UNUSED(usChar);
    if (i_LOG_HBFUNCS == TRUE)
        log_printf("hb_gtnap_ScrollUp %d", iRows);
}

/*---------------------------------------------------------------------------*/

static void hb_gtnap_Box(PHB_GT pGT, int iTop, int iLeft, int iBottom, int iRight, const char *pbyFrame, int bColor)
{
    HB_SYMBOL_UNUSED(pGT);
    HB_SYMBOL_UNUSED(pbyFrame);
    if (i_LOG_HBFUNCS == TRUE)
        log_printf("hb_gtnap_Box %d %d %d %d (%s)", iTop, iLeft, iBottom, iRight, pbyFrame);

    if (GTNAP_GLOBAL != NULL && GTNAP_GLOBAL->debugger != NULL)
        nap_debugger_box(GTNAP_GLOBAL->debugger, (uint32_t)iTop, (uint32_t)iLeft, (uint32_t)iBottom, (uint32_t)iRight, (byte_t)bColor);
}

/*---------------------------------------------------------------------------*/

static void hb_gtnap_BoxW(PHB_GT pGT, int iTop, int iLeft, int iBottom, int iRight, const HB_WCHAR *szFrame, int iColor)
{
    HB_SYMBOL_UNUSED(pGT);
    HB_SYMBOL_UNUSED(szFrame);
    HB_SYMBOL_UNUSED(iColor);
    if (i_LOG_HBFUNCS == TRUE)
        log_printf("hb_gtnap_BoxW %d %d %d %d", iTop, iLeft, iBottom, iRight);
}

/*---------------------------------------------------------------------------*/

static void hb_gtnap_BoxD(PHB_GT pGT, int iTop, int iLeft, int iBottom, int iRight, const char *szFrame, int iColor)
{
    HB_SYMBOL_UNUSED(pGT);
    HB_SYMBOL_UNUSED(szFrame);
    HB_SYMBOL_UNUSED(iColor);
    if (i_LOG_HBFUNCS == TRUE)
        log_printf("hb_gtnap_BoxD %d %d %d %d", iTop, iLeft, iBottom, iRight);
}

/*---------------------------------------------------------------------------*/

static void hb_gtnap_BoxS(PHB_GT pGT, int iTop, int iLeft, int iBottom, int iRight, const char *szFrame, int iColor)
{
    HB_SYMBOL_UNUSED(pGT);
    HB_SYMBOL_UNUSED(szFrame);
    HB_SYMBOL_UNUSED(iColor);
    if (i_LOG_HBFUNCS == TRUE)
        log_printf("hb_gtnap_BoxS %d %d %d %d", iTop, iLeft, iBottom, iRight);
}

/*---------------------------------------------------------------------------*/

static void hb_gtnap_HorizLine(PHB_GT pGT, int iRow, int iLeft, int iRight, HB_USHORT bChar, int bColor)
{
    HB_SYMBOL_UNUSED(pGT);
    HB_SYMBOL_UNUSED(bChar);
    HB_SYMBOL_UNUSED(bColor);
    if (i_LOG_HBFUNCS == TRUE)
        log_printf("hb_gtnap_HorizLine %d %d %d", iRow, iLeft, iRight);
}

/*---------------------------------------------------------------------------*/

static void hb_gtnap_VertLine(PHB_GT pGT, int iCol, int iTop, int iBottom, HB_USHORT bChar, int bColor)
{
    HB_SYMBOL_UNUSED(pGT);
    HB_SYMBOL_UNUSED(bChar);
    HB_SYMBOL_UNUSED(bColor);
    if (i_LOG_HBFUNCS == TRUE)
        log_printf("hb_gtnap_VertLine %d %d %d", iCol, iTop, iBottom);
}

/*---------------------------------------------------------------------------*/

static HB_BOOL hb_gtnap_GetBlink(PHB_GT pGT)
{
    HB_SYMBOL_UNUSED(pGT);
    if (i_LOG_HBFUNCS == TRUE)
        log_printf("hb_gtnap_GetBlink");
    return TRUE;
}

/*---------------------------------------------------------------------------*/

static void hb_gtnap_SetBlink(PHB_GT pGT, HB_BOOL bBlink)
{
    HB_SYMBOL_UNUSED(pGT);
    if (i_LOG_HBFUNCS == TRUE)
        log_printf("hb_gtnap_SetBlink %d", bBlink);
}

/*---------------------------------------------------------------------------*/

static void hb_gtnap_SetSnowFlag(PHB_GT pGT, HB_BOOL fNoSnow)
{
    HB_SYMBOL_UNUSED(pGT);
    HB_SYMBOL_UNUSED(fNoSnow);
    if (i_LOG_HBFUNCS == TRUE)
        log_printf("hb_gtnap_SetSnowFlag");
}

/*---------------------------------------------------------------------------*/

static const char *hb_gtnap_Version(PHB_GT pGT, int iType)
{
    HB_SYMBOL_UNUSED(pGT);
    if (i_LOG_HBFUNCS == TRUE)
        log_printf("hb_gtnap_Version %d", iType);

    if (iType == 0)
        return HB_GT_DRVNAME(HB_GT_NAME);

    return "Harbour Terminal: GTNAP";
}

/*---------------------------------------------------------------------------*/

static HB_BOOL hb_gtnap_Suspend(PHB_GT pGT)
{
    HB_SYMBOL_UNUSED(pGT);
    if (i_LOG_HBFUNCS == TRUE)
        log_printf("hb_gtnap_Suspend");
    return HB_TRUE;
}

/*---------------------------------------------------------------------------*/

static HB_BOOL hb_gtnap_Resume(PHB_GT pGT)
{
    HB_SYMBOL_UNUSED(pGT);
    if (i_LOG_HBFUNCS == TRUE)
        log_printf("hb_gtnap_Resume");
    return HB_TRUE;
}

/*---------------------------------------------------------------------------*/

static HB_BOOL hb_gtnap_PreExt(PHB_GT pGT)
{
    HB_SYMBOL_UNUSED(pGT);
    if (i_LOG_HBFUNCS == TRUE)
        log_printf("hb_gtnap_PreExt");
    return HB_TRUE;
}

/*---------------------------------------------------------------------------*/

static HB_BOOL hb_gtnap_PostExt(PHB_GT pGT)
{
    HB_SYMBOL_UNUSED(pGT);
    if (i_LOG_HBFUNCS == TRUE)
        log_printf("hb_gtnap_PostExt");
    return HB_TRUE;
}

/*---------------------------------------------------------------------------*/

static void hb_gtnap_OutStd(PHB_GT pGT, const char *pbyStr, HB_SIZE ulLen)
{
    HB_SYMBOL_UNUSED(pGT);
    HB_SYMBOL_UNUSED(pbyStr);
    if (i_LOG_HBFUNCS == TRUE)
        log_printf("hb_gtnap_OutStd %d", (int)ulLen);
}

/*---------------------------------------------------------------------------*/

static void hb_gtnap_OutErr(PHB_GT pGT, const char *pbyStr, HB_SIZE ulLen)
{
    HB_SYMBOL_UNUSED(pGT);
    if (i_LOG_HBFUNCS == TRUE)
        log_printf("hb_gtnap_OutErr '%s' (%d)", pbyStr, (int)ulLen);

    if (pbyStr != NULL && ulLen > 0)
    {
        char_t utf8[STATIC_TEXT_SIZE];
        i_cp_to_utf8(pbyStr, utf8, sizeof32(utf8));
    }
}

/*---------------------------------------------------------------------------*/

static void hb_gtnap_Tone(PHB_GT pGT, double dFrequency, double dDuration)
{
    /* dDuration is in 'Ticks' (18.2 per second) */
    HB_SYMBOL_UNUSED(pGT);
    if (i_LOG_HBFUNCS == TRUE)
        log_printf("hb_gtnap_Tone %f %f", dFrequency, dDuration);
}

/*---------------------------------------------------------------------------*/

static void hb_gtnap_Bell(PHB_GT pGT)
{
    HB_SYMBOL_UNUSED(pGT);
    if (i_LOG_HBFUNCS == TRUE)
        log_printf("hb_gtnap_Bell");
}

/*---------------------------------------------------------------------------*/

static HB_BOOL hb_gtnap_Info(PHB_GT pGT, int iType, PHB_GT_INFO pInfo)
{
    HB_SYMBOL_UNUSED(pGT);
    if (i_LOG_HBFUNCS == TRUE)
        log_printf("hb_gtnap_Info %d", iType);

    switch (iType)
    {
    case HB_GTI_GETWIN:
        /* Its setting a new window */
        if (pInfo->pNewVal != NULL)
        {
            HB_TYPE type = HB_ITEM_TYPE(pInfo->pNewVal);
            if (type == HB_IT_ARRAY)
            {
                HB_SIZE n = hb_arrayLen(pInfo->pNewVal);
                if (n == 5)
                {
                    PHB_ITEM wname = hb_arrayGetItemPtr(pInfo->pNewVal, 1);
                    if (HB_ITEM_TYPE(wname) == HB_IT_STRING)
                    {
                        char_t utf8[STATIC_TEXT_SIZE];
                        hb_itemCopyStrUTF8(wname, utf8, sizeof(utf8));
                        if (str_equ_c(utf8, "Debugger") == TRUE)
                        {
                            PHB_ITEM wtop = hb_arrayGetItemPtr(pInfo->pNewVal, 2);
                            PHB_ITEM wleft = hb_arrayGetItemPtr(pInfo->pNewVal, 3);
                            PHB_ITEM wbottom = hb_arrayGetItemPtr(pInfo->pNewVal, 4);
                            PHB_ITEM wright = hb_arrayGetItemPtr(pInfo->pNewVal, 5);
                            uint32_t ncols = 0, nrows = 0;
                            cassert(HB_ITEM_TYPE(wtop) == HB_IT_INTEGER);
                            cassert(HB_ITEM_TYPE(wleft) == HB_IT_INTEGER);
                            cassert(HB_ITEM_TYPE(wbottom) == HB_IT_INTEGER);
                            cassert(HB_ITEM_TYPE(wright) == HB_IT_INTEGER);
                            ncols = hb_itemGetNI(wright) - hb_itemGetNI(wleft) + 1;
                            nrows = hb_itemGetNI(wbottom) - hb_itemGetNI(wtop) + 1;
                            cassert(GTNAP_GLOBAL->debugger == NULL);
                            unref(ncols);
                            unref(nrows);
                            GTNAP_GLOBAL->debugger = nap_debugger_create(tc(GTNAP_GLOBAL->debugger_path), GTNAP_GLOBAL->rows, GTNAP_GLOBAL->cols);
                            GTNAP_GLOBAL->debugger_visible = TRUE;
                            pInfo->pResult = hb_itemPutNI(pInfo->pResult, i_DEBUGGER_WINDOW_HASH);
                        }
                        else
                        {
                            /* TODO: Review this window class */
                            cassert(FALSE);
                        }
                    }
                    else
                    {
                        /* TODO: Review this window class */
                        cassert(FALSE);
                    }
                }
                else
                {
                    /* TODO: Review this window class */
                    cassert(FALSE);
                }
            }
        }
        /* Its asking for the current window */
        else
        {
            cassert(pInfo->pResult == NULL);
            if (GTNAP_GLOBAL->debugger != NULL && GTNAP_GLOBAL->debugger_visible == TRUE)
                pInfo->pResult = hb_itemPutNI(pInfo->pResult, i_DEBUGGER_WINDOW_HASH);
            else
                pInfo->pResult = hb_itemPutNI(pInfo->pResult, i_MAIN_WINDOW_HASH);
        }
        break;

    case HB_GTI_SETWIN:
        /* Recovering current window --> Destroy the debugger */
        if (pInfo->pNewVal != NULL)
        {
            HB_TYPE type = HB_ITEM_TYPE(pInfo->pNewVal);
            switch (type)
            {
            case HB_IT_INTEGER:
            case HB_IT_LONG:
            {
                uint32_t wid = hb_itemGetNI(pInfo->pNewVal);
                /* Close the debugger process */
                if (wid == i_MAIN_WINDOW_HASH)
                {
                    GtNapWindow *gtwin = i_current_gtwin(GTNAP_GLOBAL);
                    if (gtwin != NULL)
                        window_show(gtwin->window);

                    if (GTNAP_GLOBAL->debugger != NULL)
                    {
                        nap_debugger_show(GTNAP_GLOBAL->debugger, FALSE);
                        GTNAP_GLOBAL->debugger_visible = FALSE;
                    }
                }
                else if (wid == i_DEBUGGER_WINDOW_HASH)
                {
                    if (GTNAP_GLOBAL->debugger != NULL)
                    {
                        nap_debugger_show(GTNAP_GLOBAL->debugger, TRUE);
                        GTNAP_GLOBAL->debugger_visible = TRUE;
                    }
                }
                else
                {
                    /* TODO: Review this window ID */
                    cassert(FALSE);
                }

                break;
            }

                cassert_default();
            }
        }
        else
        {
            /* TODO: Review parameter value */
            cassert(FALSE);
        }
        break;

    case HB_GTI_COMPATBUFFER:
        pInfo->pResult = hb_itemPutL(pInfo->pResult, TRUE);
        break;

    case HB_GTI_BOXCP:
        pInfo->pResult = hb_itemPutC(pInfo->pResult,
                                     pGT->cdpBox ? pGT->cdpBox->id : NULL);
        if (hb_itemType(pInfo->pNewVal) & HB_IT_STRING)
        {
            if (hb_itemGetCLen(pInfo->pNewVal) > 0)
            {
                PHB_CODEPAGE cdpBox = hb_cdpFind(hb_itemGetCPtr(pInfo->pNewVal));
                if (cdpBox)
                    pGT->cdpBox = cdpBox;
            }
            else
                pGT->cdpBox = NULL;
        }
        break;

        cassert_default();
    }

    return TRUE;
}

/*---------------------------------------------------------------------------*/

static int hb_gtnap_Alert(PHB_GT pGT, PHB_ITEM message, PHB_ITEM options, int a, int b, double c)
{
    String *msg = NULL;
    ArrPt(String) *opts = NULL;
    uint32_t ret = 0;

    HB_SYMBOL_UNUSED(pGT);

    if (i_LOG_HBFUNCS == TRUE)
        log_printf("hb_gtnap_Alert %d %d %f", a, b, c);

    if (HB_ITEM_TYPE(message) == HB_IT_STRING)
    {
        char_t utf8[STATIC_TEXT_SIZE];
        hb_itemCopyStrUTF8(message, utf8, sizeof(utf8));
        msg = str_c(utf8);
    }
    else
    {
        msg = str_c("Unknown alert message");
    }

    opts = arrpt_create(String);
    if (HB_ITEM_TYPE(options) == HB_IT_ARRAY)
    {
        HB_SIZE i, n = hb_arrayLen(options);
        for (i = 0; i < n; ++i)
        {
            PHB_ITEM elem = hb_arrayGetItemPtr(options, i + 1);
            String *opt = NULL;
            if (HB_ITEM_TYPE(elem) == HB_IT_STRING)
            {
                char_t utf8[STATIC_TEXT_SIZE];
                hb_itemCopyStrUTF8(elem, utf8, sizeof(utf8));
                opt = str_c(utf8);
            }
            else
            {
                opt = str_c("Unknown opt");
            }

            arrpt_append(opts, opt, String);
        }
    }

    ret = gui_info_window(TRUE, tc(msg), "Harbour alert", "Please contact ASPEC technical support", "--", 0, opts, 0);
    str_destroy(&msg);
    arrpt_destroy(&opts, str_destroy, String);

    return ret + 1;
}

/*---------------------------------------------------------------------------*/

static int hb_gtnap_SetFlag(PHB_GT pGT, int iType, int iNewValue)
{
    HB_SYMBOL_UNUSED(pGT);
    HB_SYMBOL_UNUSED(iNewValue);
    if (i_LOG_HBFUNCS == TRUE)
        log_printf("hb_gtnap_SetFlag %d", iType);
    return 0;
}

/*---------------------------------------------------------------------------*/

static HB_BOOL hb_gtnap_SetDispCP(PHB_GT pGT, const char *pszTermCDP, const char *pszHostCDP, HB_BOOL fBox)
{
    HB_SYMBOL_UNUSED(pGT);
    HB_SYMBOL_UNUSED(pszTermCDP);
    HB_SYMBOL_UNUSED(pszHostCDP);
    HB_SYMBOL_UNUSED(fBox);
    if (i_LOG_HBFUNCS == TRUE)
        log_printf("hb_gtnap_SetDispCP");
    return HB_TRUE;
}

/*---------------------------------------------------------------------------*/

static HB_BOOL hb_gtnap_SetKeyCP(PHB_GT pGT, const char *pszTermCDP, const char *pszHostCDP)
{
    HB_SYMBOL_UNUSED(pGT);
    HB_SYMBOL_UNUSED(pszTermCDP);
    HB_SYMBOL_UNUSED(pszHostCDP);
    if (i_LOG_HBFUNCS == TRUE)
        log_printf("hb_gtnap_SetKeyCP");
    return HB_TRUE;
}

/*---------------------------------------------------------------------------*/

static int hb_gtnap_ReadKey(PHB_GT pGT, int iEventMask)
{
    int iKey = 0;
    HB_SYMBOL_UNUSED(pGT);

    if (i_LOG_HBFUNCS == TRUE)
        log_printf("hb_gtnap_ReadKey %d", iEventMask);

    if (GTNAP_GLOBAL != NULL && GTNAP_GLOBAL->debugger != NULL)
        iKey = (int)nap_debugger_read_key(GTNAP_GLOBAL->debugger);

    return iKey;
}

/*---------------------------------------------------------------------------*/

static int hb_gtnap_InkeyGet(PHB_GT pGT, HB_BOOL fWait, double dSeconds, int iEventMask)
{
    HB_SYMBOL_UNUSED(pGT);
    HB_SYMBOL_UNUSED(fWait);
    HB_SYMBOL_UNUSED(dSeconds);
    if (i_LOG_HBFUNCS == TRUE)
        log_printf("hb_gtnap_InkeyGet %d", iEventMask);
    return 0;
}

/*---------------------------------------------------------------------------*/

static void hb_gtnap_InkeyPut(PHB_GT pGT, int iKey)
{
    HB_SYMBOL_UNUSED(pGT);
    if (i_LOG_HBFUNCS == TRUE)
        log_printf("hb_gtnap_InkeyPut %d", iKey);
}

/*---------------------------------------------------------------------------*/

static void hb_gtnap_InkeyIns(PHB_GT pGT, int iKey)
{
    HB_SYMBOL_UNUSED(pGT);
    if (i_LOG_HBFUNCS == TRUE)
        log_printf("hb_gtnap_InkeyIns %d", iKey);
}

/*---------------------------------------------------------------------------*/

static int hb_gtnap_InkeyLast(PHB_GT pGT, int iEventMask)
{
    HB_SYMBOL_UNUSED(pGT);
    if (i_LOG_HBFUNCS == TRUE)
        log_printf("hb_gtnap_InkeyLast %d", iEventMask);
    return 0;
}

/*---------------------------------------------------------------------------*/

static int hb_gtnap_InkeyNext(PHB_GT pGT, int iEventMask)
{
    HB_SYMBOL_UNUSED(pGT);
    if (i_LOG_HBFUNCS == TRUE)
        log_printf("hb_gtnap_InkeyNext %d", iEventMask);
    return 0;
}

/*---------------------------------------------------------------------------*/

static void hb_gtnap_InkeyPoll(PHB_GT pGT)
{
    HB_SYMBOL_UNUSED(pGT);
    if (i_LOG_HBFUNCS == TRUE)
        log_printf("hb_gtnap_InkeyPoll");
}

/*---------------------------------------------------------------------------*/

static void hb_gtnap_InkeySetText(PHB_GT pGT, const char *szText, HB_SIZE nLen, HB_BOOL fEol)
{
    HB_SYMBOL_UNUSED(pGT);
    HB_SYMBOL_UNUSED(szText);
    HB_SYMBOL_UNUSED(fEol);
    if (i_LOG_HBFUNCS == TRUE)
        log_printf("hb_gtnap_InkeySetText %d", (int)nLen);
}

/*---------------------------------------------------------------------------*/

static int hb_gtnap_InkeySetLast(PHB_GT pGT, int iKey)
{
    HB_SYMBOL_UNUSED(pGT);
    if (i_LOG_HBFUNCS == TRUE)
        log_printf("hb_gtnap_InkeySetLast %d", iKey);
    return HB_TRUE;
}

/*---------------------------------------------------------------------------*/

static void hb_gtnap_InkeyReset(PHB_GT pGT)
{
    HB_SYMBOL_UNUSED(pGT);
    if (i_LOG_HBFUNCS == TRUE)
        log_printf("hb_gtnap_InkeyReset");
}

/*---------------------------------------------------------------------------*/

static void hb_gtnap_InkeyExit(PHB_GT pGT)
{
    HB_SYMBOL_UNUSED(pGT);
    if (i_LOG_HBFUNCS == TRUE)
        log_printf("hb_gtnap_InkeyExit");
}

/*---------------------------------------------------------------------------*/

static void hb_gtnap_MouseInit(PHB_GT pGT)
{
    HB_SYMBOL_UNUSED(pGT);
    if (i_LOG_HBFUNCS == TRUE)
        log_printf("hb_gtnap_MouseInit");
}

/*---------------------------------------------------------------------------*/

static void hb_gtnap_MouseExit(PHB_GT pGT)
{
    HB_SYMBOL_UNUSED(pGT);
    if (i_LOG_HBFUNCS == TRUE)
        log_printf("hb_gtnap_MouseExit");
}

/*---------------------------------------------------------------------------*/

static HB_BOOL hb_gtnap_MouseIsPresent(PHB_GT pGT)
{
    HB_SYMBOL_UNUSED(pGT);
    if (i_LOG_HBFUNCS == TRUE)
        log_printf("hb_gtnap_MouseIsPresent");
    return TRUE;
}

/*---------------------------------------------------------------------------*/

static void hb_gtnap_MouseShow(PHB_GT pGT)
{
    HB_SYMBOL_UNUSED(pGT);
    if (i_LOG_HBFUNCS == TRUE)
        log_printf("hb_gtnap_MouseShow");
}

/*---------------------------------------------------------------------------*/

static void hb_gtnap_MouseHide(PHB_GT pGT)
{
    HB_SYMBOL_UNUSED(pGT);
    if (i_LOG_HBFUNCS == TRUE)
        log_printf("hb_gtnap_MouseHide");
}

/*---------------------------------------------------------------------------*/

static HB_BOOL hb_gtnap_MouseGetCursor(PHB_GT pGT)
{
    HB_SYMBOL_UNUSED(pGT);
    if (i_LOG_HBFUNCS == TRUE)
        log_printf("hb_gtnap_MouseGetCursor");
    return HB_TRUE;
}

/*---------------------------------------------------------------------------*/

static void hb_gtnap_MouseSetCursor(PHB_GT pGT, HB_BOOL fVisible)
{
    HB_SYMBOL_UNUSED(pGT);
    HB_SYMBOL_UNUSED(fVisible);
    if (i_LOG_HBFUNCS == TRUE)
        log_printf("hb_gtnap_MouseSetCursor");
}

/*---------------------------------------------------------------------------*/

static int hb_gtnap_MouseCol(PHB_GT pGT)
{
    HB_SYMBOL_UNUSED(pGT);
    if (i_LOG_HBFUNCS == TRUE)
        log_printf("hb_gtnap_MouseCol");
    return 1;
}

/*---------------------------------------------------------------------------*/

static int hb_gtnap_MouseRow(PHB_GT pGT)
{
    HB_SYMBOL_UNUSED(pGT);
    if (i_LOG_HBFUNCS == TRUE)
        log_printf("hb_gtnap_MouseRow");
    return 1;
}

/*---------------------------------------------------------------------------*/

static void hb_gtnap_MouseGetPos(PHB_GT pGT, int *piRow, int *piCol)
{
    HB_SYMBOL_UNUSED(pGT);
    HB_SYMBOL_UNUSED(piRow);
    HB_SYMBOL_UNUSED(piCol);
    if (i_LOG_HBFUNCS == TRUE)
        log_printf("hb_gtnap_MouseGetPos");
}

/*---------------------------------------------------------------------------*/

static void hb_gtnap_MouseSetPos(PHB_GT pGT, int iRow, int iCol)
{
    HB_SYMBOL_UNUSED(pGT);
    if (i_LOG_HBFUNCS == TRUE)
        log_printf("hb_gtnap_MouseSetPos %d %d", iRow, iCol);
}

/*---------------------------------------------------------------------------*/

static void hb_gtnap_MouseSetBounds(PHB_GT pGT, int iTop, int iLeft, int iBottom, int iRight)
{
    HB_SYMBOL_UNUSED(pGT);
    if (i_LOG_HBFUNCS == TRUE)
        log_printf("hb_gtnap_MouseSetBounds %d %d %d %d", iTop, iLeft, iBottom, iRight);
}

/*---------------------------------------------------------------------------*/

static void hb_gtnap_MouseGetBounds(PHB_GT pGT, int *piTop, int *piLeft, int *piBottom, int *piRight)
{
    HB_SYMBOL_UNUSED(pGT);
    HB_SYMBOL_UNUSED(piTop);
    HB_SYMBOL_UNUSED(piLeft);
    HB_SYMBOL_UNUSED(piBottom);
    HB_SYMBOL_UNUSED(piRight);
    if (i_LOG_HBFUNCS == TRUE)
        log_printf("hb_gtnap_MouseGetBounds");
}

/*---------------------------------------------------------------------------*/

static int hb_gtnap_MouseStorageSize(PHB_GT pGT)
{
    HB_SYMBOL_UNUSED(pGT);
    if (i_LOG_HBFUNCS == TRUE)
        log_printf("hb_gtnap_MouseStorageSize");
    return 1;
}

/*---------------------------------------------------------------------------*/

static void hb_gtnap_MouseSaveState(PHB_GT pGT, void *pBuffer)
{
    HB_SYMBOL_UNUSED(pGT);
    HB_SYMBOL_UNUSED(pBuffer);
    if (i_LOG_HBFUNCS == TRUE)
        log_printf("hb_gtnap_MouseSaveState");
}

/*---------------------------------------------------------------------------*/

static void hb_gtnap_MouseRestoreState(PHB_GT pGT, const void *pBuffer)
{
    HB_SYMBOL_UNUSED(pGT);
    HB_SYMBOL_UNUSED(pBuffer);
    if (i_LOG_HBFUNCS == TRUE)
        log_printf("hb_gtnap_MouseRestoreState");
}

/*---------------------------------------------------------------------------*/

static int hb_gtnap_MouseGetDoubleClickSpeed(PHB_GT pGT)
{
    HB_SYMBOL_UNUSED(pGT);
    if (i_LOG_HBFUNCS == TRUE)
        log_printf("hb_gtnap_MouseGetDoubleClickSpeed");
    return 1;
}

/*---------------------------------------------------------------------------*/

static void hb_gtnap_MouseSetDoubleClickSpeed(PHB_GT pGT, int iSpeed)
{
    HB_SYMBOL_UNUSED(pGT);
    HB_SYMBOL_UNUSED(iSpeed);
    if (i_LOG_HBFUNCS == TRUE)
        log_printf("hb_gtnap_MouseSetDoubleClickSpeed");
}

/*---------------------------------------------------------------------------*/

static int hb_gtnap_MouseCountButton(PHB_GT pGT)
{
    HB_SYMBOL_UNUSED(pGT);
    if (i_LOG_HBFUNCS == TRUE)
        log_printf("hb_gtnap_MouseCountButton");
    return 3;
}

/*---------------------------------------------------------------------------*/

static HB_BOOL hb_gtnap_MouseButtonState(PHB_GT pGT, int iButton)
{
    HB_SYMBOL_UNUSED(pGT);
    if (i_LOG_HBFUNCS == TRUE)
        log_printf("hb_gtnap_MouseButtonState %d", iButton);
    return HB_TRUE;
}

/*---------------------------------------------------------------------------*/

static HB_BOOL hb_gtnap_MouseButtonPressed(PHB_GT pGT, int iButton, int *piRow, int *piCol)
{
    HB_SYMBOL_UNUSED(pGT);
    HB_SYMBOL_UNUSED(piRow);
    HB_SYMBOL_UNUSED(piCol);
    if (i_LOG_HBFUNCS == TRUE)
        log_printf("hb_gtnap_MouseButtonPressed %d", iButton);
    return HB_TRUE;
}

/*---------------------------------------------------------------------------*/

static HB_BOOL hb_gtnap_MouseButtonReleased(PHB_GT pGT, int iButton, int *piRow, int *piCol)
{
    HB_SYMBOL_UNUSED(pGT);
    HB_SYMBOL_UNUSED(piRow);
    HB_SYMBOL_UNUSED(piCol);
    if (i_LOG_HBFUNCS == TRUE)
        log_printf("hb_gtnap_MouseButtonReleased %d", iButton);
    return HB_TRUE;
}

/*---------------------------------------------------------------------------*/

static int hb_gtnap_MouseReadKey(PHB_GT pGT, int iEventMask)
{
    HB_SYMBOL_UNUSED(pGT);
    if (i_LOG_HBFUNCS == TRUE)
        log_printf("hb_gtnap_MouseReadKey %d", iEventMask);
    return 0;
}

/*---------------------------------------------------------------------------*/

static int hb_gtnap_gfxPrimitive(PHB_GT pGT, int iType, int iTop, int iLeft, int iBottom, int iRight, int iColor)
{
    HB_SYMBOL_UNUSED(pGT);
    HB_SYMBOL_UNUSED(iColor);
    if (i_LOG_HBFUNCS == TRUE)
        log_printf("hb_gtnap_gfxPrimitive (%d) %d %d %d %d", iType, iTop, iLeft, iBottom, iRight);

    return 1;
}

/*---------------------------------------------------------------------------*/

static void hb_gtnap_gfxText(PHB_GT pGT, int iTop, int iLeft, const char *cBuf, int iColor, int iSize, int iWidth)
{
    HB_SYMBOL_UNUSED(pGT);
    HB_SYMBOL_UNUSED(cBuf);
    HB_SYMBOL_UNUSED(iColor);
    HB_SYMBOL_UNUSED(iSize);
    HB_SYMBOL_UNUSED(iWidth);
    if (i_LOG_HBFUNCS == TRUE)
        log_printf("hb_gtnap_gfxText %d %d", iTop, iLeft);
}

/*---------------------------------------------------------------------------*/

static void hb_gtnap_WhoCares(PHB_GT pGT, void *pCargo)
{
    HB_SYMBOL_UNUSED(pGT);
    HB_SYMBOL_UNUSED(pCargo);
    if (i_LOG_HBFUNCS == TRUE)
        log_printf("hb_gtnap_WhoCares");
}

/*---------------------------------------------------------------------------*/

static HB_BOOL hb_gt_FuncInit(PHB_GT_FUNCS pFuncTable)
{
    pFuncTable->Lock = hb_gtnap_Lock;
    pFuncTable->Unlock = hb_gtnap_Unlock;
    pFuncTable->Init = hb_gtnap_Init;
    pFuncTable->Exit = hb_gtnap_Exit;

    if (i_FULL_HBFUNCS)
    {
        pFuncTable->New = hb_gtnap_New;
        pFuncTable->Free = hb_gtnap_Free;
        pFuncTable->Mark = hb_gtnap_Mark;
    }

    pFuncTable->Resize = hb_gtnap_Resize;
    pFuncTable->SetMode = hb_gtnap_SetMode;
    pFuncTable->GetSize = hb_gtnap_GetSize;

    if (i_FULL_HBFUNCS)
    {
        pFuncTable->SemiCold = hb_gtnap_SemiCold;
        pFuncTable->ColdArea = hb_gtnap_ColdArea;
    }

    pFuncTable->ExposeArea = hb_gtnap_ExposeArea;

    if (i_FULL_HBFUNCS)
    {
        pFuncTable->ScrollArea = hb_gtnap_ScrollArea;
        pFuncTable->TouchLine = hb_gtnap_TouchLine;
        pFuncTable->TouchCell = hb_gtnap_TouchCell;
        pFuncTable->Redraw = hb_gtnap_Redraw;
        pFuncTable->RedrawDiff = hb_gtnap_RedrawDiff;
        pFuncTable->Refresh = hb_gtnap_Refresh;
        pFuncTable->Flush = hb_gtnap_Flush;
    }

    pFuncTable->MaxCol = hb_gtnap_MaxCol;
    pFuncTable->MaxRow = hb_gtnap_MaxRow;
    pFuncTable->CheckPos = hb_gtnap_CheckPos;
    pFuncTable->SetPos = hb_gtnap_SetPos;
    pFuncTable->GetPos = hb_gtnap_GetPos;
    pFuncTable->IsColor = hb_gtnap_IsColor;

    if (i_FULL_HBFUNCS)
    {
        pFuncTable->GetColorStr = hb_gtnap_GetColorStr;
        pFuncTable->SetColorStr = hb_gtnap_SetColorStr;
        pFuncTable->ColorSelect = hb_gtnap_ColorSelect;
        pFuncTable->GetColor = hb_gtnap_GetColor;
        pFuncTable->ColorNum = hb_gtnap_ColorNum;
        pFuncTable->ColorsToString = hb_gtnap_ColorsToString;
        pFuncTable->StringToColors = hb_gtnap_StringToColors;
        pFuncTable->GetColorData = hb_gtnap_GetColorData;
        pFuncTable->GetClearColor = hb_gtnap_GetClearColor;
        pFuncTable->SetClearColor = hb_gtnap_SetClearColor;
        pFuncTable->GetClearChar = hb_gtnap_GetClearChar;
        pFuncTable->SetClearChar = hb_gtnap_SetClearChar;
    }

    pFuncTable->GetCursorStyle = hb_gtnap_GetCursorStyle;
    pFuncTable->SetCursorStyle = hb_gtnap_SetCursorStyle;

    if (i_FULL_HBFUNCS)
    {
        pFuncTable->GetScrCursor = hb_gtnap_GetScrCursor;
        pFuncTable->GetScrChar = hb_gtnap_GetScrChar;
        pFuncTable->PutScrChar = hb_gtnap_PutScrChar;
        pFuncTable->GetScrUC = hb_gtnap_GetScrUC;
    }

    pFuncTable->DispBegin = hb_gtnap_DispBegin;
    pFuncTable->DispEnd = hb_gtnap_DispEnd;
    pFuncTable->DispCount = hb_gtnap_DispCount;
    pFuncTable->GetChar = hb_gtnap_GetChar;
    pFuncTable->PutChar = hb_gtnap_PutChar;

    if (i_FULL_HBFUNCS)
    {
        pFuncTable->RectSize = hb_gtnap_RectSize;
    }

    pFuncTable->Save = hb_gtnap_Save;
    pFuncTable->Rest = hb_gtnap_Rest;
    pFuncTable->PutText = hb_gtnap_PutText;
    pFuncTable->PutTextW = hb_gtnap_PutTextW;
    pFuncTable->Replicate = hb_gtnap_Replicate;
    pFuncTable->WriteAt = hb_gtnap_WriteAt;

    if (i_FULL_HBFUNCS)
    {
        pFuncTable->WriteAtW = hb_gtnap_WriteAtW;
        pFuncTable->Write = hb_gtnap_Write;
        pFuncTable->WriteW = hb_gtnap_WriteW;
        pFuncTable->WriteCon = hb_gtnap_WriteCon;
        pFuncTable->WriteConW = hb_gtnap_WriteConW;
        pFuncTable->DrawShadow = hb_gtnap_DrawShadow;
        pFuncTable->ScrollUp = hb_gtnap_ScrollUp;
    }

    pFuncTable->SetAttribute = hb_gtnap_SetAttribute;
    pFuncTable->Scroll = hb_gtnap_Scroll;
    pFuncTable->Box = hb_gtnap_Box;

    if (i_FULL_HBFUNCS)
    {
        pFuncTable->BoxW = hb_gtnap_BoxW;
        pFuncTable->BoxD = hb_gtnap_BoxD;
        pFuncTable->BoxS = hb_gtnap_BoxS;
    }

    pFuncTable->HorizLine = hb_gtnap_HorizLine;
    pFuncTable->VertLine = hb_gtnap_VertLine;
    pFuncTable->GetBlink = hb_gtnap_GetBlink;
    pFuncTable->SetBlink = hb_gtnap_SetBlink;
    pFuncTable->Version = hb_gtnap_Version;

    if (i_FULL_HBFUNCS)
    {
        pFuncTable->SetSnowFlag = hb_gtnap_SetSnowFlag;
        pFuncTable->Suspend = hb_gtnap_Suspend;
        pFuncTable->Resume = hb_gtnap_Resume;
        pFuncTable->PreExt = hb_gtnap_PreExt;
        pFuncTable->PostExt = hb_gtnap_PostExt;
    }

    pFuncTable->OutStd = hb_gtnap_OutStd;
    pFuncTable->OutErr = hb_gtnap_OutErr;
    pFuncTable->Tone = hb_gtnap_Tone;
    pFuncTable->Info = hb_gtnap_Info;
    pFuncTable->Alert = hb_gtnap_Alert;

    if (i_FULL_HBFUNCS)
    {
        pFuncTable->Bell = hb_gtnap_Bell;
        pFuncTable->SetFlag = hb_gtnap_SetFlag;
    }

    /* internationalization */
    if (i_FULL_HBFUNCS)
    {
        pFuncTable->SetDispCP = hb_gtnap_SetDispCP;
        pFuncTable->SetKeyCP = hb_gtnap_SetKeyCP;
    }

    /* keyboard */
    pFuncTable->ReadKey = hb_gtnap_ReadKey;

    if (i_FULL_HBFUNCS)
    {
        pFuncTable->InkeyGet = hb_gtnap_InkeyGet;
        pFuncTable->InkeyPut = hb_gtnap_InkeyPut;
        pFuncTable->InkeyIns = hb_gtnap_InkeyIns;
        pFuncTable->InkeyLast = hb_gtnap_InkeyLast;
        pFuncTable->InkeyNext = hb_gtnap_InkeyNext;
        pFuncTable->InkeyPoll = hb_gtnap_InkeyPoll;
        pFuncTable->InkeySetText = hb_gtnap_InkeySetText;
        pFuncTable->InkeySetLast = hb_gtnap_InkeySetLast;
        pFuncTable->InkeyReset = hb_gtnap_InkeyReset;
        pFuncTable->InkeyExit = hb_gtnap_InkeyExit;
    }

    /* mouse */
    pFuncTable->MouseInit = hb_gtnap_MouseInit;
    pFuncTable->MouseExit = hb_gtnap_MouseExit;
    pFuncTable->MouseIsPresent = hb_gtnap_MouseIsPresent;

    if (i_FULL_HBFUNCS)
    {
        pFuncTable->MouseShow = hb_gtnap_MouseShow;
        pFuncTable->MouseHide = hb_gtnap_MouseHide;
        pFuncTable->MouseGetCursor = hb_gtnap_MouseGetCursor;
        pFuncTable->MouseSetCursor = hb_gtnap_MouseSetCursor;
    }

    pFuncTable->MouseCol = hb_gtnap_MouseCol;
    pFuncTable->MouseRow = hb_gtnap_MouseRow;

    if (i_FULL_HBFUNCS)
    {
        pFuncTable->MouseGetPos = hb_gtnap_MouseGetPos;
        pFuncTable->MouseSetPos = hb_gtnap_MouseSetPos;
        pFuncTable->MouseSetBounds = hb_gtnap_MouseSetBounds;
        pFuncTable->MouseGetBounds = hb_gtnap_MouseGetBounds;
        pFuncTable->MouseStorageSize = hb_gtnap_MouseStorageSize;
        pFuncTable->MouseSaveState = hb_gtnap_MouseSaveState;
        pFuncTable->MouseRestoreState = hb_gtnap_MouseRestoreState;
        pFuncTable->MouseGetDoubleClickSpeed = hb_gtnap_MouseGetDoubleClickSpeed;
        pFuncTable->MouseSetDoubleClickSpeed = hb_gtnap_MouseSetDoubleClickSpeed;
    }

    pFuncTable->MouseCountButton = hb_gtnap_MouseCountButton;
    pFuncTable->MouseButtonState = hb_gtnap_MouseButtonState;

    if (i_FULL_HBFUNCS)
    {
        pFuncTable->MouseButtonPressed = hb_gtnap_MouseButtonPressed;
        pFuncTable->MouseButtonReleased = hb_gtnap_MouseButtonReleased;
        pFuncTable->MouseReadKey = hb_gtnap_MouseReadKey;
    }

    /* Graphics API */
    pFuncTable->GfxPrimitive = hb_gtnap_gfxPrimitive;
    pFuncTable->GfxText = hb_gtnap_gfxText;

    if (i_FULL_HBFUNCS)
    {
        pFuncTable->WhoCares = hb_gtnap_WhoCares;
    }

    return TRUE;
}

/*---------------------------------------------------------------------------*/

#include "hbgtreg.h"
