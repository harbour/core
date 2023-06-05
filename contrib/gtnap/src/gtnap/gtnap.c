/*
    This is part of gtnap
    TODO: More info
    Commit - 2
*/

#include "gtnap.h"
#include "gtnap.inl"
#include "gtnap.ch"
#include "nap_menu.inl"
#include "gtconvert.h"
#include "nappgui.h"
#include "osmain.h"
#include "drawctrl.inl"

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
typedef struct _gtnap_window_t GtNapWindow;
typedef struct _gtnap_t GtNap;

typedef void(*FPtr_gtnap_callback)(GtNapCallback *callback, Event *event);

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
    int32_t key;    /* inkey.ch */
    vkey_t vkey;
    uint32_t modifiers;
};

struct _gtnap_column_t
{
    uint32_t fixed_width;
    uint32_t width;
    align_t align;
    String *title;
    HB_ITEM *block;
};

struct _gtnap_toolbar_t
{
    uint32_t pixels_image;
    uint32_t pixels_button;
    ArrPt(Button) *buttons;
};

struct _gtnap_area_t
{
    AREA *area;
    HB_ULONG cache_recno;           /* Store the DB recno while table drawing */
    GtNapObject *gtobj;
    ArrSt(uint32_t) *records;       /* Records visible in table (index, deleted, filters) */
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
    bool_t is_last_edit;
    bool_t in_scroll;
    bool_t can_auto_lista;
    bool_t has_focus;
    uint32_t editSize;
    uint32_t editBoxIndexForButton;
    PHB_ITEM text_block;
    PHB_ITEM get_set_block;
    PHB_ITEM is_editable_block;
    PHB_ITEM when_block;
    PHB_ITEM valida_block;
    PHB_ITEM message_block;
    PHB_ITEM keyfilter_block;
    PHB_ITEM auto_block;
    PHB_ITEM wizard_block;
    PHB_ITEM confirmaCodeBlock;
    PHB_ITEM getobjItem;
    ArrSt(GtNapColumn) *columns;
    GuiComponent *component;
    GtNapWindow *cuawin;
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
    uint16_t *text_buffer;
    bool_t is_configured;
    bool_t is_closed_by_esc;
    bool_t focus_by_previous;
    bool_t modal_window_alive;
    bool_t buttons_navigation;
    uint32_t message_label_id;
    uint32_t default_button;
    GtNapObject *current_obj;
    Window *window;
    S2Df panel_size;
    Panel *panel;
    Panel *scrolled_panel;
    View *canvas;
    GtNapToolbar *toolbar;
    GtNapArea *gtarea;
    uint32_t num_rows;
    // TODO HB_ITEM *row_param;
    ArrPt(GtNapObject) *gui_objects;
    ArrPt(GtNapCallback) *callbacks;
};

struct _gtnap_t
{
    Font *global_font;
    Font *reduced_font;
    uint8_t date_digits;
    uint8_t date_chars;
    String *title;
    uint32_t rows;
    uint32_t cols;
    uint32_t cell_x_size;
    uint32_t cell_y_size;
    uint32_t label_y_size;
    uint32_t button_y_size;
    uint32_t edit_y_size;
    ArrSt(GtNapWindow) *windows;
    /* TODO Implement a stack of modal window IDs 'hb_gtnap_window_stop_modal' */
};

/*---------------------------------------------------------------------------*/

DeclPt(GtNapCallback);
DeclSt(GtNapColumn);
DeclPt(GtNapArea);
DeclPt(GtNapObject);
DeclSt(GtNapWindow);
DeclPt(Button);

/*---------------------------------------------------------------------------*/

static const GtNapKey KEYMAPS[] = {
    { K_F1, ekKEY_F1, 0 },
    { K_F2, ekKEY_F2, 0 },
    { K_F3, ekKEY_F3, 0 },
    { K_F4, ekKEY_F4, 0 },
    { K_F5, ekKEY_F5, 0 },
    { K_F6, ekKEY_F6, 0 },
    { K_F7, ekKEY_F7, 0 },
    { K_F8, ekKEY_F8, 0 },
    { K_F9, ekKEY_F9, 0 },
    { K_F10, ekKEY_F10, 0 },
    { K_F11, ekKEY_F11, 0 },
    { K_F12, ekKEY_F12, 0 },

    { K_ENTER, ekKEY_RETURN, 0 },
    { K_SPACE, ekKEY_SPACE, 0},

    { 'a', ekKEY_A, 0},
    { 'b', ekKEY_B, 0},
    { 'c', ekKEY_C, 0},
    { 'd', ekKEY_D, 0},
    { 'e', ekKEY_E, 0},
    { 'f', ekKEY_F, 0},
    { 'g', ekKEY_G, 0},
    { 'h', ekKEY_H, 0},
    { 'i', ekKEY_I, 0},
    { 'j', ekKEY_J, 0},
    { 'k', ekKEY_K, 0},
    { 'l', ekKEY_L, 0},
    { 'm', ekKEY_M, 0},
    { 'n', ekKEY_N, 0},
    { 'o', ekKEY_O, 0},
    { 'p', ekKEY_P, 0},
    { 'q', ekKEY_Q, 0},
    { 'r', ekKEY_R, 0},
    { 's', ekKEY_S, 0},
    { 't', ekKEY_T, 0},
    { 'u', ekKEY_U, 0},
    { 'v', ekKEY_V, 0},
    { 'w', ekKEY_W, 0},
    { 'x', ekKEY_X, 0},
    { 'y', ekKEY_Y, 0},
    { 'z', ekKEY_Z, 0},

    { 'A', ekKEY_A, 0},
    { 'B', ekKEY_B, 0},
    { 'C', ekKEY_C, 0},
    { 'D', ekKEY_D, 0},
    { 'E', ekKEY_E, 0},
    { 'F', ekKEY_F, 0},
    { 'G', ekKEY_G, 0},
    { 'H', ekKEY_H, 0},
    { 'I', ekKEY_I, 0},
    { 'J', ekKEY_J, 0},
    { 'K', ekKEY_K, 0},
    { 'L', ekKEY_L, 0},
    { 'M', ekKEY_M, 0},
    { 'N', ekKEY_N, 0},
    { 'O', ekKEY_O, 0},
    { 'P', ekKEY_P, 0},
    { 'Q', ekKEY_Q, 0},
    { 'R', ekKEY_R, 0},
    { 'S', ekKEY_S, 0},
    { 'T', ekKEY_T, 0},
    { 'U', ekKEY_U, 0},
    { 'V', ekKEY_V, 0},
    { 'W', ekKEY_W, 0},
    { 'X', ekKEY_X, 0},
    { 'Y', ekKEY_Y, 0},
    { 'Z', ekKEY_Z, 0},

    { '0', ekKEY_0, 0},
    { '1', ekKEY_1, 0},
    { '2', ekKEY_2, 0},
    { '3', ekKEY_3, 0},
    { '4', ekKEY_4, 0},
    { '5', ekKEY_5, 0},
    { '6', ekKEY_6, 0},
    { '7', ekKEY_7, 0},
    { '8', ekKEY_8, 0},
    { '9', ekKEY_9, 0}
};

/*---------------------------------------------------------------------------*/

__EXTERN_C

/*
 * These are internal, non-documented functions of NAppGUI.
 * They are used for direct handling of widgets, avoiding the 'layout' layer.
 */
Window *_component_window(const GuiComponent *component);
void _component_attach_to_panel(GuiComponent *panel_component, GuiComponent *child_component);
void _component_detach_from_panel(GuiComponent *panel_component, GuiComponent *child_component);
void _component_set_frame(GuiComponent *component, const V2Df *origin, const S2Df *size);
void _component_get_origin(const GuiComponent *component, V2Df *origin);
void _component_get_size(const GuiComponent *component, S2Df *size);
uint32_t _component_get_tag(const GuiComponent *component);
void _component_set_tag(GuiComponent *component, const uint32_t tag);
void _component_visible(GuiComponent *component, const bool_t visible);
void _component_destroy(GuiComponent **component);
void _component_taborder(GuiComponent *component, Window *window);
const char_t *_component_type(const GuiComponent *component);
void _panel_compose(Panel *panel, const S2Df *required_size, S2Df *final_size);
void _panel_locate(Panel *panel);
void _panel_detach_components(Panel *panel);
void _window_taborder(Window *window, void *ositem);
void _window_focus(Window *window, GuiComponent *component);
void _panel_content_size(Panel *panel, const real32_t width, const real32_t height);

__END_C

/*---------------------------------------------------------------------------*/

static GtNap *GTNAP_GLOBAL = NULL;
static char_t INIT_TITLE[128];
static PHB_ITEM INIT_CODEBLOCK = NULL;
static uint32_t INIT_ROWS = 0;
static uint32_t INIT_COLS = 0;

#define STATIC_TEXT_SIZE    1024
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
    arrpt_foreach(button, toolbar->buttons, Button)
        if (button != NULL)
        {
            Button *dbutton = button;

            if (is_configured == TRUE)
                _component_detach_from_panel((GuiComponent*)panel, (GuiComponent*)button);

            _component_destroy((GuiComponent**)&dbutton);
        }
    arrpt_end();
    arrpt_destroy(&toolbar->buttons, NULL, Button);
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
    gtobj = arrpt_get(gtwin->gui_objects, index, GtNapObject);

    _component_visible(gtobj->component, FALSE);

    if (gtwin->is_configured == TRUE)
    {
        const char_t *type = _component_type(gtobj->component);
        if (str_equ_c(type, "Panel") == TRUE)
            _panel_detach_components((Panel*)gtobj->component);

        if (gtobj->in_scroll == TRUE)
            _component_detach_from_panel((GuiComponent*)gtwin->scrolled_panel, gtobj->component);
        else
            _component_detach_from_panel((GuiComponent*)gtwin->panel, gtobj->component);
    }

    _component_destroy(&gtobj->component);

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

    if (gtobj->confirmaCodeBlock != NULL)
        hb_itemRelease(gtobj->confirmaCodeBlock);

    if (gtobj->getobjItem != NULL)
        hb_itemRelease(gtobj->getobjItem);

    arrst_destopt(&gtobj->columns, i_remove_column, GtNapColumn);

    heap_delete(&gtobj, GtNapObject);
    arrpt_delete(gtwin->gui_objects, index, NULL, GtNapObject);
}

/*---------------------------------------------------------------------------*/

static void i_remove_gtwin(GtNapWindow *gtwin)
{
    cassert_no_null(gtwin);

    {
        uint32_t i, n = arrpt_size(gtwin->gui_objects, GtNapObject);
        for (i = 0; i < n; ++i)
            i_destroy_gtobject(gtwin, 0);
    }

    if (gtwin->canvas != NULL)
    {
        _component_visible((GuiComponent*)gtwin->canvas, FALSE);

        if (gtwin->is_configured == TRUE)
            _component_detach_from_panel((GuiComponent*)gtwin->panel, (GuiComponent*)gtwin->canvas);

        _component_destroy((GuiComponent**)&gtwin->canvas);
    }

    if (gtwin->scrolled_panel != NULL)
    {
        _component_visible((GuiComponent*)gtwin->scrolled_panel, FALSE);

        if (gtwin->is_configured == TRUE)
            _component_detach_from_panel((GuiComponent*)gtwin->panel, (GuiComponent*)gtwin->scrolled_panel);

        _component_destroy((GuiComponent**)&gtwin->scrolled_panel);
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

    if (gtwin->text_buffer != NULL)
    {
        uint32_t cwidth = gtwin->right - gtwin->left + 1;
        uint32_t cheight = gtwin->bottom - gtwin->top + 1;
        heap_free((byte_t**)&gtwin->text_buffer, cwidth * cheight * sizeof(uint16_t), "gtwin_textbuffer");
    }

    cassert(arrpt_size(gtwin->gui_objects, GtNapObject) == 0);
    arrpt_destroy(&gtwin->gui_objects, NULL, GtNapObject);
    arrpt_destroy(&gtwin->callbacks, i_destroy_callback, GtNapCallback);

    if (gtwin->parent_id == UINT32_MAX)
    {
        window_destroy(&gtwin->window);
    }
    else
    {
        cassert(gtwin->window == NULL);
        if (gtwin->panel != NULL)
            _component_destroy((GuiComponent**)&gtwin->panel);
    }
}

/*---------------------------------------------------------------------------*/

static void i_gtnap_destroy(GtNap **gtnap)
{
    cassert_no_null(gtnap);
    cassert_no_null(*gtnap);
    cassert(*gtnap == GTNAP_GLOBAL);
    cassert(arrst_size((*gtnap)->windows, GtNapWindow) == 0);
    arrst_destroy(&(*gtnap)->windows, i_remove_gtwin, GtNapWindow);
    font_destroy(&(*gtnap)->global_font);
    font_destroy(&(*gtnap)->reduced_font);
    str_destroy(&(*gtnap)->title);
    heap_delete(&(*gtnap), GtNap);
}

/*---------------------------------------------------------------------------*/

static GtNapWindow *i_gtwin(GtNap *gtnap, const uint32_t wid)
{
    cassert_no_null(gtnap);
    arrst_foreach(gtwin, gtnap->windows, GtNapWindow)
        if (gtwin->id == wid)
            return gtwin;
    arrst_end()
    cassert_msg(FALSE, "Invalid window id");
    return NULL;
}

/*---------------------------------------------------------------------------*/

static uint32_t i_gtwin_index(GtNap *gtnap, const uint32_t wid)
{
    cassert_no_null(gtnap);
    arrst_foreach(gtwin, gtnap->windows, GtNapWindow)
        if (gtwin->id == wid)
            return gtwin_i;
    arrst_end()
    cassert(FALSE);
    return UINT32_MAX;
}

/*---------------------------------------------------------------------------*/

static bool_t i_gtwin_has_embedded(GtNap *gtnap, const uint32_t wid)
{
    cassert_no_null(gtnap);
    arrst_foreach(gtwin, gtnap->windows, GtNapWindow)
        if (gtwin->parent_id == wid)
            return TRUE;
    arrst_end()
    return FALSE;
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

static GtNapWindow *i_current_gtwin(GtNap *gtnap)
{
    uint32_t id = 0;
    cassert_no_null(gtnap);
    id = arrst_size(gtnap->windows, GtNapWindow);
    if (id >= 1)
        return arrst_get(gtnap->windows, id - 1, GtNapWindow);
    return NULL;
}

/*---------------------------------------------------------------------------*/

static GtNapWindow *i_current_main_gtwin(GtNap *gtnap)
{
    cassert_no_null(gtnap);
    arrst_forback(gtwin, gtnap->windows, GtNapWindow)
        if (gtwin->parent_id == UINT32_MAX)
            return gtwin;
    arrst_end();
    return NULL;
}

/*---------------------------------------------------------------------------*/

/* FRAN REMOVE!!! Parent must come in hb_gtnap_window_modal */
static GtNapWindow *i_parent_cuawin(GtNap *gtnap)
{
    uint32_t c = 0;
    cassert_no_null(gtnap);
    arrst_forback(gtwin, gtnap->windows, GtNapWindow);
        if (gtwin->parent_id == UINT32_MAX)
        {
            if (c > 0)
            {
                cassert_no_null(gtwin->window);
                return gtwin;
            }

            c += 1;
        }
    arrst_end();

    return NULL;
}

/*---------------------------------------------------------------------------*/

static GtNapObject *i_gtobj(GtNap *gtnap, const uint32_t wid, const uint32_t id)
{
    GtNapWindow *gtwin = i_gtwin(gtnap, wid);
    cassert_no_null(gtwin);
    return arrpt_get(gtwin->gui_objects, id, GtNapObject);
}

/*---------------------------------------------------------------------------*/

static GtNapObject *i_get_button(GtNapWindow *gtwin, const uint32_t index)
{
    uint32_t i = 0;
    cassert_no_null(gtwin);
    arrpt_foreach(obj, gtwin->gui_objects, GtNapObject)
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
    arrpt_foreach_const(obj, gtwin->gui_objects, GtNapObject)
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
    arrpt_foreach_const(obj, gtwin->gui_objects, GtNapObject)
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
    arrpt_foreach_const(obj, gtwin->gui_objects, GtNapObject)
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
    arrpt_foreach_const(obj, gtwin->gui_objects, GtNapObject)
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
        window_defbutton(gtwin->window, (Button*)button->component);
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

static void i_create_fonts(const real32_t size, GtNap *gtnap)
{
    real32_t rsize = bmath_ceilf(size * .9f);
    cassert_no_null(gtnap);
    ptr_destopt(font_destroy, &gtnap->global_font, Font);
    ptr_destopt(font_destroy, &gtnap->reduced_font, Font);
    gtnap->global_font = font_monospace(size, 0);
    gtnap->reduced_font = font_monospace(rsize, 0);
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

static void i_compute_cell_size(GtNap *gtnap)
{
    uint32_t bh = 0, eh = 0;
    uint32_t fw = 0, fh = 0;

    cassert_no_null(gtnap);

    /* Create an impostor window, only for measure the real height of buttons and edits */
    {
        Panel *panel = panel_create();
        Button *button = button_push();
        Edit *edit = edit_create();
        Window *window = window_create(ekWINDOW_STD | ekWINDOW_OFFSCREEN);
        Layout *layout = layout_create(1, 2);
        button_text(button, "DEMO");
        button_font(button, gtnap->reduced_font);
        button_vpadding(button, i_button_vpadding());
        edit_font(edit, gtnap->reduced_font);
        edit_vpadding(edit, i_edit_vpadding());
        layout_button(layout, button, 0, 0);
        layout_edit(layout, edit, 0, 1);
        panel_layout(panel, layout);
        window_panel(window, panel);
        window_show(window);
        bh = (uint32_t)button_get_height(button);
        eh = (uint32_t)edit_get_height(edit);
        window_destroy(&window);
    }

    {
        real32_t w, h;
        font_extents(gtnap->global_font, "OOOOOO", -1, &w, &h);
        fw = (uint32_t)w;
        fh = (uint32_t)h;
    }

    gtnap->cell_x_size = fw / 6;

    /* Cell height will be the higher of labels, buttons and edits */
    gtnap->cell_y_size = fh;
    gtnap->label_y_size = fh;
    gtnap->button_y_size = bh;
    gtnap->edit_y_size = eh;

    if (bh > gtnap->cell_y_size)
        gtnap->cell_y_size = bh;

    if (eh > gtnap->cell_y_size)
        gtnap->cell_y_size = eh;
}

/*---------------------------------------------------------------------------*/

static void i_compute_font_size(const uint32_t max_width, const uint32_t max_height, GtNap *gtnap)
{
    real32_t font_size = (real32_t)(max_height / (gtnap->rows + 5));

    for(;;)
    {
        i_create_fonts(font_size, gtnap);
        i_compute_cell_size(gtnap);

        /* The total width exceeds the screen limits */
        if (gtnap->cell_x_size * gtnap->cols > max_width)
        {
            font_size -= 1;
            continue;
        }

        /* The total height exceeds the screen limits */
        if (gtnap->cell_y_size * gtnap->rows > max_height)
        {
            font_size -= 1;
            continue;
        }

        break;
    }
}

/*---------------------------------------------------------------------------*/

static uint32_t i_remove_utf8_CR(char_t *utf8)
{
    /* Remove the Carriage Return (CR) character (NAppGUI doesn't like) */
    uint32_t i = 0, j = 0;
    for(; utf8[i] != 0;)
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
    hb_itemCopyStrUTF8(item, (char*)utf8, (HB_SIZE)size);
    return i_remove_utf8_CR(utf8);
}

/*---------------------------------------------------------------------------*/

static String *i_item_to_utf8_string(HB_ITEM *item)
{
    HB_SIZE s1 = 0, s2 = 0;
    String *str = NULL;
    cassert(HB_ITEM_TYPE(item) == HB_IT_STRING);
    s1 = hb_itemCopyStrUTF8(item, NULL, (HB_SIZE)UINT32_MAX);
    str = str_reserve(s1);
    s2 = hb_itemCopyStrUTF8(item, tcc(str), s1 + 1);
    cassert(s1 == s2);
    i_remove_utf8_CR(tcc(str));
    return str;
}

/*---------------------------------------------------------------------------*/

static uint32_t i_cp_to_utf8(const char_t *cp_str, char_t *utf8, const uint32_t size)
{
    HB_CODEPAGE *cp = hb_vmCDP();
    cassert_no_null(cp_str);
    cassert_no_null(utf8);
    hb_cdpStrToUTF8(cp, (const char*)cp_str, (HB_SIZE)str_len_c(cp_str), (char*)utf8, (HB_SIZE)size);
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
    String *str = str_reserve(s1);
    HB_SIZE s2 = hb_cdpUTF8ToStr(cp, utf8, n, tcc(str), s1 + 1);
    cassert(s1 == s2);
    return str;
}

/*---------------------------------------------------------------------------*/

static GtNap *i_gtnap_create(void)
{
    S2Df screen;
    GTNAP_GLOBAL = heap_new0(GtNap);
    GTNAP_GLOBAL->title = i_cp_to_utf8_string(INIT_TITLE);
    GTNAP_GLOBAL->rows = INIT_ROWS;
    GTNAP_GLOBAL->cols = INIT_COLS;
    GTNAP_GLOBAL->windows = arrst_create(GtNapWindow);
    GTNAP_GLOBAL->date_digits = (hb_setGetCentury() == HB_TRUE) ? 8 : 6;
    GTNAP_GLOBAL->date_chars = GTNAP_GLOBAL->date_digits + 2;
    globals_resolution(&screen);
    screen.height -= 50;        /* Margin for Dock or Taskbars */
    i_compute_font_size((uint32_t)screen.width, (uint32_t)screen.height, GTNAP_GLOBAL);

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

static void i_OnCanvasDraw(GtNapWindow *gtwin, Event *e)
{
    //const EvDraw *p = event_params(e, EvDraw);
    //draw_clear(p->ctx, gui_view_color());
    //draw_font(p->ctx, GTNAP_GLOBAL->global_font);
    //drawctrl_text(p->ctx, "Hello Canvas", 300, 200, ekCTRL_STATE_NORMAL);
    //draw_line(p->ctx, 0, 0, p->width, p->height);
    unref(gtwin);
    unref(e);
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

    return s2df((max_x - min_x) + GTNAP_GLOBAL->cell_x_size, (max_y - min_y) + GTNAP_GLOBAL->cell_y_size);
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
                _component_attach_to_panel((GuiComponent*)scroll_panel, object->component);
            else
                _component_attach_to_panel((GuiComponent*)main_panel, object->component);

            _component_visible(object->component, FALSE);

            if (toolbar != NULL)
            {
                switch(type) {
                case ekOBJ_LABEL:
                    pos.y += (real32_t)toolbar->pixels_button;
                    if (GTNAP_GLOBAL->cell_y_size > GTNAP_GLOBAL->label_y_size)
                        pos.y += (real32_t)((GTNAP_GLOBAL->cell_y_size - GTNAP_GLOBAL->label_y_size) / 2);
                    break;

                case ekOBJ_EDIT:
                    pos.y += (real32_t)toolbar->pixels_button;
                    if (GTNAP_GLOBAL->cell_y_size > GTNAP_GLOBAL->edit_y_size)
                        pos.y += (real32_t)((GTNAP_GLOBAL->cell_y_size - GTNAP_GLOBAL->edit_y_size) / 2);

                    if (object->in_scroll == TRUE)
                    {
                        object->size.width -= GTNAP_GLOBAL->cell_x_size;
                    }
                    break;

                case ekOBJ_IMAGE:
                case ekOBJ_MENU:
                    pos.y += (real32_t)toolbar->pixels_button;
                    break;
                case ekOBJ_TABLEVIEW:
                case ekOBJ_TEXTVIEW:
                    pos.y += (real32_t)(toolbar->pixels_button - GTNAP_GLOBAL->cell_y_size);
                    break;
                case ekOBJ_BUTTON:
                    if (object->editBoxIndexForButton == UINT32_MAX)
                        pos.y += (real32_t)GTNAP_GLOBAL->cell_y_size;
                    else
                        // The same as related editbox
                        pos.y += (real32_t)toolbar->pixels_button;

                    if (GTNAP_GLOBAL->cell_y_size > GTNAP_GLOBAL->button_y_size)
                        pos.y += (real32_t)((GTNAP_GLOBAL->cell_y_size - GTNAP_GLOBAL->button_y_size) / 2);
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

            // FRAN: TODO! IMPROVe
            //if (object->type == ekOBJ_LABEL)
            //    i_set_label_text(object, NULL);
        }
    arrpt_end();
}

/*---------------------------------------------------------------------------*/

static void i_attach_toolbar_to_panel(const GtNapToolbar *toolbar, Panel *panel)
{
    if (toolbar != NULL)
    {
        V2Df pos;
        S2Df size;
        pos.x = 0;
        pos.y = 0;
        size.width = (real32_t)toolbar->pixels_button;
        size.height = (real32_t)toolbar->pixels_button;

        arrpt_foreach(button, toolbar->buttons, Button)
            if (button != NULL)
            {
                _component_attach_to_panel((GuiComponent*)panel, (GuiComponent*)button);
                _component_visible((GuiComponent*)button, FALSE);
                _component_set_frame((GuiComponent*)button, &pos, &size);
                pos.x += (real32_t)toolbar->pixels_button;
            }
            else
            {
                pos.x += 5.f; // Separator
            }
        arrpt_end();
    }
}

/*---------------------------------------------------------------------------*/

static void i_component_tabstop(ArrPt(GtNapObject) *objects, Window *window, const objtype_t type)
{
    arrpt_foreach(object, objects, GtNapObject)
        if (object->type == type)
        {
            _component_visible(object->component, TRUE);

            switch(object->type) {
            case ekOBJ_LABEL:
            case ekOBJ_IMAGE:
                break;
            case ekOBJ_BUTTON:
                /* Buttons don't have tabstop
                   _component_taborder(object->component, window); */
                break;
            case ekOBJ_MENU:
                nap_menuvert_taborder((Panel*)object->component, window);
                break;
            case ekOBJ_TABLEVIEW:
            case ekOBJ_TEXTVIEW:
            case ekOBJ_EDIT:
                _component_taborder(object->component, window);
                break;
            cassert_default();
            }
        }
    arrpt_end();
}

/*---------------------------------------------------------------------------*/

static void i_toolbar_tabstop(GtNapToolbar *toolbar)
{
    if (toolbar != NULL)
    {
        arrpt_foreach(button, toolbar->buttons, Button)
            if (button != NULL)
                _component_visible((GuiComponent*)button, TRUE);
        arrpt_end();
    }
}

/*---------------------------------------------------------------------------*/

static void i_OnPreviousTabstop(GtNapWindow *cuawin, Event *e)
{
    unref(e);
    cassert_no_null(cuawin);
    cuawin->focus_by_previous = TRUE;
    window_previous_tabstop(cuawin->window);
}

/*---------------------------------------------------------------------------*/

static void i_OnNextTabstop(GtNapWindow *cuawin, Event *e)
{
    unref(e);
    cassert_no_null(cuawin);
    cuawin->focus_by_previous = FALSE;
    window_next_tabstop(cuawin->window);
}

/*---------------------------------------------------------------------------*/

static void i_OnLeftButton(GtNapWindow *cuawin, Event *e)
{
    unref(e);
    cassert_no_null(cuawin);
    if (cuawin->default_button > 0)
    {
        cuawin->default_button -= 1;
        i_set_defbutton(cuawin);
    }
}

/*---------------------------------------------------------------------------*/

static void i_OnRightButton(GtNapWindow *cuawin, Event *e)
{
    unref(e);
    cassert_no_null(cuawin);
    if (cuawin->default_button < i_num_buttons(cuawin) - 1)
    {
        cuawin->default_button += 1;
        i_set_defbutton(cuawin);
    }
}

/*---------------------------------------------------------------------------*/

/* TO BE REMOVED!!! */
static GtNapObject *i_cualib_obj(ArrPt(GtNapObject) *objects, const GuiComponent *component)
{
    arrpt_foreach(obj, objects, GtNapObject)
        if (obj->component == component)
            return obj;
    arrpt_end();
    return NULL;
}

/*---------------------------------------------------------------------------*/

/* Run the codeBlock that updates after a text entry in EditBox */
static void i_update_harbour_from_edit_text(const GtNapObject *obj)
{
    cassert_no_null(obj);
    if (obj->get_set_block != NULL)
    {
        PHB_ITEM pItem = NULL;
        const char_t *text = edit_get_text((Edit*)obj->component);

        if (obj->dtype == ekTYPE_CHARACTER)
        {
            String *u1252 = gtconvert_UTF8_to_1252(text);
            pItem = hb_itemPutC(NULL, tc(u1252));
            str_destroy(&u1252);
        }
        else if (obj->dtype == ekTYPE_DATE)
        {
            pItem = hb_itemPutDS(NULL, text);
        }
        else
        {
            cassert_msg(FALSE, "Unknown data type in i_update_harbour_from_edit_text");
        }

        if (pItem != NULL)
        {
            PHB_ITEM retItem = hb_itemDo(obj->get_set_block, 1, pItem);
            hb_itemRelease(pItem);
            hb_itemRelease(retItem);
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
        label_text((Label*)obj->component, utf8_text);
    }
    else if (obj->text_block != NULL)
    {
        PHB_ITEM retItem = hb_itemDo(obj->text_block, 0);
        HB_TYPE type = HB_ITEM_TYPE(retItem);

        if (type == HB_IT_STRING)
        {
            char_t buffer[STATIC_TEXT_SIZE];
            i_item_to_utf8(retItem, buffer, sizeof32(buffer));
            nchars = unicode_nchars(buffer, ekUTF8);
            label_text((Label*)obj->component, buffer);
        }

        hb_itemRelease(retItem);
    }

    /* Text has been updated */
    if (nchars != UINT32_MAX)
    {
        obj->size.width = (real32_t)(nchars * GTNAP_GLOBAL->cell_x_size);
        if (obj->cuawin->is_configured == TRUE)
            _component_set_frame(obj->component, &obj->pos, &obj->size);
    }
}

/*---------------------------------------------------------------------------*/

static void i_OnEditChange(GtNapWindow *cuawin, Event *e)
{
    const EvText *p = event_params(e, EvText);
    GtNapObject *cuaobj = NULL;
    cassert_no_null(cuawin);

    if (cuawin->modal_window_alive == FALSE)
    {
        log_printf("i_OnEditChange: Modal Window: %p is not alive", cuawin);
        return;
    }

    /* Get the EditBox that launched the event */
    /* TODO: Change cuawin parameter with gtobj */
    cuaobj = i_cualib_obj(cuawin->gui_objects, (GuiComponent*)event_sender(e, Edit));
    cassert(cuaobj->type == ekOBJ_EDIT);

    /* Update Harbour with the content of the EditBox */
    i_update_harbour_from_edit_text(cuaobj);

    /* The editbox has a validation code block */
    if (cuaobj->valida_block != NULL)
    {
        bool_t valid = TRUE;
        PHB_ITEM retItem = hb_itemDo(cuaobj->valida_block, 0 /*,1, cuaobj->getobjItem*/);
        HB_TYPE type = HB_ITEM_TYPE(retItem);
        cassert(type == HB_IT_LOGICAL);
        valid = (bool_t)hb_itemGetL(retItem);
        hb_itemRelease(retItem);

        /* If the input is not valid --> The editbox keep the focus and event finish here */
        if (valid == FALSE)
        {
            bool_t *r = event_result(e, bool_t);
            *r = FALSE;
            return;
        }
    }

    /* The window has a global function to process invalid date */
    if (cuaobj->dtype == ekTYPE_DATE)
    {
        if (cuawin->error_date_block != NULL)
        {
            long r = hb_dateUnformat( p->text, hb_setGetDateFormat());
            log_printf("DATE processing result: %d", r);

            /* Date invalid --> The editbox keep the focus and event finish here */
            if (r == 0)
            {
                PHB_ITEM retItem = NULL;
                bool_t *r = event_result(e, bool_t);
                retItem = hb_itemDo(cuawin->error_date_block, 0);
                hb_itemRelease(retItem);
                *r = FALSE;
                return;
            }
        }
    }

    // TODO: DELETE
    if (cuaobj->is_last_edit == TRUE)
        log_printf("Entering i_OnEditChange LAST_EDIT: %p", cuaobj);
    else
        log_printf("Entering i_OnEditChange: %p", cuaobj);

    /* Update possible labels associated with this input */
    arrpt_foreach(obj, cuawin->gui_objects, GtNapObject)
        if (obj->type == ekOBJ_LABEL)
            i_set_label_text(obj, NULL);
    arrpt_end();


    /* If user have pressed the [ESC] key, we left the stop for that event */
    if (cuawin->is_closed_by_esc == FALSE)
    {
        /* The last editbox has lost the focus --> Close the window */
        if (cuaobj->is_last_edit == TRUE)
        {
            /* We dont execute confirmaCodeBlock if we moved from last edit to previos one */
            if (cuawin->focus_by_previous == FALSE)
            {
                bool_t close = TRUE;

                /* We have asociated a confirmation block */
                if (cuawin->confirm_block != NULL)
                {
                    PHB_ITEM retItem = hb_itemDo(cuawin->confirm_block, 0);
                    HB_TYPE type = HB_ITEM_TYPE(retItem);
                    cassert(type == HB_IT_LOGICAL);
                    close = (bool_t)hb_itemGetL(retItem);
                    hb_itemRelease(retItem);
                }

                if (close == TRUE)
                {
                    log_printf("--> STOP CUALIB Modal Window: %p 'i_OnEditChange'", cuawin->window);
                    cuawin->modal_window_alive = FALSE;
                    window_stop_modal(cuawin->window, 5000);
                }
            }
        }
    }



    // }

//cuaobj->validaCodeBlock
    // if (cuaobj->is_on_edit_change == TRUE)
    // {
    //     log_printf("Object: %p  is_on_edit_change", cuaobj);
    //     return;
    // }

    // if (cuaobj->is_last_edit == TRUE && cuawin->tabstop_by_return_or_arrow == FALSE)
    // {
    //         log_printf("Modal Window: %p  tabstop_by_return_or_arrow FALSE", cuawin);
    //     return;

    // }

    //cuaobj->is_on_edit_change = TRUE;
    cuawin->focus_by_previous = FALSE;
    //cuaobj->is_on_edit_change = FALSE;
    //cuawin->tabstop_by_return_or_arrow = FALSE;
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
        PHB_ITEM retItem = hb_itemDo(obj->get_set_block, 0);
        HB_TYPE type = HB_ITEM_TYPE(retItem);
        switch (type) {
        case HB_IT_STRING:
            cassert(obj->dtype == ekTYPE_CHARACTER);
            hb_itemCopyStrUTF8(retItem, (char*)utf8, (HB_SIZE)size);
            break;

        case HB_IT_DATE:
        {
            char date[16];
            char temp[16];
            cassert(obj->dtype == ekTYPE_DATE);
            hb_itemGetDS(retItem, date);
            hb_dateFormat(date, temp, hb_setGetDateFormat());
            str_copy_c(utf8, size, temp);
            break;
        }

        default:
            str_copy_c(utf8, size, "");
        }

        hb_itemRelease(retItem);
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
        for(;;)
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
            *((char_t*)d) = '\0';

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
    log_printf("Filter Date Cursor pos: %d", cpos);
    for(;;)
    {
        uint32_t nbf;
        bool_t sep = FALSE;

        /* Current character of format string */
        uint32_t f = unicode_to_u32b(format, ekUTF8, &nbf);

        /* End of format string --> bye */
        if (f == 0)
            break;

        /* Digit position */
        if(f == 'd' || f == 'D' || f == 'm' || f == 'M' || f == 'y' || f == 'Y')
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

static void i_filter_tecla(const GtNapObject *obj, const EvText *text, EvTextFilter *filter)
{
    bool_t updated = FALSE;
    cassert_no_null(obj);
    cassert_no_null(text);
    cassert_no_null(filter);
    cassert(obj->type == ekOBJ_EDIT);
    /* Some text has been inserted */
    if (text->len > 0)
    {
        /* We have a filter */
        if (obj->keyfilter_block != NULL)
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
                    // From Unicode (NappGUI) to 1252 (Cualib)
                    uint8_t cp2 = gtconvert_UTF8_to_1252_char(c);
                    uint32_t nb2, ncp;

                    // Set character as lastKey
                    hb_inkeySetLast(cp2);

                    // Call to filter
                    {
                        PHB_ITEM retItem = hb_itemDo(obj->keyfilter_block, 0);
                        HB_TYPE type = HB_ITEM_TYPE(retItem);
                        if (type == HB_IT_NIL)
                        {
                            ncp = c;
                            nb2 = nb;
                        }
                        else
                        {
                            char_t temp[1024];
                            cassert(type == HB_IT_STRING);
                            hb_itemCopyStrUTF8(retItem, temp, sizeof(temp));
                            cassert(unicode_nchars(temp, ekUTF8) == 1);
                            ncp = unicode_to_u32b(temp, ekUTF8, &nb2);
                        }

                        hb_itemRelease(retItem);
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

static void i_OnEditFilter(GtNapWindow *cuawin, Event *e)
{
    Edit *edit = event_sender(e, Edit);
    GtNapObject *cuaobj = NULL;
    cassert_no_null(cuawin);

    // FRAN: IMPROVE
    arrpt_foreach(obj, cuawin->gui_objects, GtNapObject)
        if (obj->type == ekOBJ_EDIT)
        {
            if (edit == (Edit*)obj->component)
            {
                cuaobj = obj;
                break;
            }
        }
    arrpt_end();

    if (cuaobj != NULL)
    {
        const EvText *p = event_params(e, EvText);
        EvTextFilter *res = event_result(e, EvTextFilter);

        // // FRAN: TODO
        // // This block must be move to edit_OnFocus()
        // if (cuawin->message_label_id != UINT32_MAX)
        // {
        //     GtNapObject *mes_obj = arrst_get(cuawin->gui_objects, cuawin->message_label_id, GtNapObject);
        //     i_set_edit_message(cuaobj, mes_obj);
        // }

        if (i_is_editable(cuawin, cuaobj) == FALSE)
        {
            /* If editBox is not editable --> Restore the original text */
            i_get_edit_text(cuaobj, res->text, sizeof(res->text));
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
            if (cuaobj->dtype == ekTYPE_DATE)
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
                    //cassert(filTec.cpos == p->cpos);
                    tf.text = fil1.text;
                    tf.cpos = fil1.cpos;
                    tf.len = len;
                    i_filter_overwrite(&tf, &fil2, GTNAP_GLOBAL->date_digits);
                }

                cassert(fil2.apply == TRUE);

                {
                    EvText tf;
                    //cassert(filTec.cpos == p->cpos);
                    tf.text = fil2.text;
                    tf.cpos = fil2.cpos;
                    tf.len = 0;
                    log_printf("Before filter date cpos: %d", tf.cpos);
                    //i_filter_bypass(&tf, res);
                    i_filter_date(&tf, res, hb_setGetDateFormat(), p->len >= 0);
                }

                cassert(res->apply == TRUE);

                //i_filter_date(p, res);
                //log_printf("Date CPOS: %d", res->cpos);

                if (res->cpos == 10)
                    gui_OnIdle(listener(cuawin, i_OnNextTabstop, GtNapWindow));
            }
            else
            {
                EvTextFilter filTec;
                filTec.apply = FALSE;
                i_filter_tecla(cuaobj, p, &filTec);
                cassert(filTec.apply == TRUE);

                {
                    EvText tf;
                    cassert(filTec.cpos == p->cpos);
                    tf.text = filTec.text;
                    tf.cpos = filTec.cpos;
                    tf.len = p->len;
                    i_filter_overwrite(&tf, res, cuaobj->editSize);
                }

                cassert(res->apply == TRUE);

                /* End of editable string reached. */
                if (res->cpos >= cuaobj->editSize)
                {
                    cuawin->focus_by_previous = FALSE;
                    //cuawin->tabstop_by_return_or_arrow = TRUE;
                    gui_OnIdle(listener(cuawin, i_OnNextTabstop, GtNapWindow));
                }
            }
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
        PHB_ITEM retItem = hb_itemDo(obj->message_block, 0);
        HB_TYPE type = HB_ITEM_TYPE(retItem);

        if (type == HB_IT_STRING)
        {
            char_t buffer[1024];
            uint32_t len;
            hb_itemCopyStrUTF8( retItem, (char*)buffer, (HB_SIZE)sizeof(buffer));
            len = unicode_nchars(buffer, ekUTF8);
            mes_obj->size.width = (real32_t)(len * GTNAP_GLOBAL->cell_x_size);
            _component_set_frame(mes_obj->component, &mes_obj->pos, &mes_obj->size);
            label_text((Label*)mes_obj->component, buffer);
        }
        else
        {
            cassert_msg(FALSE, "Unkown type in i_set_edit_message");
        }

        hb_itemRelease(retItem);
    }
}

/*---------------------------------------------------------------------------*/

static void i_set_edit_text(const GtNapObject *obj)
{
    char_t buffer[STATIC_TEXT_SIZE];
    cassert_no_null(obj);
    i_get_edit_text(obj, buffer, sizeof(buffer));
    edit_text((Edit*)obj->component, buffer);
}

/*---------------------------------------------------------------------------*/

static void i_launch_wizard(GtNapWindow *cuawin, GtNapObject *obj)
{
    char_t temp[1024];
    PHB_ITEM retItem = NULL;
    HB_TYPE type = HB_IT_NIL;
    cassert_no_null(cuawin);
    cassert_no_null(obj);
    cassert(obj->type == ekOBJ_EDIT);
    cassert_no_null(obj->wizard_block);
    retItem = hb_itemDo(obj->wizard_block, 0);
    type = HB_ITEM_TYPE(retItem);

    if (type != HB_IT_NIL)
    {
        cassert(type == HB_IT_STRING);
        hb_itemCopyStrUTF8(retItem, temp, sizeof(temp));
        edit_text((Edit*)obj->component, temp);
    }

    hb_itemRelease(retItem);

    if (type != HB_IT_NIL)
        gui_OnIdle(listener(cuawin, i_OnNextTabstop, GtNapWindow));
}

/*---------------------------------------------------------------------------*/

static void i_OnAutoWizard(GtNapWindow *cuawin, Event *e)
{
    GtNapObject *cuaobj = NULL;
    cassert_no_null(cuawin);
    cuaobj = cuawin->current_obj;
    cassert_no_null(cuaobj);
    unref(e);

    // log_printf("cuaobj->autoCodeBlock: %p", cuaobj->autoCodeBlock);
    if (cuaobj->can_auto_lista == TRUE && cuaobj->auto_block != NULL && cuaobj->wizard_block != NULL)
    {
        bool_t lista = FALSE;

        {
            PHB_ITEM retItem = hb_itemDo(cuaobj->auto_block, 0);
            HB_TYPE type = HB_ITEM_TYPE(retItem);
            cassert(type == HB_IT_LOGICAL);
            lista = (bool_t)hb_itemGetL(retItem);
            hb_itemRelease(retItem);
        }

        if (lista == TRUE)
        {
            cuaobj->can_auto_lista = FALSE;
            i_launch_wizard(cuawin, cuaobj);
        }
    }
    // --------------------------------
}

/*---------------------------------------------------------------------------*/

static void i_OnEditFocus(GtNapWindow *cuawin, Event *e)
{
    Edit *edit = event_sender(e, Edit);
    const bool_t *p = event_params(e, bool_t);
    GtNapObject *cuaobj = NULL;
    cassert_no_null(cuawin);

    // FRAN: IMPROVE
    arrpt_foreach(obj, cuawin->gui_objects, GtNapObject)
        if (obj->type == ekOBJ_EDIT)
        {
            if (edit == (Edit*)obj->component)
            {
                cuaobj = obj;
                obj->has_focus = TRUE;
            }
            else
            {
                /* An edit can't lauch lista auto twice if focus doen't change to other editbox */
                /* Avoid burst lists */
                obj->can_auto_lista = TRUE;
                obj->has_focus = FALSE;
            }
        }
    arrpt_end();

    if (*p == TRUE)
    {
        if (cuawin->message_label_id != UINT32_MAX)
        {
            if (cuaobj != NULL)
            {
                GtNapObject *mes_obj = arrpt_get(cuawin->gui_objects, cuawin->message_label_id, GtNapObject);
                i_set_edit_message(cuaobj, mes_obj);
            }
        }

        if (cuaobj->when_block != NULL)
        {
            PHB_ITEM retItem = hb_itemDo(cuaobj->when_block, 0);
            HB_TYPE type = HB_ITEM_TYPE(retItem);
            bool_t updated = FALSE;
            log_printf("After WHEN Type: %d", type);
            cassert(type == HB_IT_LOGICAL);
            updated = (bool_t)hb_itemGetL(retItem);
            hb_itemRelease(retItem);

            if (updated == TRUE)
                i_set_edit_text(cuaobj);
        }

        cuawin->current_obj = cuaobj;

        edit_select(edit, 0, 0);
        gui_OnIdle(listener(cuawin, i_OnAutoWizard, GtNapWindow));
    }
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

    gtwin->panel = panel_create();

    if (gtwin->toolbar != NULL)
        gtwin->panel_size.height += (real32_t)GTNAP_GLOBAL->cell_y_size;

    panel_size(gtwin->panel, gtwin->panel_size);
    panel_layout(gtwin->panel, layout);

    /* Create the view canvas*/
    cassert(gtwin->canvas == NULL);
    gtwin->canvas = view_create();
    view_OnDraw(gtwin->canvas, listener(gtwin, i_OnCanvasDraw, GtNapWindow));
    _component_attach_to_panel((GuiComponent*)gtwin->panel, (GuiComponent*)gtwin->canvas);
    _component_set_frame((GuiComponent*)gtwin->canvas, &kV2D_ZEROf, &gtwin->panel_size);
    _component_visible((GuiComponent*)gtwin->canvas, TRUE);

    if (i_with_scroll_panel(gtwin) == TRUE)
    {
        /* We add a subpanel to window main panel to implement the scroll area */
        Panel *panel = panel_scroll(FALSE, TRUE);
        S2Df csize = i_scroll_content_size(gtwin->gui_objects);
        int32_t cell_x = gtwin->scroll_left - gtwin->left;
        int32_t cell_y = gtwin->scroll_top - gtwin->top;
        real32_t pos_x = (real32_t)(cell_x * GTNAP_GLOBAL->cell_x_size);
        real32_t pos_y = (real32_t)(cell_y * GTNAP_GLOBAL->cell_y_size);
        real32_t width = (real32_t)((gtwin->scroll_right - gtwin->scroll_left + 3) * GTNAP_GLOBAL->cell_x_size);
        real32_t height = (real32_t)((gtwin->scroll_bottom - gtwin->scroll_top + 1) * GTNAP_GLOBAL->cell_y_size);
        V2Df pos = v2df(pos_x, pos_y);
        S2Df size = s2df(width, height);
        _component_attach_to_panel((GuiComponent*)gtwin->panel, (GuiComponent*)panel);
        _component_set_frame((GuiComponent*)panel, &pos, &size);
        _component_visible((GuiComponent*)panel, FALSE);
        _panel_content_size(panel, csize.width, csize.height);
        offset.x = -pos.x;
        offset.y = -pos.y;
        scroll_panel = panel;
        gtwin->scrolled_panel = panel;
    }

    /* Attach gui objects in certain Z-Order (from back to front) */
    i_attach_to_panel(gtwin->gui_objects, gtwin->panel, scroll_panel, &offset, ekOBJ_MENU, gtwin->toolbar);
    i_attach_to_panel(gtwin->gui_objects, gtwin->panel, scroll_panel, &offset, ekOBJ_TABLEVIEW, gtwin->toolbar);
    i_attach_to_panel(gtwin->gui_objects, gtwin->panel, scroll_panel, &offset, ekOBJ_TEXTVIEW, gtwin->toolbar);
    i_attach_to_panel(gtwin->gui_objects, gtwin->panel, scroll_panel, &offset, ekOBJ_LABEL, gtwin->toolbar);
    i_attach_to_panel(gtwin->gui_objects, gtwin->panel, scroll_panel, &offset, ekOBJ_BUTTON, gtwin->toolbar);
    i_attach_to_panel(gtwin->gui_objects, gtwin->panel, scroll_panel, &offset, ekOBJ_EDIT, gtwin->toolbar);
    i_attach_to_panel(gtwin->gui_objects, gtwin->panel, scroll_panel, &offset, ekOBJ_IMAGE, gtwin->toolbar);
    i_attach_toolbar_to_panel(gtwin->toolbar, gtwin->panel);

    if (scroll_panel != NULL)
        _component_visible((GuiComponent*)scroll_panel, TRUE);

    /* We are in a main (not embedded) window */
    if (gtwin->window != NULL)
    {
        cassert(gtwin == main_gtwin);
        window_panel(gtwin->window, gtwin->panel);

        /* Begin tab-stops order */
        _window_taborder(gtwin->window, NULL);
    }

    i_component_tabstop(gtwin->gui_objects, main_gtwin->window, ekOBJ_MENU);
    i_component_tabstop(gtwin->gui_objects, main_gtwin->window, ekOBJ_TABLEVIEW);
    i_component_tabstop(gtwin->gui_objects, main_gtwin->window, ekOBJ_TEXTVIEW);
    i_component_tabstop(gtwin->gui_objects, main_gtwin->window, ekOBJ_EDIT);
    i_component_tabstop(gtwin->gui_objects, main_gtwin->window, ekOBJ_BUTTON);
    i_component_tabstop(gtwin->gui_objects, main_gtwin->window, ekOBJ_LABEL);
    i_component_tabstop(gtwin->gui_objects, main_gtwin->window, ekOBJ_IMAGE);

    if (gtwin->window != NULL)
    {
        i_toolbar_tabstop(gtwin->toolbar);
    }
    else
    {
        /* Toolbar is not allowed in embedded windows */
        cassert(gtwin->toolbar == NULL);
    }

    /* Configure the child (embedded) windows as subpanels */
    if (gtwin->parent_id == UINT32_MAX)
    {
        arrst_forback(embgtwin, gtnap->windows, GtNapWindow)
            if (embgtwin->parent_id == gtwin->id)
            {
                V2Df pos;
                int32_t cell_x = embgtwin->left - gtwin->left;
                int32_t cell_y = embgtwin->top - gtwin->top;
                pos.x = (real32_t)(cell_x * GTNAP_GLOBAL->cell_x_size);
                pos.y = (real32_t)(cell_y * GTNAP_GLOBAL->cell_y_size);
                i_gtwin_configure(gtnap, embgtwin, gtwin);
                _component_attach_to_panel((GuiComponent*)gtwin->panel, (GuiComponent*)embgtwin->panel);
                _component_set_frame((GuiComponent*)embgtwin->panel, &pos, &embgtwin->panel_size);
                _component_visible((GuiComponent*)embgtwin->panel, TRUE);
            }
        arrst_end();
    }

    /* Allow navigation between edit controls with arrows and return */
    if (i_num_edits(gtwin) > 1)
    {
        /* At the moment, embedded windows with edits is not allowed */
        GtNapObject *last_edit = NULL;

        arrpt_foreach(obj, gtwin->gui_objects, GtNapObject)
            if (obj->type == ekOBJ_EDIT)
            {
                edit_OnChange((Edit*)obj->component, listener(gtwin, i_OnEditChange, GtNapWindow));
                edit_OnFilter((Edit*)obj->component, listener(gtwin, i_OnEditFilter, GtNapWindow));
                edit_OnFocus((Edit*)obj->component, listener(gtwin, i_OnEditFocus, GtNapWindow));
                obj->is_last_edit = FALSE;
                last_edit = obj;
            }
        arrpt_end();

        cassert_no_null(last_edit);
        last_edit->is_last_edit = TRUE;
    }

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

void hb_gtnap_init(const char_t *title, const uint32_t rows, const uint32_t cols, PHB_ITEM begin_block)
{
    void *hInstance = NULL;

#if defined( HB_OS_WIN )
    hb_winmainArgGet(&hInstance, NULL, NULL);
#endif

    str_copy_c(INIT_TITLE, sizeof32(INIT_TITLE), title);
    INIT_CODEBLOCK = hb_itemNew(begin_block);
    INIT_ROWS = rows;
    INIT_COLS = cols;

    osmain_imp(
                0, NULL, hInstance, 0.,
                (FPtr_app_create)i_gtnap_create,
                (FPtr_app_update)NULL,
                (FPtr_destroy)i_gtnap_destroy,
                (char_t*)"");
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
    return GTNAP_GLOBAL->cell_x_size * GTNAP_GLOBAL->cols;
}

/*---------------------------------------------------------------------------*/

uint32_t hb_gtnap_height(void)
{
    cassert_no_null(GTNAP_GLOBAL);
    return GTNAP_GLOBAL->cell_y_size * GTNAP_GLOBAL->rows;
}

/*---------------------------------------------------------------------------*/

void hb_gtnap_terminal(void)
{
    GtNap *gtnap = GTNAP_GLOBAL;
    GtNapWindow *gtwin = NULL;
    cassert(arrst_size(gtnap->windows, GtNapWindow) == 0);
    hb_gtnap_window(0, 0, gtnap->rows - 1, gtnap->cols - 1, tc(gtnap->title), FALSE, TRUE, TRUE, FALSE);
    gtwin = i_current_gtwin(gtnap);
    i_gtwin_configure(gtnap, gtwin, gtwin);

    {
        uint32_t cwidth = gtwin->right - gtwin->left + 1;
        uint32_t cheight = gtwin->bottom - gtwin->top + 1;
        cassert(gtwin->text_buffer == NULL);
        gtwin->text_buffer = (uint16_t*)heap_malloc(cwidth * cheight * sizeof(uint16_t), "gtwin_textbuffer");
        bmem_set_zero((byte_t*)gtwin->text_buffer, cwidth * cheight * sizeof(uint16_t));
    }

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

static __INLINE uint32_t i_window_flags(const bool_t close_return, const bool_t close_esc, const bool_t minimize_button)
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
    while(!found)
    {
        bool_t valid_id = TRUE;
        arrst_foreach_const(gtwin, gtnap->windows, GtNapWindow)
            if (gtwin->id == id)
            {
                id += 1;
                valid_id = FALSE;
                break;
            }
        arrst_end();

        if (valid_id == TRUE)
            found = TRUE;
    }

    return id;
}

/*---------------------------------------------------------------------------*/

static GtNapWindow *i_new_window(GtNap *gtnap, uint32_t parent_id, const int32_t top, const int32_t left, const int32_t bottom, const int32_t right)
{
    GtNapWindow *gtwin = NULL;
    cassert_no_null(gtnap);
    gtwin = arrst_new0(gtnap->windows, GtNapWindow);
    gtwin->id = i_get_window_id(gtnap);
    gtwin->parent_id = parent_id;
    gtwin->top = top;
    gtwin->left = left;
    gtwin->bottom = bottom;
    gtwin->right = right;
    gtwin->scroll_top = INT32_MIN;
    gtwin->scroll_left = INT32_MIN;
    gtwin->scroll_bottom = INT32_MIN;
    gtwin->scroll_right = INT32_MIN;
    gtwin->message_label_id = UINT32_MAX;
    gtwin->default_button = UINT32_MAX;
    gtwin->gui_objects = arrpt_create(GtNapObject);
    gtwin->callbacks = arrpt_create(GtNapCallback);
    gtwin->panel_size.width = (real32_t)(gtnap->cell_x_size * (gtwin->right - gtwin->left + 1));
    gtwin->panel_size.height = (real32_t)(gtnap->cell_y_size * (gtwin->bottom - gtwin->top + 1));
    return gtwin;
}

/*---------------------------------------------------------------------------*/

uint32_t hb_gtnap_window(const int32_t top, const int32_t left, const int32_t bottom, const int32_t right, const char_t *title, const bool_t close_return, const bool_t close_esc, const bool_t minimize_button, const bool_t buttons_navigation)
{
    GtNapWindow *gtwin = i_new_window(GTNAP_GLOBAL, UINT32_MAX, top, left, bottom, right);
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

uint32_t hb_gtnap_window_embedded(const uint32_t wid, const int32_t top, const int32_t left, const int32_t bottom, const int32_t right)
{
    GtNapWindow *gtwin = i_new_window(GTNAP_GLOBAL, wid, top, left, bottom, right);
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
            arrst_foreach(embgtwin, GTNAP_GLOBAL->windows, GtNapWindow)
                if (embgtwin->parent_id == gtwin->id)
                {
                    cassert(embgtwin->is_configured == TRUE);
                    _component_visible((GuiComponent*)embgtwin->panel, FALSE);
                    _component_detach_from_panel((GuiComponent*)gtwin->panel, (GuiComponent*)embgtwin->panel);
                }
            arrst_end()
        }
        /* We are in an embedded window --> Dettach from ONLY one parent */
        else
        {
            arrst_foreach(maingtwin, GTNAP_GLOBAL->windows, GtNapWindow)
                if (gtwin->parent_id == maingtwin->id)
                {
                    cassert(maingtwin->is_configured == TRUE);
                    _component_visible((GuiComponent*)gtwin->panel, FALSE);
                    _component_detach_from_panel((GuiComponent*)maingtwin->panel, (GuiComponent*)gtwin->panel);
                    break;
                }
            arrst_end()
        }
    }
}

/*---------------------------------------------------------------------------*/

void hb_gtnap_window_destroy(const uint32_t wid)
{
    uint32_t id = i_gtwin_index(GTNAP_GLOBAL, wid);
    GtNapWindow *gtwin = arrst_get(GTNAP_GLOBAL->windows, id, GtNapWindow);
    /* Before destroy we have to dettach the possible parent-embedded connections */
    i_dettach_embedded(GTNAP_GLOBAL, gtwin);
    arrst_delete(GTNAP_GLOBAL->windows, id, i_remove_gtwin, GtNapWindow);
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
    {
        callback->gtwin->modal_window_alive = FALSE;
        window_stop_modal(callback->gtwin->window, NAP_MODAL_HOTKEY_AUTOCLOSE + callback->autoclose_id);
    }
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
            uint32_t autoclose_id = autoclose ? nkey->vkey : UINT32_MAX;
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

static GtNapObject *i_get_first_focus(GtNapWindow *gtwin)
{
    cassert_no_null(gtwin);
    arrpt_foreach(gtobj, gtwin->gui_objects, GtNapObject)
        switch(gtobj->type) {
        case ekOBJ_EDIT:
        case ekOBJ_TABLEVIEW:
        case ekOBJ_TEXTVIEW:
            return gtobj;
        case ekOBJ_LABEL:
        case ekOBJ_BUTTON:
        case ekOBJ_MENU:
        case ekOBJ_IMAGE:
            break;
        }
    arrpt_end();

    return NULL;
}

/*---------------------------------------------------------------------------*/

uint32_t hb_gtnap_window_modal(const uint32_t wid)
{
    GtNapWindow *gtwin = i_gtwin(GTNAP_GLOBAL, wid);
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
        GtNapWindow *parent = i_parent_cuawin(GTNAP_GLOBAL);
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

        /* Launch the window */

        if (parent == NULL)
        {
            cassert(arrst_size(GTNAP_GLOBAL->windows, GtNapWindow) == 1);
        }

        pos.x = (real32_t)(gtwin->left * GTNAP_GLOBAL->cell_x_size);
        pos.y = (real32_t)(gtwin->top * GTNAP_GLOBAL->cell_y_size);

        if (parent != NULL)
        {
            const GtNapWindow *rootwin = arrst_get_const(GTNAP_GLOBAL->windows, 0, GtNapWindow);
            V2Df ppos = window_get_origin(rootwin->window);
            pos.x += ppos.x /*- 2 * GTNAP_GLOBAL->cell_x_size*/;
            pos.y += ppos.y;

            if (gtwin->toolbar != NULL)
                pos.y -= (real32_t)(GTNAP_GLOBAL->cell_y_size - (gtwin->toolbar->pixels_button - GTNAP_GLOBAL->cell_y_size));
        }

        /* Check if a first control has to be focused */
        {
            GtNapObject *gtobj_focus = NULL;

            if (embgtwin != NULL)
                gtobj_focus = i_get_first_focus(embgtwin);
            else if (i_gtwin_has_embedded(GTNAP_GLOBAL, gtwin->id) == TRUE)
                gtobj_focus = i_get_first_focus(gtwin);

            if (gtobj_focus != NULL)
                _window_focus(gtwin->window, gtobj_focus->component);
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
            window_hotkey(gtwin->window, ekKEY_UP, 0, listener(gtwin, i_OnPreviousTabstop, GtNapWindow));
            window_hotkey(gtwin->window, ekKEY_DOWN, 0, listener(gtwin, i_OnNextTabstop, GtNapWindow));
            window_hotkey(gtwin->window, ekKEY_RETURN, 0, listener(gtwin, i_OnNextTabstop, GtNapWindow));
        }

        window_origin(gtwin->window, pos);
        gtwin->is_closed_by_esc = FALSE;
        gtwin->modal_window_alive = TRUE;
        ret = window_modal(gtwin->window, parent ? parent->window : NULL);
        return ret;
    }
}

/*---------------------------------------------------------------------------*/

void hb_gtnap_window_stop_modal(const uint32_t result)
{
    GtNapWindow *gtwin = i_current_main_gtwin(GTNAP_GLOBAL);
    if (gtwin != NULL)
    {
        gtwin->modal_window_alive = FALSE;
        window_stop_modal(gtwin->window, result);
    }
}

/*---------------------------------------------------------------------------*/

static uint32_t i_add_object(const objtype_t type, const int32_t top, const int32_t left, const uint32_t cell_x_size, const uint32_t cell_y_size, const S2Df *size, const bool_t in_scroll, GuiComponent *component, GtNapWindow *gtwin)
{
    uint32_t id;
    GtNapObject *obj = NULL;
    cassert_no_null(gtwin);
    cassert_no_null(size);
    id = arrpt_size(gtwin->gui_objects, GtNapObject);
    obj = heap_new0(GtNapObject);
    arrpt_append(gtwin->gui_objects, obj, GtNapObject);
    obj->type = type;
    obj->top = top;
    obj->left = left;
    obj->component = component;
    obj->pos.x = (real32_t)(left * (int32_t)cell_x_size);
    obj->pos.y = (real32_t)(top * (int32_t)cell_y_size);
    obj->size = *size;
    obj->is_last_edit = FALSE;
    obj->in_scroll = in_scroll;
    obj->can_auto_lista = TRUE;
    obj->has_focus = FALSE;
    obj->editBoxIndexForButton = UINT32_MAX;
    obj->confirmaCodeBlock = NULL;
    obj->getobjItem = NULL;
    obj->cuawin = gtwin;
    return id;
}

/*---------------------------------------------------------------------------*/

static uint32_t i_add_label(const int32_t top, const int32_t left, const bool_t in_scroll, GtNapWindow *gtwin, GtNap *gtnap)
{
    Label *label = label_create();
    S2Df size;
    cassert_no_null(gtnap);
    label_font(label, gtnap->global_font);
    size.width = (real32_t)(1 * gtnap->cell_x_size);
    size.height = (real32_t)gtnap->label_y_size;
    return i_add_object(ekOBJ_LABEL, top, left, gtnap->cell_x_size, gtnap->cell_y_size, &size, in_scroll, (GuiComponent*)label, gtwin);
}

/*---------------------------------------------------------------------------*/

uint32_t hb_gtnap_label(const uint32_t wid, const int32_t top, const int32_t left, HB_ITEM *text_block, const bool_t in_scroll)
{
    GtNapWindow *gtwin = i_gtwin(GTNAP_GLOBAL, wid);
    uint32_t id;
    GtNapObject *obj;
    cassert(gtwin->is_configured == FALSE);
    id = i_add_label(top - gtwin->top, left - gtwin->left, in_scroll, gtwin, GTNAP_GLOBAL);
    obj = arrpt_last(gtwin->gui_objects, GtNapObject);
    cassert_no_null(obj);
    cassert(obj->type == ekOBJ_LABEL);

    if (text_block != NULL)
        obj->text_block = hb_itemNew(text_block);

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

    /* TODO: Remove (just for debug) */
    //{
    //    GtNapObject *obj = arrpt_last(gtwin->gui_objects, GtNapObject);
    //    i_set_label_text(obj, "--MENS--");
    //}

    return gtwin->message_label_id;
}

/*---------------------------------------------------------------------------*/

void hb_gtnap_label_fgcolor(const uint32_t wid, const uint32_t id, const color_t color)
{
    GtNapObject *obj = i_gtobj(GTNAP_GLOBAL, wid, id);
    cassert_no_null(obj);
    cassert(obj->type == ekOBJ_LABEL);
    label_color((Label*)obj->component, color);
}

/*---------------------------------------------------------------------------*/

void hb_gtnap_label_bgcolor(const uint32_t wid, const uint32_t id, const color_t color)
{
    GtNapObject *obj = i_gtobj(GTNAP_GLOBAL, wid, id);
    cassert_no_null(obj);
    cassert(obj->type == ekOBJ_LABEL);
    label_bgcolor((Label*)obj->component, color);
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
    {
        callback->gtwin->modal_window_alive = FALSE;
        window_stop_modal(callback->gtwin->window, NAP_MODAL_BUTTON_AUTOCLOSE + callback->autoclose_id);
    }
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
        button_text((Button*)obj->component, utf8);
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
    button_font(button, GTNAP_GLOBAL->reduced_font);
    button_vpadding(button, i_button_vpadding());
    cassert(bottom == top);
    size.width = (real32_t)((right - left + 1) * GTNAP_GLOBAL->cell_x_size);
    size.height = (real32_t)((bottom - top + 1) * GTNAP_GLOBAL->button_y_size);
    id = i_add_object(ekOBJ_BUTTON, top - gtwin->top, left - gtwin->left, GTNAP_GLOBAL->cell_x_size, GTNAP_GLOBAL->cell_y_size, &size, in_scroll, (GuiComponent*)button, gtwin);

    if (text_block != NULL)
    {
        GtNapObject *obj = arrpt_last(gtwin->gui_objects, GtNapObject);
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
    {
        callback->gtwin->modal_window_alive = FALSE;
        window_stop_modal(callback->gtwin->window, NAP_MODAL_IMAGE_AUTOCLOSE + callback->autoclose_id);
    }
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
        view_OnClick((View*)view, listener);
        imageview_scale(view, ekGUI_SCALE_AUTO);
        size.width = (real32_t)((right - left + 1) * GTNAP_GLOBAL->cell_x_size);
        size.height = (real32_t)((bottom - top + 1) * GTNAP_GLOBAL->cell_y_size);
        imageview_image(view, image);
        image_destroy(&image);
        return i_add_object(ekOBJ_IMAGE, top - gtwin->top, left - gtwin->left, GTNAP_GLOBAL->cell_x_size, GTNAP_GLOBAL->cell_y_size, &size, in_scroll, (GuiComponent*)view, gtwin);
    }

    return UINT32_MAX;
}

/*---------------------------------------------------------------------------*/

uint32_t hb_gtnap_edit(const uint32_t wid, const int32_t top, const int32_t left, const uint32_t width, const char_t type, HB_ITEM *get_set_block, HB_ITEM *is_editable_block, HB_ITEM *when_block, HB_ITEM *valida_block, HB_ITEM *message_block, HB_ITEM *keyfilter_block, HB_ITEM *auto_block, HB_ITEM *wizard_block, const bool_t in_scroll)
{
    GtNapWindow *gtwin = i_gtwin(GTNAP_GLOBAL, wid);
    Edit *edit = edit_create();
    S2Df size;
    uint32_t id = UINT32_MAX;
    GtNapObject *obj = NULL;
    cassert_no_null(gtwin);
    edit_font(edit, GTNAP_GLOBAL->reduced_font);
    edit_vpadding(edit, i_edit_vpadding());
    edit_bgcolor_focus(edit, kCOLOR_CYAN);
    size.width = (real32_t)((width + 1) * GTNAP_GLOBAL->cell_x_size);
    size.height = (real32_t)GTNAP_GLOBAL->edit_y_size;
    id = i_add_object(ekOBJ_EDIT, top - gtwin->top, left - gtwin->left, GTNAP_GLOBAL->cell_x_size, GTNAP_GLOBAL->cell_y_size, &size, in_scroll, (GuiComponent*)edit, gtwin);
    obj = arrpt_last(gtwin->gui_objects, GtNapObject);
    cassert_no_null(obj);
    cassert(obj->type = ekOBJ_EDIT);

    obj->editSize = width;

    if (type == 'C') {
        obj->dtype = ekTYPE_CHARACTER;
    }
    else if (type == 'D') {
        obj->dtype = ekTYPE_DATE;
    }
    else {
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

    unref(auto_block);
    unref(wizard_block);
    //if (auto_block != NULL)
    //    obj->auto_block = hb_itemNew(auto_block);

    //if (wizard_block != NULL)
    //    obj->wizard_block = hb_itemNew(wizard_block);

    i_set_edit_text(obj);

    //if (obj->wizard_block != NULL)
    //{
    //    Button *button = button_push();
    //    char_t text[8];
    //    GtNapObject *objbut = NULL;
    //    S2Df bsize;
    //    uint32_t editIndex;

    //    {
    //    char_t text[8];
    //        uint32_t b = unicode_to_char(0x25BE, text, ekUTF8);
    //        text[b] = '\0';
    //    }

    //    //uint32_t butIndex;
    //    button_vpadding(button, i_button_vpadding());
    //    log_printf("EDIT (%s) HAS LISTA IN SCROLL: %d", edit_get_text((Edit*)obj->component), in_scroll);
    //    button_text(button, text);
    //    button_font(button, GTNAP_GLOBAL->global_font);

    //    editIndex = arrpt_size(gtwin->gui_objects, GtNapObject) - 1;
    //    bsize.width = (real32_t)(2 * GTNAP_GLOBAL->cell_x_size);
    //    bsize.height = (real32_t)(1 * GTNAP_GLOBAL->button_y_size);
    //    i_add_object(ekOBJ_BUTTON, (top - gtwin->top), (left - gtwin->left) + width + 1, GTNAP_GLOBAL->cell_x_size, GTNAP_GLOBAL->cell_y_size, &bsize, in_scroll, (GuiComponent*)button, gtwin);

    //    objbut = arrpt_last(gtwin->gui_objects, GtNapObject);
    //    cassert_no_null(objbut);
    //    cassert(objbut->type = ekOBJ_BUTTON);
    //    cassert(objbut->editBoxIndexForButton == UINT32_MAX);
    //    objbut->cuawin = gtwin;
    //    objbut->editBoxIndexForButton = editIndex;
    //    //butIndex = arrst_size(cuawin->gui_objects, GtNapObject);

    //    {
    //        Listener *listener = i_gtnap_listener(NULL, editIndex, FALSE, gtwin, i_OnListaButton);
    //        button_OnClick(button, listener);
    //    }
    //}





    //str_destroy(&ctext);

    // if (listener != NULL)
    //     button_OnClick(button, listener);

    return id;
}

/*---------------------------------------------------------------------------*/

static void i_OnWizardButton(GtNapCallback *callback, Event *e)
{
    GtNapObject *obj = NULL;
    cassert_no_null(callback);
    cassert_no_null(callback->gtwin);
    cassert(callback->autoclose_id == UINT32_MAX);
    unref(e);
    obj = arrpt_get(callback->gtwin->gui_objects, callback->key, GtNapObject);
    i_launch_wizard(callback->gtwin, obj);
}

/*---------------------------------------------------------------------------*/

static void i_OnKeyWizard(GtNapWindow *cuawin, Event *e)
{
    cassert_no_null(cuawin);
    unref(e);
    arrpt_foreach(obj, cuawin->gui_objects, GtNapObject)
        if (obj->type == ekOBJ_EDIT)
        {
            if (obj->has_focus == TRUE)
            {
                if (obj->wizard_block != NULL)
                    i_launch_wizard(cuawin, obj);
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
        listener = i_gtnap_listener(NULL, id, UINT32_MAX, bobj->cuawin, i_OnWizardButton);
        bobj->editBoxIndexForButton = id;
        button_OnClick((Button*)bobj->component, listener);

        {
            char_t text[8];
            uint32_t b = unicode_to_char(0x25BE, text, ekUTF8);
            text[b] = '\0';
            button_text((Button*)bobj->component, text);
        }
    }

    if (nkey != NULL)
        window_hotkey(obj->cuawin->window, nkey->vkey, 0, listener(obj->cuawin, i_OnKeyWizard, GtNapWindow));
}

/*---------------------------------------------------------------------------*/

static void i_set_view_text(const GtNapObject *obj)
{
    cassert_no_null(obj);
    cassert(obj->type == ekOBJ_TEXTVIEW);
    textview_clear((TextView*)obj->component);
    if (obj->get_set_block != NULL)
    {
        PHB_ITEM ritem = hb_itemDo(obj->get_set_block, 0);
        String *str = i_item_to_utf8_string(ritem);
        textview_writef((TextView*)obj->component, tc(str));
        hb_itemRelease(ritem);
        str_destroy(&str);
    }
}

/*---------------------------------------------------------------------------*/

uint32_t hb_gtnap_textview(const uint32_t wid, const int32_t top, const int32_t left, const int32_t bottom, const int32_t right, HB_ITEM *get_set_block, HB_ITEM *valida_block, const bool_t in_scroll)
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
    size.width = (real32_t)((right - left + 1) * GTNAP_GLOBAL->cell_x_size);
    size.height = (real32_t)((bottom - top + 1) * GTNAP_GLOBAL->cell_y_size);
    id = i_add_object(ekOBJ_TEXTVIEW, top - gtwin->top, left - gtwin->left, GTNAP_GLOBAL->cell_x_size, GTNAP_GLOBAL->cell_y_size, &size, in_scroll, (GuiComponent*)view, gtwin);
    obj = arrpt_last(gtwin->gui_objects, GtNapObject);
    cassert_no_null(obj);
    cassert(obj->type = ekOBJ_TEXTVIEW);

    if (get_set_block != NULL)
        obj->get_set_block = hb_itemNew(get_set_block);

    if (valida_block != NULL)
        obj->valida_block = hb_itemNew(valida_block);

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
    textview_scroll_visible((TextView*)obj->component, horizontal, vertical);
}

/*---------------------------------------------------------------------------*/

void hb_gtnap_textview_caret(const uint32_t wid, const uint32_t id, const int64_t pos)
{
    GtNapObject *obj = i_gtobj(GTNAP_GLOBAL, wid, id);
    cassert_no_null(obj);
    cassert(obj->type == ekOBJ_TEXTVIEW);
    textview_move_caret((TextView*)obj->component, pos);
}

/*---------------------------------------------------------------------------*/

static void i_OnTextConfirm(GtNapObject *gtobj, Event *e)
{
    cassert_no_null(gtobj);
    cassert(gtobj->type == ekOBJ_TEXTVIEW);
    cassert_no_null(gtobj->cuawin);
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
            const char_t *utf8 = textview_get_text((const TextView*)gtobj->component);
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
            if (gtobj->cuawin->confirm_block != NULL)
            {
                HB_ITEM *ritem = hb_itemDo(gtobj->cuawin->confirm_block, 0);
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
            GtNapWindow *gtwin = gtobj->cuawin;
            if (gtobj->cuawin->parent_id != UINT32_MAX)
                gtwin = i_gtwin(GTNAP_GLOBAL, gtobj->cuawin->parent_id);

            gtwin->modal_window_alive = FALSE;
            window_stop_modal(gtwin->window, NAP_MODAL_TEXT_CONFIRM);
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
    button_OnClick((Button*)bobj->component, listener(obj, i_OnTextConfirm, GtNapObject));
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
        Window *window = i_effective_window(obj->cuawin, GTNAP_GLOBAL);
        window_hotkey(window, nkey->vkey, nkey->modifiers, listener(obj, i_OnTextConfirm, GtNapObject));
    }
}

/*---------------------------------------------------------------------------*/

uint32_t hb_gtnap_menu(const uint32_t wid, const int32_t top, const int32_t left, const int32_t bottom, const int32_t right, const bool_t autoclose, const bool_t in_scroll)
{
    GtNapWindow *gtwin = i_gtwin(GTNAP_GLOBAL, wid);
    Panel *panel = nap_menu_create(autoclose);
    S2Df size, final_size;
    uint32_t id = UINT32_MAX;
    cassert_no_null(gtwin);
    size.width = (real32_t)((right - left + 1) * GTNAP_GLOBAL->cell_x_size);
    size.height = (real32_t)((bottom - top + 1) * GTNAP_GLOBAL->cell_y_size);
    _panel_compose(panel, &size, &final_size);
    _panel_locate(panel);
    id = i_add_object(ekOBJ_MENU, top - gtwin->top, left - gtwin->left, GTNAP_GLOBAL->cell_x_size, GTNAP_GLOBAL->cell_y_size, &size, in_scroll, (GuiComponent*)panel, gtwin);
    return id;
}

/*---------------------------------------------------------------------------*/

void hb_gtnap_menu_add(const uint32_t wid, uint32_t id, HB_ITEM *text_block, HB_ITEM *click_block, uint32_t kpos)
{
    GtNapObject *gtobj = i_gtobj(GTNAP_GLOBAL, wid, id);
    cassert_no_null(gtobj);
    cassert_no_null(gtobj->cuawin);
    cassert(gtobj->type == ekOBJ_MENU);
    nap_menu_add((Panel*)gtobj->component, gtobj->cuawin->window, text_block, click_block, kpos);
}

/*---------------------------------------------------------------------------*/

uint32_t hb_gtnap_menu_selected(const uint32_t wid, uint32_t id)
{
    GtNapObject *gtobj = i_gtobj(GTNAP_GLOBAL, wid, id);
    cassert_no_null(gtobj);
    cassert(gtobj->type == ekOBJ_MENU);
    return nap_menu_selected((Panel*)gtobj->component);
}

/*---------------------------------------------------------------------------*/

uint32_t hb_gtnap_tableview(const uint32_t wid, const bool_t multisel, const int32_t top, const int32_t left, const int32_t bottom, const int32_t right, const bool_t in_scroll)
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
    size.width = (real32_t)((right - left + 1) * GTNAP_GLOBAL->cell_x_size);
    size.height = (real32_t)((bottom - top + 1) * GTNAP_GLOBAL->cell_y_size);
    id = i_add_object(ekOBJ_TABLEVIEW, top - gtwin->top, left - gtwin->left, GTNAP_GLOBAL->cell_x_size, GTNAP_GLOBAL->cell_y_size, &size, in_scroll, (GuiComponent*)view, gtwin);
    obj = arrpt_last(gtwin->gui_objects, GtNapObject);
    cassert_no_null(obj);
    cassert(obj->type = ekOBJ_TABLEVIEW);
    obj->multisel = multisel;
    return id;
}

/*---------------------------------------------------------------------------*/

static uint32_t i_header_char_width(const char_t *title)
{
    uint32_t nchars = 0;
    ArrPt(String) *strs = str_splits(title, "\n", TRUE);
    arrpt_foreach_const(str, strs, String)
        uint32_t n = unicode_nchars(tc(str), ekUTF8);
        if (n > nchars)
            nchars = n;
    arrpt_end();

    arrpt_destroy(&strs, str_destroy, String);
    return nchars;
}

/*---------------------------------------------------------------------------*/

static uint32_t i_col_width(const uint32_t fixed_width, const uint32_t str_width, const GtNap *gtnap)
{
    uint32_t cwidth = fixed_width;
    cassert_no_null(gtnap);

    if (cwidth == 0)
        cwidth = str_width;

    return (cwidth + 1) * gtnap->cell_x_size;
}

/*---------------------------------------------------------------------------*/

void hb_gtnap_tableview_column(const uint32_t wid, const uint32_t id, const uint32_t width, HB_ITEM *head_block, HB_ITEM *eval_block)
{
    GtNapObject *obj = i_gtobj(GTNAP_GLOBAL, wid, id);
    GtNapColumn *col = NULL;
    uint32_t cid = UINT32_MAX;
    uint32_t hnchars = 0;
    cassert_no_null(obj);
    cassert(obj->type == ekOBJ_TABLEVIEW);

    if (obj->columns == NULL)
        obj->columns = arrst_create(GtNapColumn);

    cid = tableview_new_column_text((TableView*)obj->component);
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
    hnchars = i_header_char_width(tc(col->title));
    col->fixed_width = width;
    col->width = i_col_width(col->fixed_width, hnchars, GTNAP_GLOBAL);
    col->align = ekLEFT;
    col->block = hb_itemNew(eval_block);
    tableview_header_title((TableView*)obj->component, cid, tc(col->title));
    tableview_column_width((TableView*)obj->component, cid, (real32_t)col->width);
    tableview_header_align((TableView*)obj->component, cid, col->align);
}

/*---------------------------------------------------------------------------*/

void hb_gtnap_tableview_scroll(const uint32_t wid, const uint32_t id, const bool_t horizontal, const bool_t vertical)
{
    GtNapObject *obj = i_gtobj(GTNAP_GLOBAL, wid, id);
    cassert_no_null(obj);
    cassert(obj->type == ekOBJ_TABLEVIEW);
    tableview_scroll_visible((TableView*)obj->component, horizontal, vertical);
}

/*---------------------------------------------------------------------------*/

void hb_gtnap_tableview_grid(const uint32_t wid, const uint32_t id, const bool_t hlines, const bool_t vlines)
{
    GtNapObject *obj = i_gtobj(GTNAP_GLOBAL, wid, id);
    cassert_no_null(obj);
    cassert(obj->type == ekOBJ_TABLEVIEW);
    tableview_grid((TableView*)obj->component, hlines, vlines);
}

/*---------------------------------------------------------------------------*/

void hb_gtnap_tableview_header(const uint32_t wid, const uint32_t id, const bool_t visible)
{
    GtNapObject *obj = i_gtobj(GTNAP_GLOBAL, wid, id);
    cassert_no_null(obj);
    cassert(obj->type == ekOBJ_TABLEVIEW);
    tableview_header_visible((TableView*)obj->component, visible);
}

/*---------------------------------------------------------------------------*/

void hb_gtnap_tableview_freeze(const uint32_t wid, const uint32_t id, const uint32_t col_id)
{
    GtNapObject *obj = i_gtobj(GTNAP_GLOBAL, wid, id);
    cassert_no_null(obj);
    cassert(obj->type == ekOBJ_TABLEVIEW);
    cassert(col_id > 0);
    tableview_column_freeze((TableView*)obj->component, col_id - 1);
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

static const char_t *i_area_eval_field(GtNapArea *gtarea, const uint32_t field_id, const uint32_t row_id, align_t *align)
{
    uint32_t recno = 0;
    const GtNapColumn *column = NULL;
    HB_ITEM *ritem = NULL;
    HB_TYPE type = 0;

    cassert_no_null(gtarea);
    cassert_no_null(gtarea->gtobj);
    cassert(field_id > 0);

    /* Go to DB record */
    recno = *arrst_get_const(gtarea->records, row_id, uint32_t);
    SELF_GOTO(gtarea->area, recno);

    /* Get the table column */
    column = arrst_get_const(gtarea->gtobj->columns, field_id - 1, GtNapColumn);

    /* CodeBlock that computes the cell content */
    ritem = hb_itemDo(column->block, 0);

    /* Fill the temporal cell buffer with cell result */
    type = HB_ITEM_TYPE(ritem);
    TEMP_BUFFER[0] = '\0';

    if (type == HB_IT_STRING)
    {
        hb_itemCopyStrUTF8(ritem, TEMP_BUFFER, sizeof(TEMP_BUFFER));
    }
    else if (type == HB_IT_DATE)
    {
        char date[16];
        hb_itemGetDS(ritem, date);
        hb_dateFormat(date, TEMP_BUFFER, hb_setGetDateFormat());
    }
    else if (type == HB_IT_DOUBLE)
    {
        double value = hb_itemGetND(ritem);
        bstd_sprintf(TEMP_BUFFER, sizeof(TEMP_BUFFER), "%12.4f", value);
    }

    hb_itemRelease(ritem);

    if (align != NULL)
        *align = column->align;

    return TEMP_BUFFER;
}

/*---------------------------------------------------------------------------*/

static void i_area_column_width(GtNapArea *gtarea, const uint32_t col, const char_t *text)
{
    GtNapColumn *column = NULL;
    uint32_t nchars, width;
    cassert_no_null(gtarea);
    cassert_no_null(gtarea->gtobj);
    cassert(gtarea->gtobj->type == ekOBJ_TABLEVIEW);
    column = arrst_get(gtarea->gtobj->columns, col, GtNapColumn);
    nchars = unicode_nchars(text, ekUTF8);
    width = i_col_width(column->fixed_width, nchars, GTNAP_GLOBAL);

    if (width > column->width)
    {
        column->width = width;
        tableview_column_width((TableView*)gtarea->gtobj->component, col, (real32_t)column->width);
    }
}

/*---------------------------------------------------------------------------*/

static void i_OnTableAreaData(GtNapArea *gtarea, Event *e)
{
    uint32_t etype = event_type(e);
    cassert_no_null(gtarea);

    switch(etype) {
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
    gtwin = obj->cuawin;
    cassert_no_null(gtwin);

    if (gtwin->gtarea != NULL)
        i_destroy_area(&gtwin->gtarea);

    gtwin->gtarea = i_create_area();
    gtwin->gtarea->area = (AREA*)hb_rddGetCurrentWorkAreaPointer();
    gtwin->gtarea->gtobj = obj;

    if (while_block != NULL)
        gtwin->gtarea->while_block = hb_itemNew(while_block);
    else
        gtwin->gtarea->while_block = NULL;

    tableview_OnData((TableView*)obj->component, listener(gtwin->gtarea, i_OnTableAreaData, GtNapArea));
    tableview_OnSelect((TableView*)obj->component, listener(gtwin->gtarea, i_OnTableAreaRowSelect, GtNapArea));
}

/*---------------------------------------------------------------------------*/

static const char_t *i_data_eval_field(GtNapObject *gtobj, const uint32_t col_id, const uint32_t row_id, align_t *align)
{
    const GtNapColumn *column = NULL;
    HB_ITEM *pitem = NULL;
    HB_ITEM *ritem = NULL;
    HB_TYPE type = 0;

    cassert_no_null(gtobj);

    /* Get the table column */
    column = arrst_get_const(gtobj->columns, col_id, GtNapColumn);

    /* TODO: CACHE THIS PARAM */
    pitem = hb_itemPutNI(NULL, row_id + 1);

    /* CodeBlock that computes the cell content */
    ritem = hb_itemDo(column->block, 1, pitem);

    /* Fill the temporal cell buffer with cell result */
    type = HB_ITEM_TYPE(ritem);
    TEMP_BUFFER[0] = '\0';

    if (type == HB_IT_STRING)
    {
        hb_itemCopyStrUTF8(ritem, TEMP_BUFFER, sizeof(TEMP_BUFFER));
    }
    else if (type == HB_IT_DATE)
    {
        char date[16];
        hb_itemGetDS(ritem, date);
        hb_dateFormat(date, TEMP_BUFFER, hb_setGetDateFormat());
    }
    else if (type == HB_IT_DOUBLE)
    {
        double value = hb_itemGetND(ritem);
        bstd_sprintf(TEMP_BUFFER, sizeof(TEMP_BUFFER), "%12.4f", value);
    }

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
    cassert_no_null(gtobj->cuawin);

    switch(etype) {
    case ekGUI_EVENT_TBL_BEGIN:
        break;

    case ekGUI_EVENT_TBL_END:
        break;

    case ekGUI_EVENT_TBL_NROWS:
    {
        uint32_t *n = event_result(e, uint32_t);
        *n = gtobj->cuawin->num_rows;
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
    gtwin = obj->cuawin;
    cassert_no_null(gtwin);
    gtwin->num_rows = num_rows;
    tableview_OnData((TableView*)obj->component, listener(obj, i_OnTableData, GtNapObject));
    tableview_OnSelect((TableView*)obj->component, NULL);
}

/*---------------------------------------------------------------------------*/

void hb_gtnap_tableview_deselect_all(const uint32_t wid, const uint32_t id)
{
    GtNapObject *obj = i_gtobj(GTNAP_GLOBAL, wid, id);
    cassert_no_null(obj);
    cassert(obj->type == ekOBJ_TABLEVIEW);
    tableview_deselect_all((TableView*)obj->component);
}

/*---------------------------------------------------------------------------*/

void hb_gtnap_tableview_select_row(const uint32_t wid, const uint32_t id, const uint32_t row_id)
{
    GtNapObject *obj = i_gtobj(GTNAP_GLOBAL, wid, id);
    cassert_no_null(obj);
    cassert(obj->type == ekOBJ_TABLEVIEW);
    tableview_select((TableView*)obj->component, &row_id, 1);
    tableview_update((TableView*)obj->component);
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

void hb_gtnap_tableview_toggle_row(const uint32_t wid, const uint32_t id, const uint32_t row_id)
{
    GtNapObject *obj = i_gtobj(GTNAP_GLOBAL, wid, id);
    const ArrSt(uint32_t) *sel = NULL;
    cassert_no_null(obj);
    cassert(obj->type == ekOBJ_TABLEVIEW);
    sel = tableview_selected((TableView*)obj->component);
    i_toogle_sel((TableView*)obj->component, sel, row_id);
    tableview_update((TableView*)obj->component);
}

/*---------------------------------------------------------------------------*/

const ArrSt(uint32_t) *hb_gtnap_tableview_selected_rows(const uint32_t wid, const uint32_t id)
{
    GtNapObject *obj = i_gtobj(GTNAP_GLOBAL, wid, id);
    cassert_no_null(obj);
    cassert(obj->type == ekOBJ_TABLEVIEW);
    return tableview_selected((TableView*)obj->component);
}

/*---------------------------------------------------------------------------*/

uint32_t hb_gtnap_tableview_focus_row(const uint32_t wid, const uint32_t id)
{
    GtNapObject *obj = i_gtobj(GTNAP_GLOBAL, wid, id);
    uint32_t focused;
    cassert_no_null(obj);
    cassert(obj->type == ekOBJ_TABLEVIEW);
    focused = tableview_get_focus_row((TableView*)obj->component);
    return focused;
}

/*---------------------------------------------------------------------------*/

uint32_t hb_gtnap_tableview_recno_from_row(const uint32_t wid, const uint32_t id, const uint32_t row_id)
{
    GtNapObject *obj = i_gtobj(GTNAP_GLOBAL, wid, id);
    GtNapWindow *gtwin = NULL;
    cassert_no_null(obj);
    cassert(obj->type == ekOBJ_TABLEVIEW);
    gtwin = obj->cuawin;
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
    gtwin = obj->cuawin;
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
    tableview_update((TableView*)obj->component);
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
                cassert(type == HB_IT_LOGICAL);
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
    view = (TableView*)gtarea->gtobj->component;

    /* Current selected */
    SELF_RECNO( gtarea->area, &ulCurRec );

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
    gtwin = obj->cuawin;
    view = (TableView*)obj->component;
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
    gtwin->toolbar->buttons = arrpt_create(Button);
    gtwin->toolbar->pixels_image = image_pixels;
    gtwin->toolbar->pixels_button = (uint32_t)((real32_t)image_pixels * 1.3f);
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
        cassert_no_null(gtwin);
        cassert_no_null(gtwin->toolbar);

        if (image_width(image) != gtwin->toolbar->pixels_image || image_height(image) != gtwin->toolbar->pixels_image)
        {
            Image *scaled = image_scale(image, gtwin->toolbar->pixels_image, gtwin->toolbar->pixels_image);
            image_destroy(&image);
            image = scaled;
        }

        button_image(button, image);
        button_OnClick(button, listener);
        i_cp_to_utf8(tooltip, utf8, sizeof(utf8));
        button_tooltip(button, utf8);
        arrpt_append(gtwin->toolbar->buttons, button, Button);
        image_destroy(&image);
    }
}

/*---------------------------------------------------------------------------*/

void hb_gtnap_toolbar_separator(const uint32_t wid)
{
    GtNapWindow *gtwin = i_gtwin(GTNAP_GLOBAL, wid);
    cassert_no_null(gtwin);
    cassert_no_null(gtwin->toolbar);
    arrpt_append(gtwin->toolbar->buttons, NULL, Button);
}

/*---------------------------------------------------------------------------*/

Font *hb_gtnap_font(void)
{
    cassert_no_null(GTNAP_GLOBAL);
    return GTNAP_GLOBAL->global_font;
}

/*---------------------------------------------------------------------------*/

void hb_gtnap_cualib_init_log(void)
{
    osbs_start();
    log_output(FALSE, FALSE);
    log_file("C:\\Users\\USUARIO\\AppData\\Roaming\\exemplo\\log2.txt");
}

/*---------------------------------------------------------------------------*/

void hb_gtnap_cualib_default_button(const uint32_t nDefault)
{
    GtNapWindow *cuawin = i_current_gtwin(GTNAP_GLOBAL);
    cassert(cuawin->default_button == UINT32_MAX);
    cassert(nDefault > 0);
    cuawin->default_button = nDefault - 1;
}


/*---------------------------------------------------------------------------*/

void hb_gtnap_cualib_window_f4_lista(void)
{
    GtNapWindow *cuawin = i_current_gtwin(GTNAP_GLOBAL);
    window_hotkey(cuawin->window, ekKEY_F4, 0, listener(cuawin, i_OnKeyWizard, GtNapWindow));
}

/*---------------------------------------------------------------------------*/

uint32_t hb_gtnap_cualib_window_current_edit(void)
{
    uint32_t id = 0;
    GtNapWindow *cuawin = i_current_main_gtwin(GTNAP_GLOBAL);
    arrpt_foreach(obj, cuawin->gui_objects, GtNapObject)
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

uint32_t hb_gtnap_cell_height(void)
{
    return GTNAP_GLOBAL->cell_y_size;
}

/*---------------------------------------------------------------------------*/

extern String *hb_block_to_utf8(HB_ITEM *item)
{
    PHB_ITEM ritem = hb_itemDo(item, 0);
    String *str = i_item_to_utf8_string(ritem);
    hb_itemRelease(ritem);
    return str;
}

/*---------------------------------------------------------------------------*/

static HB_BOOL hb_gtnap_Lock( PHB_GT pGT )
{
    HB_SYMBOL_UNUSED( pGT );
    return TRUE;
}

/*---------------------------------------------------------------------------*/

static void hb_gtnap_Unlock( PHB_GT pGT )
{
    HB_SYMBOL_UNUSED( pGT );
}

/*---------------------------------------------------------------------------*/

static void hb_gtnap_Init( PHB_GT pGT, HB_FHANDLE hFilenoStdin, HB_FHANDLE hFilenoStdout, HB_FHANDLE hFilenoStderr )
{
    HB_SYMBOL_UNUSED( pGT );
    HB_SYMBOL_UNUSED( hFilenoStdin );
    HB_SYMBOL_UNUSED( hFilenoStdout );
    HB_SYMBOL_UNUSED( hFilenoStderr );
}

/*---------------------------------------------------------------------------*/

static void hb_gtnap_Exit( PHB_GT pGT )
{
    HB_SYMBOL_UNUSED( pGT );
}

/*---------------------------------------------------------------------------*/

static HB_BOOL hb_gtnap_Resize( PHB_GT pGT, int iRow, int iCol )
{
    HB_SYMBOL_UNUSED( pGT );
    HB_SYMBOL_UNUSED( iRow );
    HB_SYMBOL_UNUSED( iCol );
    return TRUE;
}

/*---------------------------------------------------------------------------*/

static HB_BOOL hb_gtnap_SetMode( PHB_GT pGT, int iRow, int iCol )
{
    HB_SYMBOL_UNUSED( pGT );
    HB_SYMBOL_UNUSED( iRow );
    HB_SYMBOL_UNUSED( iCol );
    return TRUE;
}

/*---------------------------------------------------------------------------*/

static void hb_gtnap_GetSize( PHB_GT pGT, int * piRows, int  * piCols )
{
    HB_SYMBOL_UNUSED( pGT );
    *piRows = (int)GTNAP_GLOBAL->rows;
    *piCols = (int)GTNAP_GLOBAL->cols;
}

/*---------------------------------------------------------------------------*/

static void hb_gtnap_ExposeArea( PHB_GT pGT, int iTop, int iLeft, int iBottom, int iRight )
{
    HB_SYMBOL_UNUSED( pGT );
    HB_SYMBOL_UNUSED( iTop );
    HB_SYMBOL_UNUSED( iLeft );
    HB_SYMBOL_UNUSED( iBottom );
    HB_SYMBOL_UNUSED( iRight );
}

/*---------------------------------------------------------------------------*/

static int hb_gtnap_MaxCol( PHB_GT pGT )
{
    HB_SYMBOL_UNUSED( pGT );
    return GTNAP_GLOBAL->cols - 1;
}

/*---------------------------------------------------------------------------*/

static int hb_gtnap_MaxRow( PHB_GT pGT )
{
    HB_SYMBOL_UNUSED( pGT );
    return GTNAP_GLOBAL->rows - 1;
}

/*---------------------------------------------------------------------------*/

static HB_BOOL hb_gtnap_CheckPos( PHB_GT pGT, int iRow, int iCol, long * plIndex )
{
    HB_SYMBOL_UNUSED( pGT );
    HB_SYMBOL_UNUSED( iRow );
    HB_SYMBOL_UNUSED( iCol );
    *plIndex = 0;
    return TRUE;
}

/*---------------------------------------------------------------------------*/

static void hb_gtnap_SetPos( PHB_GT pGT, int iRow, int iCol )
{
    GtNapWindow *gtwin = i_current_gtwin(GTNAP_GLOBAL);
    HB_SYMBOL_UNUSED( pGT );
    if (gtwin != NULL)
    {
        gtwin->cursor_row = (int32_t)iRow;
        gtwin->cursor_col = (int32_t)iCol;
    }
}

/*---------------------------------------------------------------------------*/

static void hb_gtnap_GetPos( PHB_GT pGT, int * piRow, int * piCol )
{
    GtNapWindow *gtwin = i_current_gtwin(GTNAP_GLOBAL);
    HB_SYMBOL_UNUSED( pGT );
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

/*---------------------------------------------------------------------------*/

static HB_BOOL hb_gtnap_IsColor( PHB_GT pGT )
{
    HB_SYMBOL_UNUSED( pGT );
    return TRUE;
}

/*---------------------------------------------------------------------------*/

static int hb_gtnap_GetCursorStyle( PHB_GT pGT )
{
    HB_SYMBOL_UNUSED( pGT );
    return SC_NORMAL;
}

/*---------------------------------------------------------------------------*/

static void hb_gtnap_SetCursorStyle( PHB_GT pGT, int iStyle )
{
    HB_SYMBOL_UNUSED( pGT );

    switch( iStyle )
    {
        case SC_NONE:
            break;
        case SC_INSERT:
            break;
        case SC_SPECIAL1:
            break;
        case SC_SPECIAL2:
            break;
        case SC_NORMAL:
        default:
            break;
    }
}

/*---------------------------------------------------------------------------*/

static void hb_gtnap_DispBegin( PHB_GT pGT )
{
    HB_SYMBOL_UNUSED( pGT );
}

/*---------------------------------------------------------------------------*/

static void hb_gtnap_DispEnd( PHB_GT pGT )
{
    HB_SYMBOL_UNUSED( pGT );
}

/*---------------------------------------------------------------------------*/

static int hb_gtnap_DispCount( PHB_GT pGT )
{
    HB_SYMBOL_UNUSED( pGT );
    return 0;
}

/*---------------------------------------------------------------------------*/

static HB_BOOL hb_gtnap_GetChar( PHB_GT pGT, int iRow, int iCol, int * pbColor, HB_BYTE * pbAttr, HB_USHORT * pusChar )
{
    HB_SYMBOL_UNUSED( pGT );
    HB_SYMBOL_UNUSED( iCol );
    HB_SYMBOL_UNUSED( iRow );
    *pbColor = 0;
    *pbAttr = 0;
    *pusChar = 65;
    return TRUE;
}

/*---------------------------------------------------------------------------*/

//static GtNapWindow *i_force_gtwin(GtNap *gtnap)
//{
//    sffdgd
//    GtNapWindow *gtwin = i_current_gtwin(gtnap);
//    if (gtwin == NULL)
//    {
//        hb_gtnap_window(0, 0, gtnap->rows - 1, gtnap->cols - 1, tc(gtnap->title), FALSE, TRUE, TRUE, FALSE);
//
//    }
//}

/*---------------------------------------------------------------------------*/

static HB_BOOL hb_gtnap_PutChar( PHB_GT pGT, int iRow, int iCol, int bColor, HB_BYTE bAttr, HB_USHORT usChar )
{
        //GtNapWindow *gtwin = i_current_gtwin(GTNAP_GLOBAL);

    HB_SYMBOL_UNUSED( pGT );
    HB_SYMBOL_UNUSED( iRow );
    HB_SYMBOL_UNUSED( iCol );
    HB_SYMBOL_UNUSED( bColor );
    HB_SYMBOL_UNUSED( bAttr );
    HB_SYMBOL_UNUSED( usChar );
    return TRUE;
}

/*---------------------------------------------------------------------------*/

static void hb_gtnap_Save( PHB_GT pGT, int iTop, int iLeft, int iBottom, int iRight, void * pBuffer )
{
    HB_SYMBOL_UNUSED( pGT );
    HB_SYMBOL_UNUSED( iTop );
    HB_SYMBOL_UNUSED( iLeft );
    HB_SYMBOL_UNUSED( iBottom );
    HB_SYMBOL_UNUSED( iRight );
    HB_SYMBOL_UNUSED( pBuffer );
}

/*---------------------------------------------------------------------------*/

static void hb_gtnap_Rest( PHB_GT pGT, int iTop, int iLeft, int iBottom, int iRight, const void * pBuffer )
{
    HB_SYMBOL_UNUSED( pGT );
    HB_SYMBOL_UNUSED( iTop );
    HB_SYMBOL_UNUSED( iLeft );
    HB_SYMBOL_UNUSED( iBottom );
    HB_SYMBOL_UNUSED( iRight );
    HB_SYMBOL_UNUSED( pBuffer );
}

/*---------------------------------------------------------------------------*/

static int hb_gtnap_PutText( PHB_GT pGT, int iRow, int iCol, int bColor, const char * pText, HB_SIZE ulLen )
{
    //GtNapWindow *gtwin = i_current_gtwin(GTNAP_GLOBAL);

    HB_SYMBOL_UNUSED( pGT );
    HB_SYMBOL_UNUSED( iRow );
    HB_SYMBOL_UNUSED( iCol );
    HB_SYMBOL_UNUSED( bColor );
    HB_SYMBOL_UNUSED( pText );
    HB_SYMBOL_UNUSED( ulLen );
    return 0;
}

/*---------------------------------------------------------------------------*/

static void hb_gtnap_Replicate( PHB_GT pGT, int iRow, int iCol, int bColor, HB_BYTE bAttr, HB_USHORT usChar, HB_SIZE ulLen )
{
    HB_SYMBOL_UNUSED( pGT );
    HB_SYMBOL_UNUSED( iRow );
    HB_SYMBOL_UNUSED( iCol );
    HB_SYMBOL_UNUSED( bColor );
    HB_SYMBOL_UNUSED( bAttr );
    HB_SYMBOL_UNUSED( usChar );
    HB_SYMBOL_UNUSED( ulLen );
}

/*---------------------------------------------------------------------------*/

static void hb_gtnap_WriteAt( PHB_GT pGT, int iRow, int iCol, const char * pText, HB_SIZE ulLength )
{
    GtNapWindow *gtwin = i_current_gtwin(GTNAP_GLOBAL);
    HB_SYMBOL_UNUSED( pGT );
    HB_SYMBOL_UNUSED( ulLength );
    if (gtwin != NULL)
    {
        i_add_label(iRow - gtwin->top, iCol - gtwin->left, FALSE, gtwin, GTNAP_GLOBAL);
        if (pText != NULL)
        {
            GtNapObject *obj = arrpt_last(gtwin->gui_objects, GtNapObject);
            char_t utf8[STATIC_TEXT_SIZE];
            i_cp_to_utf8(pText, utf8, sizeof32(utf8));
            i_set_label_text(obj, utf8);
        }
    }
}

/*---------------------------------------------------------------------------*/

static void hb_gtnap_SetAttribute( PHB_GT pGT, int iTop, int iLeft, int iBottom, int iRight, int bColor )
{
    HB_SYMBOL_UNUSED( pGT );
    HB_SYMBOL_UNUSED( iTop );
    HB_SYMBOL_UNUSED( iLeft );
    HB_SYMBOL_UNUSED( iBottom );
    HB_SYMBOL_UNUSED( iRight );
    HB_SYMBOL_UNUSED( bColor );
}

/*---------------------------------------------------------------------------*/

static void hb_gtnap_Scroll( PHB_GT pGT, int iTop, int iLeft, int iBottom, int iRight, int bColor, HB_USHORT bChar, int iRows, int iCols )
{
    GtNapWindow *gtwin = i_current_gtwin(GTNAP_GLOBAL);
    HB_SYMBOL_UNUSED( pGT );
    HB_SYMBOL_UNUSED( iTop );
    HB_SYMBOL_UNUSED( iLeft );
    HB_SYMBOL_UNUSED( iBottom );
    HB_SYMBOL_UNUSED( iRight );
    HB_SYMBOL_UNUSED( bColor );
    HB_SYMBOL_UNUSED( bChar );
    HB_SYMBOL_UNUSED( iRows );
    HB_SYMBOL_UNUSED( iCols );

    if (gtwin != NULL)
    {
        uint32_t i, n;
        // FRAN: The scroll, at the moment, delete all texts
        // Improve taking into account the input rectangle
        // Take into account if a real scroll exists (iRows > 0 || iCols > 0)
        n = arrpt_size(gtwin->gui_objects, GtNapObject);
        for (i = 0; i < n; )
        {
            GtNapObject *object = arrpt_get(gtwin->gui_objects, i, GtNapObject);
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

/*---------------------------------------------------------------------------*/

static void hb_gtnap_Box( PHB_GT pGT, int iTop, int iLeft, int iBottom, int iRight, const char * pbyFrame, int bColor )
{
    HB_SYMBOL_UNUSED( pGT );
    HB_SYMBOL_UNUSED( iTop );
    HB_SYMBOL_UNUSED( iLeft );
    HB_SYMBOL_UNUSED( iBottom );
    HB_SYMBOL_UNUSED( iRight );
    HB_SYMBOL_UNUSED( pbyFrame );
    HB_SYMBOL_UNUSED( bColor );
}

/*---------------------------------------------------------------------------*/

static void hb_gtnap_HorizLine( PHB_GT pGT, int iRow, int iLeft, int iRight, HB_USHORT bChar, int bColor )
{
    HB_SYMBOL_UNUSED( pGT );
    HB_SYMBOL_UNUSED( iRow );
    HB_SYMBOL_UNUSED( iLeft );
    HB_SYMBOL_UNUSED( iRight );
    HB_SYMBOL_UNUSED( bChar );
    HB_SYMBOL_UNUSED( bColor );
}

/*---------------------------------------------------------------------------*/

static void hb_gtnap_VertLine( PHB_GT pGT, int iCol, int iTop, int iBottom, HB_USHORT bChar, int bColor )
{
    HB_SYMBOL_UNUSED( pGT );
    HB_SYMBOL_UNUSED( iCol );
    HB_SYMBOL_UNUSED( iTop );
    HB_SYMBOL_UNUSED( iBottom );
    HB_SYMBOL_UNUSED( bChar );
    HB_SYMBOL_UNUSED( bColor );
}

/*---------------------------------------------------------------------------*/

static HB_BOOL hb_gtnap_GetBlink( PHB_GT pGT )
{
    HB_SYMBOL_UNUSED( pGT );
    return TRUE;
}

/*---------------------------------------------------------------------------*/

static void hb_gtnap_SetBlink( PHB_GT pGT, HB_BOOL bBlink )
{
    HB_SYMBOL_UNUSED( pGT );
    HB_SYMBOL_UNUSED( bBlink );
}

/*---------------------------------------------------------------------------*/

static const char * hb_gtnap_Version( PHB_GT pGT, int iType )
{
    HB_SYMBOL_UNUSED( pGT );
    if( iType == 0 )
        return HB_GT_DRVNAME( HB_GT_NAME );

    return "Harbour Terminal: GTNAP";
}

/*---------------------------------------------------------------------------*/

static void hb_gtnap_OutStd( PHB_GT pGT, const char * pbyStr, HB_SIZE ulLen )
{
    HB_SYMBOL_UNUSED( pGT );
    if (pbyStr != NULL && ulLen > 0)
        log_printf("hb_gtnap_OutStd(%s (%d))", pbyStr, (int)ulLen);
    else
        log_printf("hb_gtnap_OutStd(empty)");
}

/*---------------------------------------------------------------------------*/

static void hb_gtnap_OutErr( PHB_GT pGT, const char * pbyStr, HB_SIZE ulLen )
{
    HB_SYMBOL_UNUSED( pGT );
    if (pbyStr != NULL && ulLen > 0)
    {
        char_t utf8[STATIC_TEXT_SIZE];
        i_cp_to_utf8(pbyStr, utf8, sizeof32(utf8));
        log_printf("%s", utf8);
    }
}

/*---------------------------------------------------------------------------*/

static void hb_gtnap_Tone( PHB_GT pGT, double dFrequency, double dDuration )
{
    /* dDuration is in 'Ticks' (18.2 per second) */
    HB_SYMBOL_UNUSED( pGT );
    HB_SYMBOL_UNUSED( dFrequency );
    HB_SYMBOL_UNUSED( dDuration );
}

/*---------------------------------------------------------------------------*/

static HB_BOOL hb_gtnap_Info( PHB_GT pGT, int iType, PHB_GT_INFO pInfo )
{
    HB_SYMBOL_UNUSED( pGT );
    HB_SYMBOL_UNUSED( iType );
    HB_SYMBOL_UNUSED( pInfo );
    return TRUE;
}

/*---------------------------------------------------------------------------*/

static int hb_gtnap_Alert( PHB_GT pGT, PHB_ITEM message, PHB_ITEM options, int a, int b, double c )
{
    String *msg = NULL;
    ArrPt(String) *opts = NULL;
    uint32_t ret = 0;

    HB_SYMBOL_UNUSED( pGT );
    HB_SYMBOL_UNUSED( a );
    HB_SYMBOL_UNUSED( b );
    HB_SYMBOL_UNUSED( c );

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

static void hb_gtnap_mouse_Init( PHB_GT pGT )
{
    HB_SYMBOL_UNUSED( pGT );
}

/*---------------------------------------------------------------------------*/

static void hb_gtnap_mouse_Exit( PHB_GT pGT )
{
    HB_SYMBOL_UNUSED( pGT );
}

/*---------------------------------------------------------------------------*/

static HB_BOOL hb_gtnap_mouse_IsPresent( PHB_GT pGT )
{
    HB_SYMBOL_UNUSED( pGT );
    return TRUE;
}

/*---------------------------------------------------------------------------*/

static int hb_gtnap_mouse_Col( PHB_GT pGT )
{
    HB_SYMBOL_UNUSED( pGT );
    return 1;
}

/*---------------------------------------------------------------------------*/

static int hb_gtnap_mouse_Row( PHB_GT pGT )
{
    HB_SYMBOL_UNUSED( pGT );
    return 1;
}

/*---------------------------------------------------------------------------*/

static int hb_gtnap_mouse_CountButton( PHB_GT pGT )
{
    HB_SYMBOL_UNUSED( pGT );
    return 3;
}

/*---------------------------------------------------------------------------*/

static HB_BOOL hb_gtnap_mouse_ButtonState( PHB_GT pGT, int iButton )
{
    HB_SYMBOL_UNUSED( pGT );
    HB_SYMBOL_UNUSED( iButton );
    return FALSE;
}

/*---------------------------------------------------------------------------*/

static int hb_gtnap_gfxPrimitive( PHB_GT pGT, int iType, int iTop, int iLeft, int iBottom, int iRight, int iColor )
{
    HB_SYMBOL_UNUSED( pGT );
    HB_SYMBOL_UNUSED( iType );
    HB_SYMBOL_UNUSED( iTop );
    HB_SYMBOL_UNUSED( iLeft );
    HB_SYMBOL_UNUSED( iBottom );
    HB_SYMBOL_UNUSED( iRight );
    HB_SYMBOL_UNUSED( iColor );
    return 1;
}

/*---------------------------------------------------------------------------*/

static void hb_gtnap_gfxText( PHB_GT pGT, int iTop, int iLeft, const char * cBuf, int iColor, int iSize, int iWidth )
{
    HB_SYMBOL_UNUSED( pGT );
    HB_SYMBOL_UNUSED( iTop );
    HB_SYMBOL_UNUSED( iLeft );
    HB_SYMBOL_UNUSED( cBuf );
    HB_SYMBOL_UNUSED( iColor );
    HB_SYMBOL_UNUSED( iSize );
    HB_SYMBOL_UNUSED( iWidth );
}

/*---------------------------------------------------------------------------*/

static HB_BOOL hb_gt_FuncInit( PHB_GT_FUNCS pFuncTable )
{
    pFuncTable->Lock = hb_gtnap_Lock;
    pFuncTable->Unlock = hb_gtnap_Unlock;
    pFuncTable->Init = hb_gtnap_Init;
    pFuncTable->Exit = hb_gtnap_Exit;
    // pFuncTable->New = NULL;
    // pFuncTable->Free = NULL;
    // pFuncTable->Mark = NULL;
    pFuncTable->Resize = hb_gtnap_Resize;
    pFuncTable->SetMode = hb_gtnap_SetMode;
    pFuncTable->GetSize = hb_gtnap_GetSize;
    // pFuncTable->SemiCold = NULL;
    // pFuncTable->ColdArea = NULL;
    pFuncTable->ExposeArea = hb_gtnap_ExposeArea;
    // pFuncTable->ScrollArea = NULL;
    // pFuncTable->TouchLine = NULL;
    // pFuncTable->TouchCell = NULL;
    // pFuncTable->Redraw = NULL;
    // pFuncTable->RedrawDiff = NULL;
    // pFuncTable->Refresh = NULL;
    // pFuncTable->Flush = NULL;
    pFuncTable->MaxCol = hb_gtnap_MaxCol;
    pFuncTable->MaxRow = hb_gtnap_MaxRow;
    pFuncTable->CheckPos = hb_gtnap_CheckPos;
    pFuncTable->SetPos = hb_gtnap_SetPos;
    pFuncTable->GetPos =  hb_gtnap_GetPos;
    pFuncTable->IsColor = hb_gtnap_IsColor;
    // pFuncTable->GetColorStr = NULL;
    // pFuncTable->SetColorStr = NULL;
    // pFuncTable->ColorSelect = NULL;
    // pFuncTable->GetColor = NULL;
    // pFuncTable->ColorNum = NULL;
    // pFuncTable->ColorsToString = NULL;
    // pFuncTable->StringToColors = NULL;
    // pFuncTable->GetColorData = NULL;
    // pFuncTable->GetClearColor = NULL;
    // pFuncTable->SetClearColor = NULL;
    // pFuncTable->GetClearChar = NULL;
    // pFuncTable->SetClearChar = NULL;
    pFuncTable->GetCursorStyle = hb_gtnap_GetCursorStyle;
    pFuncTable->SetCursorStyle = hb_gtnap_SetCursorStyle;
    // pFuncTable->GetScrCursor = NULL;
    // pFuncTable->GetScrChar = NULL;
    // pFuncTable->PutScrChar = NULL;
    // pFuncTable->GetScrUC = NULL;
    pFuncTable->DispBegin = hb_gtnap_DispBegin;
    pFuncTable->DispEnd = hb_gtnap_DispEnd;
    pFuncTable->DispCount = hb_gtnap_DispCount;
    pFuncTable->GetChar = hb_gtnap_GetChar;
    pFuncTable->PutChar = hb_gtnap_PutChar;
    // pFuncTable->RectSize = NULL;
    pFuncTable->Save = hb_gtnap_Save;
    pFuncTable->Rest = hb_gtnap_Rest;
    pFuncTable->PutText = hb_gtnap_PutText;
    // pFuncTable->PutTextW = NULL;
    pFuncTable->Replicate = hb_gtnap_Replicate;
    pFuncTable->WriteAt = hb_gtnap_WriteAt;
    // pFuncTable->WriteAtW = NULL;
    // pFuncTable->Write = NULL;
    // pFuncTable->WriteW = NULL;
    // pFuncTable->WriteCon = NULL;
    // pFuncTable->WriteConW = NULL;
    pFuncTable->SetAttribute = hb_gtnap_SetAttribute;
    // pFuncTable->DrawShadow = NULL;
    pFuncTable->Scroll = hb_gtnap_Scroll;
    // pFuncTable->ScrollUp = NULL;
    pFuncTable->Box = hb_gtnap_Box;
    pFuncTable->BoxW = NULL;
    pFuncTable->BoxD = NULL;
    pFuncTable->BoxS = NULL;
    pFuncTable->HorizLine = hb_gtnap_HorizLine;
    pFuncTable->VertLine  = hb_gtnap_VertLine;
    pFuncTable->GetBlink = hb_gtnap_GetBlink;
    pFuncTable->SetBlink = hb_gtnap_SetBlink;
    // pFuncTable->SetSnowFlag = NULL;
    pFuncTable->Version = hb_gtnap_Version;
    // pFuncTable->Suspend = NULL;
    // pFuncTable->Resume = NULL;
    // pFuncTable->PreExt = NULL;
    // pFuncTable->PostExt = NULL;
    pFuncTable->OutStd = hb_gtnap_OutStd;
    pFuncTable->OutErr = hb_gtnap_OutErr;
    pFuncTable->Tone = hb_gtnap_Tone;
    pFuncTable->Bell = NULL;
    pFuncTable->Info = hb_gtnap_Info;
    pFuncTable->Alert = hb_gtnap_Alert;
    pFuncTable->SetFlag = NULL;

    /* internationalization */
    // pFuncTable->SetDispCP = NULL;
    // pFuncTable->SetKeyCP = NULL;

    /* keyboard */
    // pFuncTable->ReadKey = NULL;
    // pFuncTable->InkeyGet = NULL;
    // pFuncTable->InkeyPut = NULL;
    // pFuncTable->InkeyIns = NULL;
    // pFuncTable->InkeyLast = NULL;
    // pFuncTable->InkeyNext = NULL;
    // pFuncTable->InkeyPoll = NULL;
    // pFuncTable->InkeySetText = NULL;
    // pFuncTable->InkeySetLast = NULL;
    // pFuncTable->InkeyReset = NULL;
    // pFuncTable->InkeyExit = NULL;

    /* mouse */
    pFuncTable->MouseInit = hb_gtnap_mouse_Init;
    pFuncTable->MouseExit = hb_gtnap_mouse_Exit;
    pFuncTable->MouseIsPresent = hb_gtnap_mouse_IsPresent;
    // pFuncTable->MouseShow = NULL;
    // pFuncTable->MouseHide = NULL;
    // pFuncTable->MouseGetCursor = NULL;
    // pFuncTable->MouseSetCursor = NULL;
    pFuncTable->MouseCol = hb_gtnap_mouse_Col;
    pFuncTable->MouseRow = hb_gtnap_mouse_Row;
    // pFuncTable->MouseGetPos = NULL;
    // pFuncTable->MouseSetPos = NULL;
    // pFuncTable->MouseSetBounds = NULL;
    // pFuncTable->MouseGetBounds = NULL;
    // pFuncTable->MouseStorageSize = NULL;
    // pFuncTable->MouseSaveState = NULL;
    // pFuncTable->MouseRestoreState = NULL;
    // pFuncTable->MouseGetDoubleClickSpeed = NULL;
    // pFuncTable->MouseSetDoubleClickSpeed = NULL;
    pFuncTable->MouseCountButton = hb_gtnap_mouse_CountButton;
    pFuncTable->MouseButtonState = hb_gtnap_mouse_ButtonState;
    // pFuncTable->MouseButtonPressed = NULL;
    // pFuncTable->MouseButtonReleased = NULL;
    // pFuncTable->MouseReadKey = NULL;

    /* Graphics API */
    pFuncTable->GfxPrimitive = hb_gtnap_gfxPrimitive;
    pFuncTable->GfxText = hb_gtnap_gfxText;
    pFuncTable->WhoCares = NULL;

   return TRUE;
}

/*---------------------------------------------------------------------------*/
/*
 *   Deprecated/Unused
 *
 */
//Listener *hb_gtnap_comp_listener(const uint32_t codeBlockParamId, GuiComponent *component, FPtr_gtnap_callback func_callback)
//{
//    unref(codeBlockParamId);
//    unref(component);
//    unref(func_callback);
//    cassert(FALSE);
//    return NULL;
//}

/*---------------------------------------------------------------------------*/

//Listener *hb_gtnap_wind_listener(const uint32_t codeBlockParamId, Window *window, FPtr_gtnap_callback func_callback)
//{
//    unref(codeBlockParamId);
//    unref(window);
//    unref(func_callback);
//    cassert(FALSE);
//    return NULL;
//}

/*---------------------------------------------------------------------------*/

static int s_GtId;
static HB_GT_FUNCS SuperTable;
#define HB_GTSUPER   ( &SuperTable )
#define HB_GTID_PTR  ( &s_GtId )

#include "hbgtreg.h"
