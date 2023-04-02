/*
    This is part of gtnap
    TODO: More info
    Commit - 2
*/

#include "gtnap.h"
#include "gtconvert.h"
#include "nap_menuvert.h"
#include "nappgui.h"
#include "osmain.h"
#include "hbapiitm.h"
#include "hbapirdd.h"
#include "hbapistr.h"
#include "hbdate.h"
#include "hbset.h"

typedef struct _gtnap_t GtNap;
typedef struct _gtnap_key_t GtNapKey;
typedef struct _gtnap_column_t GtNapColumn;
typedef struct _gtnap_cualib_object_t GtNapCualibObject;
typedef struct _gtnap_cualib_toolbar_t GtNapCualibToolbar;
typedef struct _gtnap_cualib_window_t GtNapCualibWindow;
typedef struct _vecitem_t VecItem;
typedef struct _gui_context_t GuiContext;

struct _gtnap_callback_t
{
    GtNapCualibWindow *cuawin;
    GuiComponent *cb_component;
    Window *cb_window;
    int32_t key;
    bool_t autoclose;
    PHB_ITEM codeBlock;
};

struct _gtnap_key_t
{
    int32_t hkey;
    vkey_t key;
    uint32_t modifiers;
};

struct _gtnap_column_t
{
    uint32_t fixed_width;
    uint32_t width;
    align_t align;
    String *title;
    PHB_ITEM codeBlock;
};

DeclSt(GtNapColumn);

struct _gtnap_area_t
{
    AREA *area;
    TableView *view;
    char_t temp[512];               // Temporal buffer between RDD and TableView
    uint32_t cache_recno;           // Store the DB recno while table drawing
    bool_t multisel;
    ArrSt(GtNapColumn) *columns;
    ArrSt(uint32_t) *records;       // Records visible in table (index, deleted, filters)
    PHB_ITEM whileCodeBlock;

};

struct _vecitem_t
{
    String *text;
    PHB_ITEM codeBlock;
    //S2Df size;
    uint32_t hoykey_pos;
    vkey_t hotkey;
    //uint32_t hotmodif;
    bool_t selected;
};

DeclSt(VecItem);

struct _gtnap_vector_t
{
    TableView *view;
    ArrSt(VecItem) *items;
    uint32_t width0;
    uint32_t width1;
    PHB_ITEM codeBlock0;
    PHB_ITEM codeBlock1;
    char_t temp[512];       // Temporal buffer between RDD and TableView
};

DeclPt(GtNapCallback);
DeclPt(GtNapArea);
DeclPt(Window);
DeclPt(Button);

typedef enum _objtype_t
{
    ekOBJ_LABEL,
    ekOBJ_EDIT,
    ekOBJ_BUTTON,
    ekOBJ_MENUVERT,
    ekOBJ_TABLEVIEW,
    ekOBJ_TEXTVIEW,
    ekOBJ_IMAGE
} objtype_t;

typedef enum _datatype_t
{
    ekTYPE_CHARACTER,
    ekTYPE_DATE
} datatype_t;

struct _gtnap_cualib_object_t
{
    objtype_t type;
    datatype_t dtype;
    int32_t cell_x;
    int32_t cell_y;
    V2Df pos;
    S2Df size;
    bool_t is_last_edit;
    bool_t is_on_edit_change;
    bool_t in_scroll_panel;
    PHB_ITEM labelCodeBlock;
    PHB_ITEM editCodeBlock;
    PHB_ITEM editableGlobalCodeBlock;
    PHB_ITEM editableLocalCodeBlock;
    PHB_ITEM mensCodeBlock;
    PHB_ITEM validaCodeBlock;
    PHB_ITEM listaCodeBlock;
    PHB_ITEM autoCodeBlock;
    PHB_ITEM getobjItem;
    GuiComponent *component;
};

struct _gtnap_cualib_toolbar_t
{
    uint32_t pixels_image;
    uint32_t pixels_button;
    ArrPt(Button) *buttons;
};

DeclSt(GtNapCualibObject);

struct _gtnap_cualib_window_t
{
    int32_t N_LinIni;
    int32_t N_ColIni;
    int32_t N_LinFin;
    int32_t N_ColFin;
    int32_t cursor_row;
    int32_t cursor_col;
    bool_t enter_tabstop;
    bool_t arrows_tabstop;
    bool_t stops_last_edit;
    bool_t scroll_panel;
    int32_t scroll_N_LinIni;
    int32_t scroll_N_ColIni;
    int32_t scroll_N_LinFin;
    int32_t scroll_N_ColFin;


    bool_t is_configured;
    bool_t is_closed_by_esc;
    bool_t focus_by_previous;
    bool_t tabstop_by_return_or_arrow;
    bool_t processing_invalid_date;
    bool_t modal_window_alive;
    bool_t buttons_navigation;
    uint32_t message_label_id;
    uint32_t default_button;
    Window *window;
    S2Df panel_size;
    Panel *panel;
    Panel *scrolled_panel;
    GtNapCualibToolbar *toolbar;
    GtNapArea *gtarea;
    GtNapVector *gtvector;
    PHB_ITEM confirmaCodeBlock;
    PHB_ITEM errorDataCodeBlock;
    ArrSt(GtNapCualibObject) *gui_objects;
    ArrPt(GtNapCallback) *callbacks;
};

DeclSt(GtNapCualibWindow);

struct _gtnap_t
{
    bool_t cualib_mode;
    Font *global_font;
    Font *reduced_font;

    // Only for cualib-gtnap
    String *title;
    uint32_t rows;
    uint32_t cols;
    uint32_t cell_x_size;
    uint32_t cell_y_size;
    uint32_t label_y_size;
    uint32_t button_y_size;
    uint32_t edit_y_size;
    uint32_t linespacing;
    ArrSt(GtNapCualibWindow) *cualib_windows;

    // Only for pure-gtnap
    ArrPt(Window) *modals;
    ArrPt(Window) *windows;
    ArrPt(GtNapCallback) *callbacks;
    ArrPt(GtNapArea) *areas;
};

/*---------------------------------------------------------------------------*/

static GtNap *GTNAP_GLOBAL = NULL;
static PHB_ITEM INIT_CODEBLOCK = NULL;
static PHB_ITEM END_CODEBLOCK = NULL;

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

// These are internal, non-documented functions of NAppGUI.
// They are used for direct handling of widgets, avoiding the 'layout' layer.
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

__END_C

/*---------------------------------------------------------------------------*/

static GtNap *i_nappgui_create(void)
{
    PHB_ITEM pRet = NULL;
    log_printf("i_nappgui_create()");
    GTNAP_GLOBAL = heap_new0(GtNap);
    GTNAP_GLOBAL->global_font = font_system(font_regular_size(), 0);
    GTNAP_GLOBAL->modals = arrpt_create(Window);
    GTNAP_GLOBAL->windows = arrpt_create(Window);
    GTNAP_GLOBAL->callbacks = arrpt_create(GtNapCallback);
    GTNAP_GLOBAL->areas = arrpt_create(GtNapArea);
    pRet = hb_itemDo(INIT_CODEBLOCK, 0);
    hb_itemRelease(pRet);
    hb_itemRelease(INIT_CODEBLOCK);
    INIT_CODEBLOCK = NULL;
    return GTNAP_GLOBAL;
}

/*---------------------------------------------------------------------------*/

static void i_destroy_callback(GtNapCallback **callback)
{
    cassert_no_null(callback);
    cassert_no_null(*callback);
    if ((*callback)->codeBlock != NULL)
        hb_itemRelease((*callback)->codeBlock);

    heap_delete(callback, GtNapCallback);
}

/*---------------------------------------------------------------------------*/

static void i_remove_column(GtNapColumn *column)
{
    cassert_no_null(column);
    str_destopt(&column->title);
    if (column->codeBlock != NULL)
        hb_itemRelease(column->codeBlock);
}

/*---------------------------------------------------------------------------*/

static void i_destroy_area(GtNapArea **area)
{
    cassert_no_null(area);
    cassert_no_null(*area);
    arrst_destroy(&(*area)->columns, i_remove_column, GtNapColumn);
    arrst_destroy(&(*area)->records, NULL, uint32_t);

    if ((*area)->whileCodeBlock)
    {
        hb_itemRelease((*area)->whileCodeBlock);
        (*area)->whileCodeBlock = NULL;
    }

    heap_delete(area, GtNapArea);
}

/*---------------------------------------------------------------------------*/

static void i_remove_item(VecItem *item)
{
    str_destroy(&item->text);

    if (item->codeBlock)
    {
        hb_itemRelease(item->codeBlock);
        item->codeBlock = NULL;
    }
}

/*---------------------------------------------------------------------------*/

static void i_destroy_vector(GtNapVector **vector)
{
    cassert_no_null(vector);
    cassert_no_null(*vector);
    arrst_destroy(&(*vector)->items, i_remove_item, VecItem);

    if ((*vector)->codeBlock0 != NULL)
        hb_itemRelease((*vector)->codeBlock0);

    if ((*vector)->codeBlock1 != NULL)
        hb_itemRelease((*vector)->codeBlock1);

    heap_delete(vector, GtNapVector);
}

/*---------------------------------------------------------------------------*/

static void i_remove_window_callbacks(Window *window)
{
    uint32_t i, n = arrpt_size(GTNAP_GLOBAL->callbacks, GtNapCallback);
    cassert_no_null(window);
    for(i = 0; i < n;)
    {
        bool_t remove = FALSE;
        GtNapCallback *callback = arrpt_get(GTNAP_GLOBAL->callbacks, i, GtNapCallback);
        if (callback->cb_window == window)
        {
            remove = TRUE;
        }
        else if (callback->cb_component != NULL)
        {
            if (_component_window(callback->cb_component) == window)
                remove = TRUE;
        }

        if (remove == TRUE)
        {
            arrpt_delete(GTNAP_GLOBAL->callbacks, i, i_destroy_callback, GtNapCallback);
            n -= 1;
        }
        else
        {
            i += 1;
        }
    }
}

/*---------------------------------------------------------------------------*/

static void i_remove_window_areas(Window *window)
{
    uint32_t i, n = arrpt_size(GTNAP_GLOBAL->areas, GtNapArea);
    cassert_no_null(window);
    for(i = 0; i < n;)
    {
        bool_t remove = FALSE;
        GtNapArea *area = arrpt_get(GTNAP_GLOBAL->areas, i, GtNapArea);

        if (_component_window((GuiComponent*)area->view) == window)
            remove = TRUE;

        if (remove == TRUE)
        {
            arrpt_delete(GTNAP_GLOBAL->areas, i, i_destroy_area, GtNapArea);
            n -= 1;
        }
        else
        {
            i += 1;
        }
    }
}

/*---------------------------------------------------------------------------*/

static void i_window_destroy(Window **window)
{
    cassert_no_null(*window);
    i_remove_window_callbacks(*window);
    i_remove_window_areas(*window);
    window_destroy(window);
}

/*---------------------------------------------------------------------------*/

static void i_nappgui_destroy(GtNap **data)
{
    PHB_ITEM pRet = NULL;
    cassert_no_null(data);
    cassert_no_null(*data);
    log_printf("i_nappgui_destroy()");
    font_destroy(&(*data)->global_font);
    cassert((*data)->reduced_font == NULL);
    arrpt_destopt(&(*data)->windows, i_window_destroy, Window);
    // No modal window can be alive here!
    cassert(arrpt_size((*data)->modals, Window) == 0);
    arrpt_destopt(&(*data)->modals, NULL, Window);
    arrpt_destopt(&(*data)->callbacks, i_destroy_callback, GtNapCallback);
    arrpt_destopt(&(*data)->areas, i_destroy_area, GtNapArea);
    pRet = hb_itemDo(END_CODEBLOCK, 0);
    hb_itemRelease(pRet);
    hb_itemRelease(END_CODEBLOCK);
    END_CODEBLOCK = NULL;
    heap_delete(data, GtNap);
    GTNAP_GLOBAL = NULL;
}

/*---------------------------------------------------------------------------*/

void hb_gtnap_runloop( void )
{
    void *hInstance = NULL;
    PHB_ITEM codeBlock_begin = hb_param(1, HB_IT_BLOCK);
    PHB_ITEM codeBlock_end = hb_param(2, HB_IT_BLOCK);

#if defined( HB_OS_WIN )
    hb_winmainArgGet(&hInstance, NULL, NULL);
#endif

    log_printf("hb_gtnap_runloop()");
    INIT_CODEBLOCK = hb_itemNew(codeBlock_begin);
    END_CODEBLOCK = hb_itemNew(codeBlock_end);

    osmain_imp(
                0, NULL, hInstance, 0.,
                (FPtr_app_create)i_nappgui_create,
                (FPtr_app_update)NULL,
                (FPtr_destroy)i_nappgui_destroy,
                (char_t*)"");
}

/*---------------------------------------------------------------------------*/

void hb_gtnap_set_global_font(Font *font)
{
    cassert_no_null(GTNAP_GLOBAL);
    font_destroy(&GTNAP_GLOBAL->global_font);
    GTNAP_GLOBAL->global_font = font;
}

/*---------------------------------------------------------------------------*/

Font *hb_gtnap_global_font(void)
{
    cassert_no_null(GTNAP_GLOBAL);
    return GTNAP_GLOBAL->global_font;
}

/*---------------------------------------------------------------------------*/

Window *hb_gtnap_main_window(void)
{
    Window *window = NULL;
    cassert_no_null(GTNAP_GLOBAL);
    window = arrpt_get(GTNAP_GLOBAL->windows, 0, Window);
    cassert_no_null(window);
    return window;
}

/*---------------------------------------------------------------------------*/

Window *hb_gtnap_current_modal(void)
{
    Window *modal = NULL;
    cassert_no_null(GTNAP_GLOBAL);
    if (arrpt_size(GTNAP_GLOBAL->modals, Window) > 0)
        modal = arrpt_last(GTNAP_GLOBAL->modals, Window);

    return modal;
}

/*---------------------------------------------------------------------------*/

void hb_gtnap_set_modal_window(Window *window)
{
    cassert_no_null(GTNAP_GLOBAL);
    arrpt_append(GTNAP_GLOBAL->modals, window, Window);
}

/*---------------------------------------------------------------------------*/

void hb_gtnap_destroy_modal(void)
{
    Window *modal = NULL;
    uint32_t i = 0;
    cassert_no_null(GTNAP_GLOBAL);
    modal = arrpt_last(GTNAP_GLOBAL->modals, Window);
    i = arrpt_find(GTNAP_GLOBAL->windows, modal, Window);
    arrpt_delete(GTNAP_GLOBAL->windows, i, i_window_destroy, Window);
    arrpt_pop(GTNAP_GLOBAL->modals, NULL, Window);
}

/*---------------------------------------------------------------------------*/

GtNapArea *hb_gtnap_new_area(TableView *view)
{
    GtNapArea *gtarea = heap_new0(GtNapArea);
    gtarea->area = (AREA*)hb_rddGetCurrentWorkAreaPointer();
    gtarea->view = view;
    gtarea->columns = arrst_create(GtNapColumn);
    arrpt_append(GTNAP_GLOBAL->areas, gtarea, GtNapArea);
    return gtarea;
}

/*---------------------------------------------------------------------------*/

GtNapArea *hb_gtnap_get_area(TableView *view)
{
    arrpt_foreach(gtarea, GTNAP_GLOBAL->areas, GtNapArea)
        if (gtarea->view == view)
            return gtarea;
    arrpt_end();
    return NULL;
}

/*---------------------------------------------------------------------------*/

void hb_gtnap_area_add_column(GtNapArea *area, const char_t *title, const real32_t width, const align_t align, PHB_ITEM codeBlock)
{
    uint32_t id = 0;
    GtNapColumn *column = NULL;
    cassert_no_null(area);
    id = tableview_new_column_text(area->view);
    tableview_header_title(area->view, id, title);
    tableview_header_align(area->view, id, align);
    tableview_column_width(area->view, id, width);
    cassert(id == arrst_size(area->columns, GtNapColumn));
    column = arrst_new0(area->columns, GtNapColumn);
    column->fixed_width = UINT32_MAX;
    column->align = align;
    column->codeBlock = hb_itemNew(codeBlock);
}

/*---------------------------------------------------------------------------*/

uint32_t hb_gtnap_area_row_count(GtNapArea *area)
{
    cassert_no_null(area);
    return arrst_size(area->records, uint32_t);
}

/*---------------------------------------------------------------------------*/

const char_t *hb_gtnap_area_eval_field(GtNapArea *area, const uint32_t field_id, const uint32_t row_id, align_t *align)
{
    uint32_t recno = 0;
    const GtNapColumn *column = NULL;
    PHB_ITEM pItem = NULL;
    HB_TYPE type = 0;

    cassert_no_null(area);
    cassert(field_id > 0);

    /* Go to DB record */
    recno = *arrst_get_const(area->records, row_id, uint32_t);
    SELF_GOTO(area->area, recno);

    /* Get the table column */
    column = arrst_get_const(area->columns, field_id - 1, GtNapColumn);

    /* CodeBlock that computes the cell content */
    pItem = hb_itemDo(column->codeBlock, 0);

    /* Fill the temporal cell buffer with cell result */
    type = HB_ITEM_TYPE(pItem);
    area->temp[0] = '\0';

    if (type == HB_IT_STRING)
    {
        hb_itemCopyStrUTF8(pItem, area->temp, sizeof(area->temp));
    }
    else if (type == HB_IT_DATE)
    {
        char date[16];
        hb_itemGetDS(pItem, date);
        hb_dateFormat(date, area->temp, "DD/MM/YYYY");
    }
    else if (type == HB_IT_DOUBLE)
    {
        double value = hb_itemGetND(pItem);
        bstd_sprintf(area->temp, sizeof(area->temp), "%12.4f", value);
    }

    hb_itemRelease(pItem);

    if (align != NULL)
        *align = column->align;

    return area->temp;
}

/*---------------------------------------------------------------------------*/

const char_t *hb_gtnap_vector_eval_field(GtNapVector *vector, const uint32_t field_id, const uint32_t row_id)
{
    const VecItem *item = NULL;
    cassert_no_null(vector);
    item = arrst_get_const(vector->items, row_id, VecItem);
    if (field_id == 0)
    {
        if (item->selected == TRUE)
        {
            // � in UTF8
            vector->temp[0] = 194;
            vector->temp[1] = 187;
            vector->temp[2] = 0;
            return vector->temp;
        }
        else
        {
            return "";
        }
    }
    else if (field_id == 1)
    {
        return tc(item->text);
    }
    else
    {
        cassert(field_id == 0 || field_id == 1);
        return "";
    }
}

/*---------------------------------------------------------------------------*/

uint32_t hb_gtnap_vector_items_count(GtNapVector *vector)
{
    cassert_no_null(vector);
    return arrst_size(vector->items, VecItem);
}

/*---------------------------------------------------------------------------*/

char_t* hb_gtnap_area_temp(GtNapArea *area, uint32_t *size)
{
    cassert_no_null(area);
    *size = sizeof(area->temp);
    return area->temp;
}

/*---------------------------------------------------------------------------*/

void* hb_gtnap_area(GtNapArea *area)
{
    cassert_no_null(area);
    return area->area;
}

/*---------------------------------------------------------------------------*/

static HB_GARBAGE_FUNC( s_gc_Image_destroy )
{
    Image **ph = (Image**)Cargo;
    if (ph && *ph)
    {
        // image_destroy set 'ph' to NULL
        image_destroy(ph);
    }
}

/*---------------------------------------------------------------------------*/

static HB_GARBAGE_FUNC( s_gc_Font_destroy )
{
    Font **ph = (Font**)Cargo;
    if (ph && *ph)
    {
        // font_destroy set 'ph' to NULL
        font_destroy(ph);
    }
}

/*---------------------------------------------------------------------------*/

static HB_GARBAGE_FUNC( s_gc_Window_destroy )
{
    Window **ph = (Window**)Cargo;
    if (ph && *ph)
    {
        // window_destroy set 'ph' to NULL
        //window_destroy(ph);
    }
}

/*---------------------------------------------------------------------------*/

static const HB_GC_FUNCS s_gc_Image_funcs =
{
    s_gc_Image_destroy,
    hb_gcDummyMark
};

static const HB_GC_FUNCS s_gc_Font_funcs =
{
    s_gc_Font_destroy,
    hb_gcDummyMark
};

static const HB_GC_FUNCS s_gc_Window_funcs =
{
    s_gc_Window_destroy,
    hb_gcDummyMark
};

/*---------------------------------------------------------------------------*/

const char_t *hb_gtnap_parText(const uint32_t iParam)
{
    static char_t TEMP_TEXT[1024 + 1];

    if (HB_ISCHAR(iParam))
    {
        const char_t *str = hb_parcx(iParam);
        HB_SIZE i, j, size = hb_parclen(iParam);

        for (i = 0, j = 0; i < size && j < 1024; )
        {
            if (str[i] != 13)
            {
                TEMP_TEXT[j] = str[i];
                i += 1;
                j += 1;
            }
            else
            {
                i += 1;
            }
        }

        TEMP_TEXT[j] = '\0';
        return TEMP_TEXT;
    }
    else
    {
        return "Unknown text"; // (const char_t*)hb_parni(iParam);
    }
}

/*---------------------------------------------------------------------------*/

Image *hb_gtnap_parImage(int iParam)
{
    void **ph = (void**)hb_parptrGC(&s_gc_Image_funcs, iParam);
    return *((Image**)ph);
}

/*---------------------------------------------------------------------------*/

Font *hb_gtnap_parFont(int iParam)
{
    void **ph = (void**)hb_parptrGC(&s_gc_Font_funcs, iParam);
    return *((Font**)ph);
}

/*---------------------------------------------------------------------------*/

Window *hb_gtnap_parWindow(int iParam)
{
    void **ph = (void**)hb_parptrGC(&s_gc_Window_funcs, iParam);
    return *((Window**)ph);
}

/*---------------------------------------------------------------------------*/

void hb_gtnap_retImageGC(Image *image)
{
    if (image != NULL)
    {
        void **ph = (void**)hb_gcAllocate(sizeof(Image*), &s_gc_Image_funcs);
        *ph = image;
        hb_retptrGC(ph);
    }
    else
    {
        hb_retptr(NULL);
    }
}

/*---------------------------------------------------------------------------*/

void hb_gtnap_retFontGC(Font *font)
{
    if (font != NULL)
    {
        void **ph = (void**)hb_gcAllocate(sizeof(Font*), &s_gc_Font_funcs);
        *ph = font;
            log_printf("'hb_gtnap_retFontGC': %p - %p", ph, font);
        hb_retptrGC(ph);
    }
    else
    {
        hb_retptr(NULL);
    }
}

/*---------------------------------------------------------------------------*/

void hb_gtnap_retWindowGC(Window *window)
{
    if (window != NULL)
    {
        void **ph = (void**)hb_gcAllocate(sizeof(Window*), &s_gc_Window_funcs);
        *ph = window;
        cassert_no_null(GTNAP_GLOBAL);
        arrpt_append(GTNAP_GLOBAL->windows, window, Window);
        hb_retptrGC(ph);
    }
    else
    {
        hb_retptr(NULL);
    }
}

/*---------------------------------------------------------------------------*/

Listener *hb_gtnap_comp_listener(const uint32_t codeBlockParamId, GuiComponent *component, FPtr_gtnap_callback func_callback)
{
    PHB_ITEM codeBlock = hb_param(codeBlockParamId, HB_IT_BLOCK);
    GtNapCallback *callback = heap_new0(GtNapCallback);
    cassert_no_null(codeBlock);
    callback->codeBlock = hb_itemNew(codeBlock);
    callback->cb_component = component;
    callback->key = INT32_MAX;
    arrpt_append(GTNAP_GLOBAL->callbacks, callback, GtNapCallback);
    return listener(callback, func_callback, GtNapCallback);
}

/*---------------------------------------------------------------------------*/

Listener *hb_gtnap_wind_listener(const uint32_t codeBlockParamId, Window *window, FPtr_gtnap_callback func_callback)
{
    PHB_ITEM codeBlock = hb_param(codeBlockParamId, HB_IT_BLOCK);
    GtNapCallback *callback = heap_new0(GtNapCallback);
    cassert_no_null(codeBlock);
    callback->codeBlock = hb_itemNew(codeBlock);
    callback->cb_window = window;
    callback->key = INT32_MAX;
    arrpt_append(GTNAP_GLOBAL->callbacks, callback, GtNapCallback);
    return listener(callback, func_callback, GtNapCallback);
}

/*---------------------------------------------------------------------------*/

void hb_gtnap_callback(GtNapCallback *callback, Event *e)
{
    cassert_no_null(callback);
    unref(e);
    if (callback->codeBlock != NULL)
    {
        PHB_ITEM phiEvent = hb_itemNew(NULL);
        PHB_ITEM retItem = NULL;
        hb_itemPutPtr(phiEvent, e);
        cassert_msg(e != NULL, "hb_gtnap_callback: NULL Event");
        retItem = hb_itemDo(callback->codeBlock, 1, phiEvent);
        hb_itemRelease(phiEvent);
        hb_itemRelease(retItem);
    }
}

/*---------------------------------------------------------------------------*/

bool_t hb_gtnap_callback_bool(GtNapCallback *callback, Event *e)
{
    bool_t ret = FALSE;
    cassert_no_null(callback);
    unref(e);
    if (callback->codeBlock != NULL)
    {
        PHB_ITEM retItem = hb_itemDo(callback->codeBlock, 0);
        HB_TYPE type = HB_ITEM_TYPE(retItem);
        cassert(type == HB_IT_LOGICAL);
        ret = (bool_t)hb_itemGetL(retItem);
        hb_itemRelease(retItem);
    }

    return ret;
}

/*---------------------------------------------------------------------------*/
//
// CUALIB Support in GTNAP
//
/*---------------------------------------------------------------------------*/

static PHB_ITEM CUALIB_INIT_CODEBLOCK = NULL;
static char_t CUALIB_TITLE[128];
static uint32_t CUALIB_ROWS = 0;
static uint32_t CUALIB_COLS = 0;

/*---------------------------------------------------------------------------*/

String *hb_gtnap_cualib_parText(const uint32_t iParam)
{
    // TODO: Translate code-page to UTF8
    if (!HB_ISNIL(iParam))
    {
        if (HB_ISCHAR(iParam))
        {
            const char_t *str = hb_parcx(iParam);
            return gtconvert_1252_to_UTF8(str);
        }
        else
        {
            // const char_t *str = (const char_t*)hb_parni(iParam);
            // return str;

            return str_c("Unknown text"); // (const char_t*)hb_parni(iParam);
        }
    }

    return str_c("");
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

    log_printf("Cell height: %d. Label height: %d Button height: %d Edit height: %d", gtnap->cell_y_size, gtnap->label_y_size, gtnap->button_y_size, gtnap->edit_y_size);
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

static void i_compute_font_size(const uint32_t max_width, const uint32_t max_height, GtNap *gtnap)
{
    real32_t font_size = (real32_t)(max_height / (gtnap->rows + 5));

    for(;;)
    {
        i_create_fonts(font_size, gtnap);
        i_compute_cell_size(gtnap);
        log_printf("Computed font: %.2f: Total width: %d, Total height: %d", font_size, gtnap->cell_x_size * gtnap->cols, gtnap->cell_y_size * gtnap->rows);

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

static GtNap *i_gtnap_cualib_create(void)
{
    S2Df screen;
    GTNAP_GLOBAL = heap_new0(GtNap);
    GTNAP_GLOBAL->cualib_mode = TRUE;
    GTNAP_GLOBAL->title = gtconvert_1252_to_UTF8(CUALIB_TITLE);
    GTNAP_GLOBAL->rows = CUALIB_ROWS;
    GTNAP_GLOBAL->cols = CUALIB_COLS;
    GTNAP_GLOBAL->linespacing = 0;
    GTNAP_GLOBAL->cualib_windows = arrst_create(GtNapCualibWindow);
    globals_resolution(&screen);
    screen.height -= 50; // S.O. Dock or Taskbars
    i_compute_font_size((uint32_t)screen.width, (uint32_t)screen.height, GTNAP_GLOBAL);
    log_printf("i_gtnap_cualib_create(%s, %d, %d)", CUALIB_TITLE, CUALIB_ROWS, CUALIB_COLS);
    log_printf("GTNAP Cell Size(%d, %d)", GTNAP_GLOBAL->cell_x_size, GTNAP_GLOBAL->cell_y_size);

    {
        PHB_ITEM pRet = NULL;
        pRet = hb_itemDo(CUALIB_INIT_CODEBLOCK, 0);
        hb_itemRelease(pRet);
        hb_itemRelease(CUALIB_INIT_CODEBLOCK);
        CUALIB_INIT_CODEBLOCK = NULL;
    }

    return GTNAP_GLOBAL;
}

/*---------------------------------------------------------------------------*/

static void i_remove_cualib_object(GtNapCualibWindow *cuawin, const uint32_t index)
{
    GtNapCualibObject *object = NULL;
    const char_t *type = NULL;
    cassert_no_null(cuawin);
    object = arrst_get(cuawin->gui_objects, index, GtNapCualibObject);

    type = _component_type(object->component);
    log_printf("Remove cualib object: %s", type);
    if (str_equ_c(type, "Panel") == TRUE)
        _panel_detach_components((Panel*)object->component);

    _component_visible(object->component, FALSE);

    if (object->in_scroll_panel == TRUE)
        _component_detach_from_panel((GuiComponent*)cuawin->scrolled_panel, object->component);
    else
        _component_detach_from_panel((GuiComponent*)cuawin->panel, object->component);

    _component_destroy(&object->component);

    if (object->labelCodeBlock != NULL)
        hb_itemRelease(object->labelCodeBlock);

    if (object->editCodeBlock != NULL)
        hb_itemRelease(object->editCodeBlock);

    if (object->editableGlobalCodeBlock != NULL)
        hb_itemRelease(object->editableGlobalCodeBlock);

    if (object->editableLocalCodeBlock != NULL)
        hb_itemRelease(object->editableLocalCodeBlock);

    if (object->mensCodeBlock != NULL)
        hb_itemRelease(object->mensCodeBlock);

    if (object->validaCodeBlock != NULL)
        hb_itemRelease(object->validaCodeBlock);

    if (object->listaCodeBlock != NULL)
        hb_itemRelease(object->listaCodeBlock);

    if (object->autoCodeBlock != NULL)
        hb_itemRelease(object->autoCodeBlock);

    if (object->getobjItem != NULL)
        hb_itemRelease(object->getobjItem);

    arrst_delete(cuawin->gui_objects, index, NULL, GtNapCualibObject);
}

/*---------------------------------------------------------------------------*/

static void i_remove_toolbar(GtNapCualibToolbar *toolbar, Panel *panel)
{
    cassert_no_null(toolbar);
    arrpt_foreach(button, toolbar->buttons, Button)
        if (button != NULL)
        {
            Button *dbutton = button;
            _component_detach_from_panel((GuiComponent*)panel, (GuiComponent*)button);
            _component_destroy((GuiComponent**)&dbutton);
        }
    arrpt_end();
    arrpt_destroy(&toolbar->buttons, NULL, Button);
}

/*---------------------------------------------------------------------------*/

static void i_remove_cualib_win(GtNapCualibWindow *cuawin)
{
    uint32_t i, n;
    cassert_no_null(cuawin);
    n = arrst_size(cuawin->gui_objects, GtNapCualibObject);
    for (i = 0; i < n; ++i)
        i_remove_cualib_object(cuawin, 0);

    if (cuawin->scrolled_panel != NULL)
    {
        _component_visible((GuiComponent*)cuawin->scrolled_panel, FALSE);
        _component_detach_from_panel((GuiComponent*)cuawin->panel, (GuiComponent*)cuawin->scrolled_panel);
        _component_destroy((GuiComponent**)&cuawin->scrolled_panel);
    }

    if (cuawin->toolbar != NULL)
    {
        i_remove_toolbar(cuawin->toolbar, cuawin->panel);
        heap_delete(&cuawin->toolbar, GtNapCualibToolbar);
    }

    if (cuawin->gtarea != NULL)
        i_destroy_area(&cuawin->gtarea);

    if (cuawin->gtvector != NULL)
        i_destroy_vector(&cuawin->gtvector);

    if (cuawin->confirmaCodeBlock != NULL)
        hb_itemRelease(cuawin->confirmaCodeBlock);

    if (cuawin->errorDataCodeBlock != NULL)
        hb_itemRelease(cuawin->errorDataCodeBlock);

    cassert(arrst_size(cuawin->gui_objects, GtNapCualibObject) == 0);
    arrst_destroy(&cuawin->gui_objects, NULL, GtNapCualibObject);
    arrpt_destroy(&cuawin->callbacks, i_destroy_callback, GtNapCallback);
}

/*---------------------------------------------------------------------------*/

static void i_gtnap_cualib_destroy(GtNap **data)
{
    cassert_no_null(data);
    cassert_no_null(*data);
    log_printf("i_gtnap_cualib_destroy()");
    cassert(*data == GTNAP_GLOBAL);
    cassert(GTNAP_GLOBAL->cualib_mode == TRUE);
    cassert(arrst_size(GTNAP_GLOBAL->cualib_windows, GtNapCualibWindow) == 0);
    arrst_destroy(&GTNAP_GLOBAL->cualib_windows, i_remove_cualib_win, GtNapCualibWindow);
    font_destroy(&GTNAP_GLOBAL->global_font);
    font_destroy(&(*data)->reduced_font);
    str_destroy(&GTNAP_GLOBAL->title);
    heap_delete(&GTNAP_GLOBAL, GtNap);
}

/*---------------------------------------------------------------------------*/

void hb_gtnap_cualib_init_log(void)
{
    osbs_start();
    log_output(FALSE, FALSE);
    log_file("C:\\Users\\USUARIO\\AppData\\Roaming\\exemplo\\log2.txt");
}

/*---------------------------------------------------------------------------*/

void hb_gtnap_cualib_setup(const char_t *title, const uint32_t rows, const uint32_t cols, PHB_ITEM codeBlock_begin)
{
    void *hInstance = NULL;

#if defined( HB_OS_WIN )
    hb_winmainArgGet(&hInstance, NULL, NULL);
#endif

    log_printf("hb_gtnap_cualib_setup()");
    CUALIB_INIT_CODEBLOCK = hb_itemNew(codeBlock_begin);
    str_copy_c(CUALIB_TITLE, sizeof(CUALIB_TITLE), title);
    CUALIB_ROWS = rows;
    CUALIB_COLS = cols;

    osmain_imp(
                0, NULL, hInstance, 0.,
                (FPtr_app_create)i_gtnap_cualib_create,
                (FPtr_app_update)NULL,
                (FPtr_destroy)i_gtnap_cualib_destroy,
                (char_t*)"");
}

/*---------------------------------------------------------------------------*/

uint32_t hb_gtnap_cualib_linespacing(void)
{
    return GTNAP_GLOBAL->linespacing;
}

/*---------------------------------------------------------------------------*/

void hb_gtnap_cualib_set_linespacing(const uint32_t spacing)
{
    GTNAP_GLOBAL->linespacing = spacing;
}

/*---------------------------------------------------------------------------*/

static GtNapCualibWindow *i_current_cuawin(GtNap *gtnap)
{
    uint32_t id = 0;
    cassert_no_null(gtnap);
    id = arrst_size(gtnap->cualib_windows, GtNapCualibWindow);
    if (id >= 1)
        return arrst_get(gtnap->cualib_windows, id - 1, GtNapCualibWindow);
    else
        return NULL;
}

/*---------------------------------------------------------------------------*/

static GtNapCualibWindow *i_parent_cuawin(GtNap *gtnap)
{
    uint32_t id = 0;
    cassert_no_null(gtnap);
    id = arrst_size(gtnap->cualib_windows, GtNapCualibWindow);
    if (id >= 2)
        return arrst_get(gtnap->cualib_windows, id - 2, GtNapCualibWindow);
    else
        return NULL;
}

/*---------------------------------------------------------------------------*/

// static void i_OnWindowClose(GtNap *gtnap, Event *e)
// {
//     GtNapCualibWindow *cuawin = i_current_cuawin(gtnap);
//     uint32_t i, n;
//     cassert_no_null(cuawin);
//     n = arrst_size(cuawin->gui_objects, GtNapCualibObject);
//     for (i = 0; i < n; ++i)
//         i_remove_cualib_object(cuawin, 0);
//     unref(e);
// }

/*---------------------------------------------------------------------------*/

static void i_add_object(const objtype_t type, const int32_t cell_x, const int32_t cell_y, const uint32_t cell_x_size, const uint32_t cell_y_size, const S2Df *size, const bool_t in_scroll_panel, GuiComponent *component, GtNapCualibWindow *cuawin)
{
    GtNapCualibObject *object = NULL;
    cassert_no_null(cuawin);
    cassert_no_null(size);
    object = arrst_new0(cuawin->gui_objects, GtNapCualibObject);
    object->type = type;
    object->cell_x = cell_x;
    object->cell_y = cell_y;
    object->component = component;
    object->pos.x = (real32_t)(cell_x * (int32_t)cell_x_size);
    object->pos.y = (real32_t)(cell_y * (int32_t)cell_y_size);
    object->size = *size;
    object->is_last_edit = FALSE;
    object->is_on_edit_change = FALSE;
    object->in_scroll_panel = in_scroll_panel;
    object->labelCodeBlock = NULL;
    object->editCodeBlock = NULL;
    object->editableGlobalCodeBlock = NULL;
    object->editableLocalCodeBlock = NULL;
    object->mensCodeBlock = NULL;
    object->validaCodeBlock = NULL;
    object->listaCodeBlock = NULL;
    object->autoCodeBlock = NULL;
    object->getobjItem = NULL;
    log_printf("Added object at: %.2f, %.2f w:%.2f h:%.2f", object->pos.x, object->pos.y, object->size.width, object->size.height);
}

/*---------------------------------------------------------------------------*/

static void i_add_label_object(const int32_t cell_x, const int32_t cell_y, const char_t *text, const color_t background, const bool_t in_scroll_panel, GtNap *gtnap, GtNapCualibWindow *cuawin)
{
    Label *label = label_create();
    S2Df size;
    cassert_no_null(gtnap);
    label_font(label, gtnap->global_font);

    if (background != 0)
        label_bgcolor(label, background);

    if (str_empty_c(text) == FALSE)
    {
        uint32_t len = str_len_c(text);
        String *ctext = gtconvert_1252_to_UTF8(text);
        label_text(label, tc(ctext));
        size.width = (real32_t)(len * gtnap->cell_x_size);
        log_printf("Added label: '%s' at %d %d", tc(ctext), cell_x, cell_y);
        str_destroy(&ctext);
    }
    else
    {
        size.width = (real32_t)(1 * gtnap->cell_x_size);
        log_printf("Added label: 'NO TEXT' at %d %d", cell_x, cell_y);
    }

    size.height = (real32_t)gtnap->label_y_size;
    i_add_object(ekOBJ_LABEL, cell_x, cell_y, gtnap->cell_x_size, gtnap->cell_y_size, &size, in_scroll_panel, (GuiComponent*)label, cuawin);
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

uint32_t hb_gtnap_cualib_window(const int32_t N_LinIni, const int32_t N_ColIni, const int32_t N_LinFin, const int32_t N_ColFin, const char_t *C_Cabec, const bool_t close_return, const bool_t close_esc, const bool_t minimize_button, const bool_t buttons_navigation)
{
    uint32_t id = UINT32_MAX;
    GtNapCualibWindow *cuawin = NULL;
    uint32_t flags = i_window_flags(close_return, close_esc, minimize_button);
    Window *window = window_create(flags);
    Panel *panel = panel_create();
    id = arrst_size(GTNAP_GLOBAL->cualib_windows, GtNapCualibWindow);
    cuawin = arrst_new0(GTNAP_GLOBAL->cualib_windows, GtNapCualibWindow);
    cuawin->window = window;
    cuawin->panel = panel;
    cuawin->N_LinIni = N_LinIni;
    cuawin->N_ColIni = N_ColIni;
    cuawin->N_LinFin = N_LinFin;
    cuawin->N_ColFin = N_ColFin;
    cuawin->cursor_row = 0;
    cuawin->cursor_col = 0;
    cuawin->enter_tabstop = FALSE;
    cuawin->arrows_tabstop = FALSE;
    cuawin->stops_last_edit = FALSE;
    cuawin->scroll_panel = FALSE;
    cuawin->is_configured = FALSE;
    cuawin->is_closed_by_esc = FALSE;
    cuawin->focus_by_previous = FALSE;
    cuawin->tabstop_by_return_or_arrow = FALSE;
    cuawin->processing_invalid_date = FALSE;
    cuawin->modal_window_alive = FALSE;
    cuawin->message_label_id = UINT32_MAX;
    cuawin->default_button = UINT32_MAX;
    cuawin->gui_objects = arrst_create(GtNapCualibObject);
    cuawin->callbacks = arrpt_create(GtNapCallback);
    cuawin->panel_size.width = (real32_t)(GTNAP_GLOBAL->cell_x_size * (cuawin->N_ColFin - cuawin->N_ColIni + 1));
    cuawin->panel_size.height = (real32_t)(GTNAP_GLOBAL->cell_y_size * (cuawin->N_LinFin - cuawin->N_LinIni + 1));
    cuawin->confirmaCodeBlock = NULL;
    cuawin->errorDataCodeBlock = NULL;
    cuawin->buttons_navigation = buttons_navigation;

    if (str_empty_c(C_Cabec) == FALSE)
        window_title(window, C_Cabec);
    else
        window_title(window, tc(GTNAP_GLOBAL->title));

    window_cycle_tabstop(window, FALSE);
    log_printf("Created new CUALIB Window: %d, %d, %d, %d", N_LinIni, N_ColIni, N_LinFin, N_ColFin);
    return id;
}

/*---------------------------------------------------------------------------*/

void hb_gtnap_cualib_window_enter_tabstop(void)
{
    GtNapCualibWindow *cuawin = i_current_cuawin(GTNAP_GLOBAL);
    cuawin->enter_tabstop = TRUE;
}

/*---------------------------------------------------------------------------*/

void hb_gtnap_cualib_window_arrows_tabstop(void)
{
    GtNapCualibWindow *cuawin = i_current_cuawin(GTNAP_GLOBAL);
    cuawin->arrows_tabstop = TRUE;
}

/*---------------------------------------------------------------------------*/

void hb_gtnap_cualib_window_stops_last_edit(void)
{
    GtNapCualibWindow *cuawin = i_current_cuawin(GTNAP_GLOBAL);
    cuawin->stops_last_edit = TRUE;
}

/*---------------------------------------------------------------------------*/

void hb_gtnap_cualib_add_message_label(const int32_t N_LinIni, const int32_t N_ColIni)
{
    GtNapCualibWindow *cuawin = i_current_cuawin(GTNAP_GLOBAL);
    uint32_t id = arrst_size(cuawin->gui_objects, GtNapCualibObject);
    i_add_label_object(N_ColIni - cuawin->N_ColIni, N_LinIni - cuawin->N_LinIni, "--MENS--", 0, FALSE, GTNAP_GLOBAL, cuawin);
    cassert(cuawin->message_label_id == UINT32_MAX);
    cuawin->message_label_id = id;
}

/*---------------------------------------------------------------------------*/

void hb_gtnap_cualib_window_scroll_panel(const int32_t N_LinIni, const int32_t N_ColIni, const int32_t N_LinFin, const int32_t N_ColFin)
{
    GtNapCualibWindow *cuawin = i_current_cuawin(GTNAP_GLOBAL);
    cuawin->scroll_panel = TRUE;
    cuawin->scroll_N_LinIni = N_LinIni;
    cuawin->scroll_N_ColIni = N_ColIni;
    cuawin->scroll_N_LinFin = N_LinFin;
    cuawin->scroll_N_ColFin = N_ColFin;
}

/*---------------------------------------------------------------------------*/

void hb_gtnap_cualib_menuvert(Panel *panel, const int32_t nTop, const int32_t nLeft, const int32_t nBottom, const int32_t nRight)
{
    GtNapCualibWindow *cuawin = i_current_cuawin(GTNAP_GLOBAL);
    S2Df size, final_size;
    cassert_no_null(cuawin);
    log_printf("Added MenuVert into CUALIB Window: %d, %d, %d, %d", nTop, nLeft, nBottom, nRight);
    size.width = (real32_t)((nRight - nLeft + 1) * GTNAP_GLOBAL->cell_x_size);
    size.height = (real32_t)((nBottom - nTop + 1) * GTNAP_GLOBAL->cell_y_size);
    _panel_compose(panel, &size, &final_size);
    _panel_locate(panel);
    i_add_object(ekOBJ_MENUVERT, nLeft - cuawin->N_ColIni, nTop - cuawin->N_LinIni, GTNAP_GLOBAL->cell_x_size, GTNAP_GLOBAL->cell_y_size, &size, FALSE, (GuiComponent*)panel, cuawin);
    //log_printf("MenuVert size: %.2f, %.2f, %.2f, %.2f", size.width, size.height, final_size.width, final_size.height);
}

/*---------------------------------------------------------------------------*/

void hb_gtnap_cualib_tableview(TableView *view, const int32_t nTop, const int32_t nLeft, const int32_t nBottom, const int32_t nRight)
{
    GtNapCualibWindow *cuawin = i_current_cuawin(GTNAP_GLOBAL);
    S2Df size;
    cassert_no_null(cuawin);
    log_printf("Added TableView into CUALIB Window: %d, %d, %d, %d", nTop, nLeft, nBottom, nRight);
    size.width = (real32_t)((nRight - nLeft + 1) * GTNAP_GLOBAL->cell_x_size);
    size.height = (real32_t)((nBottom - nTop + 1) * GTNAP_GLOBAL->cell_y_size);
    i_add_object(ekOBJ_TABLEVIEW, nLeft - cuawin->N_ColIni, nTop - cuawin->N_LinIni, GTNAP_GLOBAL->cell_x_size, GTNAP_GLOBAL->cell_y_size, &size, FALSE, (GuiComponent*)view, cuawin);
}

/*---------------------------------------------------------------------------*/

void hb_gtnap_cualib_textview(TextView *view, const int32_t nTop, const int32_t nLeft, const int32_t nBottom, const int32_t nRight)
{
    GtNapCualibWindow *cuawin = i_current_cuawin(GTNAP_GLOBAL);
    S2Df size;
    cassert_no_null(cuawin);
    log_printf("Added TextView into CUALIB Window: %d, %d, %d, %d", nTop, nLeft, nBottom, nRight);
    size.width = (real32_t)((nRight - nLeft + 1) * GTNAP_GLOBAL->cell_x_size);
    size.height = (real32_t)((nBottom - nTop + 1) * GTNAP_GLOBAL->cell_y_size);
    textview_family(view, font_family(GTNAP_GLOBAL->global_font));
    textview_fsize(view, font_size(GTNAP_GLOBAL->global_font));
    i_add_object(ekOBJ_TEXTVIEW, nLeft - cuawin->N_ColIni, nTop - cuawin->N_LinIni, GTNAP_GLOBAL->cell_x_size, GTNAP_GLOBAL->cell_y_size, &size, FALSE, (GuiComponent*)view, cuawin);
}

/*---------------------------------------------------------------------------*/

static Listener *i_gtnap_cualib_listener(const uint32_t codeBlockParamId, const int32_t key, const bool_t autoclose, GtNapCualibWindow *cuawin, FPtr_gtnap_callback func_callback)
{
    PHB_ITEM codeBlock = hb_param(codeBlockParamId, HB_IT_BLOCK);
    if (codeBlock != NULL)
    {
        GtNapCallback *callback = heap_new0(GtNapCallback);
        callback->codeBlock = hb_itemNew(codeBlock);
        callback->cuawin = cuawin;
        callback->key = key;
        callback->autoclose = autoclose;
        arrpt_append(cuawin->callbacks, callback, GtNapCallback);
        return listener(callback, func_callback, GtNapCallback);
    }
    else
    {
        log_printf("NULL listener in paramId: %d  Autoclose: %d", codeBlockParamId, autoclose);
        return NULL;
    }
}

/*---------------------------------------------------------------------------*/

static void i_OnImageClick(GtNapCallback *callback, Event *e)
{
    hb_gtnap_callback(callback, e);
    log_printf("Click image OnIdle process");
    if (callback->autoclose == TRUE)
    {
        log_printf("--> STOP CUALIB Modal Window: %p 'i_OnImageClick'", callback->cuawin->window);
        callback->cuawin->modal_window_alive = FALSE;
        window_stop_modal(callback->cuawin->window, 1000);
    }
}

/*---------------------------------------------------------------------------*/

void hb_gtnap_cualib_image(const char_t *pathname, const uint32_t codeBlockParamId, const uint32_t nTop, const uint32_t nLeft, const uint32_t nBottom, const uint32_t nRight, const bool_t autoclose)
{
    GtNapCualibWindow *cuawin = i_current_cuawin(GTNAP_GLOBAL);
    Image *image = image_from_file(pathname, NULL);
    cassert_no_null(cuawin);
    if (image != NULL)
    {
        S2Df size;
        ImageView *view = imageview_create();
        Listener *listener = i_gtnap_cualib_listener(codeBlockParamId, INT32_MAX, autoclose, cuawin, i_OnImageClick);
        log_printf("Added IMAGE into CUALIB Window: %d, %d, %d, %d", nTop, nLeft, nBottom, nRight);
        imageview_image(view, image);
        imageview_scale(view, ekGUI_SCALE_AUTO);
        size.width = (real32_t)((nRight - nLeft + 1) * GTNAP_GLOBAL->cell_x_size);
        size.height = (real32_t)((nBottom - nTop + 1) * GTNAP_GLOBAL->cell_y_size);
        i_add_object(ekOBJ_IMAGE, nLeft - cuawin->N_ColIni, nTop - cuawin->N_LinIni, GTNAP_GLOBAL->cell_x_size, GTNAP_GLOBAL->cell_y_size, &size, FALSE, (GuiComponent*)view, cuawin);
        image_destroy(&image);

        if (listener != NULL)
            view_OnClick((View*)view, listener);
    }
    else
    {
        log_printf("Cannot load '%s' image", pathname);
    }
}

/*---------------------------------------------------------------------------*/

static void i_OnButtonClick(GtNapCallback *callback, Event *e)
{
    bool_t ret = FALSE;
    cassert_no_null(callback);
    if (callback->codeBlock != NULL)
    {
        PHB_ITEM retItem = hb_itemDo(callback->codeBlock, 0);
        HB_TYPE type = HB_ITEM_TYPE(retItem);
        if (type == HB_IT_LOGICAL)
            ret = (bool_t)hb_itemGetL(retItem);
        hb_itemRelease(retItem);
    }

    log_printf("Click button");
    if (ret == TRUE || callback->autoclose == TRUE)
    {
        const Button *button = event_sender(e, Button);
        uint32_t tag = _component_get_tag((const GuiComponent*)button);
        if (tag == UINT32_MAX)
            tag = 1000;
        log_printf("--> STOP CUALIB Modal Window: %p 'i_OnButtonClick'", callback->cuawin->window);
        callback->cuawin->modal_window_alive = FALSE;
        window_stop_modal(callback->cuawin->window, tag);
    }
}

/*---------------------------------------------------------------------------*/

void hb_gtnap_cualib_button(const char_t *text, const uint32_t codeBlockParamId, const uint32_t nTag, const int32_t nTop, const int32_t nLeft, const int32_t nBottom, const int32_t nRight, const bool_t autoclose)
{
    GtNapCualibWindow *cuawin = i_current_cuawin(GTNAP_GLOBAL);
    Button *button = button_push();
    String *ctext = gtconvert_1252_to_UTF8(text);
    Listener *listener = i_gtnap_cualib_listener(codeBlockParamId, INT_MAX, autoclose, cuawin, i_OnButtonClick);
    S2Df size;
    cassert_no_null(cuawin);
    _component_set_tag((GuiComponent*)button, nTag);
    button_text(button, tc(ctext));
    button_font(button, GTNAP_GLOBAL->reduced_font);
    button_vpadding(button, i_button_vpadding());
    cassert(nBottom == nTop);
    size.width = (real32_t)((nRight - nLeft + 1) * GTNAP_GLOBAL->cell_x_size);
    size.height = (real32_t)((nBottom - nTop + 1) * GTNAP_GLOBAL->button_y_size);
    log_printf("Added BUTTON (%s) into CUALIB Window: %d, %d, %d, %d", tc(ctext), nTop, nLeft, nBottom, nRight);
    i_add_object(ekOBJ_BUTTON, nLeft - cuawin->N_ColIni, nTop - cuawin->N_LinIni, GTNAP_GLOBAL->cell_x_size, GTNAP_GLOBAL->cell_y_size, &size, FALSE, (GuiComponent*)button, cuawin);
    str_destroy(&ctext);

    if (listener != NULL)
        button_OnClick(button, listener);
}

/*---------------------------------------------------------------------------*/

static GtNapCualibObject *i_get_button_id(ArrSt(GtNapCualibObject) *objects, const uint32_t button_id)
{
    uint32_t button = 0;
    arrst_foreach(obj, objects, GtNapCualibObject)
        if (obj->type == ekOBJ_BUTTON)
        {
            if (button_id == button)
                return obj;
            button += 1;
        }
    arrst_end();
    cassert(FALSE);
    return NULL;
}

/*---------------------------------------------------------------------------*/

void hb_gtnap_cualib_change_button_block(const uint32_t button_id, const uint32_t codeBlockParamId, const bool_t autoclose)
{
    GtNapCualibWindow *cuawin = i_current_cuawin(GTNAP_GLOBAL);
    GtNapCualibObject *obj = i_get_button_id(cuawin->gui_objects, button_id - 1);
    Listener *listener = i_gtnap_cualib_listener(codeBlockParamId, INT_MAX, autoclose, cuawin, i_OnButtonClick);
    if (listener != NULL)
        button_OnClick((Button*)obj->component, listener);
}

/*---------------------------------------------------------------------------*/

void hb_gtnap_cualib_default_button(const uint32_t nDefault)
{
    GtNapCualibWindow *cuawin = i_current_cuawin(GTNAP_GLOBAL);
    cassert(cuawin->default_button == UINT32_MAX);
    cassert(nDefault > 0);
    cuawin->default_button = nDefault - 1;
}

/*---------------------------------------------------------------------------*/

static void i_set_label_text(GtNapCualibObject *obj)
{
    cassert_no_null(obj);
    cassert(obj->type == ekOBJ_LABEL);
    if (obj->labelCodeBlock != NULL)
    {
        PHB_ITEM retItem = hb_itemDo(obj->labelCodeBlock, 0);
        HB_TYPE type = HB_ITEM_TYPE(retItem);

        if (type == HB_IT_STRING)
        {
            char_t buffer[1024];
            uint32_t len;
            hb_itemCopyStrUTF8( retItem, (char*)buffer, (HB_SIZE)sizeof(buffer));
            len = unicode_nchars(buffer, ekUTF8);
            obj->size.width = (real32_t)(len * GTNAP_GLOBAL->cell_x_size);
            _component_set_frame(obj->component, &obj->pos, &obj->size);
            label_text((Label*)obj->component, buffer);
        }
        else
        {
            cassert_msg(FALSE, "Unkown type in i_set_label_text");
        }

        hb_itemRelease(retItem);
    }
}

/*---------------------------------------------------------------------------*/

static void i_set_edit_message(GtNapCualibObject *obj, GtNapCualibObject *mes_obj)
{
    cassert_no_null(obj);
    cassert_no_null(mes_obj);
    cassert(obj->type == ekOBJ_EDIT);
    cassert(mes_obj->type == ekOBJ_LABEL);
    if (obj->mensCodeBlock != NULL)
    {
        PHB_ITEM retItem = hb_itemDo(obj->mensCodeBlock, 0);
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

void hb_gtnap_cualib_label(const char_t *text, const uint32_t nLin, const uint32_t nCol, const bool_t background, const bool_t in_scroll_panel, const uint32_t updateBlockParamId)
{
    GtNapCualibWindow *cuawin = i_current_cuawin(GTNAP_GLOBAL);
    GtNapCualibObject *obj = NULL;
    PHB_ITEM labelCodeBlock = NULL;
    color_t back = (background == TRUE) ? kCOLOR_CYAN : 0;
    i_add_label_object(nCol - cuawin->N_ColIni, nLin - cuawin->N_LinIni, text, back, in_scroll_panel, GTNAP_GLOBAL, cuawin);

    obj = arrst_last(cuawin->gui_objects, GtNapCualibObject);
    cassert_no_null(obj);
    cassert(obj->type == ekOBJ_LABEL);

    //log_printf("Before LABEL CODE  BLOCK");
    labelCodeBlock = hb_param(updateBlockParamId, HB_IT_BLOCK);
    if (labelCodeBlock != NULL)
        obj->labelCodeBlock = hb_itemNew(labelCodeBlock);
    else
        obj->labelCodeBlock = NULL;
    //log_printf("AFTER LABEL CODE  BLOCK");

    //i_set_label_text(obj);
}

/*---------------------------------------------------------------------------*/

static void i_get_edit_text(const GtNapCualibObject *obj, char_t *buffer, const uint32_t size)
{
    cassert_no_null(obj);
    cassert(obj->type == ekOBJ_EDIT);

    if (obj->editCodeBlock != NULL)
    {
        PHB_ITEM retItem = hb_itemDo(obj->editCodeBlock, 0);
        HB_TYPE type = HB_ITEM_TYPE(retItem);

        if (type == HB_IT_STRING)
        {
            cassert(obj->dtype == ekTYPE_CHARACTER);
            hb_itemCopyStrUTF8( retItem, (char*)buffer, (HB_SIZE)size);
        }
        else if (type == HB_IT_DATE)
        {
            char date[16];
            char temp[16];
            cassert(obj->dtype == ekTYPE_DATE);
            hb_itemGetDS(retItem, date);
            hb_dateFormat(date, temp, "DD/MM/YYYY");
            str_copy_c(buffer, size, temp);
        }
        else
        {
            cassert_msg(FALSE, "Unknown Type in i_set_edit_text");
            str_copy_c(buffer, size, "");
        }

        hb_itemRelease(retItem);
    }
    else
    {
        str_copy_c(buffer, size, "");
    }
}

/*---------------------------------------------------------------------------*/

static void i_set_edit_text(const GtNapCualibObject *obj)
{
    char_t buffer[1024];
    cassert_no_null(obj);
    i_get_edit_text(obj, buffer, sizeof(buffer));
    edit_text((Edit*)obj->component, buffer);
}

/*---------------------------------------------------------------------------*/

/* Run the codeBlock that updates after a text entry in EditBox */
static void i_update_harbour_from_edit_text(const GtNapCualibObject *obj)
{
    cassert_no_null(obj);
    if (obj->editCodeBlock != NULL)
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
            PHB_ITEM retItem = hb_itemDo(obj->editCodeBlock, 1, pItem);
            hb_itemRelease(pItem);
            hb_itemRelease(retItem);
        }
    }
}

/*---------------------------------------------------------------------------*/

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
                    const bool_t in_scroll_panel)
{
    GtNapCualibWindow *cuawin = i_current_cuawin(GTNAP_GLOBAL);
    GtNapCualibObject *obj = NULL;
    PHB_ITEM editCodeBlock = NULL;
    PHB_ITEM editableGlobalCodeBlock = NULL;
    PHB_ITEM editableLocalCodeBlock = NULL;
    PHB_ITEM mensCodeBlock = NULL;
    PHB_ITEM validaCodeBlock = NULL;
    PHB_ITEM listaCodeBlock = NULL;
    PHB_ITEM autoCodeBlock = NULL;

    Edit *edit = edit_create();
    // //Listener *listener = i_gtnap_cualib_listener(codeBlockParamId, INT_MAX, autoclose, cuawin, i_OnButtonClick);
    S2Df size;
    cassert_no_null(cuawin);
    // //_component_set_tag((GuiComponent*)button, nTag);
    edit_font(edit, GTNAP_GLOBAL->reduced_font);
    edit_vpadding(edit, i_edit_vpadding());
    edit_bgcolor_focus(edit, kCOLOR_CYAN);
    //edit_editable(edit, editable);
    size.width = (real32_t)((nSize + 1) * GTNAP_GLOBAL->cell_x_size);
    size.height = (real32_t)GTNAP_GLOBAL->edit_y_size;
    i_add_object(ekOBJ_EDIT, nCol - cuawin->N_ColIni, nLin - cuawin->N_LinIni, GTNAP_GLOBAL->cell_x_size, GTNAP_GLOBAL->cell_y_size, &size, in_scroll_panel, (GuiComponent*)edit, cuawin);

    obj = arrst_last(cuawin->gui_objects, GtNapCualibObject);
    cassert_no_null(obj);
    cassert(obj->type = ekOBJ_EDIT);

    editCodeBlock = hb_param(editaBlockParamId, HB_IT_BLOCK);
    if (editCodeBlock != NULL)
        obj->editCodeBlock = hb_itemNew(editCodeBlock);
    else
        obj->editCodeBlock = NULL;

    editableGlobalCodeBlock = hb_param(editableGlobalParamId, HB_IT_BLOCK);
    if (editableGlobalCodeBlock != NULL)
        obj->editableGlobalCodeBlock = hb_itemNew(editableGlobalCodeBlock);
    else
        obj->editableGlobalCodeBlock = NULL;

    editableLocalCodeBlock = hb_param(editableLocalParamId, HB_IT_BLOCK);
    if (editableLocalCodeBlock != NULL)
        obj->editableLocalCodeBlock = hb_itemNew(editableLocalCodeBlock);
    else
        obj->editableLocalCodeBlock = NULL;

    mensCodeBlock = hb_param(mensParamId, HB_IT_BLOCK);
    if (mensCodeBlock != NULL)
        obj->mensCodeBlock = hb_itemNew(mensCodeBlock);
    else
        obj->mensCodeBlock = NULL;

    validaCodeBlock = hb_param(validaParamId, HB_IT_BLOCK);
    if (validaCodeBlock != NULL)
        obj->validaCodeBlock = hb_itemNew(validaCodeBlock);
    else
        obj->validaCodeBlock = NULL;

    listaCodeBlock = hb_param(listaParamId, HB_IT_BLOCK);
    if (listaCodeBlock != NULL)
        obj->listaCodeBlock = hb_itemNew(listaCodeBlock);
    else
        obj->listaCodeBlock = NULL;

    autoCodeBlock = hb_param(autoParamId, HB_IT_BLOCK);
    if (autoCodeBlock != NULL)
        obj->autoCodeBlock = hb_itemNew(autoCodeBlock);
    else
        obj->autoCodeBlock = NULL;

    if (getobj != NULL)
        obj->getobjItem = hb_itemNew(getobj);
    else
        obj->getobjItem = NULL;

    if (str_equ_c(type, "C") == TRUE)
        obj->dtype = ekTYPE_CHARACTER;
    else if (str_equ_c(type, "D") == TRUE)
        obj->dtype = ekTYPE_DATE;
    else
    {
        obj->dtype = ENUM_MAX(datatype_t);
        cassert(FALSE);
    }

    i_set_edit_text(obj);

    log_printf("Added EDIT (%s) into CUALIB Window: %d, %d, %d", edit_get_text((Edit*)obj->component), nLin, nCol, nSize);


    //str_destroy(&ctext);

    // if (listener != NULL)
    //     button_OnClick(button, listener);
}

/*---------------------------------------------------------------------------*/

void hb_gtnap_cualib_toolbar(const uint32_t nPixelsImage)
{
    GtNapCualibWindow *cuawin = i_current_cuawin(GTNAP_GLOBAL);
    cassert_no_null(cuawin);
    cassert(cuawin->toolbar == NULL);
    cuawin->toolbar = heap_new0(GtNapCualibToolbar);
    cuawin->toolbar->buttons = arrpt_create(Button);
    cuawin->toolbar->pixels_image = nPixelsImage;
    cuawin->toolbar->pixels_button = (uint32_t)((real32_t)nPixelsImage * 1.3f);
    log_printf("Created toolbar with '%d' pixels button", nPixelsImage);
}

/*---------------------------------------------------------------------------*/

void hb_gtnap_cualib_toolbar_button(const char_t *pathname, const char_t *tooltip)
{
    Image *image = image_from_file(pathname, NULL);
    if (image != NULL)
    {
        GtNapCualibWindow *cuawin = i_current_cuawin(GTNAP_GLOBAL);
        Button *button = button_flat();
        String *text = gtconvert_1252_to_UTF8(tooltip);
        cassert_no_null(cuawin);
        cassert_no_null(cuawin->toolbar);

        if (image_width(image) != cuawin->toolbar->pixels_image || image_height(image) != cuawin->toolbar->pixels_image)
        {
            Image *scaled = image_scale(image, cuawin->toolbar->pixels_image, cuawin->toolbar->pixels_image);
            image_destroy(&image);
            image = scaled;
        }

        button_image(button, image);
        button_tooltip(button, tc(text));
        arrpt_append(cuawin->toolbar->buttons, button, Button);
        log_printf("Added toolbar button '%s' with tooltip '%s'", pathname, tc(text));
        str_destroy(&text);
        image_destroy(&image);
    }
    else
    {
        log_printf("Cannot load '%s' image", pathname);
    }
}

/*---------------------------------------------------------------------------*/

void hb_gtnap_cualib_toolbar_separator(void)
{
    GtNapCualibWindow *cuawin = i_current_cuawin(GTNAP_GLOBAL);
    cassert_no_null(cuawin);
    cassert_no_null(cuawin->toolbar);
    arrpt_append(cuawin->toolbar->buttons, NULL, Button);
}

/*---------------------------------------------------------------------------*/

static GtNapArea *i_create_area(void)
{
    GtNapArea *area = heap_new0(GtNapArea);
    area->columns = arrst_create(GtNapColumn);
    area->records = arrst_create(uint32_t);
    area->cache_recno = UINT32_MAX;
    area->multisel = FALSE;
    area->whileCodeBlock = NULL;
    return area;
}

/*---------------------------------------------------------------------------*/

static GtNapVector *i_create_vector(void)
{
    GtNapVector *vector = heap_new0(GtNapVector);
    vector->items = arrst_create(VecItem);
    return vector;
}

/*---------------------------------------------------------------------------*/

GtNapArea *hb_gtnap_cualib_tableview_area(TableView *view, const uint32_t whileBlockParamId)
{
    GtNapCualibWindow *cuawin = i_current_cuawin(GTNAP_GLOBAL);
    cassert_no_null(cuawin);
    cassert(cuawin->gtarea == NULL);
    cuawin->gtarea = i_create_area();
    cuawin->gtarea->area = (AREA*)hb_rddGetCurrentWorkAreaPointer();
    cuawin->gtarea->view = view;

    {
        PHB_ITEM codeBlock = hb_param(whileBlockParamId, HB_IT_BLOCK);
        if (codeBlock != NULL)
        {
            log_printf("WHILE BLOCK IS NOT NULL");
            cuawin->gtarea->whileCodeBlock = hb_itemNew(codeBlock);
        }
        else
        {
            log_printf("WHILE BLOCK IS NULL");
            cuawin->gtarea->whileCodeBlock = NULL;
        }
    }

    if (cuawin->gtarea->area != NULL)
    {
        log_printf("hb_gtnap_cualib_area() works!!!");
    }
    else
    {
        log_printf("hb_rddGetCurrentWorkAreaPointer() fails. Not area defined");
    }

    return cuawin->gtarea;
}

/*---------------------------------------------------------------------------*/

// static void i_db_loop(GtNapArea *area)
// {
//     HB_BOOL fEof;
//     uint32_t i = 0;

//     cassert_no_null(area);
//     SELF_GOTOP(area->area);
//     SELF_EOF(area->area, &fEof);
//     while (fEof == HB_FALSE)
//     {
//         HB_ULONG uiRecNo = 0;
//         SELF_RECNO(area->area, &uiRecNo);
//         log_printf("Position: %d: Recno: %d", i, uiRecNo);
//         i += 1;
//         SELF_SKIP(area->area, 1);
//         SELF_EOF(area->area, &fEof);
//     }
// }

/*---------------------------------------------------------------------------*/

static void i_gtnap_cualib_area_refresh(GtNapArea *area)
{
    HB_ULONG ulCurRec;

    cassert_no_null(area);

    /* Database current RECNO() */
    SELF_RECNO(area->area, &ulCurRec);

    /* Clear the current record index */
    arrst_clear(area->records, NULL, uint32_t);

    /* Generate the record index for TableView */
    if (area->whileCodeBlock == NULL)
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
                PHB_ITEM retItem = hb_itemDo(area->whileCodeBlock, 0);
                HB_TYPE type = HB_ITEM_TYPE(retItem);
                bool_t add = FALSE;
                cassert(type == HB_IT_LOGICAL);
                add = (bool_t)hb_itemGetL(retItem);
                hb_itemRelease(retItem);

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

static void i_gtnap_select_row(GtNapArea *area)
{
    HB_ULONG ulCurRec;
    uint32_t sel_row;

    cassert_no_null(area);

    /* Current selected */
    SELF_RECNO( area->area, &ulCurRec );

    sel_row = i_row_from_recno(area, (uint32_t)ulCurRec);

    /* In multisel table, the selected rows comes from  VN_Selecio */
    if (area->multisel == TRUE)
    {
        log_printf("MULTISELECTION TABLE!!!!");
        if (tableview_get_focus_row(area->view) == UINT32_MAX)
        {
            /* We use RECNO for focused row */
            if (sel_row != UINT32_MAX)
            {
                tableview_focus_row(area->view, sel_row, ekTOP);
            }
            else
            {
                uint32_t nrecs = arrst_size(area->records, uint32_t);
                sel_row = tableview_get_focus_row(area->view);
                /* We move recno to current focused row */
                if (sel_row >= nrecs)
                {
                    sel_row = 0;
                }

                if (sel_row < nrecs)
                {
                    uint32_t recno = *arrst_get_const(area->records, sel_row, uint32_t);
                    tableview_select(area->view, &sel_row, 1);
                    SELF_GOTO(area->area, recno);
                }
            }
        }
    }
    else
    {
        tableview_deselect_all(area->view);

        if (sel_row != UINT32_MAX)
        {
            tableview_select(area->view, &sel_row, 1);
            tableview_focus_row(area->view, sel_row, ekTOP);
        }
        /* RECNO() doesn't exists in view (perhaps is deleted) */
        else
        {
            uint32_t nrecs = arrst_size(area->records, uint32_t);
            sel_row = tableview_get_focus_row(area->view);
            /* We move recno to current focused row */
            if (sel_row < nrecs)
            {
                uint32_t recno = *arrst_get_const(area->records, sel_row, uint32_t);
                tableview_select(area->view, &sel_row, 1);
                SELF_GOTO(area->area, recno);
            }
        }
    }
}

/*---------------------------------------------------------------------------*/

GtNapVector *hb_gtnap_cualib_tableview_vector(TableView *view)
{
    GtNapCualibWindow *cuawin = i_current_cuawin(GTNAP_GLOBAL);
    cassert_no_null(cuawin);
    cassert(cuawin->gtvector == NULL);
    cuawin->gtvector = i_create_vector();
    cuawin->gtvector->view = view;
    return cuawin->gtvector;
}

/*---------------------------------------------------------------------------*/

// GtNapArea *hb_gtnap_cualib_tableview_get_area(TableView *view)
// {
//     GtNapCualibWindow *cuawin = i_current_cuawin(GTNAP_GLOBAL);
//     cassert_no_null(cuawin);
//     cassert_no_null(cuawin->gtarea);
//     cassert_unref(cuawin->gtarea->view == view, view);
//     return cuawin->gtarea;
// }

/*---------------------------------------------------------------------------*/

static uint32_t i_column_width(const uint32_t str_len)
{
    // Default width
    if (str_len == 0)
        return 100;

    return (str_len + 1) * GTNAP_GLOBAL->cell_x_size;
}

/*---------------------------------------------------------------------------*/

static uint32_t i_header_numchars(const char_t *title)
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

void hb_gtnap_cualib_tableview_area_add_column(TableView *view, const char_t *title, const bool_t freeze, const uint32_t width, PHB_ITEM codeBlock)
{
    uint32_t id = UINT32_MAX;
    GtNapColumn *column = NULL;
    GtNapCualibWindow *cuawin = i_current_cuawin(GTNAP_GLOBAL);
    uint32_t hnchars = 0;
    cassert_no_null(cuawin);
    cassert_no_null(cuawin->gtarea);
    cassert(view == cuawin->gtarea->view);
    unref(freeze);
    id = tableview_new_column_text(view);
    cassert(id == arrst_size(cuawin->gtarea->columns, GtNapColumn));
    column = arrst_new(cuawin->gtarea->columns, GtNapColumn);
    column->title = gtconvert_1252_to_UTF8(title);
    str_repl_c(tcc(column->title), ";", "\n");
    hnchars = i_header_numchars(tc(column->title));
    column->fixed_width = width;
    column->width = i_column_width(hnchars);
    column->align = ekLEFT;
    column->codeBlock = hb_itemNew(codeBlock);
    tableview_header_title(view, id, tc(column->title));
    tableview_column_width(view, id, (real32_t)column->width);
    tableview_header_align(view, id, column->align);
    log_printf("hb_gtnap_cualib_tableview_area_add_column: '%s'", tc(column->title));
}

/*---------------------------------------------------------------------------*/

void hb_gtnap_cualib_tableview_vector_add_column(TableView *view, /*const char_t *title, const bool_t freeze,*/ const uint32_t width, PHB_ITEM codeBlock)
{
    uint32_t id = UINT32_MAX;
    GtNapCualibWindow *cuawin = i_current_cuawin(GTNAP_GLOBAL);
    cassert_no_null(cuawin);
    cassert_no_null(cuawin->gtvector);
    cassert(view == cuawin->gtvector->view);
    //unref(freeze);
    id = tableview_new_column_text(view);

    if (id == 0)
    {
        cassert(cuawin->gtvector->codeBlock0 == NULL);
        if (codeBlock != NULL)
            cuawin->gtvector->codeBlock0 = hb_itemNew(codeBlock);
        cuawin->gtvector->width0 = 11 * GTNAP_GLOBAL->cell_x_size;
        tableview_column_width(view, id, (real32_t)cuawin->gtvector->width0);
    }
    else if (id == 1)
    {
        cassert(cuawin->gtvector->codeBlock1 == NULL);
        if (codeBlock != NULL)
            cuawin->gtvector->codeBlock1 = hb_itemNew(codeBlock);
        cuawin->gtvector->width1 = i_column_width(width);
        tableview_column_width(view, id, (real32_t)cuawin->gtvector->width1);
    }
    else
    {
        // TableView for Vector only one or two columns
        cassert(id == 0 || id == 1);
    }
    // cassert(id == arrst_size(cuawin->gtarea->columns, GtNapColumn));
    // column = arrst_new(cuawin->gtarea->columns, GtNapColumn);
    // column->title = gtconvert_1252_to_UTF8(title);
    // column->fixed_width = width;
    // column->width = i_column_width(width);
    // column->align = ekLEFT;
    // column->codeBlock = hb_itemNew(codeBlock);
    // tableview_header_title(view, id, tc(column->title));
    // tableview_header_align(view, id, column->align);
    // log_printf("hb_gtnap_cualib_tableview_area_add_column: '%s'", tc(column->title));
}

/*---------------------------------------------------------------------------*/

void hb_gtnap_cualib_tableview_vector_add_item(TableView *view, String *text, PHB_ITEM codeBlock, const uint32_t hotkey_pos)
{
    GtNapCualibWindow *cuawin = i_current_cuawin(GTNAP_GLOBAL);
    VecItem *item = NULL;
    cassert_no_null(cuawin);
    cassert_no_null(cuawin->gtvector);
    cassert(view == cuawin->gtvector->view);
    item = arrst_new0(cuawin->gtvector->items, VecItem);
    item->text = text;
    item->codeBlock = hb_itemNew(codeBlock);
    item->hoykey_pos = hotkey_pos;
    item->selected = FALSE;
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

void hb_gtnap_cualib_vector_selection(const ArrSt(uint32_t) *sel)
{
    GtNapCualibWindow *cuawin = i_current_cuawin(GTNAP_GLOBAL);
    cassert_no_null(cuawin);
    cassert_no_null(cuawin->gtvector);
    arrst_foreach(item, cuawin->gtvector->items, VecItem)
        item->selected = i_in_vect(sel, item_i);
    arrst_end();
}

/*---------------------------------------------------------------------------*/

void hb_gtnap_cualib_tableview_refresh_all(void)
{
    GtNapCualibWindow *cuawin = i_current_cuawin(GTNAP_GLOBAL);
    if (cuawin != NULL)
    {
        if (cuawin->gtarea != NULL && cuawin->gtarea->view != NULL)
        {
            i_gtnap_cualib_area_refresh(cuawin->gtarea);
            tableview_update(cuawin->gtarea->view);
            i_gtnap_select_row(cuawin->gtarea);
        }
    }
}

/*---------------------------------------------------------------------------*/

void hb_gtnap_cualib_tableview_refresh_current(void)
{
    GtNapCualibWindow *cuawin = i_current_cuawin(GTNAP_GLOBAL);
    if (cuawin != NULL)
    {
        if (cuawin->gtarea != NULL && cuawin->gtarea->view != NULL)
            tableview_update(cuawin->gtarea->view);
    }
}

/*---------------------------------------------------------------------------*/

void hb_gtnap_cualib_column_width(GtNapArea *area, const uint32_t col, const char_t *text)
{
    GtNapColumn *column = NULL;
    cassert_no_null(area);
    column = arrst_get(area->columns, col, GtNapColumn);
    if (column->fixed_width == 0)
    {
        uint32_t len = str_len_c(text);
        uint32_t width = i_column_width(len);
        if (width > column->width)
        {
            column->width = width;
            tableview_column_width(area->view, col, (real32_t)column->width);
        }
    }
}

/*---------------------------------------------------------------------------*/

void hb_gtnap_area_cache_cur_db_row(GtNapArea *area)
{
    HB_ULONG uiRecNo = 0;
    cassert_no_null(area);
    SELF_RECNO(area->area, &uiRecNo);
    area->cache_recno = (uint32_t)uiRecNo;
}

/*---------------------------------------------------------------------------*/

void hb_gtnap_area_restore_cur_db_row(GtNapArea *area)
{
    cassert_no_null(area);
    SELF_GOTO(area->area, area->cache_recno);
}

/*---------------------------------------------------------------------------*/

uint32_t hb_gtnap_cualib_tableview_select_single_row(void)
{
    const ArrSt(uint32_t) *sel = NULL;
    GtNapCualibWindow *cuawin = i_current_cuawin(GTNAP_GLOBAL);
    cassert_no_null(cuawin);
    cassert_no_null(cuawin->gtarea);
    sel = tableview_selected(cuawin->gtarea->view);

    if (arrst_size(sel, uint32_t) == 1)
    {
        uint32_t row = *arrst_first_const(sel, uint32_t);
        uint32_t recno = *arrst_get_const(cuawin->gtarea->records, row, uint32_t);
        return recno;
    }

    return 0;
}

/*---------------------------------------------------------------------------*/

ArrSt(uint32_t) *hb_gtnap_cualib_tableview_select_multiple_row(void)
{
    const ArrSt(uint32_t) *sel = NULL;
    ArrSt(uint32_t) *recs = arrst_create(uint32_t);
    GtNapCualibWindow *cuawin = i_current_cuawin(GTNAP_GLOBAL);
    cassert_no_null(cuawin);
    cassert_no_null(cuawin->gtarea);
    sel = tableview_selected(cuawin->gtarea->view);

    arrst_foreach_const(row, sel, uint32_t)
        uint32_t recno = *arrst_get_const(cuawin->gtarea->records, *row, uint32_t);
        arrst_append(recs, recno, uint32_t);
    arrst_end();

    return recs;
}

/*---------------------------------------------------------------------------*/

static const GtNapKey *i_convert_key(const int32_t key)
{
    uint32_t i, n = sizeof(KEYMAPS) / sizeof(GtNapKey);

    log_printf("Convert key: %d", key);
    for (i = 0; i < n; ++i)
    {
        if (KEYMAPS[i].hkey == key)
            return &KEYMAPS[i];
    }

    log_printf("Convert key: %d IS NULL", key);
    return NULL;
}

/*---------------------------------------------------------------------------*/

static void i_OnWindowHotKey(GtNapCallback *callback, Event *e)
{
    bool_t ret = FALSE;
    cassert_no_null(callback);
    if (callback->codeBlock != NULL)
    {
        PHB_ITEM retItem = hb_itemDo(callback->codeBlock, 0);
        HB_TYPE type = HB_ITEM_TYPE(retItem);
        if (type == HB_IT_LOGICAL)
            ret = (bool_t)hb_itemGetL(retItem);
        hb_itemRelease(retItem);
    }

    log_printf("Pressed hotkey %d", callback->key);
    if (ret == TRUE || callback->autoclose == TRUE)
    {
        log_printf("--> STOP CUALIB Modal Window: %p 'i_OnWindowHotKey'", callback->cuawin->window);
        callback->cuawin->modal_window_alive = FALSE;
        window_stop_modal(callback->cuawin->window, 1001);
    }
}

/*---------------------------------------------------------------------------*/

void hb_gtnap_cualib_hotkey(const int32_t key, const uint32_t codeBlockParamId, const bool_t autoclose)
{
    GtNapCualibWindow *cuawin = i_current_cuawin(GTNAP_GLOBAL);
    const GtNapKey *nkey = i_convert_key(key);
    cassert_no_null(cuawin);

    log_printf("Looking for hotkey: %d", key);

    // Exists a Harbour/NAppGUI key convertion
    if (nkey != NULL)
    {
        uint32_t pos = UINT32_MAX;
        Listener *listener = NULL;

        log_printf("Found hotkey: %d", nkey->key);

        // Delete a previous callback on this hotkey
        arrpt_foreach(callback, cuawin->callbacks, GtNapCallback)
            if (callback->key == key)
            {
                pos = callback_i;
                break;
            }
        arrpt_end();

        if (pos != UINT32_MAX)
            arrpt_delete(cuawin->callbacks, pos, i_destroy_callback, GtNapCallback);

        listener = i_gtnap_cualib_listener(codeBlockParamId, key, autoclose, cuawin, i_OnWindowHotKey);

        if (listener != NULL)
            window_hotkey(cuawin->window, nkey->key, nkey->modifiers, listener);
    }
}

/*---------------------------------------------------------------------------*/

static void i_attach_to_panel(ArrSt(GtNapCualibObject) *objects, Panel *main_panel, Panel *scroll_panel, const V2Df *scroll_offset, const objtype_t type, const GtNapCualibToolbar *toolbar)
{
    cassert_no_null(scroll_offset);
    arrst_foreach(object, objects, GtNapCualibObject)
        if (object->type == type)
        {
            V2Df pos = object->pos;

            if (object->in_scroll_panel == TRUE)
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
                    break;

                case ekOBJ_IMAGE:
                case ekOBJ_MENUVERT:
                    pos.y += (real32_t)toolbar->pixels_button;
                    break;
                case ekOBJ_TABLEVIEW:
                case ekOBJ_TEXTVIEW:
                    pos.y += (real32_t)(toolbar->pixels_button - GTNAP_GLOBAL->cell_y_size);
                    break;
                case ekOBJ_BUTTON:
                    pos.y += (real32_t)GTNAP_GLOBAL->cell_y_size;
                    if (GTNAP_GLOBAL->cell_y_size > GTNAP_GLOBAL->button_y_size)
                        pos.y += (real32_t)((GTNAP_GLOBAL->cell_y_size - GTNAP_GLOBAL->button_y_size) / 2);
                    break;
                cassert_default();
                }
            }

            if (object->in_scroll_panel == TRUE)
            {
                pos.x += scroll_offset->x;
                pos.y += scroll_offset->y;
            }

            object->pos = pos;
            _component_set_frame(object->component, &pos, &object->size);

            // FRAN: TODO! IMPROVe
            if (object->type == ekOBJ_LABEL)
                i_set_label_text(object);
        }
    arrst_end();
}

/*---------------------------------------------------------------------------*/

static void i_attach_toolbar_to_panel(const GtNapCualibToolbar *toolbar, Panel *panel)
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

static void i_component_tabstop(ArrSt(GtNapCualibObject) *objects, Window *window, const objtype_t type)
{
    arrst_foreach(object, objects, GtNapCualibObject)
        if (object->type == type)
        {
            _component_visible(object->component, TRUE);

            switch(object->type) {
            case ekOBJ_LABEL:
            case ekOBJ_IMAGE:
                break;
            case ekOBJ_BUTTON:
                // Buttons don't have tabstop
                //_component_taborder(object->component, window);
                break;
            case ekOBJ_MENUVERT:
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
    arrst_end();
}

/*---------------------------------------------------------------------------*/

static void i_toolbar_tabstop(GtNapCualibToolbar *toolbar)
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

static void i_OnWindowClose(GtNapCallback *callback, Event *e)
{
    bool_t ret = hb_gtnap_callback_bool(callback, e);
    const EvWinClose *p = event_params(e, EvWinClose);
    bool_t *res = event_result(e, bool_t);
    *res = ret;

    if (ret == TRUE && p->origin == ekGUI_CLOSE_ESC)
        callback->cuawin->is_closed_by_esc = TRUE;

    log_printf("i_OnWindowClose EVENT RESPONSE %d", ret);
}

/*---------------------------------------------------------------------------*/

static void i_OnNextTabstop(GtNapCualibWindow *cuawin, Event *e)
{
    unref(e);
    cassert_no_null(cuawin);
    cuawin->focus_by_previous = FALSE;
    cuawin->tabstop_by_return_or_arrow = TRUE;
    window_next_tabstop(cuawin->window);
}

/*---------------------------------------------------------------------------*/

static void i_OnPreviousTabstop(GtNapCualibWindow *cuawin, Event *e)
{
    unref(e);
    cassert_no_null(cuawin);
    cuawin->focus_by_previous = TRUE;
    cuawin->tabstop_by_return_or_arrow = TRUE;
    window_previous_tabstop(cuawin->window);
}

/*---------------------------------------------------------------------------*/

static uint32_t i_num_buttons(GtNapCualibWindow *cuawin)
{
    uint32_t n = 0;
    cassert_no_null(cuawin);
    arrst_foreach_const(obj, cuawin->gui_objects, GtNapCualibObject)
        if (obj->type == ekOBJ_BUTTON)
            n += 1;
    arrst_end();
    return n;
}

/*---------------------------------------------------------------------------*/

static GtNapCualibObject *i_get_button(GtNapCualibWindow *cuawin, const uint32_t index)
{
    uint32_t i = 0;
    cassert_no_null(cuawin);
    arrst_foreach(obj, cuawin->gui_objects, GtNapCualibObject)
        if (obj->type == ekOBJ_BUTTON)
        {
            if (i == index)
                return obj;
            i += 1;
        }
    arrst_end();
    cassert(FALSE);
    return NULL;
}

/*---------------------------------------------------------------------------*/

static void i_set_defbutton(GtNapCualibWindow *cuawin)
{
    GtNapCualibObject *button = i_get_button(cuawin, cuawin->default_button);
    if (button != NULL)
        window_defbutton(cuawin->window, (Button*)button->component);
}

/*---------------------------------------------------------------------------*/

static void i_OnLeftButton(GtNapCualibWindow *cuawin, Event *e)
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

static void i_OnRightButton(GtNapCualibWindow *cuawin, Event *e)
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

static GtNapCualibObject *i_cualib_obj(ArrSt(GtNapCualibObject) *objects, const GuiComponent *component)
{
    arrst_foreach(obj, objects, GtNapCualibObject)
        if (obj->component == component)
            return obj;
    arrst_end();
    return NULL;
}

/*---------------------------------------------------------------------------*/

static void i_OnEditChange(GtNapCualibWindow *cuawin, Event *e)
{
    GtNapCualibObject *cuaobj = NULL;
    bool_t valid = TRUE;
    cassert_no_null(cuawin);


    if (cuawin->modal_window_alive == FALSE)
    {
        log_printf("Modal Window: %p  in not alive", cuawin);
        return;
    }


    /* Update Harbour with the content of the EditBox */
    cuaobj = i_cualib_obj(cuawin->gui_objects, (GuiComponent*)event_sender(e, Edit));
    cassert(cuaobj->type == ekOBJ_EDIT);

    if (cuaobj->is_on_edit_change == TRUE)
    {
        log_printf("Object: %p  is_on_edit_change", cuaobj);
        return;
    }

    if (cuaobj->is_last_edit == TRUE && cuawin->tabstop_by_return_or_arrow == FALSE)
    {
            log_printf("Modal Window: %p  tabstop_by_return_or_arrow FALSE", cuawin);
        return;

    }

    cuaobj->is_on_edit_change = TRUE;

    if (cuaobj->is_last_edit == TRUE)
        log_printf("Entering i_OnEditChange LAST_EDIT: %p", cuaobj);
    else
        log_printf("Entering i_OnEditChange: %p", cuaobj);

    i_update_harbour_from_edit_text(cuaobj);

        log_printf("Voy a hacer el VALID check!");
    if (cuaobj->validaCodeBlock != NULL)
    {
        // FRAN:TODO Corrections when NAppGUI implement NO LOSE focus OnEditChange
        // PHB_ITEM retItem = hb_itemDo(cuaobj->validaCodeBlock, 1, cuaobj->getobjItem);
        // HB_TYPE type = HB_ITEM_TYPE(retItem);
        // cassert(type == HB_IT_LOGICAL);
        // valid = (bool_t)hb_itemGetL(retItem);
        // hb_itemRelease(retItem);
        valid = TRUE;
    }
        log_printf("HE Salido del VALID check!");

    if (valid == TRUE)
    {
        /* Update possible labels associated with this input */
        arrst_foreach(obj, cuawin->gui_objects, GtNapCualibObject)
            if (obj->type == ekOBJ_LABEL)
                i_set_label_text(obj);
        arrst_end();


        /* The window must stop modal when the last input loses the focus */
        if (cuawin->stops_last_edit == TRUE)
        {
            /* If user have pressed the [ESC] key, we left the stop for that event */
            if (cuawin->is_closed_by_esc == FALSE)
            {
                /* If user have navigated with [UP] button, the window must continue open */
                if (cuawin->focus_by_previous == FALSE)
                {
                    /* The last editbox has lost the focus --> Close the window */
                    if (cuaobj->is_last_edit == TRUE)
                    {
                        bool_t close = TRUE;

                        /* We have asociated a confirmation block */
                        if (cuawin->confirmaCodeBlock != NULL)
                        {
                            PHB_ITEM retItem = hb_itemDo(cuawin->confirmaCodeBlock, 0);
                            HB_TYPE type = HB_ITEM_TYPE(retItem);
                            cassert(type == HB_IT_LOGICAL);
                            close = (bool_t)hb_itemGetL(retItem);
                            hb_itemRelease(retItem);
                        }

                        if (close == TRUE)
                        {
                            if (cuawin->processing_invalid_date == TRUE)
                                close = FALSE;
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
        }
    }

    cuawin->focus_by_previous = FALSE;
    cuaobj->is_on_edit_change = FALSE;
    cuawin->tabstop_by_return_or_arrow = FALSE;
}

/*---------------------------------------------------------------------------*/

static void i_filter_date(const EvText *text, EvTextFilter *filter)
{
    uint32_t i = 0, j = 0;
    cassert_no_null(text);
    cassert_no_null(filter);
    while(text->text[i] != '\0')
    {
        if (text->text[i] >= '0' && text->text[i] <= '9')
        {
            filter->text[j] = text->text[i];
            j += 1;

            if (j == 2 || j == 5)
            {
                filter->text[j] = '/';
                j += 1;
            }

            if (j == 10)
                break;
        }

        i += 1;
    }

    filter->apply = TRUE;
    filter->cpos = j;

    for (; j < 10; ++j)
    {
        if (j == 2 || j == 5)
            filter->text[j] = '/';
        else
            filter->text[j] = ' ';
    }

    filter->text[j] = '\0';
}

/*---------------------------------------------------------------------------*/

static bool_t i_is_editable(GtNapCualibObject *cuaobj)
{
    bool_t editable = TRUE;
    cassert_no_null(cuaobj);
    cassert(cuaobj->type == ekOBJ_EDIT);

    if (editable == TRUE && cuaobj->editableGlobalCodeBlock != NULL)
    {
        PHB_ITEM retItem = hb_itemDo(cuaobj->editableGlobalCodeBlock, 0);
        HB_TYPE type = HB_ITEM_TYPE(retItem);
        cassert(type == HB_IT_LOGICAL);
        editable = (bool_t)hb_itemGetL(retItem);
        hb_itemRelease(retItem);
    }

    if (editable == TRUE && cuaobj->editableLocalCodeBlock != NULL)
    {
        PHB_ITEM retItem = hb_itemDo(cuaobj->editableLocalCodeBlock, 0);
        HB_TYPE type = HB_ITEM_TYPE(retItem);
        cassert(type == HB_IT_LOGICAL);
        editable = (bool_t)hb_itemGetL(retItem);
        hb_itemRelease(retItem);
    }

    return editable;
}

/*---------------------------------------------------------------------------*/

static void i_OnEditFilter(GtNapCualibWindow *cuawin, Event *e)
{
    Edit *edit = event_sender(e, Edit);
    GtNapCualibObject *cuaobj = NULL;
    cassert_no_null(cuawin);

    arrst_foreach(obj, cuawin->gui_objects, GtNapCualibObject)
        if (obj->type == ekOBJ_EDIT)
        {
            if (edit == (Edit*)obj->component)
            {
                cuaobj = obj;
                break;
            }
        }
    arrst_end();

    if (cuaobj != NULL)
    {
        const EvText *p = event_params(e, EvText);
        EvTextFilter *res = event_result(e, EvTextFilter);

        // FRAN: TODO
        // This block must be move to edit_OnFocus()
        if (cuawin->message_label_id != UINT32_MAX)
        {
            GtNapCualibObject *mes_obj = arrst_get(cuawin->gui_objects, cuawin->message_label_id, GtNapCualibObject);
            i_set_edit_message(cuaobj, mes_obj);
        }

        // FRAN: TODO
        // This block must be move to edit_OnFocus()
        log_printf("cuaobj->autoCodeBlock: %p", cuaobj->autoCodeBlock);
        if (cuaobj->autoCodeBlock != NULL)
        {
            bool_t lista = FALSE;

            log_printf("BEFORE AUTO Function CALL");
            {
                PHB_ITEM retItem = hb_itemDo(cuaobj->autoCodeBlock, 0);
                HB_TYPE type = HB_ITEM_TYPE(retItem);
                cassert(type == HB_IT_LOGICAL);
                lista = (bool_t)hb_itemGetL(retItem);
                hb_itemRelease(retItem);
            }
            log_printf("END AUTO Function CALL: %d", lista);

            cuawin->processing_invalid_date = TRUE;

            if (lista == TRUE  && cuaobj->listaCodeBlock != NULL)
            {
                char_t temp[1024];
                PHB_ITEM retItem2 = hb_itemDo(cuaobj->listaCodeBlock, 0);
                HB_TYPE type2 = HB_ITEM_TYPE(retItem2);
                cassert(type2 == HB_IT_STRING);
                hb_itemCopyStrUTF8(retItem2, temp, sizeof(temp));
                edit_text((Edit*)cuaobj->component, temp);
                hb_itemRelease(retItem2);
            }

            cuawin->processing_invalid_date = FALSE;

        }
        // --------------------------------

        if (i_is_editable(cuaobj) == FALSE)
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
                i_filter_date(p, res);
                log_printf("Date CPOS: %d", res->cpos);

                if (res->cpos == 10)
                {
                    log_printf("END DATE EDITING");

                    if (cuawin->errorDataCodeBlock != NULL)
                    {
                        long r = hb_dateUnformat( res->text, hb_setGetDateFormat());
                        log_printf("DATE processing result: %d", r);


                        /* Date invalid */
                        if (r == 0)
                        {
                            PHB_ITEM retItem = NULL;
                            cuawin->processing_invalid_date = TRUE;
                            retItem = hb_itemDo(cuawin->errorDataCodeBlock, 0);
                            hb_itemRelease(retItem);
                            cuawin->processing_invalid_date = FALSE;
                        }
                    }
                }
            }
        }
    }
}

/*---------------------------------------------------------------------------*/

void hb_gtnap_cualib_error_data(const uint32_t errorDataBlockParamId)
{
    GtNapCualibWindow *cuawin = i_current_cuawin(GTNAP_GLOBAL);
    cassert_no_null(cuawin);

    // FRAN: IMPROVE... ADD TO hb_gtnap_cualib_launch_modal
    if(cuawin->errorDataCodeBlock == NULL)
    {
        PHB_ITEM codeBlock = hb_param(errorDataBlockParamId, HB_IT_BLOCK);
        if (codeBlock != NULL)
            cuawin->errorDataCodeBlock = hb_itemNew(codeBlock);
    }
}

/*---------------------------------------------------------------------------*/

static S2Df i_scroll_content_size(const ArrSt(GtNapCualibObject) *objects)
{
    real32_t min_x = 1e10f;
    real32_t min_y = 1e10f;
    real32_t max_x = -1e10f;
    real32_t max_y = -1e10f;

    arrst_foreach_const(object, objects, GtNapCualibObject)
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
    arrst_end();

    return s2df(max_x - min_x, max_y - min_y);
}

/*---------------------------------------------------------------------------*/

uint32_t hb_gtnap_cualib_launch_modal(const uint32_t confirmaBlockParamId, const uint32_t cancelBlockParamId)
{
    GtNapCualibWindow *cuawin = i_current_cuawin(GTNAP_GLOBAL);
    cassert_no_null(cuawin);

    /* Configure the Window before the first launch */
    if (cuawin->is_configured == FALSE)
    {
        Panel *scroll_panel = NULL;
        Layout *layout = layout_create(1, 1);
        V2Df offset = kV2D_ZEROf;
        Listener *listener = NULL;

        if (cuawin->toolbar != NULL)
            cuawin->panel_size.height += (real32_t)GTNAP_GLOBAL->cell_y_size;

        panel_size(cuawin->panel, cuawin->panel_size);
        panel_layout(cuawin->panel, layout);
        window_panel(cuawin->window, cuawin->panel);

        if (cuawin->scroll_panel == TRUE)
        {
            // We add a subpanel to window main panel to implement the scroll area

            // FRAN TODO: With Scroll!!!
            // Panel *panel = panel_scroll(FALSE, TRUE);
            // ------------------------------------------------

            Panel *panel = panel_create();
            S2Df csize = i_scroll_content_size(cuawin->gui_objects);
            int32_t cell_x = cuawin->scroll_N_ColIni - cuawin->N_ColIni;
            int32_t cell_y = cuawin->scroll_N_LinIni - cuawin->N_LinIni;
            real32_t pos_x = (real32_t)(cell_x * GTNAP_GLOBAL->cell_x_size);
            real32_t pos_y = (real32_t)(cell_y * GTNAP_GLOBAL->cell_y_size);
            real32_t width = (real32_t)((cuawin->scroll_N_ColFin - cuawin->scroll_N_ColIni + 1) * GTNAP_GLOBAL->cell_x_size);
            real32_t height = (real32_t)((cuawin->scroll_N_LinFin - cuawin->scroll_N_LinIni + 1) * GTNAP_GLOBAL->cell_y_size);
            V2Df pos = v2df(pos_x, pos_y);
            S2Df size = s2df(width, height);

            _component_attach_to_panel((GuiComponent*)cuawin->panel, (GuiComponent*)panel);
            _component_set_frame((GuiComponent*)panel, &pos, &size);
            _component_visible((GuiComponent*)panel, FALSE);

            // NAppGUI support for direct setting the panel_scroll content size
            log_printf("Panel scroll content size: %.2f, %.2f", csize.width, csize.height);
            // -------------------------------------------------

            offset.x = -pos.x;
            offset.y = -pos.y;
            scroll_panel = panel;
            cuawin->scrolled_panel = panel;
        }

        /* Attach gui objects in certain Z-Order (from back to front) */
        i_attach_to_panel(cuawin->gui_objects, cuawin->panel, scroll_panel, &offset, ekOBJ_MENUVERT, cuawin->toolbar);
        i_attach_to_panel(cuawin->gui_objects, cuawin->panel, scroll_panel, &offset, ekOBJ_TABLEVIEW, cuawin->toolbar);
        i_attach_to_panel(cuawin->gui_objects, cuawin->panel, scroll_panel, &offset, ekOBJ_TEXTVIEW, cuawin->toolbar);
        i_attach_to_panel(cuawin->gui_objects, cuawin->panel, scroll_panel, &offset, ekOBJ_LABEL, cuawin->toolbar);
        i_attach_to_panel(cuawin->gui_objects, cuawin->panel, scroll_panel, &offset, ekOBJ_BUTTON, cuawin->toolbar);
        i_attach_to_panel(cuawin->gui_objects, cuawin->panel, scroll_panel, &offset, ekOBJ_EDIT, cuawin->toolbar);
        i_attach_to_panel(cuawin->gui_objects, cuawin->panel, scroll_panel, &offset, ekOBJ_IMAGE, cuawin->toolbar);
        i_attach_toolbar_to_panel(cuawin->toolbar, cuawin->panel);

        /* Tab-stops order */
        _window_taborder(cuawin->window, NULL);

        if (scroll_panel != NULL)
            _component_visible((GuiComponent*)scroll_panel, TRUE);

        i_component_tabstop(cuawin->gui_objects, cuawin->window, ekOBJ_MENUVERT);
        i_component_tabstop(cuawin->gui_objects, cuawin->window, ekOBJ_TABLEVIEW);
        i_component_tabstop(cuawin->gui_objects, cuawin->window, ekOBJ_TEXTVIEW);
        i_component_tabstop(cuawin->gui_objects, cuawin->window, ekOBJ_EDIT);
        i_component_tabstop(cuawin->gui_objects, cuawin->window, ekOBJ_BUTTON);
        i_component_tabstop(cuawin->gui_objects, cuawin->window, ekOBJ_LABEL);
        i_component_tabstop(cuawin->gui_objects, cuawin->window, ekOBJ_IMAGE);
        i_toolbar_tabstop(cuawin->toolbar);

        /* Confirma Codeblock */
        {
            PHB_ITEM codeBlock = NULL;
            cassert(cuawin->confirmaCodeBlock == NULL);
            codeBlock = hb_param(confirmaBlockParamId, HB_IT_BLOCK);
            if (codeBlock != NULL)
                cuawin->confirmaCodeBlock = hb_itemNew(codeBlock);
        }

        /* OnClose listener */
        listener = i_gtnap_cualib_listener(cancelBlockParamId, INT32_MAX, FALSE, cuawin, i_OnWindowClose);
        window_OnClose(cuawin->window, listener);

        if (cuawin->enter_tabstop == TRUE)
            window_hotkey(cuawin->window, ekKEY_RETURN, 0, listener(cuawin, i_OnNextTabstop, GtNapCualibWindow));

        if (cuawin->arrows_tabstop == TRUE)
        {
            cassert(cuawin->buttons_navigation == FALSE);
            window_hotkey(cuawin->window, ekKEY_UP, 0, listener(cuawin, i_OnPreviousTabstop, GtNapCualibWindow));
            window_hotkey(cuawin->window, ekKEY_DOWN, 0, listener(cuawin, i_OnNextTabstop, GtNapCualibWindow));
        }
        else if (cuawin->buttons_navigation == TRUE)
        {
            if (i_num_buttons(cuawin) > 1)
            {
                window_hotkey(cuawin->window, ekKEY_LEFT, 0, listener(cuawin, i_OnLeftButton, GtNapCualibWindow));
                window_hotkey(cuawin->window, ekKEY_RIGHT, 0, listener(cuawin, i_OnRightButton, GtNapCualibWindow));
            }
        }


        //if (cuawin->stops_last_edit == TRUE)
        {
            GtNapCualibObject *last_edit = NULL;
            arrst_foreach(obj, cuawin->gui_objects, GtNapCualibObject)
                if (obj->type == ekOBJ_EDIT)
                {
                    edit_OnChange((Edit*)obj->component, listener(cuawin, i_OnEditChange, GtNapCualibWindow));
                    edit_OnFilter((Edit*)obj->component, listener(cuawin, i_OnEditFilter, GtNapCualibWindow));
                    obj->is_last_edit = FALSE;
                    last_edit = obj;
                }
            arrst_end();

            if (last_edit != NULL)
                last_edit->is_last_edit = TRUE;
        }

        cuawin->is_configured = TRUE;
    }

    if (cuawin->buttons_navigation == TRUE)
    {
        uint32_t n = i_num_buttons(cuawin);
        if (n > 0)
        {
            if (cuawin->default_button == UINT32_MAX)
                cuawin->default_button = 0;
            cassert(cuawin->default_button < n);
            i_set_defbutton(cuawin);
        }
    }

    /* Launch the window */
    {
        GtNapCualibWindow *parent = i_parent_cuawin(GTNAP_GLOBAL);
        V2Df pos;
        uint32_t ret = 0;

        if (parent == NULL)
        {
            cassert(arrst_size(GTNAP_GLOBAL->cualib_windows, GtNapCualibWindow) == 1);
        }

        pos.x = (real32_t)(cuawin->N_ColIni * GTNAP_GLOBAL->cell_x_size);
        pos.y = (real32_t)(cuawin->N_LinIni * GTNAP_GLOBAL->cell_y_size);

        if (parent != NULL)
        {
            const GtNapCualibWindow *rootwin = arrst_get_const(GTNAP_GLOBAL->cualib_windows, 0, GtNapCualibWindow);
            V2Df ppos = window_get_origin(rootwin->window);
            pos.x += ppos.x /*- 2 * GTNAP_GLOBAL->cell_x_size*/;
            pos.y += ppos.y;

            if (cuawin->toolbar != NULL)
                pos.y -= (real32_t)(GTNAP_GLOBAL->cell_y_size - (cuawin->toolbar->pixels_button - GTNAP_GLOBAL->cell_y_size));
        }

        window_origin(cuawin->window, pos);
        log_printf("--> Launch CUALIB Modal Window: %p, Parent %p (%d, %d, %d, %d)", cuawin->window, parent ? parent->window : NULL, cuawin->N_LinIni, cuawin->N_ColIni, cuawin->N_LinFin, cuawin->N_ColFin);
        cuawin->is_closed_by_esc = FALSE;
        cuawin->modal_window_alive = TRUE;
        ret = window_modal(cuawin->window, parent ? parent->window : NULL);
        return ret;
    }
}

/*---------------------------------------------------------------------------*/

void hb_gtnap_cualib_destroy_window(void)
{
    uint32_t id = arrst_size(GTNAP_GLOBAL->cualib_windows, GtNapCualibWindow);
    GtNapCualibWindow *cuawin = NULL;
    cassert(id > 0);
    cuawin = arrst_get(GTNAP_GLOBAL->cualib_windows, id - 1, GtNapCualibWindow);
    arrst_delete(GTNAP_GLOBAL->cualib_windows, id - 1, i_remove_cualib_win, GtNapCualibWindow);
    window_destroy(&cuawin->window);
}

/*---------------------------------------------------------------------------*/

Window *hb_gtnap_cualib_current_window(void)
{
    GtNapCualibWindow *cuawin = i_current_cuawin(GTNAP_GLOBAL);
    cassert_no_null(cuawin);
    return cuawin->window;
}

/*---------------------------------------------------------------------------*/

TableView *hb_gtnap_cualib_current_tableview(void)
{
    GtNapCualibWindow *cuawin = i_current_cuawin(GTNAP_GLOBAL);
    TableView *view = NULL;
    cassert_no_null(cuawin);
    if (cuawin->gtarea != NULL)
        return cuawin->gtarea->view;

    arrst_foreach(obj, cuawin->gui_objects, GtNapCualibObject)
        // Only one menuvert is allowed
        if (obj->type == ekOBJ_TABLEVIEW)
        {
            cassert(view == NULL);
            view = (TableView*)obj->component;
        }
    arrst_end();

    return view;
}

/*---------------------------------------------------------------------------*/

Panel *hb_gtnap_cualib_current_menuvert(void)
{
    GtNapCualibWindow *cuawin = i_current_cuawin(GTNAP_GLOBAL);
    Panel *menuvert = NULL;
    cassert_no_null(cuawin);

    arrst_foreach(obj, cuawin->gui_objects, GtNapCualibObject)
        // Only one menuvert is allowed
        if (obj->type == ekOBJ_MENUVERT)
        {
            cassert(menuvert == NULL);
            menuvert = (Panel*)obj->component;
        }
    arrst_end();

    // A MenuVert is required
    cassert_no_null(menuvert);
    return menuvert;
}

/*---------------------------------------------------------------------------*/

uint32_t hb_gtnap_cell_height(void)
{
    return GTNAP_GLOBAL->cell_y_size;
}

/*---------------------------------------------------------------------------*/

static void i_OnTableViewSelect(GtNapCallback *callback, Event *e)
{
    hb_gtnap_callback(callback, e);
    log_printf("TableView selection changed");
}

/*---------------------------------------------------------------------------*/

void hb_gtnap_cualib_tableview_OnSelect(const uint32_t codeBlockParamId)
{
    GtNapCualibWindow *cuawin = i_current_cuawin(GTNAP_GLOBAL);
    cassert_no_null(cuawin);
    if (cuawin->gtarea != NULL && cuawin->gtarea->view != NULL)
    {
        Listener *listener = i_gtnap_cualib_listener(codeBlockParamId, INT32_MAX, FALSE, cuawin, i_OnTableViewSelect);
        tableview_OnSelect(cuawin->gtarea->view, listener);
    }
}

/*---------------------------------------------------------------------------*/

static void i_OnTableViewSingleSelect(GtNapArea *area, Event *e)
{
    const EvTbSel *sel = event_params(e, EvTbSel);
    uint32_t first = 0;
    uint32_t recno = 0;
    cassert(arrst_size(sel->sel, uint32_t) == 1);
    /* The row selected in table */
    first = *arrst_first_const(sel->sel, uint32_t);
    /* The DB RECNO in this row selected in table */
    recno = *arrst_get_const(area->records, first, uint32_t);
    /* Just GOTO */
    SELF_GOTO(area->area, recno);
}

/*---------------------------------------------------------------------------*/

void hb_gtnap_cualib_tableview_On_Single_Select_Change(void)
{
    GtNapCualibWindow *cuawin = i_current_cuawin(GTNAP_GLOBAL);
    cassert_no_null(cuawin);
    if (cuawin->gtarea != NULL && cuawin->gtarea->view != NULL)
    {
        Listener *listener = listener(cuawin->gtarea, i_OnTableViewSingleSelect, GtNapArea);
        tableview_OnSelect(cuawin->gtarea->view, listener);
    }
}

/*---------------------------------------------------------------------------*/

// static int i_uint32_cmp(const uint32_t *u1, const uint32_t *u2)
// {
//     return (int)(*u1 - *u2);
// }

/*---------------------------------------------------------------------------*/

bool_t hb_gtnap_cualib_current_row_selected(void)
{
    GtNapCualibWindow *cuawin = i_current_cuawin(GTNAP_GLOBAL);
    cassert_no_null(cuawin);
    if (cuawin->gtarea != NULL && cuawin->gtarea->view != NULL)
    {
        HB_ULONG uiRecNo = 0;
        const ArrSt(uint32_t) *sel = tableview_selected(cuawin->gtarea->view);
        SELF_RECNO(cuawin->gtarea->area, &uiRecNo);
        arrst_foreach_const(row, sel, uint32_t)
            const uint32_t recno = *arrst_get_const(cuawin->gtarea->records, *row, uint32_t);
            if (recno == uiRecNo)
                return TRUE;
        arrst_end();
    }

    return FALSE;
}

/*---------------------------------------------------------------------------*/

void hb_gtnap_cualib_multisel(void)
{
    GtNapCualibWindow *cuawin = i_current_cuawin(GTNAP_GLOBAL);
    cassert_no_null(cuawin);
    cassert_no_null(cuawin->gtarea);
    cuawin->gtarea->multisel = TRUE;
}

/*---------------------------------------------------------------------------*/

void hb_gtnap_cualib_select_current(void)
{
    GtNapCualibWindow *cuawin = i_current_cuawin(GTNAP_GLOBAL);
    uint32_t focused = UINT32_MAX;
    const ArrSt(uint32_t) *sel = NULL;
    cassert_no_null(cuawin);
    cassert_no_null(cuawin->gtarea);
    cassert(cuawin->gtarea->multisel == TRUE);
    focused = tableview_get_focus_row(cuawin->gtarea->view);
    sel = tableview_selected(cuawin->gtarea->view);

    if (i_in_vect(sel, focused) == TRUE)
        tableview_deselect(cuawin->gtarea->view, &focused, 1);
    else
        tableview_select(cuawin->gtarea->view, &focused, 1);

    tableview_update(cuawin->gtarea->view);
}

/*---------------------------------------------------------------------------*/

void hb_gtnap_cualib_select_current_vector(void)
{
    GtNapCualibWindow *cuawin = i_current_cuawin(GTNAP_GLOBAL);
    uint32_t focused = UINT32_MAX;
    VecItem *item = NULL;
    const ArrSt(uint32_t) *sel = NULL;
    cassert_no_null(cuawin);
    cassert_no_null(cuawin->gtvector);
    focused = tableview_get_focus_row(cuawin->gtvector->view);
    sel = tableview_selected(cuawin->gtvector->view);

    if (i_in_vect(sel, focused) == TRUE)
        tableview_deselect(cuawin->gtvector->view, &focused, 1);
    else
        tableview_select(cuawin->gtvector->view, &focused, 1);

    item = arrst_get(cuawin->gtvector->items, focused, VecItem);
    item->selected = !item->selected;

    tableview_update(cuawin->gtvector->view);
}

/*---------------------------------------------------------------------------*/

static HB_BOOL hb_gtnap_Lock( PHB_GT pGT )
{
    HB_SYMBOL_UNUSED( pGT );
    // Lock and Unlock are always called before other operation. Avoid dirtying the log.
    // log_printf("hb_gtnap_Lock()");
    return TRUE;
}

/*---------------------------------------------------------------------------*/

static void hb_gtnap_Unlock( PHB_GT pGT )
{
    HB_SYMBOL_UNUSED( pGT );
    // Lock and Unlock are always called before other operation. Avoid dirtying the log.
    // log_printf("hb_gtnap_Unlock()");
}

/*---------------------------------------------------------------------------*/

static void hb_gtnap_Init( PHB_GT pGT, HB_FHANDLE hFilenoStdin, HB_FHANDLE hFilenoStdout, HB_FHANDLE hFilenoStderr )
{
    HB_SYMBOL_UNUSED( pGT );
    //log_printf("hb_gtnap_Init()");
    HB_SYMBOL_UNUSED( hFilenoStdin );
    HB_SYMBOL_UNUSED( hFilenoStdout );
    HB_SYMBOL_UNUSED( hFilenoStderr );
}

/*---------------------------------------------------------------------------*/

static void hb_gtnap_Exit( PHB_GT pGT )
{
    HB_SYMBOL_UNUSED( pGT );
    //log_printf("hb_gtnap_Exit()");

    // if (GTNAP_GLOBAL->cualib_mode == TRUE)
    // {
    //     log_printf("GTNAP exit of CUALIB mode");
    //     font_destroy(&GTNAP_GLOBAL->global_font);
    //     str_destroy(&GTNAP_GLOBAL->title);
    //     gui_context_destroy(&GTNAP_GLOBAL->native_gui);
    //     heap_delete(&GTNAP_GLOBAL, GtNap);
    //     osgui_finish();
    //     gui_finish();
    // }
}

/*---------------------------------------------------------------------------*/

static HB_BOOL hb_gtnap_Resize( PHB_GT pGT, int iRow, int iCol )
{
    HB_SYMBOL_UNUSED( pGT );
    //log_printf("hb_gtnap_Resize(%d, %d)", iRow, iCol);
    return TRUE;
}

/*---------------------------------------------------------------------------*/

static HB_BOOL hb_gtnap_SetMode( PHB_GT pGT, int iRow, int iCol )
{
    HB_SYMBOL_UNUSED( pGT );
    //log_printf("hb_gtnap_SetMode(%d, %d)", iRow, iCol);
    return TRUE;
}

/*---------------------------------------------------------------------------*/

static void hb_gtnap_GetSize( PHB_GT pGT, int * piRows, int  * piCols )
{
    HB_SYMBOL_UNUSED( pGT );
    //log_printf("hb_gtnap_GetSize");
    *piRows = 500;
    *piCols = 250;
}

/*---------------------------------------------------------------------------*/

static void hb_gtnap_ExposeArea( PHB_GT pGT, int iTop, int iLeft, int iBottom, int iRight )
{
    HB_SYMBOL_UNUSED( pGT );
    //log_printf("hb_gtnap_ExposeArea(%d, %d, %d, %d)", iTop, iLeft, iBottom, iRight);
}

/*---------------------------------------------------------------------------*/

static int hb_gtnap_MaxCol( PHB_GT pGT )
{
    HB_SYMBOL_UNUSED( pGT );
    //log_printf("hb_gtnap_MaxCol()");
    if (GTNAP_GLOBAL->cualib_mode == TRUE)
        return GTNAP_GLOBAL->cols - 1;
    return 100;
}

/*---------------------------------------------------------------------------*/

static int hb_gtnap_MaxRow( PHB_GT pGT )
{
    HB_SYMBOL_UNUSED( pGT );
    //log_printf("hb_gtnap_MaxRow()");
    if (GTNAP_GLOBAL->cualib_mode == TRUE)
        return GTNAP_GLOBAL->rows - 1;
    return 25;
}

/*---------------------------------------------------------------------------*/

static HB_BOOL hb_gtnap_CheckPos( PHB_GT pGT, int iRow, int iCol, long * plIndex )
{
    HB_SYMBOL_UNUSED( pGT );
    //log_printf("hb_gtnap_CheckPos(%d, %d)", iRow, iCol);
    *plIndex = 0;
    return TRUE;
}

/*---------------------------------------------------------------------------*/

static void hb_gtnap_SetPos( PHB_GT pGT, int iRow, int iCol )
{
    GtNapCualibWindow *cuawin = i_current_cuawin(GTNAP_GLOBAL);
    HB_SYMBOL_UNUSED( pGT );
    if (cuawin != NULL)
    {
        cuawin->cursor_row = (int32_t)iRow;
        cuawin->cursor_col = (int32_t)iCol;
        //log_printf("hb_gtnap_SetPos(%d, %d)", iRow, iCol);
    }
    else
    {
        //log_printf("hb_gtnap_SetPos(%d, %d). NO GTNAP Window!", iRow, iCol);
    }
}

/*---------------------------------------------------------------------------*/

static void hb_gtnap_GetPos( PHB_GT pGT, int * piRow, int * piCol )
{
    GtNapCualibWindow *cuawin = i_current_cuawin(GTNAP_GLOBAL);
    HB_SYMBOL_UNUSED( pGT );
    if (cuawin != NULL)
    {
        *piRow = (int)cuawin->cursor_row;
        *piCol = (int)cuawin->cursor_col;
        //log_printf("hb_gtnap_GetPos(%d, %d)", *piRow, *piCol);
    }
    else
    {
        *piRow = 0;
        *piCol = 0;
        //log_printf("hb_gtnap_GetPos(%d, %d). NO GTNAP Window!", *piRow, *piCol);
    }
}

/*---------------------------------------------------------------------------*/

static HB_BOOL hb_gtnap_IsColor( PHB_GT pGT )
{
    HB_SYMBOL_UNUSED( pGT );
    //log_printf("hb_gtnap_IsColor()");
    return TRUE;
}

/*---------------------------------------------------------------------------*/

static int hb_gtnap_GetCursorStyle( PHB_GT pGT )
{
    HB_SYMBOL_UNUSED( pGT );
    //log_printf("hb_gtnap_GetCursorStyle()");
    return SC_NORMAL;
}

/*---------------------------------------------------------------------------*/

static void hb_gtnap_SetCursorStyle( PHB_GT pGT, int iStyle )
{
    HB_SYMBOL_UNUSED( pGT );
    //log_printf("hb_gtnap_SetCursorStyle(%d)", iStyle);

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
    //log_printf("hb_gtnap_DispBegin()");
}

/*---------------------------------------------------------------------------*/

static void hb_gtnap_DispEnd( PHB_GT pGT )
{
    HB_SYMBOL_UNUSED( pGT );
    //log_printf("hb_gtnap_DispEnd()");
}

/*---------------------------------------------------------------------------*/

static int hb_gtnap_DispCount( PHB_GT pGT )
{
    HB_SYMBOL_UNUSED( pGT );
    //log_printf("hb_gtnap_DispCount()");
    return 0;
}

/*---------------------------------------------------------------------------*/

static HB_BOOL hb_gtnap_GetChar( PHB_GT pGT, int iRow, int iCol, int * pbColor, HB_BYTE * pbAttr, HB_USHORT * pusChar )
{
    HB_SYMBOL_UNUSED( pGT );
    //log_printf("hb_gtnap_GetChar(%d, %d)", iRow, iCol);
    *pbColor = 0;
    *pbAttr = 0;
    *pusChar = 65;
    return TRUE;
}

/*---------------------------------------------------------------------------*/

static HB_BOOL hb_gtnap_PutChar( PHB_GT pGT, int iRow, int iCol, int bColor, HB_BYTE bAttr, HB_USHORT usChar )
{
    HB_SYMBOL_UNUSED( pGT );
    //log_printf("hb_gtnap_PutChar(%d, %d): %d, %d, %d", iRow, iCol, bColor, bAttr, usChar);
    return TRUE;
}

/*---------------------------------------------------------------------------*/

static void hb_gtnap_Save( PHB_GT pGT, int iTop, int iLeft, int iBottom, int iRight, void * pBuffer )
{
    HB_SYMBOL_UNUSED( pGT );
    HB_SYMBOL_UNUSED( pBuffer );
    //log_printf("hb_gtnap_Save(%d, %d, %d, %d)", iTop, iLeft, iBottom, iRight);
}

/*---------------------------------------------------------------------------*/

static void hb_gtnap_Rest( PHB_GT pGT, int iTop, int iLeft, int iBottom, int iRight, const void * pBuffer )
{
    HB_SYMBOL_UNUSED( pGT );
    HB_SYMBOL_UNUSED( pBuffer );
    //log_printf("hb_gtnap_Rest(%d, %d, %d, %d)", iTop, iLeft, iBottom, iRight);
}

/*---------------------------------------------------------------------------*/

static int hb_gtnap_PutText( PHB_GT pGT, int iRow, int iCol, int bColor, const char * pText, HB_SIZE ulLen )
{
    HB_SYMBOL_UNUSED( pGT );
    //log_printf("hb_gtnap_hb_gtnap_PutText(%d, %d, %d): %s (%d)", iRow, iCol, bColor, pText, (int)ulLen);
    return 0;
}

/*---------------------------------------------------------------------------*/

static void hb_gtnap_Replicate( PHB_GT pGT, int iRow, int iCol, int bColor, HB_BYTE bAttr, HB_USHORT usChar, HB_SIZE ulLen )
{
    HB_SYMBOL_UNUSED( pGT );
    //log_printf("hb_gtnap_Replicate(%d, %d, %d, %d, %d, %d)", iRow, iCol, bColor, bAttr, usChar, (int)ulLen);
}

/*---------------------------------------------------------------------------*/

static void hb_gtnap_WriteAt( PHB_GT pGT, int iRow, int iCol, const char * pText, HB_SIZE ulLength )
{
    GtNapCualibWindow *cuawin = i_current_cuawin(GTNAP_GLOBAL);
    HB_SYMBOL_UNUSED( pGT );
    if (cuawin != NULL)
    {
        String *ctext = gtconvert_1252_to_UTF8(pText);

        i_add_label_object(iCol - cuawin->N_ColIni, iRow - cuawin->N_LinIni, pText, 0, FALSE, GTNAP_GLOBAL, cuawin);
        //log_printf("hb_gtnap_WriteAt(%d, %d, %d): %s", iRow, iCol, (int)ulLength, tc(ctext));
        str_destroy(&ctext);
    }
    else
    {
        //log_printf("hb_gtnap_WriteAt(%d, %d, %d): %s -- NO GTNAP Window!", iRow, iCol, (int)ulLength, pText);
    }
}

/*---------------------------------------------------------------------------*/

static void hb_gtnap_SetAttribute( PHB_GT pGT, int iTop, int iLeft, int iBottom, int iRight, int bColor )
{
    HB_SYMBOL_UNUSED( pGT );
    //log_printf("hb_gtnap_SetAttribute(%d, %d, %d, %d): (%d)", iTop, iLeft, iBottom, iRight, bColor);
}

/*---------------------------------------------------------------------------*/

static void hb_gtnap_Scroll( PHB_GT pGT, int iTop, int iLeft, int iBottom, int iRight, int bColor, HB_USHORT bChar, int iRows, int iCols )
{
    GtNapCualibWindow *cuawin = i_current_cuawin(GTNAP_GLOBAL);
    HB_SYMBOL_UNUSED( pGT );
    if (cuawin != NULL)
    {
        uint32_t i, n;
        // FRAN: The scroll, at the moment, delete all texts
        // Improve taking into account the input rectangle
        // Take into account if a real scroll exists (iRows > 0 || iCols > 0)
        n = arrst_size(cuawin->gui_objects, GtNapCualibObject);
        for (i = 0; i < n; )
        {
            GtNapCualibObject *object = arrst_get(cuawin->gui_objects, i, GtNapCualibObject);
            const char_t *type = _component_type(object->component);
            if (str_equ_c(type, "Label") == TRUE)
            {
                i_remove_cualib_object(cuawin, i);
                n -= 1;
            }
            else
            {
                i += 1;
            }
        }

        //log_printf("hb_gt_wvw_Scroll(%d, %d, %d, %d): Rows:(%d) Cols:(%d) Color(%d)", iTop, iLeft, iBottom, iRight, iRows, iCols, bColor);
    }
    else
    {
        //log_printf("hb_gt_wvw_Scroll(%d, %d, %d, %d): Rows:(%d) Cols:(%d) Color(%d) -- NO GTNAP Window!", iTop, iLeft, iBottom, iRight, iRows, iCols, bColor);
    }

    HB_SYMBOL_UNUSED( bChar );
}

/*---------------------------------------------------------------------------*/

static void hb_gtnap_Box( PHB_GT pGT, int iTop, int iLeft, int iBottom, int iRight, const char * pbyFrame, int bColor )
{
    HB_SYMBOL_UNUSED( pGT );
    //log_printf("hb_gtnap_Box(%d, %d, %d, %d): (%d)", iTop, iLeft, iBottom, iRight, bColor);
    HB_SYMBOL_UNUSED( pbyFrame );
}

/*---------------------------------------------------------------------------*/

static void hb_gtnap_HorizLine( PHB_GT pGT, int iRow, int iLeft, int iRight, HB_USHORT bChar, int bColor )
{
    HB_SYMBOL_UNUSED( pGT );
    //log_printf("hb_gtnap_HorizLine(%d, %d, %d): (%d)", iRow, iLeft, iRight, bColor);
    HB_SYMBOL_UNUSED( bChar );
}

/*---------------------------------------------------------------------------*/

static void hb_gtnap_VertLine( PHB_GT pGT, int iCol, int iTop, int iBottom, HB_USHORT bChar, int bColor )
{
    HB_SYMBOL_UNUSED( pGT );
    //log_printf("hb_gtnap_VertLine(%d, %d, %d): (%d)", iCol, iTop, iBottom, bColor);
    HB_SYMBOL_UNUSED( bChar );
}

/*---------------------------------------------------------------------------*/

static HB_BOOL hb_gtnap_GetBlink( PHB_GT pGT )
{
    HB_SYMBOL_UNUSED( pGT );
    //log_printf("hb_gtnap_GetBlink()");
    return TRUE;
}

/*---------------------------------------------------------------------------*/

static void hb_gtnap_SetBlink( PHB_GT pGT, HB_BOOL bBlink )
{
    HB_SYMBOL_UNUSED( pGT );
    //log_printf("hb_gtnap_SetBlink(%d)", bBlink);
}

/*---------------------------------------------------------------------------*/

static const char * hb_gtnap_Version( PHB_GT pGT, int iType )
{
    HB_SYMBOL_UNUSED( pGT );
    //log_printf("hb_gtnap_Version(%d)", iType);
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
        log_printf("hb_gtnap_OutErr(%s (%d))", pbyStr, (int)ulLen);
    else
        log_printf("hb_gtnap_OutErr(empty)");
}

/*---------------------------------------------------------------------------*/
// dDuration is in 'Ticks' (18.2 per second)
static void hb_gtnap_Tone( PHB_GT pGT, double dFrequency, double dDuration )
{
    HB_SYMBOL_UNUSED( pGT );
    //log_printf("hb_gtnap_Tone(%.3f, %.3f)", dFrequency, dDuration);
}

/*---------------------------------------------------------------------------*/

static HB_BOOL hb_gtnap_Info( PHB_GT pGT, int iType, PHB_GT_INFO pInfo )
{
    HB_SYMBOL_UNUSED( pGT );
    HB_SYMBOL_UNUSED( pInfo );
    //log_printf("hb_gtnap_Info(%d)", iType);
    return TRUE;
}

/*---------------------------------------------------------------------------*/

static void hb_gtnap_mouse_Init( PHB_GT pGT )
{
    HB_SYMBOL_UNUSED( pGT );
    //log_printf("hb_gtnap_mouse_Init()");
}

/*---------------------------------------------------------------------------*/

static void hb_gtnap_mouse_Exit( PHB_GT pGT )
{
    HB_SYMBOL_UNUSED( pGT );
    //log_printf("hb_gtnap_mouse_Exit()");
}

/*---------------------------------------------------------------------------*/

static HB_BOOL hb_gtnap_mouse_IsPresent( PHB_GT pGT )
{
    HB_SYMBOL_UNUSED( pGT );
    //log_printf("hb_gtnap_mouse_IsPresent()");
    return TRUE;
}

/*---------------------------------------------------------------------------*/

static int hb_gtnap_mouse_Col( PHB_GT pGT )
{
    HB_SYMBOL_UNUSED( pGT );
    //log_printf("hb_gtnap_mouse_Col()");
    return 1;
}

/*---------------------------------------------------------------------------*/

static int hb_gtnap_mouse_Row( PHB_GT pGT )
{
    HB_SYMBOL_UNUSED( pGT );
    //log_printf("hb_gtnap_mouse_Row()");
    return 1;
}

/*---------------------------------------------------------------------------*/

static int hb_gtnap_mouse_CountButton( PHB_GT pGT )
{
    HB_SYMBOL_UNUSED( pGT );
    //log_printf("hb_gtnap_mouse_CountButton()");
    return 3;
}

/*---------------------------------------------------------------------------*/

static HB_BOOL hb_gtnap_mouse_ButtonState( PHB_GT pGT, int iButton )
{
    HB_SYMBOL_UNUSED( pGT );
    //log_printf("hb_gtnap_mouse_ButtonState(%d)", iButton);
    return FALSE;
}

/*---------------------------------------------------------------------------*/

static int hb_gtnap_gfxPrimitive( PHB_GT pGT, int iType, int iTop, int iLeft, int iBottom, int iRight, int iColor )
{
    HB_SYMBOL_UNUSED( pGT );
    //log_printf("hb_gtnap_gfxPrimitive(%d, %d, %d, %d, %d, %d)", iType, iTop, iLeft, iBottom, iRight, iColor);
    return 1;
}

/*---------------------------------------------------------------------------*/

static void hb_gtnap_gfxText( PHB_GT pGT, int iTop, int iLeft, const char * cBuf, int iColor, int iSize, int iWidth )
{
    HB_SYMBOL_UNUSED( pGT );
    //log_printf("hb_gtnap_gfxText(%d, %d, %s, %d, %d, %d)", iTop, iLeft, cBuf, iColor, iSize, iWidth);
}

/*---------------------------------------------------------------------------*/

static HB_BOOL hb_gt_FuncInit( PHB_GT_FUNCS pFuncTable )
{
    //log_printf("hb_gt_FuncInit()");

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
    pFuncTable->Alert = NULL;
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

static int s_GtId;
static HB_GT_FUNCS SuperTable;
#define HB_GTSUPER   ( &SuperTable )
#define HB_GTID_PTR  ( &s_GtId )

#include "hbgtreg.h"
