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

typedef struct _gtnap_t GtNap;
typedef struct _gtnap_key_t GtNapKey;
typedef struct _gtnap_column_t GtNapColumn;
typedef struct _gtnap_cualib_object_t GtNapCualibObject;
typedef struct _gtnap_cualib_window_t GtNapCualibWindow;
typedef struct _gui_context_t GuiContext;

struct _gtnap_callback_t
{
    GtNapCualibWindow *cuawin;
    GuiComponent *cb_component;
    Window *cb_window;
    int32_t key;
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
    align_t align;
    PHB_ITEM codeBlock;
};

DeclSt(GtNapColumn);

struct _gtnap_area_t
{
    AREA *area;
    uint32_t currow;
    TableView *view;
    char_t temp[512];   // Temporal buffer between RDD and TableView
    ArrSt(GtNapColumn) *columns;
};

DeclPt(GtNapCallback);
DeclPt(GtNapArea);
DeclPt(Window);

typedef enum _objtype_t
{
    ekOBJ_LABEL,
    ekOBJ_MENUVERT,
    ekOBJ_IMAGE
} objtype_t;

struct _gtnap_cualib_object_t
{
    uint32_t cell_x;
    uint32_t cell_y;
    objtype_t type;
    GuiComponent *component;
};

DeclSt(GtNapCualibObject);

struct _gtnap_cualib_window_t
{
    uint32_t N_LinIni;
    uint32_t N_ColIni;
    uint32_t N_LinFin;
    uint32_t N_ColFin;
    uint32_t row_pos;
    uint32_t col_pos;
    Window *window;
    Panel *panel;
    ArrSt(GtNapCualibObject) *gui_objects;
    ArrPt(GtNapCallback) *callbacks;
};

DeclSt(GtNapCualibWindow);

struct _gtnap_t
{
    bool_t cualib_mode;
    Font *global_font;

    // Only for cualib-gtnap
    String *title;
    uint32_t rows;
    uint32_t cols;
    uint32_t cell_x_size;
    uint32_t cell_y_size;
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
{ K_F12, ekKEY_F12, 0 }
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
    if (column->codeBlock != NULL)
        hb_itemRelease(column->codeBlock);
}

/*---------------------------------------------------------------------------*/

static void i_destroy_area(GtNapArea **area)
{
    cassert_no_null(area);
    cassert_no_null(*area);
    arrst_destroy(&(*area)->columns, i_remove_column, GtNapColumn);
    heap_delete(area, GtNapArea);
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

    if (gtarea->area != NULL)
    {
        SELF_GOTO(gtarea->area, 1);
        gtarea->currow = 1;
    }

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
    column = arrst_new(area->columns, GtNapColumn);
    column->align = align;
    column->codeBlock = hb_itemNew(codeBlock);
}

/*---------------------------------------------------------------------------*/

void hb_gtnap_area_set_row(GtNapArea *area, const uint32_t row)
{
    cassert_no_null(area);
    if (area->currow != row)
    {
        SELF_GOTO(area->area, (HB_ULONG)row);
        area->currow = row;
    }
}

/*---------------------------------------------------------------------------*/

uint32_t hb_gtnap_area_row_count(GtNapArea *area)
{
    HB_ULONG ulRecCount = 0;
    cassert_no_null(area);
    SELF_RECCOUNT(area->area, &ulRecCount);
    return (uint32_t)ulRecCount;
}

/*---------------------------------------------------------------------------*/

const char_t *hb_gtnap_area_eval_field(GtNapArea *area, const uint32_t field_id, const uint32_t row_id, align_t *align)
{
    const GtNapColumn *column = NULL;
    PHB_ITEM pItem = NULL;
    HB_TYPE type = 0;

    cassert_no_null(area);
    cassert(field_id > 0);
    cassert(row_id > 0);

    // First, select the row in area
    if (area->currow != row_id)
    {
        SELF_GOTO(area->area, (HB_ULONG)row_id);
        area->currow = row_id;
    }

    column = arrst_get_const(area->columns, field_id - 1, GtNapColumn);
    pItem = hb_itemDo(column->codeBlock, 0);
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

static GtNap *i_gtnap_cualib_create(void)
{
    real32_t w, h;
    PHB_ITEM pRet = NULL;
    GTNAP_GLOBAL = heap_new0(GtNap);
    GTNAP_GLOBAL->cualib_mode = TRUE;
    GTNAP_GLOBAL->global_font = font_monospace(20, 0);
    GTNAP_GLOBAL->title = gtconvert_1252_to_UTF8(CUALIB_TITLE);
    GTNAP_GLOBAL->rows = CUALIB_ROWS;
    GTNAP_GLOBAL->cols = CUALIB_COLS;
    font_extents(GTNAP_GLOBAL->global_font, "OOOOOO", -1, &w, &h);
    GTNAP_GLOBAL->cell_x_size = (uint32_t)(w / 6.f);
    GTNAP_GLOBAL->cell_y_size = (uint32_t)h;
    GTNAP_GLOBAL->linespacing = 0;
    GTNAP_GLOBAL->cualib_windows = arrst_create(GtNapCualibWindow);
    log_printf("i_gtnap_cualib_create(%s, %d, %d)", CUALIB_TITLE, CUALIB_ROWS, CUALIB_COLS);
    pRet = hb_itemDo(CUALIB_INIT_CODEBLOCK, 0);
    hb_itemRelease(pRet);
    hb_itemRelease(CUALIB_INIT_CODEBLOCK);
    CUALIB_INIT_CODEBLOCK = NULL;
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
    if (str_equ_c(type, "Panel") == TRUE)
        _panel_detach_components((Panel*)object->component);

    _component_detach_from_panel((GuiComponent*)cuawin->panel, object->component);
    _component_destroy(&object->component);
    arrst_delete(cuawin->gui_objects, index, NULL, GtNapCualibObject);
}

/*---------------------------------------------------------------------------*/

static void i_remove_cualib_win(GtNapCualibWindow *cuawin)
{
    uint32_t i, n;
    cassert_no_null(cuawin);
    n = arrst_size(cuawin->gui_objects, GtNapCualibObject);
    for (i = 0; i < n; ++i)
        i_remove_cualib_object(cuawin, 0);

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
    str_destroy(&GTNAP_GLOBAL->title);
    heap_delete(&GTNAP_GLOBAL, GtNap);
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

static void i_add_object(const objtype_t type, const uint32_t cell_x, const uint32_t cell_y, const uint32_t cell_x_size, const uint32_t cell_y_size, const S2Df *size, GuiComponent *component, GtNapCualibWindow *cuawin)
{
    V2Df pos;
    GtNapCualibObject *object = NULL;
    cassert_no_null(cuawin);
    pos.x = (real32_t)(cell_x * cell_x_size);
    pos.y = (real32_t)(cell_y * cell_y_size);
    _component_attach_to_panel((GuiComponent*)cuawin->panel, component);
    _component_visible(component, FALSE);
    _component_set_frame(component, &pos, size);
    object = arrst_new0(cuawin->gui_objects, GtNapCualibObject);
    object->type = type;
    object->cell_x = cell_x;
    object->cell_y = cell_y;
    object->component = component;
    log_printf("Added object at: %.2f, %.2f w:%.2f h:%.2f", pos.x, pos.y, size->width, size->height);
}

/*---------------------------------------------------------------------------*/

static void i_add_label_object(const uint32_t cell_x, const uint32_t cell_y, const char_t *text, const color_t background, GtNap *gtnap, GtNapCualibWindow *cuawin)
{
    Label *label = label_create();
    uint32_t len = str_len_c(text);
    String *ctext = gtconvert_1252_to_UTF8(text);
    S2Df size;
    cassert_no_null(gtnap);
    log_printf("Added label: %s at %d %d", tc(ctext), cell_x, cell_y);
    label_text(label, tc(ctext));
    label_font(label, gtnap->global_font);

    if (background != 0)
        label_bgcolor(label, background);

    size.width = (real32_t)(len * gtnap->cell_x_size);
    size.height = (real32_t)gtnap->cell_y_size;
    i_add_object(ekOBJ_LABEL, cell_x, cell_y, gtnap->cell_x_size, gtnap->cell_y_size, &size, (GuiComponent*)label, cuawin);
    str_destroy(&ctext);
}

/*---------------------------------------------------------------------------*/

uint32_t hb_gtnap_cualib_window(const uint32_t N_LinIni, const uint32_t N_ColIni, const uint32_t N_LinFin, const uint32_t N_ColFin, const char_t *C_Cabec)
{
    uint32_t id = UINT32_MAX;
    GtNapCualibWindow *cuawin = NULL;
    Window *window = window_create(ekWNSTD | ekWNESC);
    Panel *panel = panel_create();
    Layout *layout = layout_create(1, 1);
    real32_t width, height;
    id = arrst_size(GTNAP_GLOBAL->cualib_windows, GtNapCualibWindow);
    cuawin = arrst_new0(GTNAP_GLOBAL->cualib_windows, GtNapCualibWindow);
    cuawin->window = window;
    cuawin->panel = panel;
    cuawin->N_LinIni = N_LinIni;
    cuawin->N_ColIni = N_ColIni;
    cuawin->N_LinFin = N_LinFin;
    cuawin->N_ColFin = N_ColFin;
    cuawin->row_pos = 0;
    cuawin->col_pos = 0;
    cuawin->gui_objects = arrst_create(GtNapCualibObject);
    cuawin->callbacks = arrpt_create(GtNapCallback);
    width = (real32_t)(GTNAP_GLOBAL->cell_x_size * (cuawin->N_ColFin - cuawin->N_ColIni + 1));
    height = (real32_t)(GTNAP_GLOBAL->cell_y_size * (cuawin->N_LinFin - cuawin->N_LinIni + 1));
    panel_size(panel, s2df(width, height));
    if (str_empty_c(C_Cabec) == FALSE)
        window_title(window, C_Cabec);
    else
        window_title(window, tc(GTNAP_GLOBAL->title));

    panel_layout(panel, layout);
    //window_OnClose(window, listener(GTNAP_GLOBAL, i_OnWindowClose, GtNap));
    window_panel(window, panel);
    return id;
}

/*---------------------------------------------------------------------------*/

void hb_gtnap_cualib_menuvert(Panel *panel, const uint32_t nTop, const uint32_t nLeft, const uint32_t nBottom, const uint32_t nRight)
{
    GtNapCualibWindow *cuawin = i_current_cuawin(GTNAP_GLOBAL);
    S2Df size, final_size;
    cassert_no_null(cuawin);
    //log_printf("Added MenuVert into CUALIB Window: %d, %d, %d, %d", nTop, nLeft, nBottom, nRight);
    size.width = (real32_t)((nRight - nLeft + 1) * GTNAP_GLOBAL->cell_x_size);
    size.height = (real32_t)((nBottom - nTop) * GTNAP_GLOBAL->cell_y_size);
    _panel_compose(panel, &size, &final_size);
    _panel_locate(panel);
    i_add_object(ekOBJ_MENUVERT, nLeft, nTop, GTNAP_GLOBAL->cell_x_size, GTNAP_GLOBAL->cell_y_size, &size, (GuiComponent*)panel, cuawin);
    //log_printf("MenuVert size: %.2f, %.2f, %.2f, %.2f", size.width, size.height, final_size.width, final_size.height);
}

/*---------------------------------------------------------------------------*/

void hb_gtnap_cualib_image(const char_t *pathname, const uint32_t nTop, const uint32_t nLeft, const uint32_t nBottom, const uint32_t nRight)
{
    GtNapCualibWindow *cuawin = i_current_cuawin(GTNAP_GLOBAL);
    Image *image = image_from_file(pathname, NULL);
    cassert_no_null(cuawin);
    if (image != NULL)
    {
        S2Df size;
        ImageView *view = imageview_create();
        log_printf("Added IMAGE into CUALIB Window: %d, %d, %d, %d", nTop, nLeft, nBottom, nRight);
        imageview_image(view, image);
        imageview_scale(view, ekAUTO);
        size.width = (real32_t)((nRight - nLeft + 1) * GTNAP_GLOBAL->cell_x_size);
        size.height = (real32_t)((nBottom - nTop + 1) * GTNAP_GLOBAL->cell_y_size);
        i_add_object(ekOBJ_IMAGE, nLeft, nTop, GTNAP_GLOBAL->cell_x_size, GTNAP_GLOBAL->cell_y_size, &size, (GuiComponent*)view, cuawin);
        image_destroy(&image);
    }
    else
    {
        log_printf("Cannot load '%s' image", pathname);
    }
}

/*---------------------------------------------------------------------------*/

void hb_gtnap_cualib_label(const char_t *text, const uint32_t nLin, const uint32_t nCol)
{
    GtNapCualibWindow *cuawin = i_current_cuawin(GTNAP_GLOBAL);
    i_add_label_object(nCol, nLin, text, kCOLOR_CYAN, GTNAP_GLOBAL, cuawin);
}

/*---------------------------------------------------------------------------*/

static Listener *i_gtnap_cualib_listener(const uint32_t codeBlockParamId, const int32_t key, GtNapCualibWindow *cuawin, FPtr_gtnap_callback func_callback)
{
    PHB_ITEM codeBlock = hb_param(codeBlockParamId, HB_IT_BLOCK);
    GtNapCallback *callback = heap_new0(GtNapCallback);
    cassert_no_null(codeBlock);
    callback->codeBlock = hb_itemNew(codeBlock);
    callback->cuawin = cuawin;
    callback->key = key;
    arrpt_append(cuawin->callbacks, callback, GtNapCallback);
    return listener(callback, func_callback, GtNapCallback);
}

/*---------------------------------------------------------------------------*/

static const GtNapKey *i_convert_key(const int32_t key)
{
    uint32_t i, n = sizeof(KEYMAPS) / sizeof(GtNapKey);
    for (i = 0; i < n; ++i)
    {
        if (KEYMAPS[i].hkey == key)
            return &KEYMAPS[i];
    }

    return NULL;
}

/*---------------------------------------------------------------------------*/

static void i_OnWindowHotKey(GtNapCallback *callback, Event *e)
{
    hb_gtnap_callback(callback, e);
    log_printf("Pressed hotkey %d", callback->key);
}

/*---------------------------------------------------------------------------*/

void hb_gtnap_cualib_hotkey(const int32_t key, const uint32_t codeBlockParamId)
{
    GtNapCualibWindow *cuawin = i_current_cuawin(GTNAP_GLOBAL);
    const GtNapKey *nkey = i_convert_key(key);
    cassert_no_null(cuawin);

    // Exists a Harbour/NAppGUI key convertion
    if (nkey != NULL)
    {
        uint32_t pos = UINT32_MAX;
        Listener *listener = NULL;

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

        listener = i_gtnap_cualib_listener(codeBlockParamId, key, cuawin, i_OnWindowHotKey);
        window_hotkey(cuawin->window, nkey->key, nkey->modifiers, listener);
    }
}

/*---------------------------------------------------------------------------*/

uint32_t hb_gtnap_cualib_launch_modal(void)
{
    GtNapCualibWindow *cuawin = i_current_cuawin(GTNAP_GLOBAL);
    GtNapCualibWindow *parent = i_parent_cuawin(GTNAP_GLOBAL);
    uint32_t ret = 0;
    cassert_no_null(cuawin);
    _window_taborder(cuawin->window, NULL);

    arrst_foreach(object, cuawin->gui_objects, GtNapCualibObject)
        _component_visible(object->component, TRUE);

        switch(object->type) {
        case ekOBJ_LABEL:
        case ekOBJ_IMAGE:
            break;
        case ekOBJ_MENUVERT:
            nap_menuvert_taborder((Panel*)object->component, cuawin->window);
            break;
        cassert_default();
        }

    arrst_end();

    ret = window_modal(cuawin->window, parent ? parent->window : NULL);
    return ret;
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
    log_printf("hb_gtnap_Init()");
    HB_SYMBOL_UNUSED( hFilenoStdin );
    HB_SYMBOL_UNUSED( hFilenoStdout );
    HB_SYMBOL_UNUSED( hFilenoStderr );
}

/*---------------------------------------------------------------------------*/

static void hb_gtnap_Exit( PHB_GT pGT )
{
    HB_SYMBOL_UNUSED( pGT );
    log_printf("hb_gtnap_Exit()");

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
    log_printf("hb_gtnap_Resize(%d, %d)", iRow, iCol);
    return TRUE;
}

/*---------------------------------------------------------------------------*/

static HB_BOOL hb_gtnap_SetMode( PHB_GT pGT, int iRow, int iCol )
{
    HB_SYMBOL_UNUSED( pGT );
    log_printf("hb_gtnap_SetMode(%d, %d)", iRow, iCol);
    return TRUE;
}

/*---------------------------------------------------------------------------*/

static void hb_gtnap_GetSize( PHB_GT pGT, int * piRows, int  * piCols )
{
    HB_SYMBOL_UNUSED( pGT );
    log_printf("hb_gtnap_GetSize");
    *piRows = 500;
    *piCols = 250;
}

/*---------------------------------------------------------------------------*/

static void hb_gtnap_ExposeArea( PHB_GT pGT, int iTop, int iLeft, int iBottom, int iRight )
{
    HB_SYMBOL_UNUSED( pGT );
    log_printf("hb_gtnap_ExposeArea(%d, %d, %d, %d)", iTop, iLeft, iBottom, iRight);
}

/*---------------------------------------------------------------------------*/

static int hb_gtnap_MaxCol( PHB_GT pGT )
{
    HB_SYMBOL_UNUSED( pGT );
    log_printf("hb_gtnap_MaxCol()");
    if (GTNAP_GLOBAL->cualib_mode == TRUE)
        return GTNAP_GLOBAL->cols;
    return 100;
}

/*---------------------------------------------------------------------------*/

static int hb_gtnap_MaxRow( PHB_GT pGT )
{
    HB_SYMBOL_UNUSED( pGT );
    log_printf("hb_gtnap_MaxRow()");
    if (GTNAP_GLOBAL->cualib_mode == TRUE)
        return GTNAP_GLOBAL->rows;
    return 25;
}

/*---------------------------------------------------------------------------*/

static HB_BOOL hb_gtnap_CheckPos( PHB_GT pGT, int iRow, int iCol, long * plIndex )
{
    HB_SYMBOL_UNUSED( pGT );
    log_printf("hb_gtnap_CheckPos(%d, %d)", iRow, iCol);
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
        cuawin->row_pos = iRow;
        cuawin->col_pos = iCol;
        log_printf("hb_gtnap_SetPos(%d, %d)", iRow, iCol);
    }
    else
    {
        log_printf("hb_gtnap_SetPos(%d, %d). NO GTNAP Window!", iRow, iCol);
    }
}

/*---------------------------------------------------------------------------*/

static void hb_gtnap_GetPos( PHB_GT pGT, int * piRow, int * piCol )
{
    GtNapCualibWindow *cuawin = i_current_cuawin(GTNAP_GLOBAL);
    HB_SYMBOL_UNUSED( pGT );
    if (cuawin != NULL)
    {
        *piRow = cuawin->row_pos;
        *piCol = cuawin->col_pos;
        log_printf("hb_gtnap_GetPos(%d, %d)", *piRow, *piCol);
    }
    else
    {
        *piRow = 0;
        *piCol = 0;
        log_printf("hb_gtnap_GetPos(%d, %d). NO GTNAP Window!", *piRow, *piCol);
    }
}

/*---------------------------------------------------------------------------*/

static HB_BOOL hb_gtnap_IsColor( PHB_GT pGT )
{
    HB_SYMBOL_UNUSED( pGT );
    log_printf("hb_gtnap_IsColor()");
    return TRUE;
}

/*---------------------------------------------------------------------------*/

static int hb_gtnap_GetCursorStyle( PHB_GT pGT )
{
    HB_SYMBOL_UNUSED( pGT );
    log_printf("hb_gtnap_GetCursorStyle()");
    return SC_NORMAL;
}

/*---------------------------------------------------------------------------*/

static void hb_gtnap_SetCursorStyle( PHB_GT pGT, int iStyle )
{
    HB_SYMBOL_UNUSED( pGT );
    log_printf("hb_gtnap_SetCursorStyle(%d)", iStyle);

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
    log_printf("hb_gtnap_DispBegin()");
}

/*---------------------------------------------------------------------------*/

static void hb_gtnap_DispEnd( PHB_GT pGT )
{
    HB_SYMBOL_UNUSED( pGT );
    log_printf("hb_gtnap_DispEnd()");
}

/*---------------------------------------------------------------------------*/

static int hb_gtnap_DispCount( PHB_GT pGT )
{
    HB_SYMBOL_UNUSED( pGT );
    log_printf("hb_gtnap_DispCount()");
    return 0;
}

/*---------------------------------------------------------------------------*/

static HB_BOOL hb_gtnap_GetChar( PHB_GT pGT, int iRow, int iCol, int * pbColor, HB_BYTE * pbAttr, HB_USHORT * pusChar )
{
    HB_SYMBOL_UNUSED( pGT );
    log_printf("hb_gtnap_GetChar(%d, %d)", iRow, iCol);
    *pbColor = 0;
    *pbAttr = 0;
    *pusChar = 65;
    return TRUE;
}

/*---------------------------------------------------------------------------*/

static HB_BOOL hb_gtnap_PutChar( PHB_GT pGT, int iRow, int iCol, int bColor, HB_BYTE bAttr, HB_USHORT usChar )
{
    HB_SYMBOL_UNUSED( pGT );
    log_printf("hb_gtnap_PutChar(%d, %d): %d, %d, %d", iRow, iCol, bColor, bAttr, usChar);
    return TRUE;
}

/*---------------------------------------------------------------------------*/

static void hb_gtnap_Save( PHB_GT pGT, int iTop, int iLeft, int iBottom, int iRight, void * pBuffer )
{
    HB_SYMBOL_UNUSED( pGT );
    HB_SYMBOL_UNUSED( pBuffer );
    log_printf("hb_gtnap_Save(%d, %d, %d, %d)", iTop, iLeft, iBottom, iRight);
}

/*---------------------------------------------------------------------------*/

static void hb_gtnap_Rest( PHB_GT pGT, int iTop, int iLeft, int iBottom, int iRight, const void * pBuffer )
{
    HB_SYMBOL_UNUSED( pGT );
    HB_SYMBOL_UNUSED( pBuffer );
    log_printf("hb_gtnap_Rest(%d, %d, %d, %d)", iTop, iLeft, iBottom, iRight);
}

/*---------------------------------------------------------------------------*/

static int hb_gtnap_PutText( PHB_GT pGT, int iRow, int iCol, int bColor, const char * pText, HB_SIZE ulLen )
{
    HB_SYMBOL_UNUSED( pGT );
    log_printf("hb_gtnap_hb_gtnap_PutText(%d, %d, %d): %s (%d)", iRow, iCol, bColor, pText, (int)ulLen);
    return 0;
}

/*---------------------------------------------------------------------------*/

static void hb_gtnap_Replicate( PHB_GT pGT, int iRow, int iCol, int bColor, HB_BYTE bAttr, HB_USHORT usChar, HB_SIZE ulLen )
{
    HB_SYMBOL_UNUSED( pGT );
    log_printf("hb_gtnap_Replicate(%d, %d, %d, %d, %d, %d)", iRow, iCol, bColor, bAttr, usChar, (int)ulLen);
}

/*---------------------------------------------------------------------------*/

static void hb_gtnap_WriteAt( PHB_GT pGT, int iRow, int iCol, const char * pText, HB_SIZE ulLength )
{
    GtNapCualibWindow *cuawin = i_current_cuawin(GTNAP_GLOBAL);
    HB_SYMBOL_UNUSED( pGT );
    if (cuawin != NULL)
    {
        i_add_label_object(iCol, iRow, pText, 0, GTNAP_GLOBAL, cuawin);
        log_printf("hb_gtnap_WriteAt(%d, %d, %d): %s", iRow, iCol, (int)ulLength, pText);
    }
    else
    {
        log_printf("hb_gtnap_WriteAt(%d, %d, %d): %s -- NO GTNAP Window!", iRow, iCol, (int)ulLength, pText);
    }
}

/*---------------------------------------------------------------------------*/

static void hb_gtnap_SetAttribute( PHB_GT pGT, int iTop, int iLeft, int iBottom, int iRight, int bColor )
{
    HB_SYMBOL_UNUSED( pGT );
    log_printf("hb_gtnap_SetAttribute(%d, %d, %d, %d): (%d)", iTop, iLeft, iBottom, iRight, bColor);
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

        log_printf("hb_gt_wvw_Scroll(%d, %d, %d, %d): Rows:(%d) Cols:(%d) Color(%d)", iTop, iLeft, iBottom, iRight, iRows, iCols, bColor);
    }
    else
    {
        log_printf("hb_gt_wvw_Scroll(%d, %d, %d, %d): Rows:(%d) Cols:(%d) Color(%d) -- NO GTNAP Window!", iTop, iLeft, iBottom, iRight, iRows, iCols, bColor);
    }

    HB_SYMBOL_UNUSED( bChar );
}

/*---------------------------------------------------------------------------*/

static void hb_gtnap_Box( PHB_GT pGT, int iTop, int iLeft, int iBottom, int iRight, const char * pbyFrame, int bColor )
{
    HB_SYMBOL_UNUSED( pGT );
    log_printf("hb_gtnap_Box(%d, %d, %d, %d): (%d)", iTop, iLeft, iBottom, iRight, bColor);
    HB_SYMBOL_UNUSED( pbyFrame );
}

/*---------------------------------------------------------------------------*/

static void hb_gtnap_HorizLine( PHB_GT pGT, int iRow, int iLeft, int iRight, HB_USHORT bChar, int bColor )
{
    HB_SYMBOL_UNUSED( pGT );
    log_printf("hb_gtnap_HorizLine(%d, %d, %d): (%d)", iRow, iLeft, iRight, bColor);
    HB_SYMBOL_UNUSED( bChar );
}

/*---------------------------------------------------------------------------*/

static void hb_gtnap_VertLine( PHB_GT pGT, int iCol, int iTop, int iBottom, HB_USHORT bChar, int bColor )
{
    HB_SYMBOL_UNUSED( pGT );
    log_printf("hb_gtnap_VertLine(%d, %d, %d): (%d)", iCol, iTop, iBottom, bColor);
    HB_SYMBOL_UNUSED( bChar );
}

/*---------------------------------------------------------------------------*/

static HB_BOOL hb_gtnap_GetBlink( PHB_GT pGT )
{
    HB_SYMBOL_UNUSED( pGT );
    log_printf("hb_gtnap_GetBlink()");
    return TRUE;
}

/*---------------------------------------------------------------------------*/

static void hb_gtnap_SetBlink( PHB_GT pGT, HB_BOOL bBlink )
{
    HB_SYMBOL_UNUSED( pGT );
    log_printf("hb_gtnap_SetBlink(%d)", bBlink);
}

/*---------------------------------------------------------------------------*/

static const char * hb_gtnap_Version( PHB_GT pGT, int iType )
{
    HB_SYMBOL_UNUSED( pGT );
    log_printf("hb_gtnap_Version(%d)", iType);
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
    log_printf("hb_gtnap_Tone(%.3f, %.3f)", dFrequency, dDuration);
}

/*---------------------------------------------------------------------------*/

static HB_BOOL hb_gtnap_Info( PHB_GT pGT, int iType, PHB_GT_INFO pInfo )
{
    HB_SYMBOL_UNUSED( pGT );
    HB_SYMBOL_UNUSED( pInfo );
    log_printf("hb_gtnap_Info(%d)", iType);
    return TRUE;
}

/*---------------------------------------------------------------------------*/

static void hb_gtnap_mouse_Init( PHB_GT pGT )
{
    HB_SYMBOL_UNUSED( pGT );
    log_printf("hb_gtnap_mouse_Init()");
}

/*---------------------------------------------------------------------------*/

static void hb_gtnap_mouse_Exit( PHB_GT pGT )
{
    HB_SYMBOL_UNUSED( pGT );
    log_printf("hb_gtnap_mouse_Exit()");
}

/*---------------------------------------------------------------------------*/

static HB_BOOL hb_gtnap_mouse_IsPresent( PHB_GT pGT )
{
    HB_SYMBOL_UNUSED( pGT );
    log_printf("hb_gtnap_mouse_IsPresent()");
    return TRUE;
}

/*---------------------------------------------------------------------------*/

static int hb_gtnap_mouse_Col( PHB_GT pGT )
{
    HB_SYMBOL_UNUSED( pGT );
    log_printf("hb_gtnap_mouse_Col()");
    return 1;
}

/*---------------------------------------------------------------------------*/

static int hb_gtnap_mouse_Row( PHB_GT pGT )
{
    HB_SYMBOL_UNUSED( pGT );
    log_printf("hb_gtnap_mouse_Row()");
    return 1;
}

/*---------------------------------------------------------------------------*/

static int hb_gtnap_mouse_CountButton( PHB_GT pGT )
{
    HB_SYMBOL_UNUSED( pGT );
    log_printf("hb_gtnap_mouse_CountButton()");
    return 3;
}

/*---------------------------------------------------------------------------*/

static HB_BOOL hb_gtnap_mouse_ButtonState( PHB_GT pGT, int iButton )
{
    HB_SYMBOL_UNUSED( pGT );
    log_printf("hb_gtnap_mouse_ButtonState(%d)", iButton);
    return FALSE;
}

/*---------------------------------------------------------------------------*/

static int hb_gtnap_gfxPrimitive( PHB_GT pGT, int iType, int iTop, int iLeft, int iBottom, int iRight, int iColor )
{
    HB_SYMBOL_UNUSED( pGT );
    log_printf("hb_gtnap_gfxPrimitive(%d, %d, %d, %d, %d, %d)", iType, iTop, iLeft, iBottom, iRight, iColor);
    return 1;
}

/*---------------------------------------------------------------------------*/

static void hb_gtnap_gfxText( PHB_GT pGT, int iTop, int iLeft, const char * cBuf, int iColor, int iSize, int iWidth )
{
    HB_SYMBOL_UNUSED( pGT );
    log_printf("hb_gtnap_gfxText(%d, %d, %s, %d, %d, %d)", iTop, iLeft, cBuf, iColor, iSize, iWidth);
}

/*---------------------------------------------------------------------------*/

static HB_BOOL hb_gt_FuncInit( PHB_GT_FUNCS pFuncTable )
{
    log_printf("hb_gt_FuncInit()");

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
