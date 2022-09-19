/*
    This is part of gtnap
*/

#include "gtnap.h"
#include "nappgui.h"

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_WINDOW_CREATE )
{
    uint32_t flags = hb_parni(1);
    Window *window = window_create(flags);
    hb_retWindow(window);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_WINDOW_PANEL )
{
    Window *window = hb_parWindow(1);
    Panel *panel = (Panel*)hb_parptr(2);
    window_panel(window, panel);
}

/*---------------------------------------------------------------------------*/

static void i_OnWindowClose(GtNapCallback *idp, Event *e)
{
    hb_gt_nap_callback(idp, e);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_WINDOW_ONCLOSE )
{
    Window *window = hb_parWindow(1);
    Listener *listener = hb_gt_nap_wind_listener(2, window, i_OnWindowClose);
    window_OnClose(window, listener);
}

/*---------------------------------------------------------------------------*/

static void i_OnWindowMoved(GtNapCallback *idp, Event *e)
{
    hb_gt_nap_callback(idp, e);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_WINDOW_ONMOVED )
{
    Window *window = hb_parWindow(1);
    Listener *listener = hb_gt_nap_wind_listener(2, window, i_OnWindowMoved);
    window_OnMoved(window, listener);
}

/*---------------------------------------------------------------------------*/

static void i_OnWindowResize(GtNapCallback *idp, Event *e)
{
    hb_gt_nap_callback(idp, e);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_WINDOW_ONRESIZE )
{
    Window *window = hb_parWindow(1);
    Listener *listener = hb_gt_nap_wind_listener(2, window, i_OnWindowResize);
    window_OnResize(window, listener);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_WINDOW_TITLE )
{
    Window *window = hb_parWindow(1);
    const char_t *text = hb_get_nap_text(2);
    window_title(window, text);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_WINDOW_SHOW )
{
    Window *window = hb_parWindow(1);
    window_show(window);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_WINDOW_HIDE )
{
    Window *window = hb_parWindow(1);
    window_hide(window);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_WINDOW_MODAL )
{
    Window *window = hb_parWindow(1);
    Window *parent = hb_gtnap_current_modal();
    uint32_t ret = UINT32_MAX;

    if (parent == NULL)
        parent = hb_gtnap_main_window();

    hb_gtnap_set_modal_window(window);
    ret = window_modal(window, parent);
    hb_gtnap_destroy_modal();
    hb_retni(ret);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_WINDOW_STOP_MODAL )
{
    Window *window = hb_gtnap_current_modal();
    uint32_t return_value = hb_parni(1);
    window_stop_modal(window, return_value);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_WINDOW_UPDATE )
{
    Window *window = hb_parWindow(1);
    window_update(window);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_WINDOW_ORIGIN )
{
    Window *window = hb_parWindow(1);
    real32_t x = (real32_t)hb_parnd(2);
    real32_t y = (real32_t)hb_parnd(3);
    window_origin(window, v2df(x, y));
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_WINDOW_SIZE )
{
    Window *window = hb_parWindow(1);
    real32_t width = (real32_t)hb_parnd(2);
    real32_t height = (real32_t)hb_parnd(3);
    window_size(window, s2df(width, height));
}

/*---------------------------------------------------------------------------*/

// V2Df window_get_origin(const Window *window);

// S2Df window_get_size(const Window *window);

// S2Df window_get_client_size(const Window *window);

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_WINDOW_DEFBUTTON )
{
    Window *window = hb_parWindow(1);
    Button *button = (Button*)hb_parptr(2);
    window_defbutton(window, button);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_WINDOW_FOCUS )
{
    Window *window = hb_parWindow(1);
    Layout *layout = (Layout*)hb_parptr(2);
    uint32_t col = hb_parni(3);
    uint32_t row = hb_parni(4);
    Cell *cell = layout_cell(layout, col, row);
    unref(window);
    cell_focus(cell);
}
