/*
    This is part of gtnap
*/

#include "gtnap.h"
#include "gtnap.inl"
#include "nappgui.h"

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_WINDOW )
{
    int32_t top = hb_parni(1);
    int32_t left = hb_parni(2);
    int32_t bottom = hb_parni(3);
    int32_t right = hb_parni(4);
    const char_t *title = hb_parcx(5);
    bool_t close_return = (bool_t)hb_parl(6);
    bool_t close_esc = (bool_t)hb_parl(7);
    bool_t minimize_button = (bool_t)hb_parl(8);
    bool_t buttons_navigation = (bool_t)hb_parl(9);
    uint32_t id = hb_gtnap_window(top, left, bottom, right, title, close_return, close_esc, minimize_button, buttons_navigation);
    hb_retni(id);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_WINDOW_SCROLL )
{
    uint32_t wid = hb_parni(1);
    int32_t top = hb_parni(2);
    int32_t left = hb_parni(3);
    int32_t bottom = hb_parni(4);
    int32_t right = hb_parni(5);
    hb_gtnap_window_scroll(wid, top, left, bottom, right);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_WINDOW_EDITABLE )
{
    uint32_t wid = hb_parni(1);
    HB_ITEM *is_editable_block = hb_param(2, HB_IT_BLOCK);
    hb_gtnap_window_editable(wid, is_editable_block);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_WINDOW_CONFIRM )
{
    uint32_t wid = hb_parni(1);
    HB_ITEM *confirm_block = hb_param(2, HB_IT_BLOCK);
    hb_gtnap_window_confirm(wid, confirm_block);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_WINDOW_DESIST )
{
    uint32_t wid = hb_parni(1);
    HB_ITEM *desist_block = hb_param(2, HB_IT_BLOCK);
    hb_gtnap_window_desist(wid, desist_block);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_WINDOW_ERRDATE )
{
    uint32_t wid = hb_parni(1);
    HB_ITEM *error_date_block = hb_param(2, HB_IT_BLOCK);
    hb_gtnap_window_errdate(wid, error_date_block);
}
















/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_WINDOW_PANEL )
{
    Window *window = hb_gtnap_parWindow(1);
    Panel *panel = (Panel*)hb_parptr(2);
    window_panel(window, panel);
}

///*---------------------------------------------------------------------------*/
//
//static void i_OnWindowClose(GtNapCallback *idp, Event *e)
//{
//    hb_gtnap_callback(idp, e);
//}
//
///*---------------------------------------------------------------------------*/
//
//HB_FUNC( NAP_WINDOW_ONCLOSE )
//{
//    Window *window = hb_gtnap_parWindow(1);
//    Listener *listener = hb_gtnap_wind_listener(2, window, i_OnWindowClose);
//    window_OnClose(window, listener);
//}

/*---------------------------------------------------------------------------*/

//static void i_OnWindowMoved(GtNapCallback *idp, Event *e)
//{
//    hb_gtnap_callback(idp, e);
//}
//
///*---------------------------------------------------------------------------*/
//
//HB_FUNC( NAP_WINDOW_ONMOVED )
//{
//    Window *window = hb_gtnap_parWindow(1);
//    Listener *listener = hb_gtnap_wind_listener(2, window, i_OnWindowMoved);
//    window_OnMoved(window, listener);
//}

/*---------------------------------------------------------------------------*/

//static void i_OnWindowResize(GtNapCallback *idp, Event *e)
//{
//    hb_gtnap_callback(idp, e);
//}
//
///*---------------------------------------------------------------------------*/
//
//HB_FUNC( NAP_WINDOW_ONRESIZE )
//{
//    Window *window = hb_gtnap_parWindow(1);
//    Listener *listener = hb_gtnap_wind_listener(2, window, i_OnWindowResize);
//    window_OnResize(window, listener);
//}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_WINDOW_TITLE )
{
    Window *window = hb_gtnap_parWindow(1);
    const char_t *text = hb_gtnap_parText(2);
    window_title(window, text);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_WINDOW_SHOW )
{
    Window *window = hb_gtnap_parWindow(1);
    window_show(window);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_WINDOW_HIDE )
{
    Window *window = hb_gtnap_parWindow(1);
    window_hide(window);
}

/*---------------------------------------------------------------------------*/

//static void i_OnWindowHotkey(GtNapCallback *idp, Event *e)
//{
//    hb_gtnap_callback(idp, e);
//}
//
///*---------------------------------------------------------------------------*/
//
//HB_FUNC( NAP_WINDOW_HOTKEY )
//{
//    Window *window = hb_gtnap_parWindow(1);
//    vkey_t key = (vkey_t)hb_parni(2);
//    uint32_t modifiers = hb_parni(3);
//    Listener *listener = hb_gtnap_wind_listener(4, window, i_OnWindowHotkey);
//    window_hotkey(window, key, modifiers, listener);
//}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_WINDOW_UPDATE )
{
    Window *window = hb_gtnap_parWindow(1);
    window_update(window);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_WINDOW_ORIGIN )
{
    Window *window = hb_gtnap_parWindow(1);
    real32_t x = (real32_t)hb_parnd(2);
    real32_t y = (real32_t)hb_parnd(3);
    window_origin(window, v2df(x, y));
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_WINDOW_SIZE )
{
    Window *window = hb_gtnap_parWindow(1);
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
    Window *window = hb_gtnap_parWindow(1);
    Button *button = (Button*)hb_parptr(2);
    window_defbutton(window, button);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_WINDOW_FOCUS )
{
    Window *window = hb_gtnap_parWindow(1);
    Layout *layout = (Layout*)hb_parptr(2);
    uint32_t col = hb_parni(3);
    uint32_t row = hb_parni(4);
    Cell *cell = layout_cell(layout, col, row);
    unref(window);
    cell_focus(cell);
}
