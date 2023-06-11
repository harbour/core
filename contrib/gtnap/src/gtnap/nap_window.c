/*
    This is part of gtnap
*/

#include "gtnap.h"
#include "gtnap.inl"

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

HB_FUNC( NAP_WINDOW_EMBEDDED )
{
    uint32_t wid = hb_parni(1);
    int32_t top = hb_parni(2);
    int32_t left = hb_parni(3);
    int32_t bottom = hb_parni(4);
    int32_t right = hb_parni(5);
    bool_t border = (bool_t)hb_parl(6);
    uint32_t id = hb_gtnap_window_embedded(wid, top, left, bottom, right, border);
    hb_retni(id);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_WINDOW_DESTROY )
{
    uint32_t wid = hb_parni(1);
    hb_gtnap_window_destroy(wid);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_WINDOW_HOTKEY )
{
    uint32_t wid = hb_parni(1);
    int32_t key = hb_parni(2);
    HB_ITEM *block = hb_param(3, HB_IT_BLOCK);
    bool_t autoclose = (bool_t)hb_parl(4);
    hb_gtnap_window_hotkey(wid, key, block, autoclose);
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

HB_FUNC( NAP_WINDOW_COPY )
{
    uint32_t wid = hb_parni(1);
    hb_gtnap_window_copy(wid);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_WINDOW_PASTE )
{
    uint32_t wid = hb_parni(1);
    hb_gtnap_window_paste(wid);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_WINDOW_CUT )
{
    uint32_t wid = hb_parni(1);
    hb_gtnap_window_cut(wid);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_WINDOW_UNDO )
{
    uint32_t wid = hb_parni(1);
    hb_gtnap_window_undo(wid);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_WINDOW_MODAL )
{
    uint32_t wid = hb_parni(1);
    uint32_t delay_seconds = hb_parni(2);
    uint32_t ret = hb_gtnap_window_modal(wid, delay_seconds);
    hb_retni(ret);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_WINDOW_STOP_MODAL )
{
    uint32_t result = hb_parni(1);
    hb_gtnap_window_stop_modal(result);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_TOOLBAR )
{
    uint32_t wid = hb_parni(1);
    uint32_t image_pixels = hb_parni(2);
    hb_gtnap_toolbar(wid, image_pixels);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_TOOLBAR_BUTTON )
{
    uint32_t wid = hb_parni(1);
    const char_t *pathname = hb_parcx(2);
    const char_t *tooltip = hb_parcx(3);
    HB_ITEM *click_block = hb_param(4, HB_IT_BLOCK);
    hb_gtnap_toolbar_button(wid, pathname, tooltip, click_block);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_TOOLBAR_SEPARATOR )
{
    uint32_t wid = hb_parni(1);
    hb_gtnap_toolbar_separator(wid);
}
