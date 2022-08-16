/*
    This is part of gtnap
*/

#include "hbgtnap.h"
#include "nappgui.h"

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_PANEL_CREATE )
{
    Panel *panel = panel_create();
    hb_retptr(panel);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_PANEL_SCROLL )
{
    bool_t hscroll = (bool_t)hb_parl(1);
    bool_t vscroll = (bool_t)hb_parl(2);
    Panel *panel = panel_scroll(hscroll, vscroll);
    hb_retptr(panel);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_PANEL_SIZE )
{
    Panel *panel = (Panel*)hb_parptr(1);
    real32_t width = (real32_t)hb_parnd(2);
    real32_t height = (real32_t)hb_parnd(3);
    panel_size(panel, s2df(width, height));
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_PANEL_LAYOUT )
{
    Panel *panel = (Panel*)hb_parptr(1);
    Layout *layout = (Layout*)hb_parptr(2);
    uint32_t ret = panel_layout(panel, layout);
    hb_retni(ret);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_PANEL_GET_LAYOUT )
{
    Panel *panel = (Panel*)hb_parptr(1);
    uint32_t index = hb_parni(2);
    Layout *layout = panel_get_layout(panel, index);
    hb_retptr(layout);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_PANEL_VISIBLE_LAYOUT )
{
    Panel *panel = (Panel*)hb_parptr(1);
    uint32_t index = hb_parni(2);
    panel_visible_layout(panel, index);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_PANEL_UPDATE )
{
    Panel *panel = (Panel*)hb_parptr(1);
    panel_update(panel);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_PANEL_SCROLL_WIDTH )
{
    Panel *panel = (Panel*)hb_parptr(1);
    real32_t width = panel_scroll_width(panel);
    hb_retnd(width);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_PANEL_SCROLL_HEIGHT )
{
    Panel *panel = (Panel*)hb_parptr(1);
    real32_t height = panel_scroll_height(panel);
    hb_retnd(height);
}
