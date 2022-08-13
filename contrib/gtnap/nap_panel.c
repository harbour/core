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

HB_FUNC( NAP_PANEL_LAYOUT )
{
    Panel *panel = (Panel*)hb_parptr(1);
    Layout *layout = (Layout*)hb_parptr(2);
    uint32_t ret = panel_layout(panel, layout);
    hb_retni(ret);
}

