/*
    This is part of gtnap
*/

#include "hbgtnap.h"
#include "nappgui.h"

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_PANELCREATE )
{
    Panel *panel = panel_create();
    HB_RETHANDLE(panel);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_PANELLAYOUT )
{
    Panel *panel = (Panel*)HB_PARHANDLE(1);
    Layout *layout = (Layout*)HB_PARHANDLE(2);
    uint32_t ret = panel_layout(panel, layout);
    hb_retni(ret);
    return;
}

