/*
    This is part of gtnap
*/

#include "hbgtnap.h"
#include "nappgui.h"

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_GLOBALPANEL )
{
    Panel *panel = (Panel*)hb_parptr(1);
    hb_gt_nap_set_GlobalPanel(panel);
    hb_retl(TRUE);
    return;
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_GLOBAL_FONT )
{
    real32_t size = (real32_t)hb_parnd(1);
    uint32_t style = hb_parni(2);
    Font *font = font_system(size, style);
    hb_gtnap_set_global_font(font);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_GLOBAL_RUNLOOP )
{
    hb_gt_nap_runloop();
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_GLOBAL_EXIT )
{
    osapp_finish();
}