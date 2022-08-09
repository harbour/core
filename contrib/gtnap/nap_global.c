/*
    This is part of gtnap
*/

#include "hbgtnap.h"
#include "nappgui.h"

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_GLOBALPANEL )
{
    Panel *panel = (Panel*)HB_PARHANDLE(1);
    hb_gt_nap_set_GlobalPanel(panel);
    hb_retl(TRUE);
    return;
}
