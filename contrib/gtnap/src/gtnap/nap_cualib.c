/*
    This is part of gtnap
*/

#include "gtnap.h"
#include "gtnap.inl"
#include "nappgui.h"

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_CUALIB_INIT_LOG )
{
    hb_gtnap_cualib_init_log();
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_CUALIB_WINDOW_F4_LISTA )
{
    hb_gtnap_cualib_window_f4_lista();
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_CUALIB_WINDOW_CURRENT_EDIT )
{
    uint32_t id = hb_gtnap_cualib_window_current_edit();
    hb_retni(id + 1);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_CUALIB_DEFAULT_BUTTON )
{
    uint32_t nDef = hb_parni(1);
    hb_gtnap_cualib_default_button(nDef);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_CUALIB_TOOLBAR )
{
    uint32_t nPixelsImage = hb_parni(1);
    hb_gtnap_cualib_toolbar(nPixelsImage);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_CUALIB_TOOLBAR_BUTTON )
{
    String *pathname = str_c(hb_gtnap_parText(1));
    String *tooltip = str_c(hb_gtnap_parText(2));
    hb_gtnap_cualib_toolbar_button(tc(pathname), tc(tooltip));
    str_destroy(&pathname);
    str_destroy(&tooltip);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_CUALIB_TOOLBAR_SEPARATOR )
{
    hb_gtnap_cualib_toolbar_separator();
}
