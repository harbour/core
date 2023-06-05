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
    if (id == UINT32_MAX)
        id = 0;
    else
        id += 1;
    hb_retni(id);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_CUALIB_DEFAULT_BUTTON )
{
    uint32_t nDef = hb_parni(1);
    hb_gtnap_cualib_default_button(nDef);
}

