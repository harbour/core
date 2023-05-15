/*
    This is part of gtnap
*/

#include "gtnap.h"
#include "gtnap.inl"
#include "nappgui.h"

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_INIT )
{
    const char_t *title = hb_parcx(1);
    uint32_t rows = hb_parni(2);
    uint32_t cols = hb_parni(3);
    PHB_ITEM begin_block = hb_param(4, HB_IT_BLOCK);
    hb_gtnap_init(title, rows, cols, begin_block);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_TERMINAL )
{
    hb_gtnap_terminal();
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_EXIT )
{
    osapp_finish();
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_LOG )
{
    String *text = hb_gtnap_parstr(1);
    log_printf("%s", tc(text));
    str_destroy(&text);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_CRASH )
{
    cassert_no_null(NULL);
}