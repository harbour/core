/*
    This is part of gtnap
*/

#include "gtnap.h"
#include "nappgui.h"

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_GLOBAL_FONT )
{
    real32_t size = (real32_t)hb_parnd(1);
    uint32_t style = hb_parni(2);
    Font *font = font_system(size, style);
    hb_gtnap_set_global_font(font);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_GLOBAL_EXIT )
{
    osapp_finish();
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_LOG )
{
    String *text = hb_gtnap_cualib_parText(1);
    log_printf("%s", tc(text));
    str_destroy(&text);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_CRASH )
{
    cassert_no_null(NULL);
}