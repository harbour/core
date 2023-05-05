/*
    This is part of gtnap
*/

#include "gtnap.h"
#include "nappgui.h"

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_GLOBAL_EXIT )
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