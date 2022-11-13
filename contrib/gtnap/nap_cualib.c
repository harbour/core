/*
    This is part of gtnap
*/

#include "gtnap.h"
#include "nappgui.h"

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_CUALIB_SETUP )
{
    const char_t *title = hb_gtnap_cualib_parText(1);
    uint32_t qt_lin = hb_parni(2);
    uint32_t qt_col = hb_parni(3);
    hb_gtnap_cualib_setup(title, qt_lin, qt_col);
}
