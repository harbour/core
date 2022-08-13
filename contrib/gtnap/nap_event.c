/*
    This is part of gtnap
    TODO: More info
*/

// NAppGUI-Event wrapper for Harbour
// https://nappgui.com/en/core/event.html
#include "hbgtnap.h"
#include "nappgui.h"

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_EVENT_TYPE )
{
    Event *ev = (Event*)hb_parptr(1);
    uint32_t type = event_type(ev);
    hb_retni((int)type);
}
