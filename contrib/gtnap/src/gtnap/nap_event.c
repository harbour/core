/*
    This is part of gtnap
    TODO: More info
*/

// NAppGUI-Event wrapper for Harbour
// https://nappgui.com/en/core/event.html
#include "gtnap.h"
#include "nappgui.h"

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_EVENT_TYPE )
{
    Event *ev = (Event*)hb_parptr(1);
    uint32_t type = event_type(ev);
    hb_retni((int)type);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_EVENT_BUTTON )
{
    Event *ev = (Event*)hb_parptr(1);
    Button *button = event_sender(ev, Button);
    hb_retptr(button);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_EVENT_EDIT )
{
    Event *ev = (Event*)hb_parptr(1);
    Edit *edit = event_sender(ev, Edit);
    hb_retptr(edit);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_EVENT_RESULT_FALSE )
{
    Event *ev = (Event*)hb_parptr(1);
    bool_t *ret = event_result(ev, bool_t);
    *ret = FALSE;
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_EVENT_WINCLOSE_ESC )
{
    Event *ev = (Event*)hb_parptr(1);
    const EvWinClose *p = event_params(ev, EvWinClose);
    hb_retl(p->origin == ekGUI_CLOSE_ESC);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_EVENT_WINCLOSE_RETURN )
{
    Event *ev = (Event*)hb_parptr(1);
    const EvWinClose *p = event_params(ev, EvWinClose);
    hb_retl(p->origin == ekGUI_CLOSE_INTRO);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_EVENT_WINCLOSE_BUTTON )
{
    Event *ev = (Event*)hb_parptr(1);
    const EvWinClose *p = event_params(ev, EvWinClose);
    hb_retl(p->origin == ekGUI_CLOSE_BUTTON);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_EVENT_TEXT )
{
    Event *ev = (Event*)hb_parptr(1);
    const EvText *p = event_params(ev, EvText);
    hb_retc(p->text);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_EVENT_TEXT_FILTER )
{
    Event *ev = (Event*)hb_parptr(1);
    const char_t *text = hb_gtnap_parText(2);
    EvTextFilter *r = event_result(ev, EvTextFilter);
    str_copy_c(r->text, sizeof(r->text), text);
    r->apply = TRUE;
}
