/*
    This is part of gtnap
*/

#include "hbgtnap.h"
#include "nappgui.h"

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_BUTTONPUSH )
{
    Button *button = button_push();
    HB_RETHANDLE(button);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_BUTTONTEXT )
{
    Button *button = (Button*)HB_PARHANDLE(1);
    const char_t *text = NULL;

    if (HB_ISCHAR(2))
        text = hb_parcx(2);
    else
        text = (const char_t*)hb_parni(2);

    button_text(button, text);
    hb_retl(TRUE);
    return;
}

/*---------------------------------------------------------------------------*/

