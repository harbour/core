/*
    This is part of gtnap
*/

#include "hbgtnap.h"
#include "nappgui.h"

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_LABELCREATE )
{
    Label *label = label_create();
    HB_RETHANDLE(label);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_LABELTEXT )
{
    Label *label = (Label*)HB_PARHANDLE(1);
    const char_t *text = NULL;

    if (HB_ISCHAR(2))
        text = hb_parcx(2);
    else
        text = (const char_t*)hb_parni(2);

    label_text(label, text);
    hb_retl(TRUE);
    return;
}

/*---------------------------------------------------------------------------*/

