/*
    This is part of gtnap
*/

#include "hbgtnap.h"
#include "nappgui.h"

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_LABEL_CREATE )
{
    Label *label = label_create();
    hb_retptr(label);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_LABEL_WITH_TEXT )
{
    Label *label = label_create();
    const char_t *text = hb_get_nap_text(1);
    label_text(label, text);
    label_font(label, hb_gt_global_font());
    hb_retptr(label);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_LABEL_TEXT )
{
    Label *label = (Label*)hb_parptr(1);
    label_text(label, hb_get_nap_text(2));
}

/*---------------------------------------------------------------------------*/

