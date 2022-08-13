/*
    This is part of gtnap
*/

#include "hbgtnap.h"
#include "nappgui.h"

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_LABELCREATE )
{
    Label *label = label_create();
    hb_retptr(label);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_LABELTEXT )
{
    Label *label = (Label*)hb_parptr(1);
    label_text(label, hb_get_nap_text(2));
}

/*---------------------------------------------------------------------------*/

