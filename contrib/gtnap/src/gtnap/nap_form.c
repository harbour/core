/*
    This is part of gtnap
*/

#include "gtnap.h"
#include "gtnap.inl"

/*---------------------------------------------------------------------------*/

HB_FUNC(NAP_FORM_LOAD)
{
    const char_t *pathname = hb_parcx(1);
    GtNapForm *form = hb_gtnap_form_load(pathname);
    hb_retptr(form);
}

/*---------------------------------------------------------------------------*/

HB_FUNC(NAP_FORM_TITLE)
{
    GtNapForm *form = cast(hb_parptr(1), GtNapForm);
    HB_ITEM *text_block = hb_param(2, HB_IT_BLOCK | HB_IT_STRING);
    hb_gtnap_form_title(form, text_block);
}

/*---------------------------------------------------------------------------*/

HB_FUNC(NAP_FORM_MODAL)
{
    GtNapForm *form = cast(hb_parptr(1), GtNapForm);
    hb_gtnap_form_modal(form);
}

/*---------------------------------------------------------------------------*/

HB_FUNC(NAP_FORM_DESTROY)
{
    GtNapForm *form = cast(hb_parptr(1), GtNapForm);
    hb_gtnap_form_destroy(&form);
}

