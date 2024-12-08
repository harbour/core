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

HB_FUNC(NAP_FORM_DBIND)
{
    GtNapForm *form = cast(hb_parptr(1), GtNapForm);
    HB_ITEM *bind_block = hb_param(2, HB_IT_ARRAY);
    hb_gtnap_form_dbind(form, bind_block);
}

/*---------------------------------------------------------------------------*/

HB_FUNC(NAP_FORM_ONCLICK)
{
    GtNapForm *form = cast(hb_parptr(1), GtNapForm);
    const char_t *button_cell_name = hb_parcx(2);
    HB_ITEM *click_block = hb_param(3, HB_IT_BLOCK);
    hb_gtnap_form_OnClick(form, button_cell_name, click_block);
}

/*---------------------------------------------------------------------------*/

HB_FUNC(NAP_FORM_MODAL)
{
    GtNapForm *form = cast(hb_parptr(1), GtNapForm);
    uint32_t ret = hb_gtnap_form_modal(form);
    hb_retni(ret);
}

/*---------------------------------------------------------------------------*/

HB_FUNC(NAP_FORM_STOP_MODAL)
{
    GtNapForm *form = cast(hb_parptr(1), GtNapForm);
    uint32_t value = hb_parni(2);
    hb_gtnap_form_stop_modal(form, value);
}

/*---------------------------------------------------------------------------*/

HB_FUNC(NAP_FORM_DESTROY)
{
    GtNapForm *form = cast(hb_parptr(1), GtNapForm);
    hb_gtnap_form_destroy(&form);
}

