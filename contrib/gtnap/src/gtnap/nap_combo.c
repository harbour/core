/*
    This is part of gtnap
*/

#include "gtnap.h"
#include "nappgui.h"

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_COMBO_CREATE )
{
    Combo *combo = combo_create();
    hb_retptr(combo);
}

/*---------------------------------------------------------------------------*/

//static void i_OnComboFilter(GtNapCallback *idp, Event *e)
//{
//    hb_gtnap_callback(idp, e);
//}

/*---------------------------------------------------------------------------*/

//HB_FUNC( NAP_COMBO_ONFILTER )
//{
//    Combo *combo = (Combo*)hb_parptr(1);
//    Listener *listener = hb_gtnap_comp_listener(2, (GuiComponent*)combo, i_OnComboFilter);
//    combo_OnFilter(combo, listener);
//}

/*---------------------------------------------------------------------------*/

//static void i_OnComboChange(GtNapCallback *idp, Event *e)
//{
//    hb_gtnap_callback(idp, e);
//}

/*---------------------------------------------------------------------------*/

//HB_FUNC( NAP_COMBO_ONCHANGE )
//{
//    Combo *combo = (Combo*)hb_parptr(1);
//    Listener *listener = hb_gtnap_comp_listener(2, (GuiComponent*)combo, i_OnComboChange);
//    combo_OnChange(combo, listener);
//}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_COMBO_TEXT )
{
    Combo *combo = (Combo*)hb_parptr(1);
    const char_t *text = hb_gtnap_parText(2);
    combo_text(combo, text);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_COMBO_ALIGN )
{
    Combo *combo = (Combo*)hb_parptr(1);
    align_t align = (align_t)hb_parni(2);
    combo_align(combo, align);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_COMBO_TOOLTIP )
{
    Combo *combo = (Combo*)hb_parptr(1);
    const char_t *text = hb_gtnap_parText(2);
    combo_tooltip(combo, text);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_COMBO_COLOR )
{
    Combo *combo = (Combo*)hb_parptr(1);
    color_t color = (color_t)hb_parni(2);
    combo_color(combo, color);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_COMBO_COLOR_FOCUS )
{
    Combo *combo = (Combo*)hb_parptr(1);
    color_t color = (color_t)hb_parni(2);
    combo_color_focus(combo, color);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_COMBO_BGCOLOR )
{
    Combo *combo = (Combo*)hb_parptr(1);
    color_t color = (color_t)hb_parni(2);
    combo_bgcolor(combo, color);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_COMBO_BGCOLOR_FOCUS )
{
    Combo *combo = (Combo*)hb_parptr(1);
    color_t color = (color_t)hb_parni(2);
    combo_bgcolor_focus(combo, color);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_COMBO_PHTEXT )
{
    Combo *combo = (Combo*)hb_parptr(1);
    const char_t *text = hb_gtnap_parText(2);
    combo_phtext(combo, text);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_COMBO_PHCOLOR )
{
    Combo *combo = (Combo*)hb_parptr(1);
    color_t color = (color_t)hb_parni(2);
    combo_phcolor(combo, color);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_COMBO_PHSTYLE )
{
    Combo *combo = (Combo*)hb_parptr(1);
    uint32_t fstyle = hb_parni(2);
    combo_phstyle(combo, fstyle);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_COMBO_GET_TEXT )
{
    Combo *combo = (Combo*)hb_parptr(1);
    uint32_t index = hb_parni(2);
    const char_t *text = combo_get_text(combo, index);
    hb_retc(text);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_COMBO_COUNT )
{
    Combo *combo = (Combo*)hb_parptr(1);
    uint32_t count = combo_count(combo);
    hb_retni(count);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_COMBO_ADD_ELEM )
{
    Combo *combo = (Combo*)hb_parptr(1);
    const char_t *text = hb_gtnap_parText(2);
    //Image *image = hb_gtnap_parImage(3);
    combo_add_elem(combo, text, NULL);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_COMBO_SET_ELEM )
{
    Combo *combo = (Combo*)hb_parptr(1);
    uint32_t index = hb_parni(2);
    const char_t *text = hb_gtnap_parText(3);
    //Image *image = hb_gtnap_parImage(4);
    combo_set_elem(combo, index, text, NULL);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_COMBO_INS_ELEM )
{
    Combo *combo = (Combo*)hb_parptr(1);
    uint32_t index = hb_parni(2);
    const char_t *text = hb_gtnap_parText(3);
    //Image *image = hb_gtnap_parImage(4);
    combo_ins_elem(combo, index, text, NULL);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_COMBO_DEL_ELEM )
{
    Combo *combo = (Combo*)hb_parptr(1);
    uint32_t index = hb_parni(2);
    combo_del_elem(combo, index);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_COMBO_DUPLICATES )
{
    Combo *combo = (Combo*)hb_parptr(1);
    bool_t duplicates = (bool_t)hb_parl(2);
    combo_duplicates(combo, duplicates);
}

