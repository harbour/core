/*
    This is part of gtnap
*/

#include "hbgtnap.h"
#include "nappgui.h"

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_LABEL_CREATE )
{
    Label *label = label_create();
    label_font(label, hb_gtnap_global_font());
    hb_retptr(label);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_LABEL_WITH_TEXT )
{
    Label *label = label_create();
    const char_t *text = hb_get_nap_text(1);
    label_text(label, text);
    label_font(label, hb_gtnap_global_font());
    hb_retptr(label);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_LABEL_MULTILINE )
{
    Label *label = label_multiline();
    label_font(label, hb_gtnap_global_font());
    hb_retptr(label);
}

/*---------------------------------------------------------------------------*/

static void i_OnLabelClick(GtNapCallback *idp, Event *e)
{
    hb_gt_nap_callback(idp, e);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_LABEL_ONCLICK )
{
    Label *label = (Label*)hb_parptr(1);
    Listener *listener = hb_gt_nap_listener(2, i_OnLabelClick);
    label_OnClick(label, listener);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_LABEL_TEXT )
{
    Label *label = (Label*)hb_parptr(1);
    const char_t *text = hb_get_nap_text(2);
    label_text(label, text);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_LABEL_FONT )
{
    Label *label = (Label*)hb_parptr(1);
    Font *font = hb_parFont(2);
    label_font(label, font);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_LABEL_STYLE_OVER )
{
    Label *label = (Label*)hb_parptr(1);
    uint32_t fstyle = hb_parni(2);
    label_style_over(label, fstyle);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_LABEL_ALIGN )
{
    Label *label = (Label*)hb_parptr(1);
    align_t align = (align_t)hb_parni(2);
    label_align(label, align);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_LABEL_COLOR )
{
    Label *label = (Label*)hb_parptr(1);
    color_t color = (color_t)hb_parni(2);
    label_color(label, color);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_LABEL_COLOR_OVER )
{
    Label *label = (Label*)hb_parptr(1);
    color_t color = (color_t)hb_parni(2);
    label_color_over(label, color);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_LABEL_BGCOLOR )
{
    Label *label = (Label*)hb_parptr(1);
    color_t color = (color_t)hb_parni(2);
    label_bgcolor(label, color);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_LABEL_BGCOLOR_OVER )
{
    Label *label = (Label*)hb_parptr(1);
    color_t color = (color_t)hb_parni(2);
    label_bgcolor_over(label, color);
}
