/*
    This is part of gtnap
*/

#include "gtnap.h"
#include "nappgui.h"

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_LABEL )
{
    uint32_t wid = hb_parni(1);
    int32_t top = hb_parni(2);
    int32_t left = hb_parni(3);
    HB_ITEM *text_block = hb_param(4, HB_IT_BLOCK);
    bool_t in_scroll = (bool_t)hb_parl(5);
    uint32_t id = hb_gtnap_label(wid, top, left, text_block, in_scroll);
    hb_retni(id);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_LABEL_MESSAGE )
{
    uint32_t wid = hb_parni(1);
    int32_t top = hb_parni(2);
    int32_t left = hb_parni(3);
    bool_t in_scroll = (bool_t)hb_parl(4);
    uint32_t id = hb_gtnap_label_message(wid, top, left, in_scroll);
    hb_retni(id);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_LABEL_FGCOLOR )
{
    uint32_t wid = hb_parni(1);
    uint32_t id = hb_parni(2);
    color_t color = (color_t)hb_parni(3);
    hb_gtnap_label_fgcolor(wid, id, color);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_LABEL_BGCOLOR )
{
    uint32_t wid = hb_parni(1);
    uint32_t id = hb_parni(2);
    color_t color = (color_t)hb_parni(3);
    hb_gtnap_label_bgcolor(wid, id, color);
}









/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_LABEL_CREATE )
{
    Label *label = label_create();
    label_font(label, hb_gtnap_font());
    hb_retptr(label);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_LABEL_WITH_TEXT )
{
    Label *label = label_create();
    const char_t *text = hb_gtnap_parText(1);
    label_text(label, text);
    label_font(label, hb_gtnap_font());
    hb_retptr(label);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_LABEL_MULTILINE )
{
    Label *label = label_multiline();
    label_font(label, hb_gtnap_font());
    hb_retptr(label);
}

/*---------------------------------------------------------------------------*/

static void i_OnLabelClick(GtNapCallback *idp, Event *e)
{
    hb_gtnap_callback(idp, e);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_LABEL_ONCLICK )
{
    Label *label = (Label*)hb_parptr(1);
    Listener *listener = hb_gtnap_comp_listener(2, (GuiComponent*)label, i_OnLabelClick);
    label_OnClick(label, listener);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_LABEL_TEXT )
{
    Label *label = (Label*)hb_parptr(1);
    const char_t *text = hb_gtnap_parText(2);
    label_text(label, text);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_LABEL_FONT )
{
    Label *label = (Label*)hb_parptr(1);
    Font *font = hb_gtnap_parFont(2);
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

HB_FUNC( NAP_LABEL_COLOR_OVER )
{
    Label *label = (Label*)hb_parptr(1);
    color_t color = (color_t)hb_parni(2);
    label_color_over(label, color);
}


/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_LABEL_BGCOLOR_OVER )
{
    Label *label = (Label*)hb_parptr(1);
    color_t color = (color_t)hb_parni(2);
    label_bgcolor_over(label, color);
}
