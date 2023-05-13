/*
    This is part of gtnap
*/

#include "gtnap.h"
#include "nappgui.h"

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_TEXTVIEW )
{
    uint32_t wid = hb_parni(1);
    int32_t top = hb_parni(2);
    int32_t left = hb_parni(3);
    int32_t bottom = hb_parni(4);
    int32_t right = hb_parni(5);
    HB_ITEM *get_set_block = hb_param(6, HB_IT_BLOCK);
    HB_ITEM *valida_block = hb_param(7, HB_IT_BLOCK);
    bool_t in_scroll = (bool_t)hb_parl(8);
    uint32_t id = hb_gtnap_textview(wid, top, left, bottom, right, get_set_block, valida_block, in_scroll);
    hb_retni(id);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_TEXTVIEW_SCROLL )
{
    uint32_t wid = hb_parni(1);
    uint32_t id = hb_parni(2);
    bool_t horizontal = (bool_t)hb_parl(3);
    bool_t vertical = (bool_t)hb_parl(4);
    hb_gtnap_textview_scroll(wid, id, horizontal, vertical);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_TEXTVIEW_CARET )
{
    uint32_t wid = hb_parni(1);
    uint32_t id = hb_parni(2);
    int64_t pos = hb_parni(3);
    hb_gtnap_textview_caret(wid, id, pos);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_TEXTVIEW_BUTTON )
{
    uint32_t wid = hb_parni(1);
    uint32_t id = hb_parni(2);
    uint32_t bid = hb_parni(3);
    hb_gtnap_textview_button(wid, id, bid);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_TEXTVIEW_HOTKEY )
{
    uint32_t wid = hb_parni(1);
    uint32_t id = hb_parni(2);
    int32_t key = hb_parni(3);
    hb_gtnap_textview_hotkey(wid, id, key);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_TEXTVIEW_CREATE )
{
    TextView *view = textview_create();
    hb_retptr(view);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_TEXTVIEW_SIZE )
{
    TextView *view = (TextView*)hb_parptr(1);
    real32_t width = (real32_t)hb_parnd(2);
    real32_t height = (real32_t)hb_parnd(3);
    textview_size(view, s2df(width, height));
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_TEXTVIEW_CLEAR )
{
    TextView *view = (TextView*)hb_parptr(1);
    textview_clear(view);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_TEXTVIEW_WRITE )
{
    TextView *view = (TextView*)hb_parptr(1);
    const char_t *text = hb_gtnap_parText(2);
    textview_writef(view, text);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_TEXTVIEW_UNITS )
{
    TextView *view = (TextView*)hb_parptr(1);
    uint32_t units = hb_parni(2);
    textview_units(view, units);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_TEXTVIEW_FAMILY )
{
    TextView *view = (TextView*)hb_parptr(1);
    const char_t *family = hb_gtnap_parText(2);
    textview_family(view, family);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_TEXTVIEW_FSIZE )
{
    TextView *view = (TextView*)hb_parptr(1);
    real32_t size = (real32_t)hb_parnd(2);
    textview_fsize(view, size);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_TEXTVIEW_FSTYLE )
{
    TextView *view = (TextView*)hb_parptr(1);
    uint32_t style = hb_parni(2);
    textview_fstyle(view, style);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_TEXTVIEW_COLOR )
{
    TextView *view = (TextView*)hb_parptr(1);
    color_t color = (color_t)hb_parni(2);
    textview_color(view, color);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_TEXTVIEW_BGCOLOR )
{
    TextView *view = (TextView*)hb_parptr(1);
    color_t color = (color_t)hb_parni(2);
    textview_bgcolor(view, color);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_TEXTVIEW_PGCOLOR )
{
    TextView *view = (TextView*)hb_parptr(1);
    color_t color = (color_t)hb_parni(2);
    textview_pgcolor(view, color);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_TEXTVIEW_HALIGN )
{
    TextView *view = (TextView*)hb_parptr(1);
    align_t align = (align_t)hb_parni(2);
    textview_halign(view, align);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_TEXTVIEW_LSPACING )
{
    TextView *view = (TextView*)hb_parptr(1);
    real32_t scale = (real32_t)hb_parnd(2);
    textview_lspacing(view, scale);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_TEXTVIEW_BFSPACE )
{
    TextView *view = (TextView*)hb_parptr(1);
    real32_t space = (real32_t)hb_parnd(2);
    textview_bfspace(view, space);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_TEXTVIEW_AFSPACE )
{
    TextView *view = (TextView*)hb_parptr(1);
    real32_t space = (real32_t)hb_parnd(2);
    textview_afspace(view, space);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_TEXTVIEW_EDITABLE )
{
    TextView *view = (TextView*)hb_parptr(1);
    bool_t is_editable = (bool_t)hb_parl(2);
    textview_editable(view, is_editable);
}

/*---------------------------------------------------------------------------*/
