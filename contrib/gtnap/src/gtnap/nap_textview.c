/*
    This is part of gtnap
*/

#include "gtnap.h"
#include "nappgui.h"

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_TEXTVIEW_CREATE )
{
    TextView *view = textview_create();
    hb_retptr(view);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_TEXTVIEW_SCROLL )
{
    TextView *view = (TextView*)hb_parptr(1);
    bool_t hor = (bool_t)hb_parl(2);
    bool_t ver = (bool_t)hb_parl(3);
    textview_scroll_visible(view, hor, ver);
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
