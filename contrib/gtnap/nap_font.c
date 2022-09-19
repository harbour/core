/*
    This is part of gtnap
*/

#include "gtnap.h"
#include "nappgui.h"

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_FONT_CREATE )
{
    const char_t *family = hb_get_nap_text(1);
    real32_t size = (real32_t)hb_parnd(2);
    uint32_t style = hb_parni(3);
    Font *font = font_create(family, size, style);
    hb_retFont(font);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_FONT_SYSTEM )
{
    real32_t size = (real32_t)hb_parnd(1);
    uint32_t style = hb_parni(2);
    Font *font = font_system(size, style);
    hb_retFont(font);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_FONT_MONOSPACE )
{
    real32_t size = (real32_t)hb_parnd(1);
    uint32_t style = hb_parni(2);
    Font *font = font_monospace(size, style);
    hb_retFont(font);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_FONT_WITH_STYLE )
{
    Font *cfont = hb_parFont(1);
    uint32_t style = hb_parni(2);
    Font *font = font_with_style(cfont, style);
    hb_retFont(font);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_FONT_EQUALS )
{
    Font *font1 = hb_parFont(1);
    Font *font2 = hb_parFont(2);
    bool_t equals = font_equals(font1, font2);
    hb_retl(equals);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_FONT_REGULAR_SIZE )
{
    real32_t size = font_regular_size();
    hb_retnd(size);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_FONT_SMALL_SIZE )
{
    real32_t size = font_small_size();
    hb_retnd(size);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_FONT_MINI_SIZE )
{
    real32_t size = font_mini_size();
    hb_retnd(size);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_FONT_FAMILY )
{
    Font *font = hb_parFont(1);
    const char_t *family = font_family(font);
    hb_retc(family);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_FONT_SIZE )
{
    Font *font = hb_parFont(1);
    real32_t size = font_size(font);
    hb_retnd(size);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_FONT_HEIGHT )
{
    Font *font = hb_parFont(1);
    real32_t height = font_height(font);
    hb_retnd(height);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_FONT_STYLE )
{
    Font *font = hb_parFont(1);
    uint32_t style = font_style(font);
    hb_retni(style);
}

/*---------------------------------------------------------------------------*/
