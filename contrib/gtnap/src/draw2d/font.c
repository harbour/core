/*
 * NAppGUI Cross-platform C SDK
 * 2015-2024 Francisco Garcia Collado
 * MIT Licence
 * https://nappgui.com/en/legal/license.html
 *
 * File: font.c
 *
 */

/* Fonts */

#include "font.h"
#include "font.inl"
#include "draw2d.inl"
#include <core/heap.h>
#include <core/strings.h>
#include <sewer/bmath.h>
#include <sewer/cassert.h>
#include <sewer/ptr.h>

struct _font_t
{
    uint32_t num_instances;
    uint32_t family;
    uint32_t style;
    real32_t size;
    real32_t width;
    real32_t cell_size;
    real32_t leading;
    real32_t ascent;
    real32_t descent;
    real32_t avg_width;
    real32_t x_scale;
    bool_t monospace;
    bool_t metrics;
    String *family_name;
    OSFont *osfont;
};

/*---------------------------------------------------------------------------*/

#define i_abs(x) (((x) < 0.f) ? -(x) : (x))

/*---------------------------------------------------------------------------*/

static Font *i_create_font(const uint32_t family, const real32_t size, const real32_t width, const uint32_t style)
{
    Font *font = heap_new(Font);
    font->num_instances = 1;
    font->family = family;
    font->size = size;
    font->width = width;
    font->style = style;
    font->cell_size = -1;
    font->leading = -1;
    font->ascent = -1;
    font->descent = -1;
    font->avg_width = -1;
    font->x_scale = -1;
    font->monospace = FALSE;
    font->metrics = FALSE;
    font->family_name = NULL;
    font->osfont = NULL;
    return font;
}

/*---------------------------------------------------------------------------*/

void font_destroy(Font **font)
{
    cassert_no_null(font);
    cassert_no_null(*font);
    if ((*font)->num_instances == 1)
    {
        if ((*font)->osfont != NULL)
            osfont_destroy(&(*font)->osfont);
        str_destopt(&(*font)->family_name);
        heap_delete(font, Font);
    }
    else
    {
        cassert((*font)->num_instances > 0);
        (*font)->num_instances -= 1;
    }
}

/*---------------------------------------------------------------------------*/

Font *font_create(const char_t *family, const real32_t size, const uint32_t style)
{
    uint32_t ffamily = draw2d_register_font(family);
    return i_create_font(ffamily, size, -1, style);
}

/*---------------------------------------------------------------------------*/

Font *font_system(const real32_t size, const uint32_t style)
{
    return i_create_font((uint32_t)ekFONT_FAMILY_SYSTEM, size, -1, style);
}

/*---------------------------------------------------------------------------*/

Font *font_monospace(const real32_t size, const uint32_t style)
{
    return i_create_font((uint32_t)ekFONT_FAMILY_MONOSPACE, size, -1, style);
}

/*---------------------------------------------------------------------------*/

Font *font_with_style(const Font *font, const uint32_t style)
{
    cassert_no_null(font);
    return i_create_font(font->family, font->size, font->width, style);
}

/*---------------------------------------------------------------------------*/

Font *font_with_width(const Font *font, const real32_t width)
{
    cassert_no_null(font);
    return i_create_font(font->family, font->size, width, font->style);
}

/*---------------------------------------------------------------------------*/

Font *font_copy(const Font *font)
{
    cassert_no_null(font);
    cast(font, Font)->num_instances += 1;
    return cast(font, Font);
}

/*---------------------------------------------------------------------------*/

bool_t font_equals(const Font *font1, const Font *font2)
{
    cassert_no_null(font1);
    cassert_no_null(font2);
    if (font1->family != font2->family)
        return FALSE;
    if (i_abs(font1->size - font2->size) > 0.0001f)
        return FALSE;
    if (i_abs(font1->width - font2->width) > 0.01f)
        return FALSE;
    if (font1->style != font2->style)
        return FALSE;
    return TRUE;
}

/*---------------------------------------------------------------------------*/

static ___INLINE void i_osfont(Font *font)
{
    cassert_no_null(font);
    if (font->osfont == NULL)
    {
        const char_t *fname = draw2d_font_family(font->family);
        font->osfont = osfont_create(fname, font->size, font->width, font->style);
    }
}

/*---------------------------------------------------------------------------*/

const char_t *font_family(const Font *font)
{
    cassert_no_null(font);
    if (font->family_name == NULL)
    {
        i_osfont(cast(font, Font));
        cast(font, Font)->family_name = osfont_family_name(font->osfont);
    }

    return tc(font->family_name);
}

/*---------------------------------------------------------------------------*/

real32_t font_size(const Font *font)
{
    cassert_no_null(font);
    return font->size;
}

/*---------------------------------------------------------------------------*/

static ___INLINE void i_metrics(Font *font)
{
    cassert_no_null(font);
    if (font->metrics == FALSE)
    {
        i_osfont(font);
        osfont_metrics(font->osfont, font->size, &font->ascent, &font->descent, &font->leading, &font->cell_size, &font->monospace);
        font->metrics = TRUE;
    }
}

/*---------------------------------------------------------------------------*/

real32_t font_height(const Font *font)
{
    cassert_no_null(font);
    i_metrics(cast(font, Font));
    return font->cell_size;
}

/*---------------------------------------------------------------------------*/

real32_t font_width(const Font *font)
{
    cassert_no_null(font);
    if (font->avg_width < 0)
    {
        uint32_t len = 0;
        const char_t *reftext = draw2d_str_avg_char_width(&len);
        real32_t w, h;
        font_extents(font, reftext, -1, &w, &h);
        cast(font, Font)->avg_width = w / len;
    }

    return font->avg_width;
}

/*---------------------------------------------------------------------------*/

real32_t font_xscale(const Font *font)
{
    cassert_no_null(font);
    /* Only compute scale for custom char widths */
    if (font->width > 0)
    {
        if (font->x_scale < 0)
        {
            Font *rfont = i_create_font(font->family, font->size, -1, font->style);
            real32_t w1 = font_width(font);
            real32_t w2 = font_width(rfont);
            font_destroy(&rfont);
            cast(font, Font)->x_scale = w1 / w2;
        }
        
        return font->x_scale;
    }
    else
    {
        return 1.f;
    }
}

/*---------------------------------------------------------------------------*/

real32_t font_ascent(const Font *font)
{
    cassert_no_null(font);
    i_metrics(cast(font, Font));
    return font->ascent;
}

/*---------------------------------------------------------------------------*/

real32_t font_descent(const Font *font)
{
    cassert_no_null(font);
    i_metrics(cast(font, Font));
    return font->descent;
}

/*---------------------------------------------------------------------------*/

real32_t font_leading(const Font *font)
{
    cassert_no_null(font);
    i_metrics(cast(font, Font));
    return font->leading;
}

/*---------------------------------------------------------------------------*/

bool_t font_is_monospace(const Font *font)
{
    cassert_no_null(font);
    i_metrics(cast(font, Font));
    return font->monospace;
}

/*---------------------------------------------------------------------------*/

uint32_t font_style(const Font *font)
{
    cassert_no_null(font);
    return font->style;
}

/*---------------------------------------------------------------------------*/

void font_extents(const Font *font, const char_t *text, const real32_t refwidth, real32_t *width, real32_t *height)
{
    cassert_no_null(font);
    i_osfont(cast(font, Font));
    osfont_extents(font->osfont, text, refwidth, width, height);
}

/*---------------------------------------------------------------------------*/

void font_preferred_monospace(const char_t *family)
{
    draw2d_preferred_monospace(family);
}

/*---------------------------------------------------------------------------*/

const void *font_native(const Font *font)
{
    cassert_no_null(font);
    i_osfont(cast(font, Font));
    return font->osfont;
}
