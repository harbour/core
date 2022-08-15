/*
    This is part of gtnap
*/

#include "hbgtnap.h"
#include "nappgui.h"

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_COLOR_RGB )
{
    uint8_t r = (uint8_t)hb_parni(1);
    uint8_t g = (uint8_t)hb_parni(2);
    uint8_t b = (uint8_t)hb_parni(3);
    color_t c = color_rgb(r, g, b);
    hb_retni(c);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_COLOR_RGBA )
{
    uint8_t r = (uint8_t)hb_parni(1);
    uint8_t g = (uint8_t)hb_parni(2);
    uint8_t b = (uint8_t)hb_parni(3);
    uint8_t a = (uint8_t)hb_parni(4);
    color_t c = color_rgba(r, g, b, a);
    hb_retni(c);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_COLOR_TRANSPARENT )
{
    hb_retni(kCOLOR_TRANSPARENT);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_COLOR_DEFAULT )
{
    hb_retni(kCOLOR_DEFAULT);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_COLOR_BLACK )
{
    hb_retni(kCOLOR_BLACK);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_COLOR_WHITE )
{
    hb_retni(kCOLOR_WHITE);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_COLOR_RED )
{
    hb_retni(kCOLOR_RED);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_COLOR_GREEN )
{
    hb_retni(kCOLOR_GREEN);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_COLOR_BLUE )
{
    hb_retni(kCOLOR_BLUE);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_COLOR_YELLOW )
{
    hb_retni(kCOLOR_YELLOW);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_COLOR_CYAN )
{
    hb_retni(kCOLOR_CYAN);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_COLOR_MAGENTA )
{
    hb_retni(kCOLOR_MAGENTA);
}
