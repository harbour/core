/*
    This is part of gtnap
*/

#include "gtnap.h"
#include "nappgui.h"

/*---------------------------------------------------------------------------*/

HB_FUNC(NAP_COLOR_RGB)
{
    uint8_t r = (uint8_t)hb_parni(1);
    uint8_t g = (uint8_t)hb_parni(2);
    uint8_t b = (uint8_t)hb_parni(3);
    color_t c = color_rgb(r, g, b);
    hb_retni(c);
}

/*---------------------------------------------------------------------------*/

HB_FUNC(NAP_COLOR_RGBA)
{
    uint8_t r = (uint8_t)hb_parni(1);
    uint8_t g = (uint8_t)hb_parni(2);
    uint8_t b = (uint8_t)hb_parni(3);
    uint8_t a = (uint8_t)hb_parni(4);
    color_t c = color_rgba(r, g, b, a);
    hb_retni(c);
}

/*---------------------------------------------------------------------------*/

HB_FUNC(NAP_COLOR_BLACK)
{
    color_t c = hb_gtnap_color_black();
    hb_retni(c);
}

/*---------------------------------------------------------------------------*/

HB_FUNC(NAP_COLOR_BLUE)
{
    color_t c = hb_gtnap_color_blue();
    hb_retni(c);
}

/*---------------------------------------------------------------------------*/

HB_FUNC(NAP_COLOR_GREEN)
{
    color_t c = hb_gtnap_color_green();
    hb_retni(c);
}

/*---------------------------------------------------------------------------*/

HB_FUNC(NAP_COLOR_CYAN)
{
    color_t c = hb_gtnap_color_cyan();
    hb_retni(c);
}

/*---------------------------------------------------------------------------*/

HB_FUNC(NAP_COLOR_RED)
{
    color_t c = hb_gtnap_color_red();
    hb_retni(c);
}

/*---------------------------------------------------------------------------*/

HB_FUNC(NAP_COLOR_MAGENTA)
{
    color_t c = hb_gtnap_color_magenta();
    hb_retni(c);
}

/*---------------------------------------------------------------------------*/

HB_FUNC(NAP_COLOR_BROWN)
{
    color_t c = hb_gtnap_color_brown();
    hb_retni(c);
}

/*---------------------------------------------------------------------------*/

HB_FUNC(NAP_COLOR_WHITE)
{
    color_t c = hb_gtnap_color_white();
    hb_retni(c);
}

/*---------------------------------------------------------------------------*/

HB_FUNC(NAP_COLOR_LIGHT_GRAY)
{
    color_t c = hb_gtnap_color_light_gray();
    hb_retni(c);
}

/*---------------------------------------------------------------------------*/

HB_FUNC(NAP_COLOR_BRIGHT_BLUE)
{
    color_t c = hb_gtnap_color_bright_blue();
    hb_retni(c);
}

/*---------------------------------------------------------------------------*/

HB_FUNC(NAP_COLOR_BRIGHT_GREEN)
{
    color_t c = hb_gtnap_color_bright_green();
    hb_retni(c);
}

/*---------------------------------------------------------------------------*/

HB_FUNC(NAP_COLOR_BRIGHT_CYAN)
{
    color_t c = hb_gtnap_color_bright_cyan();
    hb_retni(c);
}

/*---------------------------------------------------------------------------*/

HB_FUNC(NAP_COLOR_BRIGHT_RED)
{
    color_t c = hb_gtnap_color_bright_red();
    hb_retni(c);
}

/*---------------------------------------------------------------------------*/

HB_FUNC(NAP_COLOR_BRIGHT_MAGENTA)
{
    color_t c = hb_gtnap_color_bright_magenta();
    hb_retni(c);
}

/*---------------------------------------------------------------------------*/

HB_FUNC(NAP_COLOR_YELLOW)
{
    color_t c = hb_gtnap_color_yellow();
    hb_retni(c);
}

/*---------------------------------------------------------------------------*/

HB_FUNC(NAP_COLOR_BRIGHT_WHITE)
{
    color_t c = hb_gtnap_color_bright_white();
    hb_retni(c);
}
