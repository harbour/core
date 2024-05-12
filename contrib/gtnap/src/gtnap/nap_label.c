/*
    This is part of gtnap
*/

#include "gtnap.h"
#include "gtnap.inl"

/*---------------------------------------------------------------------------*/

HB_FUNC(NAP_LABEL)
{
    uint32_t wid = hb_parni(1);
    int32_t top = hb_parni(2);
    int32_t left = hb_parni(3);
    HB_ITEM *text_block = hb_param(4, HB_IT_BLOCK | HB_IT_STRING);
    bool_t in_scroll = (bool_t)hb_parl(5);
    uint32_t id = hb_gtnap_label(wid, top, left, text_block, in_scroll);
    hb_retni(id);
}

/*---------------------------------------------------------------------------*/

HB_FUNC(NAP_LABEL_MESSAGE)
{
    uint32_t wid = hb_parni(1);
    int32_t top = hb_parni(2);
    int32_t left = hb_parni(3);
    bool_t in_scroll = (bool_t)hb_parl(4);
    uint32_t id = hb_gtnap_label_message(wid, top, left, in_scroll);
    hb_retni(id);
}

/*---------------------------------------------------------------------------*/

HB_FUNC(NAP_LABEL_UPDATE)
{
    uint32_t wid = hb_parni(1);
    uint32_t id = hb_parni(2);
    int32_t top = hb_parni(3);
    int32_t left = hb_parni(4);
    HB_ITEM *text_block = hb_param(5, HB_IT_BLOCK | HB_IT_STRING);
    hb_gtnap_label_update(wid, id, top, left, text_block);
}

/*---------------------------------------------------------------------------*/

HB_FUNC(NAP_LABEL_FGCOLOR)
{
    uint32_t wid = hb_parni(1);
    uint32_t id = hb_parni(2);
    color_t color = (color_t)hb_parni(3);
    hb_gtnap_label_fgcolor(wid, id, color);
}

/*---------------------------------------------------------------------------*/

HB_FUNC(NAP_LABEL_BGCOLOR)
{
    uint32_t wid = hb_parni(1);
    uint32_t id = hb_parni(2);
    color_t color = (color_t)hb_parni(3);
    hb_gtnap_label_bgcolor(wid, id, color);
}

/*---------------------------------------------------------------------------*/

HB_FUNC(NAP_LABEL_COLOR)
{
    uint32_t wid = hb_parni(1);
    uint32_t id = hb_parni(2);
    const char_t *hb_color = hb_parcx(3);
    hb_gtnap_label_color(wid, id, hb_color);
}
