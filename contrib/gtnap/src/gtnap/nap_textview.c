/*
    This is part of gtnap
*/

#include "gtnap.h"
#include "gtnap.inl"

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
    HB_ITEM *keyfilter_block = hb_param(8, HB_IT_BLOCK);
    bool_t in_scroll = (bool_t)hb_parl(9);
    uint32_t id = hb_gtnap_textview(wid, top, left, bottom, right, get_set_block, valida_block, keyfilter_block, in_scroll);
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
