/*
    This is part of gtnap
    TODO: More info
*/

#include "gtnap.h"

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_BUTTON )
{
    uint32_t wid = hb_parni(1);
    int32_t top = hb_parni(2);
    int32_t left = hb_parni(3);
    int32_t bottom = hb_parni(4);
    int32_t right = hb_parni(5);
    HB_ITEM *text_block = hb_param(6, HB_IT_BLOCK);
    HB_ITEM *click_block = hb_param(7, HB_IT_BLOCK);
    bool_t autoclose = (bool_t)hb_parl(8);
    bool_t in_scroll = (bool_t)hb_parl(9);
    uint32_t id = hb_gtnap_button(wid, top, left, bottom, right, text_block, click_block, autoclose, in_scroll);
    hb_retni(id);
}
