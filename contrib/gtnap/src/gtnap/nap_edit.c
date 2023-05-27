/*
    This is part of gtnap
*/

#include "gtnap.h"
#include "gtnap.inl"

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_EDIT )
{
    uint32_t wid = hb_parni(1);
    int32_t top = hb_parni(2);
    int32_t left = hb_parni(3);
    uint32_t width = hb_parni(4);
    const char_t *type = hb_parcx(5);
    HB_ITEM *get_set_block = hb_param(6, HB_IT_BLOCK);
    HB_ITEM *is_editable_block = hb_param(7, HB_IT_BLOCK);
    HB_ITEM *when_block = hb_param(8, HB_IT_BLOCK);
    HB_ITEM *valida_block = hb_param(9, HB_IT_BLOCK);
    HB_ITEM *message_block = hb_param(10, HB_IT_BLOCK);
    HB_ITEM *keyfilter_block = hb_param(11, HB_IT_BLOCK);
    HB_ITEM *auto_block = hb_param(12, HB_IT_BLOCK);
    HB_ITEM *lista_block = hb_param(13, HB_IT_BLOCK);
    bool_t in_scroll = (bool_t)hb_parl(14);
    uint32_t id = hb_gtnap_edit(wid, top, left, width, type ? type[0] : 'X', get_set_block, is_editable_block, when_block, valida_block, message_block, keyfilter_block, auto_block, lista_block, in_scroll);
    hb_retni(id);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_EDIT_WIZARD )
{
    uint32_t wid = hb_parni(1);
    uint32_t id = hb_parni(2);
    uint32_t bid = hb_parni(3);
    int32_t key = hb_parni(4);
    HB_ITEM *auto_block = hb_param(5, HB_IT_BLOCK);
    HB_ITEM *wizard_block = hb_param(6, HB_IT_BLOCK);
    hb_gtnap_edit_wizard(wid, id, bid, key, auto_block, wizard_block);
}
