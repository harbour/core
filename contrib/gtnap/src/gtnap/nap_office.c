/*
    This is part of gtnap
*/

#include "gtnap.h"

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_OFFICE_TEXT_TO_PDF )
{
    HB_ITEM *src_block = hb_param(1, HB_IT_BLOCK);
    HB_ITEM *dest_block = hb_param(2, HB_IT_BLOCK);
    uint32_t id = hb_gtnap_office_text_to_pdf(src_block, dest_block);
    hb_retni(id);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_OFFICE_ERROR )
{
    uint32_t errcode = hb_parni(1);
    const char_t *err = hb_gtnap_office_error(errcode);
    hb_retc(err);
}

