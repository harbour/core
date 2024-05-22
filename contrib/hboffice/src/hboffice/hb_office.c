/*
    This is part of gtnap
*/

#include "gtnap.h"

/*---------------------------------------------------------------------------*/

HB_FUNC(NAP_OFFICE_TEXT_TO_PDF)
{
    HB_ITEM *src_block = hb_param(1, HB_IT_STRING | HB_IT_BLOCK);
    HB_ITEM *dest_block = hb_param(2, HB_IT_STRING | HB_IT_BLOCK);
    uint32_t id = hb_gtnap_office_text_to_pdf(src_block, dest_block);
    hb_retni(id);
}

/*---------------------------------------------------------------------------*/

HB_FUNC(NAP_OFFICE_ERROR_STR)
{
    uint32_t errcode = hb_parni(1);
    const char_t *err = hb_gtnap_office_error(errcode);
    hb_retc(err);
}

/*---------------------------------------------------------------------------*/

HB_FUNC(NAP_OFFICE_LAST_ERROR)
{
    uint32_t errcode = hb_gtnap_office_last_error();
    hb_retni(errcode);
}

/*---------------------------------------------------------------------------*/

HB_FUNC(NAP_OFFICE_BROWSE_DOC)
{
    HB_ITEM *pathname_block = hb_param(1, HB_IT_STRING | HB_IT_BLOCK);
    hb_gtnap_office_browse_doc(pathname_block);
}

/*---------------------------------------------------------------------------*/

HB_FUNC(NAP_OFFICE_RGB)
{
    uint8_t red = (uint8_t)hb_parni(1);
    uint8_t green = (uint8_t)hb_parni(2);
    uint8_t blue = (uint8_t)hb_parni(3);
    uint32_t rgb = hb_gtnap_office_rgb(red, green, blue);
    hb_retni(rgb);
}
