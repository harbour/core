/*
 * LibreOffice Harbour Wrapper
 * 2024 - Francisco Garcia Collado
 */

#include "hboffice.h"

/*---------------------------------------------------------------------------*/

HB_FUNC(HBOFFICE_INIT)
{
    uint32_t ok = hb_office_init();
    hb_retl(ok);
}

/*---------------------------------------------------------------------------*/

HB_FUNC(HBOFFICE_FINISH)
{
    hb_office_finish();
}

/*---------------------------------------------------------------------------*/

HB_FUNC(HBOFFICE_LAST_ERROR)
{
    uint32_t errcode = hb_office_last_error();
    hb_retni(errcode);
}

/*---------------------------------------------------------------------------*/

HB_FUNC(HBOFFICE_ERROR_STR)
{
    uint32_t errcode = hb_parni(1);
    const char_t *err = hb_office_error_str(errcode);
    hb_retc(err);
}

/*---------------------------------------------------------------------------*/

HB_FUNC(HBOFFICE_BROWSE_DOC)
{
    HB_ITEM *pathname_block = hb_param(1, HB_IT_STRING | HB_IT_BLOCK);
    hb_office_browse_doc(pathname_block);
}

/*---------------------------------------------------------------------------*/

HB_FUNC(HBOFFICE_RGB)
{
    uint8_t red = (uint8_t)hb_parni(1);
    uint8_t green = (uint8_t)hb_parni(2);
    uint8_t blue = (uint8_t)hb_parni(3);
    uint32_t rgb = hb_office_rgb(red, green, blue);
    hb_retni(rgb);
}
