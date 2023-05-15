/*
    This is part of gtnap
*/

#include "gtnap.h"
#include "gtnap.inl"
#include "nappgui.h"

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_IMAGE )
{
    uint32_t wid = hb_parni(1);
    int32_t top = hb_parni(2);
    int32_t left = hb_parni(3);
    int32_t bottom = hb_parni(4);
    int32_t right = hb_parni(5);
    const char_t *pathname = hb_parcx(6);
    HB_ITEM *click_block = hb_param(7, HB_IT_BLOCK);
    bool_t autoclose = (bool_t)hb_parl(8);
    bool_t in_scroll = (bool_t)hb_parl(9);
    uint32_t id = hb_gtnap_image(wid, top, left, bottom, right, pathname, click_block, autoclose, in_scroll);
    hb_retni(id);
}











/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_IMAGE_FROM_FILE )
{
    const char_t *pathname = hb_gtnap_parText(1);
    String *cpath = str_cpath("%s", pathname);
    Image *image = image_from_file(tc(cpath), NULL);
    str_destroy(&cpath);
    hb_gtnap_retImageGC(image);
}
