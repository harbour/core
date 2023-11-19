/*
    This is part of gtnap
*/

#include "gtnap.h"
#include "gtnap.inl"
#include "nappgui.h"

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_INIT )
{
    const char_t *title = hb_parcx(1);
    uint32_t rows = hb_parni(2);
    uint32_t cols = hb_parni(3);
    PHB_ITEM begin_block = hb_param(4, HB_IT_BLOCK);
    hb_gtnap_init(title, rows, cols, begin_block);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_WIDTH )
{
    uint32_t width = hb_gtnap_width();
    hb_retni(width);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_HEIGHT )
{
    uint32_t height = hb_gtnap_height();
    hb_retni(height);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_WORK_PATH )
{
    const char_t *path = hb_gtnap_working_path();
    hb_retc(path);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_TERMINAL )
{
    hb_gtnap_terminal();
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_EXIT )
{
    osapp_finish();
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_INKEY )
{
    vkey_t vkey = (vkey_t)hb_parni(1);
    int32_t key = hb_gtnap_inkey(vkey);
    hb_retni(key);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_LOG )
{
    const char_t *text = hb_parcx(1);
    hb_gtnap_log(text);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_CRASH )
{
    cassert_no_null(NULL);
}