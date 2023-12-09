/*
    This is part of gtnap
*/

#include "gtnap.h"

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_OFFICE_TEXT_TO_PDF )
{
    HB_ITEM *src_block = hb_param(1, HB_IT_STRING | HB_IT_BLOCK);
    HB_ITEM *dest_block = hb_param(2, HB_IT_STRING | HB_IT_BLOCK);
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

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_OFFICE_LAST_ERROR )
{
    uint32_t errcode = hb_gtnap_office_last_error();
    hb_retni(errcode);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_XLS_OPEN )
{
    HB_ITEM *pathname_block = hb_param(1, HB_IT_STRING | HB_IT_BLOCK);
    SheetDoc *doc = hb_gtnap_office_sheetdoc_open(pathname_block);
    hb_retptr(doc);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_XLS_SAVE )
{
    SheetDoc *doc = (SheetDoc*)hb_parptr(1);
    HB_ITEM *pathname_block = hb_param(2, HB_IT_STRING | HB_IT_BLOCK);
    hb_gtnap_office_sheetdoc_save(doc, pathname_block);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_XLS_CLOSE )
{
    SheetDoc *doc = (SheetDoc*)hb_parptr(1);
    hb_gtnap_office_sheetdoc_close(doc);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_XLS_CELL_TEXT )
{
    SheetDoc *doc = (SheetDoc*)hb_parptr(1);
    uint32_t sheet_id = hb_parni(2);
    uint32_t col = hb_parni(3);
    uint32_t row = hb_parni(4);
    HB_ITEM *text_block = hb_param(5, HB_IT_STRING | HB_IT_BLOCK);
    hb_gtnap_office_sheetdoc_cell_text(doc, sheet_id, col, row, text_block);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_XLS_CELL_FORMAT )
{
    SheetDoc *doc = (SheetDoc*)hb_parptr(1);
    uint32_t sheet_id = hb_parni(2);
    uint32_t col = hb_parni(3);
    uint32_t row = hb_parni(4);
    HB_ITEM *ffamily_block = hb_param(5, HB_IT_STRING | HB_IT_BLOCK);
    real32_t fsize = (real32_t)hb_parnd(6);
    bool_t bold = (bool_t)hb_parl(7);
    bool_t italic = (bool_t)hb_parl(8);
    hb_gtnap_office_sheetdoc_cell_format(doc, sheet_id, col, row, ffamily_block, fsize, bold, italic);
}


/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_XLS_COLUMN_FORMAT )
{
    SheetDoc *doc = (SheetDoc*)hb_parptr(1);
    uint32_t sheet_id = hb_parni(2);
    uint32_t col = hb_parni(3);
    bool_t visible = (bool_t)hb_parl(4);
    bool_t optimal_width = (bool_t)hb_parl(5);
    uint32_t width = hb_parni(6);
    hb_gtnap_office_sheetdoc_column_format(doc, sheet_id, col, visible, optimal_width, width);
}

/*---------------------------------------------------------------------------*/
