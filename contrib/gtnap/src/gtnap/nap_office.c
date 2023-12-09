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

HB_FUNC( NAP_OFFICE_BROWSE_DOC )
{
    HB_ITEM *pathname_block = hb_param(1, HB_IT_STRING | HB_IT_BLOCK);
    hb_gtnap_office_browse_doc(pathname_block);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_XLS_OPEN )
{
    HB_ITEM *pathname_block = hb_param(1, HB_IT_STRING | HB_IT_BLOCK);
    Sheet *sheet = hb_gtnap_office_sheet_open(pathname_block);
    hb_retptr(sheet);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_XLS_SAVE )
{
    Sheet *sheet = (Sheet*)hb_parptr(1);
    HB_ITEM *pathname_block = hb_param(2, HB_IT_STRING | HB_IT_BLOCK);
    hb_gtnap_office_sheet_save(sheet, pathname_block);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_XLS_CLOSE )
{
    Sheet *sheet = (Sheet*)hb_parptr(1);
    hb_gtnap_office_sheet_close(sheet);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_XLS_CELL_TEXT )
{
    Sheet *sheet = (Sheet*)hb_parptr(1);
    uint32_t page = hb_parni(2);
    uint32_t col = hb_parni(3);
    uint32_t row = hb_parni(4);
    HB_ITEM *text_block = hb_param(5, HB_IT_STRING | HB_IT_BLOCK);
    hb_gtnap_office_sheet_cell_text(sheet, page, col, row, text_block);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_XLS_CELL_FONT_FAMILY )
{
    Sheet *sheet = (Sheet*)hb_parptr(1);
    uint32_t page = hb_parni(2);
    uint32_t col = hb_parni(3);
    uint32_t row = hb_parni(4);
    HB_ITEM *ffamily_block = hb_param(5, HB_IT_STRING | HB_IT_BLOCK);
    hb_gtnap_office_sheet_cell_font_family(sheet, page, col, row, ffamily_block);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_XLS_CELL_FONT_SIZE )
{
    Sheet *sheet = (Sheet*)hb_parptr(1);
    uint32_t page = hb_parni(2);
    uint32_t col = hb_parni(3);
    uint32_t row = hb_parni(4);
    real32_t fsize = (real32_t)hb_parnd(5);
    hb_gtnap_office_sheet_cell_font_size(sheet, page, col, row, fsize);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_XLS_CELL_BOLD )
{
    Sheet *sheet = (Sheet*)hb_parptr(1);
    uint32_t page = hb_parni(2);
    uint32_t col = hb_parni(3);
    uint32_t row = hb_parni(4);
    bool_t bold = (bool_t)hb_parl(5);
    hb_gtnap_office_sheet_cell_bold(sheet, page, col, row, bold);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_XLS_CELL_ITALIC )
{
    Sheet *sheet = (Sheet*)hb_parptr(1);
    uint32_t page = hb_parni(2);
    uint32_t col = hb_parni(3);
    uint32_t row = hb_parni(4);
    bool_t italic = (bool_t)hb_parl(5);
    hb_gtnap_office_sheet_cell_italic(sheet, page, col, row, italic);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_XLS_COLUMN_VISIBLE )
{
    Sheet *sheet = (Sheet*)hb_parptr(1);
    uint32_t page = hb_parni(2);
    uint32_t col = hb_parni(3);
    bool_t visible = (bool_t)hb_parl(4);
    hb_gtnap_office_sheet_column_visible(sheet, page, col, visible);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_XLS_COLUMN_OPTIMAL_WIDTH )
{
    Sheet *sheet = (Sheet*)hb_parptr(1);
    uint32_t page = hb_parni(2);
    uint32_t col = hb_parni(3);
    bool_t optimal_width = (bool_t)hb_parl(4);
    hb_gtnap_office_sheet_column_optimal_width(sheet, page, col, optimal_width);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_XLS_COLUMN_WIDTH )
{
    Sheet *sheet = (Sheet*)hb_parptr(1);
    uint32_t page = hb_parni(2);
    uint32_t col = hb_parni(3);
    uint32_t width = hb_parni(4);
    hb_gtnap_office_sheet_column_width(sheet, page, col, width);
}
