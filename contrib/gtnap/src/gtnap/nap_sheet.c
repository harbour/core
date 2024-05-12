/*
    This is part of gtnap
*/

#include "gtnap.h"

/*---------------------------------------------------------------------------*/

HB_FUNC(NAP_XLS_OPEN)
{
    HB_ITEM *pathname_block = hb_param(1, HB_IT_STRING | HB_IT_BLOCK);
    Sheet *sheet = hb_gtnap_office_sheet_open(pathname_block);
    hb_retptr(sheet);
}

/*---------------------------------------------------------------------------*/

HB_FUNC(NAP_XLS_CREATE)
{
    Sheet *sheet = hb_gtnap_office_sheet_create();
    hb_retptr(sheet);
}

/*---------------------------------------------------------------------------*/

HB_FUNC(NAP_XLS_SAVE)
{
    Sheet *sheet = (Sheet *)hb_parptr(1);
    HB_ITEM *pathname_block = hb_param(2, HB_IT_STRING | HB_IT_BLOCK);
    hb_gtnap_office_sheet_save(sheet, pathname_block);
}

/*---------------------------------------------------------------------------*/

HB_FUNC(NAP_XLS_PDF)
{
    Sheet *sheet = (Sheet *)hb_parptr(1);
    HB_ITEM *pathname_block = hb_param(2, HB_IT_STRING | HB_IT_BLOCK);
    hb_gtnap_office_sheet_pdf(sheet, pathname_block);
}

/*---------------------------------------------------------------------------*/

HB_FUNC(NAP_XLS_PRINT)
{
    Sheet *sheet = (Sheet *)hb_parptr(1);
    HB_ITEM *filename_block = hb_param(2, HB_IT_STRING | HB_IT_BLOCK);
    HB_ITEM *printer_block = hb_param(3, HB_IT_STRING | HB_IT_BLOCK);
    paperorient_t orient = (paperorient_t)hb_parni(4);
    paperformat_t format = (paperformat_t)hb_parni(5);
    uint32_t paper_width = hb_parni(6);
    uint32_t paper_height = hb_parni(7);
    uint32_t num_copies = hb_parni(8);
    bool_t collate_copies = (bool_t)hb_parl(9);
    HB_ITEM *pages_block = hb_param(10, HB_IT_STRING | HB_IT_BLOCK);
    hb_gtnap_office_sheet_print(sheet, filename_block, printer_block, orient, format, paper_width, paper_height, num_copies, collate_copies, pages_block);
}

/*---------------------------------------------------------------------------*/

HB_FUNC(NAP_XLS_CLOSE)
{
    Sheet *sheet = (Sheet *)hb_parptr(1);
    hb_gtnap_office_sheet_close(sheet);
}

/*---------------------------------------------------------------------------*/

HB_FUNC(NAP_XLS_ADD)
{
    Sheet *sheet = (Sheet *)hb_parptr(1);
    uint32_t id = hb_gtnap_office_sheet_add(sheet);
    hb_retni(id);
}

/*---------------------------------------------------------------------------*/

HB_FUNC(NAP_XLS_NAME)
{
    Sheet *sheet = (Sheet *)hb_parptr(1);
    uint32_t page = hb_parni(2);
    HB_ITEM *name_block = hb_param(3, HB_IT_STRING | HB_IT_BLOCK);
    hb_gtnap_office_sheet_name(sheet, page, name_block);
}

/*---------------------------------------------------------------------------*/

HB_FUNC(NAP_XLS_PROTECT)
{
    Sheet *sheet = (Sheet *)hb_parptr(1);
    uint32_t page = hb_parni(2);
    bool_t protect = (bool_t)hb_parl(3);
    HB_ITEM *pass_block = hb_param(4, HB_IT_STRING | HB_IT_BLOCK);
    hb_gtnap_office_sheet_protect(sheet, page, protect, pass_block);
}

/*---------------------------------------------------------------------------*/

HB_FUNC(NAP_XLS_FREEZE)
{
    Sheet *sheet = (Sheet *)hb_parptr(1);
    uint32_t page = hb_parni(2);
    uint32_t ncols = hb_parni(3);
    uint32_t nrows = hb_parni(4);
    hb_gtnap_office_sheet_freeze(sheet, page, ncols, nrows);
}

/*---------------------------------------------------------------------------*/

HB_FUNC(NAP_CELL_REF)
{
    Sheet *sheet = (Sheet *)hb_parptr(1);
    uint32_t page = hb_parni(2);
    uint32_t col = hb_parni(3);
    uint32_t row = hb_parni(4);
    const char_t *ref = hb_gtnap_office_cell_ref(sheet, page, col, row);
    hb_retc(ref);
}

/*---------------------------------------------------------------------------*/

HB_FUNC(NAP_XLS_CELL_TEXT)
{
    Sheet *sheet = (Sheet *)hb_parptr(1);
    uint32_t page = hb_parni(2);
    uint32_t col = hb_parni(3);
    uint32_t row = hb_parni(4);
    HB_ITEM *text_block = hb_param(5, HB_IT_STRING | HB_IT_BLOCK);
    hb_gtnap_office_sheet_cell_text(sheet, page, col, row, text_block);
}

/*---------------------------------------------------------------------------*/

HB_FUNC(NAP_XLS_CELL_VALUE)
{
    Sheet *sheet = (Sheet *)hb_parptr(1);
    uint32_t page = hb_parni(2);
    uint32_t col = hb_parni(3);
    uint32_t row = hb_parni(4);
    real64_t value = hb_parnd(5);
    hb_gtnap_office_sheet_cell_value(sheet, page, col, row, value);
}

/*---------------------------------------------------------------------------*/

HB_FUNC(NAP_XLS_CELL_DATE)
{
    Sheet *sheet = (Sheet *)hb_parptr(1);
    uint32_t page = hb_parni(2);
    uint32_t col = hb_parni(3);
    uint32_t row = hb_parni(4);
    uint8_t day = (uint8_t)hb_parni(5);
    uint8_t month = (uint8_t)hb_parni(6);
    int16_t year = (int16_t)hb_parni(7);
    hb_gtnap_office_sheet_cell_date(sheet, page, col, row, day, month, year);
}

/*---------------------------------------------------------------------------*/

HB_FUNC(NAP_XLS_CELL_FORMULA)
{
    Sheet *sheet = (Sheet *)hb_parptr(1);
    uint32_t page = hb_parni(2);
    uint32_t col = hb_parni(3);
    uint32_t row = hb_parni(4);
    HB_ITEM *formula_block = hb_param(5, HB_IT_STRING | HB_IT_BLOCK);
    hb_gtnap_office_sheet_cell_formula(sheet, page, col, row, formula_block);
}

/*---------------------------------------------------------------------------*/

HB_FUNC(NAP_XLS_CELL_NUMFORMAT)
{
    Sheet *sheet = (Sheet *)hb_parptr(1);
    uint32_t page = hb_parni(2);
    uint32_t col = hb_parni(3);
    uint32_t row = hb_parni(4);
    numformat_t format = (numformat_t)hb_parni(5);
    hb_gtnap_office_sheet_cell_numformat(sheet, page, col, row, format);
}

/*---------------------------------------------------------------------------*/

HB_FUNC(NAP_XLS_CELL_FONT_FAMILY)
{
    Sheet *sheet = (Sheet *)hb_parptr(1);
    uint32_t page = hb_parni(2);
    uint32_t col = hb_parni(3);
    uint32_t row = hb_parni(4);
    HB_ITEM *ffamily_block = hb_param(5, HB_IT_STRING | HB_IT_BLOCK);
    hb_gtnap_office_sheet_cell_font_family(sheet, page, col, row, ffamily_block);
}

/*---------------------------------------------------------------------------*/

HB_FUNC(NAP_XLS_CELL_FONT_SIZE)
{
    Sheet *sheet = (Sheet *)hb_parptr(1);
    uint32_t page = hb_parni(2);
    uint32_t col = hb_parni(3);
    uint32_t row = hb_parni(4);
    real32_t fsize = (real32_t)hb_parnd(5);
    hb_gtnap_office_sheet_cell_font_size(sheet, page, col, row, fsize);
}

/*---------------------------------------------------------------------------*/

HB_FUNC(NAP_XLS_CELL_BOLD)
{
    Sheet *sheet = (Sheet *)hb_parptr(1);
    uint32_t page = hb_parni(2);
    uint32_t col = hb_parni(3);
    uint32_t row = hb_parni(4);
    bool_t bold = (bool_t)hb_parl(5);
    hb_gtnap_office_sheet_cell_bold(sheet, page, col, row, bold);
}

/*---------------------------------------------------------------------------*/

HB_FUNC(NAP_XLS_CELL_ITALIC)
{
    Sheet *sheet = (Sheet *)hb_parptr(1);
    uint32_t page = hb_parni(2);
    uint32_t col = hb_parni(3);
    uint32_t row = hb_parni(4);
    bool_t italic = (bool_t)hb_parl(5);
    hb_gtnap_office_sheet_cell_italic(sheet, page, col, row, italic);
}

/*---------------------------------------------------------------------------*/

HB_FUNC(NAP_XLS_CELL_HALIGN)
{
    Sheet *sheet = (Sheet *)hb_parptr(1);
    uint32_t page = hb_parni(2);
    uint32_t col = hb_parni(3);
    uint32_t row = hb_parni(4);
    halign_t align = (halign_t)hb_parni(5);
    hb_gtnap_office_sheet_cell_halign(sheet, page, col, row, align);
}

/*---------------------------------------------------------------------------*/

HB_FUNC(NAP_XLS_CELL_VALIGN)
{
    Sheet *sheet = (Sheet *)hb_parptr(1);
    uint32_t page = hb_parni(2);
    uint32_t col = hb_parni(3);
    uint32_t row = hb_parni(4);
    valign_t align = (valign_t)hb_parni(5);
    hb_gtnap_office_sheet_cell_valign(sheet, page, col, row, align);
}

/*---------------------------------------------------------------------------*/

HB_FUNC(NAP_XLS_CELL_WRAP)
{
    Sheet *sheet = (Sheet *)hb_parptr(1);
    uint32_t page = hb_parni(2);
    uint32_t col = hb_parni(3);
    uint32_t row = hb_parni(4);
    bool_t wrapped = (bool_t)hb_parl(5);
    hb_gtnap_office_sheet_cell_wrap(sheet, page, col, row, wrapped);
}

/*---------------------------------------------------------------------------*/

HB_FUNC(NAP_XLS_CELL_COLOR)
{
    Sheet *sheet = (Sheet *)hb_parptr(1);
    uint32_t page = hb_parni(2);
    uint32_t col = hb_parni(3);
    uint32_t row = hb_parni(4);
    uint32_t rgb = hb_parni(5);
    hb_gtnap_office_sheet_cell_color(sheet, page, col, row, rgb);
}

/*---------------------------------------------------------------------------*/

HB_FUNC(NAP_XLS_CELL_BACKCOLOR)
{
    Sheet *sheet = (Sheet *)hb_parptr(1);
    uint32_t page = hb_parni(2);
    uint32_t col = hb_parni(3);
    uint32_t row = hb_parni(4);
    uint32_t rgb = hb_parni(5);
    hb_gtnap_office_sheet_cell_backcolor(sheet, page, col, row, rgb);
}

/*---------------------------------------------------------------------------*/

HB_FUNC(NAP_XLS_CELLS_BACKCOLOR)
{
    Sheet *sheet = (Sheet *)hb_parptr(1);
    uint32_t page = hb_parni(2);
    uint32_t st_col = hb_parni(3);
    uint32_t st_row = hb_parni(4);
    uint32_t ed_col = hb_parni(5);
    uint32_t ed_row = hb_parni(6);
    uint32_t rgb = hb_parni(7);
    hb_gtnap_office_sheet_cells_backcolor(sheet, page, st_col, st_row, ed_col, ed_row, rgb);
}

/*---------------------------------------------------------------------------*/

HB_FUNC(NAP_XLS_CELL_IMAGE)
{
    Sheet *sheet = (Sheet *)hb_parptr(1);
    uint32_t page = hb_parni(2);
    uint32_t col = hb_parni(3);
    uint32_t row = hb_parni(4);
    HB_ITEM *image_path_block = hb_param(5, HB_IT_STRING | HB_IT_BLOCK);
    hb_gtnap_office_sheet_cell_image(sheet, page, col, row, image_path_block);
}

/*---------------------------------------------------------------------------*/

HB_FUNC(NAP_XLS_CELL_BORDER)
{
    Sheet *sheet = (Sheet *)hb_parptr(1);
    uint32_t page = hb_parni(2);
    uint32_t col = hb_parni(3);
    uint32_t row = hb_parni(4);
    linestyle_t style = (linestyle_t)hb_parni(5);
    uint32_t thickness = hb_parni(6);
    uint32_t rgb = hb_parni(7);
    hb_gtnap_office_sheet_cell_border(sheet, page, col, row, style, thickness, rgb);
}

/*---------------------------------------------------------------------------*/

HB_FUNC(NAP_XLS_CELLS_BORDER)
{
    Sheet *sheet = (Sheet *)hb_parptr(1);
    uint32_t page = hb_parni(2);
    uint32_t st_col = hb_parni(3);
    uint32_t st_row = hb_parni(4);
    uint32_t ed_col = hb_parni(5);
    uint32_t ed_row = hb_parni(6);
    linestyle_t style = (linestyle_t)hb_parni(7);
    uint32_t thickness = hb_parni(8);
    uint32_t rgb = hb_parni(9);
    hb_gtnap_office_sheet_cells_border(sheet, page, st_col, st_row, ed_col, ed_row, style, thickness, rgb);
}

/*---------------------------------------------------------------------------*/

HB_FUNC(NAP_XLS_CELLS_MERGE)
{
    Sheet *sheet = (Sheet *)hb_parptr(1);
    uint32_t page = hb_parni(2);
    uint32_t st_col = hb_parni(3);
    uint32_t st_row = hb_parni(4);
    uint32_t ed_col = hb_parni(5);
    uint32_t ed_row = hb_parni(6);
    hb_gtnap_office_sheet_cells_merge(sheet, page, st_col, st_row, ed_col, ed_row);
}

/*---------------------------------------------------------------------------*/

HB_FUNC(NAP_XLS_COLUMN_VISIBLE)
{
    Sheet *sheet = (Sheet *)hb_parptr(1);
    uint32_t page = hb_parni(2);
    uint32_t col = hb_parni(3);
    bool_t visible = (bool_t)hb_parl(4);
    hb_gtnap_office_sheet_column_visible(sheet, page, col, visible);
}

/*---------------------------------------------------------------------------*/

HB_FUNC(NAP_XLS_COLUMN_OPTIMAL_WIDTH)
{
    Sheet *sheet = (Sheet *)hb_parptr(1);
    uint32_t page = hb_parni(2);
    uint32_t col = hb_parni(3);
    bool_t optimal_width = (bool_t)hb_parl(4);
    hb_gtnap_office_sheet_column_optimal_width(sheet, page, col, optimal_width);
}

/*---------------------------------------------------------------------------*/

HB_FUNC(NAP_XLS_COLUMN_WIDTH)
{
    Sheet *sheet = (Sheet *)hb_parptr(1);
    uint32_t page = hb_parni(2);
    uint32_t col = hb_parni(3);
    uint32_t width = hb_parni(4);
    hb_gtnap_office_sheet_column_width(sheet, page, col, width);
}

/*---------------------------------------------------------------------------*/

HB_FUNC(NAP_XLS_ROW_VISIBLE)
{
    Sheet *sheet = (Sheet *)hb_parptr(1);
    uint32_t page = hb_parni(2);
    uint32_t row = hb_parni(3);
    bool_t visible = (bool_t)hb_parl(4);
    hb_gtnap_office_sheet_row_visible(sheet, page, row, visible);
}

/*---------------------------------------------------------------------------*/

HB_FUNC(NAP_XLS_ROW_OPTIMAL_HEIGHT)
{
    Sheet *sheet = (Sheet *)hb_parptr(1);
    uint32_t page = hb_parni(2);
    uint32_t row = hb_parni(3);
    bool_t optimal_height = (bool_t)hb_parl(4);
    hb_gtnap_office_sheet_row_optimal_height(sheet, page, row, optimal_height);
}

/*---------------------------------------------------------------------------*/

HB_FUNC(NAP_XLS_ROW_HEIGHT)
{
    Sheet *sheet = (Sheet *)hb_parptr(1);
    uint32_t page = hb_parni(2);
    uint32_t row = hb_parni(3);
    uint32_t height = hb_parni(4);
    hb_gtnap_office_sheet_row_height(sheet, page, row, height);
}
