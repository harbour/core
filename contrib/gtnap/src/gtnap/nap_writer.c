/*
    This is part of gtnap
*/

#include "gtnap.h"

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_DOC_OPEN )
{
    HB_ITEM *pathname_block = hb_param(1, HB_IT_STRING | HB_IT_BLOCK);
    Writer *writer = hb_gtnap_office_writer_open(pathname_block);
    hb_retptr(writer);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_DOC_CREATE )
{
    Writer *writer = hb_gtnap_office_writer_create();
    hb_retptr(writer);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_DOC_SAVE )
{
    Writer *writer = (Writer*)hb_parptr(1);
    HB_ITEM *pathname_block = hb_param(2, HB_IT_STRING | HB_IT_BLOCK);
    hb_gtnap_office_writer_save(writer, pathname_block);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_DOC_PDF )
{
    Writer *writer = (Writer*)hb_parptr(1);
    HB_ITEM *pathname_block = hb_param(2, HB_IT_STRING | HB_IT_BLOCK);
    hb_gtnap_office_writer_pdf(writer, pathname_block);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_DOC_PRINT )
{
    Writer *writer = (Writer*)hb_parptr(1);
    HB_ITEM *filename_block = hb_param(2, HB_IT_STRING | HB_IT_BLOCK);
    HB_ITEM *printer_block = hb_param(3, HB_IT_STRING | HB_IT_BLOCK);
    paperorient_t orient = (paperorient_t)hb_parni(4);
    paperformat_t format = (paperformat_t)hb_parni(5);
    uint32_t paper_width = hb_parni(6);
    uint32_t paper_height = hb_parni(7);
    uint32_t num_copies = hb_parni(8);
    bool_t collate_copies = (bool_t)hb_parl(9);
    HB_ITEM *pages_block = hb_param(10, HB_IT_STRING | HB_IT_BLOCK);
    hb_gtnap_office_writer_print(writer, filename_block, printer_block, orient, format, paper_width, paper_height, num_copies, collate_copies, pages_block);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_DOC_CLOSE )
{
    Writer *writer = (Writer*)hb_parptr(1);
    hb_gtnap_office_writer_close(writer);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_DOC_PAGE_HEADER_SHOW )
{
    Writer *writer = (Writer*)hb_parptr(1);
    bool_t show = (bool_t)hb_parl(2);
    hb_gtnap_office_writer_page_header_show(writer, show);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_DOC_PAGE_HEADER_MARGINS )
{
    Writer *writer = (Writer*)hb_parptr(1);
    uint32_t left = hb_parni(2);
    uint32_t right = hb_parni(3);
    uint32_t spacing = hb_parni(4);
    uint32_t height = hb_parni(5);
    bool_t dynamic_spacing = (bool_t)hb_parl(6);
    bool_t dynamic_height = (bool_t)hb_parl(7);
    hb_gtnap_office_writer_page_header_margins(writer, left, right, spacing, height, dynamic_spacing, dynamic_height);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_DOC_PAGE_FOOTER_SHOW )
{
    Writer *writer = (Writer*)hb_parptr(1);
    bool_t show = (bool_t)hb_parl(2);
    hb_gtnap_office_writer_page_footer_show(writer, show);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_DOC_PAGE_FOOTER_MARGINS )
{
    Writer *writer = (Writer*)hb_parptr(1);
    uint32_t left = hb_parni(2);
    uint32_t right = hb_parni(3);
    uint32_t spacing = hb_parni(4);
    uint32_t height = hb_parni(5);
    bool_t dynamic_spacing = (bool_t)hb_parl(6);
    bool_t dynamic_height = (bool_t)hb_parl(7);
    hb_gtnap_office_writer_page_footer_margins(writer, left, right, spacing, height, dynamic_spacing, dynamic_height);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_DOC_PAGE_MARGINS )
{
    Writer *writer = (Writer*)hb_parptr(1);
    uint32_t left = hb_parni(2);
    uint32_t right = hb_parni(3);
    uint32_t top = hb_parni(4);
    uint32_t bottom = hb_parni(5);
    uint32_t gutter = hb_parni(6);
    hb_gtnap_office_writer_page_margins(writer, left, right, top, bottom, gutter);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_DOC_TEXT_SPACE )
{
    Writer *writer = (Writer*)hb_parptr(1);
    textspace_t space = (textspace_t)hb_parni(2);
    hb_gtnap_office_writer_text_space(writer, space);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_DOC_FONT_FAMILY )
{
    Writer *writer = (Writer*)hb_parptr(1);
    HB_ITEM *ffamily_block = hb_param(2, HB_IT_STRING | HB_IT_BLOCK);
    hb_gtnap_office_writer_font_family(writer, ffamily_block);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_DOC_FONT_SIZE )
{
    Writer *writer = (Writer*)hb_parptr(1);
    real32_t fsize = (real32_t)hb_parnd(2);
    hb_gtnap_office_writer_font_size(writer, fsize);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_DOC_BOLD )
{
    Writer *writer = (Writer*)hb_parptr(1);
    bool_t bold = (bool_t)hb_parl(2);
    hb_gtnap_office_writer_bold(writer, bold);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_DOC_ITALIC )
{
    Writer *writer = (Writer*)hb_parptr(1);
    bool_t italic = (bool_t)hb_parl(2);
    hb_gtnap_office_writer_italic(writer, italic);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_DOC_PARAGRAPH_HALIGN )
{
    Writer *writer = (Writer*)hb_parptr(1);
    halign_t align = (halign_t)hb_parni(2);
    hb_gtnap_office_writer_paragraph_halign(writer, align);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_DOC_PARAGRAPH_LSPACING )
{
    Writer *writer = (Writer*)hb_parptr(1);
    uint32_t height = hb_parni(2);
    hb_gtnap_office_writer_paragraph_lspacing(writer, height);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_DOC_INSERT_TEXT )
{
    Writer *writer = (Writer*)hb_parptr(1);
    HB_ITEM *text_block = hb_param(2, HB_IT_STRING | HB_IT_BLOCK);
    hb_gtnap_office_writer_insert_text(writer, text_block);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_DOC_INSERT_DASH )
{
    Writer *writer = (Writer*)hb_parptr(1);
    uint32_t n = hb_parni(2);
    if (n == 0)
        n = 1;
    hb_gtnap_office_writer_insert_dash(writer, n);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_DOC_INSERT_IMAGE )
{
    Writer *writer = (Writer*)hb_parptr(1);
    anchortype_t anchor = (anchortype_t)hb_parni(2);
    uint32_t width = hb_parni(3);
    uint32_t height = hb_parni(4);
    halign_t halign = (halign_t)hb_parni(5);
    valign_t valign = (valign_t)hb_parni(6);
    HB_ITEM *image_path_block = hb_param(7, HB_IT_STRING | HB_IT_BLOCK);
    hb_gtnap_office_writer_insert_image(writer, anchor, width, height, halign, valign, image_path_block);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_DOC_INSERT_PAGE_NUMBER )
{
    Writer *writer = (Writer*)hb_parptr(1);
    hb_gtnap_office_writer_insert_page_number(writer);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_DOC_INSERT_NEW_LINE )
{
    Writer *writer = (Writer*)hb_parptr(1);
    hb_gtnap_office_writer_insert_new_line(writer);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_DOC_INSERT_PARAGRAPH )
{
    Writer *writer = (Writer*)hb_parptr(1);
    hb_gtnap_office_writer_insert_paragraph(writer);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_DOC_INSERT_PAGE_BREAK )
{
    Writer *writer = (Writer*)hb_parptr(1);
    hb_gtnap_office_writer_insert_page_break(writer);
}
