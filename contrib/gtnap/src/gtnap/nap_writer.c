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

HB_FUNC( NAP_DOC_INSERT )
{
    Writer *writer = (Writer*)hb_parptr(1);
    HB_ITEM *text_block = hb_param(2, HB_IT_STRING | HB_IT_BLOCK);
    hb_gtnap_office_writer_insert(writer, text_block);
}
