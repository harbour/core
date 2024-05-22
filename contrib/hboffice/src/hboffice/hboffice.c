/*
 * LibreOffice Harbour Wrapper
 * 2024 - Francisco Garcia Collado
 */

#include "hboffice.h"
// #include "gtnap.inl"
// #include "gtnap.ch"
// #include "nap_menu.inl"
// #include "nappgui.h"
// #include <osapp/osmain.h>
// #include <gui/drawctrl.inl>
// #include <officesdk/officesdk.h>
// #include <officesdk/sheetsdk.h>
// #include <officesdk/writersdk.h>

// #include "hbapiitm.h"
// #include "hbapirdd.h"
// #include "hbapistr.h"
// #include "hbdate.h"
// #include "hbset.h"

typedef struct _hboffice_t HBOffice;

struct _hboffice_t
{
    textspace_t text_space;
    String *last_cell_ref;
    sdkres_t last_error;
};

/*---------------------------------------------------------------------------*/

static HBOffice *HBOFFICE_GLOBAL = NULL;

#define STATIC_TEXT_SIZE 1024
char_t TEMP_BUFFER[STATIC_TEXT_SIZE];

/*---------------------------------------------------------------------------*/

static void i_hboffice_destroy(HBOffice **office)
{
    cassert_no_null(office);
    cassert_no_null(*office);
    cassert(*office == HBOFFICE_GLOBAL);
    str_destopt(&(*office)->last_cell_ref);
    officesdk_finish();
    heap_delete(&(*office), HBOffice);
}

/*---------------------------------------------------------------------------*/

static HBOffice *i_hboffice_create(void)
{
    HBOFFICE_GLOBAL = heap_new0(HBOffice);
    HBOFFICE_GLOBAL->text_space = ekTEXT_SPACE_PAGE;
    HBOFFICE_GLOBAL->last_cell_ref = NULL;
    HBOFFICE_GLOBAL->last_error = ENUM_MAX(sdkres_t);
    return HBOFFICE_GLOBAL;
}

/*---------------------------------------------------------------------------*/

void hb_office_init(void)
{
}

/*---------------------------------------------------------------------------*/

void hb_office_finish(void)
{
}

/*---------------------------------------------------------------------------*/

uint32_t hb_office_text_to_pdf(HB_ITEM *src_block, HB_ITEM *dest_block)
{
    String *src = hb_block_to_utf8(src_block);
    String *dest = hb_block_to_utf8(dest_block);
    sdkres_t res = officesdk_text_to_pdf(tc(src), tc(dest));
    str_destroy(&src);
    str_destroy(&dest);
    return (uint32_t)res;
}

/*---------------------------------------------------------------------------*/

uint32_t hb_office_last_error(void)
{
    return (uint32_t)HBOFFICE_GLOBAL->last_error;
}

/*---------------------------------------------------------------------------*/

const char_t *hb_office_error(const uint32_t errcode)
{
    return officesdk_error((sdkres_t)errcode);
}

/*---------------------------------------------------------------------------*/

void hb_office_browse_doc(HB_ITEM *pathname_block)
{
    String *pathname = hb_block_to_utf8(pathname_block);
    officesdk_browse_doc(tc(pathname), &GTNAP_GLOBAL->office_last_error);
    str_destroy(&pathname);
}

/*---------------------------------------------------------------------------*/

uint32_t hb_office_rgb(const uint8_t red, const uint8_t green, const uint8_t blue)
{
    return officesdk_rgb(red, green, blue);
}

/*---------------------------------------------------------------------------*/

Sheet *hb_office_sheet_open(HB_ITEM *pathname_block)
{
    String *pathname = hb_block_to_utf8(pathname_block);
    Sheet *sheet = officesdk_sheet_open(tc(pathname), &GTNAP_GLOBAL->office_last_error);
    str_destroy(&pathname);
    return sheet;
}

/*---------------------------------------------------------------------------*/

Sheet *hb_office_sheet_create(void)
{
    return officesdk_sheet_create(&GTNAP_GLOBAL->office_last_error);
}

/*---------------------------------------------------------------------------*/

void hb_office_sheet_save(Sheet *sheet, HB_ITEM *pathname_block)
{
    String *pathname = hb_block_to_utf8(pathname_block);
    officesdk_sheet_save(sheet, tc(pathname), &GTNAP_GLOBAL->office_last_error);
    str_destroy(&pathname);
}

/*---------------------------------------------------------------------------*/

void hb_office_sheet_pdf(Sheet *sheet, HB_ITEM *pathname_block)
{
    String *pathname = hb_block_to_utf8(pathname_block);
    officesdk_sheet_pdf(sheet, tc(pathname), &GTNAP_GLOBAL->office_last_error);
    str_destroy(&pathname);
}

/*---------------------------------------------------------------------------*/

void hb_office_sheet_print(Sheet *sheet, HB_ITEM *filename_block, HB_ITEM *printer_block, const paperorient_t orient, const paperformat_t format, const uint32_t paper_width, const uint32_t paper_height, const uint32_t num_copies, const bool_t collate_copies, HB_ITEM *pages_block)
{
    String *filename = hb_block_to_utf8(filename_block);
    String *printer = hb_block_to_utf8(printer_block);
    String *pages = hb_block_to_utf8(pages_block);
    officesdk_sheet_print(sheet, tc(filename), tc(printer), orient, format, paper_width, paper_height, num_copies, collate_copies, tc(pages), &GTNAP_GLOBAL->office_last_error);
    str_destroy(&filename);
    str_destroy(&printer);
    str_destroy(&pages);
}

/*---------------------------------------------------------------------------*/

void hb_office_sheet_close(Sheet *sheet)
{
    officesdk_sheet_close(sheet, &GTNAP_GLOBAL->office_last_error);
}

/*---------------------------------------------------------------------------*/

uint32_t hb_office_sheet_add(Sheet *sheet)
{
    return officesdk_sheet_add(sheet, &GTNAP_GLOBAL->office_last_error);
}

/*---------------------------------------------------------------------------*/

void hb_office_sheet_name(Sheet *sheet, const uint32_t page, HB_ITEM *name_block)
{
    String *name = hb_block_to_utf8(name_block);
    officesdk_sheet_name(sheet, page, tc(name), &GTNAP_GLOBAL->office_last_error);
    str_destroy(&name);
}

/*---------------------------------------------------------------------------*/

void hb_office_sheet_protect(Sheet *sheet, const uint32_t page, const bool_t protect, HB_ITEM *pass_block)
{
    String *pass = hb_block_to_utf8(pass_block);
    officesdk_sheet_protect(sheet, page, protect, tc(pass), &GTNAP_GLOBAL->office_last_error);
    str_destroy(&pass);
}

/*---------------------------------------------------------------------------*/

void hb_office_sheet_freeze(Sheet *sheet, const uint32_t page, const uint32_t ncols, const uint32_t nrows)
{
    officesdk_sheet_freeze(sheet, page, ncols, nrows, &GTNAP_GLOBAL->office_last_error);
}

/*---------------------------------------------------------------------------*/

const char_t *hb_office_cell_ref(Sheet *sheet, const uint32_t page, const uint32_t col, const uint32_t row)
{
    str_destopt(&GTNAP_GLOBAL->office_last_cell_ref);
    GTNAP_GLOBAL->office_last_cell_ref = officesdk_sheet_cell_ref(sheet, page, col, row, &GTNAP_GLOBAL->office_last_error);
    return tc(GTNAP_GLOBAL->office_last_cell_ref);
}

/*---------------------------------------------------------------------------*/

void hb_office_sheet_cell_text(Sheet *sheet, const uint32_t page, const uint32_t col, const uint32_t row, HB_ITEM *text_block)
{
    String *text = hb_block_to_utf8(text_block);
    officesdk_sheet_cell_text(sheet, page, col, row, tc(text), &GTNAP_GLOBAL->office_last_error);
    str_destroy(&text);
}

/*---------------------------------------------------------------------------*/

void hb_office_sheet_cell_value(Sheet *sheet, const uint32_t page, const uint32_t col, const uint32_t row, const real64_t value)
{
    officesdk_sheet_cell_value(sheet, page, col, row, value, &GTNAP_GLOBAL->office_last_error);
}

/*---------------------------------------------------------------------------*/

void hb_office_sheet_cell_date(Sheet *sheet, const uint32_t page, const uint32_t col, const uint32_t row, const uint8_t day, const uint8_t month, const int16_t year)
{
    officesdk_sheet_cell_date(sheet, page, col, row, day, month, year, &GTNAP_GLOBAL->office_last_error);
}

/*---------------------------------------------------------------------------*/

void hb_office_sheet_cell_formula(Sheet *sheet, const uint32_t page, const uint32_t col, const uint32_t row, HB_ITEM *formula_block)
{
    String *formula = hb_block_to_utf8(formula_block);
    officesdk_sheet_cell_formula(sheet, page, col, row, tc(formula), &GTNAP_GLOBAL->office_last_error);
    str_destroy(&formula);
}

/*---------------------------------------------------------------------------*/

void hb_office_sheet_cell_numformat(Sheet *sheet, const uint32_t page, const uint32_t col, const uint32_t row, const numformat_t format)
{
    officesdk_sheet_cell_numformat(sheet, page, col, row, format, &GTNAP_GLOBAL->office_last_error);
}

/*---------------------------------------------------------------------------*/

void hb_office_sheet_cell_font_family(Sheet *sheet, const uint32_t page, const uint32_t col, const uint32_t row, HB_ITEM *ffamily_block)
{
    String *ffamily = hb_block_to_utf8(ffamily_block);
    officesdk_sheet_cell_font_family(sheet, page, col, row, tc(ffamily), &GTNAP_GLOBAL->office_last_error);
    str_destroy(&ffamily);
}

/*---------------------------------------------------------------------------*/

void hb_office_sheet_cell_font_size(Sheet *sheet, const uint32_t page, const uint32_t col, const uint32_t row, const real32_t fsize)
{
    officesdk_sheet_cell_font_size(sheet, page, col, row, fsize, &GTNAP_GLOBAL->office_last_error);
}

/*---------------------------------------------------------------------------*/

void hb_office_sheet_cell_bold(Sheet *sheet, const uint32_t page, const uint32_t col, const uint32_t row, const bool_t bold)
{
    officesdk_sheet_cell_bold(sheet, page, col, row, bold, &GTNAP_GLOBAL->office_last_error);
}

/*---------------------------------------------------------------------------*/

void hb_office_sheet_cell_italic(Sheet *sheet, const uint32_t page, const uint32_t col, const uint32_t row, const bool_t italic)
{
    officesdk_sheet_cell_italic(sheet, page, col, row, italic, &GTNAP_GLOBAL->office_last_error);
}

/*---------------------------------------------------------------------------*/

void hb_office_sheet_cell_halign(Sheet *sheet, const uint32_t page, const uint32_t col, const uint32_t row, const halign_t align)
{
    officesdk_sheet_cell_halign(sheet, page, col, row, align, &GTNAP_GLOBAL->office_last_error);
}

/*---------------------------------------------------------------------------*/

void hb_office_sheet_cell_valign(Sheet *sheet, const uint32_t page, const uint32_t col, const uint32_t row, const valign_t align)
{
    officesdk_sheet_cell_valign(sheet, page, col, row, align, &GTNAP_GLOBAL->office_last_error);
}

/*---------------------------------------------------------------------------*/

void hb_office_sheet_cell_wrap(Sheet *sheet, const uint32_t page, const uint32_t col, const uint32_t row, const bool_t wrapped)
{
    officesdk_sheet_cell_wrap(sheet, page, col, row, wrapped, &GTNAP_GLOBAL->office_last_error);
}

/*---------------------------------------------------------------------------*/

void hb_office_sheet_cell_color(Sheet *sheet, const uint32_t page, const uint32_t col, const uint32_t row, const uint32_t rgb)
{
    officesdk_sheet_cell_color(sheet, page, col, row, rgb, &GTNAP_GLOBAL->office_last_error);
}

/*---------------------------------------------------------------------------*/

void hb_office_sheet_cell_backcolor(Sheet *sheet, const uint32_t page, const uint32_t col, const uint32_t row, const uint32_t rgb)
{
    officesdk_sheet_cell_backcolor(sheet, page, col, row, rgb, &GTNAP_GLOBAL->office_last_error);
}

/*---------------------------------------------------------------------------*/

void hb_office_sheet_cells_backcolor(Sheet *sheet, const uint32_t page, const uint32_t st_col, const uint32_t st_row, const uint32_t ed_col, const uint32_t ed_row, const uint32_t rgb)
{
    officesdk_sheet_cells_backcolor(sheet, page, st_col, st_row, ed_col, ed_row, rgb, &GTNAP_GLOBAL->office_last_error);
}

/*---------------------------------------------------------------------------*/

void hb_office_sheet_cell_image(Sheet *sheet, const uint32_t page, const uint32_t col, const uint32_t row, HB_ITEM *image_path_block)
{
    String *image_path = hb_block_to_utf8(image_path_block);
    officesdk_sheet_cell_image(sheet, page, col, row, tc(image_path), &GTNAP_GLOBAL->office_last_error);
    str_destroy(&image_path);
}

/*---------------------------------------------------------------------------*/

void hb_office_sheet_cell_border(Sheet *sheet, const uint32_t page, const uint32_t col, const uint32_t row, const linestyle_t style, const uint32_t thickness, const uint32_t rgb)
{
    officesdk_sheet_cell_border(sheet, page, col, row, style, thickness, rgb, &GTNAP_GLOBAL->office_last_error);
}

/*---------------------------------------------------------------------------*/

void hb_office_sheet_cells_border(Sheet *sheet, const uint32_t page, const uint32_t st_col, const uint32_t st_row, const uint32_t ed_col, const uint32_t ed_row, const linestyle_t style, const uint32_t thickness, const uint32_t rgb)
{
    officesdk_sheet_cells_border(sheet, page, st_col, st_row, ed_col, ed_row, style, thickness, rgb, &GTNAP_GLOBAL->office_last_error);
}

/*---------------------------------------------------------------------------*/

void hb_office_sheet_cells_merge(Sheet *sheet, const uint32_t page, const uint32_t st_col, const uint32_t st_row, const uint32_t ed_col, const uint32_t ed_row)
{
    officesdk_sheet_cells_merge(sheet, page, st_col, st_row, ed_col, ed_row, &GTNAP_GLOBAL->office_last_error);
}

/*---------------------------------------------------------------------------*/

void hb_office_sheet_column_visible(Sheet *sheet, const uint32_t page, const uint32_t col, const bool_t visible)
{
    officesdk_sheet_column_visible(sheet, page, col, visible, &GTNAP_GLOBAL->office_last_error);
}

/*---------------------------------------------------------------------------*/

void hb_office_sheet_column_optimal_width(Sheet *sheet, const uint32_t page, const uint32_t col, const bool_t optimal_width)
{
    officesdk_sheet_column_optimal_width(sheet, page, col, optimal_width, &GTNAP_GLOBAL->office_last_error);
}

/*---------------------------------------------------------------------------*/

void hb_office_sheet_column_width(Sheet *sheet, const uint32_t page, const uint32_t col, const uint32_t width)
{
    officesdk_sheet_column_width(sheet, page, col, width, &GTNAP_GLOBAL->office_last_error);
}

/*---------------------------------------------------------------------------*/

void hb_office_sheet_row_visible(Sheet *sheet, const uint32_t page, const uint32_t row, const bool_t visible)
{
    officesdk_sheet_row_visible(sheet, page, row, visible, &GTNAP_GLOBAL->office_last_error);
}

/*---------------------------------------------------------------------------*/

void hb_office_sheet_row_optimal_height(Sheet *sheet, const uint32_t page, const uint32_t row, const bool_t optimal_height)
{
    officesdk_sheet_row_optimal_height(sheet, page, row, optimal_height, &GTNAP_GLOBAL->office_last_error);
}

/*---------------------------------------------------------------------------*/

void hb_office_sheet_row_height(Sheet *sheet, const uint32_t page, const uint32_t row, const uint32_t height)
{
    officesdk_sheet_row_height(sheet, page, row, height, &GTNAP_GLOBAL->office_last_error);
}

/*---------------------------------------------------------------------------*/

Writer *hb_office_writer_open(HB_ITEM *pathname_block)
{
    String *pathname = hb_block_to_utf8(pathname_block);
    Writer *writer = officesdk_writer_open(tc(pathname), &GTNAP_GLOBAL->office_last_error);
    str_destroy(&pathname);
    return writer;
}

/*---------------------------------------------------------------------------*/

Writer *hb_office_writer_create(void)
{
    return officesdk_writer_create(&GTNAP_GLOBAL->office_last_error);
}

/*---------------------------------------------------------------------------*/

void hb_office_writer_save(Writer *writer, HB_ITEM *pathname_block)
{
    String *pathname = hb_block_to_utf8(pathname_block);
    officesdk_writer_save(writer, tc(pathname), &GTNAP_GLOBAL->office_last_error);
    str_destroy(&pathname);
}

/*---------------------------------------------------------------------------*/

void hb_office_writer_pdf(Writer *writer, HB_ITEM *pathname_block)
{
    String *pathname = hb_block_to_utf8(pathname_block);
    officesdk_writer_pdf(writer, tc(pathname), &GTNAP_GLOBAL->office_last_error);
    str_destroy(&pathname);
}

/*---------------------------------------------------------------------------*/

void hb_office_writer_print(Writer *writer, HB_ITEM *filename_block, HB_ITEM *printer_block, const paperorient_t orient, const paperformat_t format, const uint32_t paper_width, const uint32_t paper_height, const uint32_t num_copies, const bool_t collate_copies, HB_ITEM *pages_block)
{
    String *filename = hb_block_to_utf8(filename_block);
    String *printer = hb_block_to_utf8(printer_block);
    String *pages = hb_block_to_utf8(pages_block);
    officesdk_writer_print(writer, tc(filename), tc(printer), orient, format, paper_width, paper_height, num_copies, collate_copies, tc(pages), &GTNAP_GLOBAL->office_last_error);
    str_destroy(&filename);
    str_destroy(&printer);
    str_destroy(&pages);
}

/*---------------------------------------------------------------------------*/

void hb_office_writer_close(Writer *writer)
{
    officesdk_writer_close(writer, &GTNAP_GLOBAL->office_last_error);
}

/*---------------------------------------------------------------------------*/

void hb_office_writer_page_header_show(Writer *writer, const bool_t show)
{
    officesdk_writer_page_header_show(writer, show, &GTNAP_GLOBAL->office_last_error);
}

/*---------------------------------------------------------------------------*/

void hb_office_writer_page_header_margins(Writer *writer, const uint32_t left, const uint32_t right, const uint32_t spacing, const uint32_t height, const bool_t dynamic_spacing, const bool_t dynamic_height)
{
    officesdk_writer_page_header_margins(writer, left, right, spacing, height, dynamic_spacing, dynamic_height, &GTNAP_GLOBAL->office_last_error);
}

/*---------------------------------------------------------------------------*/

void hb_office_writer_page_footer_show(Writer *writer, const bool_t show)
{
    officesdk_writer_page_footer_show(writer, show, &GTNAP_GLOBAL->office_last_error);
}

/*---------------------------------------------------------------------------*/

void hb_office_writer_page_footer_margins(Writer *writer, const uint32_t left, const uint32_t right, const uint32_t spacing, const uint32_t height, const bool_t dynamic_spacing, const bool_t dynamic_height)
{
    officesdk_writer_page_footer_margins(writer, left, right, spacing, height, dynamic_spacing, dynamic_height, &GTNAP_GLOBAL->office_last_error);
}

/*---------------------------------------------------------------------------*/

void hb_office_writer_page_margins(Writer *writer, const uint32_t left, const uint32_t right, const uint32_t top, const uint32_t bottom, const uint32_t gutter)
{
    officesdk_writer_page_margins(writer, left, right, top, bottom, gutter, &GTNAP_GLOBAL->office_last_error);
}

/*---------------------------------------------------------------------------*/

void hb_office_writer_text_space(Writer *writer, const textspace_t space)
{
    /* TODO: Writer cache if we have several DOC at same time */
    unref(writer);
    GTNAP_GLOBAL->office_text_space = space;
}

/*---------------------------------------------------------------------------*/

void hb_office_writer_font_family(Writer *writer, HB_ITEM *ffamily_block)
{
    String *ffamily = hb_block_to_utf8(ffamily_block);
    officesdk_writer_font_family(writer, GTNAP_GLOBAL->office_text_space, tc(ffamily), &GTNAP_GLOBAL->office_last_error);
    str_destroy(&ffamily);
}

/*---------------------------------------------------------------------------*/

void hb_office_writer_font_size(Writer *writer, const real32_t fsize)
{
    officesdk_writer_font_size(writer, GTNAP_GLOBAL->office_text_space, fsize, &GTNAP_GLOBAL->office_last_error);
}

/*---------------------------------------------------------------------------*/

void hb_office_writer_bold(Writer *writer, const bool_t bold)
{
    officesdk_writer_bold(writer, GTNAP_GLOBAL->office_text_space, bold, &GTNAP_GLOBAL->office_last_error);
}

/*---------------------------------------------------------------------------*/

void hb_office_writer_italic(Writer *writer, const bool_t italic)
{
    officesdk_writer_italic(writer, GTNAP_GLOBAL->office_text_space, italic, &GTNAP_GLOBAL->office_last_error);
}

/*---------------------------------------------------------------------------*/

void hb_office_writer_paragraph_halign(Writer *writer, const halign_t align)
{
    officesdk_writer_paragraph_halign(writer, GTNAP_GLOBAL->office_text_space, align, &GTNAP_GLOBAL->office_last_error);
}

/*---------------------------------------------------------------------------*/

void hb_office_writer_paragraph_lspacing(Writer *writer, const uint32_t height)
{
    officesdk_writer_paragraph_lspacing(writer, GTNAP_GLOBAL->office_text_space, height, &GTNAP_GLOBAL->office_last_error);
}

/*---------------------------------------------------------------------------*/

void hb_office_writer_insert_text(Writer *writer, HB_ITEM *text_block)
{
    String *text = hb_block_to_utf8(text_block);
    officesdk_writer_insert_text(writer, GTNAP_GLOBAL->office_text_space, tc(text), &GTNAP_GLOBAL->office_last_error);
    str_destroy(&text);
}

/*---------------------------------------------------------------------------*/

void hb_office_writer_insert_dash(Writer *writer, const uint32_t n)
{
    uint8_t dash[3] = {0xE2, 0x80, 0x94};
    char_t *dashes = heap_new_n(n * 3 + 1, char_t);
    char_t *it = dashes;
    uint32_t i = 0;
    for (i = 0; i < n; ++i)
    {
        *it++ = dash[0];
        *it++ = dash[1];
        *it++ = dash[2];
    }
    *it = 0;

    officesdk_writer_insert_text(writer, GTNAP_GLOBAL->office_text_space, dashes, &GTNAP_GLOBAL->office_last_error);
    heap_delete_n(&dashes, n * 3 + 1, char_t);
}

/*---------------------------------------------------------------------------*/

void hb_office_writer_insert_image(Writer *writer, const anchortype_t anchor, const uint32_t width, const uint32_t height, const halign_t halign, const valign_t valign, HB_ITEM *image_path_block)
{
    String *image_path = hb_block_to_utf8(image_path_block);
    officesdk_writer_insert_image(writer, GTNAP_GLOBAL->office_text_space, anchor, width, height, halign, valign, tc(image_path), &GTNAP_GLOBAL->office_last_error);
    str_destroy(&image_path);
}

/*---------------------------------------------------------------------------*/

void hb_office_writer_insert_page_number(Writer *writer)
{
    officesdk_writer_insert_page_number(writer, GTNAP_GLOBAL->office_text_space, &GTNAP_GLOBAL->office_last_error);
}

/*---------------------------------------------------------------------------*/

void hb_office_writer_insert_new_line(Writer *writer)
{
    officesdk_writer_insert_new_line(writer, GTNAP_GLOBAL->office_text_space, &GTNAP_GLOBAL->office_last_error);
}

/*---------------------------------------------------------------------------*/

void hb_office_writer_insert_paragraph(Writer *writer)
{
    officesdk_writer_insert_paragraph(writer, GTNAP_GLOBAL->office_text_space, &GTNAP_GLOBAL->office_last_error);
}

/*---------------------------------------------------------------------------*/

void hb_office_writer_insert_page_break(Writer *writer)
{
    officesdk_writer_insert_page_break(writer, &GTNAP_GLOBAL->office_last_error);
}
