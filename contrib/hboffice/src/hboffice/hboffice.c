/*
 * LibreOffice Harbour Wrapper
 * 2024 - Francisco Garcia Collado
 */

#include "hboffice.h"
#include <officesdk/officesdk.h>
#include <officesdk/sheetsdk.h>
#include <officesdk/writersdk.h>

#include "hbapiitm.h"
#include "hbapistr.h"

#define i_CELL_REF_SIZE 128
typedef struct _hboffice_t HBOffice;

struct _hboffice_t
{
    bool_t init;
    textspace_t text_space;
    sdkres_t last_error;
    char_t last_cell_ref[i_CELL_REF_SIZE];
};

/*---------------------------------------------------------------------------*/

static HBOffice HBOFFICE_GLOBAL = {FALSE, ENUM_MAX(textspace_t), ENUM_MAX(sdkres_t), {0}};

/*---------------------------------------------------------------------------*/

static uint32_t i_remove_utf8_CR(char_t *utf8)
{
    /* Remove the Carriage Return (CR) character (NAppGUI doesn't like) */
    uint32_t i = 0, j = 0;
    for (; utf8[i] != 0;)
    {
        if (utf8[i] != 13)
        {
            utf8[j] = utf8[i];
            j += 1;
        }

        i += 1;
    }

    utf8[j] = 0;
    return j;
}

/*---------------------------------------------------------------------------*/

static char_t *i_item_to_utf8_string(HB_ITEM *item)
{
    HB_SIZE s1 = 0, s2 = 0;
    char_t *str = NULL;
    s1 = hb_itemCopyStrUTF8(item, NULL, (HB_SIZE)UINT32_MAX);
    str = cast(malloc(s1 + 1), char_t);
    s2 = hb_itemCopyStrUTF8(item, str, s1 + 1);
    i_remove_utf8_CR(str);
    (void)(s2);
    return str;
}

/*---------------------------------------------------------------------------*/

static char_t *i_block_to_utf8(HB_ITEM *item)
{
    char_t *str = NULL;

    if (HB_ITEM_TYPE(item) == HB_IT_STRING)
    {
        str = i_item_to_utf8_string(item);
    }
    else if (HB_ITEM_TYPE(item) == HB_IT_BLOCK)
    {
        PHB_ITEM ritem = hb_itemDo(item, 0);
        str = i_item_to_utf8_string(ritem);
        hb_itemRelease(ritem);
    }
    else
    {
        str = cast(malloc(1), char_t);
        str[0] = 0;
    }

    return str;
}

/*---------------------------------------------------------------------------*/

static void i_destroy_utf8(char_t **str)
{
    free(cast(*str, void));
    *str = NULL;
}

/*---------------------------------------------------------------------------*/

bool_t hb_office_init(void)
{
    if (HBOFFICE_GLOBAL.init == FALSE)
    {
        HBOFFICE_GLOBAL.text_space = ekTEXT_SPACE_PAGE;
        HBOFFICE_GLOBAL.last_cell_ref[0] = '\0';
        HBOFFICE_GLOBAL.last_error = ENUM_MAX(sdkres_t);
        HBOFFICE_GLOBAL.init = TRUE;
    }

    return HBOFFICE_GLOBAL.init;
}

/*---------------------------------------------------------------------------*/

void hb_office_finish(void)
{
    if (HBOFFICE_GLOBAL.init == TRUE)
    {
        officesdk_finish();
        HBOFFICE_GLOBAL.init = FALSE;
    }
}

/*---------------------------------------------------------------------------*/

uint32_t hb_office_last_error(void)
{
    if (HBOFFICE_GLOBAL.init == TRUE)
        return (uint32_t)HBOFFICE_GLOBAL.last_error;
    return UINT32_MAX;
}

/*---------------------------------------------------------------------------*/

const char_t *hb_office_error_str(const uint32_t errcode)
{
    if (HBOFFICE_GLOBAL.init == TRUE)
        return officesdk_error_str((sdkres_t)errcode);
    return "";
}

/*---------------------------------------------------------------------------*/

void hb_office_browse_doc(HB_ITEM *pathname_block)
{
    if (HBOFFICE_GLOBAL.init == TRUE)
    {
        char_t *pathname = i_block_to_utf8(pathname_block);
        officesdk_browse_doc(pathname, &HBOFFICE_GLOBAL.last_error);
        i_destroy_utf8(&pathname);
    }
}

/*---------------------------------------------------------------------------*/

uint32_t hb_office_rgb(const uint8_t red, const uint8_t green, const uint8_t blue)
{
    if (HBOFFICE_GLOBAL.init == TRUE)
        return officesdk_rgb(red, green, blue);
    return 0;
}

/*---------------------------------------------------------------------------*/

Sheet *hb_office_sheet_open(HB_ITEM *pathname_block)
{
    if (HBOFFICE_GLOBAL.init == TRUE)
    {
        char_t *pathname = i_block_to_utf8(pathname_block);
        Sheet *sheet = officesdk_sheet_open(pathname, &HBOFFICE_GLOBAL.last_error);
        i_destroy_utf8(&pathname);
        return sheet;
    }

    return NULL;
}

/*---------------------------------------------------------------------------*/

Sheet *hb_office_sheet_create(void)
{
    if (HBOFFICE_GLOBAL.init == TRUE)
        return officesdk_sheet_create(&HBOFFICE_GLOBAL.last_error);
    return NULL;
}

/*---------------------------------------------------------------------------*/

void hb_office_sheet_save(Sheet *sheet, HB_ITEM *pathname_block)
{
    if (HBOFFICE_GLOBAL.init == TRUE)
    {
        char_t *pathname = i_block_to_utf8(pathname_block);
        officesdk_sheet_save(sheet, pathname, &HBOFFICE_GLOBAL.last_error);
        i_destroy_utf8(&pathname);
    }
}

/*---------------------------------------------------------------------------*/

void hb_office_sheet_pdf(Sheet *sheet, HB_ITEM *pathname_block)
{
    if (HBOFFICE_GLOBAL.init == TRUE)
    {
        char_t *pathname = i_block_to_utf8(pathname_block);
        officesdk_sheet_pdf(sheet, pathname, &HBOFFICE_GLOBAL.last_error);
        i_destroy_utf8(&pathname);
    }
}

/*---------------------------------------------------------------------------*/

void hb_office_sheet_print(Sheet *sheet, HB_ITEM *filename_block, HB_ITEM *printer_block, const paperorient_t orient, const paperformat_t format, const uint32_t paper_width, const uint32_t paper_height, const uint32_t num_copies, const bool_t collate_copies, HB_ITEM *pages_block)
{
    if (HBOFFICE_GLOBAL.init == TRUE)
    {
        char_t *filename = i_block_to_utf8(filename_block);
        char_t *printer = i_block_to_utf8(printer_block);
        char_t *pages = i_block_to_utf8(pages_block);
        officesdk_sheet_print(sheet, filename, printer, orient, format, paper_width, paper_height, num_copies, collate_copies, pages, &HBOFFICE_GLOBAL.last_error);
        i_destroy_utf8(&filename);
        i_destroy_utf8(&printer);
        i_destroy_utf8(&pages);
    }
}

/*---------------------------------------------------------------------------*/

void hb_office_sheet_close(Sheet *sheet)
{
    if (HBOFFICE_GLOBAL.init == TRUE)
        officesdk_sheet_close(sheet, &HBOFFICE_GLOBAL.last_error);
}

/*---------------------------------------------------------------------------*/

uint32_t hb_office_sheet_add(Sheet *sheet)
{
    if (HBOFFICE_GLOBAL.init == TRUE)
        return officesdk_sheet_add(sheet, &HBOFFICE_GLOBAL.last_error);
    return UINT32_MAX;
}

/*---------------------------------------------------------------------------*/

void hb_office_sheet_name(Sheet *sheet, const uint32_t page, HB_ITEM *name_block)
{
    if (HBOFFICE_GLOBAL.init == TRUE)
    {
        char_t *name = i_block_to_utf8(name_block);
        officesdk_sheet_name(sheet, page, name, &HBOFFICE_GLOBAL.last_error);
        i_destroy_utf8(&name);
    }
}

/*---------------------------------------------------------------------------*/

void hb_office_sheet_protect(Sheet *sheet, const uint32_t page, const bool_t protect, HB_ITEM *pass_block)
{
    if (HBOFFICE_GLOBAL.init == TRUE)
    {
        char_t *pass = i_block_to_utf8(pass_block);
        officesdk_sheet_protect(sheet, page, protect, pass, &HBOFFICE_GLOBAL.last_error);
        i_destroy_utf8(&pass);
    }
}

/*---------------------------------------------------------------------------*/

void hb_office_sheet_freeze(Sheet *sheet, const uint32_t page, const uint32_t ncols, const uint32_t nrows)
{
    if (HBOFFICE_GLOBAL.init == TRUE)
        officesdk_sheet_freeze(sheet, page, ncols, nrows, &HBOFFICE_GLOBAL.last_error);
}

/*---------------------------------------------------------------------------*/

const char_t *hb_office_cell_ref(Sheet *sheet, const uint32_t page, const uint32_t col, const uint32_t row)
{
    if (HBOFFICE_GLOBAL.init == TRUE)
    {
        officesdk_sheet_cell_ref(sheet, page, col, row, HBOFFICE_GLOBAL.last_cell_ref, sizeof(HBOFFICE_GLOBAL.last_cell_ref), &HBOFFICE_GLOBAL.last_error);
        return HBOFFICE_GLOBAL.last_cell_ref;
    }

    return "";
}

/*---------------------------------------------------------------------------*/

void hb_office_sheet_cell_text(Sheet *sheet, const uint32_t page, const uint32_t col, const uint32_t row, HB_ITEM *text_block)
{
    if (HBOFFICE_GLOBAL.init == TRUE)
    {
        char_t *text = i_block_to_utf8(text_block);
        officesdk_sheet_cell_text(sheet, page, col, row, text, &HBOFFICE_GLOBAL.last_error);
        i_destroy_utf8(&text);
    }
}

/*---------------------------------------------------------------------------*/

void hb_office_sheet_cell_value(Sheet *sheet, const uint32_t page, const uint32_t col, const uint32_t row, const real64_t value)
{
    if (HBOFFICE_GLOBAL.init == TRUE)
        officesdk_sheet_cell_value(sheet, page, col, row, value, &HBOFFICE_GLOBAL.last_error);
}

/*---------------------------------------------------------------------------*/

void hb_office_sheet_cell_date(Sheet *sheet, const uint32_t page, const uint32_t col, const uint32_t row, const uint8_t day, const uint8_t month, const int16_t year)
{
    if (HBOFFICE_GLOBAL.init == TRUE)
        officesdk_sheet_cell_date(sheet, page, col, row, day, month, year, &HBOFFICE_GLOBAL.last_error);
}

/*---------------------------------------------------------------------------*/

void hb_office_sheet_cell_formula(Sheet *sheet, const uint32_t page, const uint32_t col, const uint32_t row, HB_ITEM *formula_block)
{
    if (HBOFFICE_GLOBAL.init == TRUE)
    {
        char_t *formula = i_block_to_utf8(formula_block);
        officesdk_sheet_cell_formula(sheet, page, col, row, formula, &HBOFFICE_GLOBAL.last_error);
        i_destroy_utf8(&formula);
    }
}

/*---------------------------------------------------------------------------*/

void hb_office_sheet_cell_numformat(Sheet *sheet, const uint32_t page, const uint32_t col, const uint32_t row, const numformat_t format)
{
    if (HBOFFICE_GLOBAL.init == TRUE)
        officesdk_sheet_cell_numformat(sheet, page, col, row, format, &HBOFFICE_GLOBAL.last_error);
}

/*---------------------------------------------------------------------------*/

void hb_office_sheet_cell_font_family(Sheet *sheet, const uint32_t page, const uint32_t col, const uint32_t row, HB_ITEM *ffamily_block)
{
    if (HBOFFICE_GLOBAL.init == TRUE)
    {
        char_t *ffamily = i_block_to_utf8(ffamily_block);
        officesdk_sheet_cell_font_family(sheet, page, col, row, ffamily, &HBOFFICE_GLOBAL.last_error);
        i_destroy_utf8(&ffamily);
    }
}

/*---------------------------------------------------------------------------*/

void hb_office_sheet_cell_font_size(Sheet *sheet, const uint32_t page, const uint32_t col, const uint32_t row, const real32_t fsize)
{
    if (HBOFFICE_GLOBAL.init == TRUE)
        officesdk_sheet_cell_font_size(sheet, page, col, row, fsize, &HBOFFICE_GLOBAL.last_error);
}

/*---------------------------------------------------------------------------*/

void hb_office_sheet_cell_bold(Sheet *sheet, const uint32_t page, const uint32_t col, const uint32_t row, const bool_t bold)
{
    if (HBOFFICE_GLOBAL.init == TRUE)
        officesdk_sheet_cell_bold(sheet, page, col, row, bold, &HBOFFICE_GLOBAL.last_error);
}

/*---------------------------------------------------------------------------*/

void hb_office_sheet_cell_italic(Sheet *sheet, const uint32_t page, const uint32_t col, const uint32_t row, const bool_t italic)
{
    if (HBOFFICE_GLOBAL.init == TRUE)
        officesdk_sheet_cell_italic(sheet, page, col, row, italic, &HBOFFICE_GLOBAL.last_error);
}

/*---------------------------------------------------------------------------*/

void hb_office_sheet_cell_halign(Sheet *sheet, const uint32_t page, const uint32_t col, const uint32_t row, const halign_t align)
{
    if (HBOFFICE_GLOBAL.init == TRUE)
        officesdk_sheet_cell_halign(sheet, page, col, row, align, &HBOFFICE_GLOBAL.last_error);
}

/*---------------------------------------------------------------------------*/

void hb_office_sheet_cell_valign(Sheet *sheet, const uint32_t page, const uint32_t col, const uint32_t row, const valign_t align)
{
    if (HBOFFICE_GLOBAL.init == TRUE)
        officesdk_sheet_cell_valign(sheet, page, col, row, align, &HBOFFICE_GLOBAL.last_error);
}

/*---------------------------------------------------------------------------*/

void hb_office_sheet_cell_wrap(Sheet *sheet, const uint32_t page, const uint32_t col, const uint32_t row, const bool_t wrapped)
{
    if (HBOFFICE_GLOBAL.init == TRUE)
        officesdk_sheet_cell_wrap(sheet, page, col, row, wrapped, &HBOFFICE_GLOBAL.last_error);
}

/*---------------------------------------------------------------------------*/

void hb_office_sheet_cell_color(Sheet *sheet, const uint32_t page, const uint32_t col, const uint32_t row, const uint32_t rgb)
{
    if (HBOFFICE_GLOBAL.init == TRUE)
        officesdk_sheet_cell_color(sheet, page, col, row, rgb, &HBOFFICE_GLOBAL.last_error);
}

/*---------------------------------------------------------------------------*/

void hb_office_sheet_cell_backcolor(Sheet *sheet, const uint32_t page, const uint32_t col, const uint32_t row, const uint32_t rgb)
{
    if (HBOFFICE_GLOBAL.init == TRUE)
        officesdk_sheet_cell_backcolor(sheet, page, col, row, rgb, &HBOFFICE_GLOBAL.last_error);
}

/*---------------------------------------------------------------------------*/

void hb_office_sheet_cells_backcolor(Sheet *sheet, const uint32_t page, const uint32_t st_col, const uint32_t st_row, const uint32_t ed_col, const uint32_t ed_row, const uint32_t rgb)
{
    if (HBOFFICE_GLOBAL.init == TRUE)
        officesdk_sheet_cells_backcolor(sheet, page, st_col, st_row, ed_col, ed_row, rgb, &HBOFFICE_GLOBAL.last_error);
}

/*---------------------------------------------------------------------------*/

void hb_office_sheet_cell_image(Sheet *sheet, const uint32_t page, const uint32_t col, const uint32_t row, HB_ITEM *image_path_block)
{
    if (HBOFFICE_GLOBAL.init == TRUE)
    {
        char_t *image_path = i_block_to_utf8(image_path_block);
        officesdk_sheet_cell_image(sheet, page, col, row, image_path, &HBOFFICE_GLOBAL.last_error);
        i_destroy_utf8(&image_path);
    }
}

/*---------------------------------------------------------------------------*/

void hb_office_sheet_cell_border(Sheet *sheet, const uint32_t page, const uint32_t col, const uint32_t row, const linestyle_t style, const uint32_t thickness, const uint32_t rgb)
{
    if (HBOFFICE_GLOBAL.init == TRUE)
        officesdk_sheet_cell_border(sheet, page, col, row, style, thickness, rgb, &HBOFFICE_GLOBAL.last_error);
}

/*---------------------------------------------------------------------------*/

void hb_office_sheet_cells_border(Sheet *sheet, const uint32_t page, const uint32_t st_col, const uint32_t st_row, const uint32_t ed_col, const uint32_t ed_row, const linestyle_t style, const uint32_t thickness, const uint32_t rgb)
{
    if (HBOFFICE_GLOBAL.init == TRUE)
        officesdk_sheet_cells_border(sheet, page, st_col, st_row, ed_col, ed_row, style, thickness, rgb, &HBOFFICE_GLOBAL.last_error);
}

/*---------------------------------------------------------------------------*/

void hb_office_sheet_cells_merge(Sheet *sheet, const uint32_t page, const uint32_t st_col, const uint32_t st_row, const uint32_t ed_col, const uint32_t ed_row)
{
    if (HBOFFICE_GLOBAL.init == TRUE)
        officesdk_sheet_cells_merge(sheet, page, st_col, st_row, ed_col, ed_row, &HBOFFICE_GLOBAL.last_error);
}

/*---------------------------------------------------------------------------*/

void hb_office_sheet_column_visible(Sheet *sheet, const uint32_t page, const uint32_t col, const bool_t visible)
{
    if (HBOFFICE_GLOBAL.init == TRUE)
        officesdk_sheet_column_visible(sheet, page, col, visible, &HBOFFICE_GLOBAL.last_error);
}

/*---------------------------------------------------------------------------*/

void hb_office_sheet_column_optimal_width(Sheet *sheet, const uint32_t page, const uint32_t col, const bool_t optimal_width)
{
    if (HBOFFICE_GLOBAL.init == TRUE)
        officesdk_sheet_column_optimal_width(sheet, page, col, optimal_width, &HBOFFICE_GLOBAL.last_error);
}

/*---------------------------------------------------------------------------*/

void hb_office_sheet_column_width(Sheet *sheet, const uint32_t page, const uint32_t col, const uint32_t width)
{
    if (HBOFFICE_GLOBAL.init == TRUE)
        officesdk_sheet_column_width(sheet, page, col, width, &HBOFFICE_GLOBAL.last_error);
}

/*---------------------------------------------------------------------------*/

void hb_office_sheet_row_visible(Sheet *sheet, const uint32_t page, const uint32_t row, const bool_t visible)
{
    if (HBOFFICE_GLOBAL.init == TRUE)
        officesdk_sheet_row_visible(sheet, page, row, visible, &HBOFFICE_GLOBAL.last_error);
}

/*---------------------------------------------------------------------------*/

void hb_office_sheet_row_optimal_height(Sheet *sheet, const uint32_t page, const uint32_t row, const bool_t optimal_height)
{
    if (HBOFFICE_GLOBAL.init == TRUE)
        officesdk_sheet_row_optimal_height(sheet, page, row, optimal_height, &HBOFFICE_GLOBAL.last_error);
}

/*---------------------------------------------------------------------------*/

void hb_office_sheet_row_height(Sheet *sheet, const uint32_t page, const uint32_t row, const uint32_t height)
{
    if (HBOFFICE_GLOBAL.init == TRUE)
        officesdk_sheet_row_height(sheet, page, row, height, &HBOFFICE_GLOBAL.last_error);
}

/*---------------------------------------------------------------------------*/

Writer *hb_office_writer_open(HB_ITEM *pathname_block)
{
    if (HBOFFICE_GLOBAL.init == TRUE)
    {
        char_t *pathname = i_block_to_utf8(pathname_block);
        Writer *writer = officesdk_writer_open(pathname, &HBOFFICE_GLOBAL.last_error);
        i_destroy_utf8(&pathname);
        return writer;
    }

    return NULL;
}

/*---------------------------------------------------------------------------*/

Writer *hb_office_writer_create(void)
{
    if (HBOFFICE_GLOBAL.init == TRUE)
        return officesdk_writer_create(&HBOFFICE_GLOBAL.last_error);
    return NULL;
}

/*---------------------------------------------------------------------------*/

void hb_office_writer_save(Writer *writer, HB_ITEM *pathname_block)
{
    if (HBOFFICE_GLOBAL.init == TRUE)
    {
        char_t *pathname = i_block_to_utf8(pathname_block);
        officesdk_writer_save(writer, pathname, &HBOFFICE_GLOBAL.last_error);
        i_destroy_utf8(&pathname);
    }
}

/*---------------------------------------------------------------------------*/

void hb_office_writer_pdf(Writer *writer, HB_ITEM *pathname_block)
{
    if (HBOFFICE_GLOBAL.init == TRUE)
    {
        char_t *pathname = i_block_to_utf8(pathname_block);
        officesdk_writer_pdf(writer, pathname, &HBOFFICE_GLOBAL.last_error);
        i_destroy_utf8(&pathname);
    }
}

/*---------------------------------------------------------------------------*/

void hb_office_writer_print(Writer *writer, HB_ITEM *filename_block, HB_ITEM *printer_block, const paperorient_t orient, const paperformat_t format, const uint32_t paper_width, const uint32_t paper_height, const uint32_t num_copies, const bool_t collate_copies, HB_ITEM *pages_block)
{
    if (HBOFFICE_GLOBAL.init == TRUE)
    {
        char_t *filename = i_block_to_utf8(filename_block);
        char_t *printer = i_block_to_utf8(printer_block);
        char_t *pages = i_block_to_utf8(pages_block);
        officesdk_writer_print(writer, filename, printer, orient, format, paper_width, paper_height, num_copies, collate_copies, pages, &HBOFFICE_GLOBAL.last_error);
        i_destroy_utf8(&filename);
        i_destroy_utf8(&printer);
        i_destroy_utf8(&pages);
    }
}

/*---------------------------------------------------------------------------*/

void hb_office_writer_close(Writer *writer)
{
    if (HBOFFICE_GLOBAL.init == TRUE)
        officesdk_writer_close(writer, &HBOFFICE_GLOBAL.last_error);
}

/*---------------------------------------------------------------------------*/

void hb_office_writer_page_header_show(Writer *writer, const bool_t show)
{
    if (HBOFFICE_GLOBAL.init == TRUE)
        officesdk_writer_page_header_show(writer, show, &HBOFFICE_GLOBAL.last_error);
}

/*---------------------------------------------------------------------------*/

void hb_office_writer_page_header_margins(Writer *writer, const uint32_t left, const uint32_t right, const uint32_t spacing, const uint32_t height, const bool_t dynamic_spacing, const bool_t dynamic_height)
{
    if (HBOFFICE_GLOBAL.init == TRUE)
        officesdk_writer_page_header_margins(writer, left, right, spacing, height, dynamic_spacing, dynamic_height, &HBOFFICE_GLOBAL.last_error);
}

/*---------------------------------------------------------------------------*/

void hb_office_writer_page_footer_show(Writer *writer, const bool_t show)
{
    if (HBOFFICE_GLOBAL.init == TRUE)
        officesdk_writer_page_footer_show(writer, show, &HBOFFICE_GLOBAL.last_error);
}

/*---------------------------------------------------------------------------*/

void hb_office_writer_page_footer_margins(Writer *writer, const uint32_t left, const uint32_t right, const uint32_t spacing, const uint32_t height, const bool_t dynamic_spacing, const bool_t dynamic_height)
{
    if (HBOFFICE_GLOBAL.init == TRUE)
        officesdk_writer_page_footer_margins(writer, left, right, spacing, height, dynamic_spacing, dynamic_height, &HBOFFICE_GLOBAL.last_error);
}

/*---------------------------------------------------------------------------*/

void hb_office_writer_page_margins(Writer *writer, const uint32_t left, const uint32_t right, const uint32_t top, const uint32_t bottom, const uint32_t gutter)
{
    if (HBOFFICE_GLOBAL.init == TRUE)
        officesdk_writer_page_margins(writer, left, right, top, bottom, gutter, &HBOFFICE_GLOBAL.last_error);
}

/*---------------------------------------------------------------------------*/

void hb_office_writer_text_space(Writer *writer, const textspace_t space)
{
    unref(writer);
    if (HBOFFICE_GLOBAL.init == TRUE)
    {
        /* TODO: Writer cache if we have several DOC at same time */
        HBOFFICE_GLOBAL.text_space = space;
    }
}

/*---------------------------------------------------------------------------*/

void hb_office_writer_font_family(Writer *writer, HB_ITEM *ffamily_block)
{
    if (HBOFFICE_GLOBAL.init == TRUE)
    {
        char_t *ffamily = i_block_to_utf8(ffamily_block);
        officesdk_writer_font_family(writer, HBOFFICE_GLOBAL.text_space, ffamily, &HBOFFICE_GLOBAL.last_error);
        i_destroy_utf8(&ffamily);
    }
}

/*---------------------------------------------------------------------------*/

void hb_office_writer_font_size(Writer *writer, const real32_t fsize)
{
    if (HBOFFICE_GLOBAL.init == TRUE)
        officesdk_writer_font_size(writer, HBOFFICE_GLOBAL.text_space, fsize, &HBOFFICE_GLOBAL.last_error);
}

/*---------------------------------------------------------------------------*/

void hb_office_writer_bold(Writer *writer, const bool_t bold)
{
    if (HBOFFICE_GLOBAL.init == TRUE)
        officesdk_writer_bold(writer, HBOFFICE_GLOBAL.text_space, bold, &HBOFFICE_GLOBAL.last_error);
}

/*---------------------------------------------------------------------------*/

void hb_office_writer_italic(Writer *writer, const bool_t italic)
{
    if (HBOFFICE_GLOBAL.init == TRUE)
        officesdk_writer_italic(writer, HBOFFICE_GLOBAL.text_space, italic, &HBOFFICE_GLOBAL.last_error);
}

/*---------------------------------------------------------------------------*/

void hb_office_writer_paragraph_halign(Writer *writer, const halign_t align)
{
    if (HBOFFICE_GLOBAL.init == TRUE)
        officesdk_writer_paragraph_halign(writer, HBOFFICE_GLOBAL.text_space, align, &HBOFFICE_GLOBAL.last_error);
}

/*---------------------------------------------------------------------------*/

void hb_office_writer_paragraph_lspacing(Writer *writer, const uint32_t height)
{
    if (HBOFFICE_GLOBAL.init == TRUE)
        officesdk_writer_paragraph_lspacing(writer, HBOFFICE_GLOBAL.text_space, height, &HBOFFICE_GLOBAL.last_error);
}

/*---------------------------------------------------------------------------*/

void hb_office_writer_insert_text(Writer *writer, HB_ITEM *text_block)
{
    if (HBOFFICE_GLOBAL.init == TRUE)
    {
        char_t *text = i_block_to_utf8(text_block);
        officesdk_writer_insert_text(writer, HBOFFICE_GLOBAL.text_space, text, &HBOFFICE_GLOBAL.last_error);
        i_destroy_utf8(&text);
    }
}

/*---------------------------------------------------------------------------*/

void hb_office_writer_insert_dash(Writer *writer, const uint32_t n)
{
    if (HBOFFICE_GLOBAL.init == TRUE)
    {
        uint8_t dash[3] = {0xE2, 0x80, 0x94};
        char_t *dashes = cast(malloc(n * 3 + 1), char_t);
        char_t *it = dashes;
        uint32_t i = 0;
        for (i = 0; i < n; ++i)
        {
            *it++ = dash[0];
            *it++ = dash[1];
            *it++ = dash[2];
        }
        *it = 0;

        officesdk_writer_insert_text(writer, HBOFFICE_GLOBAL.text_space, dashes, &HBOFFICE_GLOBAL.last_error);
        free(dashes);
    }
}

/*---------------------------------------------------------------------------*/

void hb_office_writer_insert_image(Writer *writer, const anchortype_t anchor, const uint32_t width, const uint32_t height, const halign_t halign, const valign_t valign, HB_ITEM *image_path_block)
{
    if (HBOFFICE_GLOBAL.init == TRUE)
    {
        char_t *image_path = i_block_to_utf8(image_path_block);
        officesdk_writer_insert_image(writer, HBOFFICE_GLOBAL.text_space, anchor, width, height, halign, valign, image_path, &HBOFFICE_GLOBAL.last_error);
        i_destroy_utf8(&image_path);
    }
}

/*---------------------------------------------------------------------------*/

void hb_office_writer_insert_page_number(Writer *writer)
{
    if (HBOFFICE_GLOBAL.init == TRUE)
        officesdk_writer_insert_page_number(writer, HBOFFICE_GLOBAL.text_space, &HBOFFICE_GLOBAL.last_error);
}

/*---------------------------------------------------------------------------*/

void hb_office_writer_insert_new_line(Writer *writer)
{
    if (HBOFFICE_GLOBAL.init == TRUE)
        officesdk_writer_insert_new_line(writer, HBOFFICE_GLOBAL.text_space, &HBOFFICE_GLOBAL.last_error);
}

/*---------------------------------------------------------------------------*/

void hb_office_writer_insert_paragraph(Writer *writer)
{
    if (HBOFFICE_GLOBAL.init == TRUE)
        officesdk_writer_insert_paragraph(writer, HBOFFICE_GLOBAL.text_space, &HBOFFICE_GLOBAL.last_error);
}

/*---------------------------------------------------------------------------*/

void hb_office_writer_insert_page_break(Writer *writer)
{
    if (HBOFFICE_GLOBAL.init == TRUE)
        officesdk_writer_insert_page_break(writer, &HBOFFICE_GLOBAL.last_error);
}
