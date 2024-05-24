/*
 * LibreOffice Harbour Wrapper
 * 2024 - Francisco Garcia Collado
 */

#include "hboffice.h"
#include "hboffice.inl"
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

#define i_CELL_REF_SIZE 128
typedef struct _hbofficefuncs_t HBOfficeFuncs;
typedef struct _hboffice_t HBOffice;
typedef struct _string_t String;

/*---------------------------------------------------------------------------*/

/* LibreOffice SDK C mapping for runtime symbol loading */
typedef void (*FPtr_officesdk_finish)(void);
typedef const char_t *(*FPtr_officesdk_error)(const sdkres_t code);
typedef void (*FPtr_officesdk_browse_doc)(const char_t *pathname, sdkres_t *err);
typedef uint32_t (*FPtr_officesdk_rgb)(const uint8_t red, const uint8_t green, const uint8_t blue);

/* LibreOffice Writter mapping */
typedef Writer *(*FPtr_officesdk_writer_open)(const char_t *pathname, sdkres_t *err);
typedef Writer *(*FPtr_officesdk_writer_create)(sdkres_t *err);
typedef void (*FPtr_officesdk_writer_save)(Writer *writer, const char_t *pathname, sdkres_t *err);
typedef void (*FPtr_officesdk_writer_pdf)(Writer *writer, const char_t *pathname, sdkres_t *err);
typedef void (*FPtr_officesdk_writer_print)(Writer *writer, const char_t *filename, const char_t *printer, const paperorient_t orient, const paperformat_t format, const uint32_t paper_width, const uint32_t paper_height, const uint32_t num_copies, const bool_t collate_copies, const char_t *pages, sdkres_t *err);
typedef void (*FPtr_officesdk_writer_close)(Writer *writer, sdkres_t *err);
typedef void (*FPtr_officesdk_writer_page_header_show)(Writer *writer, const bool_t show, sdkres_t *err);
typedef void (*FPtr_officesdk_writer_page_header_margins)(Writer *writer, const uint32_t left, const uint32_t right, const uint32_t spacing, const uint32_t height, const bool_t dynamic_spacing, const bool_t dynamic_height, sdkres_t *err);
typedef void (*FPtr_officesdk_writer_page_footer_show)(Writer *writer, const bool_t show, sdkres_t *err);
typedef void (*FPtr_officesdk_writer_page_footer_margins)(Writer *writer, const uint32_t left, const uint32_t right, const uint32_t spacing, const uint32_t height, const bool_t dynamic_spacing, const bool_t dynamic_height, sdkres_t *err);
typedef void (*FPtr_officesdk_writer_page_margins)(Writer *writer, const uint32_t left, const uint32_t right, const uint32_t top, const uint32_t bottom, const uint32_t gutter, sdkres_t *err);
typedef void (*FPtr_officesdk_writer_font_family)(Writer *writer, const textspace_t space, const char_t *font_family, sdkres_t *err);
typedef void (*FPtr_officesdk_writer_font_size)(Writer *writer, const textspace_t space, const real32_t font_size, sdkres_t *err);
typedef void (*FPtr_officesdk_writer_bold)(Writer *writer, const textspace_t space, const bool_t bold, sdkres_t *err);
typedef void (*FPtr_officesdk_writer_italic)(Writer *writer, const textspace_t space, const bool_t italic, sdkres_t *err);
typedef void (*FPtr_officesdk_writer_paragraph_halign)(Writer *writer, const textspace_t space, const halign_t align, sdkres_t *err);
typedef void (*FPtr_officesdk_writer_paragraph_lspacing)(Writer *writer, const textspace_t space, const uint32_t height, sdkres_t *err);
typedef void (*FPtr_officesdk_writer_insert_text)(Writer *writer, const textspace_t space, const char_t *text, sdkres_t *err);
typedef void (*FPtr_officesdk_writer_insert_image)(Writer *writer, const textspace_t space, const anchortype_t anchor, const uint32_t width, const uint32_t height, const halign_t halign, const valign_t valign, const char_t *image_path, sdkres_t *err);
typedef void (*FPtr_officesdk_writer_insert_page_number)(Writer *writer, const textspace_t space, sdkres_t *err);
typedef void (*FPtr_officesdk_writer_insert_new_line)(Writer *writer, const textspace_t space, sdkres_t *err);
typedef void (*FPtr_officesdk_writer_insert_paragraph)(Writer *writer, const textspace_t space, sdkres_t *err);
typedef void (*FPtr_officesdk_writer_insert_page_break)(Writer *writer, sdkres_t *err);

/* LibreOffice Calc mapping */
typedef Sheet *(*FPtr_officesdk_sheet_open)(const char_t *pathname, sdkres_t *err);
typedef Sheet *(*FPtr_officesdk_sheet_create)(sdkres_t *err);
typedef void (*FPtr_officesdk_sheet_save)(Sheet *sheet, const char_t *pathname, sdkres_t *err);
typedef void (*FPtr_officesdk_sheet_pdf)(Sheet *sheet, const char_t *pathname, sdkres_t *err);
typedef void (*FPtr_officesdk_sheet_print)(Sheet *sheet, const char_t *filename, const char_t *printer, const paperorient_t orient, const paperformat_t format, const uint32_t paper_width, const uint32_t paper_height, const uint32_t num_copies, const bool_t collate_copies, const char_t *pages, sdkres_t *err);
typedef void (*FPtr_officesdk_sheet_close)(Sheet *sheet, sdkres_t *err);
typedef uint32_t (*FPtr_officesdk_sheet_add)(Sheet *sheet, sdkres_t *err);
typedef void (*FPtr_officesdk_sheet_name)(Sheet *sheet, const uint32_t page, const char_t *name, sdkres_t *err);
typedef void (*FPtr_officesdk_sheet_protect)(Sheet *sheet, const uint32_t page, const bool_t protect, const char_t *pass, sdkres_t *err);
typedef void (*FPtr_officesdk_sheet_freeze)(Sheet *sheet, const uint32_t page, const uint32_t ncols, const uint32_t nrows, sdkres_t *err);
typedef String *(FPtr_officesdk_sheet_cell_ref)(Sheet *sheet, const uint32_t page, const uint32_t col, const uint32_t row, sdkres_t *err);
typedef void (*FPtr_officesdk_sheet_cell_text)(Sheet *sheet, const uint32_t page, const uint32_t col, const uint32_t row, const char_t *text, sdkres_t *err);
typedef void (*FPtr_officesdk_sheet_cell_value)(Sheet *sheet, const uint32_t page, const uint32_t col, const uint32_t row, const real64_t value, sdkres_t *err);
typedef void (*FPtr_officesdk_sheet_cell_date)(Sheet *sheet, const uint32_t page, const uint32_t col, const uint32_t row, const uint8_t day, const uint8_t month, const int16_t year, sdkres_t *err);
typedef void (*FPtr_officesdk_sheet_cell_formula)(Sheet *sheet, const uint32_t page, const uint32_t col, const uint32_t row, const char_t *formula, sdkres_t *err);
typedef void (*FPtr_officesdk_sheet_cell_numformat)(Sheet *sheet, const uint32_t page, const uint32_t col, const uint32_t row, const numformat_t format, sdkres_t *err);
typedef void (*FPtr_officesdk_sheet_cell_font_family)(Sheet *sheet, const uint32_t page, const uint32_t col, const uint32_t row, const char_t *font_family, sdkres_t *err);
typedef void (*FPtr_officesdk_sheet_cell_font_size)(Sheet *sheet, const uint32_t page, const uint32_t col, const uint32_t row, const real32_t font_size, sdkres_t *err);
typedef void (*FPtr_officesdk_sheet_cell_bold)(Sheet *sheet, const uint32_t page, const uint32_t col, const uint32_t row, const bool_t bold, sdkres_t *err);
typedef void (*FPtr_officesdk_sheet_cell_italic)(Sheet *sheet, const uint32_t page, const uint32_t col, const uint32_t row, const bool_t italic, sdkres_t *err);
typedef void (*FPtr_officesdk_sheet_cell_halign)(Sheet *sheet, const uint32_t page, const uint32_t col, const uint32_t row, const halign_t align, sdkres_t *err);
typedef void (*FPtr_officesdk_sheet_cell_valign)(Sheet *sheet, const uint32_t page, const uint32_t col, const uint32_t row, const valign_t align, sdkres_t *err);
typedef void (*FPtr_officesdk_sheet_cell_wrap)(Sheet *sheet, const uint32_t page, const uint32_t col, const uint32_t row, const bool_t wrapped, sdkres_t *err);
typedef void (*FPtr_officesdk_sheet_cell_color)(Sheet *sheet, const uint32_t page, const uint32_t col, const uint32_t row, const uint32_t rgb, sdkres_t *err);
typedef void (*FPtr_officesdk_sheet_cell_backcolor)(Sheet *sheet, const uint32_t page, const uint32_t col, const uint32_t row, const uint32_t rgb, sdkres_t *err);
typedef void (*FPtr_officesdk_sheet_cells_backcolor)(Sheet *sheet, const uint32_t page, const uint32_t st_col, const uint32_t st_row, const uint32_t ed_col, const uint32_t ed_row, const uint32_t rgb, sdkres_t *err);
typedef void (*FPtr_officesdk_sheet_cell_image)(Sheet *sheet, const uint32_t page, const uint32_t col, const uint32_t row, const char_t *image_path, sdkres_t *err);
typedef void (*FPtr_officesdk_sheet_cell_border)(Sheet *sheet, const uint32_t page, const uint32_t col, const uint32_t row, const linestyle_t style, const uint32_t thickness, const uint32_t rgb, sdkres_t *err);
typedef void (*FPtr_officesdk_sheet_cells_border)(Sheet *sheet, const uint32_t page, const uint32_t st_col, const uint32_t st_row, const uint32_t ed_col, const uint32_t ed_row, const linestyle_t style, const uint32_t thickness, const uint32_t rgb, sdkres_t *err);
typedef void (*FPtr_officesdk_sheet_cells_merge)(Sheet *sheet, const uint32_t page, const uint32_t st_col, const uint32_t st_row, const uint32_t ed_col, const uint32_t ed_row, sdkres_t *err);
typedef void (*FPtr_officesdk_sheet_column_visible)(Sheet *sheet, const uint32_t page, const uint32_t col, const bool_t visible, sdkres_t *err);
typedef void (*FPtr_officesdk_sheet_column_optimal_width)(Sheet *sheet, const uint32_t page, const uint32_t col, const bool_t optimal_width, sdkres_t *err);
typedef void (*FPtr_officesdk_sheet_column_width)(Sheet *sheet, const uint32_t page, const uint32_t col, const uint32_t width, sdkres_t *err);
typedef void (*FPtr_officesdk_sheet_row_visible)(Sheet *sheet, const uint32_t page, const uint32_t row, const bool_t visible, sdkres_t *err);
typedef void (*FPtr_officesdk_sheet_row_visible)(Sheet *sheet, const uint32_t page, const uint32_t row, const bool_t visible, sdkres_t *err);
typedef void (*FPtr_officesdk_sheet_row_optimal_height)(Sheet *sheet, const uint32_t page, const uint32_t row, const bool_t optimal_height, sdkres_t *err);
typedef void (*FPtr_officesdk_sheet_row_height)(Sheet *sheet, const uint32_t page, const uint32_t row, const uint32_t height, sdkres_t *err);

struct _hbofficefuncs_t
{
    FPtr_officesdk_finish func_officesdk_finish;
    FPtr_officesdk_error func_officesdk_error;
    FPtr_officesdk_browse_doc func_officesdk_browse_doc;
    FPtr_officesdk_rgb func_officesdk_rgb;
    FPtr_officesdk_writer_open func_officesdk_writer_open;
    FPtr_officesdk_writer_create func_officesdk_writer_create;
    FPtr_officesdk_writer_save func_officesdk_writer_save;
    FPtr_officesdk_writer_pdf func_officesdk_writer_pdf;
    FPtr_officesdk_writer_print func_officesdk_writer_print;
    FPtr_officesdk_writer_close func_officesdk_writer_close;
    FPtr_officesdk_writer_page_header_show func_officesdk_writer_page_header_show;
    FPtr_officesdk_writer_page_header_margins func_officesdk_writer_page_header_margins;
    FPtr_officesdk_writer_page_footer_show func_officesdk_writer_page_footer_show;
    FPtr_officesdk_writer_page_footer_margins func_officesdk_writer_page_footer_margins;
    FPtr_officesdk_writer_page_margins func_officesdk_writer_page_margins;
    FPtr_officesdk_writer_font_family func_officesdk_writer_font_family;
    FPtr_officesdk_writer_font_size func_officesdk_writer_font_size;
    FPtr_officesdk_writer_bold func_officesdk_writer_bold;
    FPtr_officesdk_writer_italic func_officesdk_writer_italic;
    FPtr_officesdk_writer_paragraph_halign func_officesdk_writer_paragraph_halign;
    FPtr_officesdk_writer_paragraph_lspacing func_officesdk_writer_paragraph_lspacing;
    FPtr_officesdk_writer_insert_text func_officesdk_writer_insert_text;
    FPtr_officesdk_writer_insert_image func_officesdk_writer_insert_image;
    FPtr_officesdk_writer_insert_page_number func_officesdk_writer_insert_page_number;
    FPtr_officesdk_writer_insert_new_line func_officesdk_writer_insert_new_line;
    FPtr_officesdk_writer_insert_paragraph func_officesdk_writer_insert_paragraph;
    FPtr_officesdk_writer_insert_page_break func_officesdk_writer_insert_page_break;
    FPtr_officesdk_sheet_open func_officesdk_sheet_open;
    FPtr_officesdk_sheet_create func_officesdk_sheet_create;
    FPtr_officesdk_sheet_save func_officesdk_sheet_save;
    FPtr_officesdk_sheet_pdf func_officesdk_sheet_pdf;
    FPtr_officesdk_sheet_print func_officesdk_sheet_print;
    FPtr_officesdk_sheet_close func_officesdk_sheet_close;
    FPtr_officesdk_sheet_add func_officesdk_sheet_add;
    FPtr_officesdk_sheet_name func_officesdk_sheet_name;
    FPtr_officesdk_sheet_protect func_officesdk_sheet_protect;
    FPtr_officesdk_sheet_freeze func_officesdk_sheet_freeze;
    FPtr_officesdk_sheet_cell_ref func_officesdk_sheet_cell_ref;
    FPtr_officesdk_sheet_cell_text func_officesdk_sheet_cell_text;
    FPtr_officesdk_sheet_cell_value func_officesdk_sheet_cell_value;
    FPtr_officesdk_sheet_cell_date func_officesdk_sheet_cell_date;
    FPtr_officesdk_sheet_cell_formula func_officesdk_sheet_cell_formula;
    FPtr_officesdk_sheet_cell_numformat func_officesdk_sheet_cell_numformat;
    FPtr_officesdk_sheet_cell_font_family func_officesdk_sheet_cell_font_family;
    FPtr_officesdk_sheet_cell_font_size func_officesdk_sheet_cell_font_size;
    FPtr_officesdk_sheet_cell_bold func_officesdk_sheet_cell_bold;
    FPtr_officesdk_sheet_cell_italic func_officesdk_sheet_cell_italic;
    FPtr_officesdk_sheet_cell_halign func_officesdk_sheet_cell_halign;
    FPtr_officesdk_sheet_cell_valign func_officesdk_sheet_cell_valign;
    FPtr_officesdk_sheet_cell_wrap func_officesdk_sheet_cell_wrap;
    FPtr_officesdk_sheet_cell_color func_officesdk_sheet_cell_color;
    FPtr_officesdk_sheet_cell_backcolor func_officesdk_sheet_cell_backcolor;
    FPtr_officesdk_sheet_cells_backcolor func_officesdk_sheet_cells_backcolor;
    FPtr_officesdk_sheet_cell_image func_officesdk_sheet_cell_image;
    FPtr_officesdk_sheet_cell_border func_officesdk_sheet_cell_border;
    FPtr_officesdk_sheet_cells_border func_officesdk_sheet_cells_border;
    FPtr_officesdk_sheet_cells_merge func_officesdk_sheet_cells_merge;
    FPtr_officesdk_sheet_column_visible func_officesdk_sheet_column_visible;
    FPtr_officesdk_sheet_column_optimal_width func_officesdk_sheet_column_optimal_width;
    FPtr_officesdk_sheet_column_width func_officesdk_sheet_column_width;
    FPtr_officesdk_sheet_row_visible func_officesdk_sheet_row_visible;
    FPtr_officesdk_sheet_row_visible func_officesdk_sheet_row_visible;
    FPtr_officesdk_sheet_row_optimal_height func_officesdk_sheet_row_optimal_height;
    FPtr_officesdk_sheet_row_height func_officesdk_sheet_row_height;
};

struct _hboffice_t
{
    bool_t init;
    bool_t with_funcs;
    textspace_t text_space;
    char_t last_cell_ref[i_CELL_REF_SIZE];
    sdkres_t last_error;
    HBOfficeFuncs funcs;
};

/*---------------------------------------------------------------------------*/

static HBOffice HBOFFICE_GLOBAL = {.init = FALSE};
// #define STATIC_TEXT_SIZE 1024
// char_t TEMP_BUFFER[STATIC_TEXT_SIZE];

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
    s2 = hb_itemCopyStrUTF8(item, tcc(str), s1 + 1);
    i_remove_utf8_CR(str);
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
        cassert(HB_ITEM_TYPE(item) == HB_IT_BLOCK);
        str = i_item_to_utf8_string(ritem);
        hb_itemRelease(ritem);
    }
    else
    {
        cassert_msg(FALSE, "Unknown block type");
        str = str_c("");
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

static bool_t i_check_funcs(const HBOfficeFuncs *funcs)
{
    uint32_t i, n = sizeof(HBOfficeFuncs) / sizeof(FPtr_libproc);
    const byte_t *proc = cast(funcs, void);
    for (i = 0; i < n; ++i, proc += sizeof(void (*)()))
    {
        if (cast_func_ptr(proc, FPtr_libproc) == NULL)
            return FALSE;
    }

    return TRUE;
}

/*---------------------------------------------------------------------------*/

static bool_t i_init_funcs(HBOfficeFuncs *funcs)
{
    DLib *lib = hbdlib_open(NULL, "officesdk");
    if (lib != NULL)
    {
        funcs->func_officesdk_finish = hbdlib_proc(lib, "officesdk_finish", FPtr_officesdk_finish);
        funcs->func_officesdk_error = hbdlib_proc(lib, "officesdk_error", FPtr_officesdk_error);
        funcs->func_officesdk_browse_doc = hbdlib_proc(lib, "officesdk_browse_doc", FPtr_officesdk_browse_doc);
        funcs->func_officesdk_rgb = hbdlib_proc(lib, "officesdk_rgb", FPtr_officesdk_rgb);
        funcs->func_officesdk_writer_open = hbdlib_proc(lib, "officesdk_writer_open", FPtr_officesdk_writer_open);
        funcs->func_officesdk_writer_create = hbdlib_proc(lib, "officesdk_writer_create", FPtr_officesdk_writer_create);
        funcs->func_officesdk_writer_save = hbdlib_proc(lib, "officesdk_writer_save", FPtr_officesdk_writer_save);
        funcs->func_officesdk_writer_pdf = hbdlib_proc(lib, "officesdk_writer_pdf", FPtr_officesdk_writer_pdf);
        funcs->func_officesdk_writer_print = hbdlib_proc(lib, "officesdk_writer_print", FPtr_officesdk_writer_print);
        funcs->func_officesdk_writer_close = hbdlib_proc(lib, "officesdk_writer_close", FPtr_officesdk_writer_close);
        funcs->func_officesdk_writer_page_header_show = hbdlib_proc(lib, "officesdk_writer_page_header_show", FPtr_officesdk_writer_page_header_show);
        funcs->func_officesdk_writer_page_header_margins = hbdlib_proc(lib, "officesdk_writer_page_header_margins", FPtr_officesdk_writer_page_header_margins);
        funcs->func_officesdk_writer_page_footer_show = hbdlib_proc(lib, "officesdk_writer_page_footer_show", FPtr_officesdk_writer_page_footer_show);
        funcs->func_officesdk_writer_page_footer_margins = hbdlib_proc(lib, "officesdk_writer_page_footer_margins", FPtr_officesdk_writer_page_footer_margins);
        funcs->func_officesdk_writer_page_margins = hbdlib_proc(lib, "officesdk_writer_page_margins", FPtr_officesdk_writer_page_margins);
        funcs->func_officesdk_writer_font_family = hbdlib_proc(lib, "officesdk_writer_font_family", FPtr_officesdk_writer_font_family);
        funcs->func_officesdk_writer_font_size = hbdlib_proc(lib, "officesdk_writer_font_size", FPtr_officesdk_writer_font_size);
        funcs->func_officesdk_writer_bold = hbdlib_proc(lib, "officesdk_writer_bold", FPtr_officesdk_writer_bold);
        funcs->func_officesdk_writer_italic = hbdlib_proc(lib, "officesdk_writer_italic", FPtr_officesdk_writer_italic);
        funcs->func_officesdk_writer_paragraph_halign = hbdlib_proc(lib, "officesdk_writer_paragraph_halign", FPtr_officesdk_writer_paragraph_halign);
        funcs->func_officesdk_writer_paragraph_lspacing = hbdlib_proc(lib, "officesdk_writer_paragraph_lspacing", FPtr_officesdk_writer_paragraph_lspacing);
        funcs->func_officesdk_writer_insert_text = hbdlib_proc(lib, "officesdk_writer_insert_text", FPtr_officesdk_writer_insert_text);
        funcs->func_officesdk_writer_insert_image = hbdlib_proc(lib, "officesdk_writer_insert_image", FPtr_officesdk_writer_insert_image);
        funcs->func_officesdk_writer_insert_page_number = hbdlib_proc(lib, "officesdk_writer_insert_page_number", FPtr_officesdk_writer_insert_page_number);
        funcs->func_officesdk_writer_insert_new_line = hbdlib_proc(lib, "officesdk_writer_insert_new_line", FPtr_officesdk_writer_insert_new_line);
        funcs->func_officesdk_writer_insert_paragraph = hbdlib_proc(lib, "officesdk_writer_insert_paragraph", FPtr_officesdk_writer_insert_paragraph);
        funcs->func_officesdk_writer_insert_page_break = hbdlib_proc(lib, "officesdk_writer_insert_page_break", FPtr_officesdk_writer_insert_page_break);
        funcs->func_officesdk_sheet_open = hbdlib_proc(lib, "officesdk_sheet_open", FPtr_officesdk_sheet_open);
        funcs->func_officesdk_sheet_create = hbdlib_proc(lib, "officesdk_sheet_create", FPtr_officesdk_sheet_create);
        funcs->func_officesdk_sheet_save = hbdlib_proc(lib, "officesdk_sheet_save", FPtr_officesdk_sheet_save);
        funcs->func_officesdk_sheet_pdf = hbdlib_proc(lib, "officesdk_sheet_pdf", FPtr_officesdk_sheet_pdf);
        funcs->func_officesdk_sheet_print = hbdlib_proc(lib, "officesdk_sheet_print", FPtr_officesdk_sheet_print);
        funcs->func_officesdk_sheet_close = hbdlib_proc(lib, "officesdk_sheet_close", FPtr_officesdk_sheet_close);
        funcs->func_officesdk_sheet_add = hbdlib_proc(lib, "officesdk_sheet_add", FPtr_officesdk_sheet_add);
        funcs->func_officesdk_sheet_name = hbdlib_proc(lib, "officesdk_sheet_name", FPtr_officesdk_sheet_name);
        funcs->func_officesdk_sheet_protect = hbdlib_proc(lib, "officesdk_sheet_protect", FPtr_officesdk_sheet_protect);
        funcs->func_officesdk_sheet_freeze = hbdlib_proc(lib, "officesdk_sheet_freeze", FPtr_officesdk_sheet_freeze);
        funcs->func_officesdk_sheet_cell_ref = hbdlib_proc(lib, "officesdk_sheet_cell_ref", FPtr_officesdk_sheet_cell_ref);
        funcs->func_officesdk_sheet_cell_text = hbdlib_proc(lib, "officesdk_sheet_cell_text", FPtr_officesdk_sheet_cell_text);
        funcs->func_officesdk_sheet_cell_value = hbdlib_proc(lib, "officesdk_sheet_cell_value", FPtr_officesdk_sheet_cell_value);
        funcs->func_officesdk_sheet_cell_date = hbdlib_proc(lib, "officesdk_sheet_cell_date", FPtr_officesdk_sheet_cell_date);
        funcs->func_officesdk_sheet_cell_formula = hbdlib_proc(lib, "officesdk_sheet_cell_formula", FPtr_officesdk_sheet_cell_formula);
        funcs->func_officesdk_sheet_cell_numformat = hbdlib_proc(lib, "officesdk_sheet_cell_numformat", FPtr_officesdk_sheet_cell_numformat);
        funcs->func_officesdk_sheet_cell_font_family = hbdlib_proc(lib, "officesdk_sheet_cell_font_family", FPtr_officesdk_sheet_cell_font_family);
        funcs->func_officesdk_sheet_cell_font_size = hbdlib_proc(lib, "officesdk_sheet_cell_font_size", FPtr_officesdk_sheet_cell_font_size);
        funcs->func_officesdk_sheet_cell_bold = hbdlib_proc(lib, "officesdk_sheet_cell_bold", FPtr_officesdk_sheet_cell_bold);
        funcs->func_officesdk_sheet_cell_italic = hbdlib_proc(lib, "officesdk_sheet_cell_italic", FPtr_officesdk_sheet_cell_italic);
        funcs->func_officesdk_sheet_cell_halign = hbdlib_proc(lib, "officesdk_sheet_cell_halign", FPtr_officesdk_sheet_cell_halign);
        funcs->func_officesdk_sheet_cell_valign = hbdlib_proc(lib, "officesdk_sheet_cell_valign", FPtr_officesdk_sheet_cell_valign);
        funcs->func_officesdk_sheet_cell_wrap = hbdlib_proc(lib, "officesdk_sheet_cell_wrap", FPtr_officesdk_sheet_cell_wrap);
        funcs->func_officesdk_sheet_cell_color = hbdlib_proc(lib, "officesdk_sheet_cell_color", FPtr_officesdk_sheet_cell_color);
        funcs->func_officesdk_sheet_cell_backcolor = hbdlib_proc(lib, "officesdk_sheet_cell_backcolor", FPtr_officesdk_sheet_cell_backcolor);
        funcs->func_officesdk_sheet_cells_backcolor = hbdlib_proc(lib, "officesdk_sheet_cells_backcolor", FPtr_officesdk_sheet_cells_backcolor);
        funcs->func_officesdk_sheet_cell_image = hbdlib_proc(lib, "officesdk_sheet_cell_image", FPtr_officesdk_sheet_cell_image);
        funcs->func_officesdk_sheet_cell_border = hbdlib_proc(lib, "officesdk_sheet_cell_border", FPtr_officesdk_sheet_cell_border);
        funcs->func_officesdk_sheet_cells_border = hbdlib_proc(lib, "officesdk_sheet_cells_border", FPtr_officesdk_sheet_cells_border);
        funcs->func_officesdk_sheet_cells_merge = hbdlib_proc(lib, "officesdk_sheet_cells_merge", FPtr_officesdk_sheet_cells_merge);
        funcs->func_officesdk_sheet_column_visible = hbdlib_proc(lib, "officesdk_sheet_column_visible", FPtr_officesdk_sheet_column_visible);
        funcs->func_officesdk_sheet_column_optimal_width = hbdlib_proc(lib, "officesdk_sheet_column_optimal_width", FPtr_officesdk_sheet_column_optimal_width);
        funcs->func_officesdk_sheet_column_width = hbdlib_proc(lib, "officesdk_sheet_column_width", FPtr_officesdk_sheet_column_width);
        funcs->func_officesdk_sheet_row_visible = hbdlib_proc(lib, "officesdk_sheet_row_visible", FPtr_officesdk_sheet_row_visible);
        funcs->func_officesdk_sheet_row_visible = hbdlib_proc(lib, "officesdk_sheet_row_visible", FPtr_officesdk_sheet_row_visible);
        funcs->func_officesdk_sheet_row_optimal_height = hbdlib_proc(lib, "officesdk_sheet_row_optimal_height", FPtr_officesdk_sheet_row_optimal_height);
        funcs->func_officesdk_sheet_row_height = hbdlib_proc(lib, "officesdk_sheet_row_height", FPtr_officesdk_sheet_row_height);
        hbdlib_close(&lib);
        return TRUE;
    }
    else
    {
        memset(cast(funcs, void), 0, sizeof(HBOfficeFuncs));
        return FALSE;
    }
}

/*---------------------------------------------------------------------------*/

bool_t hb_office_init(void)
{
    if (HBOFFICE_GLOBAL.init == FALSE)
    {
        HBOFFICE_GLOBAL.text_space = ekTEXT_SPACE_PAGE;
        HBOFFICE_GLOBAL.last_cell_ref[0] = '\0';
        HBOFFICE_GLOBAL.last_error = ENUM_MAX(sdkres_t);
        HBOFFICE_GLOBAL.with_funcs = i_init_funcs(&HBOFFICE_GLOBAL.funcs);
        if (HBOFFICE_GLOBAL.with_funcs == TRUE)
            HBOFFICE_GLOBAL.init = i_check_funcs(&HBOFFICE_GLOBAL.funcs);
        else
            HBOFFICE_GLOBAL.init = TRUE;
    }

    return HBOFFICE_GLOBAL.init;
}

/*---------------------------------------------------------------------------*/

void hb_office_finish(void)
{
    if (HBOFFICE_GLOBAL.init == TRUE)
    {
        if (HBOFFICE_GLOBAL.funcs.func_officesdk_finish != NULL)
            HBOFFICE_GLOBAL.funcs.func_officesdk_finish();

        HBOFFICE_GLOBAL.init == FALSE;
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

const char_t *hb_office_error(const uint32_t errcode)
{
    if (HBOFFICE_GLOBAL.init && HBOFFICE_GLOBAL.with_funcs)
        return HBOFFICE_GLOBAL.funcs.func_officesdk_error((sdkres_t)errcode);
    return "";
}

/*---------------------------------------------------------------------------*/

void hb_office_browse_doc(HB_ITEM *pathname_block)
{
    if (HBOFFICE_GLOBAL.init && HBOFFICE_GLOBAL.with_funcs)
    {
        char_t *pathname = i_block_to_utf8(pathname_block);
        HBOFFICE_GLOBAL.funcs.func_officesdk_browse_doc(pathname, &HBOFFICE_GLOBAL.last_error);
        i_destroy_utf8(&pathname);
    }
}

/*---------------------------------------------------------------------------*/

uint32_t hb_office_rgb(const uint8_t red, const uint8_t green, const uint8_t blue)
{
    if (HBOFFICE_GLOBAL.init && HBOFFICE_GLOBAL.with_funcs)
        return HBOFFICE_GLOBAL.funcs.func_officesdk_rgb(red, green, blue);
    return 0;
}

/*---------------------------------------------------------------------------*/

Sheet *hb_office_sheet_open(HB_ITEM *pathname_block)
{
    if (HBOFFICE_GLOBAL.init && HBOFFICE_GLOBAL.with_funcs)
    {
        char_t *pathname = i_block_to_utf8(pathname_block);
        Sheet *sheet = HBOFFICE_GLOBAL.funcs.func_officesdk_sheet_open(tc(pathname), &GTNAP_GLOBAL->office_last_error);
        str_destroy(&pathname);
        return sheet;
    }
}

/*---------------------------------------------------------------------------*/

Sheet *hb_office_sheet_create(void)
{
    return HBOFFICE_GLOBAL.funcs.func_officesdk_sheet_create(&GTNAP_GLOBAL->office_last_error);
}

/*---------------------------------------------------------------------------*/

void hb_office_sheet_save(Sheet *sheet, HB_ITEM *pathname_block)
{
    String *pathname = hb_block_to_utf8(pathname_block);
    HBOFFICE_GLOBAL.funcs.func_officesdk_sheet_save(sheet, tc(pathname), &GTNAP_GLOBAL->office_last_error);
    str_destroy(&pathname);
}

/*---------------------------------------------------------------------------*/

void hb_office_sheet_pdf(Sheet *sheet, HB_ITEM *pathname_block)
{
    String *pathname = hb_block_to_utf8(pathname_block);
    HBOFFICE_GLOBAL.funcs.func_officesdk_sheet_pdf(sheet, tc(pathname), &GTNAP_GLOBAL->office_last_error);
    str_destroy(&pathname);
}

/*---------------------------------------------------------------------------*/

void hb_office_sheet_print(Sheet *sheet, HB_ITEM *filename_block, HB_ITEM *printer_block, const paperorient_t orient, const paperformat_t format, const uint32_t paper_width, const uint32_t paper_height, const uint32_t num_copies, const bool_t collate_copies, HB_ITEM *pages_block)
{
    String *filename = hb_block_to_utf8(filename_block);
    String *printer = hb_block_to_utf8(printer_block);
    String *pages = hb_block_to_utf8(pages_block);
    HBOFFICE_GLOBAL.funcs.func_officesdk_sheet_print(sheet, tc(filename), tc(printer), orient, format, paper_width, paper_height, num_copies, collate_copies, tc(pages), &GTNAP_GLOBAL->office_last_error);
    str_destroy(&filename);
    str_destroy(&printer);
    str_destroy(&pages);
}

/*---------------------------------------------------------------------------*/

void hb_office_sheet_close(Sheet *sheet)
{
    HBOFFICE_GLOBAL.funcs.func_officesdk_sheet_close(sheet, &GTNAP_GLOBAL->office_last_error);
}

/*---------------------------------------------------------------------------*/

uint32_t hb_office_sheet_add(Sheet *sheet)
{
    return HBOFFICE_GLOBAL.funcs.func_officesdk_sheet_add(sheet, &GTNAP_GLOBAL->office_last_error);
}

/*---------------------------------------------------------------------------*/

void hb_office_sheet_name(Sheet *sheet, const uint32_t page, HB_ITEM *name_block)
{
    String *name = hb_block_to_utf8(name_block);
    HBOFFICE_GLOBAL.funcs.func_officesdk_sheet_name(sheet, page, tc(name), &GTNAP_GLOBAL->office_last_error);
    str_destroy(&name);
}

/*---------------------------------------------------------------------------*/

void hb_office_sheet_protect(Sheet *sheet, const uint32_t page, const bool_t protect, HB_ITEM *pass_block)
{
    String *pass = hb_block_to_utf8(pass_block);
    HBOFFICE_GLOBAL.funcs.func_officesdk_sheet_protect(sheet, page, protect, tc(pass), &GTNAP_GLOBAL->office_last_error);
    str_destroy(&pass);
}

/*---------------------------------------------------------------------------*/

void hb_office_sheet_freeze(Sheet *sheet, const uint32_t page, const uint32_t ncols, const uint32_t nrows)
{
    HBOFFICE_GLOBAL.funcs.func_officesdk_sheet_freeze(sheet, page, ncols, nrows, &GTNAP_GLOBAL->office_last_error);
}

/*---------------------------------------------------------------------------*/

const char_t *hb_office_cell_ref(Sheet *sheet, const uint32_t page, const uint32_t col, const uint32_t row)
{
    str_destopt(&GTNAP_GLOBAL->office_last_cell_ref);
    GTNAP_GLOBAL->office_last_cell_ref = HBOFFICE_GLOBAL.funcs.func_officesdk_sheet_cell_ref(sheet, page, col, row, &GTNAP_GLOBAL->office_last_error);
    return tc(GTNAP_GLOBAL->office_last_cell_ref);
}

/*---------------------------------------------------------------------------*/

void hb_office_sheet_cell_text(Sheet *sheet, const uint32_t page, const uint32_t col, const uint32_t row, HB_ITEM *text_block)
{
    String *text = hb_block_to_utf8(text_block);
    HBOFFICE_GLOBAL.funcs.func_officesdk_sheet_cell_text(sheet, page, col, row, tc(text), &GTNAP_GLOBAL->office_last_error);
    str_destroy(&text);
}

/*---------------------------------------------------------------------------*/

void hb_office_sheet_cell_value(Sheet *sheet, const uint32_t page, const uint32_t col, const uint32_t row, const real64_t value)
{
    HBOFFICE_GLOBAL.funcs.func_officesdk_sheet_cell_value(sheet, page, col, row, value, &GTNAP_GLOBAL->office_last_error);
}

/*---------------------------------------------------------------------------*/

void hb_office_sheet_cell_date(Sheet *sheet, const uint32_t page, const uint32_t col, const uint32_t row, const uint8_t day, const uint8_t month, const int16_t year)
{
    HBOFFICE_GLOBAL.funcs.func_officesdk_sheet_cell_date(sheet, page, col, row, day, month, year, &GTNAP_GLOBAL->office_last_error);
}

/*---------------------------------------------------------------------------*/

void hb_office_sheet_cell_formula(Sheet *sheet, const uint32_t page, const uint32_t col, const uint32_t row, HB_ITEM *formula_block)
{
    String *formula = hb_block_to_utf8(formula_block);
    HBOFFICE_GLOBAL.funcs.func_officesdk_sheet_cell_formula(sheet, page, col, row, tc(formula), &GTNAP_GLOBAL->office_last_error);
    str_destroy(&formula);
}

/*---------------------------------------------------------------------------*/

void hb_office_sheet_cell_numformat(Sheet *sheet, const uint32_t page, const uint32_t col, const uint32_t row, const numformat_t format)
{
    HBOFFICE_GLOBAL.funcs.func_officesdk_sheet_cell_numformat(sheet, page, col, row, format, &GTNAP_GLOBAL->office_last_error);
}

/*---------------------------------------------------------------------------*/

void hb_office_sheet_cell_font_family(Sheet *sheet, const uint32_t page, const uint32_t col, const uint32_t row, HB_ITEM *ffamily_block)
{
    String *ffamily = hb_block_to_utf8(ffamily_block);
    HBOFFICE_GLOBAL.funcs.func_officesdk_sheet_cell_font_family(sheet, page, col, row, tc(ffamily), &GTNAP_GLOBAL->office_last_error);
    str_destroy(&ffamily);
}

/*---------------------------------------------------------------------------*/

void hb_office_sheet_cell_font_size(Sheet *sheet, const uint32_t page, const uint32_t col, const uint32_t row, const real32_t fsize)
{
    HBOFFICE_GLOBAL.funcs.func_officesdk_sheet_cell_font_size(sheet, page, col, row, fsize, &GTNAP_GLOBAL->office_last_error);
}

/*---------------------------------------------------------------------------*/

void hb_office_sheet_cell_bold(Sheet *sheet, const uint32_t page, const uint32_t col, const uint32_t row, const bool_t bold)
{
    HBOFFICE_GLOBAL.funcs.func_officesdk_sheet_cell_bold(sheet, page, col, row, bold, &GTNAP_GLOBAL->office_last_error);
}

/*---------------------------------------------------------------------------*/

void hb_office_sheet_cell_italic(Sheet *sheet, const uint32_t page, const uint32_t col, const uint32_t row, const bool_t italic)
{
    HBOFFICE_GLOBAL.funcs.func_officesdk_sheet_cell_italic(sheet, page, col, row, italic, &GTNAP_GLOBAL->office_last_error);
}

/*---------------------------------------------------------------------------*/

void hb_office_sheet_cell_halign(Sheet *sheet, const uint32_t page, const uint32_t col, const uint32_t row, const halign_t align)
{
    HBOFFICE_GLOBAL.funcs.func_officesdk_sheet_cell_halign(sheet, page, col, row, align, &GTNAP_GLOBAL->office_last_error);
}

/*---------------------------------------------------------------------------*/

void hb_office_sheet_cell_valign(Sheet *sheet, const uint32_t page, const uint32_t col, const uint32_t row, const valign_t align)
{
    HBOFFICE_GLOBAL.funcs.func_officesdk_sheet_cell_valign(sheet, page, col, row, align, &GTNAP_GLOBAL->office_last_error);
}

/*---------------------------------------------------------------------------*/

void hb_office_sheet_cell_wrap(Sheet *sheet, const uint32_t page, const uint32_t col, const uint32_t row, const bool_t wrapped)
{
    HBOFFICE_GLOBAL.funcs.func_officesdk_sheet_cell_wrap(sheet, page, col, row, wrapped, &GTNAP_GLOBAL->office_last_error);
}

/*---------------------------------------------------------------------------*/

void hb_office_sheet_cell_color(Sheet *sheet, const uint32_t page, const uint32_t col, const uint32_t row, const uint32_t rgb)
{
    HBOFFICE_GLOBAL.funcs.func_officesdk_sheet_cell_color(sheet, page, col, row, rgb, &GTNAP_GLOBAL->office_last_error);
}

/*---------------------------------------------------------------------------*/

void hb_office_sheet_cell_backcolor(Sheet *sheet, const uint32_t page, const uint32_t col, const uint32_t row, const uint32_t rgb)
{
    HBOFFICE_GLOBAL.funcs.func_officesdk_sheet_cell_backcolor(sheet, page, col, row, rgb, &GTNAP_GLOBAL->office_last_error);
}

/*---------------------------------------------------------------------------*/

void hb_office_sheet_cells_backcolor(Sheet *sheet, const uint32_t page, const uint32_t st_col, const uint32_t st_row, const uint32_t ed_col, const uint32_t ed_row, const uint32_t rgb)
{
    HBOFFICE_GLOBAL.funcs.func_officesdk_sheet_cells_backcolor(sheet, page, st_col, st_row, ed_col, ed_row, rgb, &GTNAP_GLOBAL->office_last_error);
}

/*---------------------------------------------------------------------------*/

void hb_office_sheet_cell_image(Sheet *sheet, const uint32_t page, const uint32_t col, const uint32_t row, HB_ITEM *image_path_block)
{
    String *image_path = hb_block_to_utf8(image_path_block);
    HBOFFICE_GLOBAL.funcs.func_officesdk_sheet_cell_image(sheet, page, col, row, tc(image_path), &GTNAP_GLOBAL->office_last_error);
    str_destroy(&image_path);
}

/*---------------------------------------------------------------------------*/

void hb_office_sheet_cell_border(Sheet *sheet, const uint32_t page, const uint32_t col, const uint32_t row, const linestyle_t style, const uint32_t thickness, const uint32_t rgb)
{
    HBOFFICE_GLOBAL.funcs.func_officesdk_sheet_cell_border(sheet, page, col, row, style, thickness, rgb, &GTNAP_GLOBAL->office_last_error);
}

/*---------------------------------------------------------------------------*/

void hb_office_sheet_cells_border(Sheet *sheet, const uint32_t page, const uint32_t st_col, const uint32_t st_row, const uint32_t ed_col, const uint32_t ed_row, const linestyle_t style, const uint32_t thickness, const uint32_t rgb)
{
    HBOFFICE_GLOBAL.funcs.func_officesdk_sheet_cells_border(sheet, page, st_col, st_row, ed_col, ed_row, style, thickness, rgb, &GTNAP_GLOBAL->office_last_error);
}

/*---------------------------------------------------------------------------*/

void hb_office_sheet_cells_merge(Sheet *sheet, const uint32_t page, const uint32_t st_col, const uint32_t st_row, const uint32_t ed_col, const uint32_t ed_row)
{
    HBOFFICE_GLOBAL.funcs.func_officesdk_sheet_cells_merge(sheet, page, st_col, st_row, ed_col, ed_row, &GTNAP_GLOBAL->office_last_error);
}

/*---------------------------------------------------------------------------*/

void hb_office_sheet_column_visible(Sheet *sheet, const uint32_t page, const uint32_t col, const bool_t visible)
{
    HBOFFICE_GLOBAL.funcs.func_officesdk_sheet_column_visible(sheet, page, col, visible, &GTNAP_GLOBAL->office_last_error);
}

/*---------------------------------------------------------------------------*/

void hb_office_sheet_column_optimal_width(Sheet *sheet, const uint32_t page, const uint32_t col, const bool_t optimal_width)
{
    HBOFFICE_GLOBAL.funcs.func_officesdk_sheet_column_optimal_width(sheet, page, col, optimal_width, &GTNAP_GLOBAL->office_last_error);
}

/*---------------------------------------------------------------------------*/

void hb_office_sheet_column_width(Sheet *sheet, const uint32_t page, const uint32_t col, const uint32_t width)
{
    HBOFFICE_GLOBAL.funcs.func_officesdk_sheet_column_width(sheet, page, col, width, &GTNAP_GLOBAL->office_last_error);
}

/*---------------------------------------------------------------------------*/

void hb_office_sheet_row_visible(Sheet *sheet, const uint32_t page, const uint32_t row, const bool_t visible)
{
    HBOFFICE_GLOBAL.funcs.func_officesdk_sheet_row_visible(sheet, page, row, visible, &GTNAP_GLOBAL->office_last_error);
}

/*---------------------------------------------------------------------------*/

void hb_office_sheet_row_optimal_height(Sheet *sheet, const uint32_t page, const uint32_t row, const bool_t optimal_height)
{
    HBOFFICE_GLOBAL.funcs.func_officesdk_sheet_row_optimal_height(sheet, page, row, optimal_height, &GTNAP_GLOBAL->office_last_error);
}

/*---------------------------------------------------------------------------*/

void hb_office_sheet_row_height(Sheet *sheet, const uint32_t page, const uint32_t row, const uint32_t height)
{
    HBOFFICE_GLOBAL.funcs.func_officesdk_sheet_row_height(sheet, page, row, height, &GTNAP_GLOBAL->office_last_error);
}

/*---------------------------------------------------------------------------*/

Writer *hb_office_writer_open(HB_ITEM *pathname_block)
{
    String *pathname = hb_block_to_utf8(pathname_block);
    Writer *writer = HBOFFICE_GLOBAL.funcs.func_officesdk_writer_open(tc(pathname), &GTNAP_GLOBAL->office_last_error);
    str_destroy(&pathname);
    return writer;
}

/*---------------------------------------------------------------------------*/

Writer *hb_office_writer_create(void)
{
    return HBOFFICE_GLOBAL.funcs.func_officesdk_writer_create(&GTNAP_GLOBAL->office_last_error);
}

/*---------------------------------------------------------------------------*/

void hb_office_writer_save(Writer *writer, HB_ITEM *pathname_block)
{
    String *pathname = hb_block_to_utf8(pathname_block);
    HBOFFICE_GLOBAL.funcs.func_officesdk_writer_save(writer, tc(pathname), &GTNAP_GLOBAL->office_last_error);
    str_destroy(&pathname);
}

/*---------------------------------------------------------------------------*/

void hb_office_writer_pdf(Writer *writer, HB_ITEM *pathname_block)
{
    String *pathname = hb_block_to_utf8(pathname_block);
    HBOFFICE_GLOBAL.funcs.func_officesdk_writer_pdf(writer, tc(pathname), &GTNAP_GLOBAL->office_last_error);
    str_destroy(&pathname);
}

/*---------------------------------------------------------------------------*/

void hb_office_writer_print(Writer *writer, HB_ITEM *filename_block, HB_ITEM *printer_block, const paperorient_t orient, const paperformat_t format, const uint32_t paper_width, const uint32_t paper_height, const uint32_t num_copies, const bool_t collate_copies, HB_ITEM *pages_block)
{
    String *filename = hb_block_to_utf8(filename_block);
    String *printer = hb_block_to_utf8(printer_block);
    String *pages = hb_block_to_utf8(pages_block);
    HBOFFICE_GLOBAL.funcs.func_officesdk_writer_print(writer, tc(filename), tc(printer), orient, format, paper_width, paper_height, num_copies, collate_copies, tc(pages), &GTNAP_GLOBAL->office_last_error);
    str_destroy(&filename);
    str_destroy(&printer);
    str_destroy(&pages);
}

/*---------------------------------------------------------------------------*/

void hb_office_writer_close(Writer *writer)
{
    HBOFFICE_GLOBAL.funcs.func_officesdk_writer_close(writer, &GTNAP_GLOBAL->office_last_error);
}

/*---------------------------------------------------------------------------*/

void hb_office_writer_page_header_show(Writer *writer, const bool_t show)
{
    HBOFFICE_GLOBAL.funcs.func_officesdk_writer_page_header_show(writer, show, &GTNAP_GLOBAL->office_last_error);
}

/*---------------------------------------------------------------------------*/

void hb_office_writer_page_header_margins(Writer *writer, const uint32_t left, const uint32_t right, const uint32_t spacing, const uint32_t height, const bool_t dynamic_spacing, const bool_t dynamic_height)
{
    HBOFFICE_GLOBAL.funcs.func_officesdk_writer_page_header_margins(writer, left, right, spacing, height, dynamic_spacing, dynamic_height, &GTNAP_GLOBAL->office_last_error);
}

/*---------------------------------------------------------------------------*/

void hb_office_writer_page_footer_show(Writer *writer, const bool_t show)
{
    HBOFFICE_GLOBAL.funcs.func_officesdk_writer_page_footer_show(writer, show, &GTNAP_GLOBAL->office_last_error);
}

/*---------------------------------------------------------------------------*/

void hb_office_writer_page_footer_margins(Writer *writer, const uint32_t left, const uint32_t right, const uint32_t spacing, const uint32_t height, const bool_t dynamic_spacing, const bool_t dynamic_height)
{
    HBOFFICE_GLOBAL.funcs.func_officesdk_writer_page_footer_margins(writer, left, right, spacing, height, dynamic_spacing, dynamic_height, &GTNAP_GLOBAL->office_last_error);
}

/*---------------------------------------------------------------------------*/

void hb_office_writer_page_margins(Writer *writer, const uint32_t left, const uint32_t right, const uint32_t top, const uint32_t bottom, const uint32_t gutter)
{
    HBOFFICE_GLOBAL.funcs.func_officesdk_writer_page_margins(writer, left, right, top, bottom, gutter, &GTNAP_GLOBAL->office_last_error);
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
    HBOFFICE_GLOBAL.funcs.func_officesdk_writer_font_family(writer, GTNAP_GLOBAL->office_text_space, tc(ffamily), &GTNAP_GLOBAL->office_last_error);
    str_destroy(&ffamily);
}

/*---------------------------------------------------------------------------*/

void hb_office_writer_font_size(Writer *writer, const real32_t fsize)
{
    HBOFFICE_GLOBAL.funcs.func_officesdk_writer_font_size(writer, GTNAP_GLOBAL->office_text_space, fsize, &GTNAP_GLOBAL->office_last_error);
}

/*---------------------------------------------------------------------------*/

void hb_office_writer_bold(Writer *writer, const bool_t bold)
{
    HBOFFICE_GLOBAL.funcs.func_officesdk_writer_bold(writer, GTNAP_GLOBAL->office_text_space, bold, &GTNAP_GLOBAL->office_last_error);
}

/*---------------------------------------------------------------------------*/

void hb_office_writer_italic(Writer *writer, const bool_t italic)
{
    HBOFFICE_GLOBAL.funcs.func_officesdk_writer_italic(writer, GTNAP_GLOBAL->office_text_space, italic, &GTNAP_GLOBAL->office_last_error);
}

/*---------------------------------------------------------------------------*/

void hb_office_writer_paragraph_halign(Writer *writer, const halign_t align)
{
    HBOFFICE_GLOBAL.funcs.func_officesdk_writer_paragraph_halign(writer, GTNAP_GLOBAL->office_text_space, align, &GTNAP_GLOBAL->office_last_error);
}

/*---------------------------------------------------------------------------*/

void hb_office_writer_paragraph_lspacing(Writer *writer, const uint32_t height)
{
    HBOFFICE_GLOBAL.funcs.func_officesdk_writer_paragraph_lspacing(writer, GTNAP_GLOBAL->office_text_space, height, &GTNAP_GLOBAL->office_last_error);
}

/*---------------------------------------------------------------------------*/

void hb_office_writer_insert_text(Writer *writer, HB_ITEM *text_block)
{
    String *text = hb_block_to_utf8(text_block);
    HBOFFICE_GLOBAL.funcs.func_officesdk_writer_insert_text(writer, GTNAP_GLOBAL->office_text_space, tc(text), &GTNAP_GLOBAL->office_last_error);
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

    HBOFFICE_GLOBAL.funcs.func_officesdk_writer_insert_text(writer, GTNAP_GLOBAL->office_text_space, dashes, &GTNAP_GLOBAL->office_last_error);
    heap_delete_n(&dashes, n * 3 + 1, char_t);
}

/*---------------------------------------------------------------------------*/

void hb_office_writer_insert_image(Writer *writer, const anchortype_t anchor, const uint32_t width, const uint32_t height, const halign_t halign, const valign_t valign, HB_ITEM *image_path_block)
{
    String *image_path = hb_block_to_utf8(image_path_block);
    HBOFFICE_GLOBAL.funcs.func_officesdk_writer_insert_image(writer, GTNAP_GLOBAL->office_text_space, anchor, width, height, halign, valign, tc(image_path), &GTNAP_GLOBAL->office_last_error);
    str_destroy(&image_path);
}

/*---------------------------------------------------------------------------*/

void hb_office_writer_insert_page_number(Writer *writer)
{
    HBOFFICE_GLOBAL.funcs.func_officesdk_writer_insert_page_number(writer, GTNAP_GLOBAL->office_text_space, &GTNAP_GLOBAL->office_last_error);
}

/*---------------------------------------------------------------------------*/

void hb_office_writer_insert_new_line(Writer *writer)
{
    HBOFFICE_GLOBAL.funcs.func_officesdk_writer_insert_new_line(writer, GTNAP_GLOBAL->office_text_space, &GTNAP_GLOBAL->office_last_error);
}

/*---------------------------------------------------------------------------*/

void hb_office_writer_insert_paragraph(Writer *writer)
{
    HBOFFICE_GLOBAL.funcs.func_officesdk_writer_insert_paragraph(writer, GTNAP_GLOBAL->office_text_space, &GTNAP_GLOBAL->office_last_error);
}

/*---------------------------------------------------------------------------*/

void hb_office_writer_insert_page_break(Writer *writer)
{
    HBOFFICE_GLOBAL.funcs.func_officesdk_writer_insert_page_break(writer, &GTNAP_GLOBAL->office_last_error);
}
