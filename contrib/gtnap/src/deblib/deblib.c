/* Debugger library */

#include "deblib.h"
#include <gui/gui.h>
#include <draw2d/color.h>
#include <core/stream.h>
#include <core/strings.h>
#include <sewer/cassert.h>
#include <sewer/unicode.h>

/*---------------------------------------------------------------------------*/

uint16_t kDEBLIB_SERVER_PORT = 3555;

/*---------------------------------------------------------------------------*/

void deblib_send_resolution(Stream *stm, const uint32_t num_rows, const uint32_t num_cols)
{
    stm_write_enum(stm, ekMSG_SET_SIZE, msg_type_t);
    stm_write_u32(stm, num_rows);
    stm_write_u32(stm, num_cols);
}

/*---------------------------------------------------------------------------*/

void deblib_send_scroll(Stream *stm, const uint32_t top, const uint32_t left, const uint32_t bottom, const uint32_t right, const uint32_t row, const uint32_t col, const uint32_t codepoint, const byte_t color)
{
    stm_write_enum(stm, ekMSG_SCROLL, msg_type_t);
    stm_write_u32(stm, top);
    stm_write_u32(stm, left);
    stm_write_u32(stm, bottom);
    stm_write_u32(stm, right);
    stm_write_u32(stm, row);
    stm_write_u32(stm, col);
    stm_write_u32(stm, codepoint);
    stm_write_u8(stm, color);
}

/*---------------------------------------------------------------------------*/

void deblib_send_box(Stream *stm, const uint32_t top, const uint32_t left, const uint32_t bottom, const uint32_t right, const byte_t color)
{
    stm_write_enum(stm, ekMSG_BOX, msg_type_t);
    stm_write_u32(stm, top);
    stm_write_u32(stm, left);
    stm_write_u32(stm, bottom);
    stm_write_u32(stm, right);
    stm_write_u8(stm, color);
}

/*---------------------------------------------------------------------------*/

void deblib_send_cursor(Stream *stm, const cursor_t cursor)
{
    stm_write_enum(stm, ekMSG_CURSOR, msg_type_t);
    stm_write_enum(stm, cursor, cursor_t);
}

/*---------------------------------------------------------------------------*/

void deblib_send_set_pos(Stream *stm, const uint32_t row, const uint32_t col)
{
    stm_write_enum(stm, ekMSG_SET_POS, msg_type_t);
    stm_write_u32(stm, row);
    stm_write_u32(stm, col);
}

/*---------------------------------------------------------------------------*/

void deblib_send_putchar(Stream *stm, const uint32_t row, const uint32_t col, const uint32_t codepoint, const byte_t color, const byte_t attrib)
{
    stm_write_enum(stm, ekMSG_PUTCHAR, msg_type_t);
    stm_write_u32(stm, row);
    stm_write_u32(stm, col);
    stm_write_u32(stm, codepoint);
    stm_write_u8(stm, color);
    stm_write_u8(stm, attrib);
}

/*---------------------------------------------------------------------------*/

void deblib_send_puttext(Stream *stm, const uint32_t row, const uint32_t col, const byte_t color, const char_t *utf8)
{
    uint32_t len = str_len_c(utf8);
    if (len > 0)
    {
        stm_write_enum(stm, ekMSG_PUTTEXT, msg_type_t);
        stm_write_u32(stm, row);
        stm_write_u32(stm, col);
        stm_write_u8(stm, color);
        stm_write_u32(stm, len);
        stm_write(stm, (const byte_t*)utf8, len);
    }
}

/*---------------------------------------------------------------------------*/

void deblib_send_save(Stream *stm, const uint32_t top, const uint32_t left, const uint32_t bottom, const uint32_t right, byte_t *buffer)
{
    uint32_t n = (bottom - top + 1) * (right - left + 1);
    stm_write_enum(stm, ekMSG_SAVE, msg_type_t);
    stm_write_u32(stm, top);
    stm_write_u32(stm, left);
    stm_write_u32(stm, bottom);
    stm_write_u32(stm, right);
    stm_read(stm, buffer, n * (sizeof(uint16_t) + sizeof(byte_t)));
}

/*---------------------------------------------------------------------------*/

void deblib_send_rest(Stream *stm, const uint32_t top, const uint32_t left, const uint32_t bottom, const uint32_t right, const byte_t *buffer)
{
    uint32_t n = (bottom - top + 1) * (right - left + 1);
    stm_write_enum(stm, ekMSG_REST, msg_type_t);
    stm_write_u32(stm, top);
    stm_write_u32(stm, left);
    stm_write_u32(stm, bottom);
    stm_write_u32(stm, right);
    stm_write(stm, buffer, n * (sizeof(uint16_t) + sizeof(byte_t)));
}

/*---------------------------------------------------------------------------*/

void deblib_read_key(Stream *stm, vkey_t *key, uint32_t *modifiers)
{
    cassert_no_null(key);
    cassert_no_null(modifiers);
    stm_write_enum(stm, ekMSG_READ_KEY, msg_type_t);
    *key = stm_read_enum(stm, vkey_t);
    *modifiers = stm_read_u32(stm);
}

/*---------------------------------------------------------------------------*/

void deblib_close(Stream *stm)
{
    stm_write_enum(stm, ekMSG_CLOSE, msg_type_t);
}

/*---------------------------------------------------------------------------*/

static uint32_t i_stm_read_codepoint(Stream *stm)
{
    uint32_t codepoint = stm_read_u32(stm);
    return codepoint == 0 ? ' ' : codepoint;
}

/*---------------------------------------------------------------------------*/

void deblib_recv_message(Stream *stm, DebMsg *msg)
{
    cassert_no_null(msg);
    msg->type = stm_read_enum(stm, msg_type_t);
    switch (msg->type) {
    case ekMSG_SET_SIZE:
        msg->row = stm_read_u32(stm);
        msg->col = stm_read_u32(stm);
        break;

    case ekMSG_SCROLL:
    {
        uint32_t codepoint, nbytes;
        msg->top = stm_read_u32(stm);
        msg->left = stm_read_u32(stm);
        msg->bottom = stm_read_u32(stm);
        msg->right = stm_read_u32(stm);
        msg->row = stm_read_u32(stm);
        msg->col = stm_read_u32(stm);
        codepoint = i_stm_read_codepoint(stm);
        msg->colorb = (byte_t)stm_read_u8(stm);
        nbytes = unicode_to_char(codepoint, msg->utf8, ekUTF8);
        msg->utf8[nbytes] = 0;
        break;
    }

    case ekMSG_BOX:
        msg->top = stm_read_u32(stm);
        msg->left = stm_read_u32(stm);
        msg->bottom = stm_read_u32(stm);
        msg->right = stm_read_u32(stm);
        msg->colorb = (byte_t)stm_read_u8(stm);
        break;

    case ekMSG_CURSOR:
        msg->attrib = (byte_t)stm_read_enum(stm, cursor_t);
        break;

    case ekMSG_SET_POS:
        msg->row = stm_read_u32(stm);
        msg->col = stm_read_u32(stm);
        break;

    case ekMSG_PUTCHAR:
    {
        uint32_t codepoint, nbytes;
        msg->row = stm_read_u32(stm);
        msg->col = stm_read_u32(stm);
        codepoint = i_stm_read_codepoint(stm);
        msg->colorb = (byte_t)stm_read_u8(stm);
        msg->attrib = (byte_t)stm_read_u8(stm);
        nbytes = unicode_to_char(codepoint, msg->utf8, ekUTF8);
        msg->utf8[nbytes] = 0;
        break;
    }

    case ekMSG_PUTTEXT:
    {
        uint32_t len;
        msg->row = stm_read_u32(stm);
        msg->col = stm_read_u32(stm);
        msg->colorb = (byte_t)stm_read_u8(stm);
        len = stm_read_u32(stm);
        cassert(len > 0);
        cassert(len < MAX_UTF8_SIZE);
        stm_read(stm, (byte_t*)msg->utf8, len);
        msg->utf8[len] = 0;
        break;
    }

    case ekMSG_SAVE:
        msg->top = stm_read_u32(stm);
        msg->left = stm_read_u32(stm);
        msg->bottom = stm_read_u32(stm);
        msg->right = stm_read_u32(stm);
        break;

    case ekMSG_REST:
        msg->top = stm_read_u32(stm);
        msg->left = stm_read_u32(stm);
        msg->bottom = stm_read_u32(stm);
        msg->right = stm_read_u32(stm);
        break;

    case ekMSG_READ_KEY:
        break;

    case ekMSG_CLOSE:
        break;

    cassert_default();
    }
}

/*---------------------------------------------------------------------------*/

const char_t *deblib_cursor_str(const cursor_t cursor)
{
    switch(cursor) {
    case ekCURSOR_NONE:
        return "CURSOR_NONE";
    case ekCURSOR_NORMAL:
        return "CURSOR_NORMAL";
    case ekCURSOR_INSERT:
        return "CURSOR_INSERT";
    case ekCURSOR_SPECIAL1:
        return "CURSOR_SPECIAL1";
    case ekCURSOR_SPECIAL2:
        return "CURSOR_SPECIAL2";
    cassert_default();
    }

    return "";
}

/*---------------------------------------------------------------------------*/
/*
 * https://gogh-co.github.io/Gogh/
 * Dark mode: Breeze
 * Light mode: Clrs
 */
void deblib_init_colors(color_t *colors)
{
    /* In dark mode, black and white are inverted */
    color_t DARK_COL_BLACK = color_html("#FCFCFC");
    color_t DARK_COL_BLUE = color_html("#1D99F3");
    color_t DARK_COL_GREEN = color_html("#11D116");
    color_t DARK_COL_CYAN = color_html("#1ABC9C");
    color_t DARK_COL_RED = color_html("#ED1515");
    color_t DARK_COL_MAGENTA = color_html("#9B59B6");
    color_t DARK_COL_BROWN = color_html("#F67400");
    color_t DARK_COL_WHITE = color_html("#232627");
    color_t DARK_COL_LIGHT_GRAY = color_html("#FFFFFF");
    color_t DARK_COL_BRIGHT_BLUE = color_html("#3DAEE9");
    color_t DARK_COL_BRIGHT_GREEN = color_html("#1CDC9A");
    color_t DARK_COL_BRIGHT_CYAN = color_html("#3DAEE9");
    color_t DARK_COL_BRIGHT_RED = color_html("#C0392B");
    color_t DARK_COL_BRIGHT_MAGENTA = color_html("#8E44AD");
    color_t DARK_COL_YELLOW = color_html("#FDBC4B");
    color_t DARK_COL_BRIGHT_WHITE = color_html("#7F8C8D");

    color_t LIGHT_COL_BLACK = color_html("#000000");
    color_t LIGHT_COL_BLUE = color_html("#135CD0");
    color_t LIGHT_COL_GREEN = color_html("#328A5D");
    color_t LIGHT_COL_CYAN = color_html("#33C3C1");
    color_t LIGHT_COL_RED = color_html("#F8282A");
    color_t LIGHT_COL_MAGENTA = color_html("#9F00BD");
    color_t LIGHT_COL_BROWN = color_html("#FA701D");
    color_t LIGHT_COL_WHITE = color_html("#B3B3B3");
    color_t LIGHT_COL_LIGHT_GRAY = color_html("#555753");
    color_t LIGHT_COL_BRIGHT_BLUE = color_html("#1670FF");
    color_t LIGHT_COL_BRIGHT_GREEN = color_html("#2CC631");
    color_t LIGHT_COL_BRIGHT_CYAN = color_html("#00FFFF");
    color_t LIGHT_COL_BRIGHT_RED = color_html("#FB0416");
    color_t LIGHT_COL_BRIGHT_MAGENTA = color_html("#E900B0");
    color_t LIGHT_COL_YELLOW = color_html("#FDD727");
    color_t LIGHT_COL_BRIGHT_WHITE = color_html("#EEEEEC");

    cassert_no_null(colors);
    colors[COL_BLACK] = gui_alt_color(LIGHT_COL_BLACK, DARK_COL_BLACK);
    colors[COL_BLUE] = gui_alt_color(LIGHT_COL_BLUE, DARK_COL_BLUE);
    colors[COL_GREEN] = gui_alt_color(LIGHT_COL_GREEN, DARK_COL_GREEN);
    colors[COL_CYAN] = gui_alt_color(LIGHT_COL_CYAN, DARK_COL_CYAN);
    colors[COL_RED] = gui_alt_color(LIGHT_COL_RED, DARK_COL_RED);
    colors[COL_MAGENTA] = gui_alt_color(LIGHT_COL_MAGENTA, DARK_COL_MAGENTA);
    colors[COL_BROWN] = gui_alt_color(LIGHT_COL_BROWN, DARK_COL_BROWN);
    colors[COL_WHITE] = gui_alt_color(LIGHT_COL_WHITE, DARK_COL_WHITE);
    colors[COL_LIGHT_GRAY] = gui_alt_color(LIGHT_COL_LIGHT_GRAY, DARK_COL_LIGHT_GRAY);
    colors[COL_BRIGHT_BLUE] = gui_alt_color(LIGHT_COL_BRIGHT_BLUE, DARK_COL_BRIGHT_BLUE);
    colors[COL_BRIGHT_GREEN] = gui_alt_color(LIGHT_COL_BRIGHT_GREEN, DARK_COL_BRIGHT_GREEN);
    colors[COL_BRIGHT_CYAN] = gui_alt_color(LIGHT_COL_BRIGHT_CYAN, DARK_COL_BRIGHT_CYAN);
    colors[COL_BRIGHT_RED] = gui_alt_color(LIGHT_COL_BRIGHT_RED, DARK_COL_BRIGHT_RED);
    colors[COL_BRIGHT_MAGENTA] = gui_alt_color(LIGHT_COL_BRIGHT_MAGENTA, DARK_COL_BRIGHT_MAGENTA);
    colors[COL_YELLOW] = gui_alt_color(LIGHT_COL_YELLOW, DARK_COL_YELLOW);
    colors[COL_BRIGHT_WHITE] = gui_alt_color(LIGHT_COL_BRIGHT_WHITE, DARK_COL_BRIGHT_WHITE);
}
