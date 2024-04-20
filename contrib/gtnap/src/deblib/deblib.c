/* Debugger library */

#include "deblib.h"
#include <core/stream.h>
#include <core/strings.h>
#include <sewer/cassert.h>
#include <sewer/unicode.h>

/*---------------------------------------------------------------------------*/

void deblib_send_resolution(Stream *stm, const uint32_t num_rows, const uint32_t num_cols)
{
    stm_write_enum(stm, ekMSG_SET_SIZE, msg_type_t);
    stm_write_u32(stm, num_rows);
    stm_write_u32(stm, num_cols);
}

/*---------------------------------------------------------------------------*/

void deblib_send_scroll(Stream *stm, const uint32_t top, const uint32_t left, const uint32_t bottom, const uint32_t right, const uint32_t row, const uint32_t col, const uint32_t codepoint, const uint32_t color)
{
    stm_write_enum(stm, ekMSG_SCROLL, msg_type_t);
    stm_write_u32(stm, top);
    stm_write_u32(stm, left);
    stm_write_u32(stm, bottom);
    stm_write_u32(stm, right);
    stm_write_u32(stm, row);
    stm_write_u32(stm, col);
    stm_write_u32(stm, codepoint);
    stm_write_u32(stm, color);
}

/*---------------------------------------------------------------------------*/

void deblib_send_box(Stream *stm, const uint32_t top, const uint32_t left, const uint32_t bottom, const uint32_t right, const uint32_t color)
{
    stm_write_enum(stm, ekMSG_BOX, msg_type_t);
    stm_write_u32(stm, top);
    stm_write_u32(stm, left);
    stm_write_u32(stm, bottom);
    stm_write_u32(stm, right);
    stm_write_u32(stm, color);
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

void deblib_send_putchar(Stream *stm, const uint32_t row, const uint32_t col, const uint32_t codepoint, const uint32_t color, const byte_t attrib)
{
    stm_write_enum(stm, ekMSG_PUTCHAR, msg_type_t);
    stm_write_u32(stm, row);
    stm_write_u32(stm, col);
    stm_write_u32(stm, codepoint);
    stm_write_u32(stm, color);
    stm_write_u8(stm, (uint8_t)attrib);
}

/*---------------------------------------------------------------------------*/

void deblib_send_puttext(Stream *stm, const uint32_t row, const uint32_t col, const uint32_t color, const char_t *utf8)
{
    uint32_t len = str_len_c(utf8);
    if (len > 0)
    {
        stm_write_enum(stm, ekMSG_PUTTEXT, msg_type_t);
        stm_write_u32(stm, row);
        stm_write_u32(stm, col);
        stm_write_u32(stm, color);
        stm_write_u32(stm, len);
        stm_write(stm, (const byte_t*)utf8, len);
    }
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
        msg->color = stm_read_u32(stm);
        nbytes = unicode_to_char(codepoint, msg->utf8, ekUTF8);
        msg->utf8[nbytes] = 0;
        break;
    }

    case ekMSG_BOX:
        msg->top = stm_read_u32(stm);
        msg->left = stm_read_u32(stm);
        msg->bottom = stm_read_u32(stm);
        msg->right = stm_read_u32(stm);
        msg->color = stm_read_u32(stm);
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
        msg->color = stm_read_u32(stm);
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
        msg->color = stm_read_u32(stm);
        len = stm_read_u32(stm);
        cassert(len > 0);
        cassert(len < MAX_UTF8_SIZE);
        stm_read(stm, (byte_t*)msg->utf8, len);
        msg->utf8[len] = 0;
        break;
    }

    case ekMSG_READ_KEY:
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
