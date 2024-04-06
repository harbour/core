/* Debugger library */

#include "deblib.h"
#include <core/stream.h>
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

void deblib_recv_message(Stream *stm, DebMsg *msg)
{
    cassert_no_null(msg);
    msg->type = stm_read_enum(stm, msg_type_t);
    switch (msg->type) {
    case ekMSG_SET_SIZE:
        msg->row = stm_read_u32(stm);
        msg->col = stm_read_u32(stm);
        break;
    case ekMSG_PUTCHAR:
    {
        uint32_t codepoint, nbytes;
        msg->row = stm_read_u32(stm);
        msg->col = stm_read_u32(stm);
        codepoint = stm_read_u32(stm);
        msg->color = stm_read_u32(stm);
        msg->attrib = (byte_t)stm_read_u8(stm);
        nbytes = unicode_to_char(codepoint, msg->utf8, ekUTF8);
        msg->utf8[nbytes] = 0;
        break;
    }

    cassert_default();
    }
}
