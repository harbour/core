/* Debugger library */

#include "deblib.h"
#include <core/stream.h>
#include <sewer/cassert.h>

/*---------------------------------------------------------------------------*/

void deblib_send_resolution(Stream *stm, const uint32_t num_rows, const uint32_t num_cols)
{
    stm_write_enum(stm, ekMSG_SET_SIZE, msg_type_t);
    stm_write_u32(stm, num_rows);
    stm_write_u32(stm, num_cols);
}

/*---------------------------------------------------------------------------*/

void deblib_recv_message(Stream *stm, DebMsg *msg)
{
    cassert_no_null(msg);
    msg->type = stm_read_enum(stm, msg_type_t);
    switch (msg->type) {
    case ekMSG_SET_SIZE:
        msg->p0 = stm_read_u32(stm);
        msg->p1 = stm_read_u32(stm);
        break;
    cassert_default();
    }
}
