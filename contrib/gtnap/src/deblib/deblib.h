/* Debugger library */

#include "deblib.hxx"

__EXTERN_C

void deblib_send_resolution(Stream *stm, const uint32_t num_rows, const uint32_t num_cols);

void deblib_recv_message(Stream *stm, DebMsg *msg);

__END_C
