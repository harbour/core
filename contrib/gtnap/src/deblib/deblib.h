/* Debugger library */

#include "deblib.hxx"

__EXTERN_C

void deblib_send_resolution(Stream *stm, const uint32_t num_rows, const uint32_t num_cols);

void deblib_send_scroll(Stream *stm, const uint32_t top, const uint32_t left, const uint32_t bottom, const uint32_t right, const uint32_t row, const uint32_t col, const uint32_t codepoint, const uint32_t color);

void deblib_send_box(Stream *stm, const uint32_t top, const uint32_t left, const uint32_t bottom, const uint32_t right, const uint32_t color);

void deblib_send_cursor(Stream *stm, const cursor_t cursor);

void deblib_send_putchar(Stream *stm, const uint32_t row, const uint32_t col, const uint32_t codepoint, const uint32_t color, const byte_t attrib);

void deblib_send_puttext(Stream *stm, const uint32_t row, const uint32_t col, const uint32_t color, const char_t *utf8);

void deblib_recv_message(Stream *stm, DebMsg *msg);

const char_t *deblib_cursor_str(const cursor_t cursor);

__END_C
