/* Debugger library */

#include "deblib.hxx"

__EXTERN_C

const char_t* deblib_path(void);

void deblib_send_resolution(Stream *stm, const uint32_t num_rows, const uint32_t num_cols);

void deblib_send_scroll(Stream *stm, const uint32_t top, const uint32_t left, const uint32_t bottom, const uint32_t right, const uint32_t row, const uint32_t col, const uint32_t codepoint, const byte_t color);

void deblib_send_box(Stream *stm, const uint32_t top, const uint32_t left, const uint32_t bottom, const uint32_t right, const byte_t color);

void deblib_send_cursor(Stream *stm, const cursor_t cursor);

void deblib_send_set_pos(Stream *stm, const uint32_t row, const uint32_t col);

void deblib_send_get_pos(Stream *stm, uint32_t *row, uint32_t *col);

void deblib_send_putchar(Stream *stm, const uint32_t row, const uint32_t col, const uint32_t codepoint, const byte_t color, const byte_t attrib);

void deblib_send_puttext(Stream *stm, const uint32_t row, const uint32_t col, const byte_t color, const char_t *utf8);

void deblib_send_save(Stream *stm, const uint32_t top, const uint32_t left, const uint32_t bottom, const uint32_t right, byte_t *buffer);

void deblib_send_rest(Stream *stm, const uint32_t top, const uint32_t left, const uint32_t bottom, const uint32_t right, const byte_t *buffer);

void deblib_read_key(Stream *stm, vkey_t *key, uint32_t *modifiers);

void deblib_close(Stream *stm);

void deblib_recv_message(Stream *stm, DebMsg *msg);

const char_t *deblib_msg_str(const msg_type_t msg);

const char_t *deblib_cursor_str(const cursor_t cursor);

void deblib_init_colors(color_t *colors);

extern uint16_t kDEBLIB_SERVER_PORT;

__END_C
