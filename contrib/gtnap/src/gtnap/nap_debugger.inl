/*
    This is part of gtnap
    TODO: More info
*/

#include "gtnap.ixx"

__EXTERN_C

GtNapDebugger *nap_debugger_create(const char_t *path, const uint32_t nrows, const uint32_t ncols);

void nap_debugger_destroy(GtNapDebugger **debug);

void nap_debugger_scroll(GtNapDebugger *debug, const uint32_t top, const uint32_t left, const uint32_t bottom, const uint32_t right, const uint32_t row, const uint32_t col, const uint32_t codepoint, const byte_t color);

void nap_debugger_box(GtNapDebugger *debug, const uint32_t top, const uint32_t left, const uint32_t bottom, const uint32_t right, const byte_t color);

void nap_debugger_cursor(GtNapDebugger *debug, const uint32_t style);

void nap_debugger_set_pos(GtNapDebugger *debug, const uint32_t row, const uint32_t col);

void nap_debugger_get_pos(GtNapDebugger *debug, uint32_t *row, uint32_t *col);

void nap_debugger_putchar(GtNapDebugger *debug, const uint32_t row, const uint32_t col, const uint32_t codepoint, const byte_t color, const byte_t attrib); 

void nap_debugger_puttext(GtNapDebugger *debug, const uint32_t row, const uint32_t col, const byte_t color, const char_t *utf8); 

void nap_debugger_save(GtNapDebugger *debug, const uint32_t top, const uint32_t left, const uint32_t bottom, const uint32_t right, byte_t *buffer);

void nap_debugger_rest(GtNapDebugger *debug, const uint32_t top, const uint32_t left, const uint32_t bottom, const uint32_t right, const byte_t *buffer);

int32_t nap_debugger_read_key(GtNapDebugger *debug); 

__END_C

