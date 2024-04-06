/*
    This is part of gtnap
    TODO: More info
*/

#include "gtnap.ixx"

__EXTERN_C

GtNapDebugger *nap_debugger_create(const char_t *path, const uint32_t nrows, const uint32_t ncols);

void nap_debugger_destroy(GtNapDebugger **debug);

void nap_debugger_putchar(GtNapDebugger *debug, const uint32_t row, const uint32_t col, const uint32_t codepoint, const uint32_t color, const byte_t attrib); 

__END_C

