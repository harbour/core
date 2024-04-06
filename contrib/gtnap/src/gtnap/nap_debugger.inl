/*
    This is part of gtnap
    TODO: More info
*/

#include "gtnap.ixx"

__EXTERN_C

GtNapDebugger *nap_debugger_create(const char_t *path, const uint32_t nrows, const uint32_t ncols);

void nap_debugger_destroy(GtNapDebugger **debug);

__END_C

