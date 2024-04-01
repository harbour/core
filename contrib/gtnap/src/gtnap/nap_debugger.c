/*
    This is part of gtnap
    TODO: More info
    Commit - 2
*/

#include "nap_debugger.inl"
#include "nappgui.h"

struct _gtnap_debugger_t
{
    Window *window;
};

/*---------------------------------------------------------------------------*/

GtNapDebugger *nap_debugger_create(const uint32_t nrows, const uint32_t ncols)
{
    GtNapDebugger *debug = heap_new(GtNapDebugger);
    unref(nrows);
    unref(ncols);
    return debug;
}

/*---------------------------------------------------------------------------*/

void nap_debugger_destroy(GtNapDebugger **debug)
{
    heap_delete(debug, GtNapDebugger);
}

/*---------------------------------------------------------------------------*/
