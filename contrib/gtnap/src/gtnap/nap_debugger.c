/*
    This is part of gtnap
    TODO: More info
    Commit - 2
*/

#include "nap_debugger.inl"
#include <deblib/deblib.h>
#include "setcurs.ch"
#include "nappgui.h"

struct _gtnap_debugger_t
{
    Proc *proc;
    Socket *socket;
    Stream *stream;
};

/*---------------------------------------------------------------------------*/

GtNapDebugger *nap_debugger_create(const char_t *path, const uint32_t nrows, const uint32_t ncols)
{
    GtNapDebugger *debug = heap_new(GtNapDebugger);
    uint32_t ip = bsocket_url_ip("localhost", NULL);
    String *cmd = str_cpath("%s %d %d", path, nrows, ncols);
    debug->proc = bproc_exec(tc(cmd), NULL);
    bthread_sleep(100);

    debug->socket = bsocket_connect(ip, 3555, 0, NULL);
    if (debug->socket != NULL)
        debug->stream = stm_socket(debug->socket);

    str_destroy(&cmd);
    return debug;
}

/*---------------------------------------------------------------------------*/

void nap_debugger_destroy(GtNapDebugger **debug)
{
    heap_delete(debug, GtNapDebugger);
}

/*---------------------------------------------------------------------------*/

void nap_debugger_scroll(GtNapDebugger *debug, const uint32_t top, const uint32_t left, const uint32_t bottom, const uint32_t right, const uint32_t row, const uint32_t col, const uint32_t codepoint, const uint32_t color)
{
    cassert_no_null(debug);
    if (debug->stream != NULL)
        deblib_send_scroll(debug->stream, top, left, bottom, right, row, col, codepoint, color);
}

/*---------------------------------------------------------------------------*/

void nap_debugger_box(GtNapDebugger *debug, const uint32_t top, const uint32_t left, const uint32_t bottom, const uint32_t right, const uint32_t color)
{
    cassert_no_null(debug);
    if (debug->stream != NULL)
        deblib_send_box(debug->stream, top, left, bottom, right, color);
}

/*---------------------------------------------------------------------------*/

static cursor_t i_cursor(const uint32_t style)
{
    switch( style ) {
    case SC_NONE:
        return ekCURSOR_NONE;
    case SC_NORMAL:
        return ekCURSOR_NORMAL;
    case SC_INSERT:
        return ekCURSOR_INSERT;
    case SC_SPECIAL1:
        return ekCURSOR_SPECIAL1;
    case SC_SPECIAL2:
        return ekCURSOR_SPECIAL2;
    cassert_default();
    }

    return ekCURSOR_NORMAL;
}

/*---------------------------------------------------------------------------*/

void nap_debugger_cursor(GtNapDebugger *debug, const uint32_t style)
{
    cassert_no_null(debug);
    if (debug->stream != NULL) 
    {
        cursor_t cursor = i_cursor(style);
        deblib_send_cursor(debug->stream, cursor);
    }
}

/*---------------------------------------------------------------------------*/

void nap_debugger_set_pos(GtNapDebugger *debug, const uint32_t row, const uint32_t col)
{
    cassert_no_null(debug);
    if (debug->stream != NULL) 
        deblib_send_set_pos(debug->stream, row, col);
}

/*---------------------------------------------------------------------------*/

void nap_debugger_putchar(GtNapDebugger *debug, const uint32_t row, const uint32_t col, const uint32_t codepoint, const uint32_t color, const byte_t attrib)
{
    cassert_no_null(debug);
    if (debug->stream != NULL)
        deblib_send_putchar(debug->stream, row, col, codepoint, color, attrib);
}

/*---------------------------------------------------------------------------*/

void nap_debugger_puttext(GtNapDebugger *debug, const uint32_t row, const uint32_t col, const uint32_t color, const char_t *utf8)
{
    cassert_no_null(debug);
    if (debug->stream != NULL)
        deblib_send_puttext(debug->stream, row, col, color, utf8);
}
