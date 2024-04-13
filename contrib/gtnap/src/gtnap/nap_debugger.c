/*
    This is part of gtnap
    TODO: More info
    Commit - 2
*/

#include "nap_debugger.inl"
#include <deblib/deblib.h>
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
