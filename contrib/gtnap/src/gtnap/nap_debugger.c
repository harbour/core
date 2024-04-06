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
    debug->proc = bproc_exec(path, NULL);
    bthread_sleep(1000);
    debug->socket = bsocket_connect(ip, 3555, 0, NULL);
    if (debug->socket != NULL)
        debug->stream = stm_socket(debug->socket);

    if (debug->stream != NULL)
        deblib_send_resolution(debug->stream, nrows, ncols);

    return debug;
}

/*---------------------------------------------------------------------------*/

void nap_debugger_destroy(GtNapDebugger **debug)
{
    heap_delete(debug, GtNapDebugger);
}

/*---------------------------------------------------------------------------*/
