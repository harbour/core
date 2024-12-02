/* NAppGUI form */

#include "nforms.hxx"
#include <nflib/flayout.h>
#include <gui/panel.h>
#include <gui/window.h>
#include <core/heap.h>
#include <core/stream.h>
#include <sewer/cassert.h>

struct _nform_t
{
    FLayout *flayout;
};

/*---------------------------------------------------------------------------*/

static NForm *i_from_stream(Stream **stm)
{
    cassert_no_null(stm);
    if (*stm != NULL)
    {
        NForm *form = heap_new0(NForm);
        form->flayout = flayout_read(*stm);
        stm_close(stm);
        return form;
    }

    return NULL;
}

/*---------------------------------------------------------------------------*/

NForm *nform_create(const byte_t *data, const uint32_t size)
{
    Stream *stm = stm_from_block(data, size);
    return i_from_stream(&stm);
}

/*---------------------------------------------------------------------------*/

NForm *nform_from_file(const char_t *pathname, ferror_t *error)
{
    Stream *stm = stm_from_file(pathname, error);
    return i_from_stream(&stm);
}

/*---------------------------------------------------------------------------*/

void nform_destroy(NForm **form)
{
    cassert_no_null(form);
    cassert_no_null(*form);
    flayout_destroy(&(*form)->flayout);
    heap_delete(form, NForm);
}

/*---------------------------------------------------------------------------*/

Window *nform_window(const NForm *form)
{
    Layout *layout = NULL;
    Panel *panel = NULL;
    Window *window = NULL;
    cassert_no_null(form);
    layout = flayout_to_gui(form->flayout, 40.f, 20.f);
    panel = panel_create();
    window = window_create(ekWINDOW_STD);
    window_panel(window, panel);
    return window;
}
