/* NAppGUI form */

#include "nforms.hxx"
#include <nflib/flayout.h>
#include <gui/button.h>
#include <gui/edit.h>
#include <gui/guicontrol.h>
#include <gui/label.h>
#include <gui/panel.h>
#include <gui/window.h>
#include <core/heap.h>
#include <core/stream.h>
#include <sewer/cassert.h>

struct _nform_t
{
    FLayout *flayout;
    Layout *glayout;
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

NForm *nform_from_data(const byte_t *data, const uint32_t size)
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
    (*form)->glayout = NULL;
    heap_delete(form, NForm);
}

/*---------------------------------------------------------------------------*/

Window *nform_window(const NForm *form, const uint32_t flags)
{
    Layout *layout = NULL;
    Panel *panel = NULL;
    Window *window = NULL;
    cassert_no_null(form);
    layout = flayout_to_gui(form->flayout, 40.f, 20.f);
    panel = panel_create();
    window = window_create(flags);
    panel_layout(panel, layout);
    window_panel(window, panel);
    cast(form, NForm)->glayout = layout;
    return window;
}

/*---------------------------------------------------------------------------*/

void nform_set_control_str(NForm *form, const char_t *cell_name, const char_t *value)
{
    GuiControl *control = NULL;
    cassert_no_null(form);
    cassert_no_null(form->glayout);
    control = flayout_search_gui_control(form->flayout, form->glayout, cell_name);
    if (control != NULL)
    {
        Button *button = guicontrol_button(control);
        Label *label = guicontrol_label(control);
        Edit *edit = guicontrol_edit(control);

        if (button != NULL)
            button_text(button, value);
        else if (label != NULL)
            label_text(label, value);
        else if (edit != NULL)
            edit_text(edit, value);
    }
}

/*---------------------------------------------------------------------------*/

void nform_set_control_bool(NForm *form, const char_t *cell_name, const bool_t value)
{
    GuiControl *control = NULL;
    cassert_no_null(form);
    cassert_no_null(form->glayout);
    control = flayout_search_gui_control(form->flayout, form->glayout, cell_name);
    if (control != NULL)
    {
        Button *button = guicontrol_button(control);
        if (button != NULL)
            button_state(button, value == TRUE ? ekGUI_ON : ekGUI_OFF);
    }
}

/*---------------------------------------------------------------------------*/

bool_t nform_set_listener(NForm *form, const char_t *cell_name, Listener *listener)
{
    GuiControl *control = NULL;
    cassert_no_null(form);
    cassert_no_null(form->glayout);
    control = flayout_search_gui_control(form->flayout, form->glayout, cell_name);
    if (control != NULL)
    {
        Button *button = guicontrol_button(control);
        if (button != NULL)
        {
            button_OnClick(button, listener);
            return TRUE;
        }
    }

    return FALSE;
}

